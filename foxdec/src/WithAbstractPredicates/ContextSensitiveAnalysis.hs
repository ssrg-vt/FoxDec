{-# LANGUAGE PartialTypeSignatures , DeriveGeneric, BangPatterns, StrictData, FunctionalDependencies, FlexibleInstances, UndecidableInstances, FlexibleContexts#-}

module WithAbstractPredicates.ContextSensitiveAnalysis where

import Base
import Config

import WithAbstractPredicates.Class
import WithAbstractPredicates.GenerateCFG
import WithAbstractPredicates.GenerateInvariants
import WithAbstractPredicates.ControlFlow
import OutputGeneration.CallGraph

import Data.JumpTarget
import Data.Symbol
import Data.L0
import Data.CFG
import Data.Indirection
import Data.VerificationCondition
import Data.X86.Opcode
import Data.X86.Instruction

import Binary.Generic
import Binary.FunctionNames



import Algorithm.SCC



import Data.Word
import Data.List
import Data.List.Extra (firstJust)
import Data.Maybe
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS


import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Extra

import System.Directory (doesFileExist)



import GHC.Generics
import Debug.Trace












type WithLifting bin pred finit v = StateT (L0 pred finit v) (ReaderT (bin,Config) IO)


-- A mapping of blockIDs to predicates
type Invariants pred = IM.IntMap pred




type Recursions = IM.IntMap IS.IntSet






data AnalysisResult pred finit v = FoundNewCalls (IM.IntMap finit) | AnalyzedWithResult (FResult pred v)





-- Lift the binary to the L0 representation
lift_to_L0 :: WithAbstractPredicates bin pred finit v => Config -> bin -> finit -> IM.IntMap Indirections -> IO (L0 pred finit v)
lift_to_L0 config bin finit inds = do
  entries0 <- get_entries
  let exported_functions = map fst $ binary_get_exported_functions bin
  let entries = filter ((/=) 0) $ entries0 ++ exported_functions


  let l0  = L0 IM.empty inds IM.empty ""
  let l0' = foldr (\entry -> l0_insert_new_entry entry finit) l0 entries
  let recs = IM.empty
  let entry_graph = Edges $ IM.fromList (zip entries $ repeat IS.empty)
  runReaderT (execStateT (exploreFunctionEntries entry_graph recs) l0') (bin,config)
 where
  get_entries = do
    let dirname  = binary_dir_name bin
    let name     = binary_file_name bin
    let fname    = dirname ++ name ++ ".entry" 
    let entries0 = filter ((/=) 0) $ map fromIntegral $ binary_entry bin
    exists <- doesFileExist fname
    if exists then do
      entries <- liftIO $ parse $! fname
      return $ entries0 ++ entries
    else
      return entries0
  parse filename = do
    ls <- readFile filename
    return $ map read_line $ lines ls
  read_line = readHex' . tail . tail


exploreFunctionEntries :: WithAbstractPredicates bin pred finit v => Graph -> Recursions -> WithLifting bin pred finit v ()
exploreFunctionEntries entries recursions = do
  -- Check if a (mutually) recursive function must be reconsidered as all its calls are done.
  rec <- reconsider_mutual_recursive_call recursions
  case rec of
    Just (entry,trgts) -> do
      -- If yes, then reinsert the function entry, remove the current known results and continue
      done <- entry_has_been_done_and_is_terminating entry
      if done then do
        let recursions' = IM.delete entry recursions
        continue_with_next_entry entries recursions'
      else do
        liftIO $ putStrLn $ "Reconsidering (due to mutual recursion) entry " ++ showHex entry ++ " as all entries " ++ showHex_set trgts ++ " are now done."
        let entries'    = graph_add_edges entries entry IS.empty 
        let recursions' = IM.delete entry recursions
        modify $ l0_adjust_result entry Nothing
        exploreFunctionEntry entries' recursions' $ fromIntegral entry
    Nothing -> continue_with_next_entry entries recursions 
 where
  continue_with_next_entry entries recursions = do
    -- Find next entry to consider
    case next_entry entries recursions of
      Nothing -> exploreDanglingFunctionPointers 
      Just a  -> exploreFunctionEntry entries recursions $ fromIntegral a
  next_entry entries recursions = try_end_node_from_node_to_be_reconsidered entries recursions `orTry` graph_find_next entries
  try_end_node_from_node_to_be_reconsidered entries recursions = firstJust (try_find_end_node_from_node entries) $ IS.toList $ IS.unions $ IM.elems recursions

  entry_has_been_done_and_is_terminating entry = do
    result <- gets $ l0_lookup_entry entry
    case result of
      Just (_,Just result) -> return $ result_post result == Terminates
      _                    -> return $ False

exploreDanglingFunctionPointers :: WithAbstractPredicates bin pred finit v => WithLifting bin pred finit v ()
exploreDanglingFunctionPointers = do
  l0 <- get
  static <- mk_static

  let fptrs = concatMap (get_dangling_function_pointers $ l0_functions l0) $ IM.elems $ l0_functions l0

  case fptrs of
    [] -> exploreDanglingRelocations
    _ -> do
      liftIO $ putStrLn $ "Dangling function pointers found: " ++ (showHex_set $ IS.fromList fptrs) ++ "\n\n"
      let entries' = Edges $ IM.fromList (zip fptrs $ repeat IS.empty)
      let finit = new_finit static
      mapM_ (\a -> modify $ l0_insert_new_entry a finit) fptrs
      exploreFunctionEntries entries' IM.empty
 where
  get_dangling_function_pointers l0 (_,Nothing) = []
  get_dangling_function_pointers l0 (_,Just r)  = concatMap (mkDanglingFunctionPointers l0) $ S.toList $ result_vcs r

  mkDanglingFunctionPointers l0 (FunctionPointers a ptrs) = concatMap (isDangling l0) $ IS.toList ptrs

  isDangling l0 ptr = 
    case IM.lookup ptr l0 of
      Nothing -> if isVisited l0 ptr then [] else [ptr] 
      Just _  -> []

  isVisited l0 ptr = any (function_contains_address ptr) l0

  function_contains_address a (_,Nothing) = False
  function_contains_address a (_,Just (FResult cfg _ _ _ _ _)) = cfg_contains_address a cfg

  cfg_contains_address a cfg = any (block_contains_address a) $ cfg_instrs cfg

  block_contains_address a b = any (\i -> inAddress i == fromIntegral a) b

exploreDanglingRelocations :: WithAbstractPredicates bin pred finit v => WithLifting bin pred finit v ()
exploreDanglingRelocations = do
  (bin,_)        <- ask
  relocs <- filterM is_dangling $ S.toList $ binary_get_relocations bin
  case relocs of
    [] -> finishExploration
    _  -> do
      liftIO $ putStrLn $ "Dangling relocations found: " ++ show relocs
      let fptrs = map get_fptr relocs

      let entries' = Edges $ IM.fromList (zip fptrs $ repeat IS.empty)
      static <- mk_static
      let finit = new_finit static
      mapM_ (\a -> modify $ l0_insert_new_entry a finit) fptrs
      exploreFunctionEntries entries' IM.empty
 where
  get_fptr (Relocation _ a1) = fromIntegral a1
  is_dangling (Relocation a0 a1) = do
    (bin,_)        <- ask
    let valid_entry = address_has_instruction bin a1 
    has_been_done  <- entry_has_been_done IM.empty a1 
    -- TODO a0 within data section?
    return $ valid_entry && not has_been_done



exploreFunctionEntry :: WithAbstractPredicates bin pred finit v => Graph -> Recursions -> Word64 ->  WithLifting bin pred finit v ()
exploreFunctionEntry entries recursions entry = do
  (bin,_)        <- ask
  let valid_entry = address_has_instruction bin entry
  explore valid_entry 
 where
  explore :: WithAbstractPredicates bin pred finit v => Bool ->  WithLifting bin pred finit v ()
  explore False = do
    -- Entry is not within a text section of the binary, delete it and continue
    liftIO $ putStrLn $ "\n\nEntry " ++ showHex entry ++ " ignored."
    modify $ l0_adjust_result entry (Just empty_result) 

    let entries' = graph_delete entries $ fromIntegral entry
    exploreFunctionEntries entries' recursions    
  explore True = do
    -- Entry is to be explored
    l0 <- get
    liftIO $ putStrLn $ "\n\nEntry " ++ showHex entry ++ " (#entries explored/to be explored: " ++ show (IM.size $ l0_functions l0) ++ "/" ++ show (IS.size $ intgraph_V entries) ++ ")"
    result <- analyze_entry entry 
    case result of
      FoundNewCalls m -> do
        -- Found calls to unexplored functions, or functions called with a new FInit
        -- technically we should remove the old result here, in case of mutual recursion but that also leads to unreachable instructions
        let new_calls = IM.toList m
        let entries' = foldr (\(trgt,finit) entries -> graph_add_edges entries (fromIntegral entry) (IS.singleton trgt)) entries new_calls
        mapM_ (\(trgt,finit) -> modify $ l0_insert_new_entry trgt finit) new_calls
        exploreFunctionEntries entries' recursions
      AnalyzedWithResult result -> do
        -- Finished analysis of the function entry
        curr_result <- gets $ l0_lookup_result entry
        let curr_post = result_post <$> curr_result
        entries'' <- if curr_post /= Just Terminates && result_post result == Terminates then do
                       mark_callers_of_newly_terminating_callee entries recursions entry
                     else
                       return entries

        modify $ l0_adjust_result entry (Just result) 
        let entries' = graph_delete entries'' (fromIntegral entry)
        recursions' <- mark_mutual_recursive_calls entry result recursions
        report_result result
        exploreFunctionEntries entries' recursions'
       
  report_result result = do
    liftIO $ putStrLn $ "Entry " ++ showHex entry ++ " lifted."
    Just (finit,_) <- gets $ l0_lookup_entry entry
    static <- mk_static
    let empty_finit = new_finit static
    when (finit /= empty_finit) $ do
      liftIO $ putStrLn $ "Function precondition:\n" ++ pp_finit static finit
    liftIO $ putStrLn $ "Function postcondition: " ++ (show $ result_post result)
    liftIO $ putStrLn $ (intercalate "\n" $ map show $ S.toList $ result_vcs result)


mark_callers_of_newly_terminating_callee :: WithAbstractPredicates bin pred finit v => Graph -> Recursions -> Word64 -> WithLifting bin pred finit v Graph
mark_callers_of_newly_terminating_callee entries recursions entry = do
  static <- mk_static
  funcs <- gets l0_functions
  let callers  = IM.keysSet $ IM.filter (calls_entry static) funcs
  let not_already_reconsidered = IS.filter (\caller -> caller `IM.notMember` recursions && not (graph_is_parent entries caller)) callers
  if IS.null not_already_reconsidered then
    return entries
  else do
    liftIO $ putStrLn $ "Reconsidering (due to callee " ++ showHex entry ++ " being a terminating function) entries " ++ showHex_set not_already_reconsidered
    let entries' = foldr (\caller entries -> graph_add_edges entries (fromIntegral caller) IS.empty) entries $ IS.toList not_already_reconsidered
    mapM (\caller -> modify $ l0_adjust_result caller Nothing) $ IS.toList not_already_reconsidered
    return entries'
 where
  calls_entry static (_,Just (FResult cfg _ _ _ _ _)) = fromIntegral entry `IS.member` calls_of_cfg static cfg
  calls_entry _ _ = False

entry_has_been_done :: Recursions -> Word64 -> WithLifting bin pred finit v Bool
entry_has_been_done recursions entry = do
  result <- gets $ l0_lookup_entry entry
  case result of
    Just (_,Just _) -> return $ not $ fromIntegral entry `IM.member` recursions
    _               -> return $ False


error_if_open_mutual_recursions :: Graph -> Recursions -> WithLifting bin pred finit v ()
error_if_open_mutual_recursions entries recursions
  | IM.null recursions = return ()
  | otherwise = error $ (intercalate "\n" $ map show_entry $ IM.assocs recursions) ++ "Entries:\n" ++ show entries
 where
  show_entry (entry,trgts) = "0x" ++ showHex entry ++ ": " ++ showHex_set trgts

reconsider_mutual_recursive_call :: Recursions -> WithLifting bin pred finit v (Maybe (Int,IS.IntSet))
reconsider_mutual_recursive_call recursions = findM to_be_reconsidered $ IM.toList recursions
 where
  to_be_reconsidered (entry,trgts) = allM (entry_has_been_done recursions) $ map fromIntegral $ IS.toList trgts


-- If an entry has an FInit but no FResult, this means that it has been visited but not analyzed yet.
-- Calls to those functions indicate recursion. It is stored that the current entry did a recursive call to those functions,
-- so that it can be reconsidered later.
mark_mutual_recursive_calls :: Word64 -> FResult pred v -> Recursions -> WithLifting bin pred finit v Recursions
mark_mutual_recursive_calls entry result recursions = do
  new_recursions <- IS.fromList . map fromIntegral <$> (filterM is_recursive $ S.toList $ result_calls result)
  if IS.null new_recursions then
    return recursions
  else do
    liftIO $ putStrLn $ "Entry " ++ showHex entry ++ " is part of mutual recursion and treated entries " ++ showHex_set new_recursions ++ " as external. It will be reconsidered after those entries have been done."
    return $ IM.insertWith IS.union (fromIntegral entry) new_recursions recursions
 where
  is_recursive :: Word64 -> WithLifting bin pred finit v Bool
  is_recursive entry = do
    result <- gets $ l0_lookup_entry entry
    case result of
      Nothing          -> return $ True
      Just (_,Nothing) -> return $ True
      _                -> return $ False --  fromIntegral entry `IM.member` recursions -- False



mk_static :: WithLifting bin pred finit v (Lifting bin pred finit v)
mk_static = do
  l <- get
  (bin,config) <- ask
  return (bin,config,l)

analyze_entry :: WithAbstractPredicates bin pred finit v => Word64 -> WithLifting bin pred finit v (AnalysisResult pred finit v)
analyze_entry entry = do
  (Just (finit,_)) <- gets $ l0_lookup_entry entry

  static <- mk_static 
  liftIO $ putStrLn $ "Entry " ++ showHex entry ++ ": starting CFG generation."
  !cfg <- liftIO $ generate_cfg static entry
  liftIO $ putStrLn $ "Entry " ++ showHex entry ++ ": CFG generation done: #basic blocks = " ++ show (IM.size $ cfg_instrs cfg) ++ ", #instructions = " ++ show (num_of_instructions cfg)

  liftIO $ putStrLn $ "Entry " ++ showHex entry ++ ": starting invariant generation."
  let !(invs,gmem_structure',vcs)  = generate_invariants (withEntry entry static) cfg finit
  modify $ l0_set_gmem_structure gmem_structure'

  new_calls <- (join_duplicate_new_calls static . catMaybes) <$> (concatMapM (get_new_calls invs) $ IM.assocs $ cfg_instrs cfg)

  if new_calls /= [] then do
    liftIO $ putStrLn $ "Entry " ++ showHex entry ++ ": Adding new internal functions at " ++ showHex_list (map fst new_calls) ++ " to entries to be explored."
    return $ FoundNewCalls $ IM.fromList new_calls
  else do
    to_be_resolved_blocks <- filterM (is_to_be_resolved_block) $ IM.assocs $ cfg_instrs cfg
    is_newly_resolved <- mapM (try_resolve_indirection invs) to_be_resolved_blocks
    if or is_newly_resolved then do
      liftIO $ putStrLn $ "Entry " ++ showHex entry ++ ": Continuing to next round as there are newly resolved indirections."
      analyze_entry entry 
    else do
      liftIO $ putStrLn $ "Entry " ++ showHex entry ++ ": Terminating as no new indirections are resolved."

      -- liftIO $ putStrLn $ "Invariants:\n" ++ intercalate "\n" (map show $ IM.assocs invs)

      let posts = invs_to_post (withEntry entry static) cfg invs
      let join  = invs_to_joined_post (withEntry entry static) cfg invs
      let pars  = invs_to_PA (withEntry entry static) cfg invs
      calls <- S.fromList <$> (concatMapM get_internal_calls $ concat $ IM.elems $ cfg_instrs cfg)
      return $ AnalyzedWithResult $ FResult cfg posts (Just join) calls vcs pars
 where
  join_duplicate_new_calls static [] = []
  join_duplicate_new_calls static ((a,finit):new_calls) =
    let (same,rest) = partition (\(a',_) -> a == a') new_calls
        finit'      = foldr1 (join_finits $ withEntry (fromIntegral a) static) (finit:map snd same) in
      (a,finit') : join_duplicate_new_calls static rest

  get_new_calls invs (blockID,instrs)
    -- Assumes calls are always last intruction in block (TODO jumps that are actually calls)
    | isCall (inOperation $ last instrs) = do
      static <- mk_static
      let trgts = get_known_jump_targets static $ last instrs
      mapM (get_new_calls_from_trgt invs blockID instrs) trgts
    | otherwise = return [Nothing]

  get_new_calls_from_trgt invs blockID instrs (ImmediateAddress a') = do
    result <- gets $ l0_lookup_entry a'
    case result of
      Nothing             -> do -- Unvisited
                               static <- withEntry entry <$> mk_static
                               let (post,_) = get_postcondition_for_block static blockID (init instrs) invs
                               let new_finit = pred_to_finit static post
                               return $ Just (fromIntegral a',new_finit)
      Just (_,Nothing)    -> do -- Recursive unanalyzed call
                               return Nothing
      Just (finit,Just r) -> do -- Already analyzed call
                               static <- withEntry entry <$> mk_static 
                               let (post,_) = get_postcondition_for_block static blockID (init instrs) invs
                               let new_finit = pred_to_finit static post
                               let join = join_finits static new_finit finit
                               if finit /= join then do
                                  return $ Just (fromIntegral a',join)
                               else
                                  return Nothing
  get_new_calls_from_trgt _ _ _ _ = return Nothing


  get_internal_calls i
    | isCall (inOperation i) = do
      static <- mk_static 
      let trgts = get_known_jump_targets static i
      return $ concatMap get_internal_call_from_trgt trgts
    -- TODO jump
    | otherwise = return []

  get_internal_call_from_trgt (ImmediateAddress a') = [a']
  get_internal_call_from_trgt _ = []


  is_to_be_resolved_block :: WithAbstractPredicates bin pred finit v => (Int,[Instruction]) -> WithLifting bin pred finit v Bool
  is_to_be_resolved_block (blockID,instrs)
    | isJump (inOperation $ last instrs) || isCall (inOperation $ last instrs) || isSyscall (inOperation $ last instrs) = is_to_be_resolved_indirection (last instrs)
    | otherwise = return False

  is_to_be_resolved_indirection :: WithAbstractPredicates bin pred finit v => Instruction -> WithLifting bin pred finit v Bool
  is_to_be_resolved_indirection i = do
   l0 <- get
   (bin,config) <- ask
   case jump_target_for_instruction bin i of
     Unresolved -> return True
     External "error" -> return True
     _ -> return False

  try_resolve_indirection :: WithAbstractPredicates bin pred finit v => IM.IntMap pred -> (Int,[Instruction]) -> WithLifting bin pred finit v Bool
  try_resolve_indirection invs (blockID,instrs) = do
    let pre = fromJust $ IM.lookup blockID invs
    static <- mk_static
    let inds = resolve_indirection (withEntry entry static) pre instrs
    case Indirection_Unresolved `S.member` inds of
      True -> do
        liftIO $ putStrLn $ "Unresolved indirection: " ++ show (last instrs) ++ "\n" ++ show inds
        liftIO $ putStrLn $ show pre
        add_newly_resolved_indirection inds (inAddress $ last instrs)
      _ -> do
        liftIO $ putStrLn $ "Resolved indirection: " ++ show (last instrs)
        liftIO $ putStrLn $ intercalate ", " $ map show $ S.toList inds
        add_newly_resolved_indirection inds (inAddress $ last instrs) 

add_newly_resolved_indirection inds a' = do
  curr_inds <- gets $ l0_lookup_indirection a'
  case curr_inds of
    Nothing -> modify (l0_insert_indirection a' inds) >> return True
    Just inds' -> if inds `S.isSubsetOf` inds' then 
                    return False
                  else do
                    liftIO $ putStrLn $ "Extending previous indirection resolving @ " ++ showHex a' ++ ": " ++ show (S.toList inds') ++ " with new resolving: " ++ show (S.toList inds)
                    modify $ l0_insert_indirection a' (S.union inds inds')
                    return True









finishExploration :: WithAbstractPredicates bin pred finit v => WithLifting bin pred finit v ()
finishExploration = return () 










