{-# LANGUAGE PartialTypeSignatures, DeriveGeneric, BangPatterns, Strict, ScopedTypeVariables #-}

module InputLifting.Lift where

import Base
import Config
import Conventions

import Data.JumpTarget
import Data.Symbol
import Data.Size
import Data.CFG
import Data.CFI
import Data.Indirection
import Data.VerificationCondition
import Data.X86.Opcode
import Data.X86.Instruction
import Data.SymbolicExpression
import Data.X86.Register

import Algorithm.Graph
import qualified Data.Tree as T
import qualified Data.Tree.View as TV

import Binary.Generic
import Binary.FunctionNames

import WithNoAbstraction.Pointers (expr_is_global_immediate)
import InputLifting.ControlFlowGraph
import InputLifting.SymbolicExecution
import InputLifting.Types
import InputLifting.NextRips

import Data.Word
import Data.List
import Data.List.Extra (firstJust)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS


import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Extra




import GHC.Generics
import Debug.Trace



-- TODO MOVE
lift_to_lifted_representation_functions :: BinaryClass bin => bin -> Config -> IO (LiftedRepresentationFunctions bin)
lift_to_lifted_representation_functions bin config = do
  lru <- lift_to_unstructured_representation bin config
  let instrs = current_instrs lru
  let nexts  = current_nexts lru
  let comms  = current_comments lru
  funcs     <- runReaderT lr_make_functions (bin,config,lru,IM.empty)
  xtoLog $ "#instructions = " ++ show (IM.size instrs)
  xtoLog $ "#functions    = " ++ show (IM.size funcs)
  return $ LiftedRepresentationFunctions bin config instrs nexts funcs comms


lift_to_unstructured_representation :: BinaryClass bin => bin -> Config -> IO LiftedRepresentationUnstructured
lift_to_unstructured_representation bin config = runReaderT (execStateT run init_lr) (bin,config)
 where
  run = do
    get_ehframe_locations >>= add_function_entries_to_bag
    explore_bag False
    add_function_entries_to_bag mk_init_bag
    xlift

  mk_init_bag = S.fromList $ map mk_entry $ delete 0 $ binary_entry bin
  mk_entry e  = (FunctionEntry $ fromIntegral e,InstructionAddress $ fromIntegral e)




------------------
-- Main algorithm
------------------

-- The main lifting algorithm: recursive traversal (essentially a DFS), with the frontier stored in the bag.
-- After exploration, dangling function entries are explored until no more exists.
xlift :: BinaryClass bin => XLifting bin ()
xlift = do
  explore_bag True
  ifM explore_more_entries xlift xlift_finish
 where
  explore_more_entries = orM [
      explore_dangling get_dangling_relocations     "relocations"
    , explore_dangling get_dangling_exports         "exported functions"
    , explore_dangling get_dangling_LEA_entries     "LEA's"
    ]

  explore_dangling get_dangling_fptrs msg = do
    dangling <- get_dangling_fptrs
    if S.null dangling then do
      return False
    else do
      xtoLog $ "Dangling function pointers from " ++ msg ++ ": " ++ show (S.toList $ S.map snd dangling)
      add_function_entries_to_bag dangling
      return True


-- After lifting, finish by reporting some stats.
xlift_finish :: BinaryClass bin => XLifting bin ()
xlift_finish = do
  (bin,_) <- ask
  xtoLog $ binary_file_name bin ++ ": done!\n"


-- Add new function entries to the bag
add_function_entries_to_bag :: BinaryClass bin => S.Set (FunctionEntry,InstructionAddress) -> XLifting bin ()
add_function_entries_to_bag = mapM_ (\(e,a) -> add_instruction_to_fmap e a >> add_address_to_bag Nothing a) . S.toList

-- Add an edge (a0,a1) only if its new
add_next_address :: BinaryClass bin => InstructionAddress -> InstructionAddress -> XLifting bin ()
add_next_address a a' = do
  cfg <- gets current_cfg
  if xgraph_is_edge cfg (toInt a) (toInt a') then
    return ()
  else do
    modify $ register_edge a a'
    add_address_to_bag (Just a) a'

-- Add an edge (a0,a1) to the bag so that a1 will be explored further in the main algorithm
-- If a0 == Nothing, then a1 was not reached through exploration but is a new entry to be explored.
add_address_to_bag :: BinaryClass bin => Maybe InstructionAddress -> InstructionAddress -> XLifting bin ()
add_address_to_bag a a' = fetch a' >>= onJustM add_instruction
 where
  add_instruction :: BinaryClass bin => Instruction -> XLifting bin ()
  add_instruction i = do
    xDebug 1 $ "Adding to bag " ++ show (a,a')
    modify $ register_add_to_bag a a'



-----------------------
-- Getting entry points
-----------------------

get_ehframe_locations :: BinaryClass bin => XLifting bin (S.Set (FunctionEntry,InstructionAddress))
get_ehframe_locations = do
  (bin,_)   <- ask
  let fptrs  = map (\a -> (FunctionEntry a, InstructionAddress a)) $ map (fromIntegral . function_entry) $ all_tables bin
  let lps    = get_all_landing_pads' bin
  let starts = get_all_region_starts bin
  let bag    = S.fromList $ concat [fptrs, lps,starts]
  modify $ register_real_entries $ S.map fst bag
  return bag
 where
  get_all_landing_pads' bin = map withFunctionEntry $ get_all_landing_pads bin
  withFunctionEntry (e,lp) = (FunctionEntry $ fromIntegral e,InstructionAddress lp)
  all_tables bin = IM.elems $ cfi_gcc_except_tables $ binary_get_cfi bin

  get_all_region_starts bin = concatMap mk_region_start $ all_tables bin
  mk_region_start t =
    let region_starts = IS.toList $ get_callsite_region_starts_from_gcc_except_table t in
      map (\start -> (FunctionEntry $ fromIntegral $ function_entry t ,InstructionAddress start)) region_starts

get_dangling_LEA_entries :: BinaryClass bin => XLifting bin (S.Set (FunctionEntry,InstructionAddress))
get_dangling_LEA_entries = do
  is <- gets current_instrs 
  let leas = mapMaybe get_lea_pointer $ IM.assocs is
  S.fromList <$> mapMaybeM whenDangling leas
 where
  get_lea_pointer (a,Instruction label prefix LEA [dst,op] _ si) = mk_rip_relative a si op
  get_lea_pointer _ = Nothing

  mk_rip_relative :: Int -> Int -> Operand -> Maybe Int
  mk_rip_relative a si op@(Op_Mem _ (Reg64 RIP) RegNone 0 displ Nothing info) = Just $ fromIntegral $ fromIntegral a + displ + fromIntegral si
  mk_rip_relative a si op@(Op_Mem _ (Reg64 RIP) _ _ _ _ _) = error $ "TODO: " ++ show op
  mk_rip_relative a si op@(Op_Mem _ _ (Reg64 RIP) _ _ _ _) = error $ "TODO: " ++ show op
  mk_rip_relative a si op = Nothing


get_dangling_relocations :: BinaryClass bin => XLifting bin (S.Set (FunctionEntry,InstructionAddress))
get_dangling_relocations = do
  (bin,_) <- ask
  let relocs = map getInt $ IM.elems $ binary_get_relocations bin
  modify $ register_real_entries $ S.fromList $ map FunctionEntry relocs
  S.fromList <$> mapMaybeM whenDangling relocs
 where
  getInt (Relocation a) = fromIntegral a



get_dangling_exports :: BinaryClass bin => XLifting bin (S.Set (FunctionEntry,InstructionAddress))
get_dangling_exports = do
  (bin,_) <- ask
  let exports = IS.toList $ IS.fromList $ map fst $ binary_get_exported_functions bin
  modify $ register_real_entries $ S.fromList $ map FunctionEntry exports
  S.fromList <$> mapMaybeM whenDangling exports


isDangling :: BinaryClass bin => Int -> XLifting bin Bool
isDangling a = do
  (bin,_) <- ask
  is <- gets current_instrs 
  return $ IM.notMember a is && address_has_instruction bin (fromIntegral a)

whenDangling a = do
  dangling <- isDangling a
  return $ if dangling then Just (FunctionEntry a,InstructionAddress a) else Nothing







------------------------------------
-- Exploration of control flow edges
------------------------------------

-- The recursive traversal. Pick an edge (a0,a1) from the bag. Address a0 is already explored, so we should know which function it belongs to.
-- Based on that, we decide the function entry of a1. Then we explore address a1.
explore_bag :: BinaryClass bin => Bool -> XLifting bin ()
explore_bag continue = whenJustM pick_from_bag $ \x -> explore x >> when continue (explore_bag continue)
 where
  -- Explore address $a1$ coming from address $a0$
  explore (a0,a1) = do
    Just i1 <- fetch a1
    continue <- add_edge_to_fmap a0 a1
    if continue then do
      nxt <- next_rips i1
      xDebug 1 $ "Exploring " ++ show (a0,a1) ++ " to " ++ show nxt
      explore_address a1 nxt 
    else
      return ()
  -- Pick the item from the bag 
  pick_from_bag = do
    bag <- gets current_bag
    case S.minView bag of
      Nothing -> return Nothing
      Just (x,bag') -> do
        modify $ \lr -> lr { current_bag = bag' }
        return $ Just x



-- Add a new instruction address to the function map.
-- We must know to which function the address belongs.
-- We check if there is an inconsistency (an address cannot belong to multiple functions, in such case inlining should have been detected).
-- If so, we error out.
add_instruction_to_fmap :: BinaryClass bin => FunctionEntry -> InstructionAddress -> XLifting bin ()
add_instruction_to_fmap new_entry a = do
  old_entry <- get_entry_of_instruction a
  xDebug 1 $ "Registering entry " ++ show new_entry ++ " for address " ++ show a
  case old_entry of
    Nothing -> modify $ register_entry_for_instruction new_entry a
    Just e  -> if e /= new_entry then  error $ "ERROR: address " ++ show a ++ " already explored (new_entry == " ++ show new_entry ++ ", old_entry == " ++ show e  ++ ")" else return ()


-- Given an edge (a0,a1) try to figure out what function entry address a1 belongs to, and then add it to the current function map.
add_edge_to_fmap :: BinaryClass bin => Maybe InstructionAddress -> InstructionAddress -> XLifting bin Bool
add_edge_to_fmap Nothing   a1 = return True
add_edge_to_fmap (Just a0) a1 = do
  entry_after_a0 <- mk_entry_after_a0
  case entry_after_a0 of
    Nothing -> error $ "Does this happen?" -- return False
    Just e0 -> add_instruction_to_fmap e0 a1 >> return True
 where
  -- We have an edge (a0,a1). For a0 we know to which function it belongs. 
  -- So what function does a1 belong to?
  mk_entry_after_a0 = do
    (bin,_) <- ask
    Just i0 <- fetch a0
    nxt <- next_rips i0
    case nxt of
      NxtInternalCall a' -> do
        -- If a0 was a call to an internal function, a1 is the start of a new function.
        when (a1 /= a') $ error $ show (a1,a')
        old_entry1 <- get_entry_of_instruction a1
        when (old_entry1 /= Nothing && (toInt <$> old_entry1) /= Just (toInt a1)) $ do
          xDebug 0 $ "Going to make new entry: " ++ show a1
          explore_call a0 a1
        -- TODO when a1 is covered by different entry than a1 itself, error out
        return $ Just $ cast_address_to_entry a1
      NxtReturn _ -> do
        -- If a0 was a return, then we should know already to which function a1 belongs.
        get_entry_of_instruction a1
      NxtAddresses f as -> do
        old_entry0 <- get_entry_of_instruction a0
        old_entry1 <- get_entry_of_instruction a1
        if old_entry1 /= Nothing && old_entry1 /= old_entry0 then do
          -- a1 was already explored, and normal control flow goes from a0 to a1. Still they belong to different snippets.
          -- This can happen, e.g, if a0 is a JMP to function entry a1. We explore the edge again as "normal control flow"
          -- which can cause a1 to become its own function entry.
          explore_normal_addresses a0 as
          get_entry_of_instruction a1
        else do
          find_table <- find_ehframe_table_covering_address a1
          case find_table of
            Just t -> do
              let real_entry = FunctionEntry $ fromIntegral $ function_entry t
              when (old_entry1 == Nothing && old_entry0 /= Nothing && Just real_entry /= old_entry0) $ do
                xDebug 0 $ "Leaking from " ++ show (fromJust old_entry0) ++ " into " ++ show real_entry ++ " as " ++ show a1 ++ " is covered by ehframe table, " ++ show (cast_address_to_entry a1) ++ " inlined in function " ++ show (fromJust old_entry0)
                modify $ register_inlining (cast_address_to_entry a1) (fromJust old_entry0) 
              return $ Just $ real_entry
            Nothing ->
              -- a1 is not explored yet, or was explored and belongs to the same function as a0. 
              -- So a1 belongs to the same function as a0
              return old_entry0
      _ -> do
        -- The default case: normal control flow within a function, so a1 belongs to the same function as a0.
        get_entry_of_instruction a0



explore_address :: BinaryClass bin => InstructionAddress -> Next -> XLifting bin ()
explore_address a (NxtTerminal f)      = return ()
explore_address a (NxtAddresses f as)  = explore_normal_addresses a as
explore_address a (NxtInternalCall a') = explore_call a a'
explore_address a (NxtReturn f)        = explore_return a 
explore_address a nxt                  = do
  i <- fetch a
  error $ "TODO: " ++ show a ++ "\n" ++ show nxt ++ "\n" ++ show i




-- Explore next addresses due to normal control flow
explore_normal_addresses :: BinaryClass bin => InstructionAddress -> S.Set InstructionAddress -> XLifting bin ()
explore_normal_addresses a as = do
  Just i <- fetch a
  mapM_ (explore_addr $ isCall $ inOperation i) as
 where
  explore_addr isCall a' = do
    Just a_entry <- get_entry_of_instruction a
    a'_entry     <- get_entry_of_instruction a' 

    if not isCall && a'_entry /= Nothing && a'_entry /= Just a_entry then do
      -- A function entry is explored without calling it
      if (a'_entry /= Just (cast_address_to_entry a')) then do
        xDebug 0 $ "Normal control flow leading to within another function: "  ++ show a ++ " within function " ++ show a_entry ++ " leads to " ++ show a' ++ " inlined in function " ++ show (fromJust a'_entry)
        make_new_entry a' (fromJust a'_entry)
      else do
        xDebug 0 $ "Normal control flow leading to a function entry: " ++ show a ++ " leads to function entry " ++ show a' ++ " inlined in function " ++ show a_entry
        modify $ register_inlining (cast_address_to_entry a') a_entry
      -- Register a caller
      xDebug 0 $ "Registering caller " ++ show a ++ " for entry " ++ show a'
      register_caller (cast_address_to_entry a') a
      -- The called function is triggered
      trigger_call_to_entry $ cast_address_to_entry a'
      modify $ register_edge a a'
    else do
      add_next_address a a'





explore_call a a' = do
  let entry' = cast_address_to_entry a'
  modify $ register_real_entries $ S.singleton entry'
  a'_entry <- get_entry_of_instruction a'
  cfg <- gets current_cfg
  bag <- gets current_bag

  xDebug 0 $ "Registering caller " ++ show a ++ " for entry " ++ show entry'
  if (a'_entry /= Nothing && a'_entry /= Just (cast_address_to_entry a')) && xgraph_is_parent cfg (toInt a') then do
    -- Earlier, the called function entry was explored as part of another function, i.e., without calling it
    Just a'_entry <- get_entry_of_instruction a'
    xDebug 0 $ "Calling to within an already explored function " ++ show a ++ " --> " ++ show a' ++ ": function " ++ show entry' ++ " is inlined in function " ++ show a'_entry
    make_new_entry a' a'_entry
    -- Register a caller
    register_caller entry' a
    -- The called function is triggered
    trigger_call_to_entry entry'
    modify $ register_edge a a'
  else do
    -- Register a caller
    register_caller entry' a
    -- The called function is triggered
    trigger_call_to_entry entry'

    explore_normal_addresses a $ S.singleton a'

make_new_entry :: BinaryClass bin => InstructionAddress -> FunctionEntry -> XLifting bin ()
make_new_entry a encompassing_function = do
  let new_entry = cast_address_to_entry a 
  old_entry <- get_entry_of_instruction a
  cfg <- gets current_cfg

  let parents = xgraph_parents cfg $ toInt a
  reach <- get_function_reach a encompassing_function

  xDebug 1 $ "Reach: " ++ showHex_set reach
  new_rets <- IS.fromList <$> (filterM isRET $ IS.toList reach)

  xDebug 0 $ "Making new entry " ++ show new_entry ++ " inlined in " ++ show encompassing_function ++ " with RETs " ++ showHex_set new_rets  ++ " and callers " ++ showHex_set parents
  -- remove all RETs from reach from "current_returns old_entry"
  when (old_entry /= Nothing) $ do
    modify $ \lr -> lr { current_returns = IM.adjust (\rets -> IS.difference rets new_rets) (toInt $ fromJust old_entry) $ current_returns lr }
  -- add all those RETS to "current_returns new_entry"
  modify $ \lr -> lr { current_returns = IM.insert (toInt new_entry) new_rets $ current_returns lr }
  -- update fmap: all instructions in reach now belong to new_entry
  modify $ \lr -> lr { current_fmap = IS.foldr (\k -> IM.insert k new_entry) (current_fmap lr) reach }
  -- update callers: add the instruction(s) that had a normal edge to new_entry be callers
  modify $ \lr -> lr { current_callers = IM.insertWith IS.union (toInt a) parents $ current_callers lr }
  -- Register inlining
  modify $ register_inlining new_entry encompassing_function
 
  forM_ (IS.toList reach) $ \a -> do
    Just i <- fetch $ InstructionAddress a
    nxt <- next_rips i
    case nxt of
      NxtAddresses f as -> do
        let escapes = IS.difference (IS.fromList $ map toInt $ S.toList as) reach 
        when (not $ IS.null escapes) $ do 
          xDebug 0 $ "Normal control flow from 0x" ++ showHex a ++ " to " ++ showHex_set escapes ++ " causing inlining of " ++ showHex_set escapes ++ " in " ++ show new_entry
          mapM_ (\escape -> modify $ register_inlining (FunctionEntry escape) new_entry) $ IS.toList escapes
      _ -> return ()
 where
  isRET a = do
    Just i <- fetch $ InstructionAddress a
    instructionIsReturn i

  modify_bag_elt reach old_entry new_entry elt@(a,entry)
    | toInt a `IS.member` reach  = (a,new_entry)
    | toInt a == toInt new_entry = (a,new_entry)
    | toInt entry `IS.member` reach = error "TODO"
    | otherwise = elt

  show_ndedata fmap old_entry (Unfinished v) = showHex v ++ " ..."
  show_ndedata fmap old_entry (Vertices vs)  = showHex_list vs




    
-- Explore a RET
-- Register the RET for the current function entry.
-- Trigger all CALLs to the current function entry, which will cause all its registered RETs to be explored.
explore_return :: BinaryClass bin => InstructionAddress -> XLifting bin ()
explore_return a = do
  Just entry <- get_entry_of_instruction a
  xDebug 0 $ "Registering RET at " ++ show a ++ " for entry " ++ show entry
  modify $ register_return entry a
  trigger_call_to_entry entry


get_encompassing :: BinaryClass bin => FunctionEntry -> XLifting bin (S.Set FunctionEntry)
get_encompassing entry = do
  inlining  <- gets current_inlining
  let parents = xgraph_parents inlining (toInt entry)
  return $ S.fromList $ map FunctionEntry $ IS.toList parents


trigger_call_to_entry :: BinaryClass bin => FunctionEntry -> XLifting bin () 
trigger_call_to_entry entry0 = do
  inlined_in   <- (S.toList . S.insert entry0) <$> get_inlining entry0
  encompassing <- (S.toList . S.insert entry0) <$> get_encompassing entry0

  rets       <- S.unions <$> mapM get_returns encompassing

  xDebug 1 $ "Call to " ++ show entry0 ++ " triggers RETs at " ++ show rets ++ " from " ++ show encompassing ++ " with callers from " ++ show inlined_in
  edges      <- S.unions <$> mapM (get_edges_from_returns_to_next_of_callers rets) inlined_in
  mapM_ (\(ret,next_of_ret) -> add_next_address ret next_of_ret) edges
 where
  -- trigger the call to an entry
  get_edges_from_returns_to_next_of_callers rets entry = do
    callers  <- get_callers entry
    edges    <- evalStateT (concatMapM (trigger entry) $ S.toList $ S.cartesianProduct rets callers) $ S.singleton entry
    return $ S.fromList edges



  -- trigger entry (return,caller)
  -- "return" is the address of a RET instruction reachable from "entry"
  -- "caller" is the address of a caller with as target "entry"
  trigger :: BinaryClass bin => FunctionEntry -> (InstructionAddress,InstructionAddress) -> StateT (S.Set FunctionEntry) (XLifting bin) [(InstructionAddress,InstructionAddress)]
  trigger entry (address_of_return,address_of_caller) = do
    Just instr_of_caller <- lift $ fetch address_of_caller
    Just entry_of_caller <- lift $ get_entry_of_instruction address_of_caller
    let op                = inOperation instr_of_caller 
    is_call_to_entry     <- lift $ is_call_to_entry instr_of_caller $ cast_entry_to_address entry

    if is_call_to_entry then do
      -- If "caller" is indeed a call to the entry, then the RET goes to the instruction right after "caller"
      let next_of_caller = instruction_next_address instr_of_caller
      xDebug 0 $ "Call to " ++ show entry0 ++ " triggers RET at " ++ show address_of_return ++ " which returns to " ++ show next_of_caller
      old_entry <- lift $ get_entry_of_instruction next_of_caller
      if (old_entry == Nothing || old_entry == Just entry_of_caller) then
        -- Register that the next instruction belongs to the same functions as "caller"
        lift $ add_instruction_to_fmap entry_of_caller next_of_caller
      else 
         xDebug 0 $ "Unexpected control flow (function crossing): " ++ show next_of_caller  ++ " is from entry " ++ show (fromJust old_entry) ++ " but " ++ show address_of_caller ++ " belongs to " ++ show entry_of_caller
      -- We have control flow from "return" to "next" of "caller"
      return [(address_of_return, next_of_caller)]
    else do
      -- "entry" was not entered not by a CALL. This typically happens when a JMP jumps to a function entry.
      -- It may also happen that the function was not called but entered via the instruction just before the entry.
      -- In both cases, the RET should return to the callers of the entry of the instruction that entered the function.
      new_callers <- lift $ get_callers entry_of_caller
      if isJump op then do
        xDebug 0 $ "Call to " ++ show entry0 ++ " triggers RET at " ++ show address_of_return ++ " whose entry was reached via " ++ show address_of_caller  ++ " from entry " ++ show entry_of_caller ++ " called by " ++ show (S.toList new_callers)
      else
        xDebug 0 $ "Unexpected control flow (function crossing): the entry of RET at " ++ show address_of_return ++ " was reached via " ++ show address_of_caller  ++ " from entry " ++ show entry_of_caller 

      visited <- get
      if (not $ entry_of_caller `S.member` visited) then do
        modify $ S.insert entry_of_caller
        -- Register "return" as a RET for the entry of the entering instruction
        lift $ modify $ register_return entry_of_caller address_of_return
        -- "return" is reachable from "entry_of_caller": we have "entry_of_caller" --> "caller" -->>> "return" (inductively)
        -- "new_caller" has as target "entry_of_caller"
        concatMapM (\new_caller -> trigger entry_of_caller (address_of_return, new_caller)) $ S.toList $ new_callers
      else
        -- TODO check the else-case
        return []

  -- is the instruction a CALL to the address?
  is_call_to_entry i a
    | isCall (inOperation i) = do
      nxt <- next_rips i
      case nxt of
        NxtInternalCall trgt -> return $ a == trgt
        _ -> return False
    | otherwise = return False
    




get_function_reach :: BinaryClass bin => InstructionAddress -> FunctionEntry -> XLifting bin IS.IntSet
get_function_reach a encompassing_function = do
  find_entry <- get_entry_of_instruction a
  case find_entry of
    Nothing -> return IS.empty
    Just entry -> get_function_reach_from entry
 where
  -- Get the reach through a dfs witihn the current function entry. Stop at any RET belonging to the current entry.
  get_function_reach_from :: BinaryClass bin => FunctionEntry -> XLifting bin IS.IntSet
  get_function_reach_from entry = do
    g     <- gets current_cfg
    reach <- dfs (snd <.> get_next_collapsed_calls (S.singleton entry)) (is_RET_in_entry entry) is_encompassing_or_covered a 
    return reach

  dfs next stopAtIncl stopAtExcl a = do
    stop <- stopAtExcl $ toInt a
    if stop then 
      return IS.empty
    else
      dfs' next stopAtIncl stopAtExcl IS.empty $ IS.singleton $ toInt a

  dfs' :: (InstructionAddress -> XLifting bin (S.Set InstructionAddress)) -> (Int -> XLifting bin Bool) -> (Int -> XLifting bin Bool) -> IS.IntSet -> IS.IntSet -> XLifting bin IS.IntSet
  dfs' next stopAtIncl stopAtExcl visited frontier
    | IS.null frontier = return visited
    | otherwise = do
      let visited'  = visited `IS.union` frontier
      frontier'    <- filterM (not <.> stopAtIncl) $ IS.toList frontier
      neighbors0   <- IS.unions <$> map (fromIntSet . S.map toInt) <$> mapM (next . InstructionAddress) frontier'
      neighbors    <- filterM (not <.> stopAtExcl) $ IS.toList neighbors0
      let frontier' =  IS.fromList neighbors `IS.difference` visited'
      dfs' next stopAtIncl stopAtExcl visited' frontier'

  -- Stop the dfs at a RET from the current entry
  -- Note that a JUMP to a function entry is also a RET
  is_RET_in_entry entry a = do
    instrs <- gets current_instrs
    fmap <- gets current_fmap 
    case (IM.lookup a instrs,IM.lookup a fmap) of
      (Just i,Just entry0) -> do
        if entry0 == entry then do
          instructionIsReturn i
        else
          return False
      _ -> return False

  -- Stop the dfs when we hit an instruction covered by a region in an ehframe section, or
  -- when we hit the encompassing function entry again
  is_encompassing_or_covered :: BinaryClass bin => Int -> XLifting bin Bool
  is_encompassing_or_covered a
    | FunctionEntry a == encompassing_function = return True
    | otherwise = do
      find_table <- find_ehframe_table_covering_address $ InstructionAddress a
      case find_table  of
        Just t  -> return $ True 
        Nothing -> return $ False






find_ehframe_table_covering_address :: BinaryClass bin => InstructionAddress -> XLifting bin (Maybe GCC_Except_Table)
find_ehframe_table_covering_address a = do
  (bin,_) <- ask
  case get_gcc_except_table_covering_address (binary_get_ehframe_covering bin) $ toInt a of
    Just k -> return $ Just $ (cfi_gcc_except_tables $ binary_get_cfi bin) IM.! k
    Nothing -> return Nothing
