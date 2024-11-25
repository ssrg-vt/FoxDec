{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StrictData #-}


module Algorithm.L0_Lifting (
  lift_to_L0
 ) where




import Base
import Config

import Analysis.Context
import Analysis.FunctionNames
import Analysis.ControlFlow
import Analysis.Pointers

import Data.Pred

import Algorithm.SCC

import Generic.Binary
import Generic.SymbolicPropagation
import Instantiation.SymbolicPropagation
import Instantiation.MachineState

import Data.JumpTarget
import Data.SymbolicExpression

import Parser.ParserIndirections

import OutputGeneration.Metrics
import OutputGeneration.CallGraph

-- import Pass.ACode_Gen


import X86.Conventions
import X86.Register (Register(..))
import X86.Opcode
import X86.Instruction
import qualified Generic.Instruction as Instr
import Generic.Operand (GenericOperand(..))

import Numeric (readHex)
import Control.Monad.State.Strict
import Control.Monad (filterM, when, forM)
import Control.Monad.Extra (whenM)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List (intercalate,delete,isInfixOf,partition,nub)
import Data.Foldable
import qualified Data.Serialize as Cereal hiding (get,put)
import qualified Data.ByteString as BS (readFile,writeFile) 
import Data.IORef

import Debug.Trace
import System.IO
import System.Process (callCommand)
import System.Timeout (timeout)
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Exit (die)
import Data.Functor.Identity

import System.IO.Unsafe (unsafePerformIO)










to_out_r = liftIO . putStrLn
to_out   = whenM (gets ctxt_verbose) . to_out_r
to_log log s = liftIO $ appendFile log $ s ++ "\n"


-- | The main algorithm for lifting a binary to an L0 representation (i.e., a Hoare Graph)
lift_to_L0 :: StateT Context IO ()
lift_to_L0 = do
  -- 1.) read entry points, clear .indirections file
  ctxt_read_and_set_entries
  ctxt_init_indirections

  -- 2.) repeatedly, do 2.1 -> 2.3
  repeat
 where
  -- 2.)
  repeat = do
    reconsider <- ctxt_reconsider_mutual_recursive_call
    case reconsider of
      Just (entry,trgts) -> do
        to_out $ "\n\nReconsidering (due to mutual recursion) entry " ++ showHex entry ++ " as all entries " ++ showHex_set trgts ++ " are now done."
        ctxt <- get
        modify $ set_ctxt_recursions (IM.delete entry $ ctxt_recursions ctxt)
        modify $ set_ctxt_calls (IM.delete entry $ ctxt_calls ctxt)
        modify $ set_ctxt_entries (graph_add_edges (ctxt_entries ctxt) entry IS.empty)
        repeat
      Nothing -> do
        entries <- gets ctxt_entries
        case graph_find_next entries of
          Nothing -> ctxt_finish_repeat
          Just a  -> ctxt_run_entry a

  ctxt_finish_repeat = do
    to_out $ "Done!"





  -- 2.1) consider the current entry address $a$
  ctxt_run_entry a = do
    -- 2.2) Start analysis at entry-address $a$
    calls <- gets ctxt_calls
    has_instruction <- gets (flip address_has_instruction $ a)
    case (IM.lookup a calls, has_instruction)  of
      (Just _,_) -> do
        -- 2.2.1) Entry is already done, delete it and continue
        ctxt_del_entry a
        repeat
      (_,False)  -> do
        to_out $ "\n\nEntry " ++ showHex a ++ " ignored."
        ctxt_del_entry a
        repeat
      _ -> do
        to_out $ "\n\nEntry " ++ showHex a
        -- 2.2.3) Entry is new, generate a CFG
        curr_g <- gets (IM.lookup a . ctxt_cfgs)
        (curr_finit,curr_invs,curr_posts,curr_sps) <- ctxt_get_curr_posts a


        to_out $ "Entry " ++ showHex a ++ ": starting CFG generation."
        ctxt_generate_cfg a
        ctxt_generate_invs a curr_invs curr_posts curr_sps

        g <- gets (IM.lookup a . ctxt_cfgs)
        (finit,invs,posts,sps) <- ctxt_get_curr_posts a
        let do_repeat = curr_g /= g || curr_finit /= finit

        new_calls <- ctxt_get_new_calls a
        if new_calls == [] then do
          to_out $ "Entry " ++ showHex a ++ ": no new entries."
          ctxt_after_invgen a do_repeat 
        else do
          mapM_ (ctxt_add_entry_edge a) new_calls
          to_out $ "Entry " ++ showHex a ++ ": Adding new internal functions at " ++ showHex_list (map fst new_calls) ++ " to graph of entries."
          repeat

  -- 2.3) Analyze a CFG, either needing a new round (in case of newly resolved indirections) are not.
  ctxt_after_invgen entry do_repeat = do
    -- analyze unresolved jumps
    continue <- ctxt_analyze_unresolved_indirections entry
    if continue then do
      -- if new jumps where resolved, repeat with current function
      to_out $ "Entry " ++ showHex entry ++ ": Continuing to next round as there are newly resolved indirections."
      repeat
    else if do_repeat then do
      -- if new invariants were found, repeat with current function
      to_out $ "Entry " ++ showHex entry ++ ": Continuing to next round as CFG or invariants where refined."
      repeat
    else do
      -- if no new jumps where resolved, this function is done: store entry address as an analyzed call and pop the entry-stack
      to_out $ "Entry " ++ showHex entry ++ ": Terminating as no new indirections are resolved."
      verified <- ctxt_verify_proper_return entry
      to_out $ "Entry " ++ showHex entry ++ ": Verified == " ++ show verified

      ctxt_add_call entry verified
      ctxt_del_entry entry
      ctxt_add_to_results entry verified
      ctxt_get_recursive_calls entry
      repeat





ctxt_get_recursive_calls :: Int -> StateT Context IO ()
ctxt_get_recursive_calls entry = do
  ctxt <- get
  let g = ctxt_cfgs ctxt IM.! entry
  let recursions = IS.fromList $ catMaybes $ concat $ concatMap (get_new_calls ctxt) $ IM.elems $ cfg_instrs g
  when (not $ IS.null recursions) $ do
    modify $ set_ctxt_recursions (IM.insertWith IS.union entry recursions (ctxt_recursions ctxt))
    to_out $ "Entry " ++ showHex entry ++ " is part of mutual recursion and treated entries " ++ showHex_set recursions ++ " as external. It will be reconsidered after those entries have been done."
 where
  get_new_calls ctxt is = map (get_new_call ctxt) is
  get_new_call  ctxt i =
    if isCall (Instr.opcode i) then
      map (when_trgt_is_not_done_yet ctxt) $ resolve_jump_target ctxt i
    else
      []

  when_trgt_is_not_done_yet ctxt (ImmediateAddress trgt) =
    case (IM.lookup (fromIntegral trgt) $ ctxt_calls ctxt) of
      Nothing -> Just $ fromIntegral trgt
      Just _  -> Nothing
  when_trgt_is_not_done_yet ctxt _ = Nothing


ctxt_reconsider_mutual_recursive_call :: StateT Context IO (Maybe (Int,IS.IntSet))
ctxt_reconsider_mutual_recursive_call = do
  ctxt <- get
  recursions <- gets ctxt_recursions
  return $ find (to_be_reconsidered ctxt) $ IM.toList recursions
 where
  to_be_reconsidered ctxt (entry,trgts) = all (\trgt -> IM.lookup trgt (ctxt_calls ctxt) /= Nothing) $ IS.toList trgts






ctxt_get_new_calls entry = do
  ctxt   <- get
  fctxt  <- ctxt_mk_fcontext entry
  let g   = ctxt_cfgs ctxt IM.! entry

  return $ gather fctxt $ catMaybes $ concat $ concatMap (get_new_calls fctxt) $ IM.elems $ cfg_instrs g
 where
  get_new_calls fctxt is = map (get_new_call fctxt) is
  get_new_call  fctxt i =
    if isCall (Instr.opcode i) || (isJump (Instr.opcode i) && jump_is_actually_a_call (f_ctxt fctxt) i) then
       map (trgt_to_finit fctxt i) $ resolve_jump_target (f_ctxt fctxt) i
    else
      []

  trgt_to_finit fctxt i (ImmediateAddress trgt) =
   let ctxt = f_ctxt fctxt in
    case (IM.lookup (fromIntegral trgt) $ ctxt_calls ctxt, IM.lookup (fromIntegral trgt) $ ctxt_finits ctxt, get_invariant fctxt $ fromIntegral $ addressof i) of
      (Nothing,Nothing,Just inv) -> Just $ (fromIntegral trgt,invariant_to_finit fctxt inv)
      (_,Just finit,Just inv)    -> do
        let finit' = join_finit fctxt (invariant_to_finit fctxt inv) finit
        if finit /= finit' || (graph_is_vertex (ctxt_entries ctxt) (fromIntegral trgt) && not (graph_is_edge (ctxt_entries ctxt) entry (fromIntegral trgt))) then
          return (fromIntegral trgt,finit') -- trace ("new finit@" ++ showHex trgt ++ "\n" ++ show finit ++ "\n" ++ show (invariant_to_finit fctxt inv) ++ "\n" ++ show finit') 
        else
          Nothing
      (_,_,Nothing) -> Nothing -- time out
      (x,y,z) -> error (show (x,y,z))
  trgt_to_finit ctxt i _ = Nothing

  gather fctxt []         = []
  gather fctxt [new_call] = [new_call]
  gather fctxt ((entry',finit):new_calls) =
    let (same,others) = partition ((==) entry' . fst) new_calls in
      (entry', foldr1 (join_finit fctxt) (finit:map snd same)) : gather fctxt others
    



ctxt_add_entry_edge :: Int -> (Int,FInit) -> StateT Context IO ()
ctxt_add_entry_edge a (trgt,finit) = do
  ctxt <- get
  modify $ set_ctxt_entries (graph_add_edges (ctxt_entries ctxt) a (IS.singleton trgt))
  modify $ set_ctxt_finits  (IM.insert trgt finit $ ctxt_finits ctxt)
  modify $ set_ctxt_calls   (IM.delete trgt $ ctxt_calls ctxt)
  modify $ set_ctxt_invs    (IM.delete trgt $ ctxt_invs  ctxt)
  modify $ set_ctxt_posts   (IM.delete trgt $ ctxt_posts ctxt)


ctxt_get_curr_posts :: Int -> StateT Context IO (FInit,Invariants,S.Set (NodeInfo,Predicate),S.Set StatePart)
ctxt_get_curr_posts entry = do
  finits <- gets ctxt_finits
  invs   <- gets ctxt_invs
  posts  <- gets ctxt_posts
  sps    <- gets ctxt_stateparts
  return (IM.lookup entry finits `orElse` init_finit, IM.lookup entry invs `orElse` IM.empty, IM.lookup entry posts `orElse` S.empty,IM.lookup entry sps `orElse` S.empty)




-- Assuming a CFG and invariants are generated, verify for each block $b$ that is an end-node in the CFG whether:
-- if the end-node "returns normally", i.e., ends in a RET, then it returns correctly, i.e., the return address is 
-- not overwritten and callee-saved registers are preserved and RSP is set to RSP0+8
ctxt_verify_proper_return :: Int -> StateT Context IO VerificationResult
ctxt_verify_proper_return entry = do
  ctxt            <- get
  fctxt           <- ctxt_mk_fcontext entry
  let vcs          = IM.lookup entry (ctxt_vcs ctxt) `orElse` S.empty
  (_,_,posts,_)   <- ctxt_get_curr_posts entry
  all_checks      <- mapM (correct fctxt) $ filter (\(ni,q) -> ni == Normal) $ S.toList posts

  if S.null posts then
    return (VerificationError "time out reached")
  else if any (\(ni,q) -> ni == UnresolvedIndirection) posts then do
    return VerificationUnresolvedIndirection
  else if any ((/=) Nothing) $ concat all_checks then do
    return (VerificationError $ intercalate "\n" $ catMaybes $ concat all_checks)
  else if any (not . is_precondition) vcs then do
    return VerificationSuccesWithAssumptions
  else
    return VerificationSuccess
 where
  -- is the postcondition "correct"? 
  -- A succesfull check returns Nothing, a verification error produces a "Just err" with an error-message
  correct :: FContext -> (NodeInfo,Predicate) -> StateT Context IO [Maybe String]
  correct fctxt (_,q) = do
    return $ runIdentity $ evalStateT (do_post_check fctxt) q


  -- do one more tau-transformation on the node, as the stored invariant is a 
  -- precondition (not a postcondition) of the node.
  do_post_check :: FContext -> State Predicate [Maybe String]
  do_post_check fctxt = do
    let f   = function_name_of_entry (f_ctxt fctxt) (f_entry fctxt)
    rsp    <- read_reg fctxt RSP
    rip    <- read_reg fctxt RIP
    checks <- return [] --forM (RSP callee_saved_registers) (\r -> read_reg fctxt r >>= return . reg_check fctxt r) 
    s  <- get
    return $ [rsp_check fctxt f rsp, rip_check fctxt s f rip] ++ checks
    -- return $ []

  -- check: are all caller-saved-registers restored to their initial values?
  reg_check fctxt reg v =
    case try_deterministic fctxt v of
      -- REG == REG_0
      Just (SE_Var (SP_Reg reg)) -> Nothing
      _ -> Just $ "Verification error: " ++ show reg ++ " == " ++ show v

  -- check: is RSP restored to RSP_0 + 8 ?
  rsp_check fctxt f rsp = 
    case try_deterministic fctxt rsp of
      -- RSP == RSP_0 + 8
      Just (SE_Op Plus _ [SE_Var (SP_StackPointer f'), SE_Immediate 0x8]) -> if f == f' then Nothing else v_error rsp
      -- RSP == RSP_0 - 0xfffffffffffffff8
      Just (SE_Op Minus _ [SE_Var (SP_StackPointer f'), SE_Immediate 0xfffffffffffffff8]) -> if f == f' then Nothing else v_error rsp
      _ -> v_error rsp
  v_error rsp = Just $ "Verification error: RSP == " ++ show rsp

  -- check: is RIP set to the value originally stored at the top of the stack frame?
  rip_check fctxt  s f rip = 
    case try_deterministic fctxt rip of
      -- RIP == *[RSP_0,8]
      Just (SE_Var (SP_Mem (SE_Var (SP_StackPointer f)) 8)) -> Nothing
      e -> Just $ "Verification error: RIP == " ++ show rip ++ "\nState: " ++ show s 



-- After a succesfull effort, store the current CFG, the invariants,...,  in the "report"-field of the context
ctxt_add_to_results entry verified = do
  results <- gets ctxt_results
  modify $ set_ctxt_results (IM.insert entry verified results)

  ctxt     <- get
  base     <- ctxt_base_name entry
  let invs  = ctxt_invs ctxt IM.! entry
  let g     = ctxt_cfgs ctxt IM.! entry
  let ret   = ctxt_calls ctxt IM.! entry
  let finit = IM.lookup entry $ ctxt_finits ctxt
  let vcs   = IM.lookup entry (ctxt_vcs ctxt) `orElse` S.empty
  let log   = base ++ ".log"

  liftIO $ writeFile log ""

  to_log log $ "Function entry:             " ++ showHex entry
  to_log log $ "Verification result:        " ++ show_verification_result verified
  to_log log $ "#instructions:              " ++ show (num_of_instructions g)
  to_log log $ "#assertions:                " ++ show (count_instructions_with_assertions vcs)
  to_log log $ "#unresolved indirect jumps: " ++ show (num_of_unres_inds_in_cfg ctxt isJump g)
  to_log log $ "#unresolved indirect calls: " ++ show (num_of_unres_inds_in_cfg ctxt isCall g)
  to_log log $ "#blocks:                    " ++ show (num_of_blocks       g)
  to_log log $ "#edges:                     " ++ show (num_of_edges        g)
  to_log log $ ""

  to_log log $ summarize_finit finit
  to_log log $ ""

  to_log log $ "Return behavior: " ++ show_return_behavior ret  

  --to_log log $ summarize_preconditions_long ctxt vcs -- TODO make configurable
  --to_log log $ summarize_assertions_long ctxt vcs
  to_log log $ summarize_function_constraints_long ctxt vcs
  to_log log $ summarize_function_pointers ctxt vcs


  when (ctxt_verbose_logs ctxt) $ do
    to_log log $ show_invariants g invs
 where
  show_return_behavior (ReturningWith p)  = "returning with\n" ++ show p ++ "\n\n"
  show_return_behavior Terminating        = "terminating"
  show_return_behavior UnknownRetBehavior = "unknown"

  show_verification_result (VerificationError msg)           = "\n\n\n---------------------\nVerification error!!!\n---------------------\n" ++ msg
  show_verification_result VerificationSuccesWithAssumptions = "Verified (with assumptions)"
  show_verification_result VerificationSuccess               = "Verified"
  show_verification_result VerificationUnresolvedIndirection = "Unresolved indirections"
  show_verification_result Unverified                        = "UNVERIFIED"

















-- Read entry from file producing entries :: [Int]
ctxt_read_entries :: StateT Context IO [Int]
ctxt_read_entries = do
  ctxt <- get
  let dirname = ctxt_dirname ctxt
  let name    = ctxt_name ctxt
  let fname   = dirname ++ name ++ ".entry" 

  liftIO $ parse $! fname
 where
  parse filename = do
    ls <- readFile filename
    return $ map read_line $ lines ls
  read_line = readHex' . tail . tail





-- Read entry from file producing entry :: Int
ctxt_read_and_set_entries :: StateT Context IO ()
ctxt_read_and_set_entries = do
  entries <- ctxt_read_entries
  read_arrays <- ctxt_read_init_fini_arrays
  let extra_ptrs = map fromIntegral $ filter ((/=) 0) $ filter ((/=) (0-1)) $ concat read_arrays
  modify $ set_ctxt_entries (Edges $ IM.fromList (zip (entries ++ extra_ptrs) $ repeat IS.empty))
  modify $ set_ctxt_start (head entries)
 where
  ctxt_read_init_fini_arrays = gets (si_sections . ctxt_sections) >>= mapM ctxt_read_init_fini_array 

  ctxt_read_init_fini_array ("", ".init_array",a0,si,_) = read_pointers_from_ro_data_section a0 si
  ctxt_read_init_fini_array ("", ".fini_array",a0,si,_) = read_pointers_from_ro_data_section a0 si
  ctxt_read_init_fini_array _                           = return []

  read_pointers_from_ro_data_section a0 si =
    if si < 8 then
      return []
    else do
      ctxt <- get
      let ptr = read_from_ro_datasection ctxt a0 (fromIntegral si)
      ptrs <- read_pointers_from_ro_data_section (a0+8) (si-8)
      return $ (fromJust ptr):ptrs
      




-- Produce a base file name (without .extension) based on the current entry under consideration
-- If necessary, make the directory
ctxt_base_name :: Int -> StateT Context IO String
ctxt_base_name entry = do
  dirname  <- gets ctxt_dirname
  name     <- gets ctxt_name

  let functions_dirname = dirname ++ "functions/"
  liftIO $ createDirectoryIfMissing False functions_dirname      
  let entry_dirname = functions_dirname ++ showHex entry ++ "/"
  liftIO $ createDirectoryIfMissing False entry_dirname      
  return $ entry_dirname ++ name

-- For the current entry under consideration, read indirections producing inds :: IM.IntMap IS.IntSet
-- Per address a set of next addresses (jump targets).
ctxt_init_indirections :: StateT Context IO ()
ctxt_init_indirections = do
  dirname   <- gets ctxt_dirname
  name      <- gets ctxt_name
  let fname  = dirname ++ name ++ ".indirections" 

  --exists <- liftIO $ doesFileExist fname
  --if exists then do
  --  inds <- liftIO $ parse fname
  --  ctxt <- get
  --  put $ ctxt { ctxt_inds = inds }
  --else do
  liftIO $ writeFile fname ""
  modify $ set_ctxt_inds IM.empty
 where
  parse fname = do
    ret0 <- parse_indirections fname
    case ret0 of
      Left err   -> error $ show err
      Right inds -> return inds





-- TODO generate dot only once
ctxt_generate_cfg :: Int -> StateT Context IO (S.Set (Instruction,Int))
ctxt_generate_cfg entry = do
  -- Generate a CFG, write dot file if no new calls discovered
  ctxt          <- get
  base          <- ctxt_base_name entry
  let log        = base ++ ".log"
  let fname      = base ++ ".dot"
  let pdf        = base ++ ".pdf"
  let do_pdfs    = ctxt_generate_pdfs ctxt
  (new_calls,g) <- liftIO $ cfg_gen ctxt get_invariant entry

  cfgs <- gets ctxt_cfgs
  modify (set_ctxt_cfgs $ IM.insert entry g cfgs)

  if S.null new_calls then do
    liftIO $ writeFile fname $ cfg_to_dot ctxt g
    if do_pdfs then do
      liftIO $ callCommand $ "dot -Tpdf " ++ fname ++ " -o " ++ pdf
      to_out $ "Generated CFG, exported to files: " ++ fname ++ " and " ++ pdf
    else do
      to_out $ "Generated CFG, exported to file: " ++ fname 
    return new_calls
  else
    return new_calls

     


ctxt_add_invariants entry finit invs posts vcs sps = do
  all_invs   <- gets ctxt_invs
  all_posts  <- gets ctxt_posts
  all_vcs    <- gets ctxt_vcs
  all_finits <- gets ctxt_finits
  all_sps    <- gets ctxt_stateparts

  modify (set_ctxt_invs       $ IM.insert entry invs all_invs)
  modify (set_ctxt_posts      $ IM.insert entry posts all_posts)
  modify (set_ctxt_vcs        $ IM.insert entry vcs all_vcs)
  modify (set_ctxt_finits     $ IM.insert entry finit all_finits)
  modify (set_ctxt_stateparts $ IM.insert entry sps all_sps)




ctxt_mk_fcontext :: Int -> StateT Context IO FContext
ctxt_mk_fcontext entry = do
  ctxt <- get
  return $ mk_fcontext ctxt entry

ctxt_generate_invs :: Int -> Invariants -> S.Set (NodeInfo,Predicate) -> S.Set StatePart -> StateT Context IO ()
ctxt_generate_invs entry curr_invs curr_posts curr_sps = do
  -- Generate invariants
  ctxt  <- get
  let get_max_time         = ctxt_max_time ctxt
  let get_max_time_minutes = get_max_time `div` 60000000

  fctxt    <- ctxt_mk_fcontext entry
  let finit = f_init fctxt
  let f     = function_name_of_entry (f_ctxt fctxt) (f_entry fctxt)

  
  -- when (finit /= init_finit) $ to_out $ "Function initialisation: " ++ show finit

  -- let a       = acode_simp $ cfg_to_acode g 0 IS.empty
  g          <- gets (fromJust . IM.lookup entry . ctxt_cfgs)
  let p       = init_pred fctxt curr_invs curr_posts 

  result  <- liftIO (timeout get_max_time $ return $! do_prop fctxt g 0 p) -- TODO always 0?

  case result of
    Nothing         -> do
      to_out $ "Entry " ++ showHex entry ++ ": time out of " ++ show get_max_time_minutes ++ " minutes reached."
      ctxt_add_invariants entry finit IM.empty S.empty S.empty S.empty
    Just (invs,vcs) -> do
      let blocks  = IM.keys $ cfg_blocks g
      posts     <- S.fromList <$> catMaybes <$> mapM (get_post fctxt g invs) blocks
      ctxt_add_invariants entry finit invs posts vcs (S.union curr_sps $ gather_stateparts invs posts)
 where
  get_post :: FContext -> CFG -> Invariants -> Int -> StateT Context IO (Maybe ((NodeInfo,Predicate)))
  get_post fctxt g invs b = do
    let ctxt = f_ctxt fctxt
    if is_end_node g b then do
      let q = execState (do_tau fctxt g b) (im_lookup ("B.) Block " ++ show b ++ " in invs") invs b)
      return $ Just (node_info_of ctxt g b,q)
    else
      return $ Nothing

  -- do one more tau-transformation on the node, as the stored invariant is a 
  -- precondition (not a postcondition) of the node.
  do_tau :: FContext -> CFG -> Int -> State Predicate ()
  do_tau fctxt g b = do
    (s',_) <- gets (tau fctxt (fetch_block g b) Nothing)
    put s'







ctxt_del_entry :: Int -> StateT Context IO ()
ctxt_del_entry entry = do
  ctxt <- get
  modify $ set_ctxt_entries $ graph_delete (ctxt_entries ctxt) entry

ctxt_add_call entry verified = do
  ctxt <- get
  fctxt <- ctxt_mk_fcontext entry
  let calls       = ctxt_calls ctxt
  let dirname     = ctxt_dirname ctxt
  let name        = ctxt_name ctxt
  let fname       = dirname ++ name ++ ".calls"

  (_,invs,posts,_) <- ctxt_get_curr_posts entry
  
  (ret,msg) <-
    if (any ((==) UnresolvedIndirection) $ S.map fst posts) || verified `notElem` [VerificationSuccesWithAssumptions,VerificationSuccess] then
      return (UnknownRetBehavior, "unknown.")
    else if all ((==) Terminal) $ S.map fst posts then
      return (Terminating, "always terminating.")
    else let q = supremum fctxt $ map snd $ S.toList $ S.filter ((==) Normal . fst) $ posts in
      return (ReturningWith q, "normally returning.")

  modify $ set_ctxt_calls (IM.insert entry ret $ calls)
  to_out $ "Function at entry: " ++ showHex entry ++ " analyzed, return behavior is " ++ msg






ctxt_analyze_unresolved_indirections :: Int -> StateT Context IO Bool
ctxt_analyze_unresolved_indirections entry = do
  ctxt <- get

  (finit,invs,posts,_) <- ctxt_get_curr_posts entry
  let f        = function_name_of_entry ctxt entry
  let g        = ctxt_cfgs ctxt IM.! entry

  let bs = filter (\b -> node_info_of ctxt g b == UnresolvedIndirection) $ IM.keys $ cfg_blocks g
  if bs == [] then do
    to_out $ "No unresolved indirections."
    return False
  else if IM.null invs then do
    -- time out
    return False
  else do
    results <- forM bs $ try_to_resolve_indirection f g invs
    return $ not $ all not results -- prevent lazy execution
 where
  try_to_resolve_indirection f g invs b = do
    ctxt <- get
    fctxt <- ctxt_mk_fcontext entry
    max_tries <- gets ctxt_max_jump_table_size

    dirname   <- gets ctxt_dirname
    name      <- gets ctxt_name
    let fname  = dirname ++ name ++ ".indirections" 

    let i                 = last (fetch_block g b)
    let [trgt]            = Instr.srcs i
    let p@(Predicate _ flgs) = im_lookup ("A.) Block " ++ show b ++ " in invs") invs b


    case flagstatus_to_tries max_tries flgs of
      Nothing -> try_to_resolve_from_invariant fname f g b p i trgt
      Just (op1,n) -> do
        let values1 = map (\n -> evalState (try fctxt f (addressof i) g b op1 trgt n) (clean_sstate fctxt p)) [0..n]
        if values1 == [] || any ((==) Nothing) values1 then
          try_to_resolve_from_invariant fname f g b p i trgt
        else if all is_jump_table_entry values1 then do
          let trgts = map (fromIntegral . get_immediate . S.findMin . fromJust) values1
          to_out $ "Resolved indirection at block " ++ show b
          to_out $ "Instruction = " ++ show i
          to_out $ "Operand " ++ show trgt ++ " evaluates to: " ++ showHex_list (nub trgts)
          to_out $ "Because of bounded jump table access: " ++ show flgs
          to_out $ "Updated file: " ++ fname

          inds <- gets ctxt_inds

          let tbl = JumpTable op1 (fromIntegral n) trgt (IM.fromList $ zip [0..fromIntegral n] trgts)
          liftIO $ appendFile fname $ showHex (addressof i) ++ " " ++ show tbl ++ "\n"

          let inds' = IM.insert (fromIntegral $ addressof i) (Indirection_JumpTable tbl) inds -- TODO check if already exists
          modify $ set_ctxt_inds inds'

          return True
        else do
          to_out $ "Unresolved block " ++ show b ++ "\n" ++ show i ++ "\n" ++ show p ++ "\n" ++ show trgt
          return False
          -- error $ show ("resolving:",values1,p)


  try_to_resolve_from_invariant fname f g b p i trgt = do
    ctxt <- get
    fctxt <- ctxt_mk_fcontext entry
    let values0 = evalState (try' fctxt f g b trgt) p
    if values0 /= Nothing then do
      let value = fromJust values0
      to_out $ "Resolved indirection at block " ++ show b
      to_out $ "Instruction = " ++ show i
      to_out $ "Operand " ++ show trgt ++ " evaluates to: " ++ show value
      to_out $ "State:\n" ++ show p
      to_out $ "Updated file: " ++ fname
      liftIO $ appendFile fname $ showHex (addressof i) ++ " " ++ show (Indirection_Resolved value) ++ "\n"

      inds <- gets ctxt_inds
      let inds' = IM.insert (fromIntegral $ addressof i) (Indirection_Resolved value) inds -- TODO check if already exists
      modify $ set_ctxt_inds inds'
      return True
    else do
      to_out $ "Unresolved block " ++ show b ++ "\n" ++ show i ++ "\n" ++ show p ++ "\n" ++ show trgt
      return False


  flagstatus_to_tries max_tries (FS_CMP (Just True) op1 (Immediate n)) = if n <= fromIntegral max_tries then Just (op1,n) else Nothing
  flagstatus_to_tries max_tries _ = Nothing


  is_jump_table_entry Nothing      = False
  is_jump_table_entry (Just trgts) = S.size trgts == 1 && all is_immediate trgts
  is_immediate (ImmediateAddress _) = True
  is_immediate _                    = False
  get_immediate (ImmediateAddress a) = a

  -- write an immediate value to operand op1, then run symbolic exection to see if
  -- after executing a block the target-operand is an immediate value as well.
  try fctxt f i_a g blockId op1 trgt n = do
    write_operand fctxt op1 (SE_Immediate n)
    try' fctxt f g blockId trgt

  try' fctxt f g blockId trgt = do
    let ctxt   = f_ctxt fctxt
    let instrs = fetch_block g blockId
    (s',_) <- gets (tau_block fctxt (init instrs) Nothing)
    put s'
    set_rip fctxt (last instrs)
    val <- read_operand fctxt trgt
    case val of
      e@(SE_Immediate a)                 -> if expr_is_global_immediate ctxt e then return $ Just $ S.singleton $ ImmediateAddress a else return Nothing
      Bottom (FromNonDeterminism es)     -> return $ if all (expr_highly_likely_pointer fctxt) es then Just $ S.map (mk_resolved_jump_target ctxt) es else Nothing
      SE_Var (SP_Mem (SE_Immediate a) _) -> case find_external_symbol_for_address ctxt a of
                                              Just sym -> return $ Just $ S.singleton $ External sym 
                                              Nothing  -> return $ Nothing
      e                                  -> return $ Nothing

  mk_resolved_jump_target ctxt (SE_Immediate a)                     = ImmediateAddress a
  mk_resolved_jump_target ctxt (SE_Var (SP_Mem (SE_Immediate a) _)) = case find_external_symbol_for_address ctxt a of
                                                                        Just sym -> External sym 
                                                                        Nothing  -> error $ show ("indirections", showHex a)
  mk_resolved_jump_target ctxt a                                    = error $ show ("resolving", a)




  clean_sstate fctxt (Predicate p fs) = Predicate (M.filter (not . contains_bot) p) fs




