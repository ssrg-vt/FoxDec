{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StrictData #-}


module Algorithm.L0_Lifting where




import Base
import Config

import Analysis.Context
import Analysis.FunctionNames
import Analysis.ControlFlow

import Generic.Binary
import Generic.SymbolicPropagation
import Generic.SymbolicConstituents
import Instantiation.SymbolicPropagation

import Data.JumpTarget
import Data.SymbolicExpression

import Parser.ParserIndirections

import OutputGeneration.Metrics
import OutputGeneration.JSON
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
import Control.Monad (filterM)
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


-- the main algorithm
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
    dangling_fptrs <- ctxt_find_dangling_function_pointers
    to_out $ "Done!"
    when (dangling_fptrs /= []) $ do
      to_out_r $ ""
      to_out_r $ ""
      to_out_r $ "Dangling function pointers found, which may point to function entries currently not analyzed."
      to_out_r $ "These require manual analysis: if they correspond to real function entries, add them to the .entry file"
      to_out_r $ show_dangling_function_pointers dangling_fptrs




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
        let do_repeat = curr_g /= g || curr_sps /= sps -- TODO why sps, not posts?

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



ctxt_find_dangling_function_pointers :: StateT Context IO [(Int,Int)]
ctxt_find_dangling_function_pointers = do
  ctxt <- get
  let ptrs = concatMap mk_function_pointer (S.toList $ S.unions $ ctxt_vcs ctxt)
  filterM is_dangling ptrs
 where
  mk_function_pointer (FunctionPointers a ptrs) = map (pair (fromIntegral a)) $ IS.toList ptrs
  mk_function_pointer _                         = []

  is_dangling (a,ptr) = do
    calls <- gets ctxt_calls
    return $ IM.lookup ptr calls == Nothing
    
    
show_dangling_function_pointers [] = ""
show_dangling_function_pointers ((a,fptr):fptrs) = 
  let (match,remaining) = partition does_match fptrs in
    "Function pointer " ++ showHex fptr ++ " introduced at " ++ showHex_list (a : map fst  match) ++ "\n" ++ show_dangling_function_pointers remaining
 where
  does_match (_,fptr') = fptr == fptr'
  



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
    if isCall (Instr.opcode i) then
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
          return (fromIntegral trgt,finit') -- trace ("new finit@" ++ showHex trgt ++ "\n" ++ show finit ++ "\n" ++ show (invariant_to_finit ctxt finit_of_entry inv) ++ "\n" ++ show finit') $ 
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


ctxt_get_curr_posts :: Int -> StateT Context IO (FInit,Invariants,S.Set (NodeInfo,Predicate),S.Set SStatePart)
ctxt_get_curr_posts entry = do
  finits <- gets ctxt_finits
  invs   <- gets ctxt_invs
  posts  <- gets ctxt_posts
  sps    <- gets ctxt_stateparts
  return (IM.lookup entry finits `orElse` M.empty, IM.lookup entry invs `orElse` IM.empty, IM.lookup entry posts `orElse` S.empty,IM.lookup entry sps `orElse` S.empty)




-- Assuming a CFG and invariants are generated, verify for each block $b$ that is an end-node in the CFG whether:
-- if the end-node "returns normally", i.e., ends in a RET, then it returns correctly, i.e., the return address is 
-- not overwritten and callee-saved registers are preserved and RSP is set to RSP0+8
ctxt_verify_proper_return :: Int -> StateT Context IO VerificationResult
ctxt_verify_proper_return entry = do
  ctxt            <- get
  fctxt           <- ctxt_mk_fcontext entry
  let vcs          = IM.lookup entry (ctxt_vcs ctxt) `orElse` S.empty
  (_,_,posts,_)   <- ctxt_get_curr_posts entry
  all_checks      <- mapM (correct fctxt) $ filter (\(ni,q) -> ni /= Terminal) $ S.toList posts

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
    return $ runIdentity $ evalStateT (do_post_check fctxt) (q,S.empty) 


  -- do one more tau-transformation on the node, as the stored invariant is a 
  -- precondition (not a postcondition) of the node.
  do_post_check :: FContext -> State (Predicate,VCS) [Maybe String]
  do_post_check fctxt = do
    let f   = function_name_of_entry (f_ctxt fctxt) (f_entry fctxt)
    rsp    <- sread_reg fctxt RSP
    rip    <- sread_reg fctxt RIP
    checks <- return [] -- forM (delete RSP callee_saved_registers) (\r -> read_reg ctxt r >>= return . reg_check r) 
    (s,_)  <- get
    return $ [rsp_check fctxt f rsp, rip_check fctxt s f rip] ++ checks

  -- check: are all caller-saved-registers restored to their initial values?
  reg_check fctxt reg v =
    case stry_deterministic fctxt v of
      -- REG == REG_0
      Just (SE_Var (SP_Reg reg)) -> Nothing
      _ -> Just $ "Verification error: " ++ show reg ++ " == " ++ show v

  -- check: is RSP restored to RSP_0 + 8 ?
  rsp_check fctxt  f rsp = 
    case stry_deterministic fctxt rsp of
      -- RSP == RSP_0 + 8
      Just (SE_Op Plus _ [SE_Var (SP_StackPointer f'), SE_Immediate 0x8]) -> if f == f' then Nothing else v_error rsp
      -- RSP == RSP_0 - 0xfffffffffffffff8
      Just (SE_Op Minus _ [SE_Var (SP_StackPointer f'), SE_Immediate 0xfffffffffffffff8]) -> if f == f' then Nothing else v_error rsp
      _ -> v_error rsp
  v_error rsp = Just $ "Verification error: RSP == " ++ show rsp

  -- check: is RIP set to the value originally stored at the top of the stack frame?
  rip_check fctxt  s f rip = 
    case stry_deterministic fctxt rip of
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










{--

-- At the end, generate a report (.log file)
ctxt_generate_end_report :: StateT Context IO ()
ctxt_generate_end_report = do
  ctxt <- get
  let dirname     = ctxt_dirname ctxt
  let name        = ctxt_name ctxt
  let log         = dirname ++ name ++ ".log"

  liftIO $ writeFile log ""
  to_out $ "Writing log to file: " ++ log

  report_total ctxt log 
  mapM_ (report_per_entry ctxt log) $ IM.keys $ ctxt_calls ctxt
 where
  report_total ctxt log = do
    to_log log $ "Total"
    to_log log $ "#functions:                           " ++ show (IM.size $ ctxt_results ctxt)
    to_log log $ "    of which verified                 " ++ show (num_of_verified                              $ ctxt_results ctxt)
    to_log log $ "    of which verified with assertions " ++ show (num_of_verifiedw                             $ ctxt_results ctxt)
    to_log log $ "    of which verif_error              " ++ show (num_of_verif_error                           $ ctxt_results ctxt)
    to_log log $ "#instructions:                        " ++ show (sum_total num_of_instructions                $ ctxt_cfgs ctxt)
    to_log log $ "#assertions:                          " ++ show (sum_total count_instructions_with_assertions $ ctxt_vcs  ctxt)
    to_log log $ "#unresolved indirect jumps:           " ++ show (num_of_unres_inds ctxt isJump                $ ctxt_cfgs ctxt)
    to_log log $ "#unresolved indirect calls:           " ++ show (num_of_unres_inds ctxt isCall                $ ctxt_cfgs ctxt)
    to_log log $ "#blocks:                              " ++ show (sum_total num_of_blocks                      $ ctxt_cfgs ctxt)
    to_log log $ "#edges:                               " ++ show (sum_total num_of_edges                       $ ctxt_cfgs ctxt)
    to_log log $ "#resolved indirection jumps:          " ++ show (num_of_resolved_indirection_jumps ctxt)
    to_log log $ "#resolved indirection calls:          " ++ show (num_of_resolved_indirection_calls ctxt)

    to_log log $ "\n\n"
    to_log log $ summarize_function_pointers ctxt (S.unions $ ctxt_vcs ctxt)
    to_log log $ "\n\n"

  sum_total num_of = sum . map num_of . IM.elems

  report_per_entry ctxt log entry = do 
    let g      = ctxt_cfgs    ctxt IM.! entry
    let vcs    = ctxt_vcs     ctxt IM.! entry
    let ret    = ctxt_calls   ctxt IM.! entry
    let result = ctxt_results ctxt IM.! entry

    to_log log $ "Function entry:             " ++ showHex entry
    to_log log $ "Verification result:        " ++ show result
    to_log log $ "#instructions:              " ++ show (num_of_instructions g)
    to_log log $ "#assertions:                " ++ show (count_instructions_with_assertions vcs)
    to_log log $ "unresolved indirect jumps:  " ++ show (num_of_unres_inds_in_cfg ctxt isJump g)
    to_log log $ "unresolved indirect calls:  " ++ show (num_of_unres_inds_in_cfg ctxt isCall g)
    to_log log $ "#blocks:                    " ++ show (num_of_blocks       g)
    to_log log $ "#edges:                     " ++ show (num_of_edges        g)
    to_log log $ "return behavior:            " ++ show_return_behavior ret
    to_log log $ "\n\n"



  num_of_resolved_indirection_calls ctxt         = IM.size $ IM.filterWithKey (indirectionIsCall ctxt) $ ctxt_inds ctxt
  num_of_resolved_indirection_jumps ctxt         = IM.size $ IM.filterWithKey (indirectionIsJump ctxt) $ ctxt_inds ctxt

  num_of_verified        = IM.size . IM.filter ((==) VerificationSuccess)
  num_of_verifiedw       = IM.size . IM.filter ((==) VerificationSuccesWithAssumptions)
  num_of_verif_error     = IM.size . IM.filter isVerificationError
  num_of_unres_inds ctxt chkKind cfgs = sum (map (num_of_unres_inds_in_cfg ctxt chkKind) $ IM.elems cfgs)



  isVerificationError (VerificationError _) = True
  isVerificationError _                     = False

  show_return_behavior (ReturningWith p)  = "Returning normally"
  show_return_behavior Terminating        = "Terminating"
  show_return_behavior UnknownRetBehavior = "Unknown"

  indirectionIsCall ctxt a _ =
    case unsafePerformIO $ fetch_instruction ctxt $ fromIntegral a of -- Should be safe as result is immutable.
      Nothing -> False
      Just i  -> isCall $ Instr.opcode i
  indirectionIsJump ctxt a _ =
    case unsafePerformIO $ fetch_instruction ctxt $ fromIntegral a of -- Should be safe as result is immutable.
      Nothing -> False
      Just i  -> not $ isCall $ Instr.opcode i
  

num_of_instructions           g = sum (map length $ IM.elems $ cfg_blocks g)
num_of_blocks                 g = IM.size $ cfg_blocks g
num_of_edges                  g = sum (map IS.size $ IM.elems $ cfg_edges g)
--}










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

  ctxt_read_init_fini_array ("", ".init_array",a0,si) = read_pointers_from_ro_data_section a0 si
  ctxt_read_init_fini_array ("", ".fini_array",a0,si) = read_pointers_from_ro_data_section a0 si
  ctxt_read_init_fini_array _                         = return []

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
  let entry_dirname = dirname ++ showHex entry ++ "/"
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
  (new_calls,g) <- liftIO $ cfg_gen ctxt entry

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
  ctxt        <- get
  return $ mk_fcontext ctxt entry

ctxt_generate_invs :: Int -> Invariants -> S.Set (NodeInfo,Predicate) -> S.Set SStatePart -> StateT Context IO ()
ctxt_generate_invs entry curr_invs curr_posts curr_sps = do
  -- Generate invariants
  ctxt  <- get
  let get_max_time         = ctxt_max_time ctxt
  let get_max_time_minutes = get_max_time `div` 60000000

  fctxt    <- ctxt_mk_fcontext entry
  let finit = f_init fctxt
  let f     = function_name_of_entry (f_ctxt fctxt) (f_entry fctxt)

  
  when (not $ M.null finit) $ to_out $ "Function initialisation: " ++ show_finit finit

  -- let a       = acode_simp $ cfg_to_acode g 0 IS.empty
  g          <- gets (fromJust . IM.lookup entry . ctxt_cfgs)
  let p       = init_pred fctxt f curr_invs curr_posts curr_sps

  result  <- liftIO (timeout get_max_time $ return $! do_prop fctxt g 0 p) -- TODO always 0?

  case result of
    Nothing         -> do
      to_out $ "Entry " ++ showHex entry ++ ": time out of " ++ show get_max_time_minutes ++ " minutes reached."
      ctxt_add_invariants entry finit IM.empty S.empty S.empty S.empty
    Just (invs,vcs) -> do
      let blocks  = IM.keys $ cfg_blocks g
      postss     <- catMaybes <$> mapM (get_post fctxt g invs) blocks
      let posts   = S.fromList $ map fst postss
      let vcs'    = S.unions $ map snd postss 
      ctxt_add_invariants entry finit invs posts (S.union vcs vcs') (S.union curr_sps $ gather_stateparts invs posts)
 where
  get_post :: FContext -> CFG -> Invariants -> Int -> StateT Context IO (Maybe ((NodeInfo,Predicate),VCS))
  get_post fctxt g invs b = do
    let ctxt = f_ctxt fctxt
    if is_end_node g b then do
      let (q,vcs') = runIdentity $ execStateT (do_tau fctxt g b) (im_lookup ("B.) Block " ++ show b ++ " in invs") invs b, S.empty)
      return $ Just ((node_info_of ctxt g b,q),vcs')
    else
      return $ Nothing

  -- do one more tau-transformation on the node, as the stored invariant is a 
  -- precondition (not a postcondition) of the node.
  do_tau :: FContext -> CFG -> Int -> State (Predicate,VCS) ()
  do_tau fctxt g b = modify $ (tau fctxt (fetch_block g b) Nothing . fst)







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
  let g        =  ctxt_cfgs ctxt IM.! entry

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
    let p                 = im_lookup ("A.) Block " ++ show b ++ " in invs") invs b


    case flagstatus_to_tries max_tries (sflags p) of
      Nothing -> try_to_resolve_from_invariant fname f g b p i trgt
      Just (op1,n) -> do
        let values1 = map (\n -> evalSstate (try fctxt f (addressof i) g b op1 trgt n) p) [0..n]
        if values1 == [] || any ((==) Nothing) values1 then
          try_to_resolve_from_invariant fname f g b p i trgt
        else if all is_jump_table_entry values1 then do
          let trgts = map (fromIntegral . get_immediate . S.findMin . fromJust) values1
          to_out $ "Resolved indirection at block " ++ show b
          to_out $ "Instruction = " ++ show i
          to_out $ "Operand " ++ show trgt ++ " evaluates to: " ++ showHex_list (nub trgts)
          to_out $ "Because of bounded jump table access: " ++ show (sflags p)
          to_out $ "Updated file: " ++ fname
          liftIO $ appendFile fname $ showHex (addressof i) ++ " " ++ showHex_list trgts ++ "\n"

          inds <- gets ctxt_inds
          let inds' = IM.insert (fromIntegral $ addressof i) (IndirectionJumpTable $ JumpTable op1 trgt trgts) inds -- TODO check if already exists
          modify $ set_ctxt_inds inds'

          return True
        else
          error $ show ("resolving:",values1)


  try_to_resolve_from_invariant fname f g b p i trgt = do
    ctxt <- get
    fctxt <- ctxt_mk_fcontext entry
    let values0 = evalSstate (try' fctxt f g b trgt) p
    if values0 /= Nothing && S.size (fromJust values0) == 1 then do
      let value = fromJust values0
      to_out $ "Resolved indirection at block " ++ show b
      to_out $ "Instruction = " ++ show i
      to_out $ "Operand " ++ show trgt ++ " evaluates to: " ++ show value
      to_out $ "State:\n" ++ show p
      to_out $ "Updated file: " ++ fname
      liftIO $ appendFile fname $ showHex (addressof i) ++ " " ++ show [value] ++ "\n"

      inds <- gets ctxt_inds
      let inds' = IM.insert (fromIntegral $ addressof i) (IndirectionResolved value) inds -- TODO check if already exists
      modify $ set_ctxt_inds inds'
      return True
    else do
      to_out $ "Unresolved block " ++ show b
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
    swrite_operand fctxt False op1 (simmediate fctxt n)
    try' fctxt f g blockId trgt

  try' fctxt f g blockId trgt = do
    let ctxt = f_ctxt fctxt
    modify $ (sexec_block fctxt (init $ fetch_block g blockId) Nothing . fst)
    val <- sread_operand fctxt trgt
    return $ stry_jump_targets fctxt val


