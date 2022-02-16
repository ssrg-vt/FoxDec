{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}


module Main where

import System.Console.ArgParser


import Base
import Context
import Config
import X86_Datastructures
import ParserDump
import ParserSymbols
import ParserSections
import ParserIndirections
import ParserCalls 
import CFG_Gen
import SymbolicExecution
import SimplePred
import MachineState
import ACode_Gen
import Propagation
import CallGraph
import Conventions
import ControlFlow
import Pointers

import Numeric (readHex)
import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List (intercalate,delete,isInfixOf,partition)
import Data.Foldable
import qualified Data.Serialize as Cereal hiding (get,put)
import qualified Data.ByteString as BS (readFile,writeFile) 

import Debug.Trace
import System.IO
import System.Process (callCommand)
import System.Timeout (timeout)
import System.Directory (doesFileExist,createDirectoryIfMissing)
import Data.Functor.Identity


to_out       = liftIO . putStrLn
to_log log s = liftIO $ appendFile log $ s ++ "\n"



-- the main algorithm
run_with_ctxt :: StateT Context IO ()
run_with_ctxt = do
  -- 1.) read instructions, data, symbols, section info, the entry point(s), and the currently known indirections
  ctxt_read_dump
  ctxt_read_syms
  ctxt_read_sections
  ctxt_read_and_set_entries
  ctxt_init_indirections

  -- 2.) repeatedly, do 2.1 -> 2.3
  repeat

  -- 3.) generate report and call graph
  ctxt_generate_end_report
  ctxt_generate_call_graph
  ctxt_serialize_ctxt
 where
  -- 2.)
  repeat = do
    reconsider <- ctxt_reconsider_mutual_recursive_call
    case reconsider of
      Just (entry,trgts) -> do
        to_out $ "\n\nReconsidering (due to mutual recursion) entry " ++ showHex entry ++ " as all entries " ++ showHex_set trgts ++ " are now done."
        modify (\ctxt -> ctxt { ctxt_recursions = IM.delete entry $ ctxt_recursions ctxt, ctxt_calls = IM.delete entry $ ctxt_calls ctxt, ctxt_entries = graph_add_edges (ctxt_entries ctxt) entry IS.empty })
        repeat
      Nothing -> do
        entries <- gets ctxt_entries
        case graph_find_next entries of
          Nothing -> to_out $ "Done!"
          Just a  -> ctxt_run_entry a



  -- 2.1) consider the current entry address $a$
  ctxt_run_entry a = do
    -- 2.2) Start analysis at entry-address $a$
    calls <- gets ctxt_calls
    case IM.lookup a $ calls of
      Just _  -> do
        -- 2.2.1) Entry is already done, delete it and continue
        ctxt_del_entry a
        repeat
      _ -> do
        to_out $ "\n\nEntry " ++ showHex a
        -- 2.2.3) Entry is new, generate a CFG
        curr_g                            <- gets (IM.lookup a . ctxt_cfgs)
        (curr_finit,curr_invs,curr_posts) <- ctxt_get_curr_posts a


        to_out $ "Entry " ++ showHex a ++ ": starting CFG generation."
        ctxt_generate_cfg a
        ctxt_generate_invs a curr_invs curr_posts   

        g                  <- gets (IM.lookup a . ctxt_cfgs)
        (finit,invs,posts) <- ctxt_get_curr_posts a
        let do_repeat = curr_g /= g {-- || invs /= curr_invs --} || posts /= curr_posts
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
      verified     <- ctxt_verify_proper_return entry
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
    modify (\ctxt -> ctxt { ctxt_recursions = IM.insertWith IS.union entry recursions (ctxt_recursions ctxt) })
    to_out $ "Entry " ++ showHex entry ++ " is part of mutual recursion and treated entries " ++ showHex_set recursions ++ " as external. It will be reconsidered after those entries have been done."
 where
  get_new_calls ctxt is = map (get_new_call ctxt) is
  get_new_call  ctxt i =
    if is_call (i_opcode i) then
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
  ctxt <- get
  let g = ctxt_cfgs ctxt IM.! entry
  (finit_of_entry,_,_) <- ctxt_get_curr_posts entry

  return $ gather ctxt $ catMaybes $ concat $ concatMap (get_new_calls ctxt finit_of_entry) $ IM.elems $ cfg_instrs g
 where
  get_new_calls ctxt finit_of_entry is = map (get_new_call ctxt finit_of_entry) is
  get_new_call  ctxt finit_of_entry i =
    if is_call (i_opcode i) then
       map (trgt_to_finit ctxt finit_of_entry i) $ resolve_jump_target ctxt i
    else
      []

  trgt_to_finit ctxt finit_of_entry i (ImmediateAddress trgt) =
    case (IM.lookup (fromIntegral trgt) $ ctxt_calls ctxt, IM.lookup (fromIntegral trgt) $ ctxt_finits ctxt, get_invariant ctxt entry $ i_addr i) of
      (Nothing,Nothing,Just inv) -> Just $ (fromIntegral trgt,invariant_to_finit ctxt finit_of_entry inv)
      (_,Just finit,Just inv)    -> do
        let finit' = join_finit ctxt (invariant_to_finit ctxt finit_of_entry inv) finit
        if finit /= finit' || (graph_is_vertex (ctxt_entries ctxt) (fromIntegral trgt) && not (graph_is_edge (ctxt_entries ctxt) entry (fromIntegral trgt))) then
          return (fromIntegral trgt,finit') -- trace ("new finit@" ++ showHex trgt ++ "\n" ++ show finit ++ "\n" ++ show (invariant_to_finit ctxt finit_of_entry inv) ++ "\n" ++ show finit') $ 
        else
          Nothing
      (x,y,z) -> error (show (x,y,z))
  trgt_to_finit ctxt finit_of_entry i _ = Nothing

  gather ctxt []         = []
  gather ctxt [new_call] = [new_call]
  gather ctxt ((entry',finit):new_calls) =
    let (same,others) = partition ((==) entry' . fst) new_calls in
      (entry', foldr1 (join_finit ctxt) (finit:map snd same)) : gather ctxt others
    


ctxt_serialize_ctxt :: StateT Context IO ()
ctxt_serialize_ctxt = do
  ctxt <- get
  dirname  <- gets ctxt_dirname
  name     <- gets ctxt_name
  let fname = dirname ++ name ++ ".report" 
  liftIO $ BS.writeFile fname $ Cereal.encode ctxt
  to_out $ "Generated verification report: " ++ fname 





ctxt_add_entry_edge :: Int -> (Int,FInit) -> StateT Context IO ()
ctxt_add_entry_edge a (trgt,finit) = do
  modify (\ctxt -> ctxt { ctxt_entries = graph_add_edges (ctxt_entries ctxt) a (IS.singleton trgt) })
  modify (\ctxt -> ctxt { ctxt_finits  = IM.insert trgt finit $ ctxt_finits ctxt })
  modify (\ctxt -> ctxt { ctxt_calls   = IM.delete trgt $ ctxt_calls ctxt })
  modify (\ctxt -> ctxt { ctxt_invs    = IM.delete trgt $ ctxt_invs  ctxt })
  modify (\ctxt -> ctxt { ctxt_posts   = IM.delete trgt $ ctxt_posts ctxt })


ctxt_get_curr_posts :: Int -> StateT Context IO (FInit,Invariants,S.Set (NodeInfo,Pred))
ctxt_get_curr_posts entry = do
  finits <- gets ctxt_finits
  invs   <- gets ctxt_invs
  posts  <- gets ctxt_posts
  return (IM.lookup entry finits `orElse` M.empty, IM.lookup entry invs `orElse` IM.empty, IM.lookup entry posts `orElse` S.empty)




-- Assuming a CFG and invariants are generated, verify for each block $b$ that is an end-node in the CFG whether:
-- if the end-node "returns normally", i.e., ends in a RET, then it returns correctly, i.e., the return address is 
-- not overwritten and callee-saved registers are preserved and RSP is set to RSP0+8
ctxt_verify_proper_return :: Int -> StateT Context IO VerificationResult
ctxt_verify_proper_return entry = do
  ctxt            <- get
  let vcs          = IM.lookup entry (ctxt_vcs ctxt) `orElse` S.empty
  (_,_,posts)     <- ctxt_get_curr_posts entry
  all_checks      <- mapM (correct ctxt) $ filter (\(ni,q) -> ni /= Terminal) $ S.toList posts

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
  correct :: Context -> (NodeInfo,Pred) -> StateT Context IO [Maybe String]
  correct ctxt (_,q) = do
    return $ runIdentity $ evalStateT (do_post_check ctxt) (q,S.empty) 


  -- do one more tau-transformation on the node, as the stored invariant is a 
  -- precondition (not a postcondition) of the node.
  do_post_check :: Context -> State (Pred,VCS) [Maybe String]
  do_post_check ctxt = do
    rsp    <- read_reg ctxt RSP
    rip    <- read_reg ctxt RIP
    checks <- return [] -- forM (delete RSP callee_saved_registers) (\r -> read_reg ctxt r >>= return . reg_check r) 
    return $ [rsp_check rsp, rip_check rip] ++ checks

  -- check: are all caller-saved-registers restored to their initial values?
  reg_check reg v =
    case v of
      -- REG == REG_0
      SE_Var (SP_Reg reg) -> Nothing
      _ -> Just $ "Verification error: " ++ show reg ++ " == " ++ show v

  -- check: is RSP restored to RSP_0 + 8 ?
  rsp_check rsp =
    case rsp of
      -- RSP == RSP_0 + 8
      SE_Op (Plus _)  [SE_Var (SP_Reg RSP), SE_Immediate 0x8] -> Nothing
      -- RSP == RSP_0 - 0xfffffffffffffff8
      SE_Op (Minus _) [SE_Var (SP_Reg RSP), SE_Immediate 0xfffffffffffffff8] -> Nothing
      _ -> Just $ "Verification error: RSP == " ++ show rsp

  -- check: is RIP set to the value originally stored at the top of the stack frame?
  rip_check rip = 
    case rip of
      -- RIP == *[RSP_0,8]
      SE_Var (SP_Mem (SE_Var (SP_Reg RSP)) 8) -> Nothing
      e -> Just $ "Verification error: RIP == " ++ show rip ++ "\n"



-- After a succesfull effort, store the current CFG, the invariants,...,  in the "report"-field of the context
ctxt_add_to_results entry verified = do
  results <- gets ctxt_results
  modify (\ctxt -> ctxt { ctxt_results = IM.insert entry verified results } )

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
  to_log log $ "#unresolved indirections:   " ++ show (num_of_unres_inds_in_cfg ctxt g)
  to_log log $ "#sourceless memwrites:      " ++ show (count_sourceless_memwrites vcs)
  to_log log $ "#memwrites (approximation): " ++ show (count_all_mem_writes g)
  to_log log $ "#blocks:                    " ++ show (num_of_blocks       g)
  to_log log $ "#edges:                     " ++ show (num_of_edges        g)
  to_log log $ ""

  to_log log $ summarize_finit finit
  to_log log $ ""

  to_log log $ "Return behavior: " ++ show_return_behavior ret  

  to_log log $ summarize_preconditions_long ctxt vcs
  to_log log $ ""

  to_log log $ summarize_assertions_long ctxt vcs
  to_log log $ ""

  to_log log $ summarize_function_constraints_long ctxt vcs
  to_log log $ ""

  to_log log $ summarize_sourceless_memwrites_long ctxt vcs
  to_log log $ ""


  --to_log log $ "Generated invariants:" -- TODO make configurable
  --to_log log $ show_invariants g invs
 where
  show_return_behavior (ReturningWith p)  = "returning with\n" ++ pp_pred p ++ "\n\n"
  show_return_behavior Terminating        = "terminating"
  show_return_behavior UnknownRetBehavior = "unknown"

  show_verification_result (VerificationError msg)           = "\n\n\n---------------------\nVerification error!!!\n---------------------\n" ++ msg
  show_verification_result VerificationSuccesWithAssumptions = "Verified (with assumptions)"
  show_verification_result VerificationSuccess               = "Verified"
  show_verification_result VerificationUnresolvedIndirection = "Unresolved indirections"
  show_verification_result Unverified                        = "UNVERIFIED"

num_of_unres_inds_in_cfg ctxt g = 
  let blocks  = IM.keys $ cfg_blocks g in
    length (filter (\b -> node_info_of ctxt g b == UnresolvedIndirection) blocks)


count_all_mem_writes g = 
  let instrs = S.fromList $ concat $ IM.elems $ cfg_instrs g
      writes = S.filter is_mem_write instrs in
    S.size writes
 where
  is_mem_write i = (is_mem_operand $ i_op1 i) || i_opcode i `elem` [PUSH,POP,CALL,RET]

  is_mem_operand (Just (Address  _)) = True
  is_mem_operand _                   = False


-- Generate the call graph
ctxt_generate_call_graph :: StateT Context IO ()
ctxt_generate_call_graph = do
  cfgs     <- gets ctxt_cfgs
  dirname  <- gets ctxt_dirname
  name     <- gets ctxt_name
  do_pdfs  <- gets ctxt_generate_pdfs
  let fname   = dirname ++ name ++ "_calls.dot" 
  let pdfname = dirname ++ name ++ "_calls.pdf" 
  ctxt     <- get


  let g = Edges $ IM.map (calls_of_cfg ctxt) cfgs
  liftIO $ writeFile fname $ callgraph_to_dot ctxt g

  if do_pdfs then do
    liftIO $ callCommand $ "dot -Tpdf " ++ fname ++ " -o " ++ pdfname
    to_out $ "Generated call graph, exported to files: " ++ fname ++ " and " ++ pdfname
  else do
    to_out $ "Generated call graph, exported to file: " ++ fname 

  




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
    to_log log $ "unresolved indirections:              " ++ show (num_of_unres_inds ctxt                       $ ctxt_cfgs ctxt)
    to_log log $ "#sourceless memwrites:                " ++ show (sum_total count_sourceless_memwrites         $ ctxt_vcs  ctxt)
    to_log log $ "#memwrites (approximation):           " ++ show (sum_total count_all_mem_writes               $ ctxt_cfgs  ctxt)
    to_log log $ "#blocks:                              " ++ show (sum_total num_of_blocks                      $ ctxt_cfgs ctxt)
    to_log log $ "#edges:                               " ++ show (sum_total num_of_edges                       $ ctxt_cfgs ctxt)
    to_log log $ "#resolved indirections:               " ++ show (num_of_resolved_indirections ctxt)
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
    to_log log $ "unresolved indirections:    " ++ show (num_of_unres_inds_in_cfg ctxt g)
    to_log log $ "#sourceless memwrites:      " ++ show (count_sourceless_memwrites vcs)
    to_log log $ "#memwrites (approximation): " ++ show (count_all_mem_writes g)
    to_log log $ "#blocks:                    " ++ show (num_of_blocks       g)
    to_log log $ "#edges:                     " ++ show (num_of_edges        g)
    to_log log $ "return behavior:            " ++ show_return_behavior ret
    to_log log $ "\n\n"



  num_of_resolved_indirections ctxt         = IM.size $ ctxt_inds ctxt

  num_of_verified        = IM.size . IM.filter ((==) VerificationSuccess)
  num_of_verifiedw       = IM.size . IM.filter ((==) VerificationSuccesWithAssumptions)
  num_of_verif_error     = IM.size . IM.filter isVerificationError
  num_of_unres_inds ctxt cfgs = sum (map (num_of_unres_inds_in_cfg ctxt) $ IM.elems cfgs)



  isVerificationError (VerificationError _) = True
  isVerificationError _                     = False

  show_return_behavior (ReturningWith p)  = "Returning normally"
  show_return_behavior Terminating        = "Terminating"
  show_return_behavior UnknownRetBehavior = "Unknown"

  

num_of_instructions           g = sum (map length $ IM.elems $ cfg_blocks g)
num_of_blocks                 g = IM.size $ cfg_blocks g
num_of_edges                  g = sum (map IS.size $ IM.elems $ cfg_edges g)









-- Read symbol table from file producing symbols :: IM.IntMap String
-- Per address a symbol name
ctxt_read_syms :: StateT Context IO ()
ctxt_read_syms = do
  ctxt <- get
  let dirname     = ctxt_dirname ctxt
  let name        = ctxt_name ctxt
  let syms_fname  = dirname ++ name ++ ".symbols" 

  symbols <- liftIO $ parse syms_fname
  put $ ctxt { ctxt_syms = symbols }
 where
  parse sfilename = do
    ret0 <- parse_symbols sfilename
    case ret0 of
      Left err -> error $ show err
      Right syms -> return syms

-- Read section info from file producing sections :: IM.IntMap Int
-- Per address a symbol name
ctxt_read_sections :: StateT Context IO ()
ctxt_read_sections = do
  ctxt <- get
  let dirname     = ctxt_dirname ctxt
  let name        = ctxt_name ctxt
  let sects_fname = dirname ++ name ++ ".sections" 

  sections <- liftIO $ parse sects_fname
  put $ ctxt { ctxt_sections = sections }
 where
  parse sfilename = do
    ret0 <- parse_sections sfilename
    case ret0 of
      Left err -> error $ show err
      Right sections -> return sections



-- Read dump from file producing dump :: IM.IntMap Word8
-- Per address a byte
ctxt_read_dump :: StateT Context IO ()
ctxt_read_dump = do
  ctxt <- get
  let dirname     = ctxt_dirname ctxt
  let name        = ctxt_name ctxt
  let fname       = dirname ++ name ++ ".dump"
  let dname       = dirname ++ name ++ ".data"

  dump <- liftIO $ parse fname
  dat  <- liftIO $ parse dname

  put $ ctxt { ctxt_dump = dump, ctxt_data = dat }
 where
  parse fname = do
    ret0 <- parse_dump fname
    case ret0 of
      Left err -> error $ show err
      Right syms -> return syms


-- Read entry from file producing entries :: [Int]
ctxt_read_entries :: StateT Context IO [Int]
ctxt_read_entries = do
  ctxt <- get
  let dirname = ctxt_dirname ctxt
  let name    = ctxt_name ctxt
  let fname   = dirname ++ name ++ ".entry" 

  liftIO $ parse fname
 where
  parse filename = do
    ls <- readFile filename
    return $ map read_line $ lines ls
  read_line = readHex' . tail . tail





-- Read entry from file producing entry :: Int
ctxt_read_and_set_entries :: StateT Context IO ()
ctxt_read_and_set_entries = do
  entries <- ctxt_read_entries
  modify $ (\ctxt -> ctxt { ctxt_entries = Edges $ IM.fromList (zip entries $ repeat IS.empty) })





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
  modify $ (\ctxt ->  ctxt { ctxt_inds = IM.empty })
 where
  parse fname = do
    ret0 <- parse_indirections fname
    case ret0 of
      Left err   -> error $ show err
      Right inds -> return inds





-- TODO generate dot only once
ctxt_generate_cfg :: Int -> StateT Context IO (S.Set (Instr,Int))
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
  modify (\ctxt -> ctxt { ctxt_cfgs = IM.insert entry g cfgs })

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

     


ctxt_add_invariants entry finit invs posts vcs = do
  all_invs   <- gets ctxt_invs
  all_posts  <- gets ctxt_posts
  all_vcs    <- gets ctxt_vcs
  all_finits <- gets ctxt_finits
  modify (\ctxt -> ctxt { ctxt_invs = IM.insert entry invs all_invs, ctxt_posts = IM.insert entry posts all_posts, ctxt_vcs = IM.insert entry vcs all_vcs, ctxt_finits = IM.insert entry finit all_finits } )




ctxt_generate_invs :: Int -> Invariants -> S.Set (NodeInfo,Pred) -> StateT Context IO ()
ctxt_generate_invs entry curr_invs curr_posts = do
  -- Generate invariants
  ctxt <- get
  (finit,_,_) <- ctxt_get_curr_posts entry
  when (not $ M.null finit) $ to_out $ "Function initialisation: " ++ show_finit finit

  -- let a       = acode_simp $ cfg_to_acode g 0 IS.empty
  g          <- gets (fromJust . IM.lookup entry . ctxt_cfgs)
  let p       = init_pred curr_invs curr_posts finit


  -- TODO remove
  entries <- ctxt_read_entries
  let p'   = if entries == [entry] then fst $ runIdentity $ execStateT (write_reg ctxt RSI $ SE_Malloc (Just 0) (Just "initial")) (p,S.empty) else p
  result  <- liftIO (timeout max_time $ return $! do_prop ctxt finit g 0 p') -- TODO always 0?

  case result of
    Nothing         -> do
      to_out $ "Entry " ++ showHex entry ++ ": time out of " ++ show max_time_minutes ++ " minutes reached."
      ctxt_add_invariants entry finit IM.empty S.empty S.empty
    Just (invs,vcs) -> do
      let blocks  = IM.keys $ cfg_blocks g
      postss     <- catMaybes <$> mapM (get_post ctxt finit g invs) blocks
      let posts   = S.fromList $ map fst postss
      let vcs'    = S.unions $ map snd postss 
      ctxt_add_invariants entry finit invs posts (S.union vcs vcs')
 where
  get_post :: Context -> FInit -> CFG -> Invariants -> Int -> StateT Context IO (Maybe ((NodeInfo,Pred),VCS))
  get_post ctxt finit g invs b = do
    if is_end_node g b then do
      let (q,vcs') = runIdentity $ execStateT (do_tau ctxt finit g b) (im_lookup ("B.) Block " ++ show b ++ " in invs") invs b, S.empty)
      return $ Just ((node_info_of ctxt g b,q),vcs')
    else
      return $ Nothing

  -- do one more tau-transformation on the node, as the stored invariant is a 
  -- precondition (not a postcondition) of the node.
  do_tau :: Context -> FInit -> CFG -> Int -> State (Pred,VCS) ()
  do_tau ctxt finit g b = modify $ (tau_block ctxt finit (fetch_block g b) Nothing . fst)



ctxt_del_entry :: Int -> StateT Context IO ()
ctxt_del_entry entry = modify (\ctxt -> ctxt { ctxt_entries = graph_delete (ctxt_entries ctxt) entry })

ctxt_add_call entry verified = do
  ctxt <- get
  let calls       = ctxt_calls ctxt
  let dirname     = ctxt_dirname ctxt
  let name        = ctxt_name ctxt
  let fname       = dirname ++ name ++ ".calls"

  (finit,invs,posts) <- ctxt_get_curr_posts entry
  
  (ret,msg) <-
    if (any ((==) UnresolvedIndirection) $ S.map fst posts) || verified `notElem` [VerificationSuccesWithAssumptions,VerificationSuccess] then
      return (UnknownRetBehavior, "unknown.")
    else if all ((==) Terminal) $ S.map fst posts then
      return (Terminating, "always terminating.")
    else let q = supremum ctxt finit $ map snd $ S.toList $ S.filter ((==) Normal . fst) $ posts in
      return (ReturningWith q, "normally returning.")

  put $ ctxt { ctxt_calls = IM.insert entry ret $ calls }
  to_out $ "Function at entry: " ++ showHex entry ++ " analyzed, return behavior is " ++ msg






operand_that_provides_jump_target ctxt i =
   if is_call_to_libc_start_main then
     Just $ Reg RDI
   else
     i_op1 i
 where
  is_call_to_libc_start_main = is_call (i_opcode i) && 
   (case operand_static_resolve ctxt i (i_op1 i) of
      External sym -> "libc_start_main" `isInfixOf` sym
      _            -> False
   )

ctxt_analyze_unresolved_indirections :: Int -> StateT Context IO Bool
ctxt_analyze_unresolved_indirections entry = do
  ctxt <- get

  (finit,invs,posts) <- ctxt_get_curr_posts entry
  let g        =  ctxt_cfgs ctxt IM.! entry

  let bs = filter (\b -> node_info_of ctxt g b == UnresolvedIndirection) $ IM.keys $ cfg_blocks g
  if bs == [] then do
    to_out $ "No unresolved indirections."
    return False
  else do
    results <- forM bs $ try_to_resolve_indirection finit g invs
    return $ not $ all not results -- prevent lazy execution
 where
  try_to_resolve_indirection finit g invs b = do
    ctxt <- get
    dirname   <- gets ctxt_dirname
    name      <- gets ctxt_name
    let fname  = dirname ++ name ++ ".indirections" 

    let i                   = last (fetch_block g b)
    let Just trgt           = operand_that_provides_jump_target ctxt i
    let p                   = im_lookup ("A.) Block " ++ show b ++ " in invs") invs b
    let Predicate eqs flg _ = p

    let values0 = evalState (try' ctxt finit g b trgt) (p,S.empty)
    let values1 = case flagstatus_to_tries flg of
                   Nothing      -> S.empty
                   Just (op1,n) -> S.unions $ map (\n -> evalState (try ctxt finit (i_addr i) g b op1 trgt n) (p,S.empty)) [0..n-1]
    let values  = S.union values0 values1

    -- TODO instead of once and for all, widen it
    if all ((==) Nothing) values || S.null values then do
      to_out $ "Unresolved block " ++ show b
      return False
    else do
      let trgts = IS.fromList $ map fromJust $ filter ((/=) Nothing) $ S.toList values
      to_out $ "Resolved indirection at block " ++ show b
      to_out $ "Instruction = " ++ show i
      to_out $ "Operand " ++ show_operand' trgt ++ " evaluates to: [" ++ (intercalate "," $ map (\a -> showHex a) $ IS.toList trgts) ++ "]"
      to_out $ "Updated file: " ++ fname
      liftIO $ appendFile fname $ showHex (i_addr i) ++ " [" ++ (intercalate "," $ map (\a -> showHex a) (IS.toList trgts)) ++ "]\n"

      inds <- gets ctxt_inds
      let inds' = IM.insertWith IS.union (i_addr i) trgts inds
      put $ ctxt { ctxt_inds = inds' }

      return True

  flagstatus_to_tries (FS_CMP (Just True) op1 (Immediate n)) = if n <= fromIntegral max_jump_table_size then Just (op1,n) else Nothing
  flagstatus_to_tries _ = Nothing

  -- write an immediate value to operand op1, then run symbolic exection to see if
  -- after executing a block the target-operand is an immediate value as well.
  try ctxt finit i_a g blockId op1 trgt n = do
    write_operand ctxt finit i_a op1 (SE_Immediate n)
    try' ctxt finit g blockId trgt

  try' ctxt finit g blockId trgt = do
    modify $ (tau_block ctxt finit (init $ fetch_block g blockId) Nothing . fst)
    val <- read_operand ctxt finit trgt
    case val of
      SE_Immediate a                     -> return $ S.singleton $ Just $ fromIntegral a
      Bottom (FromNonDeterminism es)     -> return $ if all (expr_highly_likely_pointer ctxt) es then S.map take_immediates es else S.singleton Nothing
      SE_Var (SP_Mem (SE_Immediate a) _) -> return $ if address_has_symbol ctxt a then S.singleton $ Just $ fromIntegral a else S.singleton Nothing
      e                                  -> return $ S.singleton Nothing

  take_immediates (SE_Immediate a) = Just $ fromIntegral a
  take_immediates _                = Nothing








-- Command line arguments parser
data Args =  Args Int String String Bool
  deriving (Show)

argsParser = Args
  `parsedBy` optPos 0  "pdf"      `Descr` "If > 0, then generate PDFs (note: sometimes graphviz may get stuck)."
  `andBy`    optPos [] "dirname"  `Descr` "Name of directory (including ending /)"
  `andBy`    optPos [] "filename" `Descr` "Basename of file (without directory) without dot and without file-extension."
  `andBy`    boolFlag "u"         `Descr` "Show information on usage and quit."
  
-- if the -u flag is not set, check whether a config file has been given.
-- Read it it, if so.
run (Args generate_pdfs dirname name False) =
  case (dirname,name) of
    ([],_) -> putStrLn "No directory name given. Use -u for information on usage."
    (_,[]) -> putStrLn "No base-filename given. Use -u for information on usage."
    _      -> evalStateT run_with_ctxt (init_context dirname name (generate_pdfs /= 0))

-- if the -u flag is set, output the message on usage
run (Args generate_pdfs _ _ True) =
  putStrLn usage_msg 


usage_msg = intercalate "\n" [
  "FoxDec usage:",
  "",
  "  foxdec $PDF $DIRNAME $BASE",
  "",
  "$PDF     = Either 0 or 1. Iff 1 then use graphviz to generate PDFs from .dot files. Note: sometimes graphviz may get stuck.",
  "$DIRNAME = Name of directory where a $BASE.dump, a $BASE.symbols and a $BASE.entry file are located.",
  "$BASE    = Base name of files, without a dot and without extension",
  "",
  "Example usage:",
  "",
  "  foxdec 0 ../examples/du/ du",
  "",
  "For information on how to generate .dump, .symbols, .sections and .entry file, see the README."
 ] 


-- Parse the command line arguments and run
main = do
  withParseResult argsParser run


