{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}


module Main where

import System.Console.ArgParser


import Base
import Context
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
import Isabelle

import Numeric (readHex)
import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List (intercalate,delete,isInfixOf)
import Data.Foldable
import qualified Data.Serialize as Cereal hiding (get,put)
import qualified Data.ByteString as BS (readFile,writeFile) 

import Debug.Trace
import System.IO
import System.Process (callCommand)
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
  ctxt_init_calls
  -- 2.) repeatedly, do 2.1 -> 2.3
  repeat
  -- 3.) generate report and call graph
  ctxt_generate_end_report
  ctxt_generate_call_graph

  ctxt_create_hoare_triples
  ctxt_serialize_ctxt
 where
  -- 2.)
  repeat = do
    -- 2.1) consider the current entry address $a$
    entries <- gets ctxt_entries
    to_out $ "\n\n"
    case graph_find_next entries of
      Nothing -> to_out $ "Done!"
      Just a  -> ctxt_run_entry a

  

  ctxt_run_entry a = do
    -- 2.2) Start analysis at entry-address $a$
    to_out $ "Entry " ++ showHex a
    calls <- ctxt_read_calls
    ctxt  <- get
    instr <- liftIO $ fetch_instruction ctxt a
    case (IM.lookup a calls, instr) of
      (Just _,_) -> do
        -- 2.2.1) Entry is already done, delete it and continue
        to_out $ "Entry " ++ showHex a ++ ": already analyzed."
        ctxt_del_entry a
        repeat
      (Nothing, Nothing) -> do
        -- 2.2.2) Entry address does not exist, treat as an external function
        to_out $ "Entry " ++ showHex a ++ ": DOES NOT EXIST (IS TREATED AS EXTERNAL)."
        ctxt_add_call_external a
        ctxt_del_entry a
        repeat
      _ -> do
        -- 2.2.3) Entry is new, generate a CFG
        to_out $ "Entry " ++ showHex a ++ ": starting CFG generation."
        cfg_result <- ctxt_generate_cfg a
        case cfg_result of
          Right g -> 
            -- succesfull CFG generation
            ctxt_run_cfg a g
          Left new_entries -> do
            -- no CFG, but found new internal function(s), stop current analysis, add new entries to entry-graph
            ctxt_add_entries a new_entries
            to_out $ "Entry " ++ showHex a ++ ": Adding new internal functions at " ++ showHex_set new_entries ++ " to graph of entries."
            repeat

  -- 2.3) Analyze a CFG, either needing a new round (in case of newly resolved indirections) are not.
  ctxt_run_cfg a g = do
    -- generate invariants
    (curr_invs,curr_posts) <- ctxt_get_curr_posts a
    (invs,posts) <- ctxt_generate_invs a g curr_invs curr_posts
    ctxt_add_to_report a Unverified S.empty g invs posts
    -- analyze unresolved jumps
    continue <- ctxt_analyze_unresolved_indirections g invs
    if continue then do
      -- if new jumps where resolved, repeat with current function
      to_out $ "Entry " ++ showHex a ++ ": Continuing to next round as there are newly resolved indirections."
      repeat
    else if invs /= curr_invs || posts /= curr_posts then do
      -- if new invariants were found, repeat with current function
      to_out $ "Entry " ++ showHex a ++ ": Continuing to next round as invariants where refined."
      repeat
    else do
      -- if no new jumps where resolved, this function is done: store entry address as an analyzed call and pop the entry-stack
      to_out $ "Entry " ++ showHex a ++ ": Terminating as no new indirections are resolved."
      verified     <- ctxt_verify_proper_return a g invs
      vcs          <- ctxt_collect_verification_conditions a g invs
      let verified' = if verified == VerificationSuccess && any (not . is_precondition) vcs then VerificationSuccesWithAssertions else verified
      to_out $ "Entry " ++ showHex a ++ ": Verified == " ++ show verified'

      ctxt_add_to_report a verified' vcs g invs posts
      ctxt_add_call a g
      ctxt_del_entry a
      repeat




ctxt_serialize_ctxt :: StateT Context IO ()
ctxt_serialize_ctxt = do
  ctxt <- get
  dirname  <- gets ctxt_dirname
  name     <- gets ctxt_name
  let fname = dirname ++ name ++ ".report" 
  liftIO $ BS.writeFile fname $ Cereal.encode ctxt
  to_out $ "Generated verification report: " ++ fname 



ctxt_get_curr_posts :: Int -> StateT Context IO (Invariants,S.Set (NodeInfo,Pred))
ctxt_get_curr_posts a = do
  report <- gets ctxt_report
  case IM.lookup a report of
    Nothing -> return $ (IM.empty,S.empty)
    Just (Report _ invs posts _ _) -> return (invs,posts)


ctxt_create_hoare_triples :: StateT Context IO ()
ctxt_create_hoare_triples = do
  ctxt    <- get
  report  <- gets ctxt_report
  imports <- mapM (report_entry_to_hoare_triples ctxt) $ IM.toList report

  
  dirname  <- gets ctxt_dirname
  name     <- gets ctxt_name
  let fname   = dirname ++ name ++ ".thy" 
  generate_isa_main_thy name fname imports
  to_out $ "Generated Isabelle thy file file: " ++ fname 
 where
  report_entry_to_hoare_triples ctxt (entry,(Report g invs posts _ vcs)) = do

    name        <- gets ctxt_name
    base        <- ctxt_base_name entry
    let fname    = base ++ "_" ++ showHex entry ++ ".thy"
    generate_isa_thy name entry fname g invs posts vcs

-- Assuming a CFG and invariants are generated, verify for each block $b$ that is an end-node in the CFG whether:
-- if the end-node "returns normally", i.e., ends in a RET, then it returns correctly, i.e., the return address is 
-- not overwritten and callee-saved registers are preserved and RSP is set to RSP0+8
ctxt_verify_proper_return :: Int -> CFG -> Invariants -> StateT Context IO VerificationResult
ctxt_verify_proper_return entry g invs = do
  ctxt       <- get
  base       <- ctxt_base_name entry
  let log     = base ++ ".log"

  (_,posts) <- ctxt_get_curr_posts entry
  all_checks <- mapM (correct ctxt) $ filter (\(ni,q) -> ni /= Terminal) $ S.toList posts
  if any (\(ni,q) -> ni == UnresolvedIndirection) posts then do
    to_log log $ "Function has unresolved indirections."
    return VerificationUnresolvedIndirection
  else if any ((/=) Nothing) $ concat all_checks then do
    to_log log $ "\n\n\n---------------------\nVerification error!!!\n---------------------\n"
    to_log log $ intercalate "\n\n" $ catMaybes $ concat all_checks 
    return VerificationError
  else do
    to_log log $ "Function verified."
    return VerificationSuccess
 where
  -- is the postcondition "correct"? 
  -- A succesfull check returns Nothing, a verification error produces a "Just err" with an error-message
  correct :: Context -> (NodeInfo,Pred) -> StateT Context IO [Maybe String]
  correct ctxt (_,q) = do
    return $ runIdentity $ evalStateT (do_post_check ctxt) $ q 


  -- do one more tau-transformation on the node, as the stored invariant is a 
  -- precondition (not a postcondition) of the node.
  do_post_check :: Context -> State Pred [Maybe String]
  do_post_check ctxt = do
    rsp    <- read_reg RSP
    rip    <- read_reg RIP
    checks <- forM (delete RSP callee_saved_registers) (\r -> read_reg r >>= return . reg_check r) 
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
      e -> Just $ "Verification error: RIP == " ++ show rip ++ "\n" ++ "\n----"



-- After a succesfull effort, store the current CFG, the invariants,...,  in the "report"-field of the context
ctxt_add_to_report a verified vcs g invs posts = do
  report <- gets ctxt_report
  modify (\ctxt -> ctxt { ctxt_report = IM.insert a (Report g invs posts verified vcs) $ report } )



-- Generate the call graph
ctxt_generate_call_graph :: StateT Context IO ()
ctxt_generate_call_graph = do
  report   <- gets ctxt_report
  dirname  <- gets ctxt_dirname
  name     <- gets ctxt_name
  do_pdfs  <- gets ctxt_generate_pdfs
  let fname   = dirname ++ name ++ "_calls.dot" 
  let pdfname = dirname ++ name ++ "_calls.pdf" 
  ctxt     <- get


  let g = Edges $ IM.map (\(Report cfg _ _ _ _) -> calls_of_cfg ctxt cfg) report
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
  let report      = ctxt_report ctxt

  liftIO $ writeFile log ""
  to_out $ "Writing log to file: " ++ log

  report_total ctxt log report 
  mapM_ (report_per_entry ctxt log) $ IM.toList report
 where
  report_total ctxt log report = do
    to_log log $ "Total"
    to_log log $ "#functions:                           " ++ show (IM.size report)
    to_log log $ "    of which verified                 " ++ show (num_of_verified                 report)
    to_log log $ "    of which verified with assertions " ++ show (num_of_verifiedw                report)
    to_log log $ "    of which unres_inds               " ++ show (num_of_unres_inds               report)
    to_log log $ "    of which verif_error              " ++ show (num_of_verif_error              report)
    to_log log $ "#instructions:                        " ++ show (sum_total num_of_instructions   report)
    to_log log $ "#assertions:                          " ++ show (sum_total num_of_assertions     report)
    to_log log $ "#blocks:                              " ++ show (sum_total num_of_blocks         report)
    to_log log $ "#edges:                               " ++ show (sum_total num_of_edges          report)
    to_log log $ "#resolved indirections:               " ++ show (num_of_resolved_indirections    ctxt)
    to_log log $ "\n\n"

  sum_total num_of = sum . map num_of . IM.elems

  report_per_entry ctxt log (a,e@(Report g invs posts verified preconditions)) = do
    instr <- liftIO $ fetch_instruction ctxt a
    case instr of
      Nothing -> to_log log $ "Function entry:           " ++ showHex a ++ " DOES NOT EXIST (IS TREATED AS EXTERNAL)."
      Just _ -> do
        to_log log $ "Function entry:           " ++ showHex a
        to_log log $ "Verification result:      " ++ show (verified)
        to_log log $ "#instructions:            " ++ show (num_of_instructions                 e)
        to_log log $ "#assertions:              " ++ show (num_of_assertions                   e)
        to_log log $ "#blocks:                  " ++ show (num_of_blocks                       e)
        to_log log $ "#edges:                   " ++ show (num_of_edges                        e)
        to_log log $ "\n\n"

  num_of_instructions  (Report g _ _ _ _)   = sum (map length $ IM.elems $ cfg_blocks g)
  num_of_assertions    (Report _ _ _ _ vcs) = count_instructions_with_assertions vcs
  num_of_blocks        (Report g _ _ _ _)   = IM.size $ cfg_blocks g
  num_of_edges         (Report g _ _ _ _)   = sum (map IS.size $ IM.elems $ cfg_edges g)

  num_of_resolved_indirections ctxt         = IM.size $ ctxt_inds ctxt

  num_of_verified    = IM.size . IM.filter (\(Report _ _ _ verified _) -> verified == VerificationSuccess)
  num_of_verifiedw   = IM.size . IM.filter (\(Report _ _ _ verified _) -> verified == VerificationSuccesWithAssertions)
  num_of_unres_inds  = IM.size . IM.filter (\(Report _ _ _ verified _) -> verified == VerificationUnresolvedIndirection)
  num_of_verif_error = IM.size . IM.filter (\(Report _ _ _ verified _) -> verified == VerificationError)




-- Collect all verification conditions
ctxt_collect_verification_conditions :: Int -> CFG -> Invariants -> StateT Context IO (S.Set VerificationCondition)
ctxt_collect_verification_conditions entry g invs = do
  base       <- ctxt_base_name entry
  ctxt       <- get
  let log     = base ++ ".log"
  let blocks  = IM.keys $ cfg_blocks g
  vcss       <- mapM collect_verification_conditions blocks

  let all_verification_conditions = foldr S.union S.empty vcss
  let summary = summarize_verification_conditions ctxt all_verification_conditions

  if S.null all_verification_conditions then do
    to_log log $ "No verification conditions."
  else do
    to_log log $ "\n\nVerification conditions:\n\n" ++ summary -- ++ show all_verification_conditions ++ "\n"
  return all_verification_conditions


 where
  collect_verification_conditions :: Int -> StateT Context IO (S.Set VerificationCondition)
  collect_verification_conditions b = do
    ctxt <- get
   -- if is_end_node g b then
    return $ runIdentity $ evalStateT (do_tau ctxt b) $ im_lookup ("C.) Block " ++ show b ++ " in invs") invs b
   -- else
    --  return $ S.empty 
  -- do one more tau-transformation on the node, as the stored invariant is a 
  -- precondition (not a postcondition) of the node,
  do_tau :: Context -> Int -> State Pred (S.Set VerificationCondition)
  do_tau ctxt b = do
    modify $ tau_blockID ctxt g b Nothing False
    Predicate _ _ vcs _ <- get
    return vcs






-- intialize an empty context based on the command-line parameters
init_context dirname name generate_pdfs = 
  let dirname' = if last dirname  == '/' then dirname else dirname ++ "/" in
    Context IM.empty IM.empty IM.empty (Edges IM.empty) [] dirname' name generate_pdfs IM.empty IM.empty








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

  ds <- liftIO $ parse fname
  put $ ctxt { ctxt_dump = ds }
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
  ctxt <- get
  put $ ctxt { ctxt_entries = Edges $ IM.fromList (zip entries $ repeat IS.empty) }


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
  ctxt <- get
  put $ ctxt { ctxt_inds = IM.empty }
 where
  parse fname = do
    ret0 <- parse_indirections fname
    case ret0 of
      Left err   -> error $ show err
      Right inds -> return inds

-- Delete .calls file
ctxt_init_calls :: StateT Context IO ()
ctxt_init_calls = do
  ctxt <- get
  let dirname     = ctxt_dirname ctxt
  let name        = ctxt_name ctxt
  let fname       = dirname ++ name ++ ".calls"
  liftIO $ writeFile fname ""

-- Read calls from file producing calls :: IM.IntMap Bool
-- Per address a Bool indicating whether the function terminates (True) or returns normally (False)
ctxt_read_calls :: StateT Context IO (IM.IntMap Bool)
ctxt_read_calls = do
  ctxt <- get
  let dirname     = ctxt_dirname ctxt
  let name        = ctxt_name ctxt
  let fname       = dirname ++ name ++ ".calls"

  calls <- do
    exists <- liftIO $ doesFileExist fname
    if exists then
      liftIO $ parse fname
    else do
      liftIO $ writeFile fname ""
      return IM.empty
  put $ ctxt { ctxt_calls = calls }
  return calls
 where
  parse fname = do
    ret0 <- parse_calls fname
    case ret0 of
      Left err -> error $ show err
      Right calls -> return calls




-- TODO generate dot only once
ctxt_generate_cfg :: Int -> StateT Context IO (Either IS.IntSet CFG)
ctxt_generate_cfg entry = do
  -- Generate a CFG, and color it according to strongly connected components
  ctxt <- get
  base <- ctxt_base_name entry
  let log     = base ++ ".log"
  let fname   = base ++ ".dot"
  let pdf     = base ++ ".pdf"
  let do_pdfs = ctxt_generate_pdfs ctxt
  g          <- liftIO $ cfg_gen ctxt entry
  case g of
    Right g -> do
      ctxt <- get


      liftIO $ writeFile fname $ cfg_to_dot ctxt g
      if do_pdfs then do
        liftIO $ callCommand $ "dot -Tpdf " ++ fname ++ " -o " ++ pdf
        to_out $ "Generated CFG, exported to files: " ++ fname ++ " and " ++ pdf
      else do
        to_out $ "Generated CFG, exported to file: " ++ fname 
      return $ Right g
    Left as -> do
      return $ Left as
     


ctxt_generate_invs :: Int -> CFG -> Invariants -> S.Set (NodeInfo,Pred) -> StateT Context IO (Invariants,S.Set (NodeInfo,Pred))
ctxt_generate_invs entry g curr_invs curr_posts = do
  -- Generate invariants
  ctxt <- get
  base       <- ctxt_base_name entry
  let log     = base ++ ".log"
  -- let a       = acode_simp $ cfg_to_acode g 0 IS.empty
  let p       = init_pred curr_invs curr_posts
  let m       = do_prop ctxt g 0 p -- TODO always 0?
  to_log log $ "Generated invariants:"
  to_log log $ show_invariants g m

  let blocks  = IM.keys $ cfg_blocks g
  posts <- mapM (get_post ctxt m) blocks

  return (m, S.fromList $ catMaybes $ posts)
 where
  get_post :: Context -> Invariants -> Int -> StateT Context IO (Maybe (NodeInfo,Pred))
  get_post ctxt invs b = do
    if is_end_node g b then do
      return $ Just  $ (node_info_of ctxt g b, runIdentity $ execStateT (do_tau ctxt b) $ im_lookup ("B.) Block " ++ show b ++ " in invs") invs b)
    else
      return $ Nothing

  -- do one more tau-transformation on the node, as the stored invariant is a 
  -- precondition (not a postcondition) of the node.
  do_tau :: Context -> Int -> State Pred ()
  do_tau ctxt b = modify $ tau_blockID ctxt g b Nothing False


ctxt_add_entries :: Int -> IS.IntSet -> StateT Context IO ()
ctxt_add_entries entry new_entries = modify (\ctxt -> ctxt { ctxt_entries = graph_add_edges (ctxt_entries ctxt) entry new_entries })

ctxt_del_entry :: Int -> StateT Context IO ()
ctxt_del_entry entry = modify (\ctxt -> ctxt { ctxt_entries = graph_delete (ctxt_entries ctxt) entry })

ctxt_add_call a g = do
  ctxt <- get
  let calls       = ctxt_calls ctxt
  let dirname     = ctxt_dirname ctxt
  let name        = ctxt_name ctxt
  let fname       = dirname ++ name ++ ".calls"

  let terminating = all (end_nodes_are_terminal ctxt) $ IM.keys $ cfg_blocks g
  put $ ctxt { ctxt_calls = IM.insert a terminating $ calls }
  to_out $ "Function at entry: " ++ showHex a ++ " analyzed: all nodes are non-returning == " ++ show terminating 
  liftIO $ appendFile fname $ showHex a ++ " " ++ (if terminating then "terminating" else "returning") ++ "\n"
 where
  end_nodes_are_terminal ctxt b = not (IS.null (post g b)) || node_info_of ctxt g b == Terminal

ctxt_add_call_external a = do
  ctxt <- get
  let calls       = ctxt_calls ctxt
  let dirname     = ctxt_dirname ctxt
  let name        = ctxt_name ctxt
  let fname       = dirname ++ name ++ ".calls"

  put $ ctxt { ctxt_calls = IM.insert a False $ calls }
  to_out $ "Function at entry: " ++ showHex a ++ " considered external and normally returning.\n"
  liftIO $ appendFile fname $ showHex a ++ " " ++ "returning" ++ "\n"




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

ctxt_analyze_unresolved_indirections :: CFG -> Invariants -> StateT Context IO Bool
ctxt_analyze_unresolved_indirections g invs = do
  ctxt <- get
  let bs = filter (\b -> node_info_of ctxt g b == UnresolvedIndirection) $ IM.keys $ cfg_blocks g
  if bs == [] then do
    to_out $ "No unresolved indirections."
    return False
  else do
    results <- forM bs try_to_resolve_indirection
    return $ not $ all not results -- prevent lazy execution
 where
  try_to_resolve_indirection b = do
    ctxt <- get
    dirname   <- gets ctxt_dirname
    name      <- gets ctxt_name
    let fname  = dirname ++ name ++ ".indirections" 

    let i                      = last (fetch_block g b)
    let Just trgt              = operand_that_provides_jump_target ctxt i
    let p                      = im_lookup ("A.) Block " ++ show b ++ " in invs") invs b
    let Predicate eqs flg _ _  = p

    let values0 = evalState (try' ctxt b trgt) p
    let values1 = case flagstatus_to_tries flg of
                   Nothing      -> S.empty
                   Just (op1,n) -> S.unions $ map (\n -> evalState (try ctxt b op1 trgt n) p) [0..n-1]
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

  flagstatus_to_tries (FS_CMP (Just True) op1 (Immediate n)) = if n <= fromInteger max_jump_table_size then Just (op1,n) else Nothing
  flagstatus_to_tries _ = Nothing

  -- write an immediate value to operand op1, then run symbolic exection to see if
  -- after executing a block the target-operand is an immediate value as well.
  try ctxt blockId op1 trgt n = do
    write_operand ctxt op1 (SE_Immediate n)
    try' ctxt blockId trgt

  try' ctxt blockId trgt = do
    modify $ tau_blockID ctxt g blockId Nothing True
    val <- read_operand ctxt trgt
    case val of
      SE_Immediate a                     -> return $ S.singleton $ Just $ fromIntegral a
      Bottom (FromNonDeterminism es) _   -> return $ if all (expr_highly_likely_pointer ctxt) es then S.map take_immediates es else S.singleton Nothing
      SE_Var (SP_Mem (SE_Immediate a) _) -> return $ if address_has_symbol ctxt a then S.singleton $ Just $ fromIntegral a else S.singleton $ Just 0 
      e                                  -> return $ S.singleton Nothing

  take_immediates (SE_Immediate a) = Just $ fromIntegral a
  take_immediates _                = Nothing


do_continue :: IO Bool
do_continue = do
  putStr $ "Continue to next round? y/n: "
  hFlush stdout
  str <- getLine
  case str of
    "y" -> return True
    "n" -> return False
    _   -> putStrLn "Invalid input." >> do_continue








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
  "  foxdec 0 ../examples/vi/ vi",
  "",
  "For information on how to generate .dump, .symbols and .entry file, see the README.",
  "Note that:",
  "  - FoxDec reuses results stored in the .calls file (e.g.: ../examples/vi/vi.calls).",
  "    To do a fresh run, remove that file.",
  "  - FoxDec will create various subdirectories in $DIRNAME$."
 ] 


-- Parse the command line arguments and run
main = do
  withParseResult argsParser run


