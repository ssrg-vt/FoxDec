{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}
{-# OPTIONS_HADDOCK hide #-}

module Isabelle where


import Base
import Data.SimplePred
import Analysis.Context
import X86.Register (Register(..))
import X86.Opcode (Opcode(..), isJump, isCall)
import Analysis.SymbolicExecution
import Data.MachineState
import Data.ControlFlow
import Data.Pointers
import VerificationReportInterface
import Typeclasses.HasSize(sizeof)
import X86.Address (GenericAddress(..))
import           Generic.Operand (GenericOperand(..))
import Generic.Address (AddressWord64(AddressWord64))
import qualified X86.Instruction as Instr

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List 
import Data.Foldable

import Control.Monad.State.Strict
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Environment (getArgs)

main = do
  args <- getArgs
  if args == [] then
    putStrLn $ "Usage:\n\n  foxdec-isabelle-exe NAME.report\n\nHere NAME refers to the NAME used when running foxdec-exe.\nRun this program from the same directory foxdec-exe was run."
  else do
    exists <- doesFileExist $ head args
    if exists then do
      ctxt <- ctxt_read_report $ head args
      ctxt_create_hoare_triples ctxt
    else
      putStrLn $ "File: " ++ show (head args) ++ " does not exist."

-- Produce a base file name (without .extension) based on the current entry under consideration
-- If necessary, make the directory
ctxt_base_name :: Context -> Int -> IO String
ctxt_base_name ctxt entry = do
  let dirname  = ctxt_dirname ctxt
  let name     = ctxt_name ctxt
  let entry_dirname = dirname ++ showHex entry ++ "/"
  createDirectoryIfMissing False entry_dirname      
  return $ entry_dirname ++ name


ctxt_create_hoare_triples :: Context -> IO ()
ctxt_create_hoare_triples ctxt = do
  imports <- mapM entry_to_hoare_triples $ IM.keys $ ctxt_calls ctxt

  let dirname = ctxt_dirname ctxt
  let name    = ctxt_name ctxt
  let fname   = dirname ++ name ++ ".thy" 
  generate_isa_main_thy name fname imports
  putStrLn $ "Generated Isabelle thy file file: " ++ fname 
 where
  entry_to_hoare_triples entry = do
    let name     = ctxt_name ctxt
    base        <- ctxt_base_name ctxt entry
    let fname    = base ++ "_" ++ showHex entry ++ ".thy"

    let g        = ctxt_cfgs   ctxt IM.! entry
    let invs     = ctxt_invs   ctxt IM.! entry
    let posts    = ctxt_posts  ctxt IM.! entry
    let vcs      = ctxt_vcs    ctxt IM.! entry
    let fctxt    = mk_fcontext ctxt entry

    putStrLn $ "Generated Isabelle thy file file: " ++ fname 
    generate_isa_thy fctxt name fname g invs posts vcs




generate_isa_main_thy name fname imports = do
  let thy_contents = concat $ ["theory ", name, "\n  imports\n"] ++ map (\s -> "  \"" ++ s ++ "\"\n") imports ++ ["\n\nbegin\n\nend\n"]
  writeFile fname thy_contents

generate_isa_thy fctxt name fname g invs posts vcs = do
  let f        = f_name fctxt
  let entry    = f_entry fctxt
  let finit    = f_init fctxt
  let thy_name = name ++ "_" ++ showHex entry
  -- generate header
  let header   = isa_file_header thy_name
  writeFile fname header
  -- per block, generate sets of Hoare triples
  let all_precs   = all_preconditions fctxt finit invs posts vcs
  let all_asserts = S.filter is_assertion vcs 
  mapM_ (block_to_hoare_triples fctxt fname all_precs all_asserts invs) (IM.toList $ cfg_instrs g)
  -- generate end of file
  appendFile fname $ isa_file_end
  return $ showHex entry ++ "/" ++ thy_name

-- header
isa_file_header thy_name = concat ["theory ", thy_name, "\nimports \"../../isabelle/X86_Semantics/X86_Parse_Hoare_Triples\"\n\n\nbegin\n\n\ncontext semantics\nbegin\n\n"] -- TODO dir to isa files

-- end 
isa_file_end = "end\nend\n"


-- Generate Hoare triples for a block of instructions
block_to_hoare_triples fctxt fname all_precs all_asserts invs (blockId,instrs) = do
  let entry = f_entry fctxt
  let p = im_lookup ("Block " ++ show blockId ++ " in invs") invs blockId
  appendFile fname $ mk_isa_comment $ mk_block $ "Entry = " ++ showHex entry ++ ", blockId == " ++ show blockId
  appendFile fname $ "\n"
  foldlM (instr_to_hoare_triple fctxt fname all_precs all_asserts) p instrs
  appendFile fname $ "\n\n"
  return ()

-- Generate a single Hoare triple for an instruction with precondition p
instr_to_hoare_triple fctxt fname all_precs all_asserts p i = do
  -- obtain postcondition q
  let (q,_) = tau_block fctxt [i] Nothing p
  -- filter separations relevant for p
  let isa_precs = mk_isa_preconditions $ S.unions $ map (get_relevant_precs_for fctxt p i) $ S.toList all_precs

  let htriple_name = "ht_" ++ (showHex $ Instr.addr i)
  appendFile fname $ 
      "htriple "        ++ mk_quote htriple_name ++ "\n" ++
      " Separations \"" ++ isa_precs ++ "\"\n" ++
      -- " Assertions  \"" ++ mk_isa_asserts i all_asserts ++ "\"\n" ++
      " Pre   \""       ++ mk_pred_for_hoare_triple p ++ "\"\n" ++
      " Instruction \"" ++ mk_instr fctxt i ++ "\"\n" ++
      " Post  \""       ++ mk_pred_for_hoare_triple q ++ "\"\n" ++
      mk_fcs fctxt i p   ++
      "  by (htriple_solver seps: conjI[OF seps asserts,simplified] assms: assms)\n" ++
      "\n\n"
  return q


-- generate a separation relation
mk_isa_separation a0 si0 a1 si1 = parens (intercalate "," [expr_to_isa a0,show si0]) ++ " SEP " ++ parens (intercalate ","  [expr_to_isa a1,show si1])

-- generate a list of separation relations for Preconditions and Assertions
mk_isa_preconditions precs = intercalate "; " $ map mk_isa_precondition $ S.toList precs
mk_isa_precondition (Precondition a0 si0 a1 si1) = mk_isa_separation a0 si0 a1 si1
   
mk_isa_asserts i all_asserts = 
  let a'      = Instr.addr i + fromIntegral (sizeof i)
      asserts = S.filter (\(Assertion a _ _ _ _) -> a == SE_Immediate a') all_asserts in
    intercalate "; " (map mk_assertion $ S.toList asserts)
mk_assertion (Assertion _ a0 si0 a1 si1) = mk_isa_separation a0 si0 a1 si1   


-- generate a predicate
mk_pred_for_hoare_triple (Predicate eqs _) =
  intercalate " ; " $ mapMaybe mk_eq_for_hoare_triple $ M.toList eqs
 where
  mk_eq_for_hoare_triple (sp,e) =
    if contains_bot_sp sp ||  contains_bot e || sp == SP_Reg RIP then --  || (contains_bot e && not (is_return_value_of_call e)) 
      Nothing
    else
      Just $ sp_to_isa sp ++ " = " ++ expr_to_isa e

-- generate an instruction
mk_instr fctxt i =
  if isCall (Instr.opcode i) || (isJump (Instr.opcode i) && instruction_jumps_to_external (f_ctxt fctxt) i) then
    let fname = function_name_of_instruction (f_ctxt fctxt) i
        call  = if isJump (Instr.opcode i) then "ExternalCallWithReturn" else "ExternalCall" in
      showHex (Instr.addr i) ++ ": " ++ call ++ " " ++ mk_safe_isa_fun_name fname ++ " " ++ show (sizeof i)
  else
    show i

-- generate function constraints
mk_fcs fctxt i p =
  if isCall (Instr.opcode i) || (isJump (Instr.opcode i) && instruction_jumps_to_external (f_ctxt fctxt) i) then
    let fname = function_name_of_instruction (f_ctxt fctxt) i in      
      " FunctionConstraints \"PRESERVES " ++ mk_safe_isa_fun_name fname ++ " {" ++ intercalate ";" (map sp_to_isa $ preserved_stateparts i p) ++ "}\"\n"
  else
      ""
 where
  preserved_stateparts i (Predicate eqs _) =
     [] -- TODO map fst $ filter (\(sp,v) -> not (contains_bot_sp sp) && statepart_is_preserved_after_function_call ctxt i sp v) $ M.toList eqs


-- gather all preconditions:
--   any precondition occuring in the verification conditions without bot or a return-value of a call
-- and
--   any relation between any stateparts sp0 and sp1 that are assumed to be separate (see function pointers_have_separate_bases)
all_preconditions ctxt finit invs posts vcs =
  let precs0 = S.filter is_precondition_without_bot $ S.filter is_precondition vcs
      precs1 = gather_precs_from_stateparts ctxt $ S.toList $ gather_stateparts invs posts in
    S.union precs0 precs1
 where
  gather_precs_from_stateparts ctxt [] = S.empty
  gather_precs_from_stateparts ctxt (sp0:sps) = S.union (S.fromList $ mapMaybe (mk_prec_from_stateparts ctxt sp0) sps) (gather_precs_from_stateparts ctxt sps)
  
  mk_prec_from_stateparts ctxt (SP_Mem a0 si0) (SP_Mem a1 si1) = if necessarily_separate ctxt a0 si0 a1 si1 then Just (Precondition a0 si0 a1 si1) else Nothing
  mk_prec_from_stateparts ctxt _ _ = Nothing

  is_precondition_without_bot (Precondition a0 _ a1 _) = (not (contains_bot a0) && not (contains_bot a1)) -- is_return_value_of_call a0 || is_return_value_of_call a1 || 


-- produces the precondition, if any, relevant to predicate p and instruction i
-- the resulting precondition has the region accessed by the instruction on the left, 
-- and a region from the predicdate to the right
get_relevant_precs_for ctxt p i prec =
  let sps = instruction_to_stateparts p i in
    S.fromList $ concatMap (get_relevant_prec_for_sp prec) sps
 where
  get_relevant_prec_for_sp prec@(Precondition a0 si0 a1 si1) (SP_Mem a si) = 
    if (a0,si) == (a,si) then
      [prec]
    else if (a1,si1) == (a,si) then
      [Precondition a si a0 si0]
    else if necessarily_enclosed a si a0 si0 then
      [Precondition a si a1 si1]
    else if necessarily_enclosed a si a1 si1 then
      [Precondition a si a0 si0]
    else
      []

  instruction_to_stateparts p i = 
    let operands = Instr.srcs i ++ extra_operands i in
      concatMap (operand_to_statepart p i) operands

  extra_operands i =
    case (Instr.opcode i,Instr.srcs i) of 
      (PUSH,[op1]) -> [Memory (AddressMinus (AddressStorage RSP) (AddressImm $ fromIntegral $ sizeof op1)) (sizeof op1) ]
      (POP, [op1]) -> [Memory (AddressPlus (AddressStorage RSP) (AddressImm $ fromIntegral $ sizeof op1)) (sizeof op1) ]
      _            -> []

  operand_to_statepart p i (Memory a si) = [SP_Mem (evalState (resolve_address_of_operand i a) (p,S.empty)) si]
  operand_to_statepart p _ _             = []

  resolve_address_of_operand i a = do
    write_reg ctxt (Instr.addr i) RIP (SE_Immediate $ Instr.addr i + (fromIntegral $ sizeof i))
    resolve_address ctxt a




  
-- make a safe function name:
-- 	no dollar signs
-- 	starts not with a number or underscore
-- 	ends not with an underscore
mk_safe_isa_fun_name str = 
  dropWhileEnd isUnsafe (map repl $ if head str `elem` "_0123456789" then "fun_" ++ str else str)
 where
   repl c = if isUnsafe c then '_' else c
   isUnsafe '_' = True
   isUnsafe '$' = True
   isUnsafe '@' = True
   isUnsafe c   = False




-- generate an isa-string for an expression
expr_to_isa (Bottom (FromCall f)) = "bot(" ++ f ++ ")"
expr_to_isa (SE_Malloc id h)      = "malloc(" ++ show id ++ ")"
expr_to_isa e@(Bottom _ )         = error $ "Cannot translate " ++ show e ++ " to Isabelle"
expr_to_isa (SE_Var sp)           = sp_to_isa sp ++ "_0"
expr_to_isa (SE_Immediate i)      = if i > 2000 then "0x" ++ showHex i else show i
expr_to_isa (SE_StatePart sp )    = sp_to_isa sp

expr_to_isa (SE_Op (Plus  b) [a0,a1]) = parens (expr_to_isa a0 ++ " " ++ show (Plus  b) ++ " " ++ expr_to_isa a1)
expr_to_isa (SE_Op (Minus b) [a0,a1]) = parens (expr_to_isa a0 ++ " " ++ show (Minus b) ++ " " ++ expr_to_isa a1)
expr_to_isa (SE_Op (Times b) [a0,a1]) = parens (expr_to_isa a0 ++ " " ++ show (Times b) ++ " " ++ expr_to_isa a1)
expr_to_isa (SE_Op (And   b) [a0,a1]) = parens (expr_to_isa a0 ++ " " ++ show (And   b) ++ " " ++ expr_to_isa a1)
expr_to_isa (SE_Op (Or    b) [a0,a1]) = parens (expr_to_isa a0 ++ " " ++ show (Or    b) ++ " " ++ expr_to_isa a1)
expr_to_isa (SE_Op (Xor   b) [a0,a1]) = parens (expr_to_isa a0 ++ " " ++ show (Xor   b) ++ " " ++ expr_to_isa a1)
expr_to_isa (SE_Op op as)             = show op ++ parens (intercalate "," (map expr_to_isa as))
expr_to_isa (SE_Bit i a)              = "b" ++ show i ++ parens (expr_to_isa a)
expr_to_isa (SE_SExtend l h a)        = "signextend(" ++ show l ++ "," ++ show h ++ ", " ++ expr_to_isa a ++ ")"
expr_to_isa (SE_Overwrite i a b)      = "overwrite(" ++ intercalate "," [show i,expr_to_isa a,expr_to_isa b] ++ ")"



sp_to_isa (SP_Reg r)    = show r
sp_to_isa (SP_Mem a si) = "[" ++ expr_to_isa a ++ "," ++ show si ++ "]"

src_to_isa src              = error $ "Cannot translate bottom-source to Isabelle: " ++ show src



parens str = "(" ++ str ++ ")"

mk_quote str = "\"" ++ str ++ "\""

mk_block str = concat [replicate (length str + 6) '#' , "\n## ", str, " ##\n", replicate (length str + 6) '#' ]

mk_isa_comment str = "(*\n" ++ str ++ "\n*)\n"

