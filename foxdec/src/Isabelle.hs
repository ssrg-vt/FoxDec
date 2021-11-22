{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}

module Isabelle where


import Base
import SimplePred
import Context
import X86_Datastructures
import SymbolicExecution
import MachineState
import CFG_Gen

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List 
import Data.Foldable

import Control.Monad.State.Strict
import Data.Functor.Identity



generate_isa_main_thy name fname imports = do
  let thy_contents = concat $ ["theory ", name, "\n  imports\n"] ++ map (\s -> "  \"" ++ s ++ "\"\n") imports ++ ["\n\nbegin\n\nend\n"]
  liftIO $ writeFile fname thy_contents

generate_isa_thy name entry fname g invs posts vcs = do
  ctxt <- get
  let thy_name = name ++ "_" ++ showHex entry
  -- generate header
  let header   = isa_file_header thy_name
  liftIO $ writeFile fname header
  -- per block, generate sets of Hoare triples
  let all_precs   = all_preconditions ctxt invs posts vcs
  let all_asserts = S.filter is_assertion vcs 
  mapM_ (block_to_hoare_triples ctxt fname entry all_precs all_asserts invs) (IM.toList $ cfg_instrs g)
  -- generate end of file
  liftIO $ appendFile fname $ isa_file_end
  return $ showHex entry ++ "/" ++ thy_name

-- header
isa_file_header thy_name = concat ["theory ", thy_name, "\nimports \"../../../isabelle/X86_Semantics/X86_Parse_Hoare_Triples\"\n\n\nbegin\n\n\ncontext semantics\nbegin\n\n"] -- TODO dir to isa files

-- end 
isa_file_end = "end\nend\n"


-- Generate Hoare triples for a block of instructions
block_to_hoare_triples ctxt fname entry all_precs all_asserts invs (blockId,instrs) = do
  let p = im_lookup ("Block " ++ show blockId ++ " in invs") invs blockId
  liftIO $ appendFile fname $ mk_isa_comment $ mk_block $ "Entry = " ++ showHex entry ++ ", blockId == " ++ show blockId
  liftIO $ appendFile fname $ "\n"
  foldlM (instr_to_hoare_triple ctxt fname all_precs all_asserts) p instrs
  liftIO $ appendFile fname $ "\n\n"
  return ()

-- Generate a single Hoare triple for an instruction with precondition p
instr_to_hoare_triple ctxt fname all_precs all_asserts p i = do
  ctxt <- get
  -- obtain postcondition q
  let q = runIdentity (execStateT (tau_b ctxt [i]) p)
  -- filter separations relevant for p
  let isa_precs = mk_isa_preconditions $ S.unions $ map (get_relevant_precs_for p i) $ S.toList all_precs

  let htriple_name = "ht_" ++ (showHex $ i_addr i)
  liftIO $ appendFile fname $ 
      "htriple "        ++ mk_quote htriple_name ++ "\n" ++
      " Separations \"" ++ isa_precs ++ "\"\n" ++
      " Assertions  \"" ++ mk_isa_asserts i all_asserts ++ "\"\n" ++
      " Pre   \""       ++ mk_pred_for_hoare_triple p ++ "\"\n" ++
      " Instruction \"" ++ mk_instr ctxt i ++ "\"\n" ++
      " Post  \""       ++ mk_pred_for_hoare_triple q ++ "\"\n" ++
      mk_fcs ctxt i p   ++
      "  by (htriple_solver seps: conjI[OF seps asserts,simplified] assms: assms)\n" ++
      "\n\n"
  return q


-- generate a separation relation
mk_isa_separation a0 si0 a1 si1 = parens (intercalate "," [expr_to_isa a0,show si0]) ++ " SEP " ++ parens (intercalate ","  [expr_to_isa a1,show si1])

-- generate a list of separation relations for Preconditions and Assertions
mk_isa_preconditions precs = intercalate "; " $ map mk_isa_precondition $ S.toList precs
mk_isa_precondition (Precondition a0 si0 a1 si1) = mk_isa_separation a0 si0 a1 si1
   
mk_isa_asserts i all_asserts = 
  let a'      = fromIntegral (i_addr i + i_size i)
      asserts = S.filter (\(Assertion a _ _ _ _) -> a == SE_Immediate a') all_asserts in
    intercalate "; " (map mk_assertion $ S.toList asserts)
mk_assertion (Assertion _ a0 si0 a1 si1) = mk_isa_separation a0 si0 a1 si1   


-- generate a predicate
mk_pred_for_hoare_triple (Predicate eqs _ _ _) =
  intercalate " ; " $ mapMaybe mk_eq_for_hoare_triple $ M.toList eqs
 where
  mk_eq_for_hoare_triple (sp,e) =
    if contains_bot_sp sp || (contains_bot e && not (is_return_value_of_call e)) || sp == SP_Reg RIP then
      Nothing
    else
      Just $ sp_to_isa sp ++ " = " ++ expr_to_isa e

-- generate an instruction
mk_instr ctxt i =
  if is_call (i_opcode i) || (is_jump (i_opcode i) && instruction_jumps_to_external ctxt i) then
    let fname = function_name_of_instruction ctxt i
        call  = if is_jump (i_opcode i) then "ExternalCallWithReturn" else "ExternalCall" in
      showHex (i_addr i) ++ ": " ++ call ++ " " ++ mk_safe_isa_fun_name fname ++ " " ++ show (i_size i)
  else
    show i

-- generate function constraints
mk_fcs ctxt i p =
  if is_call (i_opcode i) || (is_jump (i_opcode i) && instruction_jumps_to_external ctxt i) then
    let fname = function_name_of_instruction ctxt i in      
      " FunctionConstraints \"PRESERVES " ++ mk_safe_isa_fun_name fname ++ " {" ++ intercalate ";" (map sp_to_isa $ preserved_stateparts ctxt i p) ++ "}\"\n"
  else
      ""
 where
  preserved_stateparts ctxt i (Predicate eqs _ _ _) =
    map fst $ filter (\(sp,v) -> not (contains_bot_sp sp) && statepart_is_preserved_after_function_call ctxt i sp) $ M.toList eqs


-- gather all preconditions:
--   any precondition occuring in the verification conditions without bot or a return-value of a call
-- and
--   any relation between any stateparts sp0 and sp1 that are assumed to be separate (see function compare_srcs)
all_preconditions ctxt invs posts vcs =
  let precs0 = S.filter is_precondition_without_bot $ S.filter is_precondition vcs
      precs1 = gather_precs_from_stateparts ctxt $ S.toList $ gather_stateparts invs posts in
    S.union precs0 precs1
 where
  gather_precs_from_stateparts ctxt [] = S.empty
  gather_precs_from_stateparts ctxt (sp0:sps) = S.union (S.fromList $ mapMaybe (mk_prec_from_stateparts ctxt sp0) sps) (gather_precs_from_stateparts ctxt sps)
  
  mk_prec_from_stateparts ctxt (SP_Mem a0 si0) (SP_Mem a1 si1) = if compare_srcs ctxt a0 a1 && not (contains_bot a0) && not (contains_bot a1) then Just (Precondition a0 si0 a1 si1) else Nothing
  mk_prec_from_stateparts ctxt _ _ = Nothing

  is_precondition_without_bot (Precondition a0 _ a1 _) = is_return_value_of_call a0 || is_return_value_of_call a1 || (not (contains_bot a0) && not (contains_bot a1))


-- produces the precondition, if any, relevant to predicate p and instruction i
-- the resulting precondition has the region accessed by the instruction on the left, 
-- and a region from the predicdate to the right
get_relevant_precs_for p i prec =
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
    let operands = [i_op1 i,i_op2 i,i_op3 i] ++ map Just (extra_operands i) in
      concatMap (operand_to_statepart p i) operands

  extra_operands i =
    case i_opcode i of 
      PUSH -> [Address $ SizeDir (operand_size (fromJust $ i_op1 i)) (AddrMinus (FromReg RSP) (AddrImm $ fromIntegral $ operand_size (fromJust $ i_op1 i))) ]
      POP  -> [Address $ SizeDir (operand_size (fromJust $ i_op1 i)) (AddrPlus (FromReg RSP) (AddrImm $ operand_size (fromJust $ i_op1 i)))  ]
      _    -> []

  operand_to_statepart p i (Just (Address (SizeDir si a))) = [SP_Mem (evalState (resolve_address_of_operand i a) p) si]
  operand_to_statepart p _ _                               = []

  resolve_address_of_operand i a = do
    write_rreg RIP (SE_Immediate $ fromIntegral $ i_addr i + i_size i)
    resolve_address a




  
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
expr_to_isa (Bottom FromCall srcs) = "bot(" ++ intercalate "," (map src_to_isa $ S.toList srcs) ++ ")"
expr_to_isa e@(Bottom _ _)         = error "Cannot translate " ++ show e ++ " to Isabelle"
expr_to_isa (SE_Var sp)            = sp_to_isa sp ++ "_0"
expr_to_isa (SE_Immediate i)       = if i > 2000 then "0x" ++ showHex i else show i
expr_to_isa (SE_StatePart sp )     = sp_to_isa sp

expr_to_isa (SE_Op (Plus  b) [a0,a1]) = parens (expr_to_isa a0 ++ " " ++ show (Plus  b) ++ " " ++ expr_to_isa a1)
expr_to_isa (SE_Op (Minus b) [a0,a1]) = parens (expr_to_isa a0 ++ " " ++ show (Minus b) ++ " " ++ expr_to_isa a1)
expr_to_isa (SE_Op (Times b) [a0,a1]) = parens (expr_to_isa a0 ++ " " ++ show (Times b) ++ " " ++ expr_to_isa a1)
expr_to_isa (SE_Op (And   b) [a0,a1]) = parens (expr_to_isa a0 ++ " " ++ show (And   b) ++ " " ++ expr_to_isa a1)
expr_to_isa (SE_Op (Or    b) [a0,a1]) = parens (expr_to_isa a0 ++ " " ++ show (Or    b) ++ " " ++ expr_to_isa a1)
expr_to_isa (SE_Op (Xor   b) [a0,a1]) = parens (expr_to_isa a0 ++ " " ++ show (Xor   b) ++ " " ++ expr_to_isa a1)
expr_to_isa (SE_Op op as)           = show op ++ parens (intercalate "," (map expr_to_isa as))
expr_to_isa (SE_Bit i a)            = "b" ++ show i ++ parens (expr_to_isa a)
expr_to_isa (SE_SExtend l h a)      = "signextend(" ++ show l ++ "," ++ show h ++ ", " ++ expr_to_isa a ++ ")"
expr_to_isa (SE_Overwrite i a b)    = "overwrite(" ++ intercalate "," [show i,expr_to_isa a,expr_to_isa b] ++ ")"


sp_to_isa (SP_Reg r)    = show r
sp_to_isa (SP_Mem a si) = "[" ++ expr_to_isa a ++ "," ++ show si ++ "]"

src_to_isa (Src_Function f) = mk_safe_isa_fun_name f
src_to_isa src              = error $ "Cannot translate bottom-source to Isabelle: " ++ show src



parens str = "(" ++ str ++ ")"

mk_quote str = "\"" ++ str ++ "\""

mk_block str = concat [replicate (length str + 6) '#' , "\n## ", str, " ##\n", replicate (length str + 6) '#' ]

mk_isa_comment str = "(*\n" ++ str ++ "\n*)\n"

