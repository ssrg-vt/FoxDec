
module WithAbstractSymbolicValues.ResolveIndirections where

import Base

import WithAbstractSymbolicValues.Class
import WithAbstractSymbolicValues.FInit
import WithAbstractSymbolicValues.Sstate
import WithAbstractSymbolicValues.SymbolicExecution

import Data.X86.Opcode
import Data.X86.Instruction
import Data.X86.Register
import Data.JumpTarget
import Data.Indirection
import Data.SymbolicExpression 
import Data.GlobalMem
import Data.VerificationCondition


import qualified Data.Serialize as Cereal
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Set.NonEmpty as NES

import Data.List
import Data.List.Extra (firstJust)
import Data.Word 
import Data.Maybe

import Control.Monad.Extra
import Control.Monad.State.Strict hiding (join)
import Control.DeepSeq

import Debug.Trace


stry_resolve_indirection :: WithAbstractSymbolicValues ctxt bin v p => ctxt -> Sstate v p -> [Instruction] -> Indirections
stry_resolve_indirection ctxt p@(Sstate regs mem gmem flgs) instrs =
  let [trgt] = inSrcs $ last instrs in
    case flagstatus_to_tries 10000 flgs of -- TODO
      [] -> try_to_resolve_syscall (last instrs) `orTry` try_to_resolve_error_call `orTry` try_to_resolve_from_pre trgt p  `orTry` try_to_resolve_AND trgt `orElse` S.singleton Indirection_Unresolved
      bounds ->
        try_to_resolve_syscall (last instrs)
        `orTry` try_to_resolve_error_call
        `orTry` try_to_resolve_using_bounds instrs bounds trgt
        `orTry` try_to_resolve_from_pre trgt p
        `orTry` try_to_resolve_AND trgt
        `orElse` S.singleton Indirection_Unresolved
 where
  -- INDIRECTION RESOLVING TACTIC:
  -- if there is an AND with an immediate $imm$ of the form 2^n-1, then that induces a bound of $imm+1$
  try_to_resolve_AND trgt = 
    case break is_bounding_AND instrs of
      (_,[]) -> Nothing
      (is0,(i:is1)) ->
        let op1 = head $ inDests i
            n   = get_bound_from_AND i in
          try_to_resolve_using_bound is1 op1 (n-1) trgt

  is_bounding_AND (Instruction _ _ AND [op1,Op_Imm (Immediate _ imm)] _ _) = isPower2Minus1 imm
  is_bounding_AND _ = False
  get_bound_from_AND (Instruction _ _ AND [op1,Op_Imm (Immediate _ imm)] _ _) = imm+1


  -- INDIRECTION RESOLVING TACTIC:
  -- If a flg was set y a CMP (or similar), then that induces a bound
  try_to_resolve_using_bounds instrs bounds trgt = firstJust (\(op1,n) -> try_to_resolve_using_bound instrs op1 n trgt) bounds

  try_to_resolve_using_bound instrs op1 n trgt =
    let values1 = map (\n -> runState (evaluate_target_after_setting_value_and_block instrs op1 n trgt) (clean_sstate p, S.empty)) [0..n] in
      if values1 == [] then
        Nothing
      else if all is_jump_table_entry $ map fst values1 then do
        let trgts = map (fromIntegral . get_immediate . S.findMin . fromJust) $ map fst values1 
            vcs   = S.unions $ map (snd . snd) values1
            base  = vcs_to_jump_table_base ctxt vcs
            tbl   = JumpTable op1 (fromIntegral n) trgt (IM.fromList $ zip [0..fromIntegral n] trgts) base in
          Just $ S.singleton $ Indirection_JumpTable tbl
      else
        Nothing

  -- INDIRECTION RESOLVING TACTIC:
  -- Simply try to run the block from the currently known precondition
  try_to_resolve_from_pre trgt p =
    let (trgts,(_,vcs)) = runState (evaluate_target_after_block instrs trgt) (p,S.empty) in
      S.map Indirection_Resolved <$> trgts

  -- INDIRECTION RESOLVING TACTIC:
  -- If the block ends in a syscall, run the block and evaluate RAX
  try_to_resolve_syscall :: Instruction -> Maybe Indirections
  try_to_resolve_syscall i
    | isSyscall (inOperation i) =  S.singleton <$> evalSstate evaluate_syscall_after_block p
    | otherwise = Nothing


  -- INDIRECTION RESOLVING TACTIC:
  -- If the block ends in a call to "error", run the block and evlauate "RDI"
  try_to_resolve_error_call = S.singleton <$> evalSstate evaluate_error_call_after_block p


  -- Set the given operand $op1$ to the given value $n$ and run the block to evaluate the value of $trgt$ after execution of the block
  evaluate_target_after_setting_value_and_block instrs op1 n trgt = do
    sset_rip ctxt (head instrs)
    let ops = get_equivalent_ops flgs [op1]
    mapM_ (\op1 -> swrite_operand ctxt (head instrs) False op1 (simmediate ctxt n)) ops
    evaluate_target_after_block instrs trgt

  get_equivalent_ops flgs ops = 
    case find (is_FS_EQ_to ops) flgs of
      Nothing -> ops
      Just (FS_EQ op0 op1) -> get_equivalent_ops flgs $ nub $ [op0,op1] ++ ops

  is_FS_EQ_to ops (FS_EQ op0 op1) = (op0 `notElem` ops && op1 `elem` ops) || (op0 `elem` ops && op1 `notElem` ops)
  is_FS_EQ_to _ _ = False


  -- Evaluate $trgt$ after running the block (ignore the last instruction)
  evaluate_target_after_block instrs trgt = do
    (p,_) <- get
    sexec_block ctxt True (init instrs) Nothing
    (q,_) <- get
    sset_rip ctxt (last instrs)
    val <- sread_operand ctxt "indirection resolving" trgt
    return $ stry_jump_targets ctxt (last instrs) val -- $  trace ("is = " ++ show (instrs) ++ "P:\n" ++ show p ++ "\nQ:\n" ++ show q ++ "\nval,trgt: " ++ show (val,trgt)) $ 

  flagstatus_to_tries max_tries flgs = concatMap (mk_FS_CMP max_tries) flgs
  mk_FS_CMP max_tries (FS_CMP (Just True) op1 (Op_Imm (Immediate _ n)))
    | n <= fromIntegral max_tries = [(op1,n)]
    | otherwise = []
  mk_FS_CMP _ _ = []


  -- Evaluate RAX after running the block, and try to produce an immediate value
  evaluate_syscall_after_block  = do
    sexec_block ctxt False (init instrs) Nothing
    sset_rip ctxt (last instrs)
    rax <- sread_reg ctxt (Reg64 RAX)
    case stry_immediate ctxt rax of
      Just i -> return $ Just $ Indirection_Resolved $ Syscall $ fromIntegral i
      Nothing -> return $ Just $ Indirection_Unresolved --  error $ show (last instrs, rax)

  
  -- Evaluate RDI after running the block, and try to produce a decision on whether it terminates or not
  evaluate_error_call_after_block = do
    sexec_block ctxt False (init instrs) Nothing
    sset_rip ctxt (last instrs)
    rdi <- sread_reg ctxt (Reg64 RDI)
    return $ stry_resolve_error_call ctxt (last instrs) rdi



  is_jump_table_entry Nothing      = False
  is_jump_table_entry (Just trgts) = S.size trgts == 1 && all is_immediate trgts
  is_immediate (ImmediateAddress _) = True
  is_immediate _                    = False
  get_immediate (ImmediateAddress a) = a

  clean_sstate (Sstate sregs smem gmem fs) = Sstate (clean_regs sregs) (clean_smem smem) gmem fs 
  clean_regs = M.filter (\v -> sis_deterministic ctxt v)
  clean_smem = M.filter (\v -> sis_deterministic ctxt v)


vcs_to_jump_table_base :: WithAbstractSymbolicValues ctxt bin v p => ctxt -> S.Set (VerificationCondition v) -> Word64
vcs_to_jump_table_base ctxt vcs =
  let as = concatMap get_immediate_reads $ S.toList $ vcs in
    case find_distanced_by_four $ sort as of
      Just base -> base
      _ -> error $ "Cannot find jump table base for vcs = " ++ show vcs
 where
  find_distanced_by_four []  = Nothing
  find_distanced_by_four [a] = Nothing 
  find_distanced_by_four (a0:a1:as)
    | a0+4==a1 = Just a0
    | otherwise = find_distanced_by_four (a1:as)


  get_immediate_reads (PointerAnalysis _ (PointerAnalysisResult _ rs)) = mapMaybe (stry_immediate ctxt) $ catMaybes rs
  get_immediate_reads _ = []


