
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
import Data.SymbolicExpression (FlagStatus(..)) -- TODO


import qualified Data.Serialize as Cereal
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Set.NonEmpty as NES

import Data.List (intercalate,partition,intersectBy,find,nub)
import Data.Word 
import Data.Maybe

import Control.Monad.Extra
import Control.Monad.State.Strict hiding (join)
import Control.DeepSeq




stry_resolve_indirection :: WithAbstractSymbolicValues ctxt v p => ctxt -> Sstate v p -> [Instruction] -> Indirections
stry_resolve_indirection ctxt p@(Sstate regs mem flg) instrs =
  let [trgt] = srcs $ last instrs in
    case flagstatus_to_tries 10000 flg of -- TODO
      Nothing -> try_to_resolve_error_call `orTry` try_to_resolve_from_pre trgt p `orElse` S.singleton Indirection_Unresolved
      Just (op1,n) -> try_to_resolve_error_call `orTry` try_to_resolve_using_bound (head instrs) op1 n trgt `orTry` try_to_resolve_from_pre trgt p `orElse` S.singleton Indirection_Unresolved

 where
  try_to_resolve_using_bound i op1 n trgt =
    let values1 = map (\n -> evalState (evaluate_target_after_setting_value_and_block i op1 n trgt) (clean_sstate p, S.empty)) [0..n] in
      if values1 == [] then
        Nothing
      else if all is_jump_table_entry values1 then do
        let trgts = map (fromIntegral . get_immediate . S.findMin . fromJust) values1 
            tbl = JumpTable op1 (fromIntegral n) trgt (IM.fromList $ zip [0..fromIntegral n] trgts) in
          Just $ S.singleton $ Indirection_JumpTable tbl
      else
        Nothing

  try_to_resolve_from_pre trgt p =
    let trgts = evalState (evaluate_target_after_block trgt) (p,S.empty) in
      S.map Indirection_Resolved <$> trgts


  evaluate_target_after_setting_value_and_block i op1 n trgt = do
    sset_rip ctxt (head instrs)
    swrite_operand ctxt i False op1 (simmediate ctxt n)
    evaluate_target_after_block  trgt

  evaluate_target_after_block trgt = do
    (p,_) <- get
    sexec_block ctxt False (init instrs) Nothing
    (q,_) <- get
    sset_rip ctxt (last instrs)
    val <- sread_operand ctxt "indirection resolving" trgt
    return $ stry_jump_targets ctxt val -- $ trace ("is = " ++ show (instrs) ++ "P:\n" ++ show p ++ "\nQ:\n" ++ show q ++ "\nval,trgt: " ++ show (val,trgt)) $ 

  flagstatus_to_tries max_tries (FS_CMP (Just True) op1 (Op_Imm (Immediate _ n))) = if n <= fromIntegral max_tries then Just (op1,n) else Nothing
  flagstatus_to_tries max_tries _ = Nothing


  try_to_resolve_error_call = S.singleton <$> evalSstate evaluate_error_call_after_block p

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

  clean_sstate (Sstate sregs smem fs) = Sstate sregs (clean_smem smem) fs
  clean_smem = M.filterWithKey (\(a,si) v -> sis_deterministic ctxt v)




