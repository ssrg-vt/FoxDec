{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, StrictData #-}

module WithAbstractSymbolicValues.SymbolicExecution where

import Base

import WithAbstractSymbolicValues.Class
import WithAbstractSymbolicValues.FInit
import WithAbstractSymbolicValues.GMem
import WithAbstractSymbolicValues.Sstate

import Data.Size
import Data.X86.Opcode
import Data.X86.Instruction
import Data.X86.Register
import Data.SymbolicExpression (FlagStatus(..)) -- TODO
import Data.JumpTarget
import Data.Indirection
import Data.VerificationCondition
import Data.GlobalMem


import qualified Data.Serialize as Cereal
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Set.NonEmpty as NES

import Data.List 
import Data.Word 
import Data.Maybe

import Control.Monad.Extra
import Control.Monad.State.Strict hiding (join)
import Control.DeepSeq

import GHC.Generics (Generic)
import Debug.Trace




-- | Add a function_pointer_intro to the given symbolic predicate
add_function_pointer a ptr (s,vcs) = 
  let (match,remainder) = S.partition belongs_to_a vcs in
    case S.toList match of
      []                         -> (s,S.insert (FunctionPointers a $ IS.singleton ptr) remainder)
      [FunctionPointers _ ptrs'] -> (s,S.insert (FunctionPointers a $ IS.insert ptr ptrs') remainder)
 where
  belongs_to_a (FunctionPointers a' _) = a == a'
  belongs_to_a _ = False


add_pa_result a par (s,vcs) = (s,S.insert (PointerAnalysis a par) vcs)











-- | Given the address of an operand of an instruction, resolve it given the current state.
sresolve_address :: WithAbstractSymbolicValues ctxt v p => ctxt -> Operand -> State (Sstate v p,VCS v) v
sresolve_address ctxt (Op_Mem si aSi reg idx scale displ (Just seg)) =  do
  ra0 <- sresolve_address ctxt (Op_Mem si (BitSize 64) reg idx scale displ Nothing)
  ra1 <- sread_reg ctxt $ RegSeg seg
  return $ ssemantics ctxt "plus" $ SO_Plus ra0 ra1
sresolve_address ctxt (Op_Mem si (BitSize 64) reg idx scale displ Nothing)
  | scale /= 0 && idx /= RegNone = do
    idx_val <- sread_reg ctxt idx
    let ra1 = ssemantics ctxt "times" $ SO_Times idx_val (simmediate ctxt scale)
    ra0 <- add_base_displ ctxt reg displ
    return $ ssemantics ctxt "plus" $ SO_Plus ra0 ra1
  | otherwise = add_base_displ ctxt reg displ

add_base_displ ctxt RegNone displ = return $ simmediate ctxt (fromIntegral displ :: Word64)
add_base_displ ctxt reg displ = do
  ra0 <- sread_reg ctxt reg
  return $ ssemantics ctxt "plus" $ SO_Plus ra0 $ simmediate ctxt (fromIntegral displ :: Word64) 



sread_operand :: WithAbstractSymbolicValues ctxt v p => ctxt -> String -> Operand -> State (Sstate v p,VCS v) v
--sread_operand ctxt msg op | trace ("sgeneric_cinstr: "++ show op) False = error "trace"
sread_operand ctxt msg    (Op_Imm (Immediate (BitSize 64) a))  = return $ simmediate ctxt a
sread_operand ctxt msg    (Op_Imm (Immediate (BitSize 32) a))  = return $ simmediate ctxt $ sextend_32_64 a
sread_operand ctxt msg    (Op_Imm (Immediate (BitSize 16) a))  = return $ simmediate ctxt $ sextend_16_64 a
sread_operand ctxt msg    (Op_Imm (Immediate (BitSize 8) a))   = return $ simmediate ctxt $ sextend_8_64 a
sread_operand ctxt msg    (Op_Const c)                         = return $ simmediate ctxt $ c
sread_operand ctxt msg    (Op_Reg r)                           = sread_reg ctxt r
sread_operand ctxt msg    (Op_Near op)                         = sread_operand ctxt msg op
sread_operand ctxt msg op@(Op_Mem (BitSize si) _ _ _ _ _ _) = do
  resolved_address <- sresolve_address ctxt op
  sread_mem ctxt msg resolved_address (Just $ ByteSize (si `div` 8))

sread_operand ctxt msg    op  = error $ show (msg,op)

swrite_operand :: WithAbstractSymbolicValues ctxt v p => ctxt -> Instruction -> Bool -> Operand -> v -> State (Sstate v p,VCS v) ()
swrite_operand ctxt i use_existing_value    (Op_Reg r)   v = soverwrite_reg ctxt (show i) use_existing_value r v
swrite_operand ctxt i use_existing_value op@(Op_Mem (BitSize si) _ _ _ _ _ _) v = do
  resolved_address <- sresolve_address ctxt op
  swrite_mem ctxt use_existing_value resolved_address (Just $ ByteSize (si `div` 8)) v
swrite_operand ctxt i _ op v = error $ show (i,op)





-- TODO JE, other JMP aboves and JUMP lesses
add_jump_to_pred :: Instruction -> Instruction -> [FlagStatus] -> [FlagStatus]
add_jump_to_pred i0 i1 flgs
  | inOperation i0 `elem` [JA,JBE] =
    case inOperands i0 of
      [Op_Jmp (Immediate _ trgt)] -> map (mod_FS_CMP trgt) flgs
  | otherwise = flgs
 where
  -- FC_CMP b o1 o2
  -- b == Nothing   means "flag is not used in jump"
  -- b == Just True means "flag is used in jump, we are in path where o1 < o2"
  -- b == Just False means "flag is used in jump, we are in path where o1 >= o2"
  mod_FS_CMP trgt (FS_CMP b o1 o2)
    | inAddress i1 == fromIntegral trgt = FS_CMP jumped_to_target  o1 o2
    | otherwise                         = FS_CMP fall_through_case o1 o2
  mod_FS_CMP trgt flg = flg

  jumped_to_target
    | inOperation i0 == JA  = Just False
    | inOperation i0 == JBE = Just True
  fall_through_case
    | inOperation i0 == JA  = Just True
    | inOperation i0 == JBE = Just False



sreturn :: WithAbstractSymbolicValues ctxt v p => ctxt -> Instruction -> State (Sstate v p,VCS v) ()
sreturn ctxt i = do
  rsp_value <- sread_reg ctxt (Reg64 RSP)
  ret_addr <- sread_operand ctxt "return" $ mk_RSP_mem_operand (ByteSize 8)
  let rsp8 = ssemantics ctxt "plus" $ SO_Plus rsp_value $ simmediate ctxt 8
  swrite_reg ctxt "sret1" (Reg64 RSP) rsp8
  swrite_reg ctxt "sret2" (Reg64 RIP) ret_addr


-- LEA
slea :: WithAbstractSymbolicValues ctxt v p => ctxt -> Instruction -> Operand -> Operand -> State (Sstate v p,VCS v) ()
slea ctxt i dst op = do
  e <- sresolve_address ctxt op
  swrite_operand ctxt i True dst e
  if rip_relative op then
    case stry_immediate ctxt e of
      Nothing  -> return ()
      Just imm -> if (saddress_has_instruction ctxt imm) then modify $ add_function_pointer (inAddress i) $ fromIntegral imm else return ()
  else
    return ()
 where
  rip_relative (Op_Mem _ _ r0 r1 _ _ _) = Reg64 RIP `elem` [r0,r1]

-- MOV
smov :: WithAbstractSymbolicValues ctxt v p => ctxt -> a -> Instruction -> State (Sstate v p,VCS v) ()
smov ctxt a i@(Instruction label prefix MOV (Just dst) [src@(Op_Imm _)] annot)
  -- | saddress_has_instruction ctxt imm   = slea ctxt label dst (EffectiveAddress (AddressImm imm))
  | otherwise                           = sgeneric_cinstr ctxt i
smov ctxt a i                           = sgeneric_cinstr ctxt i


sgeneric_cinstr :: WithAbstractSymbolicValues ctxt v p => ctxt -> Instruction -> State (Sstate v p,VCS v) ()
--sgeneric_cinstr ctxt i | trace ("sgeneric_cinstr: "++ show i) False = error "trace"
sgeneric_cinstr ctxt i@(Instruction label prefix mnemonic (Just dst) srcs annot) = do
  ops <- mapM (sread_operand ctxt (show i)) srcs
  swrite_operand ctxt i True dst $ ssemantics ctxt (show i) $ SO_Op mnemonic (byteSize $ operand_size dst) (byteSize <$> maybe_operand_size srcs) ops


maybe_operand_size []   = Nothing
maybe_operand_size srcs
  | any is_immediate srcs = Nothing
  | otherwise =
    let sizes = map operand_size srcs in
      if all ((==) (head sizes)) (tail sizes) then
        Just $ head sizes
      else
        Nothing
 where
  is_immediate (Op_Imm _) = True
  is_immediate _          = False

sexec_cinstr :: WithAbstractSymbolicValues ctxt v p => ctxt -> Instruction -> State (Sstate v p,VCS v) ()
--sexec_cinstr ctxt i | trace ("sexec_cinstr: "++ show i) False = error "trace"
sexec_cinstr ctxt i@(Instruction label prefix mnemonic (Just dst) srcs annot)
  | mnemonic == LEA                      = slea ctxt i dst $ head srcs
  | mnemonic == MOV                      = smov ctxt (top ctxt "") i
  | mnemonic `elem` xors && dst == head srcs = swrite_operand ctxt i False dst $ simmediate ctxt 0
  | otherwise                                = sgeneric_cinstr ctxt i
 where
  xors = [XOR,PXOR]
sexec_cinstr ctxt i@(Instruction label prefix mnemonic Nothing _ _)
  | isRet mnemonic        = sreturn ctxt i
  | isJump mnemonic       = sjump ctxt i
  | isCall mnemonic       = scall ctxt i
  | otherwise             = return ()






sset_rip :: WithAbstractSymbolicValues ctxt v p => ctxt -> Instruction -> State (Sstate v p,VCS v) ()
sset_rip ctxt i = swrite_reg ctxt "sset_rip" (Reg64 RIP) (simmediate ctxt $ inAddress i + (fromIntegral $ inSize i))

sexec_instr :: WithAbstractSymbolicValues ctxt v p => ctxt -> Bool -> Instruction -> State (Sstate v p,VCS v) ()
--sexec_instr ctxt i | trace ("sexec_isntr: "++ show i) False = error "trace"
sexec_instr ctxt store_pointer_analysis i = do
  sset_rip ctxt i

  let i_c = canonicalize i
  maybe_store_pointer_analysis_result i_c
  mapM_ (sexec_cinstr ctxt) i_c
  swrite_flags ctxt (top ctxt "") i 
 where
  maybe_store_pointer_analysis_result i_c
    | inOperation i == LEA = return ()
    | store_pointer_analysis = do
      par <- gets $ mk_pointer_analysis_result i_c
      case par of
        Nothing  -> return ()
        Just par -> modify $ add_pa_result (inAddress i) par
    | otherwise = return ()

  mk_pointer_analysis_result i_c (p,_) = 
    let (dst,srcs)   = (inDest $ head i_c, inOperands $ head i_c)
        resolved_w   = dst >>= resolve_mem_operand p 
        resolved_rs  = map (resolve_mem_operand p) srcs in
      --if any ((==) (Just $ top ctxt "")) (resolved_w:resolved_rs) then
      --  traceShow ("TOP PTR: " ++ show i_c ++ "\n" ++ show p) $ Just $ PointerAnalysisResult resolved_w resolved_rs
      --else
      if any ((/=) Nothing) (resolved_w:resolved_rs) then
        Just $ PointerAnalysisResult resolved_w resolved_rs
      else
        Nothing


  resolve_mem_operand p op@(Op_Mem si _ base idx scale displ seg) = Just $ evalSstate (sresolve_address ctxt op) p
  resolve_mem_operand p _ = Nothing 



sexec_block :: WithAbstractSymbolicValues ctxt v p => ctxt -> Bool -> [Instruction] -> Maybe [Instruction] -> State (Sstate v p,VCS v) ()
sexec_block ctxt store_pointer_analysis []    _    = return ()
--sexec_block ctxt block next | trace ("sexec_block: "++ show (head block)) False = error "trace"
sexec_block ctxt store_pointer_analysis block next = run
 where 
  run = do
    mapM (sexec_instr ctxt store_pointer_analysis) block
    set_flagstatus next

  set_flagstatus Nothing       = return ()
  set_flagstatus (Just (i':_)) = do
    swrite_reg ctxt "set_rip" (Reg64 RIP) (simmediate ctxt $ inAddress i')
    modify $ \(s,vcs) -> (s { sflags = add_jump_to_pred (last block) i' (sflags s) }, vcs)



sjoin_mem :: WithAbstractSymbolicValues ctxt v p => ctxt -> String -> Sstate v p -> Sstate v p -> M.Map (p, Maybe ByteSize) v
sjoin_mem ctxt msg s0 s1 =
  let shared  = M.intersectionWith join_values (smem s0) (smem s1)
      from_s1 = map (\r -> (r,s0)) $ M.assocs $ M.difference (smem s1) (smem s0)
      from_s0 = map (\r -> (r,s1)) $ M.assocs $ M.difference (smem s0) (smem s1) in
    sjoin_mem' shared $ from_s1 ++ from_s0
 where
  sjoin_mem' m [] = m 
  sjoin_mem' m ((((a,si),v),s'):rest) =
    let v' = evalSstate (sread_mem_from_ptr ctxt ("joinmem" ++ show (a,si,s0,s1)++ "_" ++msg) a si) s'
        j  = join_values v v'
        m' = smem $ execSstate (swrite_mem_to_ptr ctxt True a si j) $ Sstate {sregs = M.empty, smem = m, gmem = GlobalMem IM.empty, sflags = []} in
      sjoin_mem' m' rest

  join_values a b = sjoin_values ctxt ("States (mem value) " ++ show a ++ " joined with " ++ show b ++ "_" ++ msg) [a,b]

sjoin_regs :: WithAbstractSymbolicValues ctxt v p => ctxt -> M.Map Register v -> M.Map Register v -> M.Map Register v
sjoin_regs ctxt r0 r1 = 
  let regs  = M.intersectionWithKey join_reg r0 r1
      from0 = M.mapWithKey (\r v0 -> join_with_init_value v0 r) $ M.difference r0 r1
      from1 = M.mapWithKey (\r v1 -> join_with_init_value v1 r) $ M.difference r1 r0 in
    M.unions [regs,from0,from1]
 where
  join_reg r a b 
    | a == b    = a
    | otherwise = sjoin_values ctxt ("joinregs"++show r) [a,b]

  join_with_init_value v r = sjoin_values ctxt ("joinregs2"++show r) [v, smk_init_reg_value ctxt r]
  


sjoin_gmem :: WithAbstractSymbolicValues ctxt v p => ctxt -> GlobalMem p v -> GlobalMem p v -> GlobalMem p v
sjoin_gmem ctxt (GlobalMem m0) (GlobalMem m1) = 
  let step0 = IM.foldrWithKey (join_entry m1) (GlobalMem IM.empty) m0 in
    IM.foldrWithKey (join_entry m0) step0 m1
 where
  join_entry m1 a0 ma0 j =
    let (si0,isPrecise0) = mk ma0
        touched = get_touched_mem_accesses m1 a0 si0 isPrecise0
        (a_j,ma_j) = foldr1 (join_mem_accesses ctxt) $ (a0,ma0) : touched in
      add_global_mem_access ctxt a_j ma_j j 

  mk (Stores _ si) = (Just $ ByteSize si,True)
  mk (SpansTo _)   = (Nothing,False)






--sjoin_states ctxt msg s0 s1 | trace ("sjoin_states") False = error "trace"
sjoin_states ctxt msg s0@(Sstate regs0 mem0 gmem0 flg0) s1@(Sstate regs1 mem1 gmem1 flg1) =
  let flg  = join_flags flg0 flg1
      regs = sjoin_regs ctxt regs0 regs1
      mem  = sjoin_mem ctxt msg s0 s1
      gmem = sjoin_gmem ctxt gmem0 gmem1
      s'   = Sstate regs mem gmem flg in
     s'
 where
  join_flags = intersect 



-- | The supremum of a list of predicates
supremum :: WithAbstractSymbolicValues ctxt v p => ctxt -> [Sstate v p] -> Sstate v p
supremum ctxt [] = error $ "Cannot compute supremum of []"
supremum ctxt ss = foldr1 (sjoin_states ctxt "supremum") ss

simplies ctxt s0 s1 = 
  let j = set_rip $ sjoin_states ctxt "simplies" (s0 {sflags = []}) (s1 {sflags = []}) in
    if j == set_rip (s0 {sflags = []}) then
      True
    else
      False -- trace ("s0:\n" ++ show s0 ++ "\ns1:\n" ++ show s1 ++ "\nj:\n" ++ show j) False
 where
  set_rip = execSstate (swrite_rreg ctxt "simplies" (Reg64 RIP) (simmediate ctxt 0))



sverify_postcondition :: WithAbstractSymbolicValues ctxt v p => ctxt -> Sstate v p -> Bool
sverify_postcondition ctxt = evalSstate check
 where
  check = do
    rsp <- sread_reg ctxt (Reg64 RSP)
    rip <- sread_reg ctxt (Reg64 RIP)
    return $ scheck_regs_in_postcondition ctxt rip rsp 







