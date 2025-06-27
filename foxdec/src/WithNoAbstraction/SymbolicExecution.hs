{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, FlexibleInstances, StrictData #-}

{-|
Module      : SymbolicPropagation
Description : Provides an instantiation of all functions necessary to do symbolic propagation
-}
module WithNoAbstraction.SymbolicExecution where


import Base
import Config

import Data.L0
import Data.Size
import Data.SValue
import Data.SPointer
import Data.JumpTarget
import Data.GlobalMem
import Data.SymbolicExpression
import Data.Symbol
import Data.VerificationCondition
import Data.Indirection
import Data.X86.Opcode 
import Data.X86.Instruction
import Data.X86.Register


import WithAbstractPredicates.ControlFlow
import Binary.FunctionNames
import WithNoAbstraction.Pointers

import WithAbstractSymbolicValues.Class
import WithAbstractSymbolicValues.SymbolicExecution
import WithAbstractSymbolicValues.Sstate
import WithAbstractSymbolicValues.GMem

import Binary.Generic

import Conventions


import Control.Monad.State.Strict hiding (join)
import Control.Applicative ((<|>))
import Control.Monad.Extra 
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set.NonEmpty as NES
import Data.List
import Data.Word 
import Data.Maybe
import Data.Either
import Data.Foldable (toList)
import Data.Bits (testBit, (.|.), (.&.), xor, complement)
import Control.Applicative (liftA2)

import qualified Data.Serialize as Cereal hiding (get,put)
import Control.DeepSeq
import GHC.Generics

import Debug.Trace



type Static bin v = LiftingEntry bin (Sstate SValue SPointer) (FInit SValue SPointer) v


traceTop msg = id -- trace ("TOP: " ++ msg)

mk_top ""  = Top
mk_top msg = traceTop msg Top

-- Try to get an immediate value from an SValue
ctry_immediate (SConcrete es)
  | NES.size es == 1 = try_imm $ NES.findMin es
  | otherwise        = Nothing
 where
  try_imm (SE_Immediate i) = Just i
  try_imm _                = Nothing
ctry_immediate _ = Nothing

-- Check whether the SValue is deterministic
cis_deterministic (SConcrete es)
  | NES.size es == 1 = not $ contains_bot $ NES.findMin es
  | otherwise        = False
cis_deterministic _ = False





-- Construct an SValue from SExpressions
mk_concrete :: BinaryClass bin => Static bin v -> String -> NES.NESet SimpleExpr -> SValue
---mk_concrete fctxt msg | trace ("mk_concrete: "++ show msg) False = error "trace"
mk_concrete l@(bin,config,l0,_) msg = cap . NES.map simp -- TODO simp should be unnecessary?
 where
  cap es
    | NES.size es > num_cases || any too_large es = cwiden l ("mkconcrete_" ++ msg) $ SConcrete es
    | otherwise = SConcrete es
  num_cases = fromIntegral $ max_num_of_cases config

  too_large e = expr_size e > (fromIntegral $ max_expr_size config)

mk_concreteS fctxt = mk_concrete fctxt "mk_concreteS" . NES.singleton

mk_saddends l@(bin,config,l0,_) = mk . group_immediates l
 where
  mk as
    | NES.size as >= fromIntegral (max_num_of_bases config) = traceTop "mk_saddends" Top
    | otherwise = SAddends as


-- When doing abstraction, immediate values are grouped according to whether they are pointers into the same section.
-- Immediates within the same section are merged into one immediate (the lowest).
-- For example, if the values {0x2050, 0x2052, 0x2054} are abstracted and there is a data section starting at 0x2000 with size 0x100,
-- the value 0x2050 + Top is produced
group_immediates :: BinaryClass bin => Static bin v -> NES.NESet SimpleExpr -> NES.NESet SimpleExpr
group_immediates l@(bin,config,l0,_) addends =
  let (imms,remainder) = S.partition is_imm $ NES.toSet addends in
    if S.null imms then
      addends
    else
      NES.unsafeFromSet $ S.union (S.map merge_imms $ group_imms_by_section imms) remainder
 where
  is_imm (SE_Immediate a) = find_section_for_address bin a /= Nothing
  is_imm _ = False

  group_imms_by_section :: S.Set SimpleExpr -> S.Set (S.Set SimpleExpr)
  group_imms_by_section = quotientBy same_section

  same_section imm0 imm1 = get_section_for (get_imm imm0) == get_section_for (get_imm imm1)

  get_section_for a = find_section_for_address bin a <|> find_section_ending_at bin a

  merge_imms = SE_Immediate . minimum . S.map get_imm 
  get_imm (SE_Immediate imm) = imm


cimmediate :: (Integral i, BinaryClass bin) => Static bin v -> i -> SValue
cimmediate fctxt = mk_concrete fctxt "cimmediate" . NES.singleton . SE_Immediate . fromIntegral



-- JOINING / WIDENING
cwiden :: BinaryClass bin => Static bin v -> String -> SValue -> SValue
--cwiden fctxt msg v | trace ("cwiden: " ++ msg ++ "\n" ++ show v) False = error "trace"
cwiden fctxt msg (SConcrete es) = 
  let ass = NES.map (try_get_base fctxt False) es in
    if Nothing `elem` ass then
      traceTop ("cwiden" ++ show_set es ++ "_" ++ msg) Top
    else
      mk_saddends fctxt $ NES.map fromJust ass
  

cwiden fctxt msg v = v


expr_to_addends :: BinaryClass bin => Static bin v -> SimpleExpr -> [SimpleExpr]
expr_to_addends fctxt@(bin,_,l0,entry) e =
  let adds = M.keys $ M.filter (== 1) $ addends e in
    case filter is_addend adds of
      [] -> [] -- filter is_immediate adds
      as -> as 
 where
  is_addend a@(SE_Immediate _)    = expr_is_global_immediate bin a
  is_addend (Bottom (FromCall f)) = True
  is_addend (SE_Var _)            = True
  is_addend _                     = False


---cjoin fctxt msg v0 v1 | trace ("cjoin: " ++ msg ++ "\n" ++ show (v0,v1)) False = error "trace"
cjoin fctxt msg v0 v1 = join v0 v1
 where
  join v0@(SConcrete es0) v1@(SConcrete es1) = 
    let j = NES.union es0 es1 in
      mk_concrete fctxt ("cjoin1_" ++ msg) j
  join v0@(SAddends as0) v1@(SAddends as1) = mk_saddends fctxt $ NES.union as0 as1
  join v0@(SAddends _)  v1@(SConcrete _) = join v0 (cwiden fctxt ("cjoin2" ++ msg) v1)
  join v0@(SConcrete _) v1@(SAddends _)  = join (cwiden fctxt ("cjoin3" ++ msg) v0) v1
  join Top _ = Top
  join _ Top = Top


cjoin_all :: Foldable t => BinaryClass bin => Static bin v -> String -> t SValue -> SValue
cjoin_all fctxt msg es
  | null es   = error $ "Cannot join [], msg = " ++ msg
  | otherwise = foldr1 (cjoin fctxt msg) es



cjoin_pointers :: BinaryClass bin => Static bin v -> [SPointer] -> [SPointer]
cjoin_pointers fctxt@(bin,_,l0,entry) es =
  case nub es of
    []  -> error $ "Cannot join []"
    [e] -> [e]
    es  -> 
      let bs = nub $ map (try_get_base fctxt False) $ map ptr_to_expr es in
        if Nothing `elem` bs then
          error $ show es ++ "\n" ++ (intercalate "\n" $ map show $ concatMap mk [(e0,e1) | e0 <- es, e1 <- es]) ++ "\n\nFinit:\n" ++ show (fst <$> l0_lookup_entry entry l0)
        else
          map Ptr_Base $ toList $ group_immediates fctxt $ neFromList $ map fromJust bs
 where
  mk (e0,e1)
    | e0 == e1 || cseparate fctxt "" e0 (Just $ ByteSize 1) e1 (Just $ ByteSize 1) = []
    | otherwise = [(e0,e1)]

  ptr_to_expr (Ptr_Concrete e) = e
  ptr_to_expr (Ptr_Base b) = b








-- SYMBOLIC COMPUTATIONS
mk_expr :: BinaryClass bin => Static bin v -> SimpleExpr -> SimpleExpr
mk_expr fctxt e = simp e


svalue_plus fctxt si v0@(SConcrete es0) v1@(SConcrete es1)  = mk_concrete fctxt "plus" $ NES.map (mk_expr fctxt . uncurry f) $ NES.cartesianProduct es0 es1
 where
  f e0 e1 = SE_Op Plus si [e0,e1]
svalue_plus fctxt si v0@(SAddends es0) v1@(SAddends es1)  = mk_saddends fctxt $ NES.union es0 es1 
svalue_plus fctxt si v0@(SConcrete _) v1 = svalue_plus fctxt si (cwiden fctxt "plus" v0) v1
svalue_plus fctxt si v0 v1@(SConcrete _) = svalue_plus fctxt si v0 (cwiden fctxt "plus" v1)
svalue_plus fctxt si v0@(SAddends es0) Top = v0
svalue_plus fctxt si Top v1@(SAddends es0) = v1
svalue_plus _ _      Top Top = Top



svalue_minus fctxt si v0@(SConcrete es0) v1@(SConcrete es1)  = mk_concrete fctxt "minus" $ NES.map (mk_expr fctxt . uncurry f) $ NES.cartesianProduct es0 es1
 where
  f e0 e1 = SE_Op Minus si [e0,e1]
svalue_minus fctxt si v0@(SConcrete _) _ = cwiden fctxt "minus" v0
svalue_minus fctxt si v0@(SAddends _) _  = v0 -- TODO what v1 = ( x- ptr )
svalue_minus _ _ Top _ = Top

svalue_and fctxt si v0@(SConcrete es0) v1@(SConcrete es1)  = mk_concrete fctxt "and" $ NES.map (mk_expr fctxt . uncurry f) $ NES.cartesianProduct es0 es1
 where
  f e0 e1 = SE_Op And si [e0,e1]
svalue_and _ _ _ _ = Top

svalue_unop fctxt msg f v0@(SConcrete es0) = mk_concrete fctxt msg $ NES.map (mk_expr fctxt . f) es0
svalue_unop fctxt msg f v0@(SAddends  as0) = traceTop "unop" Top
svalue_unop fctxt msg f Top = Top


svalue_takebits fctxt h   = svalue_unop fctxt "takebits" (SE_Bit h)
svalue_sextend  fctxt l h = svalue_unop fctxt "sextend"  (SE_SExtend l h)

svalue_apply :: BinaryClass bin => Static bin v -> String -> ([SimpleExpr] -> SimpleExpr) -> [SValue] -> SValue
svalue_apply fctxt msg f es
  | any (not . isConcrete) es = traceTop "svalue_apply" Top
  -- | Top `elem` es = Top
  | otherwise =
  let args = map mk_args es
      prod = crossProduct args
      app  = map (mk_expr fctxt . f) prod in
    mk_concrete fctxt "apply" $ neFromList app 
 where
  mk_args (SConcrete es) = S.toList $ NES.toSet es

  isConcrete (SConcrete es) = True
  isConcrete _ = False
  

data CSemantics = ApplyPlus Int | ApplyMinus Int | ApplyNeg Int | ApplyDec Int | ApplyInc Int | ApplyAnd Int |
                  ApplyMov | ApplyCMov | ApplySExtend Int Int |
                  Apply ([SimpleExpr] -> SimpleExpr) | SetXX | SExtension_HI | NoSemantics



csemantics :: BinaryClass bin => Static bin v -> String -> SymbolicOperation SValue -> SValue
--csemantics fctxt msg _ | trace ("csemantics: "++ msg) False = error "trace"
csemantics fctxt msg (SO_Plus  a b)         = svalue_plus fctxt 64 a b
csemantics fctxt msg (SO_Minus a b)         = svalue_minus fctxt 64 a b
csemantics fctxt msg (SO_Times a b)         = svalue_apply fctxt "times" (mk_expr fctxt . SE_Op Times 64) [a,b]
csemantics fctxt msg (SO_Overwrite n a b)   = svalue_apply fctxt "overwrite" (\[e0,e1] -> mk_expr fctxt $ SE_Overwrite n e0 e1) [a,b]
csemantics fctxt msg (SO_SExtend l h a)     = svalue_sextend fctxt l h a
csemantics fctxt msg (SO_Bit h a)           = svalue_takebits fctxt h a
csemantics fctxt msg (SO_Op op si si' es)   = 
  case mnemonic_to_semantics op (8*si) (((*) 8) <$> si') of
    ApplyMov         -> es!!0
    ApplyCMov        -> cjoin        fctxt "cmov" (es!!0) (es!!1)
    ApplyPlus  si    -> svalue_plus  fctxt si (es!!0) (es!!1)
    ApplyInc   si    -> svalue_plus  fctxt si (es!!0) (cimmediate fctxt 1)
    ApplyMinus si    -> svalue_minus fctxt si (es!!0) (es!!1)
    ApplyDec   si    -> svalue_minus fctxt si (es!!0) (cimmediate fctxt 1)
    ApplyNeg   si    -> svalue_minus fctxt si (cimmediate fctxt 0) (es!!0)
    ApplyAnd   si    -> svalue_and   fctxt si (es!!0) (es!!1)
    ApplySExtend l h -> svalue_sextend fctxt l h (es!!0)
    Apply sop        -> svalue_apply fctxt (msg ++ ", op = " ++ show op) (mk_expr fctxt . sop) es
    SetXX            -> mk_concrete fctxt "setxx"   $ neFromList [SE_Immediate 0,SE_Immediate 1]
    SExtension_HI    -> mk_concrete fctxt "sextend" $ neFromList [SE_Immediate 0,SE_Immediate 18446744073709551615]
    NoSemantics      -> mk_top ("Widening due to operand: " ++ show op)
  






mnemonic_to_semantics SUB si si'     = ApplyMinus si
mnemonic_to_semantics NEG si si'     = ApplyNeg si
mnemonic_to_semantics DEC si si'     = ApplyDec si

mnemonic_to_semantics ADD si si'     = ApplyPlus si
mnemonic_to_semantics INC si si'     = ApplyInc si
mnemonic_to_semantics XADD si si'    = error $ "TODO: XADD"

mnemonic_to_semantics IMUL_LO si si' = Apply $ SE_Op Times si
mnemonic_to_semantics SHL si si'     = NoSemantics -- Apply $ shl
 where
  shl [a,SE_Immediate i] = NoSemantics -- SE_Op Times si [a,SE_Immediate $ 2^i]
  shl [a,b]              = NoSemantics -- SE_Op Shl si [a,b]

mnemonic_to_semantics IDIV_LO si si' = Apply $ SE_Op SdivLo si
mnemonic_to_semantics SAR si si'     = NoSemantics -- Apply $ sar
 where
  sar [a,SE_Immediate i] = NoSemantics -- SE_Op Sdiv si [a,SE_Immediate $ 2^i]
  sar [a,b]              = NoSemantics -- SE_Op Sar si [a,b]


mnemonic_to_semantics DIV_LO si si'  = NoSemantics -- Apply $ SE_Op UdivLo si
mnemonic_to_semantics SHR si si'     = NoSemantics -- Apply $ shr
 where
  shr [a,SE_Immediate i] = NoSemantics -- SE_Op Udiv si [a,SE_Immediate $ 2^i]
  shr [a,b]              = NoSemantics -- SE_Op Shr si [a,b]



mnemonic_to_semantics BSR si si'     = NoSemantics -- Apply $ (\[a] -> SE_Op Bsr si [SE_Immediate 0,a])
mnemonic_to_semantics ROL si si'     = NoSemantics -- Apply $ SE_Op Rol si
mnemonic_to_semantics ROR si si'     = NoSemantics -- Apply $ SE_Op Ror si
mnemonic_to_semantics BSWAP si si'   = NoSemantics -- Apply $ SE_Op Bswap si

mnemonic_to_semantics PEXTRB si si'  = NoSemantics -- Apply $ (\[a,b,c] -> SE_Op Pextr si [a,b,c])
mnemonic_to_semantics PEXTRD si si'  = NoSemantics -- Apply $ (\[a,b,c] -> SE_Op Pextr si [a,b,c])
mnemonic_to_semantics PEXTRQ si si'  = NoSemantics -- Apply $ (\[a,b,c] -> SE_Op Pextr si [a,b,c])

mnemonic_to_semantics AND si si'     = ApplyAnd si
mnemonic_to_semantics OR  si si'     = NoSemantics -- Apply $ SE_Op Or si
mnemonic_to_semantics NOT si si'     = NoSemantics -- Apply $ (\[a] -> SE_Op Not si [a])

mnemonic_to_semantics XOR    si si'  = Apply $ SE_Op Xor si
mnemonic_to_semantics PXOR   si si'  = Apply $ SE_Op Xor si
mnemonic_to_semantics VPXOR  si si'  = Apply $ SE_Op Xor si
mnemonic_to_semantics XORPS  si si'  = Apply $ SE_Op Xor si
mnemonic_to_semantics XORPD  si si'  = Apply $ SE_Op Xor si

mnemonic_to_semantics MOV     si si'  = ApplyMov
mnemonic_to_semantics MOVSD   si si'  = ApplyMov -- TODO only if string operation
mnemonic_to_semantics MOVSS   si si'  = ApplyMov
mnemonic_to_semantics MOVAPS  si si'  = ApplyMov
mnemonic_to_semantics MOVAPD  si si'  = ApplyMov
mnemonic_to_semantics MOVUPS  si si'  = ApplyMov
mnemonic_to_semantics MOVUPD  si si'  = ApplyMov
mnemonic_to_semantics MOVABS  si si'  = ApplyMov
mnemonic_to_semantics MOVDQU  si si'  = ApplyMov
mnemonic_to_semantics MOVDQA  si si'  = ApplyMov
mnemonic_to_semantics MOVLPD  si si'  = ApplyMov
mnemonic_to_semantics MOVD    si si'  = ApplyMov
mnemonic_to_semantics MOVQ    si si'  = ApplyMov -- TODO if prefix = Nothing?
mnemonic_to_semantics VMOVD   si si'  = ApplyMov 
mnemonic_to_semantics VMOVAPD si si'  = ApplyMov
mnemonic_to_semantics VMOVAPS si si'  = ApplyMov
mnemonic_to_semantics MOVZX   si si'  = ApplyMov

mnemonic_to_semantics MOVSX  si (Just si') = ApplySExtend si' si
mnemonic_to_semantics MOVSXD si (Just si') = ApplySExtend si' si
mnemonic_to_semantics CDQE   si (Just si') = ApplySExtend si' si
mnemonic_to_semantics CWDE   si (Just si') = ApplySExtend si' si
mnemonic_to_semantics CBW    si (Just si') = ApplySExtend si' si

mnemonic_to_semantics CWD    si (Just si') = SExtension_HI
mnemonic_to_semantics CDQ    si (Just si') = SExtension_HI
mnemonic_to_semantics CQO    si (Just si') = SExtension_HI


mnemonic_to_semantics SETO   si si' = SetXX 
mnemonic_to_semantics SETNO  si si' = SetXX
mnemonic_to_semantics SETS   si si' = SetXX
mnemonic_to_semantics SETNS  si si' = SetXX
mnemonic_to_semantics SETE   si si' = SetXX
mnemonic_to_semantics SETZ   si si' = SetXX
mnemonic_to_semantics SETNE  si si' = SetXX
mnemonic_to_semantics SETNZ  si si' = SetXX
mnemonic_to_semantics SETB   si si' = SetXX
mnemonic_to_semantics SETNAE si si' = SetXX
mnemonic_to_semantics SETC   si si' = SetXX
mnemonic_to_semantics SETNB  si si' = SetXX
mnemonic_to_semantics SETAE  si si' = SetXX
mnemonic_to_semantics SETNC  si si' = SetXX
mnemonic_to_semantics SETBE  si si' = SetXX
mnemonic_to_semantics SETNA  si si' = SetXX
mnemonic_to_semantics SETA   si si' = SetXX
mnemonic_to_semantics SETNBE si si' = SetXX
mnemonic_to_semantics SETL   si si' = SetXX
mnemonic_to_semantics SETNGE si si' = SetXX
mnemonic_to_semantics SETG   si si' = SetXX
mnemonic_to_semantics SETGE  si si' = SetXX
mnemonic_to_semantics SETNL  si si' = SetXX
mnemonic_to_semantics SETLE  si si' = SetXX
mnemonic_to_semantics SETNG  si si' = SetXX
mnemonic_to_semantics SETNLE si si' = SetXX
mnemonic_to_semantics SETP   si si' = SetXX
mnemonic_to_semantics SETPE  si si' = SetXX
mnemonic_to_semantics SETNP  si si' = SetXX
mnemonic_to_semantics SETPO  si si' = SetXX

mnemonic_to_semantics CMOVO   si si' = ApplyCMov 
mnemonic_to_semantics CMOVNO  si si' = ApplyCMov
mnemonic_to_semantics CMOVS   si si' = ApplyCMov
mnemonic_to_semantics CMOVNS  si si' = ApplyCMov
mnemonic_to_semantics CMOVE   si si' = ApplyCMov
mnemonic_to_semantics CMOVZ   si si' = ApplyCMov
mnemonic_to_semantics CMOVNE  si si' = ApplyCMov
mnemonic_to_semantics CMOVNZ  si si' = ApplyCMov
mnemonic_to_semantics CMOVB   si si' = ApplyCMov
mnemonic_to_semantics CMOVNAE si si' = ApplyCMov
mnemonic_to_semantics CMOVC   si si' = ApplyCMov
mnemonic_to_semantics CMOVNB  si si' = ApplyCMov
mnemonic_to_semantics CMOVAE  si si' = ApplyCMov
mnemonic_to_semantics CMOVNC  si si' = ApplyCMov
mnemonic_to_semantics CMOVBE  si si' = ApplyCMov
mnemonic_to_semantics CMOVNA  si si' = ApplyCMov
mnemonic_to_semantics CMOVA   si si' = ApplyCMov
mnemonic_to_semantics CMOVNBE si si' = ApplyCMov
mnemonic_to_semantics CMOVL   si si' = ApplyCMov
mnemonic_to_semantics CMOVNGE si si' = ApplyCMov
mnemonic_to_semantics CMOVG   si si' = ApplyCMov
mnemonic_to_semantics CMOVGE  si si' = ApplyCMov
mnemonic_to_semantics CMOVNL  si si' = ApplyCMov
mnemonic_to_semantics CMOVLE  si si' = ApplyCMov
mnemonic_to_semantics CMOVNG  si si' = ApplyCMov
mnemonic_to_semantics CMOVNLE si si' = ApplyCMov
mnemonic_to_semantics CMOVP   si si' = ApplyCMov
mnemonic_to_semantics CMOVPE  si si' = ApplyCMov
mnemonic_to_semantics CMOVNP  si si' = ApplyCMov
mnemonic_to_semantics CMOVPO  si si' = ApplyCMov
--TODO TEST
--TODO other sign extension thingies
mnemonic_to_semantics _      _ _  = NoSemantics




replace_rip_in_operand rip op@(Op_Mem si (Reg64 RIP) RegNone 0 displ Nothing info) = Op_Mem si RegNone RegNone 0 (fromIntegral $ fromIntegral rip + displ) Nothing info
replace_rip_in_operand rip op@(Op_Mem si (Reg64 RIP) _ _ displ _ _) = error $ "TODO: " ++ show op
replace_rip_in_operand rip op = op



cflg_semantics :: BinaryClass bin => Static bin v -> a -> Instruction -> [FlagStatus] -> [FlagStatus]
cflg_semantics fctxt _ i@(Instruction label prefix mnemonic ops info annot) flgs = flg mnemonic
 where
  mk_operand = replace_rip_in_operand (inAddress i + (fromIntegral $ inSize i))

  flg CMP      = [FS_CMP Nothing (mk_operand $ ops!!0) (mk_operand $ ops!!1)] ++ filter (not . is_FS_CMP) flgs
  flg SUB      = [FS_CMP Nothing (mk_operand $ ops!!0) (mk_operand $ ops!!1)] ++ filter (not . is_FS_CMP) flgs
  flg MOV      = [FS_EQ (mk_operand $ ops!!0) (mk_operand $ ops!!1)] ++ flgs

  flg _
    | WritesToFlags `elem` info = []
    | otherwise                 = flgs


-- MAKING POINTERS FROM EXPRESSIONS


try_get_base :: BinaryClass bin => Static bin v -> Bool -> SimpleExpr -> Maybe SimpleExpr
try_get_base (bin,_,l0,entry) strict a = (mk_base $ get_pointer_base_set bin get_finit a) `orTry` (try_mk_addition $ filter is_possible_base $ M.keys $ M.filter (== 1) $ addends a)
 where
  mk_base :: S.Set PointerBase -> Maybe SimpleExpr
  mk_base bs
    | S.null bs = Nothing
    | otherwise = Just $ foldr1 plus $ S.map base_to_expr bs

  try_mk_addition :: [SimpleExpr] -> Maybe SimpleExpr
  try_mk_addition [] = Nothing
  try_mk_addition as = Just $ foldr1 plus as
  plus a b = SE_Op Plus 64 [a,b]

    
  is_possible_base (SE_Var sp) = not strict
  is_possible_base (Bottom (FromCall f)) = not strict
  is_possible_base _ = False

  get_finit =
    case l0_lookup_entry entry l0 of
      Just (finit,_) -> finit



base_to_expr (StackPointer)        = SE_Var $ SP_Reg (Reg64 RSP)
base_to_expr (Malloc id hash)      = SE_Malloc id hash
base_to_expr (GlobalAddress a)     = SE_Immediate a
base_to_expr ThreadLocalStorage    = SE_Var (SP_Reg $ RegSeg FS)
base_to_expr (BaseIsStatePart sp)  = SE_Var sp


cmk_mem_addresses :: BinaryClass bin => Static bin v -> String -> Bool -> SValue -> S.Set SPointer
cmk_mem_addresses l@(bin,_,l0,entry) msg strict v@(SConcrete es) = 
  let es' = NES.filter could_be_pointer es
      bs  = S.map (try_get_base l strict) es' in
    if any ((==) Nothing) bs then -- TODO check if all is OK
      S.empty
    else
      S.map Ptr_Concrete es'
 where
  could_be_pointer (SE_Immediate imm) = is_roughly_an_address bin imm 
  could_be_pointer _ = True
cmk_mem_addresses fctxt msg strict v@(SAddends as) = 
  let bs = NES.map try_get_base_from_addends as in
    if any ((==) Nothing) bs then
     S.empty
    else
      S.map (Ptr_Base . fromJust) $ NES.toSet bs
 where
  try_get_base_from_addends a = try_get_base fctxt strict a

cmk_mem_addresses _ _ _ Top = S.empty


-- MAKING INITIAL VALUES

cmk_init_reg_value :: BinaryClass bin => Static bin v  -> Register -> SValue
cmk_init_reg_value fctxt = mk_concreteS fctxt . SE_Var . SP_Reg


-- Concert SValues to SExpressions
cmk_init_mem_value :: BinaryClass bin => Static bin v -> String -> SPointer -> Maybe ByteSize -> SValue
cmk_init_mem_value fctxt msg (Ptr_Concrete a) si = mk_concreteS fctxt $ SE_Var $ SP_Mem a $ mk_si si
 where
  mk_si (Just (ByteSize imm)) = imm
cmk_init_mem_value fctxt msg (Ptr_Base b) si = Top





-- MEMORY RELATIONS
cseparate :: BinaryClass bin => Static bin v -> String -> SPointer -> Maybe ByteSize -> SPointer -> Maybe ByteSize -> Bool
---cseparate fctxt msg v0 s0 v1 si1 | trace ("cseparate: "++ show (v0,v1)) False = error "trace"
cseparate ctxt@(bin,_,_,_) msg (Ptr_Concrete a0) si0 (Ptr_Concrete a1) si1 = necessarily_separate bin (lookup_finit ctxt) msg a0 (mk_si si0) a1 (mk_si si1)
 where
  mk_si (Just (ByteSize si)) = Just $ SE_Immediate $ fromIntegral si
  mk_si _                    = Nothing
cseparate ctxt@(bin,_,_,_) msg (Ptr_Base a0)     si0 (Ptr_Base a1)     si1 = necessarily_separate bin (lookup_finit ctxt) msg a0 Nothing a1 Nothing
cseparate ctxt@(bin,_,_,_) msg (Ptr_Base a0)     si0 (Ptr_Concrete a1) si1 = necessarily_separate bin (lookup_finit ctxt) msg a0 Nothing a1 Nothing
cseparate ctxt@(bin,_,_,_) msg (Ptr_Concrete a0) si0 (Ptr_Base a1)     si1 = necessarily_separate bin (lookup_finit ctxt) msg a0 Nothing a1 Nothing
cseparate ctxt msg Ptr_Top _ _ _ = False
cseparate ctxt msg _ _ Ptr_Top _ = False

lookup_finit (bin,_,l0,entry) = 
  case l0_lookup_entry entry l0 of
    Just (finit,_) -> finit

calias fctxt a0 si0 a1 si1 = si0==si1 && a0==a1 -- necessarily_equal a0 a1


cnecessarily_enclosed fctxt (Ptr_Concrete a0) (Just (ByteSize si0)) (Ptr_Concrete a1) (Just (ByteSize si1)) = necessarily_enclosed a0 si0 a1 si1
cnecessarily_enclosed _ _ _ _ _ = False


csensitive (bin,_,_,_) (Ptr_Concrete a) (Just (ByteSize si)) v = is_top_stackframe a (Just (ByteSize si)) || is_pushed_reg a v || is_function_pointer bin v
 where
  is_initial_reg (SE_Var (SP_Reg _)) = True
  is_initial_reg _                   = False
  
  is_pushed_reg a' (SConcrete vs) = all is_initial_reg vs && expr_is_highly_likely_local_pointer bin a'
  is_pushed_reg a' _ = False

csensitive fctxt _ _ _ = False


cis_local (bin,_,_,_) (Ptr_Concrete a) = expr_is_maybe_local_pointer bin a
cis_local (bin,_,_,_) (Ptr_Base a) = expr_is_maybe_local_pointer bin a
cis_local _ Ptr_Top = False




is_function_pointer bin (SConcrete vs) = all is_function_pointer_expr vs
 where
  is_function_pointer_expr (SE_Immediate imm) = address_has_instruction bin imm
  is_function_pointer_expr _                  = False
is_function_pointer bin _              = False

check_regs_in_postcondition :: BinaryClass bin => Static bin v -> SValue -> SValue -> Bool
check_regs_in_postcondition ctxt rip rsp = and [ rsp_check rsp, rip_check rip ]
 where
  -- check: is RSP restored to RSP_0 + 8 ?
  rsp_check rsp@(SConcrete es)
    | cis_deterministic rsp = rsp_check_value $ NES.findMin es
    | otherwise = False
  rsp_check _ = False


  rsp_check_value (SE_Op Plus _ [SE_Var (SP_Reg (Reg64 RSP)), SE_Immediate 0x8]) = True 
  rsp_check_value (SE_Op Minus _ [SE_Var (SP_Reg (Reg64 RSP)), SE_Immediate 0xfffffffffffffff8]) = True 
  rsp_check_value _ = False


  -- check: is RIP set to the value originally stored at the top of the stack frame?
  rip_check rip@(SConcrete es)
    | cis_deterministic rip = rip_check_value $ NES.findMin es
    | otherwise = False
  rip_check _ = False


  rip_check_value (SE_Var (SP_Mem (SE_Var (SP_Reg (Reg64 RSP))) 8)) = True
  rip_check_value _ = False


ctry_resolve_error_call ctxt@(bin,config,l0,_) i rdi
  | isCall (inOperation i) || (isJump (inOperation i) && jump_is_actually_a_call (bin,config,l0) i) =
    case jump_target_for_instruction bin i of
      External "error" ->
        case ctry_immediate rdi of
          Just imm -> Just $ Indirection_Resolved $ Returns (imm==0)
          Nothing  -> Just $ Indirection_Resolved $ Returns True
      _ -> Nothing
  | otherwise = Nothing

ctry_global ctxt@(bin,_,_,_) p =
 case p of
   Ptr_Concrete a -> 
     case a of
       e@(SE_Immediate imm) -> if expr_is_global_immediate bin e then Just (fromIntegral imm,True) else Nothing
       _ -> Nothing
   Ptr_Base b -> get_global b
 where
  get_global v =
    case filter is_global_base $ S.toList $ get_pointer_base_set bin empty_finit v of
      [GlobalAddress a] -> Just (fromIntegral a,False)
      _ -> Nothing

  is_global_base (GlobalAddress _) = True
  is_global_base _ = False



  


instance BinaryClass bin => WithAbstractSymbolicValues (Static bin v) bin SValue SPointer where
  sseparate                = cseparate
  senclosed                = cnecessarily_enclosed
  salias                   = calias
  ssensitive               = csensitive
  smk_mem_addresses        = cmk_mem_addresses
  sjoin_values             = cjoin_all
  swiden_values            = cwiden
  sjoin_pointers           = cjoin_pointers
  ssemantics               = csemantics
  sflg_semantics           = cflg_semantics
  simmediate               = cimmediate
  top                      = \_ -> mk_top
  smk_init_reg_value       = cmk_init_reg_value
  smk_init_mem_value       = cmk_init_mem_value
  simmediate_to_pointer    = \_ -> Ptr_Concrete . SE_Immediate
  sjump                    = jump
  scall                    = call
  stry_jump_targets        = ctry_jump_targets
  stry_immediate           = \_ -> ctry_immediate 
  sis_deterministic        = \_ -> cis_deterministic 
  saddress_has_instruction = \(bin,_,_,_)-> address_has_instruction bin
  scheck_regs_in_postcondition = check_regs_in_postcondition
  stry_resolve_error_call  = ctry_resolve_error_call
  stry_global              = ctry_global
  sget_gmem_structure      = \(_,_,l0,entry) -> l0_gmem_structure l0 
  sget_binary              = \(bin,_,_,__) -> bin 




mapMaybeS :: Ord b => (a -> Maybe b) -> S.Set a -> S.Set b
mapMaybeS f = S.map fromJust . S.filter ((/=) Nothing) . S.map f

mapMaybeNES :: Ord b => (a -> Maybe b) -> NES.NESet a -> S.Set b
mapMaybeNES f = S.map fromJust . NES.filter ((/=) Nothing) . NES.map f

























data ExternalFunctionOutput = FreshPointer | UnknownReturnValue | Input Register

data ExternalFunctionBehavior = ExternalFunctionBehavior {
  f_inputs :: [Register],
  f_output :: ExternalFunctionOutput
 }


param 0 = Reg64 RDI 
param 1 = Reg64 RSI
param 2 = Reg64 RDX
param 3 = Reg64 RCX
param 4 = Reg64 R8
param 5 = Reg64 R9


pure_and_fresh = ExternalFunctionBehavior [] FreshPointer
pure_and_unknown = ExternalFunctionBehavior [] UnknownReturnValue

external_function_behavior :: String -> ExternalFunctionBehavior
-- | a list of some function that return a heap-pointer through RAX.
-- The pointer is assumed to  be fresh.
external_function_behavior "_malloc" = pure_and_fresh
external_function_behavior "malloc" = pure_and_fresh
external_function_behavior "_malloc_create_zone" = pure_and_fresh
external_function_behavior "_malloc_default_zone" = pure_and_fresh
external_function_behavior "_malloc_zone_malloc" = pure_and_fresh
external_function_behavior "isc__mem_allocate" = pure_and_fresh
external_function_behavior "isc_mem_allocate" = pure_and_fresh
external_function_behavior "PyType_GenericAlloc" = pure_and_fresh
external_function_behavior "PyType_GenericNew" = pure_and_fresh
external_function_behavior "PySys_GetObject" = pure_and_fresh
external_function_behavior "aligned_alloc" = pure_and_fresh
external_function_behavior "_calloc" = pure_and_fresh
external_function_behavior "calloc" = pure_and_fresh
external_function_behavior "_malloc_zone_calloc" = pure_and_fresh
external_function_behavior "_mmap" = pure_and_fresh
external_function_behavior "_av_mallocz" = pure_and_fresh
external_function_behavior "___error" = pure_and_fresh
external_function_behavior "_localeconv" = pure_and_fresh
external_function_behavior "localeconv" = pure_and_fresh
external_function_behavior "strerror" = pure_and_fresh
external_function_behavior "_strerror" = pure_and_fresh
external_function_behavior "_strerror_r" = pure_and_fresh
external_function_behavior "_wcserror" = pure_and_fresh
external_function_behavior "__wcserror" = pure_and_fresh
external_function_behavior "_EVP_CIPHER_CTX_new" = pure_and_fresh
external_function_behavior "strdup" = pure_and_fresh
external_function_behavior "_strdup" = pure_and_fresh
external_function_behavior "_getenv" = pure_and_fresh
external_function_behavior "getenv" = pure_and_fresh
external_function_behavior "_open" = pure_and_fresh
external_function_behavior "_fts_read$INODE64" = pure_and_fresh
external_function_behavior "_fts_open$INODE64" = pure_and_fresh
external_function_behavior "_opendir$INODE64" = pure_and_fresh
external_function_behavior "fopen" = pure_and_fresh
external_function_behavior "_fopen" = pure_and_fresh
external_function_behavior "_fdopen" = pure_and_fresh
external_function_behavior "_wfdopen" = pure_and_fresh
external_function_behavior "_fgetln" = pure_and_fresh
external_function_behavior "fgetln" = pure_and_fresh
external_function_behavior "_setlocale" = pure_and_fresh
external_function_behavior "_wsetlocale" = pure_and_fresh
external_function_behavior "__ctype_b_loc" = pure_and_fresh
external_function_behavior "dcgettext" = pure_and_fresh
external_function_behavior "nl_langinfo" = pure_and_fresh
external_function_behavior "setlocale" = pure_and_fresh
external_function_behavior "__errno_location" = pure_and_fresh
external_function_behavior "_popen" = pure_and_fresh
external_function_behavior "__ctype_tolower_loc" = pure_and_fresh
external_function_behavior "__ctype_toupper_loc" = pure_and_fresh
external_function_behavior "readdir" = pure_and_fresh
external_function_behavior "getmntent" = pure_and_fresh
external_function_behavior "setmntent" = pure_and_fresh
external_function_behavior "dlsym" = pure_and_fresh
external_function_behavior "dlopen" = pure_and_fresh
external_function_behavior "dlerror" = pure_and_fresh
-- | A list of some functions that are assumed not to change the state in any significant way, and that return an unknown bottom value through RAX
external_function_behavior "feof" = pure_and_unknown
external_function_behavior "_feof" = pure_and_unknown
external_function_behavior "_getc" = pure_and_unknown
external_function_behavior "getc" = pure_and_unknown
external_function_behavior "fgetc" = pure_and_unknown
external_function_behavior "_fgetc" = pure_and_unknown
external_function_behavior "_fgetwc" = pure_and_unknown
external_function_behavior "fgetwc" = pure_and_unknown
external_function_behavior "_fnmatch" = pure_and_unknown
external_function_behavior "_fputc" = pure_and_unknown
external_function_behavior "fputc" = pure_and_unknown
external_function_behavior "_close" = pure_and_unknown
external_function_behavior "close" = pure_and_unknown
external_function_behavior "fwrite" = pure_and_unknown
external_function_behavior "_fwrite" = pure_and_unknown
external_function_behavior "_fflush" = pure_and_unknown
external_function_behavior "___maskrune" = pure_and_unknown
external_function_behavior "_getbsize" = pure_and_unknown
external_function_behavior "_printf" = pure_and_unknown
external_function_behavior "printf" = pure_and_unknown
external_function_behavior "vprintf" = pure_and_unknown
external_function_behavior "_fprintf" = pure_and_unknown
external_function_behavior "fprintf" = pure_and_unknown
external_function_behavior "vfprintf" = pure_and_unknown
external_function_behavior "_fprintf_l" = pure_and_unknown
external_function_behavior "fwprintf" = pure_and_unknown
external_function_behavior "_fwprintf_l" = pure_and_unknown
external_function_behavior "__fprintf_chk" = pure_and_unknown
external_function_behavior "__printf_chk" = pure_and_unknown
external_function_behavior "_putchar" = pure_and_unknown
external_function_behavior "_puts" = pure_and_unknown
external_function_behavior "fputs" = pure_and_unknown
external_function_behavior "_fputs" = pure_and_unknown
external_function_behavior "_btowc" = pure_and_unknown
external_function_behavior "btowc" = pure_and_unknown
external_function_behavior "mbtowc" = pure_and_unknown
external_function_behavior "_mbtowc" = pure_and_unknown
external_function_behavior "_mbrtowc" = pure_and_unknown
external_function_behavior "mbrtowc" = pure_and_unknown
external_function_behavior "_atof" = pure_and_unknown
external_function_behavior "atof" = pure_and_unknown
external_function_behavior "_strcmp" = pure_and_unknown
external_function_behavior "_strncmp" = pure_and_unknown
external_function_behavior "strcmp" = pure_and_unknown
external_function_behavior "strncmp" = pure_and_unknown
external_function_behavior "strlen" = pure_and_unknown
external_function_behavior "_ilogb" = pure_and_unknown
external_function_behavior "_atoi" = pure_and_unknown
external_function_behavior "_getopt" = pure_and_unknown
external_function_behavior "getopt_long" = pure_and_unknown
external_function_behavior "_free" = pure_and_unknown
external_function_behavior "_warn" = pure_and_unknown
external_function_behavior "_warnx" = pure_and_unknown
external_function_behavior "__errno_location" = pure_and_unknown
external_function_behavior "__libc_start_main" = pure_and_unknown
external_function_behavior "__cxa_finalize" = pure_and_unknown
external_function_behavior "perror" = pure_and_unknown
external_function_behavior "fclose" = pure_and_unknown
external_function_behavior "free" = pure_and_unknown
external_function_behavior "unlink" = pure_and_unknown
external_function_behavior "unlinkat" = pure_and_unknown
external_function_behavior "strspn" = pure_and_unknown
external_function_behavior "utimensat" = pure_and_unknown
external_function_behavior "fdatasync" = pure_and_unknown
external_function_behavior "fsync" = pure_and_unknown
external_function_behavior "isatty" = pure_and_unknown
external_function_behavior "strcspn" = pure_and_unknown
external_function_behavior "memcmp" = pure_and_unknown
external_function_behavior "_memcmp" = pure_and_unknown
external_function_behavior "isprint" = pure_and_unknown
external_function_behavior "iswprint" = pure_and_unknown
external_function_behavior "_isprint_l" = pure_and_unknown
external_function_behavior "_iswprint_l" = pure_and_unknown
external_function_behavior "__cxa_atexit" = pure_and_unknown
external_function_behavior "towlower" = pure_and_unknown
external_function_behavior "towupper" = pure_and_unknown
external_function_behavior "iswalnum" = pure_and_unknown
external_function_behavior "fseeko" = pure_and_unknown
external_function_behavior "fflush" = pure_and_unknown
external_function_behavior "_fclose" = pure_and_unknown
external_function_behavior "_fgets" = pure_and_unknown
external_function_behavior "_ferror" = pure_and_unknown
external_function_behavior "_strtol" = pure_and_unknown
external_function_behavior "_strtoul" = pure_and_unknown
external_function_behavior "_munmap" = pure_and_unknown
external_function_behavior "fread_unlocked" = pure_and_unknown



-- | A list of some functions that return bottom and write to pointers passed by parameters
--external_function_behavior "_sysctlbyname" = ExternalFunctionBehavior [param 2, param 4] UnknownReturnValue
--external_function_behavior "_fstat$INODE64" = ExternalFunctionBehavior [param 1] UnknownReturnValue
--external_function_behavior "_fstatfs$INODE64" = ExternalFunctionBehavior [param 1] UnknownReturnValue
--external_function_behavior "_statfs$INODE64" = ExternalFunctionBehavior [param 1] UnknownReturnValue
external_function_behavior "snprintf"             = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "_snprintf"            = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "_snprintf_l"          = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "_snwprintf"           = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "_snwprintf_l"         = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "__snprintf_chk"       = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "_vsnprintf"           = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "sprintf"              = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "_sprintf"             = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "___bzero"             = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "sigprocmask"          = ExternalFunctionBehavior [param 2] UnknownReturnValue
external_function_behavior "__strcat_chk"         = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "strcat"               = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "strlcpy"              = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "___strlcpy_chk"       = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "sigemptyset"          = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "sigaction"            = ExternalFunctionBehavior [param 2] UnknownReturnValue
external_function_behavior "localtime"            = ExternalFunctionBehavior [param 0] FreshPointer
external_function_behavior "memset"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "_memset"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "__memset_chk"         = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "___memset_chk"        = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "_index"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_rindex"              = ExternalFunctionBehavior [] $ Input $ param 0

-- A list of functions that return a pointer given to them by a parameter
external_function_behavior "_realloc"             = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "reallocarray"         = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_malloc_zone_realloc" = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_recallocarray"       = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "realloc"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "mremap"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_strcpy"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "__strcpy_chk"         = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "_strncpy"             = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "strcpy"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "strncpy"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "stpcpy"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "memcpy"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "_memcpy"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "__memcpy_chk"         = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "___memcpy_chk"        = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "__memmove_chk"        = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "memmove"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "_memmove"             = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "strcat"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "_strcat"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "strchr"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_strchr"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "strrchr"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_strrchr"             = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_memchr"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "memchr"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "strstr"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_strstr"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_strpbrk"             = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "strpbrk"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_strtok"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "strtok"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_strlen"              = ExternalFunctionBehavior [] $ Input $ param 0


external_function_behavior f
 | is_exiting_function_call f = pure_and_unknown
 | otherwise                  = ExternalFunctionBehavior [] UnknownReturnValue -- trace ("Unknown external function: " ++ f) $ 



{-- TODO
 - functions calling function pointers
 - __cxa_finalize
 - __libc_start_main
 - pthread_*
 --}






-- | Backward transposition
-- Let p be the current predicate and let the equality sp == v be from the predicate after execution of an internal function.
-- For example, p contains:
--   RSP == RSP0 - 64
--   RSI == 10
--
-- And after execution of the function, we have:
--   *[RSP0+16,8] == RSI0
--
-- Transposing this equality produces:
--   *[RSP0-40,8] == 10

transpose_bw_svalue :: BinaryClass bin => Static bin v -> Sstate SValue SPointer -> SValue -> SValue
transpose_bw_svalue fctxt p t@Top           = t
transpose_bw_svalue fctxt p (SConcrete es)  = cjoin_all fctxt "transpose_bw" $ NES.map (transpose_bw_e fctxt p) es
transpose_bw_svalue fctxt p (SAddends as)   = cjoin_all fctxt "transpose_bw" $ NES.map (transpose_bw_addends fctxt p) as

transpose_bw_addends fctxt p a =
  let a' = transpose_bw_e fctxt p a
      v' = cwiden fctxt "transpose_bw" a' in
    v'



transpose_bw_spointer :: BinaryClass bin => Static bin v -> Sstate SValue SPointer -> SPointer -> S.Set SPointer
transpose_bw_spointer fctxt p (Ptr_Concrete a) =
  let a' = transpose_bw_e fctxt p a in
    cmk_mem_addresses fctxt "transpose_bw" False a'
transpose_bw_spointer fctxt p (Ptr_Base b) =
  let b' = transpose_bw_e fctxt p b in
    cmk_mem_addresses fctxt "transpose_bw" False $ cwiden fctxt "transpose_bw" b'
transpose_bw_spointer fctxt p Ptr_Top = S.singleton Ptr_Top



transpose_bw_reg :: BinaryClass bin => Static bin v -> Sstate SValue SPointer -> (Register, SValue) -> Maybe (Register, SValue)
transpose_bw_reg fctxt p (r,v) =
  let v' = transpose_bw_svalue fctxt p v in
    Just $ (r,v')

transpose_bw_mem :: BinaryClass bin => Static bin v -> Sstate SValue SPointer -> ((SPointer,Maybe ByteSize), SValue) -> [((SPointer,Maybe ByteSize), SValue)]
transpose_bw_mem fctxt p ((a,si),v) =
  let as' = transpose_bw_spointer fctxt p a
      v'  = if si == Just (ByteSize 8) then transpose_bw_svalue fctxt p v else mk_top "transpose_bw_mem" in
    S.toList $ S.map (\a' -> ((a',si),v')) as'

transpose_bw_gmem fctxt p (v,si) = (transpose_bw_svalue fctxt p v, si)






transpose_bw_e :: BinaryClass bin => Static bin v -> Sstate SValue SPointer -> SimpleExpr -> SValue
transpose_bw_e fctxt p (Bottom (FromCall f))            = mk_concreteS fctxt $ Bottom (FromCall f)
transpose_bw_e fctxt p (SE_Malloc id hash)              = mk_concreteS fctxt $ SE_Malloc id hash
transpose_bw_e fctxt p (SE_Immediate i)                 = cimmediate fctxt i
transpose_bw_e fctxt p (SE_StatePart sp id)             = error "Should not happen"
transpose_bw_e fctxt p (SE_Var sp)                      = transpose_bw_sp fctxt p sp
transpose_bw_e fctxt p (SE_Bit i e)                     = csemantics fctxt "transpose_bw" $ SO_Bit i $ transpose_bw_e fctxt p e
transpose_bw_e fctxt p (SE_SExtend l h e)               = csemantics fctxt "transpose_bw" $ SO_SExtend l h $ transpose_bw_e fctxt p e
transpose_bw_e fctxt p (SE_Op Plus si [a,b])            = csemantics fctxt "transpose_bw" $ SO_Op ADD (si `div` 8) Nothing [transpose_bw_e fctxt p a,transpose_bw_e fctxt p b]
transpose_bw_e fctxt p (SE_Op Minus si [a,b])           = csemantics fctxt "transpose_bw" $ SO_Op SUB (si `div` 8) Nothing [transpose_bw_e fctxt p a,transpose_bw_e fctxt p b]
transpose_bw_e fctxt p (SE_Op op si es)                 = svalue_apply fctxt "transpose_bw" (mk_expr fctxt . SE_Op op si) $ map (transpose_bw_e fctxt p) es
transpose_bw_e fctxt p (SE_Overwrite i a b)             = csemantics fctxt "transpose_bw" $ SO_Overwrite i (transpose_bw_e fctxt p a) (transpose_bw_e fctxt p b)

transpose_bw_sp :: BinaryClass bin => Static bin v -> Sstate SValue SPointer -> StatePart -> SValue
transpose_bw_sp fctxt p (SP_Reg r)    = evalSstate (sread_reg fctxt r) p
transpose_bw_sp fctxt p (SP_Mem a si) = 
  let a' = transpose_bw_e fctxt p a in
    evalSstate (sread_mem fctxt "transpose_bw_sp" a' $ Just $ ByteSize si) p





data FunctionType = AnalyzedInternalFunction (Sstate SValue SPointer) | ExternalFunction | AnalyzedInternalFunctionTerminates | AnalyzedInternalFunctionUnknown


-- TODO add VerificationError as Functiontype. If so, overwrite at least RAX/XMM0 with top
get_function_type :: BinaryClass bin => Static bin v -> Instruction -> String -> FunctionType
get_function_type l@(bin,config,l0,_) i f_callee =
  let trgts = get_known_jump_targets (bin,config,l0) i in
    ftype $ map postcondition_of_jump_target trgts
 where
  ftype posts
    | all ((==) (Just Terminates)) posts                                 = AnalyzedInternalFunctionTerminates
    | all is_returning posts                                             = AnalyzedInternalFunction $ supremum l $ map fromReturning posts
    | "0x" `isPrefixOf` f_callee || "indirection@" `isPrefixOf` f_callee = AnalyzedInternalFunctionUnknown
    | otherwise                                                          = ExternalFunction

  fromReturning (Just (ReturnsWith q)) = q
  is_returning  (Just (ReturnsWith q)) = True
  is_returning  _                      = False

  postcondition_of_jump_target (ImmediateAddress a) = 
    case l0_lookup_entry a l0 of 
      Just (_,Just r) -> Just $ result_post r
      _ -> Nothing
  postcondition_of_jump_target _                    = Nothing






-- | Executes semantics for external functions.
call :: BinaryClass bin => Static bin v -> Instruction -> State (Sstate SValue SPointer,VCS SValue) ()
call fctxt@(bin,_,l0,entry) i = do
  let f_callee = function_name_of_instruction bin i
  let ty = get_function_type fctxt i f_callee
  case ty of
    AnalyzedInternalFunctionUnknown    -> unknown_internal_function fctxt i
    AnalyzedInternalFunctionTerminates -> return ()
    AnalyzedInternalFunction q         -> internal_function q
    ExternalFunction                   -> external_function f_callee
 where
  external_function f_callee
    --   | f_callee == "error" = call_error fctxt
    | otherwise =
      case external_function_behavior f_callee of
        ExternalFunctionBehavior params output -> {--mapM_ write_param params >> --} write_output f_callee output -- writing to params really roughly overapproximates

  write_output :: String -> ExternalFunctionOutput -> State (Sstate SValue SPointer,VCS SValue) ()
  write_output f_callee FreshPointer       = swrite_reg fctxt "wo1" (Reg64 RAX) $ (mk_concreteS fctxt $ SE_Malloc (Just (inAddress i)) (Just f_callee))
  write_output f_callee UnknownReturnValue = swrite_reg fctxt "wo2" (Reg64 RAX) $ (mk_concreteS fctxt $ Bottom (FromCall f_callee))
  write_output f_callee (Input r)          = do
    --input <- sread_reg fctxt r
    --let ptrs = cmk_mem_addresses fctxt "write_output" input
    --if S.null ptrs then
      swrite_reg fctxt "wo3" (Reg64 RAX) $ mk_concreteS fctxt $ SE_Malloc (Just (inAddress i)) (Just f_callee)
    --else
    --  swrite_reg fctxt "wo4" (Reg64 RAX) input

  incr_rsp = sexec_instr fctxt False (Instruction 0 [] ADD [Op_Reg (Reg64 RSP) [Read,Write], Op_Imm $ Immediate (BitSize 64) 8] [] 1)

  decr_rsp = sexec_instr fctxt False (Instruction 0 [] SUB [Op_Reg (Reg64 RSP) [Read,Write], Op_Imm $ Immediate (BitSize 64) 8] [] 1)


  internal_function :: Sstate SValue SPointer -> State (Sstate SValue SPointer,VCS SValue) ()
  internal_function q = do
    -- push return address if is call
    decr_rsp

    (p,vcs) <- get
    -- obtain the postcondition of the function, and do backwards transposition
    let q_eqs_transposed_regs  = catMaybes $ map (transpose_bw_reg fctxt p) $ filter (\(r,_) -> r `notElem` [Reg64 RIP,Reg64 RSP]) $ sstate_to_reg_eqs q
    let q_eqs_transposed_mem   = concatMap (transpose_bw_mem fctxt p) $ filter do_transfer $ sstate_to_mem_eqs q
    let q_eqs_transposed_gmem  = IM.toList $ IM.map (transpose_bw_gmem fctxt p) $ sstate_to_gmem_eqs q

    -- write transposed postcondition to current state
    mapM_ (\(r,v) -> swrite_reg fctxt ("call: " ++ show i) r v) $ q_eqs_transposed_regs
    mapM_ (\((a,si),v) -> swrite_mem_to_ptr fctxt True a si v) $ q_eqs_transposed_mem 
    (Sstate _ _ gmem _,_) <- get
    let gmem' = foldr (\(a,(v,si)) -> add_global_mem_access fctxt False a si v) gmem q_eqs_transposed_gmem
    modify $ \(Sstate regs mem _ flgs,vcs) -> (Sstate regs mem gmem' flgs,vcs)

    incr_rsp 

  -- in case of an external function, which is passed a parameter $r$ 
  -- do a write to region [r+bot,1] to muddle the state. The value written to that region is an abstraction of what is already there.
  write_param :: Register -> State (Sstate SValue SPointer,VCS SValue) ()
  write_param r = do
    a      <- sread_reg fctxt r
    let a'  = cwiden fctxt "write_param" a
    v'     <- gets ((evalSstate $ sread_mem fctxt "write_param" a unknownSize) . fst)
    let bot = cwiden fctxt "write_param_v" v'
    swrite_mem fctxt True a' unknownSize bot


  -- TODO
  do_transfer ((p@(Ptr_Concrete a),si),v) = is_global a -- not (is_top_stackframe a si) && not (cis_local fctxt p)
  do_transfer ((p@(Ptr_Base b),si),v) = is_global b -- not (cis_local fctxt p)
  
  is_global = any is_global_base . (get_pointer_base_set bin empty_finit)
  is_global_base (GlobalAddress _) = True
  is_global_base (BaseIsStatePart _) = True
  is_global_base _ = False
  

  sstate_to_reg_eqs (Sstate regs _ _ _) = M.toList regs
  sstate_to_mem_eqs (Sstate _ mem _ _)  = M.toList mem
  sstate_to_gmem_eqs (Sstate _ _ (GlobalMem m _) _)  = m

  unknown_internal_function fctxt i = return ()-- TODO try as external



is_top_stackframe (SE_Var (SP_Reg (Reg64 RSP))) (Just (ByteSize 8)) = True
is_top_stackframe _ _ = False




jump :: BinaryClass bin => Static bin v -> Instruction -> State (Sstate SValue SPointer,VCS SValue) ()
jump l@(bin,config,l0,_) i = do
  if jump_is_actually_a_call (bin,config,l0) i then do
    call l i
    sreturn l i
  else
    return ()



ctry_jump_targets :: BinaryClass bin => Static bin v -> SValue -> Maybe (S.Set ResolvedJumpTarget)
ctry_jump_targets l@(bin,_,_,_) v@(SConcrete es) =
  let tries = mapMaybe try $ S.toList $ NES.toSet es in
    case tries of
      [] -> Nothing -- trace ("Cannot resolve indirection: " ++ show v) Nothing      
      _  -> Just $ S.fromList tries
 where
  try (SE_Immediate imm)                     = try_immediate_address imm 
  try (SE_Var (SP_Mem (SE_Immediate imm) _)) = try_reloc imm `orTry` try_symbol imm 
  try _ = Nothing


  try_immediate_address a =
    if address_has_instruction bin a then
      Just $ ImmediateAddress a
    else case IM.lookup (fromIntegral a) $ binary_get_symbol_table bin of
      Just (AddressOfLabel f True)  -> Just $ External f
      Just s                        -> error $ show (a, s) -- Just $ External f
      _                             -> Nothing

  try_symbol a =
    case IM.lookup (fromIntegral a) $ binary_get_symbol_table bin of
      Just (PointerToLabel f True)  -> Just $ External f
      Just (AddressOfLabel f True)  -> Just $ ExternalDeref f -- an external variable contains a pointer to a function
      Just (AddressOfObject f True) -> Just $ ExternalDeref f      -- an external variable contains a pointer to a function
      Just s                        -> error $ show (a, s) -- Just $ External f
      _                             -> Nothing


  try_reloc a =
    case find (is_reloc_for a) $ S.toList $ binary_get_relocations bin of
      Just (Relocation _ a') -> try_immediate_address a'
      Nothing -> Nothing

  is_reloc_for a (Relocation a' _) = a == a'

ctry_jump_targets fctxt _ = Nothing 



