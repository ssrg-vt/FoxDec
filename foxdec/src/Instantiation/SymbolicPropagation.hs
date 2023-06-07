{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, FlexibleInstances, StrictData#-}

{-|
Module      : SymbolicPropagation
Description : Provides an instantiation of all functions necessary to do symbolic propagation
-}
module Instantiation.SymbolicPropagation where


import Base
import Config

import Data.SValue
import Data.JumpTarget
import Data.SymbolicExpression

import Analysis.ControlFlow
import Analysis.Pointers
import Analysis.FunctionNames
import Analysis.Context

import Generic.SymbolicConstituents
import Generic.SymbolicPropagation
import Generic.Binary


import X86.Opcode (Opcode(..), isCondJump, isJump)
import X86.Register
import X86.Conventions
import X86.Instruction (addressof)
import Generic.HasSize (sizeof)
import qualified X86.Instruction as X86
import Generic.Instruction (GenericInstruction(Instruction))
import Generic.Address (AddressWord64(..))
import Generic.Operand


import Control.Monad.State.Strict hiding (join)
import Control.Applicative ((<|>))
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

ctry_deterministic (SConcrete es)
  | NES.size es == 1 = try_det $ NES.findMin es
  | otherwise        = Nothing
 where
  try_det e
    | contains_bot e = Nothing
    | otherwise      = Just e
ctry_deterministic _ = Nothing




-- When doing abstraction, immediate values are grouped according to whether they are pointers into the same section.
-- Immediates within the same section are merged into one immediate (the lowest).
-- For example, if the values {0x2050, 0x2052, 0x2054} are abstracted and there is a data section starting at 0x2000 with size 0x100,
-- the value 0x2050 + Top is produced
group_immediates :: FContext -> NES.NESet (S.Set SAddend) -> S.Set (S.Set SAddend)
group_immediates fctxt addends =
  let (imms,remainder) = S.partition is_imm $ NES.toSet addends in
    if S.null imms then
      remainder
    else
      S.union (S.map merge_imms $ group_imms_by_section imms) remainder
 where
  is_imm bs = S.size bs == 1 && all isImmediateBase bs

  group_imms_by_section :: S.Set (S.Set SAddend) -> S.Set (S.Set (S.Set SAddend))
  group_imms_by_section = quotientBy same_section

  same_section imms0 imms1 = get_section_for (get_imm $ S.findMin imms0) == get_section_for (get_imm $ S.findMin imms1)

  get_section_for a =
    let ctxt = f_ctxt fctxt in
      find_section_for_address ctxt a <|> find_section_ending_at ctxt a

  merge_imms = S.singleton . SAddend_Immediate . minimum . S.map (get_imm . S.findMin)
  get_imm (SAddend_Immediate imm) = imm


-- When doing abstraction, we cap the number of nested dereferences, group them if they have the some inner deref,
-- and abstract over them using Top
group_nested_dereferences :: S.Set (S.Set SAddend) -> S.Set (S.Set SAddend)
group_nested_dereferences ptrs =
  let (derefs,remainder) = S.partition is_deref ptrs
      grouped_derefs     = quotientBy same_group derefs
      groups             = S.map do_abstraction grouped_derefs in
    S.union groups remainder
 where
  is_deref = all is_deref_base

  is_deref_base (SAddend_StatePart (SP_Mem _ _)) = True
  is_deref_base _                                = False

  same_group ptr0 ptr1 = get_inner_derefs ptr0 == get_inner_derefs ptr1

  do_abstraction ptrs =
    let ptr    = S.findMin ptrs
        derefs = get_inner_derefs ptr in
      S.map SAddend_StatePart derefs

  get_inner_derefs = S.unions . S.map get_inner_derefs_base
  get_inner_derefs_base b@(SAddend_StatePart sp) = inner_derefs $ SE_Var sp

  inner_derefs (SE_Var (SP_Mem a si))   =
    let ds = inner_derefs a in
      if S.null ds then S.singleton $ SP_Mem a si else ds 
  inner_derefs e@(SE_Op op si es)       = S.unions $ map inner_derefs es
  inner_derefs e@(SE_Bit l e0)          = inner_derefs e0
  inner_derefs e@(SE_SExtend l h e0)    = inner_derefs e0
  inner_derefs e@(SE_Overwrite h e0 e1) = S.unions $ map inner_derefs [e0,e1]
  inner_derefs e                        = S.empty



-- Construct an SValue from SExpressions
mk_concrete :: FContext -> String -> NES.NESet SimpleExpr -> SValue
---mk_concrete fctxt msg | trace ("mk_concrete: "++ show msg) False = error "trace"
mk_concrete fctxt msg = cap . NES.map simp -- TODO simp should be unnecessary?
 where
  cap es
    | NES.size es > num_cases = 
      let es' = widen_exprs fctxt ("mk_concrete"++msg) es in
        mk_addends fctxt ("mk_concrete"++msg) es'
    | otherwise = SConcrete es
  num_cases = ctxt_max_num_of_cases (f_ctxt fctxt)

mk_concreteS fctxt = mk_concrete fctxt "mk_concreteS" . NES.singleton

mk_addends :: FContext -> String -> NES.NESet (S.Set SAddend) -> SValue
mk_addends fctxt msg = mk . group_nested_dereferences . group_immediates fctxt
 where
  mk addends
    | any S.null addends = mk_top $ "mk_addends: " ++ msg ++ "\n" ++ show addends
    | S.size addends > 5 = mk_top $ "mk_addends: " ++ msg ++ "\n" ++ show addends -- TODO 5: num_of_addends
    | otherwise          = SAddends $ NES.unsafeFromSet $ S.map NES.unsafeFromSet addends

cimmediate :: Integral i => FContext -> i -> SValue
cimmediate fctxt = mk_concrete fctxt "cimmediate" . NES.singleton . SE_Immediate . fromIntegral


-- WIDENING
set_unknown_offset fctxt msg (Base_StackPointer f   offset) = Base_StackPointer f UnknownOffset 
set_unknown_offset fctxt msg (Base_Immediate a      offset) = Base_Immediate a UnknownOffset
set_unknown_offset fctxt msg (Base_Malloc id h      offset) = Base_Malloc id h UnknownOffset
set_unknown_offset fctxt msg (Base_FunctionPtr a f  offset) = Base_FunctionPtr a f UnknownOffset
set_unknown_offset fctxt msg (Base_TLS              offset) = Base_TLS UnknownOffset
set_unknown_offset fctxt msg (Base_StatePart sp     offset) = Base_StatePart sp UnknownOffset
set_unknown_offset fctxt msg (Base_FunctionReturn f offset) = Base_FunctionReturn f UnknownOffset
set_unknown_offset fctxt msg b                              = error $ "set_unknown_offset: " ++ msg ++ "\n" ++ show b



abstract_addends fctxt (SE_Op Plus  si [e0,e1])                = S.unions [abstract_addends fctxt e0,abstract_addends fctxt e1]
abstract_addends fctxt (SE_Op Minus si [e0,e1])                = abstract_addends fctxt e0
abstract_addends fctxt (SE_Var (SP_StackPointer f))            = S.singleton $ SAddend_StackPointer f
abstract_addends fctxt (SE_Immediate imm)
  | immediate_maybe_a_pointer fctxt imm                        = S.singleton $ SAddend_Immediate imm
  | otherwise                                                  = S.empty
abstract_addends fctxt (SE_Malloc id h)                        = S.singleton $ SAddend_Malloc id h
abstract_addends fctxt (SE_Var sp@(SP_Mem (SE_Immediate a) 8)) = 
  case try_relocated_pointer fctxt a of
    Nothing -> S.singleton $ SAddend_StatePart sp
    Just f  -> S.singleton $ SAddend_FunctionPtr a f 
abstract_addends fctxt (SE_Var (SP_Mem (SE_Var (SP_StackPointer f)) 8)) = S.singleton $ SAddend_ReturnAddr f
abstract_addends fctxt (SE_Var (SP_Reg FS))                    = S.singleton $ SAddend_TLS
abstract_addends fctxt (SE_Var sp)                             = S.singleton $ SAddend_StatePart sp
abstract_addends fctxt (Bottom (FromCall f))
  | f == ""                                                    = S.empty
  | otherwise                                                  = S.singleton $ SAddend_FunctionReturn f
abstract_addends fctxt (SE_Bit h e)                            = abstract_addends fctxt e
abstract_addends fctxt _                                       = S.empty



widen_exprs fctxt msg es = NES.map (abstract_addends fctxt) es


cwiden :: FContext -> String -> SValue -> SValue
---cwiden fctxt msg v | trace ("cwiden: " ++ msg ++ "\n" ++ show v) False = error "trace"
cwiden fctxt msg (SConcrete es) = mk_addends fctxt ("cwiden " ++ msg) $ widen_exprs fctxt msg es
cwiden fctxt msg v = v



cwiden_all :: FContext -> String -> [SValue] -> SValue
cwiden_all fctxt msg []     = mk_top ("cwiden_all" ++ msg)
cwiden_all fctxt msg (v:vs) = foldr1 (cjoin fctxt msg) $ map (cwiden fctxt msg) (v:vs)


-- JOINING
---cjoin fctxt msg v0 v1 | trace ("cjoin: " ++ msg ++ "\n" ++ show (v0,v1)) False = error "trace"
cjoin fctxt msg v0 v1 = join v0 v1
 where
  join v0@(SConcrete es0) v1@(SConcrete es1) = mk_concrete fctxt ("cjoin " ++ msg) $ NES.union es0 es1
  join v0@(SAddends es0)  v1@(SAddends es1)  = mk_addends  fctxt ("cjoin " ++ msg) $ NES.map NES.toSet $ NES.union es0 es1
  join v0@(SConcrete es0) v1@(SAddends es1)  = join (cwiden fctxt ("cjoin " ++ msg) v0) v1
  join v0@(SAddends es0)  v1@(SConcrete es1) = join v0 (cwiden fctxt ("cjoin " ++ msg) v1)

  join t@Top _ = t
  join _ t@Top = t

cjoin_all :: Foldable t => FContext -> String -> t SValue -> SValue
cjoin_all fctxt msg es
  | null es   = error $ "Cannot join [], msg = " ++ msg
  | otherwise = foldr1 (cjoin fctxt msg) es



all_equal :: (Eq a) => [a] -> Bool
all_equal [] = True
all_equal xs = and $ map (== head xs) (tail xs)


-- TODO joining global immediates can be prevented at the cost of scalability
cjoin_pointers fctxt []    = []
cjoin_pointers fctxt [ptr] = [ptr]
cjoin_pointers fctxt ptrs
  | all_equal ptrs = [head ptrs]
  | otherwise      = 
    let ptrs'            = nub $ map (set_unknown_offset fctxt "cjoin_pointers") ptrs
        (imms,remainder) = partition is_imm ptrs' in
      merge_imms imms ++ remainder
 where
  is_imm (Base_Immediate _ _)  = True
  is_imm _                     = False

  get_imm (Base_Immediate i _) = i

  merge_imms []   = []
  merge_imms imms = [(\i -> Base_Immediate i UnknownOffset) $ minimum $ map get_imm imms]






-- MAKING POINTERS FROM EXPRESSIONS
addends_of (SE_Op Plus _ es)      = S.unions $ S.map addends_of $ S.fromList es
addends_of (SE_Op Minus _ (e:es)) = addends_of e
addends_of e                      = S.singleton e


is_local_expr (SE_Var (SP_StackPointer f))        = Just f 
is_local_expr (SE_Op Plus _ [e0,e1])              = is_local_expr e0 `orTry` is_local_expr e1
is_local_expr (SE_Op Minus _ [e0,_])              = is_local_expr e0
is_local_expr (SE_Op And _ [e,SE_Immediate mask]) = is_local_expr e
is_local_expr _                                   = Nothing
-- Returns true if the immediate value is likely a pointer
immediate_maybe_a_pointer fctxt a = find_section_for_address ctxt a /= Nothing || find_section_ending_at ctxt a /= Nothing || has_symbol a
 where
  ctxt = f_ctxt fctxt
  has_symbol a = (IM.lookup (fromIntegral a) $ ctxt_symbol_table ctxt) /= Nothing



-- Try to promote an expression to a pointer base
try_promote_expr :: FContext -> Bool -> SimpleExpr -> Maybe SPointer
try_promote_expr fctxt strict (SE_Op Plus si [SE_Immediate imm,e0]) = try_promote_expr fctxt strict (SE_Op Plus si [e0,SE_Immediate imm])
try_promote_expr fctxt strict (SE_Op Plus si [e0,SE_Immediate imm]) = 
  case try_promote_expr fctxt strict e0 of
    Just b@(Base_Immediate i (PtrOffset i0)) -> (try_promote_expr fctxt strict $ SE_Immediate $ i + i0 + imm) `orTry` (Just $ set_unknown_offset fctxt "try_promote_expr" b)
    Just b@(Base_Immediate i _)              -> Just b
    Nothing -> set_unknown_offset fctxt "try_promote_expr" <$> (try_promote_expr fctxt strict $ SE_Immediate imm)
    x -> mod_offset ((+) imm) <$> x
try_promote_expr fctxt strict (SE_Op Minus _ [e0,SE_Immediate imm]) =
  case try_promote_expr fctxt strict e0 of
    Just b@(Base_Immediate i (PtrOffset i0)) -> (try_promote_expr fctxt strict $ SE_Immediate $ i + i0 - imm)  `orTry` (Just $ set_unknown_offset fctxt "try_promote_expr" b)
    Just b@(Base_Immediate i _)              -> Just b
    x -> mod_offset (\v -> v - imm) <$> x
try_promote_expr fctxt strict e =
  let promotions = map (\a -> (a,try_promote_addend a)) $ S.toList $ addends_of e
      bases      = filter (\(_,p) -> p /= Nothing) promotions in
    case bases of
      []            -> Nothing
      [(e',Just b)] -> Just $ mk_offset b (e'==e)
      bases         -> case filter (real_base . snd) bases of
                         [(_,Just b)] -> Just $ mk_offset b False
                         _            -> Nothing -- error $ "TODO: get initial values from addends" ++ show e
 where
  mk_offset b True  = b
  mk_offset b False = set_unknown_offset fctxt "mk_offset" b

  try_promote_addend (SE_Var (SP_StackPointer f))         = Just $ Base_StackPointer f $ PtrOffset 0
  try_promote_addend (SE_Immediate imm)
    | immediate_maybe_a_pointer fctxt imm                 = Just $ Base_Immediate imm (PtrOffset 0)
    | otherwise                                           = Nothing
  try_promote_addend (SE_Malloc id hash)                  = Just $ Base_Malloc id hash $ PtrOffset 0
  try_promote_addend (SE_Var sp@(SP_Mem (SE_Immediate a) 8)) =
    case (\f -> Base_FunctionPtr a f $ PtrOffset 0) <$> try_relocated_pointer fctxt a of
      Nothing -> if strict then Nothing else Just $ Base_StatePart sp $ PtrOffset 0
      Just p  -> Just p
  -- TODO return address
  try_promote_addend (SE_Var (SP_Reg FS))                 = Just $ Base_TLS $ PtrOffset 0
  try_promote_addend e@(SE_Var sp)                        
    | not strict                                          = Just $ Base_StatePart sp $ PtrOffset 0
    | possible_pointer_addend fctxt e                     = Just $ Base_StatePart sp $ PtrOffset 0
    | otherwise                                           = Nothing
  try_promote_addend (Bottom (FromCall f))
    | not strict && f /= ""                               = Just $ Base_FunctionReturn f $ PtrOffset 0
    | otherwise                                           = Nothing
  try_promote_addend e                                    = Nothing


  real_base (Just (Base_StatePart _ _)) = False
  real_base _                           = True

possible_pointer_addend fctxt (SE_Var sp) = sp `S.member` (S.map fst sps)
 where
  FInit sps _ = f_init fctxt
possible_pointer_addend _ _               = False


cmk_mem_addresses :: FContext -> String -> SValue -> S.Set SPointer
cmk_mem_addresses fctxt msg v@(SConcrete es) = 
  let as = S.map mk $ NES.toSet es in
    if Nothing `S.member` as then
      cmk_mem_addresses fctxt msg $ cwiden fctxt ("cmk_mem_addresses of " ++ show v ++ ": " ++ msg) v
    else
      S.map fromJust as
 where
  mk e = try_promote_expr fctxt True e `orTry` try_local e `orTry` try_promote_expr fctxt False e 
  try_local e = (\f -> Base_StackPointer f UnknownOffset) <$> is_local_expr e
cmk_mem_addresses fctxt msg v@(SAddends adds) = mapMaybeS mk $ NES.toSet adds
 where
  mk adds =
    let ptrs = NES.map saddend_to_spointer adds
        (strict,nonstrict) = S.partition is_strict $ NES.toSet ptrs in
      if S.null strict && S.size nonstrict == 1 then
        Just $ set_unknown_offset fctxt ("cmk_mem_addresses (1) of " ++ show v ++ ": " ++  msg) $ S.findMin nonstrict
      else if S.size strict == 1 then
        Just $ set_unknown_offset fctxt ("cmk_mem_addresses (2) of " ++ show v ++ ": " ++  msg) $ S.findMin strict
      else
        traceTop ("cmk_mem_addresses of " ++ show v ++ ": " ++  msg ++ "\n" ++ show v ++ "\n" ++ show (strict,nonstrict)) Nothing

  is_strict (Base_StatePart sp _)     = possible_pointer_addend fctxt (SE_Var sp)
  is_strict (Base_FunctionReturn f _) = False
  is_strict _                         = True
cmk_mem_addresses fctxt msg t@Top = S.empty


-- Concert SValues to SExpressions
-- TODO UnknownOffsets?
saddend_to_spointer (SAddend_StackPointer f)   = Base_StackPointer f $ PtrOffset 0
saddend_to_spointer (SAddend_Immediate i)      = Base_Immediate i $ PtrOffset 0
saddend_to_spointer (SAddend_Malloc id h)      = Base_Malloc id h $ PtrOffset 0
saddend_to_spointer (SAddend_FunctionPtr a f)  = Base_FunctionPtr a f $ PtrOffset 0
saddend_to_spointer (SAddend_ReturnAddr f)     = Base_ReturnAddr f
saddend_to_spointer (SAddend_TLS)              = Base_TLS $ PtrOffset 0
saddend_to_spointer (SAddend_StatePart sp)     = Base_StatePart sp $ PtrOffset 0
saddend_to_spointer (SAddend_FunctionReturn f) = Base_FunctionReturn f $ PtrOffset 0





-- SYMBOLIC COMPUTATIONS
mk_expr :: FContext -> SimpleExpr -> SimpleExpr
mk_expr fctxt e = trim_expr $ simp e
 where
  trim_expr e
    | expr_size e > get_max_expr_size = error $ show e
    | otherwise = e
  get_max_expr_size = ctxt_max_expr_size $ f_ctxt fctxt

svalue_plus fctxt si v0@(SConcrete es0) v1@(SConcrete es1)  = mk_concrete fctxt "plus1" $ NES.map (mk_expr fctxt . uncurry f) $ NES.cartesianProduct es0 es1
 where
  f e0 e1 = SE_Op Plus si [e0,e1]
svalue_plus fctxt si v0@(SAddends es0)  v1@(SAddends es1)   = cjoin fctxt "plus2" v0 v1
svalue_plus fctxt si v0@(SConcrete es0) v1@(SAddends es1)   = svalue_plus fctxt si (cwiden fctxt "plus" v0) v1
svalue_plus fctxt si v0@(SAddends es0)  v1@(SConcrete es1)  = svalue_plus fctxt si v1 v0
svalue_plus fctxt si v0@(SConcrete es0) Top                 = cwiden fctxt "plus5" v0
svalue_plus fctxt si v0@(SAddends es0)  Top                 = v0
svalue_plus fctxt si t@Top v1@(SConcrete es1)               = svalue_plus fctxt si v1 t
svalue_plus fctxt si t@Top v1@(SAddends es1)                = svalue_plus fctxt si v1 t
svalue_plus fctxt si t@Top Top                              = t


svalue_minus fctxt si v0@(SConcrete es0) v1@(SConcrete es1)  = mk_concrete fctxt "minus" $ NES.map (mk_expr fctxt . uncurry f) $ NES.cartesianProduct es0 es1
 where
  f e0 e1 = SE_Op Minus si [e0,e1]
svalue_minus fctxt si v0@(SAddends es0)  v1@(SAddends es1)   = v0
svalue_minus fctxt si v0@(SConcrete es0) v1@(SAddends es1)   = cwiden fctxt "minus" v0
svalue_minus fctxt si v0@(SAddends es0)  v1@(SConcrete es1)  = v0
svalue_minus fctxt si v0@(SConcrete es0) Top                 = cwiden fctxt "minus" v0
svalue_minus fctxt si v0@(SAddends es0)  Top                 = v0
svalue_minus fctxt si t@Top              _                   = t


svalue_and fctxt si v0@(SConcrete es0) v1@(SConcrete es1)  = mk_concrete fctxt "and" $ NES.map (mk_expr fctxt . uncurry f) $ NES.cartesianProduct es0 es1
 where
  f e0 e1 = SE_Op And si [e0,e1]
svalue_and fctxt si v0                 v1                  = mk_top ("svalue_and" ++ show (v0,v1))


svalue_unop fctxt msg f v0@(SConcrete es0) = mk_concrete fctxt msg $ NES.map (mk_expr fctxt . f) es0
svalue_unop fctxt msg f v0@(SAddends es0)  = mk_top ("unop" ++ msg)
svalue_unop fctxt msg f v0@Top             = v0

svalue_takebits fctxt h   = svalue_unop fctxt "takebits" (SE_Bit h)
svalue_sextend  fctxt l h = svalue_unop fctxt "sextend"  (SE_SExtend l h)

apply_expr_op :: FContext -> String -> ([SimpleExpr] -> SimpleExpr) -> [SValue] -> SValue
apply_expr_op fctxt msg f vs
  | all isImmediate vs = 
    let es' = f $ map (SE_Immediate . fromJust . ctry_immediate) vs in
      mk_concreteS fctxt es'
  | Top `elem` vs      = mk_top ""
  | otherwise          = mk_top ("Making top from: " ++ show vs ++ "(msg == " ++ msg ++ ")")
 where
  isImmediate v = ctry_immediate v /= Nothing

  

data CSemantics = ApplyPlus Int | ApplyMinus Int | ApplyNeg Int | ApplyDec Int | ApplyInc Int | ApplyAnd Int |
                  ApplyMov | ApplyCMov | ApplySExtend Int Int |
                  Apply ([SimpleExpr] -> SimpleExpr) | SetXX | SExtension_HI | NoSemantics



csemantics :: FContext -> String -> SymbolicOperation SValue -> SValue
---csemantics fctxt msg _ | trace ("csemantics: "++ msg) False = error "trace"
csemantics fctxt msg (SO_Plus  a b)         = svalue_plus fctxt 64 a b
csemantics fctxt msg (SO_Minus a b)         = svalue_minus fctxt 64 a b
csemantics fctxt msg (SO_Times a b)         = apply_expr_op fctxt "times" (mk_expr fctxt . SE_Op Times 64) [a,b]
csemantics fctxt msg (SO_Overwrite n a b)   = apply_expr_op fctxt "overwrite" (\[e0,e1] -> mk_expr fctxt $ SE_Overwrite n e0 e1) [a,b]
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
    Apply sop        -> apply_expr_op fctxt (msg ++ ", op = " ++ show op) (mk_expr fctxt . sop) es
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
mnemonic_to_semantics SHL si si'     = Apply $ shl
 where
  shl [a,SE_Immediate i] = SE_Op Times si [a,SE_Immediate $ 2^i]
  shl [a,b]              = SE_Op Shl si [a,b]

mnemonic_to_semantics IDIV_LO si si' = Apply $ SE_Op Div si
mnemonic_to_semantics SAR si si'     = Apply $ sar
 where
  sar [a,SE_Immediate i] = SE_Op Div si [a,SE_Immediate $ 2^i]
  sar [a,b]              = SE_Op Sar si [a,b]

mnemonic_to_semantics DIV_LO si si'  = Apply $ SE_Op Udiv si
mnemonic_to_semantics SHR si si'     = Apply $ shr
 where
  shr [a,SE_Immediate i] = SE_Op Udiv si [a,SE_Immediate $ 2^i]
  shr [a,b]              = SE_Op Shr si [a,b]


mnemonic_to_semantics BSR si si'     = Apply $ (\[a] -> SE_Op Bsr si [SE_Immediate 0,a])
mnemonic_to_semantics ROL si si'     = Apply $ SE_Op Rol si
mnemonic_to_semantics ROR si si'     = Apply $ SE_Op Ror si
mnemonic_to_semantics BSWAP si si'   = Apply $ SE_Op Bswap si

mnemonic_to_semantics PEXTRB si si'  = Apply $ (\[a,b,c] -> SE_Op Pextr si [a,b,c])
mnemonic_to_semantics PEXTRD si si'  = Apply $ (\[a,b,c] -> SE_Op Pextr si [a,b,c])
mnemonic_to_semantics PEXTRQ si si'  = Apply $ (\[a,b,c] -> SE_Op Pextr si [a,b,c])

mnemonic_to_semantics AND si si'     = ApplyAnd si
mnemonic_to_semantics OR  si si'     = Apply $ SE_Op Or si
mnemonic_to_semantics NOT si si'     = Apply $ (\[a] -> SE_Op Not si [a])

mnemonic_to_semantics XOR    si si'  = Apply $ SE_Op Xor si
mnemonic_to_semantics PXOR   si si'  = Apply $ SE_Op Xor si
mnemonic_to_semantics VPXOR  si si'  = Apply $ SE_Op Xor si
mnemonic_to_semantics XORPS  si si'  = Apply $ SE_Op Xor si
mnemonic_to_semantics XORPD  si si'  = Apply $ SE_Op Xor si

mnemonic_to_semantics MOV     si si'  = ApplyMov
mnemonic_to_semantics MOVSD   si si'  = ApplyMov
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


cflg_semantics fctxt _ i@(Instruction label prefix mnemonic dst srcs annot) flgs = flg mnemonic
 where
  flg CMP      = FS_CMP Nothing (srcs!!0) (srcs!!1)

  flg PUSH     = flgs
  flg POP      = flgs
  flg LEA      = flgs
  flg LEAVE    = flgs
  flg NOP      = flgs
  flg UD2      = flgs
  flg ENDBR64  = flgs
  flg HLT      = flgs
  flg WAIT     = flgs
  flg MFENCE   = flgs
  flg CLFLUSH  = flgs
  flg MOV      = flgs
  flg MOVSD    = flgs
  flg MOVSS    = flgs
  flg MOVAPS   = flgs
  flg MOVAPD   = flgs
  flg MOVUPS   = flgs
  flg MOVUPD   = flgs
  flg MOVABS   = flgs
  flg MOVDQU   = flgs
  flg MOVDQA   = flgs
  flg MOVLPD   = flgs
  flg MOVD     = flgs
  flg VMOVD    = flgs
  flg VMOVAPD  = flgs
  flg VMOVAPS  = flgs
  flg MOVZX    = flgs
  flg MOVSX    = flgs
  flg MOVSXD   = flgs
  flg CMOVO    = flgs
  flg CMOVNO   = flgs
  flg CMOVS    = flgs
  flg CMOVNS   = flgs
  flg CMOVE    = flgs
  flg CMOVZ    = flgs
  flg CMOVNE   = flgs
  flg CMOVNZ   = flgs
  flg CMOVB    = flgs
  flg CMOVNAE  = flgs
  flg CMOVC    = flgs
  flg CMOVNB   = flgs
  flg CMOVAE   = flgs
  flg CMOVNC   = flgs
  flg CMOVBE   = flgs
  flg CMOVNA   = flgs
  flg CMOVA    = flgs
  flg CMOVNBE  = flgs
  flg CMOVL    = flgs
  flg CMOVNGE  = flgs
  flg CMOVG    = flgs
  flg CMOVGE   = flgs
  flg CMOVNL   = flgs
  flg CMOVLE   = flgs
  flg CMOVNG   = flgs
  flg CMOVNLE  = flgs
  flg CMOVP    = flgs
  flg CMOVPE   = flgs
  flg CMOVNP   = flgs
  flg CMOVPO   = flgs
  flg SETO     = flgs
  flg SETNO    = flgs
  flg SETS     = flgs
  flg SETNS    = flgs
  flg SETE     = flgs
  flg SETZ     = flgs
  flg SETNE    = flgs
  flg SETNZ    = flgs
  flg SETB     = flgs
  flg SETNAE   = flgs
  flg SETC     = flgs
  flg SETNB    = flgs
  flg SETAE    = flgs
  flg SETNC    = flgs
  flg SETBE    = flgs
  flg SETNA    = flgs
  flg SETA     = flgs
  flg SETNBE   = flgs
  flg SETL     = flgs
  flg SETNGE   = flgs
  flg SETG     = flgs
  flg SETGE    = flgs
  flg SETNL    = flgs
  flg SETLE    = flgs
  flg SETNG    = flgs
  flg SETNLE   = flgs
  flg SETP     = flgs
  flg SETPE    = flgs
  flg SETNP    = flgs
  flg SETPO    = flgs
  flg CBW      = flgs
  flg CWDE     = flgs
  flg CDQE     = flgs
  flg CWD      = flgs
  flg CDQ      = flgs
  flg CQO      = flgs
  flg XCHG     = flgs
  flg mnemonic = if isJump mnemonic || isCondJump mnemonic then flgs else None -- TODO



-- MAKING INITIAL VALUES

cmk_init_reg_value :: FContext  -> Register -> SValue
cmk_init_reg_value fctxt = mk_concreteS fctxt . SE_Var . SP_Reg

offset_to_expr UnknownOffset e = SE_Op Plus 64 [e,Bottom $ FromCall ""]
offset_to_expr (PtrOffset 0) e = e
offset_to_expr (PtrOffset i) e = simp $ SE_Op Plus 64 [e,SE_Immediate i]

-- Concert SValues to SExpressions
spointer_to_expr (Base_StackPointer f  offset)  = offset_to_expr offset $ SE_Var $ SP_StackPointer f
spointer_to_expr (Base_Immediate i     offset)  = offset_to_expr offset $ SE_Immediate i
spointer_to_expr (Base_Malloc id h     offset)  = offset_to_expr offset $ SE_Malloc id h
spointer_to_expr (Base_FunctionPtr a f offset)  = offset_to_expr offset $ SE_Var $ SP_Mem (SE_Immediate a) 8
spointer_to_expr (Base_ReturnAddr f)            = SE_Var $ SP_Mem (SE_Var $ SP_StackPointer f) 8
spointer_to_expr (Base_TLS             offset)  = offset_to_expr offset $ SE_Var (SP_Reg FS)
spointer_to_expr (Base_StatePart sp    offset)  = offset_to_expr offset $ SE_Var sp
spointer_to_expr (Base_FunctionReturn f offset) = offset_to_expr offset $ Bottom $ FromCall f

cmk_init_mem_value :: FContext -> String -> SPointer -> RegionSize -> SValue
cmk_init_mem_value fctxt msg a si = mk_concreteS fctxt $ SE_Var $ SP_Mem mk_a $ mk_si si
 where
  mk_a            = spointer_to_expr a
  mk_si (Nat imm) = fromIntegral imm
  mk_si _         = 1




-- MEMORY RELATIONS
cseparate :: FContext -> String -> SPointer -> RegionSize -> SPointer -> RegionSize -> Bool
---cseparate fctxt msg v0 s0 v1 si1 | trace ("cseparate: "++ show (v0,v1)) False = error "trace"
cseparate fctxt msg a0 (Nat si0) a1 (Nat si1) = or
  [ separation_based_on_necessity a0 a1 
  , separation_of_spointers fctxt msg a0 si0 a1 si1]
 where
  separation_based_on_necessity a0 a1 = necessarily_separate_expressions (spointer_to_expr a0) si0 (spointer_to_expr a1) si1
cseparate fctxt msg a0 _ a1 _ = separation_of_spointers fctxt msg a0 (2^20) a1 (2^20)


mk_sstatepart fctxt (SP_Reg r)    = SSP_Reg r
mk_sstatepart fctxt (SP_Mem a si) =
  case try_promote_expr fctxt False a of
    Just ptr -> SSP_Mem ptr si
    Nothing  -> error $ "CANNOT PROMOTE: " ++ show (a,si)

separation_of_spointers fctxt msg v0 si0 v1 si1 = and
  [ v0 /= v1
  , separate_bases v0 v1 ]
 where
  -- Separation using pointer bases 
  separate_bases (Base_StackPointer f0 _) (Base_StackPointer f1 _)
    | f0 == f1  = False -- if (a0,si0) == (a1,si1) then False else trace ("SEP: " ++ show (a0,si0,a1,si1)) False
    | otherwise = True -- TODO ADD VC
  separate_bases (Base_StackPointer f0 _) (Base_Malloc _ _ _)           = True
  separate_bases (Base_StackPointer f0 _) (Base_FunctionPtr _ _ _)      = True
  separate_bases (Base_StackPointer f0 _) (Base_TLS _)                  = True
  separate_bases (Base_StackPointer f0 _) (Base_StatePart _ _)          = True -- TODO add vc
  separate_bases (Base_StackPointer f0 _) (Base_Immediate _ _)          = True
  separate_bases (Base_StackPointer f0 _) (Base_FunctionReturn _ _)     = True-- TODO add vc

  separate_bases (Base_Malloc id0 h0 _)   (Base_Malloc id1 h1 _)        = id0 /= id1
  separate_bases (Base_Malloc id0 h0 _)   (Base_FunctionPtr _ _ _)      = True
  separate_bases (Base_Malloc id0 h0 _)   (Base_TLS _)                  = True
  separate_bases (Base_Malloc id0 h0 _)   (Base_StatePart _ _)          = True
  separate_bases (Base_Malloc id0 h0 _)   (Base_Immediate _ _)          = True
  separate_bases (Base_Malloc id0 h0 _)   (Base_FunctionReturn _ _)     = True


  separate_bases (Base_FunctionPtr a0 _ _) (Base_FunctionPtr a1 _ _)    = a0 /= a1
  separate_bases (Base_FunctionPtr a0 _ _) (Base_TLS _)                 = True
  separate_bases (Base_FunctionPtr a0 _ _) (Base_StatePart _  _)        = True
  separate_bases (Base_FunctionPtr a0 _ _) (Base_Immediate i1 _)        = True -- TODO???
  separate_bases (Base_FunctionPtr a0 _ _) (Base_FunctionReturn _ _)    = True-- TODO add vc

  separate_bases (Base_TLS _)              (Base_TLS _)                 = False
  separate_bases (Base_TLS _)              (Base_StatePart _ _)         = True
  separate_bases (Base_TLS _)              (Base_Immediate _ _)         = True
  separate_bases (Base_TLS _)              (Base_FunctionReturn _ _)    = True-- TODO add vc


  separate_bases (Base_StatePart sp0 _)    (Base_StatePart sp1 _)       = 
    case M.lookup (mk_sstatepart fctxt sp0,mk_sstatepart fctxt sp1) m of
      Just Separate -> True
      _             -> False
  separate_bases (Base_StatePart sp0 _)    (Base_Immediate imm1 _)      =
    case find (\(sp',_) -> sp0 == sp') sps of
      Just (_,Just imm) -> error "Should not happen?" -- cseparate fctxt msg a0 si0 imm si1
      _ -> True 
  separate_bases (Base_StatePart sp0 _)    (Base_FunctionReturn _ _)    = True-- TODO add vc



  separate_bases (Base_Immediate i0 (PtrOffset off0)) (Base_Immediate i1 UnknownOffset)  = i0 + off0 + si0 <= i1 -- TODO add VC
  separate_bases (Base_Immediate i0 UnknownOffset) (Base_Immediate i1  (PtrOffset off1)) = i1 + off1 + si1 <= i0 -- TODO add VC
  separate_bases (Base_Immediate i0 _)     (Base_Immediate i1 _)
    | pointers_from_different_global_section (f_ctxt fctxt) i0 i1      = True -- TODO ADD VC
    | i0 /= i1                                                         = False -- REMOVE TODO ADD VC error $ "Separation of " ++ show (a0, si0, a1, si1) ++ "\nFINIT ==\n" ++ show (f_init fctxt) ++ "\nmsg = " ++ msg
    | otherwise                                                        = False
  separate_bases (Base_Immediate i0 _)     (Base_FunctionReturn _ _)   = True-- TODO add vc



  separate_bases (Base_FunctionReturn f0 _ ) (Base_FunctionReturn f1 _) = f0/=f1-- TODO add vc


  separate_bases (Base_ReturnAddr f0)      (Base_ReturnAddr f1)        = f0 /= f1
  separate_bases (Base_ReturnAddr f0)      _                           = True
  separate_bases _                         (Base_ReturnAddr f0)        = True

  separate_bases b0 b1 = separate_bases b1 b0

  FInit sps m = f_init fctxt 



calias fctxt a0 si0 a1 si1 = and
  [ si0 /= UnknownSize
  , si1 /= UnknownSize
  , not $ has_unknown_offset a0
  , not $ has_unknown_offset a1
  , (a0,si0) == (a1,si1) ]

{--
  -- TODO experiment with this
  aliassing_ptrvalues si0' si1' (Base_StatePart sp0 (PtrOffset off0)) (Base_StatePart sp1 (PtrOffset off1)) = off0 == off1 && or
    [ sp0 == sp1
    , M.lookup (mk_sstatepart fctxt sp0,mk_sstatepart fctxt sp1) m == Just Aliassing ]
  aliassing_ptrvalues si0' si1' ptr0 ptr1 = and
    [ ptr0 == ptr1
    , not $ has_unknown_offset ptr0
    , not $ has_unknown_offset ptr1 ]

  FInit _ m = f_init fctxt
--}

cenclosed fctxt a0 (Nat si0) a1 (Nat si1) = 
  let v0 = spointer_to_expr a0
      v1 = spointer_to_expr a1 in
    necessarily_enclosed v0 si0 v1 si1
cenclosed fctxt a0 _ a1 _ = False




csensitive fctxt a (Nat si) v =
  case ctry_deterministic v of
    (Just v') -> is_top_stackframe a si || is_pushed_reg a si v' 
    _         -> False
 where
  is_initial_reg (SE_Var (SP_Reg _)) = True
  is_initial_reg _                   = False
  
  is_top_stackframe (Base_StackPointer _ (PtrOffset 0)) _ = True
  is_top_stackframe _ _ = False
  is_pushed_reg a' si' v' = is_initial_reg v' && is_local_spointer fctxt a'
csensitive fctxt a UnknownSize v = False


cread_from_ro_data fctxt (Base_Immediate a (PtrOffset 0)) (Nat si) = cimmediate fctxt <$> read_from_ro_datasection (f_ctxt fctxt) a (fromIntegral si)
cread_from_ro_data fctxt _ _ = Nothing

cread_from_data fctxt (Base_Immediate a (PtrOffset 0)) (Nat si) = cimmediate fctxt <$> read_from_datasection (f_ctxt fctxt) a (fromIntegral si)
cread_from_data fctxt _ _ = Nothing

ctry_relocation fctxt (Base_Immediate a (PtrOffset 0)) (Nat si) = try_reloc a <|> ((\_ -> mk_value a) <$> try_relocated_pointer fctxt a)
 where
  ctxt = f_ctxt fctxt

  try_reloc a' = get_trgt <$> (find (is_reloc_for a') $ ctxt_relocs $ f_ctxt fctxt)
  is_reloc_for a' (Relocation a0 a1) = a0 == a'
  get_trgt (Relocation a0 a1) = cimmediate fctxt a1

  mk_value a' = mk_concreteS fctxt $ SE_Var $ SP_Mem (SE_Immediate a') 8

ctry_relocation fctxt _ _ = Nothing


-- If *[a,8] contains a relocated value to some function f, return that function
try_relocated_pointer fctxt a =
  case IM.lookup (fromIntegral a) $ ctxt_symbol_table ctxt of
    Just (Relocated_Function f) -> Just f
    _ -> Nothing
 where
  ctxt = f_ctxt fctxt




instance SymbolicExecutable FContext SValue SPointer where
  sseparate                = cseparate
  senclosed                = cenclosed
  salias                   = calias
  ssensitive               = csensitive
  sread_from_ro_data       = cread_from_ro_data
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
  sjump                    = jump
  scall                    = call
  stry_jump_targets        = ctry_jump_targets
  stry_immediate           = \_ -> ctry_immediate 
  stry_deterministic       = \_ -> ctry_deterministic 
  stry_relocation          = ctry_relocation
  saddress_has_instruction = \ctxt -> address_has_instruction (f_ctxt ctxt)


instance Propagator FContext Predicate where
  tau     = sexec_block
  join    = sjoin_states
  implies = simplies






-- get the currently known invariant for the given instruction address
get_invariant :: FContext -> Int -> Maybe Predicate
get_invariant fctxt a = do
  let ctxt   = f_ctxt fctxt
  let entry  = f_entry fctxt
  g         <- IM.lookup entry $ ctxt_cfgs   ctxt
  invs      <- IM.lookup entry $ ctxt_invs   ctxt
  blockId   <- IM.lookup a $ cfg_addr_to_blockID g
  p         <- IM.lookup blockId invs
  instrs    <- IM.lookup blockId $ cfg_instrs g

  return $ fst $ sexec_block fctxt (takeWhile (\i -> fromIntegral (addressof i) /= a) instrs) Nothing p



-- | The initial predicate.
init_pred ::
  FContext                      -- ^ The current context
  -> String                     -- ^ The current function
  -> Invariants                 -- ^ The currently available invariants
  -> S.Set (NodeInfo,Predicate) -- ^ The currently known postconditions
  -> S.Set SStatePart            -- ^ The currently known stateparts of the function
  -> Predicate
init_pred fctxt f curr_invs curr_posts curr_sps =
  let FInit finit _        = f_init fctxt -- M.filter (not . contains_bot) $ 

      sps                  = S.unions [curr_sps, S.map (mk_sstatepart fctxt) $ S.map fst finit, (S.delete (SSP_Reg RIP) $ gather_stateparts curr_invs curr_posts)]
      (regs,regions)       = partitionWith reg_or_mem $ S.toList sps

      rsp0                 = SConcrete $ NES.singleton $ SE_Var $ SP_StackPointer f
      write_stack_pointer  = execSstate (swrite_rreg fctxt RSP rsp0)
      top_stack_frame      = SConcrete $ NES.singleton $ SE_Var $ SP_Mem (SE_Var $ SP_StackPointer f) 8
      write_return_address = execSstate (swrite_mem fctxt True rsp0 (Nat 8) top_stack_frame)

      sregs                = M.fromList $ map (\r -> (r,mk_concreteS fctxt $ SE_Var (SP_Reg r))) regs
      smem                 = M.empty in
    write_stack_pointer $ write_return_address $ write_finit (S.toList finit) $ (Sstate sregs smem None) 
 where
  reg_or_mem (SSP_Reg r) = Left r
  reg_or_mem (SSP_Mem a si) = Right (a,si)

  write_finit [] s                   = s
  write_finit ((sp,Nothing):finit) s = write_finit finit s
  write_finit ((sp,Just v):finit)  s = write_finit finit $ execSstate (write_sp fctxt (mk_sstatepart fctxt sp) v) s

  


-- | Given the currently known invariants and postconditions, gather all stateparts occurring in the current function.
gather_stateparts ::
     Invariants                 -- ^ The currently available invariants
  -> S.Set (NodeInfo,Predicate) -- ^ The currently known postconditions
  -> S.Set SStatePart
gather_stateparts invs posts = S.unions [IM.foldr accumulate_stateparts S.empty invs, get_stateparts_of_sstates (S.map snd posts)]
 where
  accumulate_stateparts p = S.union (get_stateparts_of_sstate p)

  get_stateparts_of_sstates ps = S.unions $ map get_stateparts_of_sstate $ S.toList $ ps

get_stateparts_of_sstate (Sstate sregs smem _) = S.unions [gather_regs sregs, gather_reg_values sregs, gather_regions smem, gather_mem_values smem]
 where
  gather_regs       regs = S.fromList $ map SSP_Reg $ M.keys regs 
  gather_reg_values regs = S.empty -- TODO
  gather_regions    mem  = S.fromList $ catMaybes $ map mk_mem_region $ M.assocs mem
  gather_mem_values mem  = S.empty -- TODO

  mk_mem_region ((a,Nat si),_)
    | has_unknown_offset a = Nothing
    | otherwise            = Just $ SSP_Mem a $ fromIntegral si
  mk_mem_region _ = Nothing





mapMaybeS :: Ord b => (a -> Maybe b) -> S.Set a -> S.Set b
mapMaybeS f = S.map fromJust . S.filter ((/=) Nothing) . S.map f

mapMaybeNES :: Ord b => (a -> Maybe b) -> NES.NESet a -> S.Set b
mapMaybeNES f = S.map fromJust . NES.filter ((/=) Nothing) . NES.map f

-- | Convert the current invariant into a function initialisation
invariant_to_finit :: FContext -> Predicate -> FInit
---invariant_to_finit fctxt p | trace ("invariant_to_finit: "++ show p) False = error "trace"
invariant_to_finit fctxt p = 
  let sps   = mapMaybeS maybe_read_sp $ get_stateparts_of_sstate p
      pairs = S.toList $ S.filter (\(x,y) -> x /= y) $ S.cartesianProduct sps sps
      finit = FInit (S.map keep_globals sps) (M.fromList $ map mk_memrel pairs) in
    finit -- trace ("Turning into finit, precondition: \n" ++ show p ++ "\n-->\n" ++ show finit) finit 
 where
  keep_globals (sp,v,_) 
    | is_global_value v = (mk_sp sp,Just v)
    | otherwise         = (mk_sp sp,Nothing)

  maybe_read_sp sp
    | suitable_sp sp =
      case evalSstate (read_sp fctxt sp) p of
        Top -> Nothing
        SAddends es -> Nothing -- TODO!!!
        SConcrete es -> 
          let es'  = NES.filter (\e -> try_promote_expr fctxt True e /= Nothing) es
              ptrs = S.map (fromJust . try_promote_expr fctxt True) es' in
            if S.null ptrs then
              Nothing
            else
              Just (sp,mk_concrete fctxt "finit" $ NES.unsafeFromSet es',ptrs)
    | otherwise = Nothing

  suitable_sp (SSP_Reg r)    = r `notElem` [RIP,RSP,RBP]
  suitable_sp (SSP_Mem a si) = is_global_ptr a && not (contains_bot $ spointer_to_expr a)


  mk_memrel ((sp0,v0,ptrs0),(sp1,v1,ptrs1))
    | all (\ptr0 -> all (\ptr1 -> cseparate fctxt "invariant_to_finit" ptr0 UnknownSize ptr1 UnknownSize) ptrs1) ptrs0 = ((sp0,sp1),Separate)
    | all (\ptr0 -> all (\ptr1 -> calias fctxt ptr0 UnknownSize ptr1 UnknownSize) ptrs1) ptrs0 = ((sp0,sp1),Aliassing)
    | otherwise = ((sp0,sp1),Unknown) -- TODO

  is_global_value (SConcrete es) = all is_global_ptr $ mapMaybeNES (try_promote_expr fctxt True) es

  is_global_ptr (Base_Immediate _ _)   = True
  is_global_ptr _                    = False

  mk_sp (SSP_Reg r)    = SP_Reg r
  mk_sp (SSP_Mem a si) = SP_Mem (spointer_to_expr a) si





-- | The join between two function initialisations
join_finit :: FContext -> FInit -> FInit -> FInit
join_finit fctxt f0@(FInit sps0 m0) f1@(FInit sps1 m1)
  | f0 == f1 = f0
  --  | f0 == init_finit = f1
  --  | f1 == init_finit = f0
  | otherwise = FInit (S.intersection sps0 sps1) (M.intersectionWith join_rel m0 m1)
 where
  join_rel r0 r1
    | r0 == r1  = r0
    | otherwise = Unknown





















data ExternalFunctionOutput = FreshPointer | UnknownReturnValue | Input Register

data ExternalFunctionBehavior = ExternalFunctionBehavior {
  f_inputs :: [Register],
  f_output :: ExternalFunctionOutput
 }


param 0 = RDI 
param 1 = RSI
param 2 = RDX
param 3 = RCX
param 4 = R8
param 5 = R9


pure_and_fresh = ExternalFunctionBehavior [] FreshPointer
pure_and_unknown = ExternalFunctionBehavior [] UnknownReturnValue

external_function_behavior :: FContext -> String -> ExternalFunctionBehavior
-- | a list of some function that return a heap-pointer through RAX.
-- The pointer is assumed to  be fresh.
external_function_behavior _ "_malloc" = pure_and_fresh
external_function_behavior _ "malloc" = pure_and_fresh
external_function_behavior _ "_malloc_create_zone" = pure_and_fresh
external_function_behavior _ "_malloc_default_zone" = pure_and_fresh
external_function_behavior _ "_malloc_zone_malloc" = pure_and_fresh
external_function_behavior _ "_calloc" = pure_and_fresh
external_function_behavior _ "calloc" = pure_and_fresh
external_function_behavior _ "_malloc_zone_calloc" = pure_and_fresh
external_function_behavior _ "_mmap" = pure_and_fresh
external_function_behavior _ "_av_mallocz" = pure_and_fresh
external_function_behavior _ "___error" = pure_and_fresh
external_function_behavior _ "_localeconv" = pure_and_fresh
external_function_behavior _ "localeconv" = pure_and_fresh
external_function_behavior _ "strerror" = pure_and_fresh
external_function_behavior _ "_strerror" = pure_and_fresh
external_function_behavior _ "_strerror_r" = pure_and_fresh
external_function_behavior _ "_wcserror" = pure_and_fresh
external_function_behavior _ "__wcserror" = pure_and_fresh
external_function_behavior _ "_EVP_CIPHER_CTX_new" = pure_and_fresh
external_function_behavior _ "strdup" = pure_and_fresh
external_function_behavior _ "_strdup" = pure_and_fresh
external_function_behavior _ "_getenv" = pure_and_fresh
external_function_behavior _ "getenv" = pure_and_fresh
external_function_behavior _ "_open" = pure_and_fresh
external_function_behavior _ "_fts_read$INODE64" = pure_and_fresh
external_function_behavior _ "_fts_open$INODE64" = pure_and_fresh
external_function_behavior _ "_opendir$INODE64" = pure_and_fresh
external_function_behavior _ "fopen" = pure_and_fresh
external_function_behavior _ "_fopen" = pure_and_fresh
external_function_behavior _ "_fdopen" = pure_and_fresh
external_function_behavior _ "_wfdopen" = pure_and_fresh
external_function_behavior _ "_fgetln" = pure_and_fresh
external_function_behavior _ "fgetln" = pure_and_fresh
external_function_behavior _ "_setlocale" = pure_and_fresh
external_function_behavior _ "_wsetlocale" = pure_and_fresh
external_function_behavior _ "__ctype_b_loc" = pure_and_fresh
external_function_behavior _ "dcgettext" = pure_and_fresh
external_function_behavior _ "nl_langinfo" = pure_and_fresh
external_function_behavior _ "setlocale" = pure_and_fresh
external_function_behavior _ "__errno_location" = pure_and_fresh
external_function_behavior _ "_popen" = pure_and_fresh
external_function_behavior _ "__ctype_tolower_loc" = pure_and_fresh
external_function_behavior _ "__ctype_toupper_loc" = pure_and_fresh
external_function_behavior _ "readdir" = pure_and_fresh
external_function_behavior _ "getmntent" = pure_and_fresh
external_function_behavior _ "setmntent" = pure_and_fresh
external_function_behavior _ "hasmntopt" = pure_and_fresh
-- | A list of some functions that are assumed not to change the state in any significant way, and that return an unknown bottom value through RAX
external_function_behavior _ "feof" = pure_and_unknown
external_function_behavior _ "_feof" = pure_and_unknown
external_function_behavior _ "_getc" = pure_and_unknown
external_function_behavior _ "getc" = pure_and_unknown
external_function_behavior _ "fgetc" = pure_and_unknown
external_function_behavior _ "_fgetc" = pure_and_unknown
external_function_behavior _ "_fgetwc" = pure_and_unknown
external_function_behavior _ "fgetwc" = pure_and_unknown
external_function_behavior _ "_fnmatch" = pure_and_unknown
external_function_behavior _ "_fputc" = pure_and_unknown
external_function_behavior _ "fputc" = pure_and_unknown
external_function_behavior _ "_close" = pure_and_unknown
external_function_behavior _ "close" = pure_and_unknown
external_function_behavior _ "fwrite" = pure_and_unknown
external_function_behavior _ "_fwrite" = pure_and_unknown
external_function_behavior _ "_fflush" = pure_and_unknown
external_function_behavior _ "___maskrune" = pure_and_unknown
external_function_behavior _ "_getbsize" = pure_and_unknown
external_function_behavior _ "_printf" = pure_and_unknown
external_function_behavior _ "printf" = pure_and_unknown
external_function_behavior _ "vprintf" = pure_and_unknown
external_function_behavior _ "_fprintf" = pure_and_unknown
external_function_behavior _ "fprintf" = pure_and_unknown
external_function_behavior _ "vfprintf" = pure_and_unknown
external_function_behavior _ "_fprintf_l" = pure_and_unknown
external_function_behavior _ "fwprintf" = pure_and_unknown
external_function_behavior _ "_fwprintf_l" = pure_and_unknown
external_function_behavior _ "__fprintf_chk" = pure_and_unknown
external_function_behavior _ "__printf_chk" = pure_and_unknown
external_function_behavior _ "_putchar" = pure_and_unknown
external_function_behavior _ "_puts" = pure_and_unknown
external_function_behavior _ "fputs" = pure_and_unknown
external_function_behavior _ "_fputs" = pure_and_unknown
external_function_behavior _ "_btowc" = pure_and_unknown
external_function_behavior _ "btowc" = pure_and_unknown
external_function_behavior _ "mbtowc" = pure_and_unknown
external_function_behavior _ "_mbtowc" = pure_and_unknown
external_function_behavior _ "_mbrtowc" = pure_and_unknown
external_function_behavior _ "mbrtowc" = pure_and_unknown
external_function_behavior _ "_atof" = pure_and_unknown
external_function_behavior _ "atof" = pure_and_unknown
external_function_behavior _ "_strcmp" = pure_and_unknown
external_function_behavior _ "_strncmp" = pure_and_unknown
external_function_behavior _ "strcmp" = pure_and_unknown
external_function_behavior _ "strncmp" = pure_and_unknown
external_function_behavior _ "strlen" = pure_and_unknown
external_function_behavior _ "_ilogb" = pure_and_unknown
external_function_behavior _ "_atoi" = pure_and_unknown
external_function_behavior _ "_getopt" = pure_and_unknown
external_function_behavior _ "getopt_long" = pure_and_unknown
external_function_behavior _ "_free" = pure_and_unknown
external_function_behavior _ "_warn" = pure_and_unknown
external_function_behavior _ "_warnx" = pure_and_unknown
external_function_behavior _ "__errno_location" = pure_and_unknown
external_function_behavior _ "__libc_start_main" = pure_and_unknown
external_function_behavior _ "__cxa_finalize" = pure_and_unknown
external_function_behavior _ "perror" = pure_and_unknown
external_function_behavior _ "fclose" = pure_and_unknown
external_function_behavior _ "free" = pure_and_unknown
external_function_behavior _ "unlink" = pure_and_unknown
external_function_behavior _ "unlinkat" = pure_and_unknown
external_function_behavior _ "strspn" = pure_and_unknown
external_function_behavior _ "utimensat" = pure_and_unknown
external_function_behavior _ "fdatasync" = pure_and_unknown
external_function_behavior _ "fsync" = pure_and_unknown
external_function_behavior _ "isatty" = pure_and_unknown
external_function_behavior _ "strcspn" = pure_and_unknown
external_function_behavior _ "memcmp" = pure_and_unknown
external_function_behavior _ "_memcmp" = pure_and_unknown
external_function_behavior _ "isprint" = pure_and_unknown
external_function_behavior _ "iswprint" = pure_and_unknown
external_function_behavior _ "_isprint_l" = pure_and_unknown
external_function_behavior _ "_iswprint_l" = pure_and_unknown
external_function_behavior _ "__cxa_atexit" = pure_and_unknown
external_function_behavior _ "towlower" = pure_and_unknown
external_function_behavior _ "towupper" = pure_and_unknown
external_function_behavior _ "iswalnum" = pure_and_unknown
external_function_behavior _ "fseeko" = pure_and_unknown
external_function_behavior _ "fflush" = pure_and_unknown
external_function_behavior _ "_fclose" = pure_and_unknown
external_function_behavior _ "_fgets" = pure_and_unknown
external_function_behavior _ "_ferror" = pure_and_unknown
external_function_behavior _ "_strtol" = pure_and_unknown
external_function_behavior _ "_strtoul" = pure_and_unknown
external_function_behavior _ "_munmap" = pure_and_unknown



-- | A list of some functions that return bottom and write to pointers passed by parameters
--external_function_behavior _ "_sysctlbyname" = ExternalFunctionBehavior [param 2, param 4] UnknownReturnValue
--external_function_behavior _ "_fstat$INODE64" = ExternalFunctionBehavior [param 1] UnknownReturnValue
--external_function_behavior _ "_fstatfs$INODE64" = ExternalFunctionBehavior [param 1] UnknownReturnValue
--external_function_behavior _ "_statfs$INODE64" = ExternalFunctionBehavior [param 1] UnknownReturnValue
external_function_behavior _ "snprintf"             = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "_snprintf"            = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "_snprintf_l"          = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "_snwprintf"           = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "_snwprintf_l"         = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "__snprintf_chk"       = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "_vsnprintf"           = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "sprintf"              = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "_sprintf"             = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "___bzero"             = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "sigprocmask"          = ExternalFunctionBehavior [param 2] UnknownReturnValue
external_function_behavior _ "__strcat_chk"         = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "strcat"               = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "strlcpy"              = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "___strlcpy_chk"       = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "sigemptyset"          = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior _ "sigaction"            = ExternalFunctionBehavior [param 2] UnknownReturnValue
external_function_behavior _ "localtime"            = ExternalFunctionBehavior [param 0] FreshPointer
external_function_behavior _ "memset"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "_memset"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "__memset_chk"         = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "___memset_chk"        = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "_index"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_rindex"              = ExternalFunctionBehavior [] $ Input $ param 0

-- A list of functions that return a pointer given to them by a parameter
external_function_behavior _ "_realloc"             = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_malloc_zone_realloc" = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_recallocarray"       = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "realloc"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_strcpy"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "__strcpy_chk"         = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "_strncpy"             = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "strcpy"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "strncpy"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "stpcpy"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "memcpy"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "_memcpy"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "__memcpy_chk"         = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "___memcpy_chk"        = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "__memmove_chk"        = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "memmove"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "_memmove"             = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "strcat"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "_strcat"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior _ "strchr"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_strchr"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "strrchr"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_strrchr"             = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_memchr"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "memchr"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "strstr"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_strstr"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_strpbrk"             = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_strtok"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "strtok"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_strlen"              = ExternalFunctionBehavior [] $ Input $ param 0


external_function_behavior fctxt f
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

transpose_bw_offset :: FContext -> PtrOffset -> SPointer -> SPointer
transpose_bw_offset fctxt (PtrOffset off) v
  | has_unknown_offset v = v
  | otherwise            = mod_offset ((+) off) v
transpose_bw_offset fctxt UnknownOffset v   = set_unknown_offset fctxt "transpose_bw_offset" v

transpose_bw_spointer :: FContext -> Sstate SValue SPointer -> Sstate SValue SPointer -> SPointer -> Maybe SPointer
transpose_bw_spointer fctxt p q b@(Base_StackPointer f offset)   = error $ "TODO" ++ show (b)-- transpose_bw_offset fctxt offset $ evalSstate (read_sp fctxt (SSP_Reg RSP)) p
transpose_bw_spointer fctxt p q b@(Base_Immediate a offset)      = Just b
transpose_bw_spointer fctxt p q b@(Base_Malloc _ _ offset)       = Just b
transpose_bw_spointer fctxt p q b@(Base_FunctionPtr _ _ offset)  = Just b
transpose_bw_spointer fctxt p q b@(Base_ReturnAddr _)            = error $ "Transposition of return address"
transpose_bw_spointer fctxt p q b@(Base_TLS offset)              = error $ "Transposition of TLS"
transpose_bw_spointer fctxt p q b@(Base_FunctionReturn f offset) = Just b
transpose_bw_spointer fctxt p q b@(Base_StatePart sp offset)     =
  let ptrs = cmk_mem_addresses fctxt ("transpose_bw_spointer\n" ++ show p ++ "\n\n" ++ show q ++ "\n\nb = " ++ show b ++ "\ne = " ++ show (transpose_bw_e fctxt p (SE_Var sp))) $ transpose_bw_e fctxt p (SE_Var sp) in
     if S.size ptrs == 1 then
      Just $ S.findMin ptrs
    else
      Nothing 

transpose_bw_addend :: FContext -> Sstate SValue SPointer -> Sstate SValue SPointer -> NES.NESet SAddend -> SValue
transpose_bw_addend fctxt p q ptrs = cwiden fctxt "transpose_bw_addend" $ NES.foldr1 (svalue_plus fctxt 64) $ NES.map transpose_bw_saddend ptrs
 where
  transpose_bw_saddend :: SAddend -> SValue
  transpose_bw_saddend (SAddend_StackPointer f)    = evalSstate (read_sp fctxt (SSP_Reg RSP)) p
  transpose_bw_saddend (SAddend_Immediate a)       = cimmediate fctxt a
  transpose_bw_saddend (SAddend_Malloc id hash)    = mk_concreteS fctxt $ SE_Malloc id hash
  transpose_bw_saddend (SAddend_FunctionPtr a _)   = mk_concreteS fctxt $ SE_Var $ SP_Mem (SE_Immediate a) 8
  transpose_bw_saddend (SAddend_ReturnAddr _)      = error $ "Transposition of return address"
  transpose_bw_saddend (SAddend_TLS)               = error $ "Transposition of TLS"
  transpose_bw_saddend (SAddend_StatePart sp)      = transpose_bw_e fctxt p (SE_Var sp) 
  transpose_bw_saddend (SAddend_FunctionReturn f)  = mk_concreteS fctxt $ Bottom $ FromCall f


transpose_bw_svalue :: FContext -> Sstate SValue SPointer -> Sstate SValue SPointer -> SValue -> SValue
transpose_bw_svalue fctxt p q (SConcrete es)  = cjoin_all fctxt "transpose_bw" $ NES.map (transpose_bw_e fctxt p) es
transpose_bw_svalue fctxt p q (SAddends adds) = cjoin_all fctxt "transpose_bw" $ NES.map (transpose_bw_addend fctxt p q) adds
transpose_bw_svalue fctxt p q t@Top           = t


    
transpose_bw_reg :: FContext -> Sstate SValue SPointer -> Sstate SValue SPointer -> (Register, SValue) -> Maybe (Register, SValue)
transpose_bw_reg fctxt p q (r,v) =
  let v' = transpose_bw_svalue fctxt p q v in
    Just $ (r,v')

transpose_bw_mem :: FContext -> Sstate SValue SPointer -> Sstate SValue SPointer -> ((SPointer,RegionSize), SValue) -> Maybe ((SPointer,RegionSize), SValue)
transpose_bw_mem fctxt p q ((a,si),v) =
  case transpose_bw_spointer fctxt p q a of
    Just a' -> Just ((a',si), transpose_bw_svalue fctxt p q v)
    Nothing -> Nothing




transpose_bw_e :: FContext -> Sstate SValue SPointer -> SimpleExpr -> SValue
transpose_bw_e fctxt p (Bottom (FromCall f))            = mk_concreteS fctxt $ Bottom (FromCall f)
transpose_bw_e fctxt p (SE_Malloc id hash)              = mk_concreteS fctxt $ SE_Malloc id hash
transpose_bw_e fctxt p (SE_Immediate i)                 = cimmediate fctxt i
transpose_bw_e fctxt p (SE_StatePart sp)                = error "Should not happen"
transpose_bw_e fctxt p (SE_Var (SP_StackPointer f))     = evalSstate (sread_reg fctxt RSP) p
transpose_bw_e fctxt p (SE_Var sp)                      = transpose_bw_sp fctxt p sp
transpose_bw_e fctxt p (SE_Bit i e)                     = csemantics fctxt "transpose_bw" $ SO_Bit i $ transpose_bw_e fctxt p e
transpose_bw_e fctxt p (SE_SExtend l h e)               = csemantics fctxt "transpose_bw" $ SO_SExtend l h $ transpose_bw_e fctxt p e
transpose_bw_e fctxt p (SE_Op Plus si [a,b])            = csemantics fctxt "transpose_bw" $ SO_Op ADD (si `div` 8) Nothing [transpose_bw_e fctxt p a,transpose_bw_e fctxt p b]
transpose_bw_e fctxt p (SE_Op Minus si [a,b])           = csemantics fctxt "transpose_bw" $ SO_Op SUB (si `div` 8) Nothing [transpose_bw_e fctxt p a,transpose_bw_e fctxt p b]
transpose_bw_e fctxt p (SE_Op op si es)                 = apply_expr_op fctxt "transpose_bw" (mk_expr fctxt . SE_Op op si) $ map (transpose_bw_e fctxt p) es
transpose_bw_e fctxt p (SE_Overwrite i a b)             = csemantics fctxt "transpose_bw" $ SO_Overwrite i (transpose_bw_e fctxt p a) (transpose_bw_e fctxt p b)

transpose_bw_sp :: FContext -> Sstate SValue SPointer -> StatePart -> SValue
transpose_bw_sp fctxt p (SP_Reg r)    = evalSstate (sread_reg fctxt r) p
transpose_bw_sp fctxt p (SP_Mem a si) = 
  let a' = transpose_bw_e fctxt p a in
    evalSstate (sread_mem fctxt "transpose_bw_sp" a' $ Nat $ fromIntegral si) p


read_sp :: FContext -> SStatePart -> State (Sstate SValue SPointer, VCS) SValue
read_sp fctxt (SSP_Reg r)    = sread_reg fctxt r
read_sp fctxt (SSP_Mem a si) = sread_mem_from_ptr fctxt "read_sp" a $ (Nat $ fromIntegral si)

write_sp :: FContext -> SStatePart -> SValue -> State (Sstate SValue SPointer, VCS) ()
write_sp fctxt (SSP_Reg r)    v = swrite_reg fctxt r v
write_sp fctxt (SSP_Mem a si) v = swrite_mem_to_ptr fctxt True a (Nat $ fromIntegral si) v


data FunctionType = AnalyzedInternalFunction (Sstate SValue SPointer) | ExternalFunction | AnalyzedInternalFunctionTerminates | AnalyzedInternalFunctionUnknown

get_function_type fctxt i f_callee =
  ftype $ map postcondition_of_jump_target $ resolve_jump_target (f_ctxt fctxt) i
 where
  ftype posts
    | all ((==) (Just Terminating)) posts                                = AnalyzedInternalFunctionTerminates
    | all is_returning posts                                             = AnalyzedInternalFunction $ supremum fctxt $ map fromReturning posts
    | "0x" `isPrefixOf` f_callee || "indirection@" `isPrefixOf` f_callee = AnalyzedInternalFunctionUnknown
    | otherwise                                                          = ExternalFunction

  fromReturning (Just (ReturningWith q)) = q
  is_returning  (Just (ReturningWith q)) = True
  is_returning  _                        = False

  postcondition_of_jump_target (ImmediateAddress a) = IM.lookup (fromIntegral a) (ctxt_calls $ f_ctxt fctxt)
  postcondition_of_jump_target _                    = Nothing



-- | Executes semantics for external functions.
call :: FContext -> Bool -> X86.Instruction -> State (Sstate SValue SPointer,VCS) ()
call fctxt is_jump i = do
  case get_function_type fctxt i f_callee of
    AnalyzedInternalFunctionUnknown    -> unknown_internal_function fctxt i
    AnalyzedInternalFunctionTerminates -> incr_rsp
    AnalyzedInternalFunction q         -> internal_function q
    ExternalFunction                   -> external_function 
 where
  external_function = case external_function_behavior fctxt f_callee of
    ExternalFunctionBehavior params output -> {--mapM_ write_param params >> --} write_output output >> incr_rsp-- writing to params really roughly overapproximates

  write_output :: ExternalFunctionOutput -> State (Sstate SValue SPointer,VCS) ()
  write_output FreshPointer       = swrite_reg fctxt RAX $ (mk_concreteS fctxt $ SE_Malloc (Just (addressof i)) (Just ""))
  write_output UnknownReturnValue = swrite_reg fctxt RAX $ (mk_concreteS fctxt $ Bottom (FromCall f_callee))
  write_output (Input r)          = sread_reg fctxt r >>= swrite_reg fctxt RAX

  incr_rsp
    | is_jump   = sexec_instr fctxt (Instruction (AddressWord64 0) Nothing ADD Nothing [Storage RSP, Immediate 8] Nothing)
    | otherwise = return ()

  decr_rsp
    | not is_jump  = sexec_instr fctxt (Instruction (AddressWord64 0) Nothing SUB Nothing [Storage RSP, Immediate 8] Nothing)
    | otherwise    = return ()

  internal_function :: Sstate SValue SPointer -> State (Sstate SValue SPointer,VCS) ()
  internal_function q = do
    -- push return address if is call
    decr_rsp

    (p,vcs) <- get
    -- obtain the postcondition of the function, and do backwards transposition
    let q_eqs_transposed_regs  = catMaybes $ map (transpose_bw_reg fctxt p q) $ filter ((/=) RIP . fst) $ sstate_to_reg_eqs q
    let q_eqs_transposed_mem   = catMaybes $ map (transpose_bw_mem fctxt p q) $ filter do_transfer $ sstate_to_mem_eqs q
    -- write transposed postcondition to current state
    mapM_ (\((a,si),v) -> swrite_mem_to_ptr fctxt True a si v) $ q_eqs_transposed_mem 
    mapM_ (\(r,v) -> swrite_reg fctxt r v) $ q_eqs_transposed_regs


  -- in case of an external function, which is passed a parameter $r$ 
  -- do a write to region [r+bot,1] to muddle the state. The value written to that region is an abstraction of what is already there.
  write_param :: Register -> State (Sstate SValue SPointer,VCS) ()
  write_param r = do
    a      <- sread_reg fctxt r
    let a'  = cwiden fctxt "write_param" a
    v'     <- gets ((evalSstate $ sread_mem fctxt "write_param" a UnknownSize) . fst)
    let bot = cwiden fctxt "write_param_v" v'
    swrite_mem fctxt True a' UnknownSize bot


  do_transfer ((a,si),v) = not (is_initial (a,si) v) && not (is_top_stackframe a si) && not (is_local_spointer fctxt a)
  
  is_initial :: (SPointer,RegionSize) -> SValue -> Bool
  is_initial (a,si) v = False {-- "TODO: " ++ show (a,si,v) 
    case (ctry_deterministic a, ctry_immediate si) of
      (Just a', Just si') -> v == cmk_init_mem_value fctxt "is_initial" a si
      _                   -> False--}

  is_top_stackframe (Base_StackPointer _ (PtrOffset 0)) (Nat 8) = True  
  is_top_stackframe _ _ = False

  f_name  = function_name_of_entry (f_ctxt fctxt) (f_entry fctxt)
  f_callee = function_name_of_instruction (f_ctxt fctxt) i

  sstate_to_reg_eqs (Sstate regs _ _) = M.toList regs
  sstate_to_mem_eqs (Sstate _ mem _)  = M.toList mem



  unknown_internal_function fctxt i = incr_rsp -- TODO try as external

is_local_spointer fctxt (Base_StackPointer _ _)  = True
is_local_spointer fctxt b@(Base_StatePart sp _ ) = is_local_var fctxt $ SE_Var sp
is_local_spointer fctxt _                        = False

is_local_var fctxt (SE_Var (SP_StackPointer _)) = True
is_local_var fctxt (SE_Var (SP_Mem a si))       = is_local_expr a /= Nothing
is_local_var fctxt _                            = False


jump :: FContext -> X86.Instruction -> State (Sstate SValue SPointer,VCS) ()
jump fctxt i
  | jump_is_actually_a_call (f_ctxt fctxt) i = call fctxt True i >> sexec_instr fctxt (Instruction (AddressWord64 0) Nothing SUB Nothing [Storage RSP, Immediate 8] Nothing) >> sreturn fctxt
  | otherwise                                = return ()


ctry_jump_targets :: FContext -> SValue -> Maybe (S.Set ResolvedJumpTarget)
ctry_jump_targets fctxt v@(SConcrete es) = 
  let tries = mapMaybeNES try es in
    if S.null tries then
      Nothing -- trace ("Cannot resolve indirection: " ++ show v) Nothing      
    else
      Just tries
 where
  try e =
    case try_promote_expr fctxt False e of
      Just ptr -> try_pointer ptr
      Nothing  -> Nothing


  try_pointer (Base_FunctionPtr a f (PtrOffset 0))  = Just $ External f
  try_pointer (Base_Immediate imm (PtrOffset 0))    = try_immediate_address imm
  try_pointer b@(Base_StatePart (SP_Mem (SE_Immediate i) _) _) = Nothing -- TODO do not report as unresolved if known symbol
  try_pointer _ = Nothing


  try_immediate_address a
    | address_has_instruction (f_ctxt fctxt) a = Just $ ImmediateAddress a 
    | otherwise = try_symbol a

  try_symbol a =
    case IM.lookup (fromIntegral a) $ ctxt_symbol_table $ f_ctxt fctxt of
      Just (Internal_Label f)  -> Just $ External f
      Just (Relocated_Label f) -> Just $ External f
      _                        -> Nothing
ctry_jump_targets fctxt _ = Nothing 



