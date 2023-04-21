{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, FlexibleInstances, Strict#-}

{-|
Module      : SymbolicPropagation
Description : Provides an instantiation of all function necessary to do symbolic propagation
-}
module Instantiation.SymbolicPropagation2 where


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


{--

is_local_ptrvalue (Base_StackPointer _) = True
is_local_ptrvalue _                     = False

is_local_svalue fctxt (SPointer ptrs) = any is_local_ptrvalue ptrs
is_local_svalue fctxt (SConcrete es)  = any (any isStackPointer . try_promote_expr fctxt) es
is_local_svalue fctxt _               = False





-- Concert SValues to SExpressions
ptrvalue_to_expr (Base_StackPointer f) = simp $ SE_Op Plus 64 [SE_Var $ SP_StackPointer f,Bottom $ FromCall ""]
ptrvalue_to_expr (Base_Section i)      = SE_Op Plus 64 [SE_Immediate i,Bottom $ FromCall ""]
ptrvalue_to_expr (Base_Malloc id h)    = simp $ SE_Op Plus 64 [SE_Malloc id h,Bottom $ FromCall ""]
ptrvalue_to_expr (Base_StatePart sp)   = simp $ SE_Op Plus 64 [SE_Var sp,Bottom $ FromCall ""]
ptrvalue_to_expr (Base_TLS)            = simp $ SE_Op Plus 64 [SE_Var (SP_Reg FS),Bottom $ FromCall ""]

addends_to_expr = NES.foldr mk_plus (Bottom $ FromCall "") . NES.map SE_Var
 where
  mk_plus e0 e1 = SE_Op Plus 64 [e0,e1]

csvalue_to_exprs (SPointer ptrs) = S.map ptrvalue_to_expr $ NES.toSet ptrs
csvalue_to_exprs (SConcrete es)  = NES.toSet es
csvalue_to_exprs (SAddends adds) = S.singleton $ addends_to_expr adds
csvalue_to_exprs Top             = S.empty


-- Try to get an immediate value from an SValue
ctry_immediate Top = Nothing
ctry_immediate (SConcrete es)
  | NES.size es == 1 = try_imm $ NES.findMin es
  | otherwise        = Nothing
 where
  try_imm (SE_Immediate i) = Just i
  try_imm _                = Nothing
ctry_immediate _ = Nothing

-- If the SValue represents a single concrete deterministic SExpression, retrieve that SExpression.
ctry_deterministic Top = Nothing
ctry_deterministic (SPointer ptrs) = Nothing
ctry_deterministic (SConcrete es)
  | NES.size es == 1 = Just $ NES.findMin es
  | otherwise        = Nothing
ctry_deterministic _ = Nothing

cimmediate :: Integral i => FContext -> i -> SValue
cimmediate fctxt = mk_concrete fctxt . NES.singleton . SE_Immediate . fromIntegral



-- CONSTRUCTION
-- Construct an SValue from SExpressions
mk_concrete fctxt es
  | NES.size es > ctxt_max_num_of_cases (f_ctxt fctxt) = error $ "Too many cases: " ++ show es
  | otherwise = SConcrete $ NES.map simp es 
-- Construct an SValue from SAddends
mk_saddends fctxt es
  | NES.size es > ctxt_max_num_of_cases (f_ctxt fctxt) = Top 
  | otherwise = SAddends es
-- Construct an SValue from SPointers
mk_spointer fctxt ptrs 
  | NES.size ptrs > ctxt_max_num_of_cases (f_ctxt fctxt) = error $ "Too many cases: " ++ show ptrs
  | otherwise = SPointer ptrs


-- WIDENING

-- Try to promote (and thus also widen) a concrete expression to pointer base
try_promote_expr :: FContext -> SimpleExpr -> Maybe PtrValue
try_promote_expr fctxt (SE_Op Plus _ [e0,SE_Immediate imm])  = try_promote_expr fctxt e0
try_promote_expr fctxt (SE_Op Plus _ [SE_Immediate imm,e0])  = try_promote_expr fctxt e0
try_promote_expr fctxt (SE_Op Minus _ [e0,SE_Immediate imm]) = try_promote_expr fctxt e0
try_promote_expr fctxt e                                     = promote_addends $ get_addends e
 where
  promote_addends addends =
    case filter ((/=) Nothing) $ S.toList $ S.map try_promote_addend addends of 
      [Just ptr] -> Just ptr
      []         -> Nothing
      x          -> trace ("Pointer with multiple promotable addends: " ++ show e ++ "   " ++ show x) Nothing

  try_promote_addend (SE_Var (SP_StackPointer f))                     = Just $ Base_StackPointer f
  try_promote_addend (SE_Malloc id hash)                              = Just $ Base_Malloc id hash
  try_promote_addend (SE_Var (SP_Reg FS))                             = Just $ Base_TLS
  try_promote_addend e@(SE_Var sp)                                               
    | possible_pointer_addend fctxt e                                 = Just $ Base_StatePart sp
    | otherwise                                                       = Nothing
  try_promote_addend e                                                = Nothing

  possible_pointer_addend fctxt (SE_Var sp) = mk_sstatepart fctxt sp `S.member` (S.map fst sps)
  possible_pointer_addend _  _              = False

  FInit sps _ = f_init fctxt
  ctxt = f_ctxt fctxt

immediate_maybe_a_pointer fctxt a = find_section_for_address ctxt a /= Nothing || find_section_ending_at ctxt a /= Nothing || has_symbol a
 where
  ctxt = f_ctxt fctxt
  has_symbol a =
    case IM.lookup (fromIntegral a) $ ctxt_symbol_table ctxt of
      Just (Internal_Label f)  -> True
      Just (Relocated_Label f) -> True
      _                        -> False

expr_maybe_a_pointer fctxt (SE_Immediate a) = immediate_maybe_a_pointer fctxt a
expr_maybe_a_pointer fctxt e                = try_promote_expr fctxt e /= Nothing 

svalue_maybe_a_pointer fctxt (SPointer _)   = True
svalue_maybe_a_pointer fctxt (SConcrete es) = all (expr_maybe_a_pointer fctxt) es
svalue_maybe_a_pointer fctxt _              = False

try_widen_to_section fctxt a = (\(_,_,a0,_) -> Base_Section a0) <$> (find_section_for_address ctxt a <|> find_section_ending_at ctxt a)
 where
  ctxt = f_ctxt fctxt

cwiden :: FContext -> String -> SValue -> SValue
cwiden fctxt msg Top             = Top
cwiden fctxt msg (SPointer ptrs) = widen_ptrs fctxt ("cwiden" ++ msg) ptrs
cwiden fctxt msg (SConcrete es)  = widen_exprs fctxt es
cwiden fctxt msg (SAddends adds) = widen_addends fctxt adds

widen_ptrs fctxt msg ptrs 
  | NES.size ptrs > get_max_num_of_cases = Top
  | otherwise = mk_spointer fctxt ptrs
 where
  get_max_num_of_cases = ctxt_max_num_of_cases $ f_ctxt fctxt

widen_exprs fctxt es = 
 let ptrs = NES.map try_widen_to_pointer es in
   --if all ((/=) Nothing) ptrs then
   --  cap_ptrs $ NES.map fromJust ptrs
   if all ((==) Nothing) ptrs then
     cap_addends $ mapMaybeS try_get_statepart $ S.unions $ NES.map get_addends es
   else
     cap_ptrs $ NES.unsafeFromSet $ S.map fromJust $ NES.filter ((/=) Nothing) ptrs -- TODO! This may loose pointers. error $ "TODO: " ++ show es ++ show (f_init fctxt) ++ show ptrs
 where
  try_widen_to_pointer (SE_Immediate imm)
    | immediate_maybe_a_pointer fctxt imm = Just $ fromJust $ try_widen_to_section fctxt imm
    | otherwise = Nothing
  try_widen_to_pointer e = try_promote_expr fctxt e

  cap_ptrs ptrs
    | NES.size ptrs > get_max_num_of_cases = Top
    | otherwise = mk_spointer fctxt ptrs

  cap_addends adds 
    | S.null adds || S.size adds > get_max_num_of_cases = Top
    | otherwise = mk_saddends fctxt $ NES.unsafeFromSet adds

  try_get_statepart (SE_Var sp) = Just sp -- TODO return values of functions?
  try_get_statepart _           = Nothing
  get_max_num_of_cases = ctxt_max_num_of_cases $ f_ctxt fctxt

widen_addends fctxt adds
 | NES.size adds > get_max_num_of_cases = Top
 | otherwise = mk_saddends fctxt adds
 where
  get_max_num_of_cases = ctxt_max_num_of_cases $ f_ctxt fctxt

cwiden_all :: FContext -> String -> [SValue] -> SValue
cwiden_all fctxt msg []     = Top
cwiden_all fctxt msg (v:vs) = foldr1 (cjoin fctxt msg) $ map (cwiden fctxt msg) (v:vs)



-- JOINING
-- cjoin fctxt msg v0 v1 | trace ("cjoin: "++ show (v0,v1)) False = error "trace"
cjoin fctxt msg (SPointer vs0) (SPointer vs1) =
  let vs' = NES.union vs0 vs1 in
    if NES.size vs' <= ctxt_max_num_of_cases (f_ctxt fctxt) then
      mk_spointer fctxt vs'
    else
      widen_ptrs fctxt "join" vs'
cjoin fctxt msg (SConcrete es0) (SConcrete es1) =
  case NES.foldr insert (Just es1) es0 of
    Just es' ->
      if NES.size es' <= ctxt_max_num_of_cases (f_ctxt fctxt) then
        mk_concrete fctxt es'
      else
        widen_exprs fctxt es'
    _ -> widen_exprs fctxt $ NES.union es0 es1
 where
  insert _ Nothing = Nothing 
  insert e0@(SE_Immediate a0) (Just es1)
    | immediate_maybe_a_pointer fctxt a0 =
      case find (imm_in_same_section a0) es1 of
        Nothing -> Just $ NES.insert e0 es1
        Just e1 -> Nothing
    | otherwise = Just $ NES.insert e0 es1
  insert e0 (Just es1) = Just $ NES.insert e0 es1

  imm_in_same_section a0 (SE_Immediate imm1)
    | immediate_maybe_a_pointer fctxt imm1 = try_widen_to_section fctxt a0 == try_widen_to_section fctxt imm1
    | otherwise = False
  
  imm_in_same_section a0 _ = False

{--
  let es' = NES.union es0 es1 in
    if NES.size es' <= ctxt_max_num_of_cases (f_ctxt fctxt) then
      if NES.size es' >= 3 && all isImmediateExpr es' && all (expr_maybe_a_pointer fctxt) es' then
        error $ "joining of " ++ show es'
      else
        mk_concrete fctxt es'
    else
      widen_exprs fctxt es'--}
cjoin fctxt msg (SAddends adds0) (SAddends adds1) = mk_saddends fctxt $ NES.union adds0 adds1

cjoin fctxt msg ptr0@(SConcrete es0)  ptr1@(SPointer _)     = cjoin fctxt msg ptr1 ptr0
cjoin fctxt msg ptr0@(SPointer _)     ptr1@(SConcrete es1)  =
  case widen_exprs fctxt es1 of
    SPointer ptrs1 -> cjoin fctxt msg ptr0 (SPointer ptrs1)
    _ -> Top --  trace $ "TODO: joining of " ++ show (ptr0,ptr1) ++ "\n" ++ msg

cjoin fctxt msg ptr0@(SAddends adds0) ptr1@(SConcrete es1)  = 
  case cwiden fctxt msg ptr1 of
    SAddends adds1 -> mk_saddends fctxt $ NES.union adds0 adds1
    SPointer ptrs1 -> Top -- error $ "TODO: joining of " ++ show (ptr0,ptr1) -- cjoin fctxt msg (mk_saddends fctxt adds0) $ cwiden fctxt msg v1
    _              -> Top
cjoin fctxt msg ptr0@(SConcrete es0)  ptr1@(SAddends adds1) = cjoin fctxt msg ptr1 ptr0

cjoin fctxt msg ptr0@(SPointer _)     ptr1@(SAddends adds1) = ptr0
cjoin fctxt msg ptr0@(SAddends adds0) ptr1@(SPointer _)     = ptr1

cjoin fctxt msg _ Top = Top
cjoin fctxt msg Top _ = Top

cjoin_all :: Foldable t => FContext -> String -> t SValue -> SValue
cjoin_all fctxt msg es
  | null es   = error $ "Cannot join [], msg = " ++ msg
  | otherwise = foldr1 (cjoin fctxt msg) es





svalue_plus :: FContext -> Int -> SValue -> SValue -> SValue
svalue_plus fctxt si v0@(SPointer ptrs0) v1@(SPointer ptrs1) = cwiden fctxt "svalue_plus" $ cjoin fctxt "svalue_plus1" v0 v1
svalue_plus fctxt si v0@(SPointer ptrs0) v1@(SAddends _)     = cwiden fctxt "svalue_plus2" v0
svalue_plus fctxt si v1@(SAddends _) v0@(SPointer ptrs0)     = cwiden fctxt "svalue_plus3" v0
svalue_plus fctxt si v1@(SConcrete es1) v0@(SPointer ptrs0)  = svalue_plus fctxt si v0 v1
svalue_plus fctxt si v0@(SPointer ptrs0) v1@(SConcrete es1)  = cwiden fctxt "svalue_plus4" v0
svalue_plus fctxt si v0@(SConcrete es0) v1@(SConcrete es1) =
   let es' = NES.map (uncurry plus) $ NES.cartesianProduct es0 es1 in 
     if NES.size es' <= get_max_num_of_cases then
       mk_concrete fctxt es'
     else
       widen_exprs fctxt es'
 where
  get_max_num_of_cases = ctxt_max_num_of_cases $ f_ctxt fctxt
  plus e0 e1 = simp $ SE_Op Plus si [e0,e1]
svalue_plus fctxt si v1@(SAddends adds1)  v0@(SConcrete es0) = svalue_plus fctxt si v0 v1
svalue_plus fctxt si v0@(SConcrete es0) v1@(SAddends adds1)  = 
  case cwiden fctxt "apply_plus" v0 of
    SAddends adds0 -> cwiden fctxt "apply_plus" $ mk_saddends fctxt $ NES.union adds0 adds1
    SPointer ptrs0 -> cwiden fctxt "apply_plus" $ SPointer ptrs0
    Top            -> Top
svalue_plus fctxt si v0@(SAddends adds0) v1@(SAddends adds1) = cwiden fctxt "apply_plus" $ mk_saddends fctxt $ NES.union adds0 adds1

svalue_plus fctxt si v0@(SPointer ptrs0) Top = cwiden fctxt "svalue_plus" v0
svalue_plus fctxt si Top v0@(SPointer ptrs0) = cwiden fctxt "svalue_plus" v0

svalue_plus fctxt si Top v0@(SConcrete es0) = cwiden fctxt "apply_plus" v0
svalue_plus fctxt si v0@(SConcrete es0) Top = cwiden fctxt "apply_plus" v0

svalue_plus fctxt si Top v0@(SAddends adds0) = svalue_plus fctxt si v0 Top
svalue_plus fctxt si v0@(SAddends adds0) Top = Top

svalue_plus fctxt si Top Top = Top 



svalue_minus :: FContext -> Int -> SValue -> SValue -> SValue
svalue_minus fctxt si v0@(SPointer ptrs0) v1@(SPointer ptrs1) = cwiden fctxt "svalue_minus1" v0
svalue_minus fctxt si v0@(SPointer ptrs0) v1@(SAddends _)     = cwiden fctxt "svalue_minus2" v0
svalue_minus fctxt si v0@(SAddends _)     v1@(SPointer ptrs1) = cwiden fctxt "svalue_minus3" v0
svalue_minus fctxt si v0@(SConcrete es1)  v1@(SPointer ptrs1) = Top -- error $ "TODO: " ++ show (v0,v1) --  cwiden fctxt "svalue_minus4" v0
svalue_minus fctxt si v0@(SPointer ptrs0) v1@(SConcrete es1)  = cwiden fctxt "svalue_plus4" v0{--
   let ptrs' = NES.map (uncurry minus) $ NES.cartesianProduct ptrs0 es1 in 
     if all isLeft ptrs' then
       if NES.size ptrs' <= get_max_num_of_cases then
         mk_spointer fctxt $ NES.map mkLeft ptrs'
       else
         widen_ptrs fctxt "4" $ NES.map mkLeft ptrs'
     else if all isRight ptrs' then
       if NES.size ptrs' <= get_max_num_of_cases then
         promote fctxt $ mk_concrete fctxt $ NES.map mkRight ptrs'
       else
         widen_exprs fctxt $ NES.map mkRight ptrs'
     else
       widen_ptrs fctxt "5" ptrs0
 where
  get_max_num_of_cases = ctxt_max_num_of_cases $ f_ctxt fctxt
  minus b0                           (SE_Immediate 0)  = Left b0
  minus (Base_Immediate i0)          (SE_Immediate i1) = 
    case cimmediate fctxt $ i0 - i1 of
      SPointer ptrs -> Left $ NES.findMin ptrs
      SConcrete es  -> Right $ NES.findMin es
  minus b0                           (SE_Immediate i1) = Left $ mod_offset b0 (\offset -> offset - i1)
  minus b0                           _                 = Left $ set_unknown_offset fctxt "minus2" b0--}
svalue_minus fctxt si v0@(SConcrete es0) v1@(SConcrete es1) =
   let es' = NES.map (uncurry minus) $ NES.cartesianProduct es0 es1 in 
     if NES.size es' <= get_max_num_of_cases then
       mk_concrete fctxt es'
     else
       widen_exprs fctxt es'
 where
  get_max_num_of_cases = ctxt_max_num_of_cases $ f_ctxt fctxt
  minus e0 e1 = simp $ SE_Op Minus si [e0,e1]
svalue_minus fctxt si v0@(SAddends adds0)  v1@(SConcrete es1)
  | any (expr_maybe_a_pointer fctxt) es1 = error $ "TODO: " ++ show (v0,v1)
  | otherwise = cwiden fctxt "svalue_minus4" v0
svalue_minus fctxt si v0@(SConcrete es0) v1@(SAddends adds1)  = cwiden fctxt "svalue_minus5" v0
svalue_minus fctxt si v0@(SAddends adds0) v1@(SAddends adds1) = Top -- cwiden fctxt "svalue_minus" v0
svalue_minus fctxt si Top v = Top 
svalue_minus fctxt si v Top = cwiden fctxt "svalue_minus" v  




svalue_and :: FContext -> Int -> SValue -> SValue -> SValue
svalue_and fctxt si v0@(SPointer ptrs0) v1@(SPointer ptrs1) = error $ "Cannot apply AND to two pointers: " ++ show (v0,v1)
--svalue_and fctxt si v0@(SPointer ptrs0) v1@(SAddends _)     = cwiden fctxt "svalue_and" v0
--svalue_and fctxt si v1@(SAddends _) v0@(SPointer ptrs0)     = cwiden fctxt "svalue_and" v0
svalue_and fctxt si v0@(SPointer ptrs0) v1@(SConcrete es1)  = cwiden fctxt "svalue_and" v0
svalue_and fctxt si v1@(SConcrete es1) v0@(SPointer ptrs0)  = svalue_and fctxt si v1 v0
svalue_and fctxt si v0@(SConcrete es0) v1@(SConcrete es1) =
   let es' = NES.map (uncurry and) $ NES.cartesianProduct es0 es1 in 
     if NES.size es' <= get_max_num_of_cases then
       mk_concrete fctxt es'
     else
       widen_exprs fctxt es'
 where
  get_max_num_of_cases = ctxt_max_num_of_cases $ f_ctxt fctxt
  and e0 e1 = simp $ SE_Op And si [e0,e1]
svalue_and fctxt si v0 v1 = Top





apply_expr_op :: FContext -> String -> ([SimpleExpr] -> Maybe SimpleExpr) -> [SValue] -> SValue
apply_expr_op fctxt msg f vs
  | all isConcrete vs ||  all isImmediate vs  =
      let ess = map csvalue_to_exprs vs in
        if product (map S.size ess) <= get_max_num_of_cases then
          let es' = map f $ crossProduct $ map S.toList ess in
            if Nothing `elem` es' then
              cwiden_all fctxt ("Abstraction applying function to: " ++ show vs) vs
            else
              mk_concrete fctxt $ neFromList $ map fromJust es'
        else
          cwiden_all fctxt ("Exceeding num of cases: " ++ show vs) vs
  | any isPointer vs   = Top -- trace ("Computation to pointers: " ++ msg ++ ": " ++ show vs)
  | Top `elem` vs      = Top
  | otherwise          = Top -- trace ("Making top from: " ++ show vs) 
 where
  get_max_num_of_cases = ctxt_max_num_of_cases $ f_ctxt fctxt



data CSemantics = ApplyPlus Int | ApplyMinus Int | ApplyNeg Int | ApplyDec Int | ApplyInc Int | ApplyAnd Int |
                  ApplyMov | ApplyCMov |
                  Apply ([SimpleExpr] -> SimpleExpr) | SetXX | SExtension_HI | NoSemantics


mk_expr :: FContext -> SimpleExpr -> Maybe SimpleExpr
mk_expr fctxt = trim_expr fctxt . simp
 where
  trim_expr fctxt e
    | expr_size e > get_max_expr_size = Nothing
    | otherwise = Just e
  get_max_expr_size = ctxt_max_expr_size $ f_ctxt fctxt



csemantics :: FContext -> String -> SymbolicOperation SValue -> SValue
csemantics fctxt msg (SO_Plus  a b)         = svalue_plus fctxt 64 a b
csemantics fctxt msg (SO_Minus a b)         = svalue_minus fctxt 64 a b
csemantics fctxt msg (SO_Times a b)         = apply_expr_op fctxt "times" (mk_expr fctxt . SE_Op Times 64) [a,b]
csemantics fctxt msg (SO_Overwrite n a b)   = apply_expr_op fctxt "overwrite" (\[e0,e1] -> mk_expr fctxt $ SE_Overwrite n e0 e1) [a,b]
csemantics fctxt msg (SO_SExtend l h a)     = apply_expr_op fctxt "sextend" (\[e] -> mk_expr fctxt $ SE_SExtend l h e) [a]
csemantics fctxt msg (SO_Bit h a)           = apply_expr_op fctxt (msg ++ "takebits" ++ show h) (\[e] -> mk_expr fctxt $ SE_Bit h e) [a]
csemantics fctxt msg (SO_Op op si si' es)   = 
  case mnemonic_to_semantics op (8*si) (((*) 8) <$> si') of
    ApplyMov       -> es!!0
    ApplyCMov      -> cjoin        fctxt "cmov" (es!!0) (es!!1)
    ApplyPlus  si  -> svalue_plus  fctxt si (es!!0) (es!!1)
    ApplyInc   si  -> svalue_plus  fctxt si (es!!0) (cimmediate fctxt 1)
    ApplyMinus si  -> svalue_minus fctxt si (es!!0) (es!!1)
    ApplyDec   si  -> svalue_minus fctxt si (es!!0) (cimmediate fctxt 1)
    ApplyNeg   si  -> svalue_minus fctxt si (cimmediate fctxt 0) (es!!0)
    ApplyAnd   si  -> svalue_and   fctxt si (es!!0) (es!!1)
    Apply sop      -> apply_expr_op fctxt (msg ++ ", op = " ++ show op) (mk_expr fctxt . sop) es
    SetXX          -> mk_concrete fctxt $ neFromList [SE_Immediate 0,SE_Immediate 1]
    SExtension_HI  -> mk_concrete fctxt $ neFromList [SE_Immediate 0,SE_Immediate 18446744073709551615]
    NoSemantics    -> Top 
                     -- trace ("Widening due to operand: " ++ show op) 
  






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

mnemonic_to_semantics MOVSX  si (Just si') = Apply $ SE_SExtend si' si . head
mnemonic_to_semantics MOVSXD si (Just si') = Apply $ SE_SExtend si' si . head
mnemonic_to_semantics CDQE   si (Just si') = Apply $ SE_SExtend si' si . head
mnemonic_to_semantics CWDE   si (Just si') = Apply $ SE_SExtend si' si . head
mnemonic_to_semantics CBW    si (Just si') = Apply $ SE_SExtend si' si . head

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





mk_cvalue fctxt = mk_concrete fctxt . NES.singleton

mk_cmem_value :: FContext -> String -> SValue -> SValue -> SValue
mk_cmem_value fctxt msg a si = 
  case ctry_immediate si of
    Just si' -> mk (S.toList $ cmk_mem_addresses fctxt msg a) si' 
    Nothing  -> Top -- trace ("Reading Top from memory: " ++ show (msg,a,si)) Top
 where
  mk [SPointer ptrs] si'
    | NES.size ptrs == 1 = mk_cvalue fctxt $ SE_Var $ SP_Mem (ptrvalue_to_expr $ NES.findMin ptrs) $ fromIntegral si'
    | otherwise          = Top
  mk [SConcrete as]  si'
    | NES.size as == 1   = mk_cvalue fctxt $ SE_Var $ SP_Mem (NES.findMin as) $ fromIntegral si'
    | otherwise          = Top
  mk _               si' = Top -- trace ("Reading from uninitialized memory: " ++ show (msg,a,si)) Top 

-- TODO also return size of region
cmk_mem_addresses :: FContext -> String -> SValue -> S.Set SValue
cmk_mem_addresses fctxt msg ptr = mk ptr
 where
  mk Top             = S.singleton Top
  mk (SPointer ptrs) = S.map (mk_spointer fctxt . NES.singleton)  $ NES.toSet ptrs
  mk (SConcrete es)
    | any is_return_value es = -- trace ("Making mem address: " ++ show ptr ++ " (msg = " ++ msg ++ ")") $ 
                               S.map (mk_cvalue fctxt) $ NES.toSet es -- TODO provide as output result
    | otherwise              = S.map (mk_cvalue fctxt) $ NES.toSet es 
  mk (SAddends es)   = S.singleton Top -- error $ "Making mem address: " ++ show ptr ++ " (msg = " ++ msg ++ ")"

  is_return_value (Bottom (FromCall f)) = f /= ""
  is_return_value _ = False

mk_sstatepart fctxt (SP_Reg r)    = SSP_Reg r
mk_sstatepart fctxt (SP_Mem a si) = SSP_Mem (mk_cvalue fctxt a) si -- TODO widen a if contains_bot




data PtrBase =
    PtrBase_StackPointer String
  | PtrBase_Section Word64
  | PtrBase_Malloc (Maybe Word64) (Maybe String)
  | PtrBase_FunctionPtr Word64 String
  | PtrBase_ReturnAddr String
  | PtrBase_TLS 
  | PtrBase_StatePart StatePart 
  | PtrBase_Immediate Word64
  | PtrBaseUnknown  -- TODO ad SimpleExpr
 deriving (Eq,Ord,Show)



svalue_to_bases fctxt (SPointer ptrs) = NES.map get_base ptrs
 where
  get_base (Base_StackPointer f) = PtrBase_StackPointer f
  get_base (Base_Section i)      = PtrBase_Section i
  get_base (Base_Malloc id h)    = PtrBase_Malloc id h
  get_base (Base_StatePart sp)   = PtrBase_StatePart sp
  get_base (Base_TLS)            = PtrBase_TLS
svalue_to_bases fctxt (SConcrete es) = NES.unions $ NES.map get_base es
 where
  get_base (SE_Immediate imm)
    | immediate_maybe_a_pointer fctxt imm = NES.singleton $ PtrBase_Immediate imm
    | otherwise = NES.singleton PtrBaseUnknown
  get_base (SE_Op Plus _ [e0,SE_Immediate imm])
    | immediate_maybe_a_pointer fctxt imm = NES.singleton $ PtrBase_Immediate imm
    | otherwise = get_base e0
  get_base (SE_Op Plus _ [SE_Immediate imm,e0])
    | immediate_maybe_a_pointer fctxt imm = NES.singleton $ PtrBase_Immediate imm
    | otherwise = get_base e0
  get_base (SE_Op Minus _ [e0,SE_Immediate imm]) = get_base e0
  get_base (SE_Var (SP_Mem (SE_Var (SP_StackPointer f)) 8)) = NES.singleton $ PtrBase_ReturnAddr f
  get_base (SE_Var (SP_Mem (SE_Immediate imm) 8)) =
    case try_relocated_pointer fctxt imm of
      Nothing -> NES.singleton $ PtrBaseUnknown
      Just f  -> NES.singleton $ PtrBase_FunctionPtr imm f
  get_base e = 
    case try_promote_expr fctxt e of
      Nothing  -> NES.singleton PtrBaseUnknown
      Just ptr -> svalue_to_bases fctxt $ SPointer $ NES.singleton ptr
svalue_to_bases _ _ = NES.singleton PtrBaseUnknown


cseparate :: FContext -> String -> SValue -> SValue -> SValue -> SValue -> Bool
-- cseparate fctxt msg v0 s0 v1 si1 | trace ("cseparate: "++ show (v0,v1)) False = error "trace"
cseparate fctxt msg Top si0 a1 si1 = False
cseparate fctxt msg a0 si0 Top si1 = False
cseparate fctxt msg a0 si0 a1 si1 =
  let si0'         = ctry_immediate si0
      si1'         = ctry_immediate si1 in
    or
      [ separation_based_on_necessity a0 si0' a1 si1'
      , separation_of_svalues a0 a1 ]
 where
  separation_based_on_necessity a0 si0' a1 si1' =
    let es0 = csvalue_to_exprs a0
        es1 = csvalue_to_exprs a1 in
      and
        [ es0 /= S.empty
        , es1 /= S.empty
        , all (uncurry $ necessarily_separate si0' si1') $ S.cartesianProduct es0 es1 ]

  -- Separation of two symbolic expressions with known immediate sizes
  necessarily_separate (Just si0') (Just si1') a0 a1 = necessarily_separate_expressions a0 si0' a1 si1'
  necessarily_separate _ _ _ _ = False


  separation_of_svalues v0 v1 = v0 /= v1 && (all (uncurry $ separate_bases) $ NES.cartesianProduct (svalue_to_bases fctxt v0) (svalue_to_bases fctxt v1))


  -- Separation using pointer bases 
  separate_bases PtrBaseUnknown PtrBaseUnknown = True -- trace ("Separation of " ++ show (a0, si0, a1, si1,svalue_to_bases fctxt a0,svalue_to_bases fctxt a1) ++ "\nFINIT ==\n" ++ show (f_init fctxt) ++ "\nmsg = " ++ msg)  TODO add VC
  separate_bases v0 PtrBaseUnknown = separate_bases PtrBaseUnknown v0
  separate_bases PtrBaseUnknown _ = True -- trace ("Separation of " ++ show (a0, si0, a1, si1,svalue_to_bases fctxt a0,svalue_to_bases fctxt a1) ++ "\nFINIT ==\n" ++ show (f_init fctxt) ++ "\nmsg = " ++ msg) TODO add VC

  separate_bases (PtrBase_StackPointer f0) (PtrBase_StackPointer f1) 
    | f0 == f1  = False --if (a0,si0) == (a1,si1) then False else error $ "sep of " ++ show (a0,si0,a1,si1) 
    | otherwise = True -- TODO ADD VC
  separate_bases (PtrBase_StackPointer f0) (PtrBase_Section _)         = True
  separate_bases (PtrBase_StackPointer f0) (PtrBase_Malloc _ _)        = True
  separate_bases (PtrBase_StackPointer f0) (PtrBase_FunctionPtr _ _)   = True
  separate_bases (PtrBase_StackPointer f0) (PtrBase_TLS)               = True
  separate_bases (PtrBase_StackPointer f0) (PtrBase_StatePart _)       = True -- TODO add vc
  separate_bases (PtrBase_StackPointer f0) (PtrBase_Immediate _ )      = True

  separate_bases (PtrBase_Section a0)      (PtrBase_Section a1)         = a0 /= a1 -- TODO ADD VC
  separate_bases (PtrBase_Section a0)      (PtrBase_Malloc id1 h1)      = True
  separate_bases (PtrBase_Section a0)      (PtrBase_FunctionPtr _ _ )   = True
  separate_bases (PtrBase_Section a0)      (PtrBase_TLS)                = True
  separate_bases (PtrBase_Section a0)      (PtrBase_StatePart sp1)      =
    case find (\(sp',_) -> mk_sstatepart fctxt sp1 == sp') sps of
      Just (_,Just imm) -> error "Should not happen?" -- cseparate fctxt msg a0 si0 imm si1
      _ -> True
  separate_bases (PtrBase_Section a0)      (PtrBase_Immediate i1)       = pointers_from_different_global_section (f_ctxt fctxt) a0 i1 -- TODO ADD VC

  separate_bases (PtrBase_Malloc id0 h0)   (PtrBase_Malloc id1 h1)        = id0 /= id1
  separate_bases (PtrBase_Malloc id0 h0)   (PtrBase_FunctionPtr _ _ )     = True
  separate_bases (PtrBase_Malloc id0 h0)   (PtrBase_TLS)                  = True
  separate_bases (PtrBase_Malloc id0 h0)   (PtrBase_StatePart _  )        = True
  separate_bases (PtrBase_Malloc id0 h0)   (PtrBase_Immediate _)          = True


  separate_bases (PtrBase_FunctionPtr a0 _) (PtrBase_FunctionPtr a1 _)    = a0 /= a1
  separate_bases (PtrBase_FunctionPtr a0 _) (PtrBase_TLS)                 = True
  separate_bases (PtrBase_FunctionPtr a0 _) (PtrBase_StatePart _  )       = True
  separate_bases (PtrBase_FunctionPtr a0 _) (PtrBase_Immediate i1)        = True -- TODO???

  separate_bases (PtrBase_TLS)              (PtrBase_TLS)                 = False
  separate_bases (PtrBase_TLS)              (PtrBase_StatePart _  )       = True
  separate_bases (PtrBase_TLS)              (PtrBase_Immediate _  )       = True

  separate_bases (PtrBase_StatePart sp0)    (PtrBase_StatePart sp1)       = 
    case M.lookup (mk_sstatepart fctxt sp0,mk_sstatepart fctxt sp1) m of
      Just Separate -> True
      _             -> False
  separate_bases (PtrBase_StatePart sp0)    (PtrBase_Immediate imm1)      =
    case find (\(sp',_) -> mk_sstatepart fctxt sp0 == sp') sps of
      Just (_,Just imm) -> error "Should not happen?" -- cseparate fctxt msg a0 si0 imm si1
      _ -> True 

  separate_bases (PtrBase_Immediate i0)    (PtrBase_Immediate i1)      
    | pointers_from_different_global_section (f_ctxt fctxt) i0 i1 = True -- TODO ADD VC
    | i0 /= i1                                                    = False -- REMOVE TODO ADD VC error $ "Separation of " ++ show (a0, si0, a1, si1) ++ "\nFINIT ==\n" ++ show (f_init fctxt) ++ "\nmsg = " ++ msg
    | otherwise                                                   = False


  separate_bases b0 b1 = separate_bases b1 b0

  FInit sps m = f_init fctxt 


get_addends (SE_Op Plus _ es)      = S.unions $ S.map get_addends $ S.fromList es
get_addends (SE_Op Minus _ (e:es)) = get_addends e
get_addends e                      = S.singleton e


calias fctxt a0 si0 a1 si1 = (a0,si0) == (a1,si1) ||
  case (ctry_immediate si0, ctry_immediate si1) of
   (Just si0',Just si1') -> si0' == si1' && aliassing a0 si0' a1 si1' 
   _                     -> False
 where
  -- aliassing (SPointer vs0) si0' (SPointer vs1) si1' = all (uncurry $ aliassing_ptrvalues si0' si1') (NES.cartesianProduct vs0 vs1)
  aliassing (SConcrete e0) si0' (SConcrete e1) si1' = e0 == e1
  aliassing _ _ _ _ = False


  -- TODO experiment with this
  --aliassing_ptrvalues si0' si1' (Base_StatePart sp0 (PtrOffset off0)) (Base_StatePart sp1 (PtrOffset off1)) = off0 == off1 && or
  --  [ sp0 == sp1
  --  , M.lookup (mk_sstatepart fctxt sp0,mk_sstatepart fctxt sp1) m == Just Aliassing ]
  {--
  aliassing_ptrvalues si0' si1' ptr0 ptr1 = and
    [ ptr0 == ptr1
    , not $ has_unknown_offset ptr0
    , not $ has_unknown_offset ptr1 ]
--}
  FInit _ m = f_init fctxt

cenclosed fctxt a0 si0 a1 si1 = 
  case (ctry_immediate si0, ctry_immediate si1) of
    (Just si0',Just si1') -> 
      let a0s  = csvalue_to_exprs a0
          a1s  = csvalue_to_exprs a1 in
        not (S.null a0s) && not (S.null a1s) && all (\a0 -> all (\a1 -> necessarily_enclosed a0 si0' a1 si1') a1s) a0s
    _ -> False




csensitive fctxt a si v =
  case (ctry_deterministic a,ctry_immediate si, ctry_deterministic v) of
    (Just a',Just si',Just v') -> is_top_stackframe a' si' v' || is_pushed_reg a' si' v' 
    _                          -> False
 where
  is_initial_reg (SE_Var (SP_Reg _)) = True
  is_initial_reg _                   = False
  
  is_top_stackframe a' si' v' = si' == 8 && a' == (SE_Var $ SP_StackPointer (function_name_of_entry (f_ctxt fctxt) (f_entry fctxt)))
  is_pushed_reg a' si' v' = is_initial_reg v' && expr_is_highly_likely_local_pointer fctxt a'


cread_from_ro_data fctxt a si = 
  case (ctry_immediate a,ctry_immediate si) of
    (Just a',Just si') -> cimmediate fctxt <$> read_from_ro_datasection (f_ctxt fctxt) a' (fromIntegral si')
    _                  -> Nothing

cread_from_data fctxt a si = 
  case (ctry_immediate a,ctry_immediate si) of
    (Just a',Just si') -> cimmediate fctxt <$> read_from_datasection (f_ctxt fctxt) a' (fromIntegral si')
    _                  -> Nothing

ctry_relocation fctxt a si = 
  case (ctry_immediate a, ctry_immediate si) of
    (Just a',Just si) -> try_reloc a' <|> ((\_ -> mk_value a') <$> try_relocated_pointer fctxt a')
    _                 -> Nothing
 where
  ctxt = f_ctxt fctxt

  try_reloc a' = get_trgt <$> (find (is_reloc_for a') $ ctxt_relocs $ f_ctxt fctxt)
  is_reloc_for a' (Relocation a0 a1) = a0 == a'
  get_trgt (Relocation a0 a1) = cimmediate fctxt a1

  mk_value a' = mk_cvalue fctxt $ SE_Var $ SP_Mem (SE_Immediate a') 8


-- If *[a,8] contains a relocated value to some function f, return that function
try_relocated_pointer fctxt a =
  case IM.lookup (fromIntegral a) $ ctxt_symbol_table ctxt of
    Just (Relocated_Function f) -> Just f -- Just $ mk_spointer fctxt $ NES.singleton $ Base_FunctionPtr a f
    _ -> Nothing
 where
  ctxt = f_ctxt fctxt





instance SymbolicExecutable FContext SValue where
  sseparate                = cseparate
  senclosed                = cenclosed
  salias                   = calias
  ssensitive               = csensitive
  sread_from_ro_data       = cread_from_ro_data
  smk_mem_addresses        = cmk_mem_addresses
  sjoin                    = cjoin_all
  swiden                   = cwiden
  ssemantics               = csemantics
  sflg_semantics           = cflg_semantics
  simmediate               = cimmediate
  top                      = \_ -> Top
  mk_svalue                = mk_cvalue
  mk_smem_value            = mk_cmem_value
  sjump                    = jump
  scall                    = call
  stry_jump_targets        = ctry_jump_targets
  stry_immediate           = \_ -> ctry_immediate 
  stry_deterministic       = \_ -> ctry_deterministic 
  stry_relocation          = ctry_relocation
  svalue_to_exprs          = \_ -> csvalue_to_exprs
  saddress_has_instruction = \ctxt _ -> address_has_instruction (f_ctxt ctxt)


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

      sps                  = S.unions [curr_sps, S.map fst finit, (S.delete (SSP_Reg RIP) $ gather_stateparts curr_invs curr_posts)]
      (regs,regions)       = partitionWith reg_or_mem $ S.toList sps

      rsp0                 = SE_Var $ SP_StackPointer f
      write_stack_pointer  = execSstate (swrite_rreg fctxt RSP $ mk rsp0)
      top_stack_frame      = mk $ SE_Var (SP_Mem rsp0 8)
      write_return_address = execSstate (swrite_mem fctxt (mk rsp0) (cimmediate fctxt 8) top_stack_frame)

      sregs                = M.fromList $ map (\r -> (r,mk $ SE_Var (SP_Reg r))) regs
      smem                 = S.empty in
    write_stack_pointer $ write_return_address $ write_finit (S.toList finit) $ (Sstate sregs smem None) 
 where
  mk = mk_svalue fctxt

  reg_or_mem (SSP_Reg r) = Left r
  reg_or_mem (SSP_Mem a si) = Right (a,si)

  write_finit [] s                   = s
  write_finit ((sp,Nothing):finit) s = write_finit finit s
  write_finit ((sp,Just v):finit)  s = write_finit finit $ execSstate (write_sp fctxt sp v) s


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
  gather_regions    mem  = S.fromList $ catMaybes $ map mk_mem_region $ S.toList mem
  gather_mem_values mem  = S.empty -- TODO

  mk_mem_region ((a,si),_) = 
    case (ctry_deterministic a, ctry_immediate si) of
      (Just a', Just si') -> Just $ SSP_Mem a $ fromIntegral si'
      _                   -> Nothing




mapMaybeS :: Ord b => (a -> Maybe b) -> S.Set a -> S.Set b
mapMaybeS f = S.map fromJust . S.filter ((/=) Nothing) . S.map f

-- | Convert the current invariant into a function initialisation
invariant_to_finit :: FContext -> Predicate -> FInit
invariant_to_finit fctxt p = 
  let sps   = mapMaybeS maybe_read_sp $ get_stateparts_of_sstate p
      pairs = S.toList $ S.filter (\(x,y) -> x /= y) $ S.cartesianProduct sps sps
      finit = FInit (S.map keep_globals sps) (M.fromList $ map mk_memrel pairs) in
    finit -- trace ("Turning into finit, precondition: \n" ++ show p ++ "\n-->\n" ++ show finit) finit 
 where
  keep_globals (sp,v) 
    | is_global v = (sp,Just v)
    | otherwise   = (sp,Nothing)

  maybe_read_sp sp
    | suitable_sp sp =
      let v = evalSstate (read_sp fctxt sp) p in
        onlyWhen (svalue_maybe_a_pointer fctxt v) (sp,v)
    | otherwise = Nothing

  suitable_sp (SSP_Reg r)    = r `notElem` [RIP,RSP,RBP]
  suitable_sp (SSP_Mem a si) = is_global a


  mk_memrel ((sp0,v0),(sp1,v1))
    | cseparate fctxt "invariant_to_finit" v0 mk0 v1 mk0 = ((sp0,sp1),Separate)
    | calias fctxt v0 mk0 v1 mk0 = ((sp0,sp1),Aliassing)
    | otherwise = ((sp0,sp1),Unknown) -- TODO
  mk0 = cimmediate fctxt 1


  is_global (SPointer ptrs) = all is_global_ptr ptrs 
  is_global (SConcrete es)  = all isImmediateExpr es && all (expr_maybe_a_pointer fctxt) es

  is_global_ptr (Base_Section _)       = True
  is_global_ptr _                      = False





-- | The join between two function initialisations
join_finit :: FContext -> FInit -> FInit -> FInit
join_finit fctxt f0@(FInit sps0 m0) f1@(FInit sps1 m1)
  | f0 == f1 = f0
  --  | f0 == init_finit = f1
  --  | f1 == init_finit = f0
  | otherwise = FInit (S.intersection sps0 sps1) (join_m m0 m1)
 where
  join_sps sps0 sps1 = S.foldr insert_sp sps1 sps0

  insert_sp (sp0,v0) sps1 =
    case find (\(sp1,_) -> sp0 == sp1) sps1 of
      Nothing       -> sps1
      Just (sp1,v1) -> S.insert (sp1,join_v v0 v1) $ S.delete (sp1,v1) sps1

  join_v (Just v0@(SPointer _)) (Just v1@(SPointer _)) = Just $ cjoin fctxt "join_finit" v0 v1
  join_v v0 v1               = Nothing

  join_m = M.intersectionWith join_rel

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

transpose_bw_ptrvalue :: FContext -> Sstate SValue -> Sstate SValue -> PtrValue -> SValue
transpose_bw_ptrvalue fctxt p q (Base_StackPointer f)     = cwiden fctxt "transpose_bw_ptrvalue" $ evalSstate (read_sp fctxt (SSP_Reg RSP)) p
transpose_bw_ptrvalue fctxt p q b@(Base_Section i)        = mk_spointer fctxt $ NES.singleton b
transpose_bw_ptrvalue fctxt p q b@(Base_Malloc _ _)       = mk_spointer fctxt $ NES.singleton b
transpose_bw_ptrvalue fctxt p q b@(Base_TLS)              = error $ "Transposition of TLS"
transpose_bw_ptrvalue fctxt p q b@(Base_StatePart sp)     = cwiden fctxt "transpose_bw_ptrvalue" $ evalSstate (read_sp fctxt $ transpose_bw_sp fctxt p sp) p

transpose_bw_spointer :: FContext -> Sstate SValue -> Sstate SValue -> SValue -> SValue
transpose_bw_spointer fctxt p q (SPointer ptrs) = cjoin_all fctxt "transpose_bw" $ NES.map (transpose_bw_ptrvalue fctxt p q) ptrs
transpose_bw_spointer fctxt p q (SConcrete es)  = cjoin_all fctxt "transpose_bw" $ NES.map (transpose_bw_e fctxt p) es
transpose_bw_spointer fctxt p q (SAddends adds) = cwiden_all fctxt "transpose_bw" $ neSetToList $ NES.map (transpose_bw_e fctxt p . SE_Var) adds
transpose_bw_spointer fctxt p q Top             = Top



    
transpose_bw_reg :: FContext -> Sstate SValue -> Sstate SValue -> (Register, SValue) -> Maybe (Register, SValue)
transpose_bw_reg fctxt p q (r,v) =
  let v' = transpose_bw_spointer fctxt p q v in
    Just $ (r,v')

transpose_bw_mem :: FContext -> Sstate SValue -> Sstate SValue -> ((SValue,SValue), SValue) -> Maybe ((SValue,SValue), SValue)
transpose_bw_mem fctxt p q ((a,si),v) =
  let a'  = transpose_bw_spointer fctxt p q a
      si' = transpose_bw_spointer fctxt p q si in
    Just ((a',si'), transpose_bw_spointer fctxt p q v)




transpose_bw_e :: FContext -> Sstate SValue -> SimpleExpr -> SValue
transpose_bw_e fctxt p (Bottom (FromCall f))            = mk_cvalue fctxt $ Bottom (FromCall f)
transpose_bw_e fctxt p (SE_Malloc id hash)              = mk_cvalue fctxt $ SE_Malloc id hash
transpose_bw_e fctxt p (SE_Immediate i)                 = cimmediate fctxt i
transpose_bw_e fctxt p (SE_StatePart sp)                = Top
transpose_bw_e fctxt p (SE_Var (SP_StackPointer f))     = evalSstate (read_sp fctxt (SSP_Reg RSP)) p
transpose_bw_e fctxt p (SE_Var sp)                      = evalSstate (read_sp fctxt $ transpose_bw_sp fctxt p sp) p
transpose_bw_e fctxt p (SE_Bit i e)                     = csemantics fctxt "transpose_bw" $ SO_Bit i $ transpose_bw_e fctxt p e
transpose_bw_e fctxt p (SE_SExtend l h e)               = csemantics fctxt "transpose_bw" $ SO_SExtend l h $ transpose_bw_e fctxt p e
transpose_bw_e fctxt p (SE_Op Plus si [a,b])            = csemantics fctxt "transpose_bw" $ SO_Op ADD (si `div` 8) Nothing [transpose_bw_e fctxt p a,transpose_bw_e fctxt p b]
transpose_bw_e fctxt p (SE_Op Minus si [a,b])           = csemantics fctxt "transpose_bw" $ SO_Op SUB (si `div` 8) Nothing [transpose_bw_e fctxt p a,transpose_bw_e fctxt p b]
transpose_bw_e fctxt p (SE_Op op si es)                 = apply_expr_op fctxt "transpose_bw" (mk_expr fctxt . SE_Op op si) $ map (transpose_bw_e fctxt p) es
transpose_bw_e fctxt p (SE_Overwrite i a b)             = csemantics fctxt "transpose_bw" $ SO_Overwrite i (transpose_bw_e fctxt p a) (transpose_bw_e fctxt p b)

transpose_bw_sp fctxt p (SP_Reg r) = SSP_Reg r
transpose_bw_sp fctxt p (SP_Mem a si) = SSP_Mem (transpose_bw_e fctxt p a) si



read_sp :: FContext -> SStatePart -> State (Sstate SValue, VCS) SValue
read_sp fctxt (SSP_Reg r)    = sread_reg fctxt r
read_sp fctxt (SSP_Mem a si) = sread_mem fctxt "read_sp" a si'
 where
   si' = cimmediate fctxt $ fromIntegral si

write_sp :: FContext -> SStatePart -> SValue -> State (Sstate SValue, VCS) ()
write_sp fctxt (SSP_Reg r)    v = swrite_reg fctxt r v
write_sp fctxt (SSP_Mem a si) v = swrite_mem fctxt a (cimmediate fctxt si) v


data FunctionType = AnalyzedInternalFunction (Sstate SValue) | ExternalFunction | AnalyzedInternalFunctionTerminates | AnalyzedInternalFunctionUnknown

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
call :: FContext -> Bool -> X86.Instruction -> State (Sstate SValue,VCS) ()
call fctxt is_jump i = do
  case get_function_type fctxt i f_callee of
    AnalyzedInternalFunctionUnknown    -> unknown_internal_function fctxt i
    AnalyzedInternalFunctionTerminates -> incr_rsp
    AnalyzedInternalFunction q         -> internal_function q
    ExternalFunction                   -> external_function 
 where
  external_function = case external_function_behavior fctxt f_callee of
    ExternalFunctionBehavior params output -> {--mapM_ write_param params >> --} write_output output >> incr_rsp-- writing to params really roughly overapproximates

  write_output FreshPointer       = swrite_reg fctxt RAX $ (mk_cvalue fctxt $ SE_Malloc (Just (addressof i)) (Just ""))
  write_output UnknownReturnValue = swrite_reg fctxt RAX $ (mk_cvalue fctxt $ Bottom (FromCall f_callee))
  write_output (Input r)          = sread_reg fctxt r >>= swrite_reg fctxt RAX

  incr_rsp
    | is_jump   = sexec_instr fctxt (Instruction (AddressWord64 0) Nothing ADD Nothing [Storage RSP, Immediate 8] Nothing)
    | otherwise = return ()

  decr_rsp
    | not is_jump  = sexec_instr fctxt (Instruction (AddressWord64 0) Nothing SUB Nothing [Storage RSP, Immediate 8] Nothing)
    | otherwise    = return ()

  internal_function q = do
    -- push return address if is call
    decr_rsp

    (p,vcs) <- get
    -- obtain the postcondition of the function, and do backwards transposition
    let q_eqs_transposed_regs  = catMaybes $ map (transpose_bw_reg fctxt p q) $ filter ((/=) RIP . fst) $ sstate_to_reg_eqs q
    let q_eqs_transposed_mem   = catMaybes $ map (transpose_bw_mem fctxt p q) $ filter do_transfer $ sstate_to_mem_eqs q
    -- write transposed postcondition to current state
    mapM_ (\((a,si),v) -> swrite_mem fctxt a si v) $ q_eqs_transposed_mem 
    mapM_ (\(r,v) -> swrite_reg fctxt r v) $ q_eqs_transposed_regs


  -- in case of an external function, which is passed a parameter $r$ 
  -- do a write to region [r+bot,1] to muddle the state. The value written to that region is an abstraction of what is already there.
  write_param r = do
    a      <- sread_reg fctxt r
    let a'  = cwiden fctxt "write_param" a
    let si' = Top
    v'     <- gets ((evalSstate $ sread_mem fctxt "write_param" a Top) . fst)
    let bot = cwiden fctxt "write_param_v" v'
    swrite_mem fctxt a' si' bot


  do_transfer ((a,si),v) = not (is_initial (a,si) v) && not (is_top_stackframe a si) && not (is_local_svalue a)
  
  is_initial :: (SValue,SValue) -> SValue -> Bool
  is_initial (a,si) v =
    case (ctry_deterministic a, ctry_immediate si) of
      (Just a', Just si') -> v == mk_svalue fctxt (SE_Var (SP_Mem a' (fromIntegral si')))
      _                   -> False

  is_top_stackframe (SConcrete es) si = NES.size es == 1 && 
    case (NES.findMin es,ctry_deterministic si) of
      (SE_Var (SP_StackPointer _), Just _) -> True
      _ -> False
  is_top_stackframe _ _ = False

  is_local_svalue (SPointer ptrs) = any is_local_ptrvalue ptrs
  is_local_svalue (SConcrete es)  = any is_local_expr es
  is_local_svalue (SAddends adds) = any is_local_var $ NES.map SE_Var adds
  is_local_svalue Top             = False

  is_local_expr = any is_local_var . get_addends

  is_local_var (SE_Var (SP_StackPointer _)) = True
  is_local_var (SE_Var (SP_Mem a si))       = is_local_expr a
  is_local_var _                            = False



  f_name  = function_name_of_entry (f_ctxt fctxt) (f_entry fctxt)
  f_callee = function_name_of_instruction (f_ctxt fctxt) i

  sstate_to_reg_eqs (Sstate regs _ _) = M.toList regs
  sstate_to_mem_eqs (Sstate _ mem _) = S.toList mem



  unknown_internal_function fctxt i = incr_rsp -- TODO try as external


jump :: FContext -> X86.Instruction -> State (Sstate SValue,VCS) ()
jump fctxt i
  | jump_is_actually_a_call (f_ctxt fctxt) i = call fctxt True i >> sexec_instr fctxt (Instruction (AddressWord64 0) Nothing SUB Nothing [Storage RSP, Immediate 8] Nothing) >> sreturn fctxt
  | otherwise                                = return ()


ctry_jump_targets :: FContext -> SValue -> Maybe (S.Set ResolvedJumpTarget)
ctry_jump_targets fctxt ptr = try ptr
 where
  try (SPointer ptrs) = trace ("Cannot resolve indirection: " ++ show ptr) Nothing
  try (SConcrete es) = 
    let addresses = NES.map try_address es in
      if all ((==) Nothing) addresses then
        trace ("Cannot resolve indirection: " ++ show ptr) Nothing
      --else if any ((==) Nothing) addresses then
      --  error $ "TODO:" ++ show ptr
      else --TODO ADD VC
        Just $ S.map fromJust $ S.filter ((/=) Nothing) $ NES.toSet addresses
  try _ = Nothing



  try_address (SE_Immediate a)
    | address_has_instruction (f_ctxt fctxt) a = Just $ ImmediateAddress a 
    | otherwise = try_symbol a
  try_address (SE_Var (SP_Mem (SE_Immediate a) 8)) = External <$> try_relocated_pointer fctxt a
  try_address _ = Nothing

  try_symbol a =
    case IM.lookup (fromIntegral a) $ ctxt_symbol_table $ f_ctxt fctxt of
      Just (Internal_Label f)  -> Just $ External f
      Just (Relocated_Label f) -> Just $ External f
      _                        -> Nothing


--}
