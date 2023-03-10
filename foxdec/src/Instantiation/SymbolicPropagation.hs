{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, FlexibleInstances, StrictData #-}

{-|
Module      : SymbolicPropagation
Description : Provides an instantiation of all function necessary to do symbolic propagation
-}
module Instantiation.SymbolicPropagation where


import Base
import Config

import Data.SPointer
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
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set.NonEmpty as NES
import Data.List
import Data.Word 
import Data.Maybe
import Data.Foldable (toList)
import Data.Bits (testBit, (.|.), (.&.), xor, complement)
import Control.Applicative (liftA2)


import Debug.Trace

debug_abstraction = False

trace' ctxt msg
  | ctxt_verbose (f_ctxt ctxt) = trace msg
  | otherwise                  = id


cspointer_to_exprs (Concrete es)   = NES.toSet es
cspointer_to_exprs (Bases bs)      = S.singleton $ Bottom $ FromPointerBases bs
cspointer_to_exprs (Sources srcs)  = S.singleton $ Bottom $ FromSources srcs
cspointer_to_exprs Top             = S.empty

ctry_immediate (Concrete es)
  | NES.size es == 1 = try_imm $ NES.findMin es
  | otherwise        = Nothing
 where
  try_imm (SE_Immediate imm) = Just imm
  try_imm _                  = Nothing
ctry_immediate _ = Nothing


ctry_deterministic (Concrete es)
  | NES.size es == 1 = Just $ NES.findMin es
  | otherwise        = Nothing
ctry_deterministic _ = Nothing

cimmediate :: Integral i => i -> SPointer
cimmediate = Concrete . NES.singleton . SE_Immediate . fromIntegral



cwiden :: FContext -> String -> SPointer -> SPointer
cwiden fctxt msg v = do_trace $ cwiden' fctxt msg v
 where
  do_trace v'
    | not debug_abstraction = v'
    | v' `moreAbstract` v   = trace' fctxt ("cwiden (" ++ msg ++ ") " ++ show v ++ " --> " ++ show v') v'
    | otherwise             = v'

  moreAbstract v' v =
    case ctry_immediate v of
      Just imm -> v' /= v && address_has_instruction (f_ctxt fctxt) imm
      Nothing  -> False

{--
  moreAbstract Top             v1              = v1 /= Top
  moreAbstract _               Top             = False
  moreAbstract (Sources srcs0) (Sources srcs1) = NES.size srcs0 > NES.size srcs1
  moreAbstract (Sources srcs0) _               = True
  moreAbstract (Bases   bs0)   (Sources srcs1) = False
  moreAbstract (Bases   bs0)   (Bases bs1)     = NES.size bs0 > NES.size bs1
  moreAbstract (Bases   bs0)   _               = True
  moreAbstract (Concrete es0)  _               = False
--}

cwiden' fctxt msg (Concrete es) =
  let domains = NES.map (get_pointer_domain fctxt) es in
    if Nothing `NES.member` domains then
      Top
    else if all is_bases domains then
      use_bases domains
    else
      use_sources domains
 where
  use_bases domains =
    let bs = NES.unions $ NES.map get_bases domains in
      cwiden' fctxt msg $ Bases bs
  use_sources domains =
    -- error $ "Widening to sources: (" ++ msg ++ "): " ++  show (Concrete es)
    let srcs = NES.unions $ NES.map get_sources domains in
      cwiden' fctxt msg $ mk_sources fctxt srcs

  is_bases (Just (Domain_Bases bs))  = True
  is_bases _                         = False
  get_bases (Just (Domain_Bases bs)) = bs

  get_sources (Just (Domain_Sources srcs)) = srcs
  get_sources (Just (Domain_Bases bs))     = NES.unions $ NES.map (srcs_of_base fctxt) bs 
cwiden' fctxt msg v@(Bases bs)
  | NES.size bs <= (ctxt_max_num_of_bases $ f_ctxt fctxt) = v
  | otherwise = cwiden' fctxt msg $ mk_sources fctxt $ NES.unions $ NES.map (srcs_of_base fctxt) bs
                -- error $ "Widening to sources (" ++ msg ++ "): " ++ show (Bases bs)
cwiden' fctxt msg v@(Sources srcs) = mk_sources fctxt srcs
cwiden' fctxt msg Top = Top

cwiden_all_to_sources :: FContext -> String -> S.Set SPointer -> SPointer
cwiden_all_to_sources fctxt msg vs -- = error $ "Widening to sources (" ++ msg ++ "): " ++  show vs
  | Top `elem` vs || S.null vs = Top
  | otherwise                  = cwiden fctxt msg $ mk_sources fctxt $ NES.unions $ NES.map get_sources $ NES.unsafeFromSet vs
 where
  get_sources (Concrete es)  = NES.unions $ NES.map (srcs_of_expr fctxt) es
  get_sources (Bases bs)     = NES.unions $ NES.map (srcs_of_base fctxt) bs
  get_sources (Sources srcs) = srcs


cjoin :: FContext -> String -> SPointer -> SPointer -> SPointer
cjoin fctxt msg (Concrete es0) (Concrete es1) = 
  let es' = NES.union es0 es1 in
    if NES.size es' <= get_max_num_of_cases then
      Concrete es'
    else
      cwiden fctxt ("max_num_of_cases reached: " ++ msg) $ Concrete es'
 where
  get_max_num_of_cases = ctxt_max_num_of_cases $ f_ctxt fctxt
cjoin fctxt msg v0 v1 = merge (cwiden fctxt msg v0) (cwiden fctxt msg v1)
 where
  merge (Bases bs0) (Bases bs1)         = cwiden fctxt msg $ Bases      $ NES.union bs0 bs1
  merge (Bases bs0) (Sources srcs1)     = cwiden fctxt msg $ mk_sources fctxt $ NES.union (NES.unions $ NES.map (srcs_of_base fctxt) bs0) srcs1
  merge (Sources srcs0) (Bases bs1)     = cwiden fctxt msg $ mk_sources fctxt $ NES.union srcs0 (NES.unions $ NES.map (srcs_of_base fctxt) bs1)
  merge (Sources srcs0) (Sources srcs1) = cwiden fctxt msg $ mk_sources fctxt $ NES.union srcs0 srcs1
  merge Top _ = Top
  merge _ Top = Top


cjoin_all :: Foldable t => FContext -> String -> t SPointer -> SPointer
cjoin_all fctxt msg es
  | null es   = error $ "Cannot join [], msg = " ++ msg
  | otherwise = foldr1 (cjoin fctxt msg) es




spointer_plus :: FContext -> Int -> SPointer -> SPointer -> SPointer
spointer_plus fctxt si v0@(Concrete es0) v1@(Concrete es1)
  | NES.size es0 * NES.size es1 <= get_max_num_of_cases = Concrete $ NES.map plus $ NES.cartesianProduct es0 es1
  | otherwise = spointer_plus fctxt si (cwiden fctxt "plus0" v0) (cwiden fctxt "plus1" v1)
 where
  get_max_num_of_cases = ctxt_max_num_of_cases $ f_ctxt fctxt

  plus (e0,e1) = simp $ SE_Op Plus si [e0,e1]

spointer_plus fctxt si v1@(Concrete es1) v0                  = spointer_plus fctxt si v0 v1

spointer_plus fctxt si Top Top                               = Top
spointer_plus fctxt si Top v1@(Concrete es)                  = spointer_plus fctxt si Top (cwiden fctxt "plus2" v1)
spointer_plus fctxt si Top v1@(Bases bs)                     = v1
spointer_plus fctxt si Top v1@(Sources srcs)                 = Top

spointer_plus fctxt si v0@(Bases bs0) v1@(Concrete es1)      = v0
spointer_plus fctxt si v0@(Bases bs0) v1@(Bases bs1)         = cwiden fctxt "plus3" $ Bases $ NES.union bs0 bs1
spointer_plus fctxt si v0@(Bases bs0) v1@(Sources srcs1)     = v0
spointer_plus fctxt si v0@(Bases bs0) Top                    = v0

spointer_plus fctxt si v0@(Sources srcs0) v1@(Concrete es1)  = cwiden_all_to_sources fctxt "plus4" $ S.fromList [v0,v1]
spointer_plus fctxt si v0@(Sources srcs0) v1@(Bases bs1)     = v1
spointer_plus fctxt si v0@(Sources srcs0) v1@(Sources srcs1) = cwiden_all_to_sources fctxt "plus5" $ S.fromList [v0,v1]
spointer_plus fctxt si v0@(Sources srcs0) Top                = Top


spointer_minus :: FContext -> Int -> SPointer -> SPointer -> SPointer
spointer_minus fctxt si v0@(Concrete es0) v1@(Concrete es1)
  | NES.size es0 * NES.size es1 <= get_max_num_of_cases = Concrete $ NES.map minus $ NES.cartesianProduct es0 es1
  | otherwise = spointer_minus fctxt si (cwiden fctxt "minus0" v0) (cwiden fctxt "minus1" v1)
 where
  get_max_num_of_cases = ctxt_max_num_of_cases $ f_ctxt fctxt

  minus (e0,e1) = simp $ SE_Op Minus si [e0,e1]
spointer_minus fctxt si v0@(Concrete es0) v1                  = spointer_minus fctxt si v1 v0
spointer_minus fctxt si v0@(Bases bs0) v1@(Concrete es1)      = v0
spointer_minus fctxt si v0@(Bases bs0) v1@(Bases bs1)         = v0
spointer_minus fctxt si v0@(Bases bs0) v1@(Sources srcs1)     = v0
spointer_minus fctxt si v0@(Bases bs0) v1@Top                 = v0
spointer_minus fctxt si v0@(Sources srcs0) v1@(Concrete es1)  = cwiden_all_to_sources fctxt "minus1" $ S.fromList [v0,v1]
spointer_minus fctxt si v0@(Sources srcs0) v1@(Bases bs1)     = cwiden_all_to_sources fctxt "minus2" $ S.fromList [v0,v1]
spointer_minus fctxt si v0@(Sources srcs0) v1@(Sources srcs1) = cwiden_all_to_sources fctxt "minus3" $ S.fromList [v0,v1]
spointer_minus fctxt si v0@(Sources srcs0) v1@Top             = Top

spointer_minus fctxt si Top _                                 = Top


apply_expr_op :: FContext -> ([SimpleExpr] -> Maybe SimpleExpr) -> [SPointer] -> SPointer
apply_expr_op fctxt f vs
  | all isConcrete vs =
      let ess = map get_exprs vs in
        if product (map NES.size ess) <= get_max_num_of_cases then
          let es' = map f $ crossProduct $ map neSetToList ess in
            if Nothing `elem` es' then
              cwiden_all_to_sources fctxt "apply0" $ S.fromList vs 
            else
              Concrete $ neFromList $ map fromJust es'
        else
          cwiden_all_to_sources fctxt "apply1" $ S.fromList vs
  | otherwise = cwiden_all_to_sources fctxt "apply2" $ S.fromList vs 
 where
  get_max_num_of_cases = ctxt_max_num_of_cases $ f_ctxt fctxt

  get_exprs (Concrete es) = es


data CSemantics = ApplyPlus Int | ApplyMinus Int | ApplyNeg Int | ApplyDec Int | ApplyInc Int |
                  Apply ([SimpleExpr] -> SimpleExpr) | SetXX | SExtension_HI | NoSemantics


mk_expr :: FContext -> SimpleExpr -> Maybe SimpleExpr
mk_expr fctxt = trim_expr fctxt . simp
 where
  trim_expr fctxt e
    | expr_size e > get_max_expr_size = Nothing
    | otherwise = Just e
  get_max_expr_size = ctxt_max_expr_size $ f_ctxt fctxt



csemantics :: FContext -> SymbolicOperation SPointer -> SPointer
csemantics fctxt (SO_Plus  a b)         = spointer_plus fctxt 64 a b
csemantics fctxt (SO_Minus a b)         = spointer_minus fctxt 64 a b
csemantics fctxt (SO_Times a b)         = apply_expr_op fctxt (mk_expr fctxt . SE_Op Times 64) [a,b]
csemantics fctxt (SO_Overwrite n a b)   = apply_expr_op fctxt (\[e0,e1] -> mk_expr fctxt $ SE_Overwrite n e0 e1) [a,b]
csemantics fctxt (SO_SExtend l h a)     = apply_expr_op fctxt (\[e] -> mk_expr fctxt $ SE_SExtend l h e) [a]
csemantics fctxt (SO_Bit h a)           = apply_expr_op fctxt (\[e] -> mk_expr fctxt $ SE_Bit h e) [a]
csemantics fctxt (SO_Op op si si' es)   = 
  case mnemonic_to_semantics op (8*si) (((*) 8) <$> si') of
    ApplyPlus  si -> spointer_plus  fctxt si (es!!0) (es!!1)
    ApplyInc   si -> spointer_plus  fctxt si (es!!0) (cimmediate 1)
    ApplyMinus si -> spointer_minus fctxt si (es!!0) (es!!1)
    ApplyDec   si -> spointer_minus fctxt si (es!!0) (cimmediate 1)
    ApplyNeg   si -> spointer_minus fctxt si (cimmediate 0) (es!!0)
    Apply sop     -> apply_expr_op fctxt (mk_expr fctxt . sop) es
    SetXX         -> Concrete $ neFromList [SE_Immediate 0,SE_Immediate 1]
    SExtension_HI -> Concrete $ neFromList [SE_Immediate 0,SE_Immediate 18446744073709551615]
    NoSemantics   -> cwiden_all_to_sources fctxt ("csemantics " ++ show op) $ S.fromList es
                     -- trace' ctxt ("Widening due to operand: " ++ show op ++ " to " ++ show es) $ CTop -- TODO cwiden es
  






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

mnemonic_to_semantics AND si si'     = Apply $ SE_Op And si
mnemonic_to_semantics OR  si si'     = Apply $ SE_Op Or si
mnemonic_to_semantics NOT si si'     = Apply $ (\[a] -> SE_Op Not si [a])

mnemonic_to_semantics XOR    si si'  = Apply $ SE_Op Xor si
mnemonic_to_semantics PXOR   si si'  = Apply $ SE_Op Xor si
mnemonic_to_semantics VPXOR  si si'  = Apply $ SE_Op Xor si
mnemonic_to_semantics XORPS  si si'  = Apply $ SE_Op Xor si
mnemonic_to_semantics XORPD  si si'  = Apply $ SE_Op Xor si

mnemonic_to_semantics MOV     si si'  = Apply $ head
mnemonic_to_semantics MOVSD   si si'  = Apply $ head
mnemonic_to_semantics MOVSS   si si'  = Apply $ head
mnemonic_to_semantics MOVAPS  si si'  = Apply $ head
mnemonic_to_semantics MOVAPD  si si'  = Apply $ head
mnemonic_to_semantics MOVUPS  si si'  = Apply $ head
mnemonic_to_semantics MOVUPD  si si'  = Apply $ head
mnemonic_to_semantics MOVABS  si si'  = Apply $ head
mnemonic_to_semantics MOVDQU  si si'  = Apply $ head
mnemonic_to_semantics MOVDQA  si si'  = Apply $ head
mnemonic_to_semantics MOVLPD  si si'  = Apply $ head
mnemonic_to_semantics MOVD    si si'  = Apply $ head
mnemonic_to_semantics MOVQ    si si'  = Apply $ head -- TODO if prefix = Nothing?
mnemonic_to_semantics VMOVD   si si'  = Apply $ head 
mnemonic_to_semantics VMOVAPD si si'  = Apply $ head
mnemonic_to_semantics VMOVAPS si si'  = Apply $ head
mnemonic_to_semantics MOVZX   si si'  = Apply $ head

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



--TODO CMOV, TEST
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
  --TODO





mk_cvalue fctxt (Bottom (FromPointerBases bs)) = Bases bs
mk_cvalue fctxt (Bottom (FromSources srcs))    = mk_sources fctxt srcs
mk_cvalue fctxt e                              = Concrete $ NES.singleton $ simp e


mk_sources fctxt srcs =
  let srcs' = flatten_srcs srcs in
   if NES.size srcs' <= ctxt_max_num_of_sources (f_ctxt fctxt) then
     Sources srcs'
   else
     trace' fctxt "Max num of sources exceeded." Top
 where
  flatten_srcs srcs = 
    let (ms,srcs') = S.partition is_src_mem $ NES.toSet srcs in
      if S.null ms then 
        NES.unsafeFromSet srcs'
      else
        let src' = Src_Mem $ NES.unions $ NES.map flatten $ NES.unsafeFromSet ms in
          NES.insertSet src' srcs'


  flatten (Src_Mem srcs) = NES.unions $ NES.map flatten srcs
  flatten src            = NES.singleton src


mk_cmem_value fctxt msg a si = try_symbol `orElse` (try_det `orElse` use_sources)
 where
  try_symbol = do
    a_v  <- ctry_immediate a
    si_v <- ctry_immediate si
    if address_has_external_symbol (f_ctxt fctxt) a_v then
      return $ mk_cvalue fctxt $ SE_Var (SP_Mem (SE_Immediate a_v) 8)
    else
      Nothing

  try_det = do
    a_v  <- ctry_deterministic a
    si_v <- ctry_immediate si
    return $ mk_cvalue fctxt $ SE_Var (SP_Mem a_v $ fromIntegral si_v)
 
  use_sources = 
    case srcs_of_spointer fctxt a of
      Nothing   -> Top
      Just srcs -> mk_sources fctxt $ NES.singleton $ Src_Mem srcs

cmk_mem_addresses fctxt (Concrete es) = S.map (Concrete . NES.singleton) $ NES.toSet es
cmk_mem_addresses fctxt a             = S.singleton a

srcs_of_spointer fctxt (Concrete es)  = Just $ NES.unions $ NES.map (srcs_of_expr fctxt) es
srcs_of_spointer fctxt (Bases bs   )  = Just $ NES.unions $ NES.map (srcs_of_base fctxt) bs
srcs_of_spointer fctxt (Sources srcs) = Just srcs
srcs_of_spointer fctxt Top            = Nothing





cseparate fctxt msg a0 si0 a1 si1 = 
  let si0' = SE_Immediate <$> ctry_immediate si0
      si1' = SE_Immediate <$> ctry_immediate si1 
      a0s  = S.map simp $ cspointer_to_exprs a0 -- TODO simp needed?
      a1s  = S.map simp $ cspointer_to_exprs a1 in
    not (S.null a0s) && not (S.null a1s) && all (\a0 -> all (\a1 -> necessarily_separate fctxt msg a0 si0' a1 si1') a1s) a0s

calias fctxt a0 si0 a1 si1 = 
  case (ctry_immediate si0, ctry_immediate si1) of
    (Just si0', Just si1') -> si0' == si1' && equal a0 a1
    _ -> False
 where
  equal (Concrete a0s) (Concrete a1s) = all (\a0 -> all (\a1 -> necessarily_equal a0 a1) a1s) a0s
  equal _ _ = False

cenclosed fctxt a0 si0 a1 si1 = 
  case (ctry_immediate si0, ctry_immediate si1) of
    (Just si0',Just si1') -> 
      let a0s  = S.map simp $ cspointer_to_exprs a0
          a1s  = S.map simp $ cspointer_to_exprs a1 in
        not (S.null a0s) && not (S.null a1s) && all (\a0 -> all (\a1 -> necessarily_enclosed a0 si0' a1 si1') a1s) a0s
    _ -> False




csensitive fctxt a si v =
  case (ctry_deterministic a,ctry_immediate si, ctry_deterministic v) of
    (Just a',Just si',Just v') -> is_top_stackframe a' si' v' || is_pushed_reg a' si' v'
    _                          -> False
 where
  is_initial_reg (SE_Var (SP_Reg _)) = True
  is_initial_reg _                   = False
  
  --is_top_stackframe (SE_Var (SP_StackPointer _)) _ _ = True
  is_top_stackframe a' si' v' = si' == 8 && a' == (SE_Var $ SP_StackPointer (function_name_of_entry (f_ctxt fctxt) (f_entry fctxt)))
  is_pushed_reg a' si' v' = is_initial_reg v' && expr_is_highly_likely_local_pointer fctxt a'


cread_from_ro_data fctxt a si = 
  case (ctry_immediate a,ctry_immediate si) of
    (Just a',Just si') -> cimmediate <$> read_from_ro_datasection (f_ctxt fctxt) a' (fromIntegral si')
    _                  -> Nothing


cread_from_data fctxt a si = 
  case (ctry_immediate a,ctry_immediate si) of
    (Just a',Just si') -> cimmediate <$> read_from_datasection (f_ctxt fctxt) a' (fromIntegral si')
    _                  -> Nothing


instance SymbolicExecutable FContext SPointer where
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
  simmediate               = \_ -> cimmediate
  top                      = \_ -> Top
  mk_svalue                = mk_cvalue
  mk_smem_value            = mk_cmem_value
  sjump                    = jump
  scall                    = call
  stry_jump_targets        = ctry_jump_targets
  stry_immediate           = \_ -> ctry_immediate 
  stry_deterministic       = \_ -> ctry_deterministic 
  spointer_to_exprs        = \_ -> cspointer_to_exprs
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
  -> S.Set SStatePart           -- ^ The currently known stateparts of the function
  -> Predicate
init_pred fctxt f curr_invs curr_posts curr_sps =
  let finit                = f_init fctxt -- M.filter (not . contains_bot) $ 

      sps                  = S.union curr_sps (S.delete (SSP_Reg RIP) $ gather_stateparts curr_invs curr_posts)
      (regs,regions)       = partitionWith reg_or_mem $ S.toList sps

      rsp0                 = mk $ SE_Var $ SP_StackPointer f
      write_stack_pointer  = execSstate (swrite_rreg fctxt RSP rsp0)
      write_return_address = execSstate (swrite_mem fctxt rsp0 (simmediate fctxt 8) (mk_mem_value rsp0 (simmediate fctxt 8)))

      sregs                = M.union finit $ M.fromList (map (\r -> (r,mk $ SE_Var (SP_Reg r))) regs)
      smem                 = S.fromList $ map mk_mem_entry regions in
    write_stack_pointer $ write_return_address $ (Sstate sregs smem None) 
 where
  mk = mk_svalue fctxt

  mk_mem_entry (a,si) =
    let si' = cimmediate $ fromIntegral si in
      ((a,si'), mk_mem_value a si')

  reg_or_mem (SSP_Reg r) = Left r
  reg_or_mem (SSP_Mem a si) = Right (a,si)

  mk_mem_value a si =
    case (ctry_deterministic a, ctry_immediate si) of
      (Just a',Just si') -> mk $ SE_Var (SP_Mem a' (fromIntegral si'))
      _ -> mk_smem_value fctxt "init_pred" a si




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

  gather_regs       regs = S.fromList $ map SSP_Reg $ M.keys regs 
  gather_reg_values regs = S.empty -- TODO
  gather_regions    mem  = S.fromList $ catMaybes $ map mk_mem_region $ S.toList mem
  gather_mem_values mem  = S.empty -- TODO

  mk_mem_region ((a,si),_) = 
    case (ctry_deterministic a, ctry_immediate si) of
      (Just _, Just si') -> Just $ SSP_Mem a $ fromIntegral si'
      _                  -> Nothing




-- | Convert the current invariant into a function initialisation
invariant_to_finit :: FContext -> Predicate -> FInit
invariant_to_finit ctxt (Sstate regs smem _) = M.fromList $ mapMaybe mk_finit_entry $ filter is_suitable_for_finit $ M.assocs regs
 where
  is_suitable_for_finit (r,_)    = r `notElem` [RIP,RSP,RBP]
  -- is_suitable_for_finit (SP_Mem a si,_) = is_immediate a -- TODO

  mk_finit_entry (r,v) =
    if is_immediate_pointer $ ctry_immediate v then
      Just (r,v)
    else
      case ctry_deterministic v of
        Just _  -> Nothing -- TODO fw transposition Just (r,v)
        Nothing ->
          case cwiden ctxt "mk_finit" v of
            v@(Bases _) -> Just (r,v)
            _           -> Nothing

  is_immediate_pointer (Just a) = find_section_for_address (f_ctxt ctxt) (fromIntegral a) /= Nothing
  is_immediate_pointer _        = False -- What if non-determinism?


-- | The join between two function initialisations
join_finit :: FContext -> FInit -> FInit -> FInit
join_finit ctxt f0 f1 = M.filter keep $ M.intersectionWith (\a b -> sjoin ctxt "finit: " [a,b]) f0 f1
 where
  keep v = ctry_deterministic v /= Nothing -- TODO























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
external_function_behavior _ "_strlen" = pure_and_unknown
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

-- A list of functions that return a pointer given to them by a parameter
external_function_behavior _ "_realloc"             = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_malloc_zone_realloc" = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_recallocarray"       = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "realloc"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_strcpy"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
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
external_function_behavior _ "_strchr"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_memchr"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior _ "_strstr"              = ExternalFunctionBehavior [] $ Input $ param 0


external_function_behavior fctxt f
 | is_exiting_function_call f = pure_and_unknown
 | otherwise = trace' fctxt ("Unknown external function: " ++ f) $ ExternalFunctionBehavior [] UnknownReturnValue



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

transpose_bw_exprs :: FContext -> String -> String -> Sstate SPointer -> NES.NESet SimpleExpr -> SPointer
transpose_bw_exprs fctxt f_caller f_callee p = cjoin_all fctxt "transpose_bw" . NES.map (transpose_bw_e fctxt f_caller f_callee p)

transpose_bw_spointer :: FContext -> String -> String -> Sstate SPointer -> SPointer -> SPointer
transpose_bw_spointer fctxt f_caller f_callee p (Concrete es)  = transpose_bw_exprs fctxt f_caller f_callee p es
transpose_bw_spointer fctxt f_caller f_callee p (Bases bs)     = Bases $ NES.map (transpose_bw_base fctxt f_caller f_callee p) bs
transpose_bw_spointer fctxt f_caller f_callee p (Sources srcs) = (mk_sources fctxt <$> (mk_NE_sources $ NES.map (transpose_bw_src fctxt f_caller f_callee p) srcs)) `orElse` Top
transpose_bw_spointer fctxt f_caller f_callee p Top            = Top



    
transpose_bw_reg :: FContext -> String -> String -> Sstate SPointer -> (Register, SPointer) -> Maybe (Register, SPointer)
transpose_bw_reg fctxt f_caller f_callee p (r,v) =
  let v' = transpose_bw_spointer fctxt f_caller f_callee p v in
    if v' == Top then
      Nothing -- TODO add vcs
    else
      Just $ (r,v') -- traceShow ("transposition" ++ show (f_caller,f_callee,r,v,v')) (r,v')

transpose_bw_mem :: FContext -> String -> String -> Sstate SPointer -> ((SPointer,SPointer), SPointer) -> Maybe ((SPointer,SPointer), SPointer)
transpose_bw_mem fctxt f_caller f_callee p ((a,si),v) =
  let a'  = transpose_bw_spointer fctxt f_caller f_callee p a
      si' = transpose_bw_spointer fctxt f_caller f_callee p si in
    if a' == Top then
      Nothing  -- TODO add vcs
    else
      -- traceShow ("transposition" ++ show (f_caller,f_callee,(a,si),(a',si'))) $ 
      Just ((a',si'), transpose_bw_spointer fctxt f_caller f_callee p v)




transpose_bw_e :: FContext -> String -> String -> Sstate SPointer -> SimpleExpr -> SPointer
transpose_bw_e fctxt f_caller f_callee p (Bottom (FromPointerBases bs))   = mk_cvalue fctxt $ Bottom (FromPointerBases $ NES.map (transpose_bw_base fctxt f_caller f_callee p) bs)
transpose_bw_e fctxt f_caller f_callee p (Bottom typ)                     = (mk_cvalue fctxt . Bottom <$> transpose_bw_bottyp fctxt f_caller f_callee p typ) `orElse` Top
transpose_bw_e fctxt f_caller f_callee p (SE_Malloc id hash)              = mk_cvalue fctxt $ SE_Malloc id hash
transpose_bw_e fctxt f_caller f_callee p (SE_Immediate i)                 = cimmediate i
transpose_bw_e fctxt f_caller f_callee p (SE_StatePart sp)                = Top
transpose_bw_e fctxt f_caller f_callee p (SE_Var (SP_StackPointer f))     = if f == f_callee then evalSstate (read_sp fctxt (SSP_Reg RSP)) p else mk_cvalue fctxt $ SE_Var (SP_StackPointer f)
transpose_bw_e fctxt f_caller f_callee p (SE_Var sp)                      = evalSstate (read_sp fctxt $ transpose_bw_sp fctxt f_caller f_callee p sp) p
transpose_bw_e fctxt f_caller f_callee p (SE_Bit i e)                     = csemantics fctxt $ SO_Bit i $ transpose_bw_e fctxt f_caller f_callee p e
transpose_bw_e fctxt f_caller f_callee p (SE_SExtend l h e)               = csemantics fctxt $ SO_SExtend l h $ transpose_bw_e fctxt f_caller f_callee p e
transpose_bw_e fctxt f_caller f_callee p (SE_Op Plus si [a,b])            = csemantics fctxt $ SO_Op ADD (si `div` 8) Nothing [transpose_bw_e fctxt f_caller f_callee p a,transpose_bw_e fctxt f_caller f_callee p b]
transpose_bw_e fctxt f_caller f_callee p (SE_Op Minus si [a,b])           = csemantics fctxt $ SO_Op SUB (si `div` 8) Nothing [transpose_bw_e fctxt f_caller f_callee p a,transpose_bw_e fctxt f_caller f_callee p b]
transpose_bw_e fctxt f_caller f_callee p (SE_Op op si es)                 = apply_expr_op fctxt (mk_expr fctxt . SE_Op op si) $ map (transpose_bw_e fctxt f_caller f_callee p) es
transpose_bw_e fctxt f_caller f_callee p (SE_Overwrite i a b)             = csemantics fctxt $ SO_Overwrite i (transpose_bw_e fctxt f_caller f_callee p a) (transpose_bw_e fctxt f_caller f_callee p b)

transpose_bw_sp fctxt f_caller f_callee p (SP_Reg r) = SSP_Reg r
transpose_bw_sp fctxt f_caller f_callee p (SP_Mem a si) = SSP_Mem (transpose_bw_e fctxt f_caller f_callee p a) si

transpose_bw_bottyp fctxt f_caller f_callee p (FromSources srcs)             = FromSources <$> (mk_NE_sources $ NES.map (transpose_bw_src fctxt f_caller f_callee p) srcs)
transpose_bw_bottyp fctxt f_caller f_callee p (FromOverlap srcs)             = FromSources <$> (mk_NE_sources $ NES.map (transpose_bw_src fctxt f_caller f_callee p) srcs)
transpose_bw_bottyp fctxt f_caller f_callee p (FromMemWrite srcs)            = FromSources <$> (mk_NE_sources $ NES.map (transpose_bw_src fctxt f_caller f_callee p) srcs)
transpose_bw_bottyp fctxt f_caller f_callee p (FromSemantics srcs)           = FromSources <$> (mk_NE_sources $ NES.map (transpose_bw_src fctxt f_caller f_callee p) srcs)
transpose_bw_bottyp fctxt f_caller f_callee p (FromBitMode srcs)             = FromSources <$> (mk_NE_sources $ NES.map (transpose_bw_src fctxt f_caller f_callee p) srcs)
transpose_bw_bottyp fctxt f_caller f_callee p (FromUninitializedMemory srcs) = FromSources <$> (mk_NE_sources $ NES.map (transpose_bw_src fctxt f_caller f_callee p) srcs)
transpose_bw_bottyp fctxt f_caller f_callee p (FromCall f)                   = Just $ FromCall f

transpose_bw_src fctxt f_caller f_callee p src@(Src_Var sp)             = srcs_of_spointer fctxt $ transpose_bw_e fctxt f_caller f_callee p (SE_Var sp)
transpose_bw_src fctxt f_caller f_callee p src@(Src_Mem srcs)           = 
  case mk_NE_sources $ NES.map (transpose_bw_src fctxt f_caller f_callee p) srcs of
    Nothing -> Nothing
    Just a  -> srcs_of_spointer fctxt $ evalSstate (sread_mem fctxt "transpose_bw" (Sources a) Top) p
transpose_bw_src fctxt f_caller f_callee p src@(Src_StackPointer f)     = Just $ if f == f_callee then NES.singleton $ Src_StackPointer f_caller else NES.singleton src
transpose_bw_src fctxt f_caller f_callee p src@(Src_Malloc id h)        = Just $ NES.singleton src
transpose_bw_src fctxt f_caller f_callee p src@(Src_Function f)         = Just $ NES.singleton src
transpose_bw_src fctxt f_caller f_callee p src@(Src_ImmediateAddress a) = Just $ NES.singleton src
transpose_bw_src fctxt f_caller f_callee p src@(Src_ImmediateConstants) = Just $ NES.singleton src

transpose_bw_base fctxt f_caller f_callee p b@(StackPointer f)      = if f == f_callee then StackPointer f_caller else b
transpose_bw_base fctxt f_caller f_callee p b@(GlobalAddress _)     = b
transpose_bw_base fctxt f_caller f_callee p b@(PointerToSymbol _ _) = b
transpose_bw_base fctxt f_caller f_callee p b@(Malloc _ _)          = b

mk_NE_sources s 
  | Nothing `NES.member` s = Nothing
  | otherwise              = Just $ NES.unions $ NES.map fromJust s


read_sp :: FContext -> SStatePart -> State (Sstate SPointer, VCS) SPointer
read_sp fctxt (SSP_Reg r)    = sread_reg fctxt r
read_sp fctxt (SSP_Mem a si) = sread_mem fctxt "read_sp" a si'
 where
   si' = cimmediate $ fromIntegral si




data FunctionType = AnalyzedInternalFunction (Sstate SPointer) | ExternalFunction | AnalyzedInternalFunctionTerminates | AnalyzedInternalFunctionUnknown

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
call :: FContext -> X86.Instruction -> State (Sstate SPointer,VCS) ()
call fctxt i = do
  case get_function_type fctxt i f_callee of
    AnalyzedInternalFunctionUnknown    -> unknown_internal_function fctxt i
    AnalyzedInternalFunctionTerminates -> return ()
    AnalyzedInternalFunction q         -> internal_function q
    ExternalFunction                   -> external_function 
 where
  external_function = case external_function_behavior fctxt f_callee of
    ExternalFunctionBehavior params output -> {--mapM_ write_param params >> --} write_output output -- writing to params really roughly overapproximates

  internal_function q = do
    -- push return address
    let return_address = addressof i + fromIntegral (sizeof i)
    sexec_instr fctxt (Instruction (AddressWord64 return_address) Nothing SUB Nothing [Storage RSP, Immediate 8] Nothing)
    -- sexec_instr fctxt (Instruction (AddressWord64 0) Nothing PUSH Nothing [Immediate return_address] Nothing)

    (p,vcs) <- get
    -- obtain the postcondition of the function, and do backwards transposition
    let q_eqs_transposed_regs  = catMaybes $ map (transpose_bw_reg fctxt f_name f_callee p) $ filter ((/=) RIP . fst) $ sstate_to_reg_eqs q
    let q_eqs_transposed_mem   = catMaybes $ map (transpose_bw_mem fctxt f_name f_callee p) $ filter do_transfer $ sstate_to_mem_eqs q


    -- let q_eqs_transposed_regs' = traceShow ("transposition", p, q_eqs_transposed_regs,q_eqs_transposed_mem) q_eqs_transposed_regs

    -- write transposed postcondition to current state
    mapM_ (uncurry $ swrite_reg fctxt) $ q_eqs_transposed_regs
    mapM_ write_sp $ q_eqs_transposed_mem 


  -- in case of an external function, which is passed a parameter $r$ 
  -- do a write to region [r+bot,1] to muddle the state. The value written to that region is an abstraction of what is already there.
  write_param r = do
    a      <- sread_reg fctxt r
    let a'  = cwiden fctxt "write_param" a
    let si' = Top
    v'     <- gets ((evalSstate $ sread_mem fctxt "write_param" a Top) . fst)
    let bot = cwiden fctxt "write_param_v" v'
    swrite_mem fctxt a' si' bot

  write_output FreshPointer       = swrite_reg fctxt RAX $ (mk_cvalue fctxt $ SE_Malloc (Just (addressof i)) (Just ""))
  write_output UnknownReturnValue = swrite_reg fctxt RAX $ (mk_sources fctxt $ NES.singleton $ Src_Function f_callee)  -- TODO overwrite volatile regs as well?
  write_output (Input r)          = sread_reg fctxt r >>= swrite_reg fctxt RAX


  do_transfer ((a,si),v) = not (is_initial (a,si) v) && srcs_of_spointer fctxt a `existsAndSatisfies` (not . any is_local_to_not_f)
  --do_transfer ((a,si),v) = not (any is_local_to_not_f $ srcs_of_spointer fctxt a) -- && not (is_initial (a,si) v) -- TODO!
  --
  
  is_initial :: (SPointer,SPointer) -> SPointer -> Bool
  is_initial (a,si) v =
    case (ctry_deterministic a, ctry_immediate si) of
      (Just a', Just si') -> v == mk_svalue fctxt (SE_Var (SP_Mem a' (fromIntegral si')))
      _                   -> False

  is_local_to_not_f (Src_StackPointer f') = f_name /= f' -- TODO keep when f' is from current SCC callgraph
  is_local_to_not_f _                     = False


  f_name  = function_name_of_entry (f_ctxt fctxt) (f_entry fctxt)
  f_callee = function_name_of_instruction (f_ctxt fctxt) i

  write_sp ((a,si),v) 
    | spointer_is_maybe_local_pointer a = return () -- TODO add VCS, use sensitive regions
    | otherwise = swrite_mem fctxt a si v


  sstate_to_reg_eqs :: Sstate a -> [(Register, a)]
  sstate_to_reg_eqs (Sstate regs _ _) = M.toList regs

  sstate_to_mem_eqs :: Sstate a -> [((a,a), a)]
  sstate_to_mem_eqs (Sstate _ mem _) = S.toList mem

  spointer_is_maybe_local_pointer v = any (expr_is_maybe_local_pointer fctxt) $ cspointer_to_exprs v


unknown_internal_function fctxt i = return () -- TODO try as external


jump :: FContext -> X86.Instruction -> State (Sstate SPointer,VCS) ()
jump fctxt i
  | jump_is_actually_a_call (f_ctxt fctxt) i = call fctxt i >> sreturn fctxt
  | otherwise                                = return ()


ctry_jump_targets :: FContext -> SPointer -> Maybe (S.Set ResolvedJumpTarget)
ctry_jump_targets fctxt (Concrete es) 
  | NES.size es == 1 =
    case NES.findMin es of
      SE_Immediate a                     -> if address_has_instruction (f_ctxt fctxt) a then Just $ S.singleton $ ImmediateAddress a else Nothing -- TODO or symbol?
      SE_Var (SP_Mem (SE_Immediate a) _) -> S.singleton <$> try_external a
      _                                  -> Nothing
  | all (expr_highly_likely_pointer fctxt) es = Just $ S.map mk_resolved_jump_target $ NES.toSet es
  | otherwise = Nothing
 where
  mk_resolved_jump_target (SE_Immediate a)                     = ImmediateAddress a
  mk_resolved_jump_target (SE_Var (SP_Mem (SE_Immediate a) _)) =
    case try_external a of
      Just trgt -> trgt
      Nothing   -> error $ show ("indirections", showHex a) 
  mk_resolved_jump_target a                                    = error $ show ("resolving", a, expr_highly_likely_pointer fctxt a,get_pointer_bases fctxt a)


  try_external a = IM.lookup (fromIntegral a) (ctxt_symbol_table $ f_ctxt fctxt) >>= try_mk_jump_target

  try_mk_jump_target (Relocated_Function sym) = Just $ External sym
  try_mk_jump_target _                        = Nothing


ctry_jump_targets fctxt _ = Nothing 
