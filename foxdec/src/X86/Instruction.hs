{-# LANGUAGE FlexibleInstances #-}

module X86.Instruction (Instruction, canonicalize, addressof) where

import           Base (orElse)
import           Data.Word (Word64)
import           Generic.Address (AddressWord64(..), GenericAddress (..))
import           Generic.Instruction (GenericInstruction(..))
import           Generic.Operand (GenericOperand(..))
import           Generic.HasSize (HasSize(sizeof))
import           X86.Opcode (Opcode(..), isCall, isJump, isCondJump, isRet, isHalt)
import           X86.Prefix (Prefix)
import           X86.Register (Register(..))

type Instruction = GenericInstruction AddressWord64 Register Prefix Opcode Int

instance HasSize Instruction where
  sizeof i = annot i `orElse` 0

addressof (Instruction (AddressWord64 a) _ _ _ _ _) = a

-- | Canonicalizes an instruction by making sure there is at most one output and inputs/outputs are syntactically clear.
-- | For example, the IMUL instruction comes in different flavors that hide the actual inputs and outputs to multiple registers
canonicalize :: Instruction -> [Instruction]
canonicalize (Instruction label prefix PUSH Nothing [op1] annot) =
  let si = sizeof op1
  in [ Instruction
         label
         prefix
         SUB
         (Just $ Storage RSP)
         [Storage RSP, Immediate $ fromIntegral si]
         annot
     , Instruction
         label
         prefix
         MOV
         (Just $ Memory (AddressStorage RSP) si)
         [op1]
         Nothing]
-- POP
canonicalize (Instruction label prefix POP Nothing [op1] annot) =
  let si = sizeof op1
  in [ Instruction
         label
         prefix
         MOV
         (Just op1)
         [Memory (AddressStorage RSP) si]
         annot
     , Instruction
         label
         prefix
         ADD
         (Just $ Storage RSP)
         [Storage RSP, Immediate $ fromIntegral si]
         Nothing]
-- LEAVE 
canonicalize (Instruction label prefix LEAVE Nothing [] annot) =
  Instruction label prefix MOV (Just $ Storage RSP) [Storage RBP] annot
  :canonicalize (Instruction label prefix POP Nothing [Storage RBP] annot)
-- XCHG
canonicalize (Instruction label prefix XCHG Nothing [dst,src] annot) =
  [
    Instruction label prefix MOV (Just $ Storage TEMP) [dst] annot,
    Instruction label prefix MOV (Just dst) [src] annot,
    Instruction label prefix MOV (Just src) [Storage TEMP] annot
  ]
-- XADD
canonicalize (Instruction label prefix XADD Nothing [dst,src] annot) =
  [
    Instruction label prefix ADD (Just $ Storage TEMP) [dst,src] annot,
    Instruction label prefix MOV (Just src) [dst] annot,
    Instruction label prefix MOV (Just dst) [Storage TEMP] annot
  ]
-- CMPXCHG
canonicalize (Instruction label prefix CMPXCHG Nothing [dst,src] annot) =
  [
    Instruction label prefix CMPXCHG (Just $ Storage RAX) [dst,src] annot,
    Instruction label prefix CMPXCHG (Just $ dst) [dst,src] annot
  ]
-- XGETBV
canonicalize (Instruction label prefix XGETBV Nothing [] annot) =
  [
    Instruction label prefix XGETBV (Just $ Storage EDX) [] annot,
    Instruction label prefix XGETBV (Just $ Storage EAX) [] annot
  ]
-- The remaining cases
canonicalize i@(Instruction label prefix mnemonic Nothing ops annot)
  | mnemonic `elem` [CBW, CWDE, CDQE] = canonicalize_sextend1 i
  | mnemonic `elem` [CWD, CDQ, CQO] = canonicalize_sextend2 i
  | mnemonic `elem` [MUL, IMUL] = canonicalize_mul i
  | mnemonic `elem` [DIV, IDIV] = canonicalize_div i
  | mnemonic_reads_from_all_operands mnemonic =
    [Instruction label prefix mnemonic (Just $ head ops) ops annot]
  | mnemonic_reads_from_all_but_first_operands mnemonic =
    [Instruction label prefix mnemonic (Just $ head ops) (tail ops) annot]
  | remove_destination mnemonic = [Instruction label prefix mnemonic Nothing [] annot]
  | do_not_modify mnemonic = [i]
  | otherwise = error $ "Cannot canonicalize instruction: " ++ show i
canonicalize _ = error "Unknown instruction"

-- CBW / CWDE / CDQE
canonicalize_sextend1 (Instruction label prefix mnemonic Nothing [] annot) =
  let srcs = case mnemonic of
        CBW  -> [AX, AL]
        CWDE -> [EAX, AX]
        CDQE -> [RAX, EAX]
        _    -> error "Invalid extend sources"
  in [ Instruction
         label
         prefix
         mnemonic
         (Just $ Storage $ head srcs)
         [Storage $ srcs !! 1]
         annot]
canonicalize_sextend1 _ = error "Invalid extend instruction"

-- CWD / CDQ / CQO
canonicalize_sextend2 (Instruction label prefix mnemonic Nothing [] annot) =
  let srcs = case mnemonic of
        CWD -> [DX, AX]
        CDQ -> [EDX, EAX]
        CQO -> [RDX, RAX]
        _   -> error "invalid extends source"
  in [ Instruction
         label
         prefix
         mnemonic
         (Just $ Storage $ head srcs)
         [Storage $ srcs !! 1]
         annot ]
canonicalize_sextend2 _ = error "Invalid extend instruction"

-- MUL /IMUL (1)
canonicalize_mul (Instruction label prefix mnemonic Nothing [op1] annot) =
  let srcs = case sizeof op1 of
        8 -> [RDX, RAX]
        4 -> [EDX, EAX]
        2 -> [DX, AX]
        1 -> [AH, AL]
        _ -> error "Invalid operand size"
  in [ Instruction
         label
         prefix
         (hipart mnemonic)
         (Just $ Storage $ head srcs)
         [Storage $ srcs !! 1, op1]
         annot
     , Instruction
         label
         prefix
         (lowpart mnemonic)
         (Just $ Storage $ srcs !! 1)
         [Storage $ srcs !! 1, op1]
         Nothing]
-- MUL /IMUL (2)
canonicalize_mul (Instruction label prefix mnemonic Nothing [op1, op2] annot) =
  [Instruction label prefix mnemonic (Just op1) [op1, op2] annot]
-- MUL /IMUL (3)
canonicalize_mul
  (Instruction label prefix mnemonic Nothing [op1, op2, op3] annot) =
  [Instruction label prefix mnemonic (Just op1) [op2, op3] annot]
canonicalize_mul _ = error "Invalid mul operation"

-- DIV /IDIV (1)
canonicalize_div (Instruction label prefix mnemonic Nothing [op1] annot) =
  let srcs = case sizeof op1 of
        8 -> [RDX, RAX]
        4 -> [EDX, EAX]
        2 -> [DX, AX]
        1 -> [AH, AL]
        _ -> error "Invalid operand size"
  in [ Instruction
         label
         prefix
         (hipart mnemonic)
         (Just $ Storage $ head srcs)
         [Storage $ head srcs, Storage $ srcs !! 1, op1]
         annot
     , Instruction
         label
         prefix
         (lowpart mnemonic)
         (Just $ Storage $ srcs !! 1)
         [Storage $ head srcs, Storage $ srcs !! 1, op1]
         Nothing]
canonicalize_div _ = error "Invalid div instruction"

-- Does the instruction read from all operands, inlcuding the first one?
mnemonic_reads_from_all_operands mnemonic = mnemonic
  `elem` [ ADD
         , SUB
         , NEG
         , INC
         , DEC
         , ADC
         , SBB
         , ROL
         , ROR
         , SHL
         , SHR
         , SAL
         , SAR
         , SHLD
         , SHRD
         , XOR
         , OR
         , AND
         , NOT
         , BT
         , BTC
         , BTR
         , BTS
         , BSWAP
         , XORPD
         , XORPS
         , SUBPS
         , ANDPD
         , ANDNPD
         , ORPD
         , SUBPD
         , ADDPD
         , HADDPD
         , MINSD
         , MAXSD
         , POR
         , PAND
         , PANDN
         , PXOR
         , VPOR
         , VPAND
         , VPANDN
         , VPXOR
         , PUNPCKLQDQ
         , PUNPCKLBW
         , PUNPCKLDQ
         , PCMPGTD
         , PADDD
         , PADDB
         , PADDQ
         , PSUBD
         , PSUBB
         , PSUBQ
         , PMULLD
         , PMINSD
         , PMAXSD
         , PMINUD
         , PMAXUD
         , PMAXUQ
         , PMAXUQ
         , PMULUDQ
         , PSRLD
         , PSRLW
         , PSRLDQ
         , PSLLDQ
         , PSLLQ
         , PSRLQ
         , PSUBUSB
         , PSUBUSW
         , PINSRB
         , PINSRQ
         , PINSRD
         , PEXTRB
         , PEXTRD
         , PEXTRQ
         , PBLENDW
         , PCLMULQDQ
         , PACKSSDW
         , PACKSSWB
         , PHADDD 
         , SUBSS
         , ADDSS
         , DIVSS
         , MULSS
         , ROUNDSS
         , SUBSD
         , ADDSD
         , DIVSD
         , MULSD
         , ROUNDSD
         , CMPEQSD, CMPLTSD, CMPNLESD, CMPNEQSD, CMPNLESD
         , UNPCKLPD
      ]

-- Does the instruction read from all operands, except for the first one?
mnemonic_reads_from_all_but_first_operands mnemonic = mnemonic
  `elem` [ LEA
         , MOV
         , MOVZX
         , MOVSX
         , MOVSXD
         , MOVAPS
         , MOVAPD
         , MOVABS
         , MOVUPD
         , MOVUPS
         , MOVDQU
         , MOVDQA
         , MOVD
         , MOVQ
         , MOVLPD
         , MOVLPS
         , MOVSD
         , MOVSS
         , MOVHPD
         , MOVHPS
         , VMOVD
         , VMOVAPD
         , VMOVAPS
         , CMOVO
         , CMOVNO
         , CMOVS
         , CMOVNS
         , CMOVE
         , CMOVZ
         , CMOVNE
         , CMOVNZ
         , CMOVB
         , CMOVNAE
         , CMOVC
         , CMOVNB
         , CMOVAE
         , CMOVNC
         , CMOVBE
         , CMOVNA
         , CMOVA
         , CMOVNBE
         , CMOVL
         , CMOVNGE
         , CMOVG
         , CMOVGE
         , CMOVNL
         , CMOVLE
         , CMOVNG
         , CMOVNLE
         , CMOVP
         , CMOVPE
         , CMOVNP
         , CMOVPO
         , SETO
         , SETNO
         , SETS
         , SETNS
         , SETE
         , SETZ
         , SETNE
         , SETNZ
         , SETB
         , SETNAE
         , SETC
         , SETNB
         , SETAE
         , SETNC
         , SETBE
         , SETNA
         , SETA
         , SETNBE
         , SETL
         , SETNGE
         , SETGE
         , SETNL
         , SETLE
         , SETNG
         , SETG
         , SETNLE
         , SETP
         , SETPE
         , SETNP
         , SETPO
         , BSR
         , BSF
         , CVTSS2SD
         , CVTSI2SS
         , CVTSI2SD
         , CVTSD2SS
         , CVTTSS2SI
         , CVTTSD2SI
         , CVTTPD2DQ
         , CVTDQ2PD
         , MOVMSKPD
         , MOVMSKPS
         , PMOVSXDQ
         , PMOVZXDQ
         , PMOVSXBD
         , PMOVZXBD
         , UNPCKLPS
         , BLENDVPD
         , BLENDVPS
         , EXTRACTPS
         , VINSERTF128
         , VEXTRACTI128
         , VEXTRACTF128
         , VPERM2F128
         , VPERM2I128
         , VPALIGNR
         , PALIGNR
         , SHUFPS
         , PSHUFB
         , PSHUFD
         , VPSHUFB
         , VPSHUFD
         , PSHUFLW
         , STOS,STOSB,STOSD,STOSQ
         , FST, FSTP, FIST, FISTP, FISTTP
         , FSTCW, FNSTCW
        ]

-- Does the instruction not make state change?
-- For example, NOP or ENDBR64
remove_destination :: Opcode -> Bool
remove_destination mnemonic =
  mnemonic
  `elem` [ NOP
         , ENDBR64
         , UD2
         , WAIT
         , MFENCE
         , CLFLUSH
         , COMISD, UCOMISD
         -- ST registers are not considered state
         , FILD, FLD, FXCH
         , FADD, FADDP, FIADD
         , FSUB, FSUBP, FISUB
         , FSUBR, FSUBRP, FISUBR
         , FMUL, FMULP, FIMUL
         , FDIV, FDIVP, FIDIV
         , FDIVR, FDIVRP, FIDIVR
         , FCOMI, FCOMIP, FUCOMI, FUCOMIP
         , FLDCW, FCHS, FLDZ, FLD1, FLDPI
         , FCMOVB, FCMOVE, FCMOVBE, FCMOVU, FCMOVNB, FCMOVNE, FCMOVNBE, FCMOVNU
         , EMMS
     ]

-- Does the instruction need no modification?
-- For example, instructions without destination (CMP, TEST) or function calls and returns.
do_not_modify :: Opcode -> Bool
do_not_modify mnemonic = isCall mnemonic
  || isJump mnemonic
  || isCondJump mnemonic
  || isRet mnemonic
  || isHalt mnemonic
  || mnemonic
  `elem` [ CMP
         , TEST
         , CMPS
         , CMPSB
         , CMPSW
         , CMPSD
         , PTEST
         , PCMPEQB
         , PCMPEQD
         , PCMPGTB
         , PCMPGTD
         , UCOMISS
         , COMISS
         , UCOMISD
         , CMPLTSD
         , CMPEQSD
         , CMPNEQSD
         ]
-- TODO:
-- BLENDVP, BLENDVPS read from XMM0 sometimes as well?
-- VANDPS: depends on number of operands (3 or 2)
-- MOVSD, MOVSQ
-- SYSRET

lowpart IMUL = IMUL_LO
lowpart MUL  = MUL_LO
lowpart IDIV = IDIV_LO
lowpart DIV  = DIV_LO
hipart IMUL  = IMUL_HI
hipart MUL   = MUL_HI
hipart IDIV  = IDIV_HI
hipart DIV   = DIV_HI





