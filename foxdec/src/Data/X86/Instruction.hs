{-# LANGUAGE DeriveGeneric #-}

module Data.X86.Instruction where

import Base

import Data.Size

import Data.Word
import Data.List
import Data.Int
import Data.X86.Opcode
import Data.X86.Register
import Debug.Trace

import qualified Data.Serialize as Cereal hiding (get,put)
import Control.DeepSeq
import GHC.Generics (Generic)


data Instruction = Instruction {
      inAddress     :: Word64
    , inPrefix      :: [Prefix]
    , inOperation   :: Opcode
    , inOperands    :: [Operand]
    , inInfo        :: [InstructionInfo]
    , inSize        :: Int
    } deriving (Eq, Generic,Ord)

data Prefix =
      PrefixO16
    | PrefixA32
    | PrefixRepNE
    | PrefixRep
    | PrefixLock
    | PrefixSeg SReg
    | PrefixRex Word8
    | PrefixBnd
    | PrefixNoTrack
    | PrefixNotTaken
    deriving (Eq, Generic, Ord)

data OperandAccessInfo = Read | Write
  deriving (Eq, Generic,Ord)

instance Show OperandAccessInfo where
  show Read = "R"
  show Write = "W"

data Operand =
        Op_Mem {
                mSize  :: BitSize
              , mReg   :: Register
              , mIdx   :: Register
              , mScale :: Word8
              , mDisp  :: Int64
              , mSeg   :: Maybe SReg
              , mInfo  :: [OperandAccessInfo]
              }
      | Op_Reg Register [OperandAccessInfo]
      | Op_Imm Immediate
    deriving (Eq, Ord, Generic)

data InstructionInfo = WritesToFlags
  deriving (Eq, Ord, Generic)



data Immediate = Immediate {
        iSize :: BitSize
      , iValue :: Word64
    } deriving (Eq, Ord, Generic)

instance Show Immediate where
  show (Immediate si v) = "0x" ++ showHex v


instance Show Prefix where
  show PrefixRepNE = "REPNE"
  show PrefixRep = "REP"
  show PrefixLock = "LOCK"
  show PrefixNotTaken = "NOTTAKEN"
  show _ = ""

instance Show Operand where
  show (Op_Reg r _) = show r
  show (Op_Imm imm) = show imm

  show (Op_Mem (BitSize si) reg idx scale displ seg _) = show_size_directive si ++ "[" ++ show_seg seg ++ show_reg reg ++ show_idx_scale idx scale ++ show_displ displ ++ "]"
   where
    show_seg Nothing  = ""
    show_seg (Just r) = show r ++ ":"
    show_reg RegNone = ""
    show_reg reg     = show reg
    show_idx_scale RegNone 0 = ""
    show_idx_scale RegNone 1 = ""
    show_idx_scale _  0      = error "todo"
    show_idx_scale idx scale = " + " ++ show idx ++ "*" ++ showHex scale
    show_displ displ = if displ < 0 then " - 0x" ++ showHex (fromIntegral (0-displ)) else " + 0x" ++ showHex displ 
    show_size_directive 512 = "ZWORD PTR "
    show_size_directive 256 = "YWORD PTR "
    show_size_directive 128 = "XWORD PTR "
    show_size_directive 80 = "TWORD PTR "
    show_size_directive 64 = "QWORD PTR "
    show_size_directive 32 = "DWORD PTR "
    show_size_directive 16 = "WORD PTR "
    show_size_directive 8 = "BYTE PTR "
    show_size_directive 0 = ""
    show_size_directive n = error $ show n


instance Show Instruction where
  show (Instruction a ps op srcs info si) = concat
    [ showHex a ++ ": "
    , concatMap show_prefix ps
    , show op
    , " "
    , intercalate ", " (map show srcs)
    , " (" ++ show si ++ ")" ]
   where
    show_prefix (PrefixRex _) = ""
    show_prefix (PrefixSeg _) = ""
    show_prefix (PrefixO16)   = ""
    show_prefix p = show p ++ " "


inDests (Instruction a ps op ops info si) = takeWhile isWritten ops
 where
  isWritten (Op_Reg _ ai) = Write `elem` ai
  isWritten (Op_Mem _ _ _ _ _ _ ai) = Write `elem` ai
  isWritten (Op_Imm _) = False

inSrcs (Instruction a ps op ops info si) = takeWhile isRead $ dropWhile (not . isRead) ops
 where
  isRead (Op_Reg _ ai) = Read `elem` ai
  isRead (Op_Mem _ _ _ _ _ _ ai) = Read `elem` ai
  isRead (Op_Imm _) = True


operand_size (Op_Reg r _)   = regSize r
operand_size (Op_Mem (BitSize 128) _ _ _ _ _ _)  = ByteSize 16
operand_size (Op_Mem (BitSize 80) _ _ _ _ _ _)   = ByteSize 10
operand_size (Op_Mem (BitSize 64) _ _ _ _ _ _)   = ByteSize 8
operand_size (Op_Mem (BitSize 32) _ _ _ _ _ _)   = ByteSize 4
operand_size (Op_Mem (BitSize 16) _ _ _ _ _ _)   = ByteSize 2
operand_size (Op_Mem (BitSize 8) _ _ _ _ _ _)    = ByteSize 1
operand_size (Op_Imm (Immediate (BitSize si) _)) = ByteSize $ si `div` 8
operand_size op = error $ show op


mk_RSP_mem_operand (ByteSize si) = Op_Mem (BitSize $ si*8) (Reg64 RSP) RegNone 0 0 Nothing 


withoutWrite (Op_Reg r info) = Op_Reg r $ delete Write info
withoutWrite (Op_Mem si reg idx scale displ seg info) = Op_Mem si reg idx scale displ seg $ delete Write info
withoutWrite (Op_Imm imm) = Op_Imm imm


-- XCHG
canonicalize (Instruction label prefix XCHG [dst,src] info annot) =
  [
    Instruction label prefix MOV [Op_Reg RegTemp [Write], withoutWrite dst] info annot,
    Instruction label prefix MOV [dst, withoutWrite src] info annot,
    Instruction label prefix MOV [src, Op_Reg RegTemp [Read]] info annot
  ]
-- FXCH 
canonicalize (Instruction label prefix FXCH [dst,src] info annot) =
  [
    Instruction label prefix MOV [Op_Reg RegTemp [Write], withoutWrite dst] info annot,
    Instruction label prefix MOV [dst, withoutWrite src] info annot,
    Instruction label prefix MOV [src, Op_Reg RegTemp [Read]] info annot
  ]
-- XADD
canonicalize (Instruction label prefix XADD [dst,src] info annot) =
  [
    Instruction label prefix MOV [Op_Reg RegTemp [Write], withoutWrite src] info annot,
    Instruction label prefix ADD [Op_Reg RegTemp [Read,Write], withoutWrite dst] info annot,
    Instruction label prefix MOV [dst, withoutWrite src] info annot,
    Instruction label prefix MOV [src, Op_Reg RegTemp [Read]] info annot
  ]
-- CMPXCHG
canonicalize (Instruction label prefix CMPXCHG [dst,src] info annot) =
  [
    Instruction label prefix CMPXCHG [Op_Reg (Reg64 RAX) [Write], dst,src] info annot,
    Instruction label prefix CMPXCHG [dst,dst,src] info annot
  ]
-- XGETBV
canonicalize (Instruction label prefix XGETBV [] info annot) =
  [
    Instruction label prefix XGETBV [Op_Reg (Reg32 RDX) [Write]] info annot,
    Instruction label prefix XGETBV [Op_Reg (Reg32 RAX) [Write]] info annot
  ]
-- The remaining cases
canonicalize i@(Instruction label prefix mnemonic ops info annot)
  | mnemonic `elem` [CBW, CWDE, CDQE] = canonicalize_sextend1 i
  | mnemonic `elem` [CWD, CDQ, CQO] = canonicalize_sextend2 i
  | mnemonic `elem` [MUL, IMUL] = canonicalize_mul i
  | mnemonic `elem` [DIV, IDIV] = canonicalize_div i
  | otherwise = [i] -- error $ "Cannot canonicalize instruction: " ++ show i


-- CBW / CWDE / CDQE
canonicalize_sextend1 (Instruction label prefix mnemonic [] info annot) =
  let srcs = case mnemonic of
        CBW  -> [Reg16 RAX, Reg8 RAX HalfL]
        CWDE -> [Reg32 RAX, Reg16 RAX]
        CDQE -> [Reg64 RAX, Reg32 RAX]
        _    -> error "Invalid extend sources"
  in [ Instruction
         label
         prefix
         mnemonic
         [ Op_Reg (srcs !! 0) [Write], Op_Reg (srcs !! 1) [Read] ]
         info 
         annot]
canonicalize_sextend1 _ = error "Invalid extend instruction"

-- CWD / CDQ / CQO
canonicalize_sextend2 (Instruction label prefix mnemonic [] info annot) =
  let srcs = case mnemonic of
        CWD -> [Reg16 RDX, Reg16 RAX]
        CDQ -> [Reg32 RDX, Reg32 RAX]
        CQO -> [Reg64 RDX, Reg64 RAX]
        _   -> error "invalid extends source"
  in [ Instruction
         label
         prefix
         mnemonic
         [ Op_Reg (srcs !! 0) [Write], Op_Reg (srcs !! 1) [Read] ]
         info 
         annot ]
canonicalize_sextend2 _ = error "Invalid extend instruction"

-- MUL /IMUL (1)
canonicalize_mul (Instruction label prefix mnemonic [op1] info annot) =
  let srcs = case operand_size op1 of
        ByteSize 8 -> [Reg64 RDX, Reg64 RAX]
        ByteSize 4 -> [Reg32 RDX, Reg32 RAX]
        ByteSize 2 -> [Reg16 RDX, Reg16 RAX]
        ByteSize 1 -> [Reg8 RAX HalfH, Reg8 RAX HalfL]
        _ -> error "Invalid operand size"
  in [ Instruction
         label
         prefix
         (hipart mnemonic)
         [ Op_Reg (srcs !! 0) [Write], Op_Reg (srcs !! 1) [Read], op1 ]
         info 
         annot
     , Instruction
         label
         prefix
         (lowpart mnemonic)
         [ Op_Reg (srcs !! 1) [Write], Op_Reg (srcs !! 1) [Read], op1 ]
         info 
         annot]
-- MUL /IMUL (2)
canonicalize_mul (Instruction label prefix mnemonic [op1,op2] info annot) =
  [Instruction label prefix mnemonic [op1, op2] info annot]
-- MUL /IMUL (3)
canonicalize_mul (Instruction label prefix mnemonic [op1,op2, op3] info annot) =
  [Instruction label prefix mnemonic [op1, op2, op3] info annot]
canonicalize_mul _ = error "Invalid mul operation"

-- DIV /IDIV (1)
canonicalize_div (Instruction label prefix mnemonic [op1] info annot) =
  let srcs = case operand_size op1 of
        ByteSize 8 -> [Reg64 RDX, Reg64 RAX]
        ByteSize 4 -> [Reg32 RDX, Reg32 RAX]
        ByteSize 2 -> [Reg16 RDX, Reg16 RAX]
        ByteSize 1 -> [Reg8 RAX HalfH, Reg8 RAX HalfL]
        _ -> error "Invalid operand size"
  in [ Instruction label prefix MOV [Op_Reg RegTemp [Write],Op_Reg (srcs!!0) [Read]] info annot
     , Instruction
         label
         prefix
         (hipart mnemonic)
         [Op_Reg (srcs!!0) [Write], Op_Reg (srcs!!0) [Read], Op_Reg (srcs!!1) [Read], op1]
         info 
         annot
     , Instruction
         label
         prefix
         (lowpart mnemonic)
         [Op_Reg (srcs!!1) [Write], Op_Reg RegTemp [Read], Op_Reg (srcs!!1) [Read], op1]
         info 
         annot
     ] -- , Instruction label prefix MOV [Op_Reg (srcs!!0) [Write], Op_Reg RegTemp [Read]] info annot ]
canonicalize_div _ = error "Invalid div instruction"




lowpart IMUL = IMUL_LO
lowpart MUL  = MUL_LO
lowpart IDIV = IDIV_LO
lowpart DIV  = DIV_LO
hipart IMUL  = IMUL_HI
hipart MUL   = MUL_HI
hipart IDIV  = IDIV_HI
hipart DIV   = DIV_HI


{--

-- TODO leverage XED info on read and writen operands instead
-- | Canonicalizes an instruction by making sure there is at most one output and inputs/outputs are syntactically clear.
-- | For example, the IMUL instruction comes in different flavors that hide the actual inputs and outputs to multiple registers
canonicalize :: Instruction -> [Instruction]
canonicalize (Instruction label prefix PUSH Nothing [op1] annot) =
  let ByteSize si' = operand_size op1
      si = 8-- if si' == 1 then 8 else si' -- TODO TEST if operandsize /= 8 or 1
  in [ Instruction
         label
         prefix
         SUB
         (Just $ Op_Reg $ Reg64 RSP)
         [Op_Reg $ Reg64 RSP, Op_Imm $ Immediate (BitSize 64) $ fromIntegral si]
         annot
     , Instruction
         label
         prefix
         MOV
         (Just $ mk_RSP_mem_operand (ByteSize si))
         [op1]
         annot]
-- POP
canonicalize (Instruction label prefix POP Nothing [op1] annot) =
  let ByteSize si' = operand_size op1
      si = 8-- if si' == 1 then 8 else si'
  in [ Instruction
         label
         prefix
         MOV
         (Just op1)
         [mk_RSP_mem_operand (ByteSize si)]
         annot
     , Instruction
         label
         prefix
         ADD
         (Just $ Op_Reg $ Reg64 RSP)
         [Op_Reg $ Reg64 RSP, Op_Imm $ Immediate (BitSize 64) $ fromIntegral si]
         annot]
-- LEAVE 
canonicalize (Instruction label prefix LEAVE Nothing [] annot) =
  Instruction label prefix MOV (Just $ Op_Reg $ Reg64 RSP) [Op_Reg $ Reg64 RBP] annot
  :canonicalize (Instruction label prefix POP Nothing [Op_Reg $ Reg64 RBP] annot)

-- Does the instruction read from all operands, including the first one?
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
         , ANDPS
         , ANDNPS
         , ORPS
         , XORPS
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
         , PUNPCKHBW
         , PUNPCKHWD
         , PUNPCKLWD
         , PUNPCKLQDQ
         , PUNPCKLBW
         , PUNPCKLDQ
         , PUNPCKHDQ
         , PCMPGTD
         , PCMPEQB
         , PCMPEQD
         , PCMPGTB
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
         , PSLLD
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
         , PEXTRW
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
         , MAXSS
         , MINSS
         , SUBSD
         , ADDSD
         , DIVSD
         , MULSD
         , ROUNDSD
         , UNPCKLPD
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
         , SHUFPD
         , SHUFPS
         , PSHUFB
         , PSHUFD
         , VPSHUFB
         , VPSHUFD
         , PSHUFLW
         , FST, FSTP, FIST, FISTP, FISTTP
         , FSTCW, FNSTCW
         , SQRTSD
         , SQRTSS
         , PMOVMSKB
         , PSRAW, PSRAD
         , MOVSLDUP, MOVLPD, MOVDDUP, MOVHLPS, MOVSHDUP
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
         , FCOMI, FCOMIP, FUCOMI, FUCOMIP, FCOMPI
         , FLDCW, FCHS, FLDZ, FLD1, FLDPI
         , FCMOVB, FCMOVE, FCMOVBE, FCMOVU, FCMOVNB, FCMOVNE, FCMOVNBE, FCMOVNU
         , SCAS, SCASB, SCASD
         , EMMS
         , CLD
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
         , UCOMISS
         , COMISS
         , UCOMISD
         , CMPSS
         , CMPLTSD
         , CMPLTSS
         , CMPEQSD
         , CMPNEQSD
         , CMPEQSD
         , CMPLTSD
         , CMPNLESD
         , CMPNLESS
         , CMPNEQSD
         , CMPNLESD
         , OUT
         ]
-- TODO:
-- BLENDVP, BLENDVPS read from XMM0 sometimes as well?
-- VANDPS: depends on number of operands (3 or 2)
-- SYSRET





--}

instance Cereal.Serialize Immediate
instance Cereal.Serialize OperandAccessInfo
instance Cereal.Serialize Operand
instance Cereal.Serialize Prefix
instance Cereal.Serialize InstructionInfo
instance Cereal.Serialize Instruction

instance NFData Immediate
instance NFData Prefix
instance NFData Instruction
instance NFData OperandAccessInfo
instance NFData Operand
instance NFData InstructionInfo
