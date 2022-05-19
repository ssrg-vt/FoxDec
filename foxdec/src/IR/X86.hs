{-# LANGUAGE TupleSections #-}

module IR.X86
    ( Label
    , Storage
    , Instruction
    , Program
    , fromContext
    , canonicalize) where

import           Base (showHex)
import           Analysis.Context (CFG(..), Context(..))
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Maybe (isNothing)
import           Data.Void (Void)
import qualified X86.Register as X86
import           X86.Register (Register(..))
import           X86.Prefix (Prefix)
import           X86.Opcode (Opcode(..), isHalt, isRet, isCondJump, isJump
                           , isCall)
import           Typeclasses.HasSize (HasSize(sizeof))
import           X86.Address (GenericAddress(..))
import           X86.Operand (GenericOperand(..))
import           Generic.Operand (GenericOperand(..)) -- TODO: why is this needed?
import           Generic.Address (AddressWord64)
import           X86.Instruction (GenericInstruction(Instruction))
import Generic.Program (GenericProgram)

--------------------------------------------------------------------------------
-- DATA
--------------------------------------------------------------------------------
type Label = AddressWord64

type Storage = X86.Register

type Instruction = GenericInstruction Label Storage Prefix Opcode Int

type Program = GenericProgram Instruction

--------------------------------------------------------------------------------
-- PARSING
--------------------------------------------------------------------------------
-- | From a context stored in a .report file, retrieve an L0 program for a given function entry.
fromContext :: Context -- ^ The context
            -> Int     -- ^ The function entry of interest
            -> Program
fromContext ctxt entry =  undefined 
  
  -- case IM.lookup entry $ ctxt_cfgs ctxt of
  -- Just cfg -> cfg_to_L0 cfg
  -- Nothing  -> error $ "Function entry " ++ showHex entry ++ " does not exist."
  -- where
  --   -- convert a CFG to L0
  --   -- add edges for all terminal blocks to an empty set of successors
  --   cfg_to_L0 cfg =
  --     let blocks = IM.map (map (Generic.StmtInstruction . pure))
  --           $ cfg_instrs cfg
  --         edges = cfg_edges cfg
  --         terminals = filter (is_terminal_block cfg) $ IM.keys (cfg_blocks cfg)
  --         edges' = IM.fromList $ map (, IS.empty) terminals
  --     in Generic.Program blocks (0, IM.unionWith IS.union edges edges')

  --   -- if the block terminal?
  --   is_terminal_block cfg b = isNothing $ IM.lookup b (cfg_edges cfg)

--------------------------------------------------------------------------------
-- X86 -> X86
--------------------------------------------------------------------------------
canonicalize :: Program -> Program
canonicalize =  undefined 
  
  -- mapP explicitize id
  -- where
  --   -- PUSH
  --   explicitize [Instruction label prefix PUSH Nothing [op1] annot] =
  --     let si = sizeof op1
  --     in [ Instruction
  --            label
  --            prefix
  --            SUB
  --            (Just $ Storage RSP)
  --            [Storage RSP, Immediate $ fromIntegral si]
  --            annot
  --        , Instruction
  --            label
  --            prefix
  --            MOV
  --            (Just $ Memory (AddressStorage RSP) si)
  --            [op1]
  --            Nothing]
  --   -- POP
  --   explicitize [Instruction label prefix POP Nothing [op1] annot] =
  --     let si = sizeof op1
  --     in [ Instruction
  --            label
  --            prefix
  --            MOV
  --            (Just op1)
  --            [Memory (AddressStorage RSP) si]
  --            annot
  --        , Instruction
  --            label
  --            prefix
  --            ADD
  --            (Just $ Storage RSP)
  --            [Storage RSP, Immediate $ fromIntegral si]
  --            Nothing]
  --   -- LEAVE 
  --   explicitize [Instruction label prefix LEAVE Nothing [] annot] =
  --     Instruction label prefix MOV (Just $ Storage RSP) [Storage RBP] annot
  --     :explicitize [Instruction label prefix POP Nothing [Storage RBP] annot]
  --   -- The remaining cases
  --   explicitize [i@(Instruction label prefix mnemonic Nothing ops annot)]
  --     | mnemonic `elem` [CBW, CWDE, CDQE] = explicitize_sextend1 i
  --     | mnemonic `elem` [CWD, CDQ, CQO] = explicitize_sextend2 i
  --     | mnemonic `elem` [MUL, IMUL] = explicitize_mul i
  --     | mnemonic `elem` [DIV, IDIV] = explicitize_div i
  --     | mnemonic_reads_from_all_operands mnemonic =
  --       [Instruction label prefix mnemonic (Just $ head ops) ops annot]
  --     | mnemonic_reads_from_all_but_first_operands mnemonic =
  --       [Instruction label prefix mnemonic (Just $ head ops) (tail ops) annot]
  --     | do_not_modify mnemonic = [i]
  --     | otherwise = error $ "Cannot canonicalize instruction: " ++ show i
  --   explicitize _ = error "Unknown instruction"

  --   -- CBW / CWDE / CDQE
  --   explicitize_sextend1 (Instruction label prefix mnemonic Nothing [] annot) =
  --     let srcs = case mnemonic of
  --           CBW  -> [AX, AL]
  --           CWDE -> [EAX, AX]
  --           CDQE -> [RAX, EAX]
  --           _    -> error "Invalid extend sources"
  --     in [ Instruction
  --            label
  --            prefix
  --            mnemonic
  --            (Just $ Storage $ head srcs)
  --            [Storage $ srcs !! 1]
  --            annot]
  --   explicitize_sextend1 _ = error "Invalid extend instruction"

  --   -- CWD / CDQ / CQO
  --   explicitize_sextend2 (Instruction label prefix mnemonic Nothing [] annot) =
  --     let srcs = case mnemonic of
  --           CWD -> [DX, AX]
  --           CDQ -> [EDX, EAX]
  --           CQO -> [RDX, RAX]
  --           _   -> error "invalid extends source"
  --     in [ Instruction
  --            label
  --            prefix
  --            mnemonic
  --            (Just $ Storage $ head srcs)
  --            [Storage $ srcs !! 1]
  --            annot
  --        , Instruction
  --            label
  --            prefix
  --            mnemonic
  --            (Just $ Storage $ srcs !! 1)
  --            [Storage $ srcs !! 1]
  --            Nothing]
  --   explicitize_sextend2 _ = error "Invalid extend instruction"

  --   -- MUL /IMUL (1)
  --   explicitize_mul (Instruction label prefix mnemonic Nothing [op1] annot) =
  --     let srcs = case sizeof op1 of
  --           8 -> [RDX, RAX]
  --           4 -> [EDX, EAX]
  --           2 -> [DX, AX]
  --           1 -> [AH, AL]
  --           _ -> error "Invalid operand size"
  --     in [ Instruction
  --            label
  --            prefix
  --            mnemonic
  --            (Just $ Storage $ head srcs)
  --            [Storage $ srcs !! 1, op1]
  --            annot
  --        , Instruction
  --            label
  --            prefix
  --            mnemonic
  --            (Just $ Storage $ srcs !! 1)
  --            [Storage $ srcs !! 1, op1]
  --            Nothing]
  --   -- MUL /IMUL (2)
  --   explicitize_mul
  --     (Instruction label prefix mnemonic Nothing [op1, op2] annot) =
  --     [Instruction label prefix mnemonic (Just op1) [op1, op2] annot]
  --   -- MUL /IMUL (3)
  --   explicitize_mul
  --     (Instruction label prefix mnemonic Nothing [op1, op2, op3] annot) =
  --     [Instruction label prefix mnemonic (Just op1) [op2, op3] annot]
  --   explicitize_mul _ = error "Invalid mul operation"

  --   -- DIV /IDIV (1)
  --   explicitize_div (Instruction label prefix mnemonic Nothing [op1] annot) =
  --     let srcs = case sizeof op1 of
  --           8 -> [RDX, RAX]
  --           4 -> [EDX, EAX]
  --           2 -> [DX, AX]
  --           1 -> [AH, AL]
  --           _ -> error "Invalid operand size"
  --     in [ Instruction
  --            label
  --            prefix
  --            mnemonic
  --            (Just $ Storage $ head srcs)
  --            [Storage $ head srcs, Storage $ srcs !! 1, op1]
  --            annot
  --        , Instruction
  --            label
  --            prefix
  --            mnemonic
  --            (Just $ Storage $ srcs !! 1)
  --            [Storage $ head srcs, Storage $ srcs !! 1, op1]
  --            Nothing]
  --   explicitize_div _ = error "Invalid div instruction"

  --   -- Does the instruction read from all operands, inlcuding the first one?
  --   mnemonic_reads_from_all_operands mnemonic = mnemonic
  --     `elem` [ ADD
  --            , SUB
  --            , NEG
  --            , INC
  --            , DEC
  --            , SHL
  --            , SHL
  --            , ADC
  --            , SBB
  --            , ROL
  --            , ROR
  --            , SHR
  --            , SHR
  --            , SAR
  --            , SAR
  --            , SHLD
  --            , SHRD
  --            , XOR
  --            , OR
  --            , AND
  --            , NOT
  --            , BT
  --            , BTC
  --            , BTR
  --            , BSR
  --            , BSF
  --            , BTS
  --            , BSWAP
  --            , SETO
  --            , SETNO
  --            , SETS
  --            , SETNS
  --            , SETE
  --            , SETZ
  --            , SETNE
  --            , SETNZ
  --            , SETB
  --            , SETNAE
  --            , SETC
  --            , SETNB
  --            , SETAE
  --            , SETNC
  --            , SETBE
  --            , SETNA
  --            , SETA
  --            , SETNBE
  --            , SETL
  --            , SETNGE
  --            , SETGE
  --            , SETNL
  --            , SETLE
  --            , SETNG
  --            , SETG
  --            , SETNLE
  --            , SETP
  --            , SETPE
  --            , SETNP
  --            , SETPO
  --            , XORPD
  --            , XORPS
  --            , ANDPD
  --            , ANDNPD
  --            , ORPD
  --            , SUBPD
  --            , ADDPD
  --            , HADDPD
  --            , POR
  --            , PAND
  --            , PANDN
  --            , PXOR
  --            , VPOR
  --            , VPAND
  --            , VPANDN
  --            , VPXOR
  --            , PUNPCKLQDQ
  --            , PUNPCKLBW
  --            , PUNPCKLDQ
  --            , PCMPGTD
  --            , PADDD
  --            , PADDB
  --            , PADDQ
  --            , PSUBD
  --            , PSUBB
  --            , PSUBQ
  --            , PMULLD
  --            , PMINSD
  --            , PMAXSD
  --            , PMINUD
  --            , PMAXUD
  --            , PMAXUQ
  --            , PMAXUQ
  --            , PSRLD
  --            , PSRLW
  --            , PSRLDQ
  --            , PSLLDQ
  --            , PSLLQ
  --            , PSRLQ
  --            , PSUBUSB
  --            , PSUBUSW
  --            , PINSRB
  --            , PINSRQ
  --            , PINSRD
  --            , PEXTRB
  --            , PEXTRD
  --            , PEXTRQ
  --            , PCLMULQDQ
  --            , PACKSSDW
  --            , PACKSSWB
  --            , SUBSS
  --            , ADDSS
  --            , DIVSS
  --            , MULSS
  --            , ROUNDSS
  --            , SUBSD
  --            , ADDSD
  --            , DIVSD
  --            , MULSD
  --            , ROUNDSD]

  --   -- Does the instruction read from all operands, except for the first one?
  --   mnemonic_reads_from_all_but_first_operands mnemonic = mnemonic
  --     `elem` [ LEA
  --            , MOV
  --            , MOVZX
  --            , MOVSX
  --            , MOVSXD
  --            , MOVAPS
  --            , MOVAPD
  --            , MOVABS
  --            , MOVUPD
  --            , MOVUPS
  --            , MOVDQU
  --            , MOVDQA
  --            , MOVD
  --            , MOVQ
  --            , MOVLPD
  --            , MOVLPS
  --            , MOVSD
  --            , MOVSS
  --            , VMOVD
  --            , VMOVAPD
  --            , VMOVAPS
  --            , CMOVO
  --            , CMOVNO
  --            , CMOVS
  --            , CMOVNS
  --            , CMOVE
  --            , CMOVZ
  --            , CMOVNE
  --            , CMOVNZ
  --            , CMOVB
  --            , CMOVNAE
  --            , CMOVC
  --            , CMOVNB
  --            , CMOVAE
  --            , CMOVNC
  --            , CMOVBE
  --            , CMOVNA
  --            , CMOVA
  --            , CMOVNBE
  --            , CMOVL
  --            , CMOVNGE
  --            , CMOVG
  --            , CMOVGE
  --            , CMOVNL
  --            , CMOVLE
  --            , CMOVNG
  --            , CMOVNLE
  --            , CMOVP
  --            , CMOVPE
  --            , CMOVNP
  --            , CMOVPO
  --            , CVTSS2SD
  --            , CVTSI2SS
  --            , CVTSI2SD
  --            , CVTSD2SS
  --            , CVTTSS2SI
  --            , CVTTSD2SI
  --            , CVTTPD2DQ
  --            , CVTDQ2PD
  --            , MOVMSKPD
  --            , MOVMSKPS
  --            , PMOVSXDQ
  --            , PMOVZXDQ
  --            , PMOVSXBD
  --            , PMOVZXBD
  --            , UNPCKLPS
  --            , BLENDVPD
  --            , BLENDVPS
  --            , EXTRACTPS
  --            , VINSERTF128
  --            , VEXTRACTI128
  --            , VEXTRACTF128
  --            , VPERM2F128
  --            , VPERM2I128
  --            , VPALIGNR
  --            , PALIGNR
  --            , SHUFPS
  --            , PSHUFB
  --            , PSHUFD
  --            , VPSHUFB
  --            , VPSHUFD
  --            , PSHUFLW]

  --   -- Does the instruction need no modification?
  --   -- For example, instructions without destination (CMP, TEST) or function calls and returns.
  --   do_not_modify mnemonic = isCall mnemonic
  --     || isJump mnemonic
  --     || isCondJump mnemonic
  --     || isRet mnemonic
  --     || isHalt mnemonic
  --     || mnemonic
  --     `elem` [ CMP
  --            , TEST
  --            , CMPS
  --            , CMPSB
  --            , CMPSW
  --            , CMPSD
  --            , PTEST
  --            , PCMPEQB
  --            , PCMPEQD
  --            , PCMPGTB
  --            , PCMPGTD
  --            , UCOMISS
  --            , COMISS
  --            , UCOMISD
  --            , CMPLTSD
  --            , CMPEQSD
  --            , CMPNEQSD
  --            , NOP
  --            , ENDBR64
  --            , UD2
  --            , WAIT
  --            , MFENCE
  --            , CLFLUSH]
-- TODO:
-- BLENDVP, BLENDVPS read from XMM0 sometimes as well?
-- At SymbolicExecution the instructions add lines 1500 to 1537 (FST to EMMS)
-- At SymbolicExecution the instructions add lines 1547 to 1572 (VANDPS to VMOVHPS)
-- At SymbolicExecution the instructions add lines 1575 to 1593 (CPUID to WRGSBASE)
-- VANDPS: depends on number of operands (3 or 2)
-- XCHG,XADD,CMPXCHG
-- MOVSD, MOVSQ
