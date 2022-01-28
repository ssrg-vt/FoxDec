{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

{-|
Module      : X86_Datastructures
Description : Datastructures for storing x86-64 instructions.
-}

module X86_Datastructures where

import Data.List
import Data.Word (Word64)
import Base
import qualified Data.Map as M
import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)


-- | An instruction
data Instr = Instr {
  i_addr :: Int,                 -- ^ address
  i_prefix :: Maybe Prefix,      -- ^ prefix, e.g., lock or repz
  i_opcode :: Opcode,            -- ^ opcode (see data Opcode)
  i_op1 :: Maybe Operand,        -- ^ optional: operand
  i_op2 :: Maybe Operand,        -- ^ optional: operand
  i_op3 :: Maybe Operand,        -- ^ optional: operand
  i_annot :: Maybe String,       -- ^ annotation, e.g., <malloc@plt + 10>
  i_size :: Int                  -- ^ size of instruction
 }
 deriving (Eq,Ord, Generic)

instance Cereal.Serialize Instr

-- | Instruction prefixes
data Prefix = InvalidPrefix | REP | REPZ | LOCK | BND
  deriving (Show,Eq,Ord,Read,Generic)

instance Cereal.Serialize Prefix

-- | Registers
data Register = InvalidRegister
  | RIP | EIP
  | RAX | EAX | AX | AH | AL 
  | RBX | EBX | BX | BH | BL
  | RCX | ECX | CX | CH | CL
  | RDX | EDX | DX | DH | DL
  | RDI | EDI | DI | DIL
  | RSI | ESI | SI | SIL
  | RSP | ESP | SP | SPL 
  | RBP | EBP | BP | BPL 
  | R15 | R15D | R15W | R15B
  | R14 | R14D | R14W | R14B
  | R13 | R13D | R13W | R13B
  | R12 | R12D | R12W | R12B
  | R11 | R11D | R11W | R11B
  | R10 | R10D | R10W | R10B
  | R9 | R9D | R9W | R9B
  | R8 | R8D | R8W | R8B
  | CS | DS | ES | FS | GS | SS
  | EIZ | RIZ
  | ST0 | ST1 | ST2 | ST3 | ST4 | ST5 | ST6 | ST7
  | YMM0 | YMM1 | YMM2 | YMM3 | YMM4 | YMM5 | YMM6 | YMM7 | YMM8 | YMM9 | YMM10 | YMM11 | YMM12 | YMM13 | YMM14 | YMM15
  | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 | XMM8 | XMM9 | XMM10 | XMM11 | XMM12 | XMM13 | XMM14 | XMM15
  -- XMM0_L | XMM1_L | XMM2_L | XMM3_L | XMM4_L | XMM5_L | XMM6_L | XMM7_L | XMM8_L | XMM9_L | XMM10_L | XMM11_L | XMM12_L | XMM13_L | XMM14_L | XMM15_L
  deriving (Show,Eq,Read,Ord,Generic)

instance Cereal.Serialize Register

-- | Flags
data Flag = ZF | CF | SF | OF | PF | InvalidFlag
  deriving (Show,Eq,Ord)

-- | An unresolved address, within the operand of an instruction.
data Address =
    FromReg Register           -- ^ Reading a pointer from a register
  | AddrImm Int                -- ^ Immediate address
  | AddrMinus Address Address  -- ^ Minus
  | AddrPlus Address Address   -- ^ Plus
  | AddrTimes Address Address  -- ^ Times
  | SizeDir Int Address        -- ^ Size directive, e.g., qword ptr, in bytes
  deriving (Eq,Ord,Generic)

instance Cereal.Serialize Address

-- | Operands of an instruction
data Operand =
    Address Address          -- ^ Unresolved addresses
  | Reg Register             -- ^ Registers
  | Immediate Word64         -- ^ Immediates
  deriving (Eq,Ord,Generic)

instance Cereal.Serialize Operand

-- | Opcodes / mnemonics
data Opcode = InvalidOpcode
  | AAA
  | AAD
  | AAM
  | AAS
  | ADC
  | ADD
  | ADDPD
  | ADDPS
  | ADDSD
  | ADDSS
  | ADDSUBPD
  | ADDUBPS
  | AND
  | ANDNPD
  | ANDNPS
  | ANDPD
  | ANDPS
  | ARPL
  | BLENDVPS
  | BOUND
  | BSF
  | BSR
  | BT
  | BTC
  | BTR
  | BTS
  | CALL
  | CALLF
  | CBW
  | CDQ
  | CDQE
  | CLC
  | CLD
  | CLFLUSH
  | CLI
  | CLTS
  | CMC
  | CMOVA
  | CMOVAE
  | CMOVB
  | CMOVBE
  | CMOVC
  | CMOVE
  | CMOVG
  | CMOVGE
  | CMOVL
  | CMOVLE
  | CMOVNA
  | CMOVNAE
  | CMOVNB
  | CMOVNBE
  | CMOVNC
  | CMOVNE
  | CMOVNG
  | CMOVNGE
  | CMOVNL
  | CMOVNLE
  | CMOVNO
  | CMOVNP
  | CMOVNS
  | CMOVNZ
  | CMOVO
  | CMOVP
  | CMOVPE
  | CMOVPO
  | CMOVS
  | CMOVZ
  | CMP
  | CMPEQSD
  | CMPNEQSD
  | CMPLTSD
  | CMPS
  | CMPSB
  | CMPSD
  | CMPXCHG
  | CMPXCHG16B
  | CMPXCHG8B
  | COMISD
  | COMISS
  | CPUID
  | CQO
  | CVTDQ2PD
  | CVTSD2SS
  | CVTSI2SD
  | CVTSI2SS
  | CVTSS2SD
  | CVTTSD2SI
  | CVTTSS2SI
  | CVTTPD2DQ
  | CWD
  | CWDE
  | DAA
  | DAS
  | DEC
  | DIV
  | DIVPD
  | DIVPS
  | DIVSD
  | DIVSS
  | EMMS
  | ENDBR64
  | ENTER
  | EXTRACTPS
  | FABS
  | FADD
  | FADDP
  | FBLD
  | FBSTP
  | FCHS
  | FCLEX
  | FCMOVB
  | FCMOVBE
  | FCMOVE
  | FCMOVNB
  | FCMOVNBE
  | FCMOVNE
  | FCMOVNU
  | FCMOVU
  | FCOM
  | FCOMI
  | FCOMIP
  | FCOMP
  | FCOMPI
  | FCOMPP
  | FCOS
  | FDIV
  | FDIVP
  | FDIVR
  | FDIVRP
  | FFREE
  | FRNDINT
  | FIADD
  | FICOM
  | FICOMP
  | FIDIV
  | FIDIVR
  | FILD
  | FIMUL
  | FINIT
  | FIST
  | FISTP
  | FISTPP
  | FISTTP
  | FISUB
  | FISUBR
  | FLD
  | FLD1
  | FLDCW
  | FLDENV
  | FLDL2E
  | FLDL2T
  | FLDLG2
  | FLDLN2
  | FLDPI
  | FLDZ
  | FMUL
  | FMULP
  | FNOP
  | FNINIT
  | FNSTCW
  | FPREM1
  | FRSTOR
  | FSAVE
  | FSIN
  | FSINCOS
  | FSCALE
  | FSQRT
  | FST
  | FSTCW
  | FSTENV
  | FSTP
  | FSTSW
  | FSUB
  | FSUBP
  | FSUBR
  | FSUBRP
  | FTST
  | FUCOM
  | FUCOMI
  | FUCOMIP
  | FUCOMP
  | FUCOMPI
  | FUCOMPP
  | FXAM
  | FXCH
  | FXRSTOR
  | FXSAVE
  | FXTRACT
  | HADDPD
  | HADDPS
  | HLT
  | HSUBPD
  | HSUBPS
  | IDIV
  | IMUL
  | BSWAP
  | IN
  | INC
  | INS
  | INSD
  | INT
  | INT3
  | INTO
  | INVD
  | INVLPG
  | INVPCID
  | IRET
  | IRETD
  | IRETQ
  | JA  
  | JAE
  | JB
  | JBE
  | JC
  | JCXZ
  | JE
  | JECXZ
  | JG
  | JGE
  | JL
  | JLE
  | JMP
  | JMPF
  | JMPN
  | JNAE
  | JNA
  | JNB
  | JNBE
  | JNC
  | JNG
  | JNE
  | JNGE
  | JNLE
  | JNL
  | JNO
  | JNP
  | JNS
  | JNZ
  | JO
  | JP
  | JPE
  | JPO
  | JRCXZ
  | JS
  | JZ
  | LAHF
  | LAR
  | LDDQU
  | LDMXCSR
  | LDS
  | LEA
  | LEAVE
  | LES
  | LFENCE
  | LFS
  | LGDT
  | LGS
  | LIDT
  | LLDT
  | LMSW
  | LODS
  | LODSB
  | LODSD
  | LODSW
  | LOOP
  | LOOPE
  | LOOPNE
  | LSL
  | LSS
  | LTR
  | MASKMOVQ
  | MAXPD
  | MAXPS
  | MAXSD
  | MAXSS
  | MFENCE
  | MINPD
  | MINPS
  | MINSD
  | MINSS
  | MONITOR
  | MOV 
  | MOVABS
  | MOVAPD
  | MOVAPS
  | MOVD
  | MOVDDUP
  | MOVDQA
  | MOVDQU
  | MOVHLPS
  | MOVHPD
  | MOVHPS
  | MOVLHPS
  | MOVLPD
  | MOVLPS
  | MOVLSDUP
  | MOVMSKPD
  | MOVMSKPS
  | MOVNTDQ
  | MOVNTPD
  | MOVNTPS
  | MOVNTQ
  | MOVQ
  | MOVS
  | MOVSD
  | MOVSLDUP
  | MOVSS
  | MOVSQ
  | MOVSX
  | MOVSXB
  | MOVSXD
  | MOVSXW
  | MOVUPD
  | MOVUPS
  | MOVZX
  | MOVZXB
  | MOVZXW
  | MUL
  | MULPD
  | MULPS
  | MULSD
  | MULSS
  | MWAIT
  | NEG
  | NOP
  | NOT
  | OR
  | ORPD
  | ORPS
  | OUT
  | OUTS
  | PALIGNR
  | PACKSSDW
  | PACKSSWB
  | PADDB
  | PADDD
  | PADDQ
  | PADDSB
  | PADDSW
  | PADDUSB
  | PADDUSW
  | PADDW
  | PAND
  | PANDN
  | PAUSE
  | PAVGB
  | PAVGW
  | PCLMULQDQ
  | PCMPEQB
  | PCMPEQD
  | PCMPGTB
  | PCMPGTD
  | PEXTRB
  | PEXTRD
  | PEXTRQ
  | PINSRD
  | PINSRQ
  | PMADDWD
  | PMAXSW
  | PMAXUB
  | PMAXUD
  | PMAXUQ
  | PMINSD
  | PMINSW
  | PMINUB
  | PMINUD
  | PMOVMSKB
  | PMOVSXDQ
  | PMOVZXDQ
  | PMOVSXBD
  | PMOVZXBD
  | PMULLD
  | PMULLQ
  | PMULHUW
  | PMULHW
  | PMULLW
  | PMULUDQ
  | POP
  | POPA
  | POPAD
  | POPF
  | POPFD
  | POPFQ
  | POR
  | PREFETCHNTA
  | PREFETCHT0
  | PREFETCHT1
  | PREFETCHT2
  | PSADBW
  | PSHUFB
  | PSHUFD
  | PSHUFLW
  | PSLLD
  | PSLLDQ
  | PSLLQ
  | PSLLW
  | PSRAD
  | PSRAW
  | PSRLD
  | PSRLDQ
  | PSRLQ
  | PSRLW
  | PSUBB
  | PSUBD
  | PSUBQ
  | PSUBSB
  | PSUBSQ
  | PSUBUSB
  | PSUBUSW
  | PSUBW
  | PTEST
  | PUNPCKLBW 
  | PUNPCKLWD
  | PUNPCKLDQ 
  | PUNPCKLQDQ
  | PUSH
  | PUSHA
  | PUSHAD
  | PUSHF
  | PUSHFD
  | PUSHFQ
  | PXOR
  | RCL
  | RCPPS
  | RCPSS
  | RCR
  | RDMSR
  | RDPMC
  | RDTSC
  | RET
  | RETF
  | RETN
  | ROL
  | ROR
  | ROUNDSD
  | ROUNDSS
  | RSM
  | RSQRTPS
  | RSQRTSS
  | SAHF
  | SAL
  | SAR
  | SBB
  | SCAS
  | SCASB
  | SCASD
  | SETA
  | SETAE
  | SETB
  | SETBE
  | SETC
  | SETE
  | SETG
  | SETGE
  | SETL
  | SETLE
  | SETNA
  | SETNAE
  | SETNB
  | SETNBE
  | SETNC
  | SETNE
  | SETNG
  | SETNGE
  | SETNL
  | SETNLE
  | SETNO
  | SETNP
  | SETNS
  | SETNZ
  | SETO
  | SETP
  | SETPE
  | SETPO
  | SETS
  | SETZ
  | SFENCE
  | SGDT
  | SHL
  | SHLD
  | SHR
  | SHRD
  | SHUFPS
  | SIDT
  | SLDT
  | SMSW
  | SQRTPD
  | SQRTPS
  | SQRTSD
  | SQRTSS
  | STC
  | STD
  | STI
  | STMXCSR
  | STOS
  | STOSD
  | STR
  | SUB
  | SUBPD
  | SUBPS
  | SUBSD
  | SUBSS
  | SWAPGS
  | SYSCALL
  | SYSENTER
  | SYSEXIT
  | SYSRET
  | TEST
  | UCOMISD
  | UCOMISS
  | UD2
  | UNPCKHPD
  | UNPCKHPS
  | UNPCKLPD
  | UNPCKLPS
  | VANDPD
  | VANDPS
  | VADDPD
  | VADDPS
  | VBLENDPS
  | VERR
  | VERW
  | VEXTRACTI128
  | VEXTRACTF128
  | VINSERTF128
  | VMCALL
  | VMCLEAR
  | VMLAUNCH
  | VMOVAPD
  | VMOVAPS
  | VMOVHPS
  | VMOVD
  | VMOVDQA
  | VMOVDQU
  | VMOVLHPS
  | VMPTRLD
  | VMPTRST
  | VMREAD
  | VMRESUME
  | VMWRITE
  | VMULPD
  | VMULPS
  | VMXOFF
  | VMXON
  | VPALIGNR
  | VPAND
  | VPANDN
  | VPCMPEQB
  | VPCMPEQW
  | VPERM2F128
  | VPERM2I128
  | VPERMILPS
  | VPOR
  | VPSHUFB
  | VPSHUFD
  | VPSLLW
  | VSHUFPS
  | VSHUFPD
  | VPXOR
  | VPUNPCKLWD
  | VPUNPCKHWD
  | VSUBPD
  | VSUBPS
  | VUNPCKHPS 
  | VUNPCKLPS 
  | VXORPD
  | VXORPS
  | VZEROUPPER
  | WAIT
  | WBINVD
  | WRFSBASE
  | WRGSBASE
  | WRMSR
  | XADD
  | XCHG
  | XLAT
  | XLATB
  | XSETBV
  | XSAVEOPT
  | XRSTOR
  | XOR
  | XORPD
  | XORPS
  deriving (Show, Eq, Read, Ord, Generic)

instance Cereal.Serialize Opcode


-- | Showing unresolved address (inner part within a ptr[...])
show_address' (FromReg r) = show r
show_address' (AddrImm i) = show i
show_address' (AddrMinus a0 a1) = show_address' a0 ++ " - " ++ show_address' a1
show_address' (AddrPlus a0 a1) = show_address' a0 ++ " + " ++ show_address' a1
show_address' (AddrTimes a0 a1) = show_address' a0 ++ " * " ++ show_address' a1
show_address' (SizeDir si a) = show_address' a

-- | Showing unresolved address
show_address (SizeDir si a) = show_size_directive si ++ " [" ++ show_address' a ++ "]"
show_address a = "[" ++ show_address' a ++ "]"

-- | Showing a size directive
show_size_directive 1  = "BYTE PTR"
show_size_directive 2  = "WORD PTR"
show_size_directive 4  = "DWORD PTR"
show_size_directive 8  = "QWORD PTR"
show_size_directive 16 = "XMMWORD PTR"
show_size_directive 32 = "YMMWORD PTR"
show_size_directive si = show (si*8) ++ " PTR"

-- | Showing an operand
show_operand' (Address a) = show_address a
show_operand' (Reg r) = show r
show_operand' (Immediate i) = show i

-- | Showing an optional operand
show_operand Nothing = ""
show_operand (Just op) = show op

-- | Showing an optional annotation
show_annot Nothing = ""
show_annot (Just s) = " <" ++ s ++ ">"

-- | Showing an optional prefix
show_prefix Nothing = ""
show_prefix (Just p) = show p ++ " "

-- | Showing an instruction
show_instruction (Instr addr pre opcode op1 op2 op3 annot size) =
     showHex addr ++ ": "
  ++ show_prefix pre
  ++ show opcode
  ++ " "
  ++ show_operand op1
  ++ show_operand2 op2
  ++ show_operand2 op3
  ++ show_annot annot
  ++ " " ++ show size
 where
  show_operand2 Nothing = ""
  show_operand2 (Just op) = ", " ++ show op

instance Show Address 
 where show = show_address'

instance Show Operand 
 where show = show_operand'

instance Show Instr
 where show = show_instruction



-- | The size of the operand, in bytes
operand_size :: Operand -> Int
operand_size (Reg r) = reg_size r
operand_size (Address (SizeDir si _)) = si 
operand_size (Immediate _) = 8

-- | Returns true iff m is the mnemonic of a conditional jump
is_cond_jump m = m `elem` [JO, JNO, JS, JNS, JE, JZ, JNE, JNZ, JB, JNAE, JC, JNB, JAE, JNC, JBE, JNA, JA, JNBE, JL, JNGE, JGE, JNL, JLE, JNG, JG, JNLE, JP, JPE, JNP, JPO, JCXZ, JECXZ, JRCXZ] 

-- | Returns true iff m is the mnemonic of a halting instruction
is_halt m = m `elem` [HLT]

-- | Returns true iff m is the mnemonic of a jump
is_jump m = m `elem` [JMP, JMPF, JMPN ] 

-- | Returns true iff m is the mnemonic of a call
is_call m = m `elem` [CALL, CALLF ]

-- | Returns true iff m is the mnemonic of a return
is_ret m = m `elem` [RET, RETF, RET, RETN, IRET, IRETD, IRETQ]


-- | List of 256 bit registers
reg256 = [YMM0,YMM1,YMM2,YMM3,YMM4,YMM5,YMM6,YMM7,YMM8,YMM9,YMM10,YMM11,YMM12,YMM13,YMM14,YMM15]
-- | List of 128 bit registers
reg128 = [XMM0,XMM1,XMM2,XMM3,XMM4,XMM5,XMM6,XMM7,XMM8,XMM9,XMM10,XMM11,XMM12,XMM13,XMM14,XMM15]
-- | List of 80 bit registers
reg80  = [ST0,ST1,ST2,ST3,ST4,ST5,ST6,ST7]
-- | List of 64 bit registers
reg64  = [RAX, RBX, RCX, RDX, RSI, RDI, RSP, RBP, R8, R9, R10, R11, R12, R13, R14, R15, RIP,CS,DS,ES,FS,GS,
          -- XMM0_L,XMM1_L,XMM2_L,XMM3_L,XMM4_L,XMM5_L,XMM6_L,XMM7_L,XMM8_L,XMM9_L,XMM10_L,XMM11_L,XMM12_L,XMM13_L,XMM14_L,XMM15_L,
          CS, DS, ES, FS, GS, SS ]
-- | List of 32 bit registers
reg32  = [EAX, EBX, ECX, EDX, ESI, EDI, ESP, EBP, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D]
-- | List of 16 bit registers
reg16  = [AX,BX,CX,DX,SI,DI,SP,BP,R8W,R9W,R10W,R11W,R12W,R13W,R14W,R15W]
-- | List of 8 bit registers
reg8   = [AL,BL,CL,DL,SIL,DIL,SPL,BPL,R8B,R9B,R10B,R11B,R12B,R13B,R14B,R15B]

-- | The size of the given register, in bytes.
reg_size r =
  if r `elem` reg256 then 32
  else if r `elem` reg128 then 16
  else if r `elem` reg80 then 10
  else if r `elem` reg64 then 8
  else if r `elem` reg32 then 4
  else if r `elem` reg16 then 2
  else if r `elem` reg8 ++ [AH,BH,CH,DH] then 1
  else error $ "Size of " ++ show r ++ " unknown"


-- | Matches register names to the real registers
-- E.g.: EAX is actually a part of RAX
real_reg EAX = RAX
real_reg EBX = RBX
real_reg ECX = RCX
real_reg EDX = RDX
real_reg ESI = RSI
real_reg EDI = RDI
real_reg ESP = RSP
real_reg EBP = RBP
real_reg R8D = R8
real_reg R9D = R9
real_reg R10D = R10
real_reg R11D = R11
real_reg R12D = R12
real_reg R13D = R13
real_reg R14D = R14
real_reg R15D = R15

real_reg AX = RAX
real_reg BX = RBX
real_reg CX = RCX
real_reg DX = RDX
real_reg SI = RSI
real_reg DI = RDI
real_reg SP = RSP
real_reg BP = RBP
real_reg R8W = R8
real_reg R9W = R9
real_reg R10W = R10
real_reg R11W = R11
real_reg R12W = R12
real_reg R13W = R13
real_reg R14W = R14
real_reg R15W = R15

real_reg AL = RAX
real_reg BL = RBX
real_reg CL = RCX
real_reg DL = RDX
real_reg SIL = RSI
real_reg DIL = RDI
real_reg SPL = RSP
real_reg BPL = RBP
real_reg R8B = R8
real_reg R9B = R9
real_reg R10B = R10
real_reg R11B = R11
real_reg R12B = R12
real_reg R13B = R13
real_reg R14B = R14
real_reg R15B = R15

real_reg XMM0 = YMM0
real_reg XMM1 = YMM1
real_reg XMM2 = YMM2
real_reg XMM3 = YMM3
real_reg XMM4 = YMM4
real_reg XMM5 = YMM5
real_reg XMM6 = YMM6
real_reg XMM7 = YMM7
real_reg XMM8 = YMM8
real_reg XMM9 = YMM9
real_reg XMM10 = YMM10
real_reg XMM11 = YMM11
real_reg XMM12 = YMM12
real_reg XMM13 = YMM13
real_reg XMM14 = YMM14
real_reg XMM15 = YMM15

real_reg ST0 = ST0
real_reg ST1 = ST1
real_reg ST2 = ST2
real_reg ST3 = ST3
real_reg ST4 = ST4
real_reg ST5 = ST5
real_reg ST6 = ST6
real_reg ST7 = ST7

real_reg AH = RAX
real_reg BH = RBX
real_reg CH = RCX
real_reg DH = RDX
real_reg r = if r `elem` (reg64 ++ reg256) then r else error $ "Cannot match register " ++ show r ++ " to real register"



