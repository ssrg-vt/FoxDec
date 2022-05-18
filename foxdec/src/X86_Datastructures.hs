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
import Generic_Datastructures
import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)
import X86.Register (Register)
import qualified X86.Register as Reg

-- | An x86 instruction
-- labels are integers, storages are registers, the annotation is the instruction size
type X86_Instruction = Instruction AddressWord64 Register Prefix Opcode Int
type X86_Operand     = GenericOperand Register
type X86_Address     = GenericAddress Register



instr_size :: X86_Instruction -> Int
instr_size i = instr_annot i `orElse` 0

instr_addr :: X86_Instruction -> Word64
instr_addr (Instruction (AddressWord64 a) _ _ _ _ _) = a





-- | Instruction prefixes
data Prefix = InvalidPrefix | REP | REPZ | REPNE | LOCK | BND
  deriving (Show,Eq,Ord,Read,Generic)

instance Cereal.Serialize Prefix

-- | Registers
-- | Flags
data Flag = ZF | CF | SF | OF | PF | InvalidFlag
  deriving (Show,Eq,Ord)





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
  | BLENDVPD
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
  | CMPSW
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
  | MOVSB
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
  | PINSRB
  | PINSRD
  | PINSRQ
  | PMADDWD
  | PMAXSD
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
  | STOSB
  | STOSD
  | STOSQ
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
  deriving (Show, Eq, Ord, Generic)


instance Cereal.Serialize Opcode





-- | The size of the operand, in bytes
operand_size :: X86_Operand -> Int
operand_size (Storage r)          = Reg.size r
operand_size (Memory _ si)        = si
operand_size (EffectiveAddress _) = 8
operand_size (Immediate _)        = 8

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
