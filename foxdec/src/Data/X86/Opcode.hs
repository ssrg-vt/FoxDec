{-# LANGUAGE DeriveGeneric #-}

module Data.X86.Opcode where

import           GHC.Generics (Generic)
import qualified Data.Serialize as Cereal
import           Control.DeepSeq


-- | Opcodes / mnemonics
data Opcode =
    InvalidOpcode String
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
  | CMPNLESD
  | CMPNLESS
  | CMPLTSD
  | CMPLTSS
  | CMPS
  | CMPSB
  | CMPSD
  | CMPSS
  | CMPSW
  | CMPXCHG
  | CMPXCHG16B
  | CMPXCHG8B
  | COMISD
  | COMISS
  | CPUID
  | CQO
  | CVTPI2PS
  | CVTTPS2PI
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
  | DIV_LO
  | DIV_HI
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
  | FDECSTP
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
  | FNSAVE
  | FNSTCW
  | FNSTENV
  | FPREM
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
  | FWAIT
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
  | IDIV_LO
  | IDIV_HI
  | IMUL
  | IMUL_LO
  | IMUL_HI
  | BSWAP
  | IN
  | INC
  | INS
  | INSB
  | INSD
  | INT
  | INT1
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
  | MOVSLHUP
  | MOVSHDUP
  | MOVSS
  | MOVSW
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
  | MUL_LO
  | MUL_HI
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
  | OUTSB
  | OUTSD
  | OUTSW
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
  | PBLENDW
  | PCLMULQDQ
  | PCMPEQB
  | PCMPEQD
  | PCMPGTB
  | PCMPGTD
  | PEXTRB
  | PEXTRD
  | PEXTRW
  | PEXTRQ
  | PHADDD
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
  | PREFETCH
  | PREFETCHNTA
  | PREFETCHT0
  | PREFETCHT1
  | PREFETCHT2
  | PSADBW
  | PSHUFB
  | PSHUFD
  | PSHUFW
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
  | PUNPCKHBW
  | PUNPCKHWD
  | PUNPCKLBW
  | PUNPCKLWD
  | PUNPCKLDQ
  | PUNPCKLQDQ
  | PUNPCKHDQ
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
  | SHUFPD
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
  | STOSW
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
  | UD0
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
  | VCMPPD
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
  | VMOVSS
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
  | VPSHUFW
  | VPSLLW
  | VPSRLW
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
  | XABORT
  | XADD
  | XCHG
  | XGETBV
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
instance NFData Opcode

-- | Returns true iff m is the mnemonic of a conditional jump
isCondJump :: Opcode -> Bool
isCondJump m = m
  `elem` [ JO
         , JNO
         , JS
         , JNS
         , JE
         , JZ
         , JNE
         , JNZ
         , JB
         , JNAE
         , JC
         , JNB
         , JAE
         , JNC
         , JBE
         , JNA
         , JA
         , JNBE
         , JL
         , JNGE
         , JGE
         , JNL
         , JLE
         , JNG
         , JG
         , JNLE
         , JP
         , JPE
         , JNP
         , JPO
         , JCXZ
         , JECXZ
         , JRCXZ
         , LOOP
         , LOOPE
         , LOOPNE ]

-- | Returns true iff m is the mnemonic of a halting instruction
isHalt :: Opcode -> Bool
isHalt m = m `elem` [HLT, UD0, UD2]

-- | Returns true iff m is the mnemonic of a jump
isJump :: Opcode -> Bool
isJump m = m `elem` [JMP, JMPF, JMPN]

-- | Returns true iff m is the mnemonic of a call
isCall :: Opcode -> Bool
isCall m = m `elem` [CALL, CALLF]

-- | Returns true iff m is the mnemonic of a return
isRet :: Opcode -> Bool
isRet m = m `elem` [RET, RETF, RET, RETN, IRET, IRETD, IRETQ]



read_opcode "AAA" = AAA
read_opcode "AAD" = AAD
read_opcode "AAM" = AAM
read_opcode "AAS" = AAS
read_opcode "ADC" = ADC
read_opcode "ADD" = ADD
read_opcode "ADDPD" = ADDPD
read_opcode "ADDPS" = ADDPS
read_opcode "ADDSD" = ADDSD
read_opcode "ADDSS" = ADDSS
read_opcode "ADDSUBPD" = ADDSUBPD
read_opcode "ADDUBPS" = ADDUBPS
read_opcode "AND" = AND
read_opcode "ANDNPD" = ANDNPD
read_opcode "ANDNPS" = ANDNPS
read_opcode "ANDPD" = ANDPD
read_opcode "ANDPS" = ANDPS
read_opcode "ARPL" = ARPL
read_opcode "BLENDVPD" = BLENDVPD
read_opcode "BLENDVPS" = BLENDVPS
read_opcode "BOUND" = BOUND
read_opcode "BSF" = BSF
read_opcode "BSR" = BSR
read_opcode "BT" = BT
read_opcode "BTC" = BTC
read_opcode "BTR" = BTR
read_opcode "BTS" = BTS
read_opcode "CALL" = CALL
read_opcode "CALLF" = CALLF
read_opcode "CBW" = CBW
read_opcode "CDQ" = CDQ
read_opcode "CDQE" = CDQE
read_opcode "CLC" = CLC
read_opcode "CLD" = CLD
read_opcode "CLFLUSH" = CLFLUSH
read_opcode "CLI" = CLI
read_opcode "CLTS" = CLTS
read_opcode "CMC" = CMC
read_opcode "CMOVA" = CMOVA
read_opcode "CMOVAE" = CMOVAE
read_opcode "CMOVB" = CMOVB
read_opcode "CMOVBE" = CMOVBE
read_opcode "CMOVC" = CMOVC
read_opcode "CMOVE" = CMOVE
read_opcode "CMOVG" = CMOVG
read_opcode "CMOVGE" = CMOVGE
read_opcode "CMOVL" = CMOVL
read_opcode "CMOVLE" = CMOVLE
read_opcode "CMOVNA" = CMOVNA
read_opcode "CMOVNAE" = CMOVNAE
read_opcode "CMOVNB" = CMOVNB
read_opcode "CMOVNBE" = CMOVNBE
read_opcode "CMOVNC" = CMOVNC
read_opcode "CMOVNE" = CMOVNE
read_opcode "CMOVNG" = CMOVNG
read_opcode "CMOVNGE" = CMOVNGE
read_opcode "CMOVNL" = CMOVNL
read_opcode "CMOVNLE" = CMOVNLE
read_opcode "CMOVNO" = CMOVNO
read_opcode "CMOVNP" = CMOVNP
read_opcode "CMOVNS" = CMOVNS
read_opcode "CMOVNZ" = CMOVNZ
read_opcode "CMOVO" = CMOVO
read_opcode "CMOVP" = CMOVP
read_opcode "CMOVPE" = CMOVPE
read_opcode "CMOVPO" = CMOVPO
read_opcode "CMOVS" = CMOVS
read_opcode "CMOVZ" = CMOVZ
read_opcode "CMP" = CMP
read_opcode "CMPEQSD" = CMPEQSD
read_opcode "CMPNEQSD" = CMPNEQSD
read_opcode "CMPNLESD" = CMPNLESD
read_opcode "CMPNLESS" = CMPNLESS
read_opcode "CMPLTSD" = CMPLTSD
read_opcode "CMPLTSS" = CMPLTSS
read_opcode "CMPS" = CMPS
read_opcode "CMPSB" = CMPSB
read_opcode "CMPSD" = CMPSD
read_opcode "CMPSS" = CMPSS
read_opcode "CMPSW" = CMPSW
read_opcode "CMPXCHG" = CMPXCHG
read_opcode "CMPXCHG16B" = CMPXCHG16B
read_opcode "CMPXCHG8B" = CMPXCHG8B
read_opcode "COMISD" = COMISD
read_opcode "COMISS" = COMISS
read_opcode "CPUID" = CPUID
read_opcode "CQO" = CQO
read_opcode "CVTPI2PS" = CVTPI2PS
read_opcode "CVTTPS2PI" = CVTTPS2PI
read_opcode "CVTDQ2PD" = CVTDQ2PD
read_opcode "CVTSD2SS" = CVTSD2SS
read_opcode "CVTSI2SD" = CVTSI2SD
read_opcode "CVTSI2SS" = CVTSI2SS
read_opcode "CVTSS2SD" = CVTSS2SD
read_opcode "CVTTSD2SI" = CVTTSD2SI
read_opcode "CVTTSS2SI" = CVTTSS2SI
read_opcode "CVTTPD2DQ" = CVTTPD2DQ
read_opcode "CWD" = CWD
read_opcode "CWDE" = CWDE
read_opcode "DAA" = DAA
read_opcode "DAS" = DAS
read_opcode "DEC" = DEC
read_opcode "DIV" = DIV
read_opcode "DIVPD" = DIVPD
read_opcode "DIVPS" = DIVPS
read_opcode "DIVSD" = DIVSD
read_opcode "DIVSS" = DIVSS
read_opcode "EMMS" = EMMS
read_opcode "ENDBR64" = ENDBR64
read_opcode "ENTER" = ENTER
read_opcode "EXTRACTPS" = EXTRACTPS
read_opcode "FABS" = FABS
read_opcode "FADD" = FADD
read_opcode "FADDP" = FADDP
read_opcode "FBLD" = FBLD
read_opcode "FBSTP" = FBSTP
read_opcode "FCHS" = FCHS
read_opcode "FCLEX" = FCLEX
read_opcode "FCMOVB" = FCMOVB
read_opcode "FCMOVBE" = FCMOVBE
read_opcode "FCMOVE" = FCMOVE
read_opcode "FCMOVNB" = FCMOVNB
read_opcode "FCMOVNBE" = FCMOVNBE
read_opcode "FCMOVNE" = FCMOVNE
read_opcode "FCMOVNU" = FCMOVNU
read_opcode "FCMOVU" = FCMOVU
read_opcode "FCOM" = FCOM
read_opcode "FCOMI" = FCOMI
read_opcode "FCOMIP" = FCOMIP
read_opcode "FCOMP" = FCOMP
read_opcode "FCOMPI" = FCOMPI
read_opcode "FCOMPP" = FCOMPP
read_opcode "FCOS" = FCOS
read_opcode "FDECSTP" = FDECSTP
read_opcode "FDIV" = FDIV
read_opcode "FDIVP" = FDIVP
read_opcode "FDIVR" = FDIVR
read_opcode "FDIVRP" = FDIVRP
read_opcode "FFREE" = FFREE
read_opcode "FRNDINT" = FRNDINT
read_opcode "FIADD" = FIADD
read_opcode "FICOM" = FICOM
read_opcode "FICOMP" = FICOMP
read_opcode "FIDIV" = FIDIV
read_opcode "FIDIVR" = FIDIVR
read_opcode "FILD" = FILD
read_opcode "FIMUL" = FIMUL
read_opcode "FINIT" = FINIT
read_opcode "FIST" = FIST
read_opcode "FISTP" = FISTP
read_opcode "FISTPP" = FISTPP
read_opcode "FISTTP" = FISTTP
read_opcode "FISUB" = FISUB
read_opcode "FISUBR" = FISUBR
read_opcode "FLD" = FLD
read_opcode "FLD1" = FLD1
read_opcode "FLDCW" = FLDCW
read_opcode "FLDENV" = FLDENV
read_opcode "FLDL2E" = FLDL2E
read_opcode "FLDL2T" = FLDL2T
read_opcode "FLDLG2" = FLDLG2
read_opcode "FLDLN2" = FLDLN2
read_opcode "FLDPI" = FLDPI
read_opcode "FLDZ" = FLDZ
read_opcode "FMUL" = FMUL
read_opcode "FMULP" = FMULP
read_opcode "FNOP" = FNOP
read_opcode "FNSAVE" = FNSAVE
read_opcode "FNINIT" = FNINIT
read_opcode "FNSTCW" = FNSTCW
read_opcode "FNSTENV" = FNSTENV
read_opcode "FPREM" = FPREM
read_opcode "FPREM1" = FPREM1
read_opcode "FRSTOR" = FRSTOR
read_opcode "FSAVE" = FSAVE
read_opcode "FSIN" = FSIN
read_opcode "FSINCOS" = FSINCOS
read_opcode "FSCALE" = FSCALE
read_opcode "FSQRT" = FSQRT
read_opcode "FST" = FST
read_opcode "FSTCW" = FSTCW
read_opcode "FSTENV" = FSTENV
read_opcode "FSTP" = FSTP
read_opcode "FSTSW" = FSTSW
read_opcode "FSUB" = FSUB
read_opcode "FSUBP" = FSUBP
read_opcode "FSUBR" = FSUBR
read_opcode "FSUBRP" = FSUBRP
read_opcode "FTST" = FTST
read_opcode "FUCOM" = FUCOM
read_opcode "FUCOMI" = FUCOMI
read_opcode "FUCOMIP" = FUCOMIP
read_opcode "FUCOMP" = FUCOMP
read_opcode "FUCOMPI" = FUCOMPI
read_opcode "FUCOMPP" = FUCOMPP
read_opcode "FXAM" = FXAM
read_opcode "FWAIT" = FWAIT
read_opcode "FXCH" = FXCH
read_opcode "FXRSTOR" = FXRSTOR
read_opcode "FXSAVE" = FXSAVE
read_opcode "FXTRACT" = FXTRACT
read_opcode "HADDPD" = HADDPD
read_opcode "HADDPS" = HADDPS
read_opcode "HLT" = HLT
read_opcode "HSUBPD" = HSUBPD
read_opcode "HSUBPS" = HSUBPS
read_opcode "IDIV" = IDIV
read_opcode "IMUL" = IMUL
read_opcode "BSWAP" = BSWAP
read_opcode "IN" = IN
read_opcode "INC" = INC
read_opcode "INS" = INS
read_opcode "INSB" = INSB
read_opcode "INSD" = INSD
read_opcode "INT" = INT
read_opcode "INT1" = INT1
read_opcode "INT3" = INT3
read_opcode "INTO" = INTO
read_opcode "INVD" = INVD
read_opcode "INVLPG" = INVLPG
read_opcode "INVPCID" = INVPCID
read_opcode "IRET" = IRET
read_opcode "IRETD" = IRETD
read_opcode "IRETQ" = IRETQ
read_opcode "JA" = JA
read_opcode "JAE" = JAE
read_opcode "JB" = JB
read_opcode "JBE" = JBE
read_opcode "JC" = JC
read_opcode "JCXZ" = JCXZ
read_opcode "JE" = JE
read_opcode "JECXZ" = JECXZ
read_opcode "JG" = JG
read_opcode "JGE" = JGE
read_opcode "JL" = JL
read_opcode "JLE" = JLE
read_opcode "JMP" = JMP
read_opcode "JMPF" = JMPF
read_opcode "JMPN" = JMPN
read_opcode "JNAE" = JNAE
read_opcode "JNA" = JNA
read_opcode "JNB" = JNB
read_opcode "JNBE" = JNBE
read_opcode "JNC" = JNC
read_opcode "JNG" = JNG
read_opcode "JNE" = JNE
read_opcode "JNGE" = JNGE
read_opcode "JNLE" = JNLE
read_opcode "JNL" = JNL
read_opcode "JNO" = JNO
read_opcode "JNP" = JNP
read_opcode "JNS" = JNS
read_opcode "JNZ" = JNZ
read_opcode "JO" = JO
read_opcode "JP" = JP
read_opcode "JPE" = JPE
read_opcode "JPO" = JPO
read_opcode "JRCXZ" = JRCXZ
read_opcode "JS" = JS
read_opcode "JZ" = JZ
read_opcode "LAHF" = LAHF
read_opcode "LAR" = LAR
read_opcode "LDDQU" = LDDQU
read_opcode "LDMXCSR" = LDMXCSR
read_opcode "LDS" = LDS
read_opcode "LEA" = LEA
read_opcode "LEAVE" = LEAVE
read_opcode "LES" = LES
read_opcode "LFENCE" = LFENCE
read_opcode "LFS" = LFS
read_opcode "LGDT" = LGDT
read_opcode "LGS" = LGS
read_opcode "LIDT" = LIDT
read_opcode "LLDT" = LLDT
read_opcode "LMSW" = LMSW
read_opcode "LODS" = LODS
read_opcode "LODSB" = LODSB
read_opcode "LODSD" = LODSD
read_opcode "LODSW" = LODSW
read_opcode "LOOP" = LOOP
read_opcode "LOOPE" = LOOPE
read_opcode "LOOPNE" = LOOPNE
read_opcode "LSL" = LSL
read_opcode "LSS" = LSS
read_opcode "LTR" = LTR
read_opcode "MASKMOVQ" = MASKMOVQ
read_opcode "MAXPD" = MAXPD
read_opcode "MAXPS" = MAXPS
read_opcode "MAXSD" = MAXSD
read_opcode "MAXSS" = MAXSS
read_opcode "MFENCE" = MFENCE
read_opcode "MINPD" = MINPD
read_opcode "MINPS" = MINPS
read_opcode "MINSD" = MINSD
read_opcode "MINSS" = MINSS
read_opcode "MONITOR" = MONITOR
read_opcode "MOV" = MOV
read_opcode "MOVABS" = MOVABS
read_opcode "MOVAPD" = MOVAPD
read_opcode "MOVAPS" = MOVAPS
read_opcode "MOVD" = MOVD
read_opcode "MOVDDUP" = MOVDDUP
read_opcode "MOVDQA" = MOVDQA
read_opcode "MOVDQU" = MOVDQU
read_opcode "MOVHLPS" = MOVHLPS
read_opcode "MOVHPD" = MOVHPD
read_opcode "MOVSHDUP" = MOVSHDUP
read_opcode "MOVHPS" = MOVHPS
read_opcode "MOVLHPS" = MOVLHPS
read_opcode "MOVLPD" = MOVLPD
read_opcode "MOVLPS" = MOVLPS
read_opcode "MOVLSDUP" = MOVLSDUP
read_opcode "MOVMSKPD" = MOVMSKPD
read_opcode "MOVMSKPS" = MOVMSKPS
read_opcode "MOVNTDQ" = MOVNTDQ
read_opcode "MOVNTPD" = MOVNTPD
read_opcode "MOVNTPS" = MOVNTPS
read_opcode "MOVNTQ" = MOVNTQ
read_opcode "MOVQ" = MOVQ
read_opcode "MOVS" = MOVS
read_opcode "MOVSB" = MOVSB
read_opcode "MOVSD" = MOVSD
read_opcode "MOVSW" = MOVSW
read_opcode "MOVSLDUP" = MOVSLDUP
read_opcode "MOVSLHUP" = MOVSLHUP
read_opcode "MOVSS" = MOVSS
read_opcode "MOVSQ" = MOVSQ
read_opcode "MOVSX" = MOVSX
read_opcode "MOVSXB" = MOVSXB
read_opcode "MOVSXD" = MOVSXD
read_opcode "MOVSXW" = MOVSXW
read_opcode "MOVUPD" = MOVUPD
read_opcode "MOVUPS" = MOVUPS
read_opcode "MOVZX" = MOVZX
read_opcode "MOVZXB" = MOVZXB
read_opcode "MOVZXW" = MOVZXW
read_opcode "MUL" = MUL
read_opcode "MULPD" = MULPD
read_opcode "MULPS" = MULPS
read_opcode "MULSD" = MULSD
read_opcode "MULSS" = MULSS
read_opcode "MWAIT" = MWAIT
read_opcode "NEG" = NEG
read_opcode "NOP" = NOP
read_opcode "NOT" = NOT
read_opcode "OR" = OR
read_opcode "ORPD" = ORPD
read_opcode "ORPS" = ORPS
read_opcode "OUT" = OUT
read_opcode "OUTS" = OUTS
read_opcode "OUTSB" = OUTSB
read_opcode "OUTSD" = OUTSD
read_opcode "OUTSW" = OUTSW
read_opcode "PALIGNR" = PALIGNR
read_opcode "PACKSSDW" = PACKSSDW
read_opcode "PACKSSWB" = PACKSSWB
read_opcode "PADDB" = PADDB
read_opcode "PADDD" = PADDD
read_opcode "PADDQ" = PADDQ
read_opcode "PADDSB" = PADDSB
read_opcode "PADDSW" = PADDSW
read_opcode "PADDUSB" = PADDUSB
read_opcode "PADDUSW" = PADDUSW
read_opcode "PADDW" = PADDW
read_opcode "PAND" = PAND
read_opcode "PANDN" = PANDN
read_opcode "PAUSE" = PAUSE
read_opcode "PAVGB" = PAVGB
read_opcode "PAVGW" = PAVGW
read_opcode "PBLENDW" = PBLENDW
read_opcode "PCLMULQDQ" = PCLMULQDQ
read_opcode "PCMPEQB" = PCMPEQB
read_opcode "PCMPEQD" = PCMPEQD
read_opcode "PCMPGTB" = PCMPGTB
read_opcode "PCMPGTD" = PCMPGTD
read_opcode "PEXTRB" = PEXTRB
read_opcode "PEXTRD" = PEXTRD
read_opcode "PEXTRW" = PEXTRW
read_opcode "PEXTRQ" = PEXTRQ
read_opcode "PHADDD" = PHADDD
read_opcode "PINSRB" = PINSRB
read_opcode "PINSRD" = PINSRD
read_opcode "PINSRQ" = PINSRQ
read_opcode "PMADDWD" = PMADDWD
read_opcode "PMAXSD" = PMAXSD
read_opcode "PMAXSW" = PMAXSW
read_opcode "PMAXUB" = PMAXUB
read_opcode "PMAXUD" = PMAXUD
read_opcode "PMAXUQ" = PMAXUQ
read_opcode "PMINSD" = PMINSD
read_opcode "PMINSW" = PMINSW
read_opcode "PMINUB" = PMINUB
read_opcode "PMINUD" = PMINUD
read_opcode "PMOVMSKB" = PMOVMSKB
read_opcode "PMOVSXDQ" = PMOVSXDQ
read_opcode "PMOVZXDQ" = PMOVZXDQ
read_opcode "PMOVSXBD" = PMOVSXBD
read_opcode "PMOVZXBD" = PMOVZXBD
read_opcode "PMULLD" = PMULLD
read_opcode "PMULLQ" = PMULLQ
read_opcode "PMULHUW" = PMULHUW
read_opcode "PMULHW" = PMULHW
read_opcode "PMULLW" = PMULLW
read_opcode "PMULUDQ" = PMULUDQ
read_opcode "POP" = POP
read_opcode "POPA" = POPA
read_opcode "POPAD" = POPAD
read_opcode "POPF" = POPF
read_opcode "POPFD" = POPFD
read_opcode "POPFQ" = POPFQ
read_opcode "POR" = POR
read_opcode "PREFETCH" = PREFETCH
read_opcode "PREFETCHNTA" = PREFETCHNTA
read_opcode "PREFETCHT0" = PREFETCHT0
read_opcode "PREFETCHT1" = PREFETCHT1
read_opcode "PREFETCHT2" = PREFETCHT2
read_opcode "PSADBW" = PSADBW
read_opcode "PSHUFB" = PSHUFB
read_opcode "PSHUFD" = PSHUFD
read_opcode "PSHUFW" = PSHUFW
read_opcode "PSHUFLW" = PSHUFLW
read_opcode "PSLLD" = PSLLD
read_opcode "PSLLDQ" = PSLLDQ
read_opcode "PSLLQ" = PSLLQ
read_opcode "PSLLW" = PSLLW
read_opcode "PSRAD" = PSRAD
read_opcode "PSRAW" = PSRAW
read_opcode "PSRLD" = PSRLD
read_opcode "PSRLDQ" = PSRLDQ
read_opcode "PSRLQ" = PSRLQ
read_opcode "PSRLW" = PSRLW
read_opcode "PSUBB" = PSUBB
read_opcode "PSUBD" = PSUBD
read_opcode "PSUBQ" = PSUBQ
read_opcode "PSUBSB" = PSUBSB
read_opcode "PSUBSQ" = PSUBSQ
read_opcode "PSUBUSB" = PSUBUSB
read_opcode "PSUBUSW" = PSUBUSW
read_opcode "PSUBW" = PSUBW
read_opcode "PTEST" = PTEST
read_opcode "PUNPCKHBW" = PUNPCKHBW
read_opcode "PUNPCKHWD" = PUNPCKHWD
read_opcode "PUNPCKLBW" = PUNPCKLBW
read_opcode "PUNPCKLWD" = PUNPCKLWD
read_opcode "PUNPCKLDQ" = PUNPCKLDQ
read_opcode "PUNPCKLQDQ" = PUNPCKLQDQ
read_opcode "PUNPCKHDQ" = PUNPCKHDQ
read_opcode "PUSH" = PUSH
read_opcode "PUSHA" = PUSHA
read_opcode "PUSHAD" = PUSHAD
read_opcode "PUSHF" = PUSHF
read_opcode "PUSHFD" = PUSHFD
read_opcode "PUSHFQ" = PUSHFQ
read_opcode "PXOR" = PXOR
read_opcode "RCL" = RCL
read_opcode "RCPPS" = RCPPS
read_opcode "RCPSS" = RCPSS
read_opcode "RCR" = RCR
read_opcode "RDMSR" = RDMSR
read_opcode "RDPMC" = RDPMC
read_opcode "RDTSC" = RDTSC
read_opcode "RET" = RET
read_opcode "RETF" = RETF
read_opcode "RETN" = RETN
read_opcode "ROL" = ROL
read_opcode "ROR" = ROR
read_opcode "ROUNDSD" = ROUNDSD
read_opcode "ROUNDSS" = ROUNDSS
read_opcode "RSM" = RSM
read_opcode "RSQRTPS" = RSQRTPS
read_opcode "RSQRTSS" = RSQRTSS
read_opcode "SAHF" = SAHF
read_opcode "SAL" = SAL
read_opcode "SAR" = SAR
read_opcode "SBB" = SBB
read_opcode "SCAS" = SCAS
read_opcode "SCASB" = SCASB
read_opcode "SCASD" = SCASD
read_opcode "SETA" = SETA
read_opcode "SETAE" = SETAE
read_opcode "SETB" = SETB
read_opcode "SETBE" = SETBE
read_opcode "SETC" = SETC
read_opcode "SETE" = SETE
read_opcode "SETG" = SETG
read_opcode "SETGE" = SETGE
read_opcode "SETL" = SETL
read_opcode "SETLE" = SETLE
read_opcode "SETNA" = SETNA
read_opcode "SETNAE" = SETNAE
read_opcode "SETNB" = SETNB
read_opcode "SETNBE" = SETNBE
read_opcode "SETNC" = SETNC
read_opcode "SETNE" = SETNE
read_opcode "SETNG" = SETNG
read_opcode "SETNGE" = SETNGE
read_opcode "SETNL" = SETNL
read_opcode "SETNLE" = SETNLE
read_opcode "SETNO" = SETNO
read_opcode "SETNP" = SETNP
read_opcode "SETNS" = SETNS
read_opcode "SETNZ" = SETNZ
read_opcode "SETO" = SETO
read_opcode "SETP" = SETP
read_opcode "SETPE" = SETPE
read_opcode "SETPO" = SETPO
read_opcode "SETS" = SETS
read_opcode "SETZ" = SETZ
read_opcode "SFENCE" = SFENCE
read_opcode "SGDT" = SGDT
read_opcode "SHL" = SHL
read_opcode "SHLD" = SHLD
read_opcode "SHR" = SHR
read_opcode "SHRD" = SHRD
read_opcode "SHUFPD" = SHUFPD
read_opcode "SHUFPS" = SHUFPS
read_opcode "SIDT" = SIDT
read_opcode "SLDT" = SLDT
read_opcode "SMSW" = SMSW
read_opcode "SQRTPD" = SQRTPD
read_opcode "SQRTPS" = SQRTPS
read_opcode "SQRTSD" = SQRTSD
read_opcode "SQRTSS" = SQRTSS
read_opcode "STC" = STC
read_opcode "STD" = STD
read_opcode "STI" = STI
read_opcode "STMXCSR" = STMXCSR
read_opcode "STOS" = STOS
read_opcode "STOSB" = STOSB
read_opcode "STOSW" = STOSW
read_opcode "STOSD" = STOSD
read_opcode "STOSQ" = STOSQ
read_opcode "STR" = STR
read_opcode "SUB" = SUB
read_opcode "SUBPD" = SUBPD
read_opcode "SUBPS" = SUBPS
read_opcode "SUBSD" = SUBSD
read_opcode "SUBSS" = SUBSS
read_opcode "SWAPGS" = SWAPGS
read_opcode "SYSCALL" = SYSCALL
read_opcode "SYSENTER" = SYSENTER
read_opcode "SYSEXIT" = SYSEXIT
read_opcode "SYSRET" = SYSRET
read_opcode "TEST" = TEST
read_opcode "UCOMISD" = UCOMISD
read_opcode "UCOMISS" = UCOMISS
read_opcode "UD0" = UD0
read_opcode "UD2" = UD2
read_opcode "UNPCKHPD" = UNPCKHPD
read_opcode "UNPCKHPS" = UNPCKHPS
read_opcode "UNPCKLPD" = UNPCKLPD
read_opcode "UNPCKLPS" = UNPCKLPS
read_opcode "VANDPD" = VANDPD
read_opcode "VANDPS" = VANDPS
read_opcode "VADDPD" = VADDPD
read_opcode "VADDPS" = VADDPS
read_opcode "VBLENDPS" = VBLENDPS
read_opcode "VCMPPD" = VCMPPD
read_opcode "VERR" = VERR
read_opcode "VERW" = VERW
read_opcode "VEXTRACTI128" = VEXTRACTI128
read_opcode "VEXTRACTF128" = VEXTRACTF128
read_opcode "VINSERTF128" = VINSERTF128
read_opcode "VMCALL" = VMCALL
read_opcode "VMCLEAR" = VMCLEAR
read_opcode "VMLAUNCH" = VMLAUNCH
read_opcode "VMOVAPD" = VMOVAPD
read_opcode "VMOVAPS" = VMOVAPS
read_opcode "VMOVHPS" = VMOVHPS
read_opcode "VMOVD" = VMOVD
read_opcode "VMOVDQA" = VMOVDQA
read_opcode "VMOVDQU" = VMOVDQU
read_opcode "VMOVLHPS" = VMOVLHPS
read_opcode "VMOVSS" = VMOVSS
read_opcode "VMPTRLD" = VMPTRLD
read_opcode "VMPTRST" = VMPTRST
read_opcode "VMREAD" = VMREAD
read_opcode "VMRESUME" = VMRESUME
read_opcode "VMWRITE" = VMWRITE
read_opcode "VMULPD" = VMULPD
read_opcode "VMULPS" = VMULPS
read_opcode "VMXOFF" = VMXOFF
read_opcode "VMXON" = VMXON
read_opcode "VPALIGNR" = VPALIGNR
read_opcode "VPAND" = VPAND
read_opcode "VPANDN" = VPANDN
read_opcode "VPCMPEQB" = VPCMPEQB
read_opcode "VPCMPEQW" = VPCMPEQW
read_opcode "VPERM2F128" = VPERM2F128
read_opcode "VPERM2I128" = VPERM2I128
read_opcode "VPERMILPS" = VPERMILPS
read_opcode "VPOR" = VPOR
read_opcode "VPSHUFB" = VPSHUFB
read_opcode "VPSHUFD" = VPSHUFD
read_opcode "VPSHUFW" = VPSHUFW
read_opcode "VPSLLW" = VPSLLW
read_opcode "VPSRLW" = VPSRLW
read_opcode "VSHUFPS" = VSHUFPS
read_opcode "VSHUFPD" = VSHUFPD
read_opcode "VPXOR" = VPXOR
read_opcode "VPUNPCKLWD" = VPUNPCKLWD
read_opcode "VPUNPCKHWD" = VPUNPCKHWD
read_opcode "VSUBPD" = VSUBPD
read_opcode "VSUBPS" = VSUBPS
read_opcode "VUNPCKHPS" = VUNPCKHPS
read_opcode "VUNPCKLPS" = VUNPCKLPS
read_opcode "VXORPD" = VXORPD
read_opcode "VXORPS" = VXORPS
read_opcode "VZEROUPPER" = VZEROUPPER
read_opcode "WAIT" = WAIT
read_opcode "WBINVD" = WBINVD
read_opcode "WRFSBASE" = WRFSBASE
read_opcode "WRGSBASE" = WRGSBASE
read_opcode "WRMSR" = WRMSR
read_opcode "XABORT" = XABORT
read_opcode "XADD" = XADD
read_opcode "XCHG" = XCHG
read_opcode "XGETBV" = XGETBV
read_opcode "XLAT" = XLAT
read_opcode "XLATB" = XLATB
read_opcode "XSETBV" = XSETBV
read_opcode "XSAVEOPT" = XSAVEOPT
read_opcode "XRSTOR" = XRSTOR
read_opcode "XOR" = XOR
read_opcode "XORPD" = XORPD
read_opcode "XORPS" = XORPS
read_opcode op = InvalidOpcode op -- error $ "UNKOWN OPCODE: " ++ op -- InvalidOpcode
