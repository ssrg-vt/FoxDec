{-# OPTIONS_HADDOCK hide #-}

-- Parser that can be used to read the output of objdump applied to X86 binaries
--
-- UBUNTU:
--    objdump -M intel -M hex --no-show-raw-insn -d #DIR#/#BINARY#
-- Tested with GNU objdump (GNU Binutils for Ubuntu) 2.26.1
--
-- MACOS:
--    objdump -no-show-raw-insn -disassemble -x86-asm-syntax=intel -print-imm-hex #DIR#/#BINARY#
--  Tested with Apple LLVM version 11.0.3 (clang-1103.0.32.59)
--
-- Example input:
--
-- 100002f85:	mov	rax, qword ptr [rip + 0x1a5084] ## literal pool symbol address: ___stack_chk_guard
-- 100002f9a:	jne	0x100003001
-- 100002fb1:	call	0x1001455d8

module Parser.ParserX86Instruction where

import Generic_Datastructures
import X86_Datastructures
import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Text.Parsec.Expr
import Data.Char
import Debug.Trace
import Data.Maybe 
import Data.List
import Data.Word (Word64)
import Data.Bits (shiftL)
import qualified Data.IntMap as IM
import Debug.Trace
import Data.Functor ((<&>))
import System.Directory (doesFileExist)
import X86.Register (Register(..))


jumps_and_calls_are_relative = False -- TODO make configurable


isWhiteSpace '\t' = True
isWhiteSpace '\f' = True
isWhiteSpace '\v' = True
isWhiteSpace ' '  = True
isWhiteSpace _    = False 

whitespace  = satisfy isWhiteSpace <?> "white space"
whitespaces = skipMany whitespace <?> "white spaces"


hexToWord64 :: [Char] -> Word64
hexToWord64 = hexToWord64' 0 . reverse
 where
  hexToWord64' _ [] = 0
  hexToWord64' n (c:cs) = shiftL (fromIntegral (digitToInt c)) n + hexToWord64' (n+4) cs

{--
-- Opcodes / mnemonics
parseMnemonic :: [Char] -> Opcode
parseMnemonic s = 
  case readsPrec 5 $ map toUpper s of
    [(m,s')] -> m
    _ -> InvalidOpcode
--}
mnemonic = try (do
  m <- many1 alphaNum
  let m' = read_opcode $ map toUpper m 
  case m' of
    InvalidOpcode -> fail ("Invalid mnemonic: " ++ m)
    _ -> return $ m'
  )

-- Registers
{--
parseRegister :: [Char] -> Register
parseRegister s = 
  if take 3 s `elem` ["ST(", "st("] then
    case readsPrec 5 $ map toUpper $ [ c | c <- s, c `notElem` ['(',')']] of
      [(m,s')] -> m
      _ -> InvalidRegister
  else if map toUpper s == "ST" then
    ST0
  else
    case readsPrec 5 $ map toUpper s of
      [(m,s')] -> m
      _ -> InvalidRegister
--}
isRegisterChar c = isAlphaNum c || c `elem` "_lh()"

register =
  try (do
    m <- many1 (satisfy isRegisterChar)
    let m' = read_regname $ map toUpper m 
    case m' of
      InvalidRegister -> fail ("Invalid register: " ++ m)
      _ -> return $ m'
  )


--Prefixes
parsePrefix :: [Char] -> Prefix
parsePrefix "REPE" = REPZ
parsePrefix "repe" = REPZ
parsePrefix s = 
  case readsPrec 5 $ map toUpper s of
    [(m,s')] -> m
    _ -> InvalidPrefix

prefix =
  try (do
    m <- many1 alphaNum
    let m' = parsePrefix m 
    case m' of
      InvalidPrefix -> fail ("Invalid prefix: " ++ m)
      _ -> return $ m'
  )


-- Addresses
-- An unresolved address is parsed by op_address.
-- It first must be either a size directive ("qword ptr ..."), dereference ("[...]"), or a register offset "cs:[...]"
-- Then, it can be an expression with address_term as terms at the leaves.
-- Address terms are immediates or registers. Address operands are '+', '-' or '*'.
address_term =
 (register <&> AddressStorage)
 <|>
 (int <&>  AddressImm)

size_directive = 
      (try (string "ymmword ptr" >> return 32))
  <|> (try (string "xmmword ptr" >> return 16))
  <|> (string "tbyte ptr" >> return 10)
  <|> (string "xword ptr" >> return 10)
  <|> (string "qword ptr" >> return 8)
  <|> (string "dword ptr" >> return 4)
  <|> (string "word ptr" >> return 2)
  <|> (string "byte ptr" >> return 1)
  <|> (try (string "YMMWORD PTR" >> return 32))
  <|> (try (string "XMMWORD PTR" >> return 16))
  <|> (string "TBYTE PTR" >> return 10)
  <|> (string "XWORD PTR" >> return 10)
  <|> (string "QWORD PTR" >> return 8)
  <|> (string "DWORD PTR" >> return 4)
  <|> (string "WORD PTR" >> return 2)
  <|> (string "BYTE PTR" >> return 1)

addr_expr0 =
  (try (do
    char '('
    whitespaces
    a <- addr_expr0
    whitespaces
    char ')'
    return a
  ))
  <|>
  (try (do
    a0 <- addr_expr1
    whitespaces
    symbol <- char '+' <|> char '-'  <|> char ':'
    whitespaces
    a1 <- addr_expr0
    case symbol of
      '+' -> return $ AddressPlus a0 a1
      ':' -> return $ AddressPlus a0 a1
      '-' -> return $ AddressMinus a0 a1
  ))
  <|>
  addr_expr1
  <|>
  address_term

addr_expr1 =
  (try (do
    char '('
    whitespaces
    a <- addr_expr0
    whitespaces
    char ')'
    return a
  ))
  <|>
  (try (do
    a0 <- address_term
    whitespaces
    symbol <- char '*'
    whitespaces
    a1 <- addr_expr1
    return $ AddressTimes a0 a1
  ))
  <|>
  address_term

address_expr_inner =
  {--(try (do
    s <- size_directive
    whitespaces
    a <- address_expr_inner
    return $ SizeDir s a
  ))
  <|> --}
  (try (do
    char '['
    whitespaces
    a <- address_expr_inner
    whitespaces
    char ']'
    return a
  ))
  <|> addr_expr0

op_address = 
  (try (do
    whitespaces
    s <- size_directive
    whitespaces
    EffectiveAddress a <- op_address
    return $ Memory a s
  ))
  <|> (try (do
    whitespaces
    char '['
    whitespaces
    a <- address_expr_inner
    whitespaces
    char ']'
    return $ EffectiveAddress a
  )) 
  <|> (try (do
    whitespaces
    r <- register
    whitespaces
    char ':'
    whitespaces
    a <- address_expr_inner
    whitespaces
    return $ EffectiveAddress $ AddressPlus (AddressStorage r) a
  ))

-- Operands
op_reg = do
  register <&> Storage

op_immediate = try (do
  sign <- option "+" (string "-")
  i <- hexnum
  return $ Immediate (if sign == "-" then 0 - i else i)
 )

op_immediate_0x = try (do
  sign <- option "+" (string "-")
  string "0x"
  i <- hexnum
  return $ Immediate (if sign == "-" then 0 - i else i)
 )

operand = 
      op_immediate_0x
  <|> op_address
  <|> op_reg
  <|> op_immediate -- must be last to try

second_operand = do
  char ','
  whitespaces
  op2 <- operand
  return $ op2


-- Annotations, e.g., <malloc@plt + 10>
{--
annotation = do
  char '<'
  cs <- many (noneOf "@>\n+")
  many (noneOf ">\n")
  char '>'
  return cs

-- Comments
comment = do
  char '#'
  skipMany (noneOf "\n")
---}
  

hexnum_with_0x = do
  string "0x"
  hexnum

-- Instructions
instruction = do
  whitespaces
  addr <- hexnum
  whitespaces
  string ":"
  whitespaces
  p <- optionMaybe prefix
  whitespaces
  m <- mnemonic 
  whitespaces
  op1 <- optionMaybe operand
  whitespaces
  op2 <- optionMaybe second_operand
  whitespaces
  op3 <- optionMaybe second_operand
  skipMany (noneOf "\n")
  {--
  whitespaces
  annot <- optionMaybe annotation
  whitespaces
  skipMany comment
  whitespaces--}
  skipMany newline
  return $ Instruction (AddressWord64 addr) p m Nothing (catMaybes [op1,op2,op3]) Nothing





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
read_opcode "CMPLTSD" = CMPLTSD
read_opcode "CMPS" = CMPS
read_opcode "CMPSB" = CMPSB
read_opcode "CMPSD" = CMPSD
read_opcode "CMPSW" = CMPSW
read_opcode "CMPXCHG" = CMPXCHG
read_opcode "CMPXCHG16B" = CMPXCHG16B
read_opcode "CMPXCHG8B" = CMPXCHG8B
read_opcode "COMISD" = COMISD
read_opcode "COMISS" = COMISS
read_opcode "CPUID" = CPUID
read_opcode "CQO" = CQO
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
read_opcode "FNINIT" = FNINIT
read_opcode "FNSTCW" = FNSTCW
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
read_opcode "INSD" = INSD
read_opcode "INT" = INT
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
read_opcode "MOVSLDUP" = MOVSLDUP
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
read_opcode "PCLMULQDQ" = PCLMULQDQ
read_opcode "PCMPEQB" = PCMPEQB
read_opcode "PCMPEQD" = PCMPEQD
read_opcode "PCMPGTB" = PCMPGTB
read_opcode "PCMPGTD" = PCMPGTD
read_opcode "PEXTRB" = PEXTRB
read_opcode "PEXTRD" = PEXTRD
read_opcode "PEXTRQ" = PEXTRQ
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
read_opcode "PREFETCHNTA" = PREFETCHNTA
read_opcode "PREFETCHT0" = PREFETCHT0
read_opcode "PREFETCHT1" = PREFETCHT1
read_opcode "PREFETCHT2" = PREFETCHT2
read_opcode "PSADBW" = PSADBW
read_opcode "PSHUFB" = PSHUFB
read_opcode "PSHUFD" = PSHUFD
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
read_opcode "PUNPCKLBW" = PUNPCKLBW 
read_opcode "PUNPCKLWD" = PUNPCKLWD
read_opcode "PUNPCKLDQ" = PUNPCKLDQ 
read_opcode "PUNPCKLQDQ" = PUNPCKLQDQ
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
read_opcode "VPSLLW" = VPSLLW
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
read_opcode "XADD" = XADD
read_opcode "XCHG" = XCHG
read_opcode "XLAT" = XLAT
read_opcode "XLATB" = XLATB
read_opcode "XSETBV" = XSETBV
read_opcode "XSAVEOPT" = XSAVEOPT
read_opcode "XRSTOR" = XRSTOR
read_opcode "XOR" = XOR
read_opcode "XORPD" = XORPD
read_opcode "XORPS" = XORPS
read_opcode _ = InvalidOpcode


read_regname "RIP" = RIP 
read_regname "EIP" = EIP
read_regname "RAX" = RAX
read_regname "EAX" = EAX 
read_regname "AX" = AX 
read_regname "AH" = AH 
read_regname "AL" = AL 
read_regname "RBX" = RBX
read_regname "EBX" = EBX
read_regname "BX" = BX
read_regname "BH" = BH
read_regname "BL" = BL
read_regname "RCX" = RCX
read_regname "ECX" = ECX
read_regname "CX" = CX
read_regname "CH" = CH
read_regname "CL" = CL
read_regname "RDX" = RDX
read_regname "EDX" = EDX
read_regname "DX" = DX
read_regname "DH" = DH
read_regname "DL" = DL
read_regname "RDI" = RDI
read_regname "EDI" = EDI
read_regname "DI" = DI
read_regname "DIL" = DIL
read_regname "RSI" = RSI
read_regname "ESI" = ESI
read_regname "SI" = SI
read_regname "SIL" = SIL
read_regname "RSP" = RSP
read_regname "ESP" = ESP
read_regname "SP" = SP
read_regname "SPL" = SPL 
read_regname "RBP" = RBP
read_regname "EBP" = EBP
read_regname "BP" = BP
read_regname "BPL" = BPL 
read_regname "R15" = R15
read_regname "R15D" = R15D
read_regname "R15W" = R15W
read_regname "R15B" = R15B
read_regname "R14" = R14
read_regname "R14D" = R14D
read_regname "R14W" = R14W
read_regname "R14B" = R14B
read_regname "R13" = R13
read_regname "R13D" = R13D
read_regname "R13W" = R13W
read_regname "R13B" = R13B
read_regname "R12" = R12
read_regname "R12D" = R12D
read_regname "R12W" = R12W
read_regname "R12B" = R12B
read_regname "R11" = R11
read_regname "R11D" = R11D
read_regname "R11W" = R11W
read_regname "R11B" = R11B
read_regname "R10" = R10
read_regname "R10D" = R10D
read_regname "R10W" = R10W
read_regname "R10B" = R10B
read_regname "R9" = R9
read_regname "R9D" = R9D
read_regname "R9W" = R9W
read_regname "R9B" = R9B
read_regname "R8" = R8
read_regname "R8D" = R8D
read_regname "R8W" = R8W
read_regname "R8B" = R8B
read_regname "CS" = CS
read_regname "DS" = DS
read_regname "ES" = ES
read_regname "FS" = FS
read_regname "GS" = GS
read_regname "SS" = SS
read_regname "EIZ" = EIZ
read_regname "RIZ" = RIZ
read_regname "ST0" = ST0
read_regname "ST1" = ST1
read_regname "ST2" = ST2
read_regname "ST3" = ST3
read_regname "ST4" = ST4
read_regname "ST5" = ST5
read_regname "ST6" = ST6
read_regname "ST7" = ST7
read_regname "YMM0" = YMM0
read_regname "YMM1" = YMM1
read_regname "YMM2" = YMM2
read_regname "YMM3" = YMM3
read_regname "YMM4" = YMM4
read_regname "YMM5" = YMM5
read_regname "YMM6" = YMM6
read_regname "YMM7" = YMM7
read_regname "YMM8" = YMM8
read_regname "YMM9" = YMM9
read_regname "YMM10" = YMM10
read_regname "YMM11" = YMM11
read_regname "YMM12" = YMM12
read_regname "YMM13" = YMM13
read_regname "YMM14" = YMM14
read_regname "YMM15" = YMM15
read_regname "XMM0" = XMM0
read_regname "XMM1" = XMM1
read_regname "XMM2" = XMM2
read_regname "XMM3" = XMM3
read_regname "XMM4" = XMM4
read_regname "XMM5" = XMM5
read_regname "XMM6" = XMM6
read_regname "XMM7" = XMM7
read_regname "XMM8" = XMM8
read_regname "XMM9" = XMM9
read_regname "XMM10" = XMM10
read_regname "XMM11" = XMM11
read_regname "XMM12" = XMM12
read_regname "XMM13" = XMM13
read_regname "XMM14" = XMM14
read_regname "XMM15" = XMM15



read_regname "ST(0)" = ST0
read_regname "ST(1)" = ST1
read_regname "ST(2)" = ST2
read_regname "ST(3)" = ST3
read_regname "ST(4)" = ST4
read_regname "ST(5)" = ST5
read_regname "ST(6)" = ST6
read_regname "ST(7)" = ST7

read_regname _ = InvalidRegister
