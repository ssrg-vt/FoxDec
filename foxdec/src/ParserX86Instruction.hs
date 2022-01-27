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

module ParserX86Instruction where

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

-- Opcodes / mnemonics
parseMnemonic :: [Char] -> Opcode
parseMnemonic s = 
  case readsPrec 5 $ map toUpper s of
    [(m,s')] -> m
    _ -> InvalidOpcode

mnemonic = try (do
  m <- many1 alphaNum
  let m' = parseMnemonic m 
  case m' of
    InvalidOpcode -> fail ("Invalid mnemonic: " ++ m)
    _ -> return $ m'
  )


-- Registers
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

isRegisterChar c = isAlphaNum c || c `elem` "_lh()"

register =
  try (do
    m <- many1 (satisfy isRegisterChar)
    let m' = parseRegister m 
    case m' of
      InvalidRegister -> fail ("Invalid register: " ++ m)
      _ -> return $ m'
  )


--Prefixes
parsePrefix :: [Char] -> Prefix
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
 (register <&> FromReg)
 <|>
 (int <&>  AddrImm)

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
      '+' -> return $ AddrPlus a0 a1
      ':' -> return $ AddrPlus a0 a1
      '-' -> return $ AddrMinus a0 a1
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
    return $ AddrTimes a0 a1
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
    Address a <- op_address
    return $ Address $ SizeDir s a
  ))
  <|> (try (do
    whitespaces
    char '['
    whitespaces
    a <- address_expr_inner
    whitespaces
    char ']'
    return $ Address a
  ))
  <|> (try (do
    whitespaces
    r <- register
    whitespaces
    char ':'
    whitespaces
    a <- address_expr_inner
    whitespaces
    return $ Address $ AddrPlus (FromReg r) a
  ))


-- Operands
op_reg = do
  register <&> Reg

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
  return $ Instr addr p m op1 op2 op3 Nothing 0

