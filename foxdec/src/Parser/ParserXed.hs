{-# OPTIONS_HADDOCK hide #-}

-- Parser that can be used to read XED output
--
-- Example input lines:


{--
0x3020 (6): push qword ptr [rip+0x1abfa]
EXPLICIT  ; R  
SUPPRESSED; RW ; REG; STACKPUSH
SUPPRESSED; W  ; MEM; [RSP]

0x3021 (5): xor eax, 0x1abfa
IMPLICIT  ; RW 
EXPLICIT  ; R  
SUPPRESSED; W  ; REG; RFLAGS

0x16428d (6): vpblendmb zmm17{k1}{z}, zmm9, zmmword ptr [r15]
EXPLICIT  ; W  
EXPLICIT  ; R  
EXPLICIT  ; R  
EXPLICIT  ; R  

--}


-- Special treatment is given to:
-- PUSH/POP/RETURN/CALL
-- MOVSD with strings
-- NOP
-- XCHG, FXCH,XADD, CMPXCHG
-- UD0

module Parser.ParserXed where

import Text.Parsec.Char (hexDigit)
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Word (Word8, Word64)
import Numeric (readHex)
import Data.List.Split (chunksOf)
import Data.Char
import Data.Maybe

import Data.X86.Opcode
import Data.X86.Register
import Data.X86.Instruction
import Data.Size
import Debug.Trace

isWhiteSpace '\t' = True
isWhiteSpace '\f' = True
isWhiteSpace '\v' = True
isWhiteSpace ' ' = True
isWhiteSpace _ = False 

whitespace  = satisfy isWhiteSpace <?> "white space"
whitespaces = skipMany whitespace  <?> "white spaces"


whitespace_comma :: Parser Char
whitespace_comma  = satisfy (\c -> isWhiteSpace c || c == ',') <?> "white space or comma" 
whitespaces_comma = skipMany whitespace_comma <?> "white spaces or comma"




hexnum_with_0x = do
  string "0x"
  hexnum

mnemonic = parseMnemonic <?> "mnemonic"
 where
  parseMnemonic = choice $ map try [jmp_far, call_far, ret_far, fdisi8087_nop, feni8087_nop, fsetpm287_nop, prefetch_reserved, normal]
  ret_far = do
    string "ret far"
    return RETF
  call_far = do
    string "call far"
    return CALLF
  jmp_far = do
    string "jmp far"
    return JMPF
  fdisi8087_nop = do
    string "fdisi8087_nop"
    return NOP
  feni8087_nop = do
    string "feni8087_nop"
    return NOP
  fsetpm287_nop = do
    string "fsetpm287_nop"
    return NOP
  prefetch_reserved = do
    string "prefetch_reserved"
    return PREFETCH



  normal = do
    str <- many1 alphaNum
    case read_opcode $ map toUpper str of
      --InvalidOpcode -> fail $ "Not a mnemonic: " ++ str
      m -> return m


prefix :: Parser (Maybe Prefix)
prefix = parsePrefix <?> "prefix"
 where 
  parsePrefix = do
    p <- choice $ map try [xacquire, xrelease,data16,data32,addr16,addr32,hint,hint_no,notrack,lock,repne,rep,bnd]
    whitespaces
    return p

  lock = string "lock" >> return (Just PrefixLock)
  repne = string "repne" >> return (Just PrefixRepNE)
  rep = string "rep" >> return (Just PrefixRep)
  bnd = string "bnd" >> return (Just PrefixBnd)
  notrack = string "notrack" >> return (Just PrefixNoTrack)
  hint = string "hint-taken" >> return Nothing
  hint_no = string "hint-not-taken" >> return (Just PrefixNotTaken)
  addr16 = string "addr16" >> return Nothing
  addr32 = string "addr32" >> return Nothing
  data16 = string "data16" >> return Nothing
  data32 = string "data32" >> return Nothing
  xrelease = string "xrelease" >> return Nothing
  xacquire = string "xacquire" >> return Nothing



register = (try mm <|> try mmx <|> try normal) <?> "register"
 where
  normal = do
    str <- many1 alphaNum
    case read_register $ map toUpper str of
      RegNone -> fail $ "Not a register: " ++ str
      r -> return r

  mmx = do
    string "st"
    index <- option 0 mmx_index
    return $ RegFPU $ mk_mmx index

  mmx_index = try $ do
    string "("
    index <- decimal
    string ")"
    return index

  mm = do
    string "mm"
    idx <- decimal
    return $ RegMM idx


reg_operand = do
  r <- register
  return $ Op_Reg r []

imm_operand imm_bitwidth = do
  imm <- hexnum_with_0x
  return $ Op_Imm $ Immediate (BitSize (if imm_bitwidth == 0 then 64 else imm_bitwidth)) imm 

data MemAddend =
    MemAddendReg Register
  | MemAddendImm Word64
  | MemAddendMult Register Word8
  | MemAddendSeg SReg
  deriving Show

mem_addend_reg = do
  r <- register
  return $ MemAddendReg r

mem_addend_imm = do
  i <- hexnum_with_0x
  return $ MemAddendImm i

mem_addend_mult = do
  r <- register
  string "*"
  scale <- hexnum
  return $ MemAddendMult r scale

mem_addend_sreg = do
  RegSeg r <- register
  string":"
  return r

mem_addend = try mem_addend_mult <|> mem_addend_reg <|> try mem_addend_imm

mem_addends = try $ do
  a <- mem_addend
  whitespaces
  as <- option [] extra_addends
  return $ a:as
 where
  extra_addends = extra_pos_addend <|> extra_neg_addend
  extra_pos_addend = do
    string "+"
    whitespaces
    mem_addends
  extra_neg_addend = do
    string "-"
    whitespaces
    subtract <$> mem_addends -- TODO make negative

  subtract [MemAddendImm i] = [MemAddendImm $ fromIntegral (0 - (fromIntegral i::Int))]

ptr_operand = do
  string "ptr"
  whitespaces
  sreg <- optionMaybe (try mem_addend_sreg)
  string "["
  addends <- mem_addends
  whitespaces
  string "]"
  case addends of
    [MemAddendImm imm] -> return $ Op_Mem (BitSize 0) RegNone RegNone 0 (fromIntegral imm) sreg []
    [MemAddendReg reg] -> return $ Op_Mem (BitSize 0) reg RegNone 0 0 sreg []
    [MemAddendReg reg0,MemAddendMult reg1 scale] -> return $ Op_Mem (BitSize 0) reg0 reg1 scale 0 sreg []
    [MemAddendReg reg0,MemAddendMult reg1 scale,MemAddendImm displ] -> return $ Op_Mem (BitSize 0) reg0 reg1 scale (fromIntegral displ) sreg []
    [MemAddendReg reg,MemAddendImm displ] -> return $ Op_Mem (BitSize 0) reg RegNone 0 (fromIntegral displ) sreg []
    [MemAddendMult reg1 scale,MemAddendImm displ] -> return $ Op_Mem (BitSize 0) RegNone reg1 scale (fromIntegral displ) sreg []
    [MemAddendMult reg1 scale] -> return $ Op_Mem (BitSize 0) RegNone reg1 scale 0 sreg []
    x -> error $ show x

size_directive = try zmmword <|> try ymmword <|> try xmmword <|> try qword <|> try dword <|> try word <|> try byte
 where
  zmmword = do
    string "zmmword"
    return 512
  ymmword = do
    string "ymmword"
    return 256
  xmmword = do
    string "xmmword"
    return 128
  qword = do
    string "qword"
    return 64
  dword = do
    string "dword"
    return 32
  word = do
    string "word"
    return 16
  byte = do
    string "byte"
    return 8

mem_operand = do
  si <- size_directive 
  whitespaces
  op <- ptr_operand
  return $ mem_operand_with_size op si

mem_operand_with_size (Op_Mem _ reg idx scale displ seg ai) si = Op_Mem (BitSize si) reg idx scale displ seg ai


operand_mask_info = do
  string "{"
  manyTill anyChar (try (string "}"))
  whitespaces


operand imm_bitwidth = parse_operand <?> "operand"
 where
  parse_operand = do
    optional operand_mask_info
    op <- try mem_operand <|> try ptr_operand <|> try (imm_operand imm_bitwidth) <|> try reg_operand
    optional operand_mask_info
    optional operand_mask_info
    whitespaces_comma
    return op



data OperandInfo = Explicit | Implicit | Suppressed | SuppressedFlags
  deriving Show


access_info = try cwrite <|> try cread <|> read <|> write
 where
  cread  = string "CR" *> return [Read]
  cwrite = string "CW" *> return [Read,Write]
  read   = string "R"  *> return [Read]
  write  = string "W"  *> return [Write]

operand_info = do
  oi <- try explicit <|> try implicit <|> try suppressed
  whitespaces
  string ";"
  whitespaces
  ais <- concat <$> many1 access_info
  whitespaces
  writesFlags <- option False (try (string "; REG; RFLAGS") >> return True)
  manyTill anyChar newline
  case (oi,writesFlags) of
    ("SUPPRESSED", True)  -> return $ (SuppressedFlags, ais)
    ("SUPPRESSED", False) -> return $ (Suppressed, ais)
    ("EXPLICIT", _)       -> return $ (Explicit, ais)
    ("IMPLICIT", _)       -> return $ (Implicit, ais)
 where
  explicit = string "EXPLICIT"
  implicit = string "IMPLICIT"
  suppressed = string "SUPPRESSED"



instruction :: Parser Instruction
instruction = do
  address <- hexnum_with_0x
  whitespaces
  string "("
  si <- nat
  string ","
  imm_bitwidth <- nat
  string "):"
  whitespaces
  prefixes <- many prefix
  m <- mnemonic
  whitespaces
  ops <- many (operand imm_bitwidth)
  whitespaces
  many newline

  ois <- many operand_info
  many newline

  let writesFlags = any writesToFlag ois

  let i = Instruction address (catMaybes prefixes) m ops [] si
  let ops' = mk_operands i m 0 ops (take (length ops) ois)
  return $ Instruction address (catMaybes prefixes) m ops' (if writesFlags then [WritesToFlags] else []) si 
 where
  isRead  (Explicit,   ais) = any ((==) Read) ais
  isRead  (Implicit,   ais) = any ((==) Read) ais
  isRead  (Suppressed, ais) = any ((==) Read) ais
  isWrite (Explicit,   ais) = any ((==) Write) ais
  isWrite (Implicit,   ais) = any ((==) Write) ais
  isWrite (Suppressed, ais) = any ((==) Write) ais

  writesToFlag (SuppressedFlags, ais) = any ((==) Write) ais
  writesToFlag _ = False


  mk_operands i m numWrites [] [] = []
  mk_operands i m numWrites (op0:ops') (i0:is')
    | is_zero_mem_operand op0 && m `elem` [FSTP,FLD]              = mk_operands i m numWrites (mem_operand_with_size op0 80:ops') (i0:is')
    | is_zero_mem_operand op0 && m `elem` [FSTENV,FNSTENV,FLDENV] = mk_operands i m numWrites (mem_operand_with_size op0 28:ops') (i0:is')
    | is_zero_mem_operand op0 && m `elem` prefetches              = mk_operands i m numWrites (mem_operand_with_size op0 8:ops') (i0:is')
    | is_zero_mem_operand op0 && m /= LEA                         = error $ show i
    | isWrite i0 && numWrites < 2 = add_info op0 i0 : mk_operands i m (numWrites+1) ops' is'
    | isRead i0 && all (not . isWrite) is' && all isRead is' = add_info op0 i0 : mk_operands i m numWrites ops' is'
    | otherwise = error $ "Instruction has unknown operand types: " ++ show i ++ "\n" ++ show (zip (op0:ops') (i0:is'))

  add_info (Op_Reg r []) (_,i) = Op_Reg r i
  add_info (Op_Mem si reg idx scale displ seg []) (_,i) = Op_Mem si reg idx scale displ seg i
  add_info (Op_Imm i) _ = Op_Imm i

  is_zero_mem_operand (Op_Mem (BitSize 0) reg idx scale displ seg []) = True
  is_zero_mem_operand _ = False

  prefetches = [PREFETCHT0,PREFETCHT1,PREFETCHT2,PREFETCHNTA]

disasm = do
  instructions <- many instruction
  whitespaces
  eof
  return $ IM.fromList $ map withAddress instructions
 where
  withAddress i = (fromIntegral $ inAddress i,i)

-- The parse function.
-- Takes as input a filename f and produces a list of instructions
-- to lists of instructions.
parse_xed :: String -> IO (Either ParseError (IM.IntMap Instruction))
parse_xed f = parseFromFile disasm f


