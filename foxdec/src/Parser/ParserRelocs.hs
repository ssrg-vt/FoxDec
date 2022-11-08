{-# OPTIONS_HADDOCK hide #-}


-- Parser that can be used to read the output of objdump applied to X86 binaries
-- Run objdump on Ubuntu as follows:
--
--
--  UBUNTU:
--      objdump -R $BINARY | grep R_X86_64_RELATIVE | cut -d ' ' -f1,4 | awk '{print "0x" $0}'
--
-- MACOS:
--      TODO
--
-- Example input:
--
-- 0x00000001001ac528 *ABS*+0x00000001001ac528

module Parser.ParserRelocs where

import Analysis.Context

import Data.Word
import Data.Binary
import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Data.IntMap as IM
import Data.Char


isWhiteSpace '\t' = True
isWhiteSpace '\f' = True
isWhiteSpace '\v' = True
isWhiteSpace ' ' = True
isWhiteSpace _ = False 

whitespace  = satisfy isWhiteSpace <?> "space"
whitespaces = skipMany whitespace  <?> "white space"



hexnum_with_0x = do
  string "0x"
  hexnum

reloc = do
  a0 <- hexnum_with_0x
  whitespaces
  string "*ABS*"
  whitespaces
  string "+"
  whitespaces
  a1 <- hexnum_with_0x
  newline
  return $ R_X86_64_RELATIVE a0 a1
 

relocs = do
  rs <- many reloc
  eof
  return rs

-- The parse function.
-- Takes as input a filename f and produces a list of instructions
-- to lists of instructions.
parse_relocs :: String -> IO (Either ParseError [Relocation])
parse_relocs f = parseFromFile relocs f


