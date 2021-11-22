-- Parser that can be used to read the output of objdump applied to X86 binaries
-- Run objdump on Ubuntu as follows:
--
--
--  UBUNTU:
--      TODO
--
-- MACOS:
--      otool -I -v #DIR#/#BINARY# | grep '^0x' | tr -s ' ' | cut -d ' ' -f1,3
--
-- Example input:
--
-- 0x00000001001ac528 _strcasecmp

module ParserSymbols where

import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Data.IntMap as IM


isWhiteSpace '\t' = True
isWhiteSpace '\f' = True
isWhiteSpace '\v' = True
isWhiteSpace ' ' = True
isWhiteSpace _ = False 

whitespace = satisfy isWhiteSpace <?> "white space"
whitespaces = skipMany whitespace <?> "white spaces"


hexnum_with_0x = do
  string "0x"
  hexnum

symb = do
  a <- hexnum_with_0x
  whitespaces
  s <- many (noneOf "\n")
  newline
  return (a,s)
 

symbols = do
  ss <- many symb
  eof
  return $ IM.fromList ss

-- The parse function.
-- Takes as input a filename f and produces a list of instructions
-- to lists of instructions.
parse_symbols  :: String -> IO (Either ParseError (IM.IntMap String))
parse_symbols f = parseFromFile symbols f


