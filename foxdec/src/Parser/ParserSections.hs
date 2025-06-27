{-# LANGUAGE Strict #-}


{-# OPTIONS_HADDOCK hide #-}

-- Parser that can be used to read section info supplied by bash script "dump_macho.sh"
--
-- Example input:
--
-- (__TEXT,__text)
--   addr = 0x0000000100002a94
--   size = 0x0000000000000f69
--
-- | (__DATA,__common)
-- |   addr = 0x00000001000041d0
-- |   size = 0x0000000000000010

module Parser.ParserSections where

import Binary.Generic

import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

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


section_info = do
  many newline

  whitespaces
  string "("
  segname <- many (noneOf ",")
  string ","
  sectname <- many (noneOf ")")
  string ")"
  whitespaces
  newline

  whitespaces
  string "addr"
  whitespaces
  string "="
  whitespaces
  addr <- hexnum_with_0x
  newline

  whitespaces
  string "size"
  whitespaces
  string "="
  whitespaces
  size <- hexnum_with_0x
  many newline
  return $ (segname,sectname,addr,size,16,[])

sections_info = do
  sis <- many section_info
  eof
  let min = minimum $ map get_min_address sis
  let max = maximum $ map get_max_address sis
  return $ SectionsInfo sis min max
 where
  get_min_address (_,_,a,_,_,_)  = a
  get_max_address (_,_,a,si,_,_) = a + si - 1

-- The parse function.
-- Takes as input a filename f and produces a list of instructions
-- to lists of instructions.
parse_sections  :: String -> IO (Either ParseError SectionsInfo)
parse_sections = parseFromFile sections_info


