{-# OPTIONS_HADDOCK hide #-}

-- Parser that can be used to read .calls file.
--
-- Example input lines:
--   100000ed4 terminating
--   100000ed4 returning

module Parser.ParserCalls where

import Text.Parsec.Token
import Text.Parsec.Char (hexDigit)
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Word (Word8)
import Numeric (readHex)
import Data.List.Split (chunksOf)
import Data.Char

isWhiteSpace '\t' = True
isWhiteSpace '\f' = True
isWhiteSpace '\v' = True
isWhiteSpace ' ' = True
isWhiteSpace _ = False 

whitespace  = satisfy isWhiteSpace <?> "space"
whitespaces = skipMany whitespace  <?> "white space"

call = do
  whitespaces
  sign <- optionMaybe (char '-')
  a <- hexnum
  whitespaces
  terminating <- try (string "terminating") <|> string "returning"
  whitespaces
  newline
  return $ (if sign == Nothing then a else 0 - a,if terminating == "terminating" then True else False)
 

calls = do
  calls <- many call
  whitespaces
  eof
  return $ IM.fromList $ calls

-- The parse function.
-- Takes as input a filename f and produces a list of instructions
-- to lists of instructions.
parse_calls :: String -> IO (Either ParseError (IM.IntMap Bool))
parse_calls f = parseFromFile calls f


