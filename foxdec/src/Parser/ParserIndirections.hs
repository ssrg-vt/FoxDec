{-# OPTIONS_HADDOCK hide #-}


-- Parser that can be used to read .indirections file.
--
-- Example input line:
--   100000ed4 [100000ed6,100000f7c,100000f8c,1000010e7]

module Parser.ParserIndirections where

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

isWhiteSpace '\t' = True
isWhiteSpace '\f' = True
isWhiteSpace '\v' = True
isWhiteSpace ' ' = True
isWhiteSpace _ = False 

whitespace = satisfy isWhiteSpace <?> "white space"
whitespaces = skipMany whitespace <?> "white spaces"


indirection = do
  whitespaces
  a <- hexnum
  whitespaces
  inds <- between (char '[') (char ']') $ hexnum `sepBy` (char ',')
  whitespaces
  newline
  return $ (a,IS.fromList inds)
 

indirections = do
  inds <- many indirection
  whitespaces
  eof
  return $ IM.fromList $ inds

-- The parse function.
-- Takes as input a filename f and produces a list of instructions
-- to lists of instructions.
parse_indirections :: String -> IO (Either ParseError (IM.IntMap IS.IntSet))
parse_indirections f = parseFromFile indirections f


