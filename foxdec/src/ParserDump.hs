{-# OPTIONS_HADDOCK hide #-}


-- Parser that can be used to read the output (data sections) of objdump/objcopy applied to X86 binaries.
--
--  UBUNTU:
--      TODO
--
-- MACOS:
--      objdump -s -j __data -j __text #DIR#/#BINARY# | tr -s ' ' |  grep "^ ......... " | cut -d ' ' -f1,2,3,4,5,6
--
--
-- Example input line:
--   1000b3f80 ffff4889 fb488b05 acc01400 488b0048  ..H..H......H..H
--
--      ^       ^     
--      |       |
--   ADDRESS   DATA
--
--   ADDRESS is in hex format without 0x

module ParserDump where

import Text.Parsec.Token
import Text.Parsec.Char (hexDigit)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Data.IntMap as IM
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

-- a hex sequence of 1 or more bytes
-- E.g.: 10631e00
bytes = do
  digits <- many1 hexDigit
  whitespaces
  return $ map (fromIntegral . fst . head . readHex) $ chunksOf 2 digits

data_line = do
  skipMany ignored_line
  whitespaces
  a <- try (string "0x" >> hexnum)
       <|>
       hexnum 
  whitespaces
  bs <- many1 bytes
  many (noneOf "\n")
  newline
  return $ zip [a..] (concat bs)
 
dump = do
  ds <- many1 (try data_line)
  return $ IM.fromList $ concat ds
  

dumps = do
  ds <- many1 dump 
  skipMany ignored_line
  eof
  return $ IM.unions ds

-- The parse function.
-- Takes as input a filename f and produces a mapping of addresses (Int) to bytes (Word8)
parse_dump :: String -> IO (Either ParseError (IM.IntMap Word8))
parse_dump f = parseFromFile dumps f









ignored_line = 
  (try (do 
    whitespaces
    string "Contents of"
    skipMany (noneOf "\n")
    newline
  ))
  <|> (try (do 
    whitespaces
    string "<skipping contents of"
    skipMany (noneOf "\n")
    newline
  ))

