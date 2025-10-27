module Parser.ParserCFI where


import Text.Parsec.Char (hexDigit,endOfLine)
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Word (Word8, Word64)
import Numeric (readHex)
import Data.List
import Data.List.Split (chunksOf)
import Data.Char
import Data.Maybe
import Base


data CFI = CFI {
  cfi_directives :: IM.IntMap String,
  cfi_data :: String
 }


instance Show CFI where
  show (CFI dirs dat) = (intercalate "\n" $ map show_dir $ IM.toList dirs) ++ "\n\n\n" ++ dat
   where
    show_dir (a,cfi) = "@0x" ++ showHex a ++ ":\n" ++ cfi

isWhiteSpace '\n' = True
isWhiteSpace '\r' = True
isWhiteSpace '\t' = True
isWhiteSpace '\f' = True
isWhiteSpace '\v' = True
isWhiteSpace ' ' = True
isWhiteSpace _ = False 

whitespace  = satisfy isWhiteSpace <?> "white space"
whitespaces = skipMany whitespace  <?> "white spaces"


line :: Parser String
line = do
  content <- many (noneOf "\r\n")
  _ <- optional endOfLine
  return content


comment = do
  string "#"
  line
  return ()

hexnum_with_0x = do
  string "0x"
  hexnum

address_label = do
  string "@"
  address <- hexnum_with_0x
  string ":"
  line
  return address

-- Parse all consecutive lines that start with '.'
dotLines :: Parser [String]
dotLines =
  manyTill dotLine (lookAhead (notFollowedBy (char '.')) <|> lookAhead eof)
  where
    dotLine = do
      _ <- char '.'
      content <- line
      return $ '.' : content

cfi_entry :: Parser (Int,String)
cfi_entry = do
  l <- address_label
  cfi <- dotLines
  whitespaces
  return $ (l,intercalate "\n" cfi)

cfi_entries :: Parser ([(Int,String)])
cfi_entries = do
  many cfi_entry

everything :: Parser String
everything = manyTill anyChar eof

cfi = do
   comment
   comment
   comment
   cfi <- cfi_entries
   rest <- everything
   return $ CFI (IM.fromList cfi) rest

-- The parse function.
-- Takes as input a filename f and produces a list of instructions
-- to lists of instructions.
parse_cfi :: String -> IO (Either ParseError CFI)
parse_cfi f = parseFromFile cfi f



