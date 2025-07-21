{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict, DeriveGeneric, StandaloneDeriving #-}

module Binary.Macho (macho_read_file) where

import Base

import Binary.Generic
import Data.Symbol

import Parser.ParserDump
import Parser.ParserSymbols
import Parser.ParserSections

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Word 
import Data.List
import Data.Bits
import Data.Maybe (fromJust)
import Data.List.Extra (firstJust)
import qualified Data.ByteString as BS
import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T



data Macho = Macho {
  macho_data :: IM.IntMap Word8,     -- writable data
  macho_dump :: IM.IntMap Word8,     -- read only data
  macho_symbols :: IM.IntMap Symbol, -- symbol list
  macho_sections :: SectionsInfo,    -- section info
  macho_entry :: [Word64],
  macho_dir_name :: String,
  macho_file_name :: String
 }


macho_read_ro_data :: Macho -> Word64 -> Int -> Maybe [Word8]
macho_read_ro_data m a si =
  let ds    = macho_dump m
      bytes = map (\a ->  IM.lookup a ds) [fromIntegral a..(fromIntegral a)+si-1] in
    case takeWhile ((/=) Nothing) bytes of
      [] -> Nothing
      bs -> Just $ map fromJust bs

macho_read_data :: Macho -> Word64 -> Int -> Maybe [Word8]
macho_read_data m a si =
  let ds    = macho_data m
      bytes = map (\a ->  IM.lookup a ds) [fromIntegral a..(fromIntegral a)+si-1] in
    if Nothing `elem` bytes then
      Nothing
    else
      Just $ map fromJust bytes

macho_get_relocs _ = []

macho_pp _ = ""


macho_text_section_size = sum . map size_of_section . filter is_text_section . si_sections . macho_sections
 where
  is_text_section (seg,sec,_,_,_,_) = (seg,sec) `elem` [("__TEXT","__text")]
  size_of_section (_,_,_,si,_,_) = fromIntegral $ si




instance BinaryClass Macho
  where
    binary_read_ro_data = macho_read_ro_data
    binary_read_data = macho_read_data
    binary_get_sections_info = macho_sections
    binary_get_symbols b = SymbolTable (macho_symbols b) IM.empty
    binary_get_relocations = \_ -> S.empty
    binary_pp = macho_pp
    binary_entry = macho_entry
    binary_text_section_size = macho_text_section_size
    binary_dir_name = macho_dir_name
    binary_file_name = macho_file_name

-- | Read in all files needed to read in MACHO binary executable: data, ro_data, symbols, sections, and entries.
macho_read_file dirname name = do
  dat  <- read_data dirname name
  dump <- read_dump dirname name
  syms <- read_syms dirname name
  secs <- read_sections dirname name
  ents <- read_entries dirname name

  return $ Macho dat dump syms secs ents dirname name


read_dump :: String -> String -> IO (IM.IntMap Word8)
read_dump dirname name = do
  let fname = dirname ++ name ++ ".dump"
  parse fname
 where
  parse fname = do
    ret0 <- parse_dump $! fname
    case ret0 of
      Left err -> error $ show err
      Right dump -> return dump

read_data :: String -> String -> IO (IM.IntMap Word8)
read_data dirname name = do
  let fname = dirname ++ name ++ ".data"
  parse fname
 where
  parse fname = do
    ret0 <- parse_dump $! fname
    case ret0 of
      Left err -> error $ show err
      Right dump -> return dump

read_syms :: String -> String -> IO (IM.IntMap Symbol)
read_syms dirname name = do
  let fname = dirname ++ name ++ ".symbols"
  parse fname
 where
  parse fname = do
    ret0 <- parse_symbols $! fname
    case ret0 of
      Left err -> error $ show err
      Right syms -> return syms


read_sections :: String -> String -> IO (SectionsInfo)
read_sections dirname name = do
  let fname = dirname ++ name ++ ".sections"
  parse fname
 where
  parse fname = do
    ret0 <- parse_sections $! fname
    case ret0 of
      Left err -> error $ show err
      Right sections -> return sections

read_entries :: String -> String -> IO [Word64]
read_entries dirname name = do
  let fname = dirname ++ name ++ ".entry"
  parse fname
 where
  parse fname = do
    ls <- readFile fname
    return $ map read_line $ lines ls
  read_line = readHex' . tail . tail

