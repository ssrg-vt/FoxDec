-- {-# LANGUAGE  #-}


module Binary.Read where

import Base
import Binary.Generic
import Binary.Elf
import Binary.Macho
import Parser.ParserXed
import Data.X86.Instruction

import System.Exit (die)

import qualified Data.ByteString as BS
import System.Directory (doesFileExist)
import qualified Data.IntMap as IM
import Data.List
import Debug.Trace

-- | Reading a binary given a filename (ELF or MachO)
read_binary :: String -> String -> IO (Maybe Binary)
read_binary dirname name = do
  let filename = dirname ++ name
  exists <- doesFileExist filename

  if exists then
    try_ELF filename `orTryM` try_MACHO filename
  else
    return Nothing
 where

  try_ELF filename = do
    -- if the original binary is given, we now assume it its an ELF
    content <-  BS.readFile filename
    if (BS.unpack $ BS.take 4 content) == [0x7f, 0x45, 0x4C, 0x46] then do
      let elf = elf_read_file content

      exists1 <- doesFileExist (filename ++ ".xed")
      if exists1 then do
        xed <- parse_xed (filename ++ ".xed")
        case xed of
          Right is -> {--trace (show_is is) $--} return $ Just $ Binary $ NamedElf elf dirname name (elf_get_sections_info elf) (elf_get_symbol_table elf) (elf_get_relocs elf) is
          Left err -> die $ "Error reading " ++ filename ++ ".xed\n" ++ show err
      else
        die $ filename ++ ".xed" ++ " does not exist." -- TODO error msg

    else
      return Nothing

  try_MACHO filename = return Nothing


show_is :: IM.IntMap Instruction -> String
show_is = intercalate "\n" . map show . IM.toList
