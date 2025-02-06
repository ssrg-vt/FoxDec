-- {-# LANGUAGE  #-}


module Binary.Read where

import Binary.Generic
import Binary.Elf
import Binary.Macho

import qualified Data.ByteString as BS
import System.Directory (doesFileExist)

-- | Reading a binary given a filename (ELF or MachO)
read_binary :: String -> String -> IO (Maybe Binary)
read_binary dirname name = do
  let filename = dirname ++ name
  exists <- doesFileExist filename
  if exists then do
    -- if the original binary is given, we now assume it its an ELF
    content <-  BS.readFile filename
    if (BS.unpack $ BS.take 4 content) == [0x7f, 0x45, 0x4C, 0x46] then do
      let elf = elf_read_file content
      return $ Just $ Binary $ NamedElf elf dirname name (elf_get_sections_info elf) (elf_get_symbol_table elf) (elf_get_relocs elf)
    else
      return Nothing
  else do
    -- otherwise, see if the binary is contained in the following files (MachO)
    exists1 <- doesFileExist (filename ++ ".dump")
    exists2 <- doesFileExist (filename ++ ".data")
    exists3 <- doesFileExist (filename ++ ".sections")
    exists4 <- doesFileExist (filename ++ ".symbols")
    exists5 <- doesFileExist (filename ++ ".entry")
    if and [exists1,exists2,exists3,exists4,exists5] then do
      macho <- macho_read_file dirname name
      return $ Just $ Binary $ macho
    else
      return Nothing
