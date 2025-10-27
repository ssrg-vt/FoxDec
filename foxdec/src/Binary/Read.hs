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
import qualified Data.Map as M
import Data.List
import Data.List.Split
import Debug.Trace

-- | Reading a binary given a filename (ELF or MachO)
read_binary :: String -> String -> IO (Maybe Binary)
read_binary dirname name = do
  let filename = "./signatures/foxdec.signs.tsv"
  signs <- read_signs filename


  let filename = dirname ++ name
  exists <- doesFileExist filename

  if exists then
    try_ELF filename signs `orTryM` try_MACHO filename
  else
    return Nothing
 where

  try_ELF filename signs = do
    -- if the original binary is given, we now assume it its an ELF
    content <-  BS.readFile filename
    if (BS.unpack $ BS.take 4 content) == [0x7f, 0x45, 0x4C, 0x46] then do
      let elf = elf_read_file content

      exists1 <- doesFileExist (filename ++ ".xed")
      if exists1 then do
        xed <- parse_xed (filename ++ ".xed")
        case xed of
          Right is -> do
            let bin  = NamedElf elf dirname name (elf_get_sections_info elf) (elf_get_symbol_table elf) (elf_get_relocs elf) is signs 
            return $ Just $ Binary bin
          Left err -> die $ "Error reading " ++ filename ++ ".xed\n" ++ show err
      else
        die $ filename ++ ".xed" ++ " does not exist. Please run the ./run_xed.sh script first."

    else
      return Nothing

  try_MACHO filename = return Nothing


read_signs filename = do
  exists <- doesFileExist filename
  if not exists then
    --die "./signatures/foxdec.signs.tsv does not exist."
    return M.empty
  else do
    filecontent <- readFile filename
    return $ M.fromList $ concatMap parseLine $ lines filecontent
 where
  parseLine line
    | line == [] || head line == '#' = []
    | otherwise =
      let (lib_name:function_names_str:argcount_str:_) = splitOn "\t" line
          function_names = splitOn "," function_names_str
          argcount = read_argcount_str argcount_str in
        map (\f -> (f,argcount)) function_names

  read_argcount_str "variadic" = Variadic
  read_argcount_str str        = Argcount $ read str
   


show_is :: IM.IntMap Instruction -> String
show_is = intercalate "\n" . map show . IM.toList



