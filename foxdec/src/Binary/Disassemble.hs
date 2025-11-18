{-# LANGUAGE ForeignFunctionInterface #-}

module Binary.Disassemble where

import Base
import Data.X86.Instruction

import Parser.ParserXed (instruction)
import Text.Parsec (parse)
import Text.Parsec.ByteString ()
import Data.Elf

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc (free)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

import Data.List


-- Import the C functions
foreign import ccall "disassemble1_buffer"
  c_disassemble1_buffer :: Ptr CChar -> CSize -> CULLong -> IO (Ptr CChar)

foreign import ccall "free_buffer"
  c_free_buffer :: Ptr CChar -> IO ()



elf_disassemble_instruction :: Elf -> Word64 -> IO (Maybe Instruction)
elf_disassemble_instruction elf a = do
  case filter (contains_address a) $ filter isExecutable $ elfSections elf of
    [] -> return Nothing
    [section] -> do
      let bytes = BS.take 20 $ BS.drop (fromIntegral $ a - elfSectionAddr section) $ elfSectionData section
      i <- BS.useAsCStringLen bytes (processBuffer a) 
      return $ Just i
    sections -> error $ show $ map (\s -> (elfSectionName s, elfSectionAddr s, elfSectionSize s)) sections
 where
  processBuffer :: Word64 -> CStringLen -> IO Instruction
  processBuffer a (ptr,len) = do
    outPtr <- c_disassemble1_buffer ptr (fromIntegral len) (fromIntegral a)
    if outPtr == nullPtr then
      error "C function returned NULL"
    else do
      str <- peekCAString outPtr
      c_free_buffer outPtr
      case parse instruction "xed" str of
        Left err -> error $ "Dissassembly error: " ++ show err
        Right i  -> return i 





elf_address_has_instruction ::
     Elf
  -> Word64  -- ^ An address
  -> Bool 
elf_address_has_instruction elf a =
  case find (contains_address a) $ filter isExecutable $ elfSections elf of
    Nothing -> False
    _ -> True

isExecutable s = SHF_EXECINSTR `elem` elfSectionFlags s

contains_address a section = 
  let a0  = elfSectionAddr section
      si0 = elfSectionSize section in
    a0 <= a && a < a0 + si0

