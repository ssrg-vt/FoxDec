{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}
{-# OPTIONS_HADDOCK hide #-}

module Pass.DisassembleCapstone where

import Generic_Datastructures
import Parser.ParserX86Instruction
import X86_Datastructures
import Base


import Hapstone.Capstone
import qualified Hapstone.Internal.Capstone as Capstone
import Data.List
import Data.List.Split (splitOn)
import Data.Char (toUpper)
import Text.ParserCombinators.Parsec
import Data.Word (Word64,Word8)
import Debug.Trace
import qualified Data.IntMap as IM
import Data.Maybe (fromJust,catMaybes)
import X86.Opcode (Opcode(..))
import X86.Prefix (Prefix(..))




-- This file provides a function 
--      disassemble :: IM.IntMap Word8 -> Int -> IO (Maybe Instr)
-- Given a dump (mapping of addresses to bytes) and an address, it tries to retrieve a single instruction.
--
-- The disassembly is based on Hapstone, see:
--    https://github.com/ibabushkin/hapstone
--
--
-- Disassembly may produce an error, e.g., if an unknown mnemonic is encountered. If so, this is solved by adding the mnemonic to 
--    src/X86_Datastructures.hs
-- Note that in that case the instruction is unsupported, i.e., it must be given semantics in src/SymbolicExecution.hs as well.
-- In case of such an error, or other ones, please contact us.




disasm_config bytes a = Disassembler { 
  -- Options: CsArchArm, CsArchArm64, CsArchMips, CsArchX86, CsArchPpc, CsArchSparc, CsArchSysz, CsArchXcore
  arch = Capstone.CsArchX86,
  -- Modes (some may be combined by adding to the list): CsModeLittleEndian, CsModeArm, CsMode16 (16-bit x86), CsMode32 (32-bit x86), CsMode64 (64-bit x86-64/amd64 or PPC), CsModeThumb, CsModeMclass, CsModeV8 (ARMv8 A32), CsModeMicro, CsModeMips3, CsModeMips32r6, CsModeMipsGp64, CsModeV9 (SparcV9 mode), CsModeBigEndian, CsModeMips32, CsModeMips64
  modes = [Capstone.CsMode64, Capstone.CsModeLittleEndian],
  -- buffer to disassemble, as [Word8]
  buffer = bytes,
  -- address of first byte in the buffer, as Word64
  addr = fromIntegral a,
  -- number of instructions to disassemble (0 for maximum)
  num = 1,
  -- include detailed information? True/False
  Hapstone.Capstone.detail = False,
  -- setup SKIPDATA options, as Maybe CsSkipdataStruct
  skip = Just (defaultSkipdataStruct),
  -- action to run on each instruction, a function with signature Csh -> CsInsn -> IO a; default is defaultAction
  action = defaultAction
 }

mk_operands cs_instr cs_ops = map mk_operand [0,1,2]
 where
  mk_operand n = 
    let splits = splitOn "," cs_ops in
      if trim cs_ops == "" then
        Nothing
      else if length splits > n then
        case parse_operand $ splits !! n of
          Left err -> error ("Could not parse operand " ++ show n ++ ": \"" ++ cs_ops ++ "\" of \"" ++ show cs_instr ++ "\"")
          Right op -> Just op
      else
        Nothing
  parse_operand :: String -> Either ParseError X86_Operand
  parse_operand = parse operand "" . trim

trim = dropWhileEnd isWhiteSpace . dropWhile isWhiteSpace

mk_instr :: Capstone.CsInsn -> X86_Instruction
mk_instr cs_instr =
  let addr          = AddressWord64 $ Capstone.address cs_instr
      ops           = mk_operands cs_instr $ Capstone.opStr cs_instr
      (prefix,m)    = parseMnemonicAndPrefix $ Capstone.mnemonic cs_instr
      size          = length $ Capstone.bytes cs_instr
      i             = Instruction addr prefix m Nothing (catMaybes ops) (Just size) in
    if m == InvalidOpcode then
      error ("Error during disassembling (translation of Capstone to datastructure): " ++ show cs_instr  ++ ": " ++ show i)
    else if prefix == Just InvalidPrefix then
      error ("Error during disassembling (translation of Capstone to datastructure): " ++ show cs_instr  ++ ": " ++ show i)
    else
       i
 where
  parseMnemonicAndPrefix str =
    case words str of
      [m]   -> (Nothing,read_opcode $ map toUpper m)
      [p,m] -> (Just $ parsePrefix p,read_opcode $ map toUpper m)



disassemble :: IM.IntMap Word8 -> Int -> IO (Maybe X86_Instruction)
disassemble dump a = do
  let buffer = readBuffer
  if head buffer == Nothing then 
    return Nothing
  else do
    let config = disasm_config (map fromJust $ takeWhile ((/=) Nothing) buffer) a
    result <- disasmIO config 
    case result of
      Left err      -> error ("Error during disassembling of address " ++ showHex a ++ ": " ++ show err)
      Right [(i,_)] -> return $ Just $ mk_instr i
      Right x       -> error $ "Error during disassembling of address " ++ showHex a ++ ": parsing result Capstone == " ++ show x ++ " @ address " ++ showHex a ++ " buffer == " ++ showHex_list (map fromJust buffer)
 where
  readBuffer = map (\n -> IM.lookup (a+n) dump) [0..20] -- 15 == maximum instruction length
