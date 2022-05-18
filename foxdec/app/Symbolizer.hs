{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}

module Symbolizer where


import Base
import Pass.CFG_Gen
import Analysis.Context
import X86_Datastructures
import Generic_Datastructures
import VerificationReportInterface
import Data.ControlFlow
import X86.Register (Register(..))
import X86.Opcode (Opcode(..), isCall, isJump, isCondJump, isRet)

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List 
import Data.Word
import Data.Foldable
import Data.Char (chr,showLitChar)

import Control.Monad.State.Strict
import Data.Functor.Identity
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Unsafe


import Debug.Trace

usage_msg = "Usage:\n\n  foxdec-symbolizer-exe NAME.report\n\nHere NAME refers to the NAME used when running foxdec-exe.\nRun this program from the same directory foxdec-exe was run."

-- | Read in first command-line argument, check whether it is a .report file.
-- Read the file, and use the "VerificationReportInterface" to get an overview of the instructions.
main = do
  args <- getArgs
  if length args /= 1 then
    putStrLn usage_msg
  else do
    exists <- doesFileExist $ head args
    if exists then do
      ctxt  <- ctxt_read_report $ head args

      entries <- retrieve_io $ ctxt_get_function_entries ctxt
      let symbolized_entries = map (symbolize_entry ctxt) $ S.toList entries

      putStrLn $ externals ctxt ++ "\n\n"
      putStrLn $ section_text ++ "\n"
      putStrLn $ "default rel" ++ "\n" -- needed for MachO
      putStrLn $ intercalate "\n\n\n" symbolized_entries ++ "\n\n\n"
      putStrLn $ literal_string_data_section ctxt
      putStrLn $ bss_data_section ctxt
      putStrLn $ data_section ctxt
      putStrLn $ ro_data_section ctxt
      putStrLn $ jump_tables ctxt
    else
      putStrLn $ "File: " ++ show (head args) ++ " does not exist."
 where
  read_address str = do
    if take 2 str == "0x" then
      return $ readHex' $ drop 2 str 
    else do
      putStrLn usage_msg
      error $ "Invalid second argument."


section_text = "section .text"
section_data = "section .data"
section_bss  = "section .bss"

literal_string_data_section ctxt =
  concatMap mk_literal_string_section $ filter is_literal_string_section $ si_sections $ ctxt_sections ctxt
 where
  mk_literal_string_section (segment,section,a0,sz) = 
    let dat = map (\offset -> IM.lookup (a0+offset) $ ctxt_dump ctxt) [0..sz-1] in
      if any ((==) Nothing) dat || dat == [] then
        ""
      else
        intercalate "\n" $ ([section_data] ++ literal_strings segment section 0 (catMaybes dat) ++ [""])


  literal_strings segment section offset []  = []
  literal_strings segment section offset dat = 
    let (str,remainder) = break ((==) 0) dat in
      mk_string_entry segment section offset str : literal_strings segment section (offset + length str + 1) (tail remainder)

  mk_string_entry segment section offset str = section_label segment section ++ "_" ++ show offset ++ ": db `" ++  concatMap charToC str ++ "`, 0" -- TODO ",0" for 0-ending?

  charToC w = showLitChar (chr $ fromIntegral w) ""


is_literal_string_section ("__TEXT","__cstring",_,_) = True
is_literal_string_section _ = False

externals ctxt =
  intercalate "\n" $ map ((++) "extern ") $ filter ((/=) "") $ nub $ IM.elems $ ctxt_syms ctxt

symbolize_entry ctxt entry =
  header_comment ++ symbolized_cfg
 where
  header_comment = comment_block ["Function: " ++ function_name_of_entry ctxt entry]
  symbolized_cfg =
      case IM.lookup entry (ctxt_cfgs ctxt) of
        Just cfg -> symbolize_cfg ctxt entry cfg


bss_data_section ctxt = 
  concatMap mk_bss_data_section $ filter is_bss_data_section $ si_sections $ ctxt_sections ctxt
 where
  mk_bss_data_section (segment,section,a0,sz) = section_bss ++ "\n" ++ section_label segment section ++ ": resb " ++ show sz ++ "\n\n"
  
is_bss_data_section ("__DATA","__bss",_,_) = True
is_bss_data_section ("__DATA","__common",_,_) = True
is_bss_data_section ("",".bss",_,_) = True
is_bss_data_section _ = False



data_section ctxt =
  let dat = ctxt_data ctxt in
    if IM.null dat then
      ""
    else
      mk_data_section dat
 where
  mk_data_section dat = intercalate "\n" $ (section_data : (data_section_label ++ ":") : (map mk_data_entry $ IM.assocs dat)) ++ [""] -- check whether contiguous 
  mk_data_entry (a,v) = "db 0" ++ showHex v ++ "h" 

ro_data_section ctxt = 
  concatMap mk_ro_data_section $ filter is_ro_data_section $ si_sections $ ctxt_sections ctxt
 where
  mk_ro_data_section (segment,section,a0,sz) = intercalate "\n" $ (section_data : (section_label segment section ++ ":") : (map (mk_data_entry $ fromIntegral a0) [0..sz-1])) ++ [""] 
  mk_data_entry a0 i = 
    case read_from_datasection ctxt (a0 + fromIntegral i) 1 of
      Just v -> "db 0" ++ showHex v ++ "h" 
  
is_ro_data_section ("",".rodata",_,_) = True
is_ro_data_section _ = False



jump_tables ctxt = 
  let inds = IM.toList $ ctxt_inds ctxt in
    if inds == [] then
      ""
    else
      intercalate "\n" (section_data : (concatMap mk_indirection $ IM.toList $ ctxt_inds ctxt))
 where
  mk_indirection (i_a,IndirectionResolved _)   = []
  mk_indirection (i_a,IndirectionJumpTable jt) = mk_jump_table i_a jt

  mk_jump_table i_a (JumpTable _ _ entries) = [intercalate "\n" (mk_label i_a : map mk_entry entries)]

  mk_entry a = "dq " ++ fromJust (symbolize_immediate ctxt a)
  mk_label i_a = jump_table_label i_a ++ ":" 



comment_block strs =
   intercalate "\n" $ comment_block_delim : (map comment strs ++ [comment_block_delim,""])
 where
  comment_block_delim = comment $ replicate (length max) '-' 
  max = maximumBy compare_lengths strs
  compare_lengths str0 str1 = compare (length str0) (length str1)

comment str = "; " ++ str


symbolize_cfg ctxt entry cfg = 
  intercalate "\n" $ map (symbolize_block ctxt entry cfg) (IM.toList $ cfg_instrs cfg)


symbolize_block ctxt entry cfg (blockID, instrs) = 
  intercalate "\n" $ block_header : symbolyzed_block
 where
  block_header = comment $ "Entry " ++ showHex entry ++ "; block " ++ show blockID ++ "; address " ++ showHex (instr_addr $ head instrs)
  symbolyzed_block = [block_label'] ++ block_body ++ [block_end]

  block_label' = block_label entry blockID ++ ":"
  block_body   = map (indent . symbolize_instr ctxt entry cfg) $ filter_unnecessary_jumps instrs
  block_end    = if instrs == [] || is_proper_block_end_instruction (instr_opcode $ last instrs) then "" else mk_extra_jmp

  filter_unnecessary_jumps instrs = (filter (not . isJump . instr_opcode) $ init instrs) ++ [last instrs]


  is_proper_block_end_instruction i = isRet i || isJump i 

  mk_extra_jmp =
    case unsafePerformIO $ stepA ctxt entry (fromIntegral $ instr_addr $ last instrs) of -- TODO, also TODO assumes fall through is last/second
      Right []        -> ""
      Right [(a,_)]   -> indent $ "JMP " ++ (symbolize_address ctxt entry cfg $ fromIntegral a) ++ "     ; inserted\n"
      Right [_,(a,_)] -> indent $ "JMP " ++ (symbolize_address ctxt entry cfg $ fromIntegral a) ++ "     ; inserted\n"
      _               -> "JMP ERROR CANNOT DETERMINE NEXT BLOCK"


block_label entry blockID = "L" ++ showHex entry ++ "_" ++ show blockID

indent str = "    " ++ str


symbolize_instr ctxt entry cfg i = 
  if isCall (instr_opcode i) || isJump (instr_opcode i) || isCondJump (instr_opcode i) then
    symbolize_operand1_of_instr ctxt entry cfg i
  else
    show_nasm_instruction ctxt entry cfg  i




-- TODO LIBC_START_MAIN
symbolize_operand1_of_instr ctxt entry cfg i@(Instruction (AddressWord64 i_a) Nothing mnemonic Nothing ops annot) =
 case resolve_jump_target ctxt i of
   [External sym]         -> show mnemonic ++ " " ++ sym
   [ImmediateAddress imm] -> show mnemonic ++ " " ++ symbolize_address ctxt entry cfg imm
   trgts                  -> show_jump_table ++ indent (show_nasm_instruction ctxt entry cfg  i ++ "; TARGETS: " ++ (intercalate "," $ map show_trgt $ nub trgts))

 where
  show_jump_table =
    case IM.lookup (fromIntegral i_a) $ ctxt_inds ctxt of
      Just (IndirectionJumpTable jt) -> mk_jump_table_instruction jt
      _                              -> "; NO JUMP TABLE FOUND.\n"

  mk_jump_table_instruction (JumpTable index_op trgt_op entries) = 
    "MOV " ++ show trgt_op ++ ", QWORD PTR [" ++ jump_table_label i_a ++ " + 8*" ++ show index_op ++ "] ; " ++ msg ++ "\n"

  msg = "inserted as implementation of the jump-table based jump below. Manually remove instructions above that originally implemented this jump table."


show_trgt (ImmediateAddress a) = showHex a
show_trgt Unresolved           = "Unresolved"
show_trgt (External sym)       = sym



symbolize_address :: Context -> Int -> CFG -> Word64 -> String
symbolize_address ctxt entry cfg imm =
  label_in_current_cfg
  `orTry`
  ((\a -> block_label a 0) <$> find ((==) imm) (map fromIntegral $ IM.keys $ ctxt_calls ctxt))
  `finally`
  ("ERROR UNRESOLVED TARGET: " ++ showHex imm)
 where
  -- search for a block in the current cfg that starts at @imm@, and if found, make a label for it
  label_in_current_cfg = ((block_label entry . fst) <$> find block_starts_at (IM.toList $ cfg_instrs cfg))

  block_starts_at (blockId, instrs) = instrs /= [] && instr_addr (head instrs) == fromIntegral imm




orTry :: Maybe a -> Maybe a -> Maybe a
orTry Nothing x  = x
orTry (Just x) _ = Just x

finally :: Maybe a -> a -> a
finally Nothing a  = a
finally (Just a) _ = a








 
-- | Showing unresolved address (inner part within a ptr[...])
--
-- Always of form [ seg: reg + reg*scale + number ]
show_nasm_address''' (AddressStorage r) = show r
show_nasm_address''' (AddressImm i) = show i
show_nasm_address''' (AddressPlus (AddressStorage r) (AddressImm i))  = show r ++ " + " ++ show i
show_nasm_address''' (AddressMinus (AddressStorage r) (AddressImm i)) = show r ++ " + " ++ show (0 -i)
show_nasm_address''' (AddressPlus (AddressStorage r) (AddressStorage r1)) = show r ++ " + " ++ show r1
show_nasm_address''' (AddressPlus (AddressPlus (AddressStorage r) (AddressStorage r1)) (AddressImm i)) = show r ++ " + " ++ show r1 ++ " + " ++ show i
show_nasm_address''' (AddressPlus (AddressStorage r) (AddressPlus (AddressStorage r1) (AddressImm i))) = show r ++ " + " ++ show r1 ++ " + " ++ show i
show_nasm_address''' (AddressPlus (AddressStorage r) (AddressMinus (AddressStorage r1) (AddressImm i))) = show r ++ " + " ++ show r1 ++ show (0-1)
show_nasm_address''' (AddressPlus (AddressTimes (AddressStorage r) (AddressImm i)) (AddressImm i1)) = show r ++ " * " ++ show i ++ " * " ++ show i1
show_nasm_address''' (AddressPlus (AddressStorage r) (AddressTimes (AddressStorage r1) (AddressImm i))) = show r ++ " + " ++ show r1 ++ " * " ++ show i
show_nasm_address''' (AddressPlus (AddressStorage r) (AddressPlus (AddressTimes (AddressStorage r1) (AddressImm i)) (AddressImm i1))) = show r ++ " + " ++ show r1 ++ " * " ++ show i ++ " + " ++ show i1
show_nasm_address''' (AddressPlus (AddressStorage r) (AddressMinus (AddressTimes (AddressStorage r1) (AddressImm i)) (AddressImm i1))) = show r ++ " + " ++ show r1 ++ " * " ++ show i ++ " + " ++ show (0-i1)
show_nasm_address''' (AddressTimes (AddressStorage r) (AddressImm i))  = show r ++ " * " ++ show i
show_nasm_address''' a           = error $ "TODO: " ++ show a



show_nasm_address'' a@(AddressPlus (AddressStorage r) a') = if is_segment_register r then show r ++ ":" ++ show_nasm_address''' a' else show_nasm_address''' a 
show_nasm_address'' a                                     = show_nasm_address''' a

is_segment_register r = r `elem` [CS,DS,ES,FS,GS,SS]

show_nasm_address ctxt entry cfg i address =
  case (symbolize_immediate ctxt . fromIntegral) <$> rip_relative_to_immediate address of
    Just (Just symbolized) -> "rel " ++ symbolized
    _                      -> show_nasm_address'' address
 where
  rip_relative_to_immediate (AddressImm imm)                          = Just $ imm
  rip_relative_to_immediate (AddressPlus (AddressStorage RIP) (AddressImm imm)) = Just $ fromIntegral (instr_addr i) + fromIntegral (instr_size i) + fromIntegral imm
  rip_relative_to_immediate (AddressPlus (AddressImm imm) (AddressStorage RIP)) = Just $ fromIntegral (instr_addr i) + fromIntegral (instr_size i) + fromIntegral imm
  rip_relative_to_immediate _                                      = Nothing


symbolize_immediate :: Context -> Int -> Maybe String
symbolize_immediate ctxt a =
  (pointer_to_instruction $ fromIntegral a)
  `orTry`
  (symbol_from_symbol_table a)
  `orTry`
  (relative_to_section a)
 where
  pointer_to_instruction a = 
    if address_has_instruction ctxt a then
      uncurry block_label <$> find_block_for_instruction ctxt a 
    else
      Nothing

  symbol_from_symbol_table a = IM.lookup (fromIntegral a) $ ctxt_syms ctxt

  relative_to_section a = 
    show_section_relative a <$> find_section_for_address ctxt a

  show_section_relative a sec@(segment,section,a0,_) =
    if is_literal_string_section sec then
      section_label segment section ++ "_" ++ show (a - a0)
    else
      section_label segment section ++ " + " ++ show (a - a0)


find_block_for_instruction ctxt a =
  if address_has_symbol ctxt a then
    Nothing
  else case catMaybes $ map (find_block_for_instruction_address_in_cfg a) $ IM.assocs $ ctxt_cfgs ctxt of
    [b] -> Just b
    x   -> trace ("Address " ++ showHex a ++ " has instruction but no block") $ Nothing
  
find_block_for_instruction_address_in_cfg a (entry,cfg) = 
  return_entry_and_blockID <$> (find (\(blockID,instrs) -> instr_addr (head instrs) == a) $ IM.toList $ cfg_instrs cfg)
 where
  return_entry_and_blockID (blockID,instrs) = (entry,blockID)




-- | Showing size directive
show_nasm_size_directive 1  = "byte"
show_nasm_size_directive 2  = "word"
show_nasm_size_directive 4  = "dword"
show_nasm_size_directive 8  = "qword"
show_nasm_size_directive 10 = "tword"
show_nasm_size_directive 16 = "oword"
show_nasm_size_directive x  = error $ show ("unknown size directive", x)


-- | Showing an operand
show_nasm_operand ctxt entry cfg i (Memory a si)        = show_nasm_size_directive si ++ " [" ++ show_nasm_address ctxt entry cfg i a ++ "]"
show_nasm_operand ctxt entry cfg i (EffectiveAddress a) = "[" ++ show_nasm_address ctxt entry cfg i a ++ "]"
show_nasm_operand ctxt entry cfg i (Storage r)          = show r
show_nasm_operand ctxt entry cfg i (Immediate imm)      = show imm


section_label "" ".data"      = data_section_label
section_label segment section = "L" ++ segment ++ "_" ++ section

data_section_label = "L_DATASECTION"
jump_table_label i_a = "L_JUMP_TABLE_" ++ showHex i_a


show_nasm_opcode MOVABS = "MOV"
show_nasm_opcode opcode = show opcode

show_nasm_instruction ctxt entry cfg  i@(Instruction addr pre opcode Nothing ops annot) =
     show pre
  ++ show_nasm_opcode opcode
  ++ " "
  ++ intercalate ", "  (map (show_nasm_operand ctxt entry cfg i) ops)



