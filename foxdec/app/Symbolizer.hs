{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}

module Symbolizer where


import Base
import CFG_Gen
import Context
import X86_Datastructures
import VerificationReportInterface
import ControlFlow

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
  concatMap mk_literal_string_section $ filter is_literal_string_section $ ctxt_sections ctxt
 where
  mk_literal_string_section (segment,section,a0,sz) = 
    let dat = map (\offset -> IM.lookup (a0+offset) $ ctxt_dump ctxt) [0..sz-1] in
      if any ((==) Nothing) dat then
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
  intercalate "\n" $ map ((++) "extern ") $ nub $ IM.elems $ ctxt_syms ctxt

symbolize_entry ctxt entry =
  header_comment ++ symbolized_cfg
 where
  header_comment = comment_block ["Function: " ++ function_name_of_entry ctxt entry]
  symbolized_cfg =
      case IM.lookup entry (ctxt_cfgs ctxt) of
        Just cfg -> symbolize_cfg ctxt entry cfg


bss_data_section ctxt = 
  concatMap mk_bss_data_section $ filter is_bss_data_section $ ctxt_sections ctxt
 where
  mk_bss_data_section (segment,section,a0,sz) = section_bss ++ "\n" ++ section_label segment section ++ ": resb " ++ show sz ++ "\n\n"
  
is_bss_data_section ("__DATA","__bss",_,_) = True
is_bss_data_section ("__DATA","__common",_,_) = True
is_bss_data_section _ = False



data_section ctxt =
  mk_data_section $ ctxt_data ctxt
 where
  mk_data_section dat = intercalate "\n" $ (section_data : data_section_label : (map mk_data_entry $ IM.assocs dat)) ++ [""] -- check whether contiguous 
  mk_data_entry (a,v) = "db 0" ++ showHex v ++ "h" 

  data_section_label = section_label "__DATA" "__data" ++ ":"


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
  block_header = comment $ "Entry " ++ showHex entry ++ "; block " ++ show blockID ++ "; address " ++ showHex (i_addr $ head instrs)
  symbolyzed_block = [block_label'] ++ block_body ++ [block_end]

  block_label' = block_label entry blockID ++ ":"
  block_body   = map (indent . symbolize_instr ctxt entry cfg) instrs
  block_end    = if instrs == [] || is_proper_block_end_instruction (i_opcode $ last instrs) then "" else mk_extra_jmp

  indent str = "    " ++ str

  is_proper_block_end_instruction i = is_ret i || is_jump i 

  mk_extra_jmp =
    case unsafePerformIO $ stepA ctxt entry (i_addr $ last instrs) of -- TODO, also TODO assumes fall trhough is last/second
      Right []        -> ""
      Right [(a,_)]   -> indent $ "JMP " ++ (symbolize_address ctxt entry cfg $ fromIntegral a) ++ "     ; inserted\n"
      Right [_,(a,_)] -> indent $ "JMP " ++ (symbolize_address ctxt entry cfg $ fromIntegral a) ++ "     ; inserted\n"
      _               -> "JMP ERROR CANNOT DETERMINE NEXT BLOCK"


block_label entry blockID = "L" ++ showHex entry ++ "_" ++ show blockID



symbolize_instr ctxt entry cfg i = 
  if is_call (i_opcode i) || is_jump (i_opcode i) || is_cond_jump (i_opcode i) then
    symbolize_operand1_of_instr ctxt entry cfg i
  else
    show_nasm_instruction ctxt entry cfg  i





symbolize_operand1_of_instr ctxt entry cfg i@(Instr i_a Nothing mnemonic op1 op2 op3 annot si) =
 case resolve_jump_target ctxt i of
   [External sym]         -> show mnemonic ++ " " ++ sym
   [ImmediateAddress imm] -> show mnemonic ++ " " ++ symbolize_address ctxt entry cfg imm
   trgts                  -> show_nasm_instruction ctxt entry cfg  i ++ "; TARGETS: " ++ (intercalate "," $ map show_trgt trgts)

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

  block_starts_at (blockId, instrs) = instrs /= [] && i_addr (head instrs) == fromIntegral imm




orTry :: Maybe a -> Maybe a -> Maybe a
orTry Nothing x  = x
orTry (Just x) _ = Just x

finally :: Maybe a -> a -> a
finally Nothing a  = a
finally (Just a) _ = a








 
-- | Showing unresolved address (inner part within a ptr[...])
--
-- Always of form [ reg + reg*scale + number ]
show_nasm_address'' (AddrReg r) = show r
show_nasm_address'' (AddrImm i) = show i
show_nasm_address'' (AddrPlus (AddrReg r) (AddrImm i))  = show r ++ " + " ++ show i
show_nasm_address'' (AddrMinus (AddrReg r) (AddrImm i)) = show r ++ " + " ++ show (0 -i)
show_nasm_address'' (AddrPlus (AddrReg r) (AddrReg r1)) = show r ++ " + " ++ show r1


show_nasm_address'' (AddrPlus (AddrTimes (AddrReg r) (AddrImm i)) (AddrImm i1)) = show r ++ " * " ++ show i ++ " * " ++ show i1
show_nasm_address'' (AddrPlus (AddrReg r) (AddrTimes (AddrReg r1) (AddrImm i))) = show r ++ " + " ++ show r1 ++ " * " ++ show i
show_nasm_address'' (AddrPlus (AddrReg r) (AddrPlus (AddrTimes (AddrReg r1) (AddrImm i)) (AddrImm i1))) = show r ++ " + " ++ show r1 ++ " * " ++ show i ++ " + " ++ show i1
show_nasm_address'' (AddrPlus (AddrReg r) (AddrMinus (AddrTimes (AddrReg r1) (AddrImm i)) (AddrImm i1))) = show r ++ " + " ++ show r1 ++ " * " ++ show i ++ " + " ++ show (0-i1)

show_nasm_address'' a           = error $ "TODO: " ++ show a


show_nasm_address' ctxt entry cfg  i address =
  case rip_relative_to_immediate address of
    Just imm -> "rel " ++ (symbolize_immediate $ fromIntegral imm)
    Nothing  -> show_nasm_address'' address
 where
  rip_relative_to_immediate (AddrImm imm)                          = Just $ imm
  rip_relative_to_immediate (AddrPlus (AddrReg RIP) (AddrImm imm)) = Just $ fromIntegral (i_addr i) + fromIntegral (i_size i) + fromIntegral imm
  rip_relative_to_immediate (AddrPlus (AddrImm imm) (AddrReg RIP)) = Just $ fromIntegral (i_addr i) + fromIntegral (i_size i) + fromIntegral imm
  rip_relative_to_immediate _                                      = Nothing


  symbolize_immediate a =
    (pointer_to_instruction a)
    `orTry`
    (symbol_from_symbol_table a)
    `orTry`
    (relative_to_section a)
    `finally`
    (show_nasm_address'' address)
    
  pointer_to_instruction a = 
    if address_has_instruction ctxt a then
      uncurry block_label <$> find_block_for_instruction ctxt cfg a 
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


find_block_for_instruction ctxt cfg a =
  case catMaybes $ map (find_block_for_instruction_address_in_cfg a) $ IM.assocs $ ctxt_cfgs ctxt of
    [b] -> Just b
    _   -> Nothing
  
find_block_for_instruction_address_in_cfg a (entry,cfg) = 
  pair entry <$> (find (\instrs -> i_addr (head instrs) == a) $ cfg_instrs cfg)


--TODO move
pair a b = (a,b)



-- | Showing size directive
show_nasm_size_directive 1  = "byte"
show_nasm_size_directive 2  = "word"
show_nasm_size_directive 4  = "dword"
show_nasm_size_directive 8  = "qword"
show_nasm_size_directive 16 = "oword"

-- | Showing unresolved address
show_nasm_address ctxt entry cfg i (SizeDir si a) = show_nasm_size_directive si ++ " [" ++ show_nasm_address' ctxt entry cfg  i a ++ "]"
show_nasm_address ctxt entry cfg i a = "[" ++ show_nasm_address' ctxt entry cfg  i a ++ "]"


-- | Showing an operand
show_nasm_operand' ctxt entry cfg  i (Address a)     = show_nasm_address ctxt entry cfg  i a
show_nasm_operand' ctxt entry cfg  i (Reg r)         = show r
show_nasm_operand' ctxt entry cfg  i (Immediate imm) = show imm

-- | Showing an optional operand
show_nasm_operand ctxt entry cfg  i Nothing   = ""
show_nasm_operand ctxt entry cfg  i (Just op) = show_nasm_operand' ctxt entry cfg  i op


section_label segment section = "L" ++ segment ++ "_" ++ section

show_nasm_instruction ctxt entry cfg  i@(Instr addr pre opcode op1 op2 op3 annot size) =
     show_prefix pre
  ++ show opcode
  ++ " "
  ++ show_nasm_operand ctxt entry cfg  i op1
  ++ show_nasm_operand2 op2
  ++ show_nasm_operand2 op3
 where
  show_nasm_operand2 Nothing = ""
  show_nasm_operand2 (Just op) = ", " ++ show_nasm_operand ctxt entry cfg  i (Just op)




