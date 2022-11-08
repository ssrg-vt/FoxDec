{-# LANGUAGE PartialTypeSignatures , FlexibleContexts #-}

module Symbolizer where


import Base
import Pass.CFG_Gen
import Data.Binary
import X86.Conventions
import X86.Register
import Data.Pointers
import Data.SimplePred
import Analysis.SymbolicExecution (get_invariant)
import Data.MachineState (read_operand,write_reg)

import Analysis.Context
import VerificationReportInterface
import Data.ControlFlow
import X86.Register (Register(..))
import Generic.Instruction (GenericInstruction(..))
import X86.Opcode (Opcode(..), isCall, isJump, isCondJump, isRet)
import qualified X86.Instruction as X86
import qualified Generic.Instruction as Instr
import Typeclasses.HasSize(sizeof)
import Typeclasses.HasAddress(addressof)
import Generic.Operand (GenericOperand(..))
import Generic.Address (GenericAddress(..),AddressWord64(AddressWord64))

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List 
import Data.Word
import Data.Foldable
import Data.Char (chr,showLitChar)
import Data.Bits (testBit)
import Data.List.Split (splitOn)

import Control.Monad.State.Strict
import Data.Functor.Identity
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Unsafe


import Debug.Trace

usage_msg = "Usage:\n\n  foxdec-symbolizer-exe BNAME NAME.report\n\nHere BNAME refers to the original binary and NAME refers to the NAME used when running foxdec-exe.\nRun this program from the same directory foxdec-exe was run."

-- | Read in first command-line argument, check whether it is a .report file.
-- Read the file, and use the "VerificationReportInterface" to get an overview of the instructions.
main = do
  args <- getArgs
  if length args /= 2 then
    putStrLn usage_msg
  else do
    exists <- doesFileExist $ args !! 1
    if exists then do
      ctxt  <- ctxt_read_report (args !! 0) (args !! 1)

      entries <- retrieve_io $ ctxt_get_function_entries ctxt
      let symbolized_entries = map (symbolize_entry ctxt) $ S.toList entries

      putStrLn $ externals ctxt ++ "\n\n"
      putStrLn $ "global _start\n\n"
      putStrLn $ section_text ++ "\n"
      putStrLn $ "default rel" ++ "\n" -- needed for MachO
      putStrLn $ intercalate "\n\n\n" symbolized_entries ++ "\n\n\n"
      putStrLn $ literal_string_data_section ctxt
      putStrLn $ bss_data_section ctxt
      putStrLn $ data_section ctxt
      putStrLn $ ro_data_section ctxt
      -- putStrLn $ jump_tables ctxt
    else
      putStrLn $ "File: " ++ show (args !! 1) ++ " does not exist."
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
  mk_literal_string_section sec@(segment,section,a0,sz) = 
    case binary_read_ro_data (ctxt_binary ctxt) a0 (fromIntegral sz) of
      Nothing  -> mk_error sec
      Just dat -> if length dat /= fromIntegral sz then mk_error sec else mk_data sec dat

  mk_error sec = error $ "Cannot read data of section: " ++ show sec
  mk_data (segment,section,a0,sz) dat = intercalate "\n" $ ([section_data] ++ literal_strings segment section a0 0 dat ++ [""])


  literal_strings segment section a0 offset []  = []
  literal_strings segment section a0 offset dat = 
    let (str,remainder) = break ((==) 0) dat in
      mk_string_entry segment section a0 offset str : literal_strings segment section a0 (offset + length str + 1) (tail remainder)

  mk_string_entry segment section a0 offset str = section_label segment section offset ++ ": db `" ++  concatMap charToC str ++ "`, 0" -- TODO ",0" for 0-ending?


  charToC w = 
    let c = showLitChar (chr $ fromIntegral w) "" in
      if c == "`" then "\\`" else c


is_literal_string_section ("__TEXT","__cstring",_,_) = True
is_literal_string_section _ = False

externals ctxt =
  intercalate "\n" $ map ((++) "extern ") $ filter ((/=) "") $ map strip_GLIBC $ nub $ IM.elems $ ctxt_syms ctxt

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
  mk_bss_data_section (segment,section,a0,sz) = section_bss ++ "\n" ++ section_label segment section a0 ++ ":\n" ++ mk_bss a0 sz

  mk_bss a0 sz =
    case sort $ map get_addr $ filter (was_relocated_and_in a0 sz) $ ctxt_relocs ctxt of
      [] ->  "resb " ++ show sz ++ "\n\n"
      (a:_) -> "resb " ++ show (fromIntegral a - a0) ++ "\n" ++ mk_reloc_symbol (R_X86_64_RELATIVE 0 a) ++ ":\n" ++ mk_bss (fromIntegral a) (sz + a0 - fromIntegral a)


  get_addr (R_X86_64_RELATIVE _ a) = a
  was_relocated_and_in a0 sz (R_X86_64_RELATIVE _ a) = fromIntegral a0 < a && a < fromIntegral (a0 + sz)-- TODO note strict inequality here
  

 
-- TODO get from Binary interface
is_bss_data_section ("__DATA","__bss",_,_) = True
is_bss_data_section ("__DATA","__common",_,_) = True
is_bss_data_section ("",".bss",_,_) = True
is_bss_data_section _ = False


data_section ctxt =  
  concatMap mk_data_section $ filter is_data_section $ si_sections $ ctxt_sections ctxt
 where
  mk_data_section (segment,section,a0,sz) = intercalate "\n" $ (section_data : (section_label segment section a0 ++ ":") : mk_data_entries 0 a0 sz) ++ ["\n\n"]

  mk_data_entries offset a0 sz 
    | offset >= sz = []
    | otherwise = optional_label_for_reloc (a0 + offset) ++ 
        case find (is_reloc_for $ fromIntegral (a0 + offset)) $ ctxt_relocs ctxt of
          Just (R_X86_64_RELATIVE _ a1) ->  ["dq " ++ fromJust (symbolize_immediate ctxt True $ fromIntegral a1)] ++ mk_data_entries (offset+8) a0 sz
          Nothing -> case binary_read_data (ctxt_binary ctxt) (fromIntegral $ a0 + offset) 1 of
                       Just [v] -> ["db 0" ++ showHex v ++ "h"] ++ mk_data_entries (offset+1) a0 sz
                       dat -> error $ "Could not read data from address: " ++ showHex (a0 + offset) ++ ": " ++ show dat

  optional_label_for_reloc a1 =
    case find (was_relocated $ fromIntegral a1) $ ctxt_relocs ctxt of
      Nothing -> []
      Just reloc -> [mk_reloc_symbol reloc ++ ":"]


is_reloc_for a0 (R_X86_64_RELATIVE a0' _) = a0 == a0'
was_relocated a1 (R_X86_64_RELATIVE a0' a1') = a1 == a1'

mk_reloc_symbol (R_X86_64_RELATIVE a0 a1) = "L_0x" ++ showHex a1


-- TODO get from Binary interface
--is_data_section ("__DATA","__got",_,_) = True
--is_data_section ("__DATA","__la_symbol_ptr",_,_) = True
is_data_section ("__DATA","__data",_,_) = True
is_data_section ("",".data",_,_) = True
is_data_section _ = False



ro_data_section ctxt = 
  concatMap mk_ro_data_section $ filter is_ro_data_section $ si_sections $ ctxt_sections ctxt
 where
  mk_ro_data_section (segment,section,a0,sz) = intercalate "\n" $ (section_data : (section_label segment section a0 ++ ":") : mk_data_entries 0 a0 sz) ++ ["\n\n"]    --  (map (mk_data_entry $ fromIntegral a0) [0..sz-1])) ++ ["\n\n"] 

  mk_data_entries offset a0 sz 
    | offset >= sz = []
    | otherwise = optional_label_for_reloc (a0 + offset) ++
        case find (is_reloc_for $ fromIntegral (a0 + offset)) $ ctxt_relocs ctxt of
          Just (R_X86_64_RELATIVE _ a1) ->  ["dq " ++ fromJust (symbolize_immediate ctxt True $ fromIntegral a1)] ++ mk_data_entries (offset+8) a0 sz
          Nothing -> case binary_read_ro_data (ctxt_binary ctxt) (fromIntegral $ a0 + fromIntegral offset) 1 of
                       Just [v] -> ["db 0" ++ showHex v ++ "h"] ++ mk_data_entries (offset+1) a0 sz

  optional_label_for_reloc a1 =
    case find (was_relocated $ fromIntegral a1) $ ctxt_relocs ctxt of
      Nothing -> []
      Just reloc -> [mk_reloc_symbol reloc ++ ":"]
  
is_ro_data_section ("",".rodata",_,_) = True
is_ro_data_section ("",".init_array",_,_) = True
is_ro_data_section ("",".fini_array",_,_) = True
is_ro_data_section ("",".data.rel.ro",_,_) = True
is_ro_data_section ("__DATA","__const",_,_) = True
is_ro_data_section _ = False


{--
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
--}

comment str = ";" ++ str

comment_block strs =
   intercalate "\n" $ comment_block_delim : (map comment strs ++ [comment_block_delim,""])
 where
  comment_block_delim = comment $ replicate (length max) '-' 
  max = maximumBy compare_lengths strs
  compare_lengths str0 str1 = compare (length str0) (length str1)

data NASM = NASM_Instruction String | NASM_Label String | NASM_Indirection_Block [NASM] | NASM_Comment String


symbolize_cfg ctxt entry cfg = 
  render_nasm ctxt $ concatMap (symbolize_block ctxt entry cfg) (IM.toList $ cfg_instrs cfg)


-- rendering a NASM data structure
render_nasm ctxt nasm =
  let (normal,ind_blocks) = partition is_normal nasm in
    (intercalate "\n" $ map render_nasm' normal)
    ++
    (if null ind_blocks then "" else intercalate "\n\n\n" $ "" : map render_nasm_block ind_blocks)
 where
  render_nasm' (NASM_Instruction str) = indent str
  render_nasm' (NASM_Label str) = str ++ ":" 
  render_nasm' (NASM_Comment str) = comment str

  render_nasm_block (NASM_Indirection_Block nasm) = intercalate "\n" $ map render_nasm' nasm

  is_normal (NASM_Indirection_Block _) = False
  is_normal _ = True


symbolize_block ctxt entry cfg (blockID, instrs) = 
  block_header : symbolyzed_block
 where
  block_header = NASM_Comment $ "Entry " ++ showHex entry ++ "; block " ++ show blockID ++ "; address " ++ showHex (addressof $ head instrs)
  symbolyzed_block = block_label' ++ block_body ++ block_end

  block_label'          = [NASM_Label $ block_label entry blockID] ++ optional_start_symbol
  block_body            = concatMap (symbolize_instr ctxt entry cfg) $ filter_unnecessary_jumps instrs
  block_end             = if instrs == [] || is_proper_block_end_instruction (Instr.opcode $ last instrs) then [] else [mk_extra_jmp]
  optional_start_symbol = if instrs == [] || addressof (head instrs) /= fromIntegral (ctxt_start ctxt) then [] else [NASM_Label "_start"]

  filter_unnecessary_jumps instrs = (filter (not . isJump . Instr.opcode) $ init instrs) ++ [last instrs]


  is_proper_block_end_instruction i = isRet i || isJump i 

  mk_extra_jmp =
    case unsafePerformIO $ stepA ctxt entry (fromIntegral $ addressof $ last instrs) of -- TODO, also TODO assumes fall through is last/second
      Right []        -> NASM_Instruction $ ""
      Right [(a,_)]   -> NASM_Instruction $ "JMP " ++ (symbolize_address ctxt entry False cfg $ fromIntegral a) ++ "     ; inserted\n"
      Right [_,(a,_)] -> NASM_Instruction $ "JMP " ++ (symbolize_address ctxt entry False cfg $ fromIntegral a) ++ "     ; inserted\n"
      x               -> NASM_Instruction $ "JMP ERROR CANNOT DETERMINE NEXT BLOCK" ++ show x


block_label entry blockID = "L" ++ showHex entry ++ "_" ++ show blockID

indent str = "    " ++ str





operands i =
  case dest i of
    Nothing  -> srcs i
    Just dst -> dst : srcs i

is_mem_operand (Memory a si) = True
is_mem_operand    _          = False



mk_indirection_label entry (Instruction (AddressWord64 i_a) prefix mnemonic Nothing [op] annot) = "L_INDIRECTION_0x" ++ showHex entry ++ "_0x" ++ showHex i_a ++ "_" ++ make_valid_label (show op)

make_valid_label :: String -> String
make_valid_label l = foldr do_replace l replacements
 where
  do_replace (old,new) = replace old new 
  replacements = [("*","mult"), ("+","plus"), (" ",""), ("[", "_"), ("]", "_"), (",","_") ]
 

replace old new = intercalate new . splitOn old
  



data TranslateAction = NormalTranslation String | SymbolizeMemWriteWithImmBase SimpleExpr Word64 | DirectTranslation String Word64 | TranslateUnresolved SimpleExpr 
  deriving Show



symbolize_instr ctxt entry cfg i = 
  if isCall (Instr.opcode i) || isJump (Instr.opcode i) || isCondJump (Instr.opcode i) then
    runtime_symbolize_jump_call ctxt entry cfg i
  else if opcode i == NOP then
    []
  else if opcode i == CMPSB then
    runtime_symbolize_cmpsb ctxt entry cfg i
  else case filter is_mem_operand $ operands i of
    []            -> [show_nasm_instruction ctxt entry cfg i ""]
    [Memory a si] -> case choose_translate_action_for_mem_access ctxt entry cfg i a si of
                       NormalTranslation msg -> [show_nasm_instruction ctxt entry cfg i msg]
                       DirectTranslation str imm -> [show_nasm_instruction_direct ctxt entry cfg i str $ "symbolization of " ++ showHex imm]
                       TranslateUnresolved ptr -> runtime_symbolize_unresolved_mem_access ctxt entry cfg i (Memory a si) ptr
                       SymbolizeMemWriteWithImmBase ptr imm -> runtime_symbolize_mem_write_with_imm_base ctxt entry cfg i (Memory a si) ptr $ fromIntegral imm
    [Memory a0 si0, Memory a1 si1] -> case (choose_translate_action_for_mem_access ctxt entry cfg i a0 si0, choose_translate_action_for_mem_access ctxt entry cfg i a1 si1) of
                                        (NormalTranslation msg0, NormalTranslation msg1) -> [show_nasm_instruction ctxt entry cfg i (msg0 ++ ", " ++ msg1)]
                                        t -> [show_nasm_instruction ctxt entry cfg i $ "UNRESOLVED: " ++ show t]
    _ -> error $ "Instruction with mulitple memory operands:" ++ show i

choose_translate_action_for_mem_access ctxt entry cfg i a si =
  case symbolize_immediate ctxt True <$> fromIntegral <$> rip_relative_to_immediate i a of
    Just (Just sym) -> NormalTranslation "concrete"
    _ -> choose_translate_action ctxt entry i (EffectiveAddress a)


-- TODO: R12 == Bot[src|d0a0,d0a8,d0b0,d0b8,d0c8,d0e0,d100,d110,d111,d112,d113,d114,d118,d120,d128,d130,d138,d150|]
choose_translate_action ctxt entry i op =
  let fctxt    = mk_fcontext ctxt entry
      Right p  = ctxt_get_invariant entry (fromIntegral $ addressof i) ctxt
      ptr      = evalState (writeRIP fctxt i >> read_operand fctxt op) (p,S.empty)
      domain   = get_pointer_domain fctxt ptr in
    case ptr of
      SE_Immediate imm -> (mk_direct_translation imm <$> symbolize_immediate ctxt True (fromIntegral imm)) `finally` choose_translate_action_based_on_domain fctxt domain ptr
      _ -> choose_translate_action_based_on_domain fctxt domain ptr
 where
  mk_direct_translation imm str = DirectTranslation ("["  ++ str ++ "]") imm

  choose_translate_action_based_on_domain fctxt domain ptr =
    case expr_is_based_in_data_section ctxt ptr of
      Just imm -> SymbolizeMemWriteWithImmBase ptr imm
      _ -> if domain_is_pointer_to_external_symbol domain then
             NormalTranslation $ "external symbol: " ++ show domain
           else if expr_is_return_value_external_function ctxt ptr then
             NormalTranslation $ "return value: " ++ show ptr
           else if domain_is_heap fctxt ptr domain then
             NormalTranslation $ "heap pointer" 
           else if expr_is_highly_likely_local_pointer fctxt ptr then
             NormalTranslation $ "local" 
           else if expr_is_segment_plus_offset ptr then
             NormalTranslation "segment-based"
           else
             TranslateUnresolved ptr




-- CALLS/JUMPS
-- * External calls require runtime symbolization of their parameters. The call itself is relative to the PLT.
-- * Internal calls to immediate addresses are symbolized statically.
-- * Unresolved indirections are translated to calls/jumps to a dedicated indirection block.
runtime_symbolize_jump_call ctxt entry cfg i@(Instruction (AddressWord64 i_a) prefix mnemonic Nothing ops@[op] annot) =
 case resolve_jump_target ctxt i of
   [External sym]         -> runtime_symbolize_parameters ctxt entry cfg i (strip_GLIBC sym) ++ [NASM_Instruction $ show mnemonic ++ " " ++ strip_GLIBC sym ++ " wrt ..plt"]
   [ImmediateAddress imm] -> map NASM_Instruction [show mnemonic ++ " " ++ symbolize_address ctxt entry is_call cfg imm]
   trgts                  -> (map NASM_Instruction [show mnemonic ++ " " ++ mk_indirection_label entry i]) ++ [mk_indirection_block trgts]
 where
  is_call = isCall mnemonic
  mk_indirection_block trgts = NASM_Indirection_Block $ [NASM_Label $ mk_indirection_label entry i] ++ body trgts
  body trgts = 
    let reg = find_element_not_in register_set (regs_of_ops ops) in
      if all is_immediate trgts then
        mk_lookup_body reg $ nub trgts
      else
        [NASM_Instruction $ ";; UNRESOLVED INDIRECTION: " ++ show i ++ " WITH TARGETS " ++ show trgts]
        ++
        symbolize_call_instruction -- symbolize_instr ctxt entry cfg (Instruction (AddressWord64 i_a) Nothing MOV Nothing [Storage RDI, op] Nothing)
        ++
        [NASM_Instruction $ "HLT"]

  symbolize_call_instruction =
    case op of
      (Storage reg) -> [show_nasm_instruction ctxt entry cfg i ""]
      (Memory a si) -> case choose_translate_action_for_mem_access ctxt entry cfg i a si of
                         NormalTranslation msg -> [show_nasm_instruction ctxt entry cfg i ""]
                         SymbolizeMemWriteWithImmBase ptr imm -> (NASM_Instruction $ "LEA GS, " ++ show_nasm_operand ctxt entry cfg i op) : (runtime_symbolize_param_with_imm_base ctxt entry i (GS) ptr $ fromIntegral imm) ++ [NASM_Instruction $ "JMP qword [GS]"]



  mk_lookup_body reg trgts = [NASM_Instruction $ "PUSH " ++ show reg] ++ mk_lookup_jmps reg trgts ++ mk_jmps reg trgts

  mk_lookup_jmps reg [] = [NASM_Instruction "hlt\n\n"]
  mk_lookup_jmps reg (ImmediateAddress trgt:trgts) = mk_lookup_jmp reg trgt ++ mk_lookup_jmps reg trgts

  mk_lookup_jmp reg trgt = map NASM_Instruction [ 
                         "MOV " ++ show reg ++ ", 0x" ++ showHex trgt,
                         "CMP " ++ show op ++ ", " ++ show reg,
                         "JE " ++ label_for_jmp trgt
                       ]

  mk_jmps reg [] = [NASM_Instruction "hlt\n\n"]
  mk_jmps reg (ImmediateAddress trgt:trgts) = mk_jmp reg trgt ++ mk_jmps reg trgts

  mk_jmp reg trgt = [NASM_Label $ label_for_jmp trgt] ++ map NASM_Instruction [
                      "POP " ++ show reg,
                      "JMP " ++ symbolize_address ctxt entry False cfg trgt
                    ]


  is_immediate (ImmediateAddress a) = True
  is_immediate _                    = False
  
  label_for_jmp trgt = mk_indirection_label entry i ++ "_" ++ symbolize_address ctxt entry False cfg trgt





-- Runtime symbolization of parameters
runtime_symbolize_parameters ctxt entry cfg i sym = 
  let params = pointer_params sym in
    if params == [] then [] else concatMap (runtime_symbolize_parameter ctxt entry i) params


runtime_symbolize_parameter ctxt entry i reg =
  case choose_translate_action ctxt entry i (Storage reg) of
    NormalTranslation msg -> [NASM_Instruction $ "; " ++ show reg ++ ": " ++ msg]
    DirectTranslation str imm -> [NASM_Instruction $ "LEA " ++ show reg ++ ", " ++ str ++ "; runtime symbolization of 0x" ++ showHex imm]
    SymbolizeMemWriteWithImmBase ptr imm -> runtime_symbolize_param_with_imm_base ctxt entry i reg ptr $ fromIntegral imm
    TranslateUnresolved ptr -> runtime_symbolize_unresolved_param ctxt entry i ptr reg


runtime_symbolize_param_with_imm_base ctxt entry i@(Instruction (AddressWord64 i_a) prefix mnemonic Nothing ops annot) r ptr imm_base = map NASM_Instruction
  [
    ";; Runtime symbolization of parameter " ++ show r ++ " with base in data section: " ++ showHex imm_base,
    ";; " ++ show r ++ " == " ++ show ptr,
    "PUSH " ++ show reg1,
    "LEA " ++ show reg1 ++ ", [rel " ++ sec_label ++ "]",
    "LEA " ++ show r ++ ", [" ++ show r ++ " + " ++ show reg1 ++ "]",
    "MOV " ++ show reg1 ++ ", -0x" ++ showHex imm_base,
    "LEA " ++ show r ++ ", [" ++ show r ++ " + " ++ show reg1 ++ "]",
    "POP " ++ show reg1,
    ";;"
   ]
 where
  reg1 = find_element_not_in register_set (regs_of_ops ops)
  Just (sec_label,sec_offset) = show_section_relative <$> find_section_for_address ctxt imm_base

  show_section_relative sec@(segment,section,a0,_) =
    (section_label segment section a0, imm_base - a0)

runtime_symbolize_unresolved_param ctxt entry i@(Instruction (AddressWord64 i_a) prefix mnemonic Nothing ops annot) ptr r =
  map NASM_Instruction [
    ";; Runtime symbolization of unresolved parameter " ++ show r,
    ";; " ++ show ptr
  ]
 ++
  concatMap mk_section (si_sections $ ctxt_sections ctxt)
 ++
  [NASM_Instruction ";;"]
 where
  mk_section sec@(segment,section,a0,si) =
    if is_symbolizable_section sec then
      map NASM_Instruction 
       [
        "CMP " ++ show r ++ ", 0x" ++ showHex a0,
        "JB " ++ mk_label sec,
        "CMP " ++ show r ++ ", 0x" ++ showHex (a0 + fromIntegral si),
        "JAE " ++ mk_label sec,
        "PUSH " ++ show reg1,
        "LEA " ++ show reg1 ++ ", [rel " ++ section_label segment section a0 ++ "]",
        "LEA " ++ show r ++ ", [" ++ show r ++ " + " ++ show reg1 ++ " - 0x" ++ showHex a0 ++ "]",
        "POP " ++ show reg1
        -- TODO jump to end
       ]
      ++
       [NASM_Label $ mk_label sec]
    else -- TODO other sections
      []

  mk_label (_,_,a0,si) = "L_0x" ++ showHex entry ++ "_0x" ++ showHex (addressof i) ++ "_" ++ show r ++ "_0x" ++ showHex a0
  reg1 = find_element_not_in register_set (r : regs_of_ops ops)


-- TODO unnecessary push/pop, sometimes
runtime_symbolize_cmpsb ctxt entry cfg i =
  init ++ pushes ++ runtime_symbolize_parameter ctxt entry i RSI ++ runtime_symbolize_parameter ctxt entry i RDI ++ instruction ++ pops ++ fini
 where
  init = [NASM_Instruction $ ";; CMPSB: symbolizing RSI and RDI"]
  fini = [NASM_Instruction $ ";; CMPSB: done"]
  instruction = [show_nasm_instruction ctxt entry cfg i ""]
  pushes = map NASM_Instruction ["PUSH RSI", "PUSH RDI"]
  pops = map NASM_Instruction ["POP RSI", "POP RDI"]



-- MEMORY ACCESSES
runtime_symbolize_mem_write_with_imm_base ctxt entry cfg i@(Instruction (AddressWord64 i_a) prefix mnemonic Nothing ops annot) (Memory a si) ptr imm_base =
  map NASM_Instruction [
    ";; MemAccess with base in data section: " ++ showHex imm_base,    
    ";; " ++ show_nasm_address'' a ++ " == " ++ show ptr,
    "PUSH " ++ show reg1
   ]
   ++
     runtime_symbolize_mem_access ctxt entry cfg i section (EffectiveAddress a) reg1 reg2
   ++
   map NASM_Instruction [
    "POP " ++ show reg1,
    ";; MemAccess done"
   ]
 where
  reg1 = find_element_not_in register_set (regs_of_ops ops)
  reg2 = find_element_not_in register_set (reg1 : regs_of_ops ops)

  Just section = find_section_for_address ctxt imm_base



-- Example:
-- 	ADD QWORD PTR [a], 0x42
-- Here, a is an address based in the given section (a0,si)
--
-- 	PUSH reg2
-- 	reg1 := a - a0 + L_a0
--  ADD QWORD PTR [reg1], 0x42
--  POP reg2
runtime_symbolize_mem_access ctxt entry cfg i@(Instruction (AddressWord64 i_a) prefix mnemonic Nothing ops annot) (segment,section,a0,si) op reg1 reg2 =
  map NASM_Instruction
    [
      "PUSH " ++ show reg2,
      "LEA " ++ show reg1 ++ ", " ++ show_nasm_operand ctxt entry cfg i op,
      "LEA " ++ show reg1 ++ ", [" ++ show reg1 ++ " - 0x" ++ showHex a0 ++ "]",
      "LEA " ++ show reg2 ++ ", [rel " ++ section_label segment section a0 ++ "]",
      "LEA " ++ show reg1 ++ ", [" ++ show reg1 ++ " + " ++ show reg2 ++ "]",
      instruction,
      "POP " ++ show reg2
    ]
 where
  instruction = show_nasm_prefix prefix ++ show_nasm_opcode mnemonic ++ " " ++ (intercalate ", " $ map show_nasm_operand' ops)
  show_nasm_operand' (Memory a si) = show_nasm_size_directive i si ++ " [" ++ show reg1 ++ "]" -- Assumes one memory operand
  show_nasm_operand' op            = show_nasm_operand ctxt entry cfg i op


-- TODO PUSH FLAGS
runtime_symbolize_unresolved_mem_access ctxt entry cfg i@(Instruction (AddressWord64 i_a) prefix mnemonic Nothing ops annot) op@(Memory a si) ptr =
  map NASM_Instruction [
    ";; Unresolved MemAccess",
    ";; " ++ show_nasm_address'' a ++ " == " ++ show ptr,
    ";; " ++ show i,
    "PUSH " ++ show reg1,
    "LEA " ++ show reg1 ++ ", " ++ show_nasm_operand ctxt entry cfg i (EffectiveAddress a)
   ]
  ++
   concatMap mk_section (si_sections $ ctxt_sections ctxt)
  ++
   [
     show_nasm_instruction ctxt entry cfg i "",
     NASM_Label $ mk_label_finish,
     NASM_Instruction $ "POP " ++ show reg1,
     NASM_Instruction $ ";; MemAccess done"
   ]
 where
  reg1 = find_element_not_in register_set (regs_of_ops ops)
  reg2 = find_element_not_in register_set (reg1 : regs_of_ops ops)
  mk_section sec@(segment,section,a0,si) =
    if is_symbolizable_section sec then
      map NASM_Instruction 
        [
          "CMP " ++ show reg1 ++ ", 0x" ++ showHex a0,
          "JB " ++ mk_label sec,
          "CMP " ++ show reg1 ++ ", 0x" ++ showHex (a0 + fromIntegral si),
          "JAE " ++ mk_label sec
        ]
      ++
        runtime_symbolize_mem_access ctxt entry cfg i (segment,section,a0,si) (EffectiveAddress a) reg1 reg2
      ++
      map NASM_Instruction [
        "JMP " ++ mk_label_finish
       ]
      ++
       [NASM_Label $ mk_label sec]
    else -- TODO other sections
      []


  mk_label (_,_,a0,si) = "L_0x" ++ showHex entry ++ "_0x" ++ showHex (addressof i) ++ {--"_" ++ make_valid_label (show_nasm_address'' a)  ++ --} "_0x" ++ showHex a0
  mk_label_finish      = "L_0x" ++ showHex entry ++ "_0x" ++ showHex (addressof i) ++ "_end"











writeRIP ctxt i = write_reg ctxt (addressof i) RIP (SE_Immediate $ addressof i + (fromIntegral $ sizeof i))

expr_is_segment_plus_offset (SE_Op (Plus _) [SE_Var r, SE_Immediate _]) = True
expr_is_segment_plus_offset (SE_Op (Plus _) [SE_Immediate _, SE_Var r]) = True
expr_is_segment_plus_offset _                                           = False


expr_is_based_in_data_section ctxt (SE_Op (Plus _) [SE_Immediate imm,e1]) = if address_in_data_section ctxt imm then Just imm else expr_is_based_in_data_section ctxt e1
expr_is_based_in_data_section ctxt (SE_Op (Plus _) [e0,SE_Immediate imm]) = if address_in_data_section ctxt imm then Just imm else expr_is_based_in_data_section ctxt e0
expr_is_based_in_data_section ctxt (SE_Op (Plus _) [e0,e1])               = expr_is_based_in_data_section ctxt e0 `orTry` expr_is_based_in_data_section ctxt e1
expr_is_based_in_data_section ctxt (SE_Immediate imm)                     = if address_in_data_section ctxt imm then Just imm else Nothing
expr_is_based_in_data_section ctxt _                                      = Nothing


is_symbolizable_section sec = is_ro_data_section sec || is_data_section sec || is_bss_data_section sec

address_in_data_section ctxt imm =
  case find_section_for_address ctxt (fromIntegral imm) of
    Nothing -> False
    Just sec -> is_symbolizable_section sec


expr_is_return_value_external_function ctxt (Bottom (FromCall f)) = f `elem` (IM.elems $ ctxt_syms ctxt)
expr_is_return_value_external_function ctxt _                     = False



strict_all p x = not (S.null x) && all p x

strict_all_bases_satisfy p (Domain_Bases bases) = strict_all p bases
strict_all_bases_satisfy p _                    = False

one_base_satisfying p (Domain_Bases bases) = S.size bases == 1 && all p bases
one_base_satisfying p _                    = False

domain_is_pointer_to_external_symbol = one_base_satisfying is_pointer_to_external_symbol
 where
  is_pointer_to_external_symbol (PointerToSymbol _ sym) = sym /= ""
  is_pointer_to_external_symbol _                       = False


domain_is_heap fctxt e domain = strict_all_bases_satisfy is_heap domain || strict_all is_heap_src (srcs_of_expr fctxt e)
 where
  is_heap (Malloc _ _) = True
  is_heap _            = False

  is_heap_src (Src_Malloc _ _) = True
  is_heap_src _                = False









regs_of_ops []                       = []
regs_of_ops (Storage r:ops)          = real r : regs_of_ops ops
regs_of_ops (Immediate _:ops)        = regs_of_ops ops
regs_of_ops (EffectiveAddress a:ops) = regs_of_address a ++ regs_of_ops ops
regs_of_ops (Memory a si:ops)        = regs_of_address a ++ regs_of_ops ops

regs_of_address (AddressPlus  a0 a1) = regs_of_address a0 ++ regs_of_address a1
regs_of_address (AddressMinus a0 a1) = regs_of_address a0 ++ regs_of_address a1
regs_of_address (AddressTimes a0 a1) = regs_of_address a0 ++ regs_of_address a1
regs_of_address (AddressImm _)       = []
regs_of_address (AddressStorage r)   = [r]


register_set = [RAX,RBX,RCX,RDX,R8,R9,R10,R11,R12,R13,R14,R15]

find_element_not_in (a:as) x = if a `elem` x then find_element_not_in as x else a



pointer_params "abort"                         = []
pointer_params "bindtextdomain"                = [RDI,RSI]
pointer_params "btowc"                         = []
pointer_params "calloc"                        = []
pointer_params "close"                         = []
pointer_params "dcgettext"                     = [RDI,RSI]
pointer_params "error"                         = [RDX, RCX]
pointer_params "exit"                          = []
pointer_params "fclose"                        = [RDI]
pointer_params "feof"                          = [RDI]
pointer_params "fgetc"                         = [RDI]
pointer_params "fgets"                         = [RDI,RDX]
pointer_params "ferror"                        = [RDI]
pointer_params "fflush"                        = [RDI]
pointer_params "fileno"                        = [RDI]
pointer_params "fopen"                         = [RDI,RSI]
pointer_params "fputc"                         = [RSI]
pointer_params "fputs"                         = [RDI,RSI]
pointer_params "fputc_unlocked"                = [RSI]
pointer_params "fputs_unlocked"                = [RDI,RSI]
pointer_params "free"                          = [RDI]
pointer_params "fseeko"                        = [RDI]
pointer_params "ftello"                        = [RDI]
pointer_params "fwrite"                        = [RDI,RCX]
pointer_params "getc"                          = [RDI]
pointer_params "getdelim"                      = [RDI,RSI, RCX]
pointer_params "getenv"                        = [RDI]
pointer_params "getline"                       = [RDI,RSI, RDX]
pointer_params "getopt_long"                   = [RSI, RDX, RCX, R8]
pointer_params "getpagesize"                   = []
pointer_params "iswspace"                      = []
pointer_params "iswprint"                      = []
pointer_params "lseek"                         = []
pointer_params "malloc"                        = []
pointer_params "mbrtowc"                       = [RDI,RSI, RCX]
pointer_params "memchr"                        = [RDI]
pointer_params "memcmp"                        = [RDI,RSI]
pointer_params "memcpy"                        = [RDI,RSI]
pointer_params "memset"                        = [RDI]
pointer_params "mbsinit"                       = [RDI]
pointer_params "nl_langinfo"                   = []
pointer_params "read"                          = [RSI]
pointer_params "realloc"                       = [RDI]
pointer_params "setlocale"                     = [RSI]
pointer_params "setvbuf"                       = [RDI,RSI]
pointer_params "strchr"                        = [RDI]
pointer_params "strcmp"                        = [RDI,RSI]
pointer_params "strlen"                        = [RDI]
pointer_params "strncmp"                       = [RDI,RSI]
pointer_params "strrchr"                       = [RDI]
pointer_params "strtok"                        = [RDI,RSI]
pointer_params "sysconf"                       = []
pointer_params "sysinfo"                       = [RDI]
pointer_params "textdomain"                    = [RDI]
pointer_params "obstack_alloc_failed_handler"  = []
pointer_params "open"                          = [RDI]
pointer_params "perror"                        = [RDI]
pointer_params "posix_fadvise"                 = []
pointer_params "printf"                        = [RDI] -- [RDI,RSI,RDX,RCX,R8]
pointer_params "puts"                          = [RDI]
pointer_params "vfprintf"                      = [RDI,RSI]
pointer_params "vprintf"                       = [RDI]
pointer_params "wcwidth"                       = []

pointer_params "_exit"                         = []
pointer_params "_ITM_registerTMCloneTable"     = [RDI]
pointer_params "_ITM_deregisterTMCloneTable"   = [RDI]

pointer_params "__assert_fail"                 = []
pointer_params "__ctype_b_loc"                 = []
pointer_params "__ctype_get_mb_cur_max"        = []
pointer_params "__cxa_atexit"                  = [RDI, RSI, RDX]
pointer_params "__cxa_finalize"                = [RDI]
pointer_params "__errno_location"              = []
pointer_params "__fpending"                    = [RDI]
pointer_params "__fprintf_chk"                 = [RDI,RDX, RCX, R8, R9]
pointer_params "__freading"                    = [RDI]
pointer_params "__fxstat"                      = [RDX]
pointer_params "__gmon_start__"                = []
pointer_params "__isoc99_sscanf"               = [RDI,RSI,RDX,RCX,R8]
pointer_params "__libc_start_main"             = [RDI, RSI, RDX, RCX, R8, R9]
pointer_params "__memmove_chk"                 = [RDI,RSI]
pointer_params "__overflow"                    = []
pointer_params "__printf_chk"                  = [RSI,RDX,RCX,R8]
pointer_params "__stack_chk_fail"              = []
pointer_params "__xstat"                       = [RSI, RDX]
pointer_params f                               = error $ "Do not know parameters of function " ++ show f



show_trgt (ImmediateAddress a) = showHex a
show_trgt Unresolved           = "Unresolved"
show_trgt (External sym)       = strip_GLIBC sym ++ " wrt ..plt"



symbolize_address :: Context -> Int -> Bool -> CFG -> Word64 -> String
symbolize_address ctxt entry is_call cfg imm =
  first `orTry` second `finally` ("ERROR UNRESOLVED TARGET: " ++ showHex imm)
 where
  (first,second) = if is_call then (find_outside_cfg,find_inside_cfg) else (find_inside_cfg,find_outside_cfg)
 
  -- search for a block in the current cfg that starts at @imm@, and if found, make a label for it
  find_inside_cfg = ((block_label entry . fst) <$> find block_starts_at (IM.toList $ cfg_instrs cfg))
  -- seach for a block outside of the current cfg
  find_outside_cfg  = ((\a -> block_label a 0) <$> find ((==) imm) (map fromIntegral $ IM.keys $ ctxt_calls ctxt))

  block_starts_at (blockId, instrs) = instrs /= [] && addressof (head instrs) == fromIntegral imm




orTry :: Maybe a -> Maybe a -> Maybe a
orTry Nothing x  = x
orTry (Just x) _ = Just x




finally :: Maybe a -> a -> a
finally Nothing a  = a
finally (Just a) _ = a








 
-- | Showing unresolved address (inner part within a ptr[...])
--
-- Always of form [ seg: reg + reg*scale + number ]


show_nasm_address_plus_imm i = if testBit (fromIntegral i::Word64) 63 then " - " ++ show (0 - i) else " + " ++ show i


show_nasm_address''' (AddressStorage r) = show r
show_nasm_address''' (AddressImm i) = show i
show_nasm_address''' (AddressPlus (AddressStorage r) (AddressImm i))  = show r ++ show_nasm_address_plus_imm i
show_nasm_address''' (AddressMinus (AddressStorage r) (AddressImm i)) = show r ++ show_nasm_address_plus_imm (0 -i)
show_nasm_address''' (AddressPlus (AddressStorage r) (AddressStorage r1)) = show r ++ " + " ++ show r1
show_nasm_address''' (AddressPlus (AddressPlus (AddressStorage r) (AddressStorage r1)) (AddressImm i)) = show r ++ " + " ++ show r1 ++ show_nasm_address_plus_imm i
show_nasm_address''' (AddressPlus (AddressStorage r) (AddressPlus (AddressStorage r1) (AddressImm i))) = show r ++ " + " ++ show r1 ++ show_nasm_address_plus_imm i
show_nasm_address''' (AddressPlus (AddressStorage r) (AddressMinus (AddressStorage r1) (AddressImm i))) = show r ++ " + " ++ show r1 ++ show (0-1)
show_nasm_address''' (AddressPlus (AddressTimes (AddressStorage r) (AddressImm i)) (AddressImm i1)) = show r ++ " * " ++ show i ++ show_nasm_address_plus_imm i1
show_nasm_address''' (AddressPlus (AddressStorage r) (AddressTimes (AddressStorage r1) (AddressImm i))) = show r ++ " + " ++ show r1 ++ " * " ++ show i
show_nasm_address''' (AddressPlus (AddressStorage r) (AddressPlus (AddressTimes (AddressStorage r1) (AddressImm i)) (AddressImm i1))) = show r ++ " + " ++ show r1 ++ " * " ++ show i ++ show_nasm_address_plus_imm i1
show_nasm_address''' (AddressPlus (AddressStorage r) (AddressMinus (AddressTimes (AddressStorage r1) (AddressImm i)) (AddressImm i1))) = show r ++ " + " ++ show r1 ++ " * " ++ show i ++ show_nasm_address_plus_imm (0-i1)
show_nasm_address''' (AddressTimes (AddressStorage r) (AddressImm i))  = show r ++ " * " ++ show i
show_nasm_address''' a           = error $ "TODO: " ++ show a



show_nasm_address'' a@(AddressPlus (AddressStorage r) a') = if is_segment_register r then show r ++ ":" ++ show_nasm_address''' a' else show_nasm_address''' a 
show_nasm_address'' a                                     = show_nasm_address''' a

is_segment_register r = r `elem` [CS,DS,ES,FS,GS,SS]

show_nasm_address ctxt entry cfg i address =
  case rip_relative_to_immediate i address of
    Just imm -> case symbolize_immediate ctxt True $ fromIntegral imm of
                  Just symbolized -> symbolized
                  _               -> "0x" ++ showHex imm
    _ -> show_nasm_address'' address
 where

rip_relative_to_immediate i (AddressImm imm)                                    = Just $ imm
rip_relative_to_immediate i (AddressPlus  (AddressStorage RIP) (AddressImm imm)) = Just $ fromIntegral (addressof i) + fromIntegral (sizeof i) + fromIntegral imm
rip_relative_to_immediate i (AddressMinus (AddressStorage RIP) (AddressImm imm)) = Just $ fromIntegral (addressof i) + fromIntegral (sizeof i) - fromIntegral imm
rip_relative_to_immediate i (AddressPlus  (AddressImm imm) (AddressStorage RIP)) = Just $ fromIntegral (addressof i) + fromIntegral (sizeof i) + fromIntegral imm
rip_relative_to_immediate i _                                                   = Nothing



symbolize_immediate :: Context -> Bool -> Int ->  Maybe String
symbolize_immediate ctxt do_globals a =
  (pointer_to_instruction $ fromIntegral a)
  `orTry`
  (symbol_from_symbol_table a)
  `orTry`
  (relocated_symbol a)
  `orTryCond`
  (relative_to_section $ fromIntegral a)
 where
  orTryCond = if do_globals then orTry else const

  pointer_to_instruction a = 
    if address_has_instruction ctxt a then
      uncurry block_label <$> find_block_for_instruction ctxt a 
    else
      Nothing

  symbol_from_symbol_table a = strip_GLIBC <$> (IM.lookup (fromIntegral a) $ IM.filter ((/=) "") $ ctxt_syms ctxt)

  relative_to_section a = 
    (\l -> "rel " ++ l) <$> show_section_relative a <$> find_section_for_address ctxt a

  show_section_relative a sec@(segment,section,a0,_) =
    if is_literal_string_section sec then
      section_label segment section a0 ++ "_0x" ++ showHex (a - a0)
    else
      section_label segment section a0 ++ " + 0x" ++ showHex (a - a0)

  relocated_symbol a1 =
    mk_reloc_symbol <$> (find (was_relocated $ fromIntegral a1) $ ctxt_relocs ctxt)


    


find_block_for_instruction ctxt a =
  if address_has_symbol ctxt a then
    Nothing
  else case catMaybes $ map (find_block_for_instruction_address_in_cfg a) $ IM.assocs $ ctxt_cfgs ctxt of
    [b] -> Just b
    x   -> trace ("Address " ++ showHex a ++ " has instruction but no block") $ Nothing
  
find_block_for_instruction_address_in_cfg a (entry,cfg) = 
  return_entry_and_blockID <$> (find (\(blockID,instrs) -> addressof (head instrs) == a) $ IM.toList $ cfg_instrs cfg)
 where
  return_entry_and_blockID (blockID,instrs) = (entry,blockID)




-- | Showing size directive
show_nasm_size_directive _ 1  = "byte"
show_nasm_size_directive _ 2  = "word"
show_nasm_size_directive _ 4  = "dword"
show_nasm_size_directive _ 8  = "qword"
show_nasm_size_directive _ 10 = "tword"
show_nasm_size_directive i 16 = if opcode i == COMISD then "qword" else "oword" -- BUG in Capstone
show_nasm_size_directive i x  = error $ "Unknown size directive " ++ show x ++ " in instruction: " ++ show i


-- | Showing an operand
show_nasm_operand ctxt entry cfg i (Memory a si)        = show_nasm_size_directive i si ++ " [" ++ show_nasm_address ctxt entry cfg i a ++ "]"
show_nasm_operand ctxt entry cfg i (EffectiveAddress a) = "[" ++ show_nasm_address ctxt entry cfg i a ++ "]"
show_nasm_operand ctxt entry cfg i (Storage r)          = show r
show_nasm_operand ctxt entry cfg i (Immediate imm)      = "0x" ++ showHex imm


section_label segment section addr = "L" ++ segment ++ "_" ++ section ++ (if is_literal_string_section (segment,section,0,0) then "_" ++ show addr else "_0x" ++ showHex addr)



jump_table_label i_a = "L_JUMP_TABLE_" ++ showHex i_a


show_nasm_opcode MOVABS = "MOV"
show_nasm_opcode opcode = show opcode

show_nasm_prefix Nothing    = ""
show_nasm_prefix (Just pre) = show pre ++ " "




show_nasm_instruction ctxt entry cfg  i@(Instruction addr prefix CMPSB Nothing [op1,op2] annot) msg =
  NASM_Instruction $ show_nasm_prefix prefix ++ "CMPSB" ++ (if msg == "" then "" else "    ; " ++ msg)
show_nasm_instruction ctxt entry cfg  i@(Instruction addr Nothing LEA Nothing (ops@[op1,EffectiveAddress address]) annot) msg =
  case rip_relative_to_immediate i address of
    Just imm -> NASM_Instruction $ "MOV " ++ show_nasm_operand ctxt entry cfg i op1 ++ ", 0x" ++ showHex imm ++ "; used to be LEA"
    _        -> NASM_Instruction $ "LEA " ++ intercalate ", " (map (show_nasm_operand ctxt entry cfg i) ops) ++ (if msg == "" then "" else "    ; " ++ msg)

show_nasm_instruction ctxt entry cfg  i@(Instruction addr pre opcode Nothing ops annot) msg = NASM_Instruction $ 
     show_nasm_prefix pre
  ++ show_nasm_opcode opcode
  ++ " "
  ++ intercalate ", " (map (show_nasm_operand ctxt entry cfg i) ops)
  ++ (if msg == "" then "" else "    ; " ++ msg)



show_nasm_instruction_direct ctxt entry cfg i@(Instruction addr pre opcode Nothing ops annot) mem_op_translation msg = NASM_Instruction $ 
     show_nasm_prefix pre
  ++ show_nasm_opcode opcode
  ++ " "
  ++ intercalate ", " (map show_nasm_operand' ops)
  ++ (if msg == "" then "" else "    ; " ++ msg)
 where
  show_nasm_operand' (Memory a si) = show_nasm_size_directive i si ++ " " ++ mem_op_translation
  show_nasm_operand' op            = show_nasm_operand ctxt entry cfg i op

