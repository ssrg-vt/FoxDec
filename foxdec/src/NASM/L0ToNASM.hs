{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : L0ToNASM
Description : Lift the L0 representation of the binary to symbolized and recompilable NASM.
-}



module NASM.L0ToNASM (lift_L0_to_NASM, render_NASM, __gmon_start_implementation, NASM) where


import Base
import Config

import NASM.NASM
import OutputGeneration.Retrieval

import Analysis.Context
import Analysis.FunctionNames
import Analysis.ControlFlow

import X86.Conventions
import X86.Instruction (addressof,Instruction)
import X86.Opcode
import X86.Address
import X86.Register

import Generic.Binary
import Generic.SymbolicConstituents (operand_size)
import Instantiation.BinaryElf
import Instantiation.SymbolicPropagation (get_invariant)

import Data.JumpTarget
import Data.Symbol

import qualified Generic.Instruction as Instr
import Generic.Instruction (GenericInstruction(..))
import Generic.Operand
import Generic.Address
import Generic.HasSize 


import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List.Extra (firstJust,replace)
import Data.List 
import Data.Word
import Data.Foldable
import Data.Char 
import Data.Bits (testBit, (.&.))
import Data.List.Split (splitOn)
import Data.ByteString.Internal (w2c)
import Data.Function (on)

import Control.Monad.State.Strict
import Data.Functor.Identity
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Unsafe


import Debug.Trace

import GHC.Base hiding (Symbol)









-- | Lift an L0 representation to position-independent NASM
lift_L0_to_NASM ctxt = NASM mk_externals mk_globals mk_sections $ mk_jump_tables ++ [mk_temp_storage] 
 where
  mk_externals       = externals ctxt
  mk_globals         = S.difference (ctxt_get_globals ctxt) mk_externals
  mk_sections        = mk_text_sections ++ [mk_ro_data_section, mk_data_section, mk_bss_section, mk_resolved_relocs]
  mk_text_sections   = map (entry_to_NASM ctxt) $ S.toList $ ctxt_get_function_entries ctxt

  mk_ro_data_section = NASM_Section_Data $ ro_data_section ctxt
  mk_data_section    = NASM_Section_Data $ data_section ctxt
  mk_bss_section     = NASM_Section_Data $ bss_data_section ctxt
  mk_resolved_relocs = NASM_Section_Data $ resolved_relocs_section ctxt

  mk_temp_storage    = "section .bss\nLtemp_storage_foxdec:\nresb 8"
  mk_jump_tables     = map (mk_jump_table ctxt) $ get_indirections_per_function ctxt
 -- intercalate "\n" $ ["; Internally resolved relocations", "section .data"] ++ 


resolved_relocs_section ctxt =
  case filter is_relocation $ IM.assocs $ ctxt_symbol_table ctxt of
    [] -> []
    relocs -> map mk_reloc relocs
 where
  is_relocation (_,Relocated_ResolvedObject _ _) = True
  is_relocation _ = False

  mk_reloc (a0,Relocated_ResolvedObject str a1) = 
    NASM_DataSection ("",".data",fromIntegral a0) 0 (IM.singleton 0 $ S.singleton $ Label (fromIntegral a0) $ strip_GLIBC str) [DataEntry_Pointer (fromIntegral a0) (try_symbolize_imm ctxt a1) ]

-- | Rendering NASM to a String
render_NASM :: Context -> NASM -> String
render_NASM ctxt (NASM exts globals sections footer) = intercalate "\n\n\n" $ [
    render_externals,
    render_globals,
    "default rel",
    mk_macros ctxt ]
    ++ render_sections
    ++ footer
    ++ [ render_annots ]
 where
  render_annots = intercalate "\n" $ [
    "; TEMP_OBJECTs are memory locations inserted by FoxDec not present in the original binary",
    "; EXT_OBJECTs are external objects such as stderr and stdout",
    "; TERMINAL_CALLs are addresses of instructions in the original binary that always terminate and do not return",
    "; The remainder is a mapping from original addresses to internal labels",
    "%ifdef COMMENT",
    intercalate "\n" $ map show_temp_object $ Label 0 "Ltemp_storage_foxdec" : (concatMap get_temp_object $ get_indirections_per_function ctxt),
    intercalate "\n" $ map render_terminal $ get_terminals_per_function ctxt,
    intercalate "\n" $ map show_external_object $ external_objects ctxt,
    show_annots $ mk_annots ctxt sections,
    "%endif" ]


  render_terminal i = "TERMINAL_CALL 0x" ++ showHex (addressof i) 

  render_externals = intercalate "\n" $ map ((++) "extern ") $ S.toList $ exts -- S.difference exts (ctxt_get_globals ctxt)
  render_sections  = map render_section sections
  render_globals   = intercalate "\n" $ map ((++) "global ") $ S.toList $ globals -- S.difference (ctxt_get_globals ctxt) exts

  render_section (NASM_Section_Text ts) = show ts
  render_section (NASM_Section_Data ds) = intercalate "\n\n" (map show ds) ++ "\n\n"




  show_external_object str = "EXT_OBJECT " ++ str
  
  get_temp_object (entry,cfg,(a,Indirection_JumpTable (JumpTable index bnd trgt tbl))) =
    [
     label_jump_table_temp_storage entry a 0
    ,label_jump_table_temp_storage entry a 1
    ]
  get_temp_object (entry,cfg,(a,_)) = []

  show_temp_object obj = "TEMP_OBJECT " ++ show obj







-- | get the external functions and objects 
externals ctxt = S.insert "exit" $ S.fromList $ map (strip_GLIBC . symbol_to_name) $ filter is_relocation $ IM.elems $ ctxt_symbol_table ctxt
 where
  is_relocation (PointerToLabel str ex)  = ex && str /= ""
  is_relocation (PointerToObject str ex) = ex && str /= ""
  is_relocation (AddressOfObject str ex) = ex && str /= ""
  is_relocation (AddressOfLabel str ex) = ex && str /= ""
  is_relocation _ = False

-- | get the external objects
external_objects ctxt = map (strip_GLIBC . symbol_to_name) $ filter is_relocation $ IM.elems $ ctxt_symbol_table ctxt
 where
  is_relocation (PointerToObject str ex) = ex && str /= ""
  is_relocation (AddressOfObject str ex) = ex && str /= ""
  is_relocation _ = False





is_address_of_symbol (AddressOfObject str ex) = str /= ""
is_address_of_symbol (AddressOfLabel str ex)  = str /= ""
is_address_of_symbol _                        = False

-- | Creating labels
-- Given the entry address of the current function, the blockID of the current basic block,
-- map an address to a label.
-- First, try to see if it matches the _start symbol.
-- Then, try to map the address to a known internal synbol (unstripped binaries may have such symbols available)
-- Then, try to see if at the address a relocation is stored, and use that lavel if so.
-- Otherwise, make a new custom label.
block_label ctxt entry a blockID = (try_symbol `orTry` try_relocation_label) `orElse` custom_label
 where
  -- Try if the address matches a known symbol
  try_symbol = do
    sym  <- (IM.lookup (fromIntegral a) $ IM.filter is_address_of_symbol $ ctxt_symbol_table ctxt)
    let name = symbol_to_name sym
    if {--nasm_with_safe_labels (ctxt_config ctxt) &&--} blockID /= 0 then
      return $ Label a $ "L" ++ showHex entry ++ "_" ++ name
    else
      return $ Label a name
  -- Try to see if the address stores a relocation
  try_relocation_label = reloc_label <$> find (reloc_for a) (ctxt_relocs ctxt)

  -- Make a new label based on the entry and blockID
  custom_label = Label a $ "L" ++ showHex entry ++ "_" ++ show blockID
  -- Make a label for relocation
  reloc_label (Relocation a0 a1) = Label a $ "L_reloc_0x" ++ showHex a0 ++ "_0x" ++ showHex a1


-- Make a label for the start of a section
section_label segment section addr = Label addr $ "L" ++ segment ++ "_" ++ section ++ "_0x" ++ showHex addr 
-- Make a label for the end of a section
end_of_section_label (segment,section,a0,sz,_) = Label (a0+sz) $ "L" ++ segment ++ "_" ++ section ++ "_END"  
-- Make a macro name
macro_name segment section a0 offset = Macro segment section a0 offset
-- Make a "safe" label (no duplicates)
mk_safe_label str a = Label a $ strip_GLIBC str ++ "_0x" ++ showHex a

-- Make a label indicating that this block is expected to terminate
terminating_label ctxt entry i blockID =
  let Label a l = block_label ctxt entry (addressof i) blockID in
    Label a $ "FOXDEC_TERMINATING_" ++ l
-- Make a label indicating that this block is a DONTCARE
dontcare_label ctxt entry a_end blockID =
  let Label a l = block_label ctxt entry a_end blockID in
    Label a $ "FOXDEC_DONTCARE_" ++ l
-- Make a label indicating that this instruction is halting
halting_label ctxt entry a blockID =
  let Label a' l = block_label ctxt entry a blockID in
    Label a' $ l ++ "_HLT"





-- | Information on sections
-- TODO: get from Binary interface
is_ro_data_section ("",".rodata",_,_,_) = True
is_ro_data_section ("",".init_array",_,_,_) = True
is_ro_data_section ("",".fini_array",_,_,_) = True
is_ro_data_section ("",".data.rel.ro",_,_,_) = True
is_ro_data_section ("__DATA","__const",_,_,_) = True
is_ro_data_section _ = False

is_data_section ("__DATA","__data",_,_,_) = True
is_data_section ("",".data",_,_,_) = True
is_data_section _ = False

is_bss_data_section ("__DATA","__bss",_,_,_) = True
is_bss_data_section ("__DATA","__common",_,_,_) = True
is_bss_data_section ("",".bss",_,_,_) = True
is_bss_data_section _ = False






-- | convert a given function entry to a NASM text section
entry_to_NASM ctxt entry = NASM_Section_Text $ NASM_TextSection mk_header mk_blocks
 where
  Label _ mk_header = block_label ctxt (fromIntegral entry) (fromIntegral entry) 0 -- function_name_of_entry ctxt entry
  Just cfg          = IM.lookup entry (ctxt_cfgs ctxt)
  mk_blocks         = 
    let blocks      = IM.keys $ cfg_blocks cfg in
      cfg_blocks_to_NASM ctxt entry cfg $ (sortBy (start_address_le cfg) blocks)

  start_address_le cfg blockID1 blockID2 = 
    case (start_address_of_block cfg blockID1, start_address_of_block cfg blockID2) of
      (Just a1, Just a2) -> compare a1 a2
      (Just a1, _)       -> LT
      (_, Just a2)       -> GT
      _ -> EQ


show_annots :: Annot -> String
show_annots = intercalate "\n" . map show_entry
 where
  show_entry (a,l,offset) = "0x" ++ showHex a ++ ": " ++ show l ++ (if offset == 0 then "" else " + 0x" ++ showHex offset)

mk_annots :: Context -> [NASM_Section] -> Annot
mk_annots ctxt sections = map replace_macro_names $ nub $ block_mapping ++ annot_from_labels
 where
  block_mapping = concatMap (mk_block_mapping_for_entry) $ S.toList $ ctxt_get_function_entries ctxt
  annot_from_labels = concatMap get_annots_section sections

  mk_block_mapping_for_entry entry = 
    let cfg = ctxt_cfgs ctxt IM.! entry in
      map (mk_block_mapping_for_block entry cfg) (IM.keys $ cfg_blocks cfg)

  mk_block_mapping_for_block entry cfg blockID = 
    let a = addressof $ head $ get_block_instrs cfg blockID in
      (a, block_label ctxt entry a blockID, 0)
  get_block_instrs cfg blockID = cfg_instrs cfg IM.! blockID 


  get_annots_section (NASM_Section_Text sec) = get_annots_textsection sec
  get_annots_section (NASM_Section_Data sec) = [] -- TODO?

  get_annots_textsection (NASM_TextSection _ blocks) = concatMap get_annots_line $ concat $ map snd blocks

  get_annots_line (NASM_Line (NASM_Instruction _ _ _ _ annot)) = annot
  get_annots_line _ = []



  replace_macro_names (a,label,0) =
    case find (\(sym,_,_,_,_) -> Label a sym == label) $ IM.elems $ internal_labels_outside_of_sections ctxt of
      Just (sym,segment,section,a0,offset) -> error "TODO msut be a?"-- (a,section_label segment section a0, offset)
      Nothing -> (a,label,0)
  replace_macro_names x = x
 


-- | convert a list of basic blocks to NASM instructions
-- Note that the order dictates if additional jumps need to be inserted
cfg_blocks_to_NASM ctxt entry cfg []                             = []
cfg_blocks_to_NASM ctxt entry cfg [blockID0]                     = [(blockID0,cfg_block_to_NASM ctxt entry cfg blockID0 $ Nothing)]
cfg_blocks_to_NASM ctxt entry cfg (blockID0:blocks@(blockID1:_)) = (blockID0,cfg_block_to_NASM ctxt entry cfg blockID0 $ Just blockID1) : cfg_blocks_to_NASM ctxt entry cfg blocks



-- | convert a single basic block to NASM instructions
-- A block is translated to a header and its instructions
cfg_block_to_NASM ctxt entry cfg blockID blockID1 = block_header : mk_block
 where
  -- header
  block_header          = NASM_Comment 0 $ "Entry " ++ showHex entry ++ "; block " ++ show blockID ++ "; address " ++ showHex (addressof $ head block_instrs)
  -- instructions: the label and the body
  mk_block              = block_label' ++ insert_nop_if_empty block_body

  -- the label
  block_label'          = [NASM_Label $ block_label ctxt entry (addressof $ head block_instrs) blockID] 
  -- the body:
  --   in case of a block ending in a jump table, instructions are inserted at beginning and end of the block
  --   in case of a block ending in an indirection resolved to a single target, translate the last instruction accordingly
  --   in case of a block ending in an unresolved indirection resolved to a single target, translate as normal but annotate
  --   in case of no indirection, translate normally but insert an extra jump to the next block if neccessary
  block_body =
    case try_indirect_block block_instrs of
      Just (Indirection_JumpTable tbl@(JumpTable index _ _ _)) -> 
        let si   = operand_size index
            reg  = reg_of_size (find_unused_register register_set block_instrs) si
            reg' = reg_of_size (find_unused_register [ r | r <- register_set, real r /= real reg] block_instrs) 8 in
          jmp_table_init tbl reg reg' (last block_instrs) ++ mk_block_instrs (init block_instrs) ++ jmp_table_end tbl reg reg' (last block_instrs)
      Just (Indirection_Resolved trgts) -> mk_block_instrs (init block_instrs) ++ resolved_jump (S.toList trgts) (last block_instrs) ++ block_end block_instrs
      Just (Indirection_Unresolved)     -> mk_block_instrs block_instrs ++ unresolved_end ++ block_end block_instrs
      Nothing                           -> mk_block_instrs block_instrs ++ block_end block_instrs


  insert_nop_if_empty [] = [NASM_Line $ mk_nasm_instr NOP [] `withComment` "NOP inserted"]
  insert_nop_if_empty b  = b

  -- if the block ends in an indirection, retrieve that indirection
  try_indirect_block instrs
    | instrs == []  = Nothing
    | is_unresolved = Just Indirection_Unresolved
    | otherwise     = IM.lookup (fromIntegral $ addressof $ last instrs) $ ctxt_inds ctxt
  is_unresolved = node_info_of ctxt cfg blockID == UnresolvedIndirection
  


  -- make a block of regular non-control-flow instructions
  mk_block_instrs instrs = concatMap (instr_to_NASM ctxt entry cfg blockID) $ filter_unnecessary_jumps instrs

  -- for a normal (non-indirection) block, see if we need to insert an additional jump to the next block
  -- Necessary when that next block is not the next to be translated.
  block_end instrs
    | instrs == [] = []
    | is_proper_block_end_instruction (Instr.opcode $ last instrs) = []
    --  | blockID1 /= Nothing && (IM.lookup blockID (cfg_edges cfg) == Just (IS.singleton $ fromJust blockID1) = []
    | otherwise = mk_extra_jmp


  -- A jump table is implemented (TODO more comments)
  jmp_table_init t@(JumpTable index bound trgt tbl) reg reg' i =
    let (trgt_str,annot) = operand_to_NASM ctxt entry cfg empty_instr False index in
      [ NASM_Comment 2 $ "Resolved indirection:"
      , NASM_Comment 2 $ show t
      , NASM_Line $ mk_nasm_instr MOV  [label_to_mem_operand (8,False) $ label_jump_table_temp_storage entry (addressof i) 0, reg_to_operand (real reg)]
      , NASM_Line $ mk_nasm_instr MOV  [label_to_mem_operand (8,False) $ label_jump_table_temp_storage entry (addressof i) 1, reg_to_operand (real reg')]
      , NASM_Line $ mk_nasm_instr MOV  [reg_to_operand reg, trgt_str] `withAnnot` annot
      ]
      ++
      (if sizeof reg < 4 then [NASM_Line $ mk_nasm_instr MOVZX [reg_to_operand $ reg_of_size (real reg) 4, reg_to_operand reg]] else [])
      ++
      [ NASM_Comment 2 $ "Start of block"  ]


  jmp_table_end t@(JumpTable index bound trgt tbl) reg reg' last_instr =
    let (trgt_str,annot) = operand_to_NASM ctxt entry cfg empty_instr False trgt 
        a_end            = addressof last_instr in
      [ NASM_Comment 2 $ "End of block"
      , NASM_Line $ mk_nasm_instr LEA [reg_to_operand reg', label_to_eff_operand $ label_jump_table_redirect_data entry a_end ]
      , NASM_Line $ mk_nasm_instr LEA [reg_to_operand reg', NASM_Operand_EffectiveAddress $ NASM_Addr_Compute $ empty_address {nasm_base = Just reg', nasm_index = Just (reg_of_size (real reg) 8), nasm_scale = 8} ]
      , NASM_Line $ mk_nasm_instr MOV [trgt_str, NASM_Operand_Memory (8,True) $ NASM_Addr_Compute $ empty_address {nasm_base = Just reg' }] `withAnnot` annot
      , NASM_Line $ mk_nasm_instr MOV [reg_to_operand (real reg), label_to_mem_operand (8,False) $ label_jump_table_temp_storage entry a_end 0 ]
      , NASM_Line $ mk_nasm_instr MOV [reg_to_operand (real reg'), label_to_mem_operand (8,False) $ label_jump_table_temp_storage entry a_end 1 ]
      , NASM_Comment 2 $ "Executing resolved indirect jump"
      , NASM_Label $ dontcare_label ctxt entry a_end blockID
      ]
      ++
      mk_jmp_call_instr ctxt entry cfg blockID last_instr



  -- A resolved jump to a single known target
  resolved_jump [External f] (Instruction addr pre op Nothing ops annot) =
    [ NASM_Comment 2 $ "Resolved indirection: " ++ show (head ops) ++ " --> " ++ f 
    , NASM_Line $ NASM_Instruction pre (opcode_to_NASM op) [NASM_Operand_Address $ NASM_Addr_Symbol $ PointerToLabel f True] "" []]
  resolved_jump [ImmediateAddress imm] i@(Instruction addr pre op Nothing ops annot) = 
    [ NASM_Comment 2 $ "Resolved indirection: " ++ show (head ops) ++ " --> " ++ showHex imm ]
    ++
    mk_jmp_call_instr ctxt entry cfg blockID i
  resolved_jump trgts i@(Instruction addr pre op Nothing ops annot) =
    [ NASM_Comment 2 $ "Resolved indirection: " ++ show (head ops) ++ " --> " ++ show trgts ]
    ++
    instr_to_NASM ctxt entry cfg blockID i


  -- An unresolved indirection annotation
  unresolved_end =
   [ NASM_Comment 2 $ "Unresolved indirection" ]

  filter_unnecessary_jumps instrs
    | instrs == [] = []
    | otherwise    = (filter (not . isJump . Instr.opcode) $ init instrs) ++ [last instrs]
  is_proper_block_end_instruction i = isRet i || isJump i 

  Just block_instrs = IM.lookup blockID $ cfg_instrs cfg

  empty_instr :: Instruction
  empty_instr = Instruction (AddressWord64 0) Nothing NOP Nothing [] Nothing

  mk_extra_jmp =
    case unsafePerformIO $ stepA ctxt get_invariant entry (fromIntegral $ addressof $ last block_instrs) of -- TODO assumes fall through is last/second
      Right []        -> mk_extra_hlt $ last block_instrs
      Right [(a,_)]   -> mk_extra_jump_maybe a
      Right [_,(a,_)] -> mk_extra_jump_maybe a
      x               -> [NASM_Comment 0 $ "JMP ERROR CANNOT DETERMINE NEXT BLOCK" ++ show x]

  mk_extra_hlt last_instr
    | isCall (opcode last_instr) =
      let a = addressof last_instr + (fromIntegral $ sizeof last_instr) in
        if is_start_of_block_anywhere ctxt a then
          []
        else  
          let label = halting_label ctxt entry a blockID in
            [ 
              NASM_Label label,
              NASM_Line $ mk_nasm_instr HLT [] `withComment` "should never be reached" `withAnnot` [(a,label,0)]
            ]
    | otherwise = []

  mk_extra_jump_maybe a
    | blockID1 /= Nothing && Just a == start_address_of_block cfg (fromJust blockID1) = []
    | otherwise = [NASM_Line $ mk_nasm_instr JMP [NASM_Operand_Address $ fst $ fromJust $ symbolize_immediate ctxt (Just (entry,cfg)) False $ fromIntegral a] `withComment` "jump is inserted"]



start_address_of_block cfg blockID = fromIntegral . addressof . head <$> (IM.lookup blockID $ cfg_instrs cfg)

label_jump_table_temp_storage  entry a n = Label 0 $ "L_jmp_tbl_temp_storage_" ++ showHex entry ++ "_" ++ showHex a ++ (if n == 0 then "_0" else "_1")
label_jump_table_redirect_data entry a = Label 0 $ "L_jmp_tbl_" ++ showHex entry ++ "_" ++ showHex a


is_start_of_block_anywhere ctxt a =
  let cfgs   = IM.elems $ ctxt_cfgs ctxt
      blocks = concatMap (IM.elems . cfg_instrs) cfgs in
    any (\block -> addressof (head block) == a) blocks
  


-- | convert an instruction to a NASM instruction
instr_to_NASM ctxt entry cfg blockID i@(Instruction addr pre op Nothing ops annot)
 | op == NOP                                = []
 | op == ENDBR64                            = []
 | no_ops pre op                            = [NASM_Line $ mk_normal_instr ctxt entry cfg $ Instruction addr pre op Nothing [] annot]
 | is_cf op                                 = mk_jmp_call_instr ctxt entry cfg blockID i
 | some_operand_reads_GOT_entry ctxt i ops  = map NASM_Line $ mk_GOT_entry_instr ctxt entry cfg i
 | otherwise                                = [NASM_Line $ mk_normal_instr ctxt entry cfg i]
 where
  is_cf op      = isCall op || isJump op || isCondJump op
  no_ops pre op = op `elem` [STOSB,STOSD,STOSQ, SCASB,SCASD, CMPSB] || is_string_mov op


  is_string_mov op
    | op `elem` [MOVSB, MOVSW, MOVSD, MOVSQ] =
      not (length ops == 2 && (operand_size (ops!!0) == 16 || operand_size (ops!!1) == 16))
      -- TODO
      --   REP MOVSQ QWORD PTR [RDI,8], QWORD PTR [RSI,8]
      --   MOVSD QWORD PTR [RSP+0x8],xmm0
    | otherwise = False


-- Make a normal instruction
mk_normal_instr ctxt entry cfg i@(Instruction addr pre op Nothing ops annot) = mk_instr
 where
  mk_instr = NASM_Instruction pre (opcode_to_NASM op) (map fst mk_ops) "" mk_annot
  mk_ops = map (operand_to_NASM ctxt entry cfg i False) ops
  mk_annot = concat $ filter ((/=) []) $ map snd mk_ops

-- Make a JUMP/CALL instruction
mk_jmp_call_instr ctxt entry cfg blockID i@(Instruction addr pre op Nothing [op1] annot) =
  add_label_if_terminating $ use_jump_target $ jump_target_for_instruction ctxt i
 where
  use_jump_target (External sym)        = NASM_Line $ NASM_Instruction pre (opcode_to_NASM op) [NASM_Operand_Address $ NASM_Addr_Symbol $ PointerToLabel sym True] "" []
  use_jump_target (ImmediateAddress a') = 
        let (op1_str,annot) = operand_to_NASM ctxt entry cfg i True (Immediate a') in
          NASM_Line $ NASM_Instruction pre (opcode_to_NASM op) [op1_str] "" annot
  use_jump_target Unresolved            =
    let (op1_str,annot) = operand_to_NASM ctxt entry cfg i True op1 in
      NASM_Line $ NASM_Instruction pre (opcode_to_NASM op) [op1_str] "" annot


  add_label_if_terminating l
    | is_terminal_call ctxt entry i = [NASM_Line $ mk_nasm_instr NOP [], NASM_Label $ terminating_label ctxt entry i blockID, l]
    | otherwise                     = [l]


is_terminal_call ctxt entry i@(Instruction addr pre op Nothing ops annot)
  | isCall op || isJump op = 
    case resolve_call ctxt get_invariant entry i of
      Right [] -> True
      _        -> False
  | otherwise = False

-- Make an instruction with an operand reading a GOT entry
-- TODO MOV reg, [rip+imm] can be more efficient
mk_GOT_entry_instr ctxt entry cfg i@(Instruction addr pre op Nothing ops annot) = 
  let [f] = name_of_external_function
      r   = find_unused_register register_set [i] in
    [ mov_reg_to_temp r
    , lea_external_function r f
    , the_actual_instr r
    , mov_temp_to_reg r ]
 where
  mov_reg_to_temp r = mk_nasm_instr MOV [ label_to_mem_operand (8,True) $ Label 0 "Ltemp_storage_foxdec", fst $ operand_to_NASM ctxt entry cfg i True $ Storage r ] `withComment` "inserted"

  lea_external_function r f = mk_nasm_instr LEA [ fst $ operand_to_NASM ctxt entry cfg i True $ Storage r,  NASM_Operand_EffectiveAddress $ NASM_Addr_Symbol f ]

  the_actual_instr r = mk_normal_instr ctxt entry cfg (Instruction addr pre op Nothing (replace_mem_op r ops) annot)

  mov_temp_to_reg r = mk_nasm_instr MOV [ fst $ operand_to_NASM ctxt entry cfg i True $ Storage r, label_to_mem_operand (8,True) $ Label 0 "Ltemp_storage_foxdec"] `withComment` "inserted"

  replace_mem_op r (Memory a si:ops) = Storage r : ops
  replace_mem_op r (op:ops)          = op:replace_mem_op r ops

  name_of_external_function = mapMaybe (try_operand_reads_GOT_entry ctxt i) ops
{--
-- TODO: this is only relevant when doing non-RIP-relative binaries and translating instructions of the form:
--    MOV dst, imm
-- where imm is (maybe) an address
--mk_instr ctxt entry cfg i@(Instruction addr pre op@MOV Nothing ops@[dst,Immediate imm] annot)
--  | is_instruction_address ctxt imm = mk_fake_lea ctxt entry cfg i
--  | otherwise = [NASM_Instruction $ concat
--      [ pre
--      , opcode_to_NASM op
--      , " "
--      ,  intercalate ", " $ map (operand_to_NASM ctxt entry cfg i False) ops ]]

--}




some_operand_reads_GOT_entry ctxt i = any (\op -> try_operand_reads_GOT_entry ctxt i op /= Nothing)

try_operand_reads_GOT_entry ctxt i (Memory addr si) =
  case rip_relative_to_immediate i addr of
    Nothing -> Nothing
    Just a  -> find_relocated_function a
 where
  find_relocated_function a = 
    case find (is_relocated_function $ fromIntegral a) $ IM.assocs $ ctxt_symbol_table ctxt of
      Just (a',sym@(PointerToLabel _ _))  -> Just sym
      --Just (a',sym@(PointerToObject _ _)) -> Just sym
      _ -> Nothing

  is_relocated_function a (a',PointerToLabel  str _) = a == a'
  --is_relocated_function a (a',PointerToObject str _) = a == a'
  is_relocated_function a _                          = False
try_operand_reads_GOT_entry ctxt i _ = Nothing


is_instruction_address ctxt a = and
  [ address_has_instruction ctxt a
  , a `elem` all_instruction_addresses ]
 where
  all_instruction_addresses = S.unions $ map (S.fromList . map addressof . concat . IM.elems . cfg_instrs) $ IM.elems $ ctxt_cfgs ctxt



-- TODO: this is only relevant when doing non-RIP-relative binaries and translating instructions of the form:
--   MOV dst, imm
-- where imm is (maybe) an address
{--
mk_fake_lea ctxt entry cfg i@(Instruction addr pre op@MOV Nothing ops@[dst@(Storage r),Immediate imm] annot) =
    let l = fromJust $ symbolize_immediate ctxt (Just (entry,cfg)) True imm in
      [NASM_Instruction $ concat
        [ pre
        , "LEA "
        , operand_to_NASM ctxt entry cfg i True dst
        , ", [" ++ l ++ "]"
        , " ; 0x" ++ showHex imm ++ " --> " ++ l ]]
mk_fake_lea ctxt entry cfg i@(Instruction addr pre op@MOV Nothing ops@[dst@(Memory a si),Immediate imm] annot) =
    let l = fromJust $ symbolize_immediate ctxt (Just (entry,cfg)) True imm
        r = find_unused_register register_set [i] in
      [ NASM_Comment 2 "inserted"
      , NASM_Instruction $ concat
        [ "MOV qword [Ltemp_storage_foxdec], " 
        , operand_to_NASM ctxt entry cfg i True $ Storage r ]
      , NASM_Instruction $ concat
        [ pre
        , "LEA "
        , operand_to_NASM ctxt entry cfg i True $ Storage r
        , ", [" ++ l ++ "]"
        , " ; 0x" ++ showHex imm ++ " --> " ++ l ] 
      , NASM_Instruction $ concat
        [ "MOV " 
        , operand_to_NASM ctxt entry cfg i True dst
        , ", "
        , operand_to_NASM ctxt entry cfg i True $ Storage r ]
      , NASM_Instruction $ concat
        [ "MOV qword " 
        , operand_to_NASM ctxt entry cfg i True $ Storage r
        , ", [Ltemp_storage_foxdec]"]
      , NASM_Comment 2 "done" ]
--}


-- | convert an operand to a NASM operand
-- Return both the translation (a String) as well as an optional annotation
operand_to_NASM ctxt entry cfg i is_addr (Storage r)          = (NASM_Operand_Address $ NASM_Addr_Compute $ empty_address { nasm_base = Just r} , [])
operand_to_NASM ctxt entry cfg i is_addr (EffectiveAddress a) =
  let (address,annot) = symbolize_address ctxt entry cfg i a in
     (NASM_Operand_EffectiveAddress address, annot)
operand_to_NASM ctxt entry cfg i is_addr (Memory a si)        = mem_operand_to_NASM ctxt entry cfg i is_addr a si
operand_to_NASM ctxt entry cfg i@(Instruction addr pre op Nothing ops annot) is_addr (Immediate imm)
  | is_addr      = 
    let (address,annot) = symbolize_address ctxt entry cfg i (AddressImm imm) in
      (NASM_Operand_Address address, annot)
  | otherwise    = (NASM_Operand_Immediate $ truncate imm $ op_size ops, [])
 where
  op_size [] = Nothing
  op_size (Immediate _: ops)      = op_size ops
  op_size (Storage r : _)         = Just $ sizeof r
  op_size (Memory a si : _)       = Just $ si
  op_size (EffectiveAddress _: _) = Just $ 8

  truncate imm Nothing   = imm
  truncate imm (Just si) = imm .&. (2^(si*8)-1)


mem_operand_to_NASM ctxt entry cfg i is_addr a si =
  let (address,annot) = symbolize_address ctxt entry cfg i a in
     (NASM_Operand_Memory (size_directive_to_NASM i si) address, annot)



-- | convert the address of an operand to a NASM address
address_to_NASM i a@(AddressPlus (AddressStorage r) a')
  | is_segment_register r = (address_to_NASM' i a'){ nasm_segment = Just r }
  | otherwise             = address_to_NASM' i a
address_to_NASM i a@(AddressPlus a' (AddressStorage r))
  | is_segment_register r = (address_to_NASM' i a'){ nasm_segment = Just r }
  | otherwise             = address_to_NASM' i a
address_to_NASM i a@(AddressStorage r)
  | is_segment_register r = empty_address{ nasm_segment = Just r }
  | otherwise             = address_to_NASM' i a
address_to_NASM i a       = address_to_NASM' i a

address_to_NASM' i a = 
  case rip_relative_to_immediate i a of
    Nothing  -> address_to_NASM'' a
    Just imm -> empty_address{ nasm_displace = Just imm }
 where
  address_to_NASM'' (AddressStorage r) = empty_address { nasm_base = Just r }
  address_to_NASM'' (AddressImm i) = empty_address{ nasm_displace = Just i }
  address_to_NASM'' (AddressPlus (AddressStorage r) (AddressImm i))  = empty_address  { nasm_base = Just r, nasm_displace = Just i } 
  address_to_NASM'' (AddressMinus (AddressStorage r) (AddressImm i)) = empty_address  { nasm_base = Just r, nasm_displace = Just (0-i) }
  address_to_NASM'' (AddressPlus (AddressStorage r) (AddressStorage r1)) = empty_address  { nasm_base = Just r, nasm_index = Just r1 }
  address_to_NASM'' (AddressPlus (AddressPlus (AddressStorage r) (AddressStorage r1)) (AddressImm i)) = empty_address  { nasm_base = Just r, nasm_index = Just r1, nasm_displace = Just i }
  address_to_NASM'' (AddressPlus (AddressStorage r) (AddressPlus (AddressStorage r1) (AddressImm i))) = empty_address  { nasm_base = Just r, nasm_index = Just r1, nasm_displace = Just i }
  address_to_NASM'' (AddressPlus (AddressStorage r) (AddressMinus (AddressStorage r1) (AddressImm i))) = empty_address  { nasm_base = Just r, nasm_index = Just r1, nasm_displace = Just  (0-i) }
  address_to_NASM'' (AddressPlus (AddressTimes (AddressStorage r) (AddressImm i)) (AddressImm i1)) = empty_address  { nasm_index = Just r, nasm_scale = i, nasm_displace = Just i1 }
  address_to_NASM'' (AddressPlus (AddressStorage r) (AddressTimes (AddressStorage r1) (AddressImm i))) = empty_address  { nasm_base = Just r, nasm_index = Just r1, nasm_scale = i }
  address_to_NASM'' (AddressPlus (AddressStorage r) (AddressPlus (AddressTimes (AddressStorage r1) (AddressImm i)) (AddressImm i1))) = empty_address  { nasm_base = Just r, nasm_index = Just r1, nasm_scale = i, nasm_displace = Just i1 }
  address_to_NASM'' (AddressPlus (AddressStorage r) (AddressMinus (AddressTimes (AddressStorage r1) (AddressImm i)) (AddressImm i1))) = empty_address  { nasm_base = Just r, nasm_index = Just r1, nasm_scale = i, nasm_displace = Just (0-i1) }
  address_to_NASM'' (AddressTimes (AddressStorage r) (AddressImm i))  = empty_address  { nasm_index = Just r, nasm_scale = i }
  address_to_NASM'' (AddressMinus (AddressTimes (AddressStorage r) (AddressImm i)) (AddressImm i1)) = empty_address  { nasm_index = Just r, nasm_scale = i, nasm_displace = Just (0-i1) }
  address_to_NASM'' a           = error $ "TODO: " ++ show a





-- | convert size directive of an operand to a NASM size directive
size_directive_to_NASM _ 1  = (1,True) -- "byte"
size_directive_to_NASM _ 2  = (2,True) -- "word"
size_directive_to_NASM _ 4  = (4,True) -- "dword"
size_directive_to_NASM i 8  
  | opcode i `elem` [ADDSD,SUBSD,DIVSD,MULSD,COMISD,UCOMISD,MINSD,MAXSD] = (8,False) -- NASM does not want size directives for these instructions
  | otherwise = (8,True) --  "qword"
size_directive_to_NASM _ 10 = (10,True) -- "tword"
size_directive_to_NASM i 16
  | opcode i == COMISD = (16,False)
  | opcode i == COMISS = (16,False)
  | otherwise          = (16,True) --  "oword" -- BUG in Capstone
size_directive_to_NASM i x  = error $ "Unknown size directive " ++ show x ++ " in instruction: " ++ show i

-- | convert opcode to a NASM opcode
opcode_to_NASM MOVABS = Just MOV
opcode_to_NASM opcode = Just opcode








----- SYMBOLIZATION -----


----- TEXT SECTIONS -----
-- | Symbolization of an address of an operand
-- Return an optional annotation as well.
symbolize_address :: Context -> Int -> CFG -> Instruction -> Address -> (NASM_Address, Annot)
symbolize_address ctxt entry cfg i a =
  case symbolize_immediate ctxt (Just (entry,cfg)) (isCall $ Instr.opcode i) <$> fromIntegral <$> rip_relative_to_immediate i a of
    Just (Just sym) -> sym 
    _               -> 
      case try_symbolize_base_of_address ctxt i a of
        Just str -> error "TODO" 
        _        -> (NASM_Addr_Compute $ address_to_NASM i a, [])


try_symbolize_base_of_address ctxt i addr = find_base addr
 where
  find_base (AddressImm imm)                  = try_symbolize_base ctxt True imm
  find_base _                                 = Nothing


-- | Symbolization of an immediate value that is used as an address
symbolize_immediate :: Context -> Maybe (Int,CFG) -> Bool -> Word64 -> Maybe (NASM_Address,Annot)
symbolize_immediate ctxt entry_cfg is_call a =
  (add_annot <$> first) `orTry` (add_annot <$> second) `orTry` relocatable_symbol ctxt a `orTry` try_symbolize_base ctxt True a
 where
  (first,second) 
    | is_call   = (find_outside_cfg,find_inside_cfg entry_cfg)
    | otherwise = (find_inside_cfg entry_cfg,find_outside_cfg)

  -- search for a block in the current cfg that starts at @a@, and if found, make a label for it
  find_inside_cfg Nothing            = Nothing 
  find_inside_cfg (Just (entry,cfg)) = ((block_label ctxt entry a . fst) <$> find block_starts_at (IM.toList $ cfg_instrs cfg))
  -- search for a block outside of the current cfg
  find_outside_cfg  = ((\a -> block_label ctxt a a 0) <$> find ((==) a) (map fromIntegral $ IM.keys $ ctxt_calls ctxt))

  block_starts_at (blockId, instrs) = instrs /= [] && addressof (head instrs) == fromIntegral a

  add_annot str = (NASM_Addr_Label str Nothing,[(a,str,0)])


-- | Symbolize (try to) an immediate address falling into the range of a section
try_symbolize_base ctxt not_part_of_larger_expression imm = within_section `orTry` (try_internal imm) `orTry` (add_annot <$> try_at_end_of_section imm)
 where
  within_section    = show_section_relative imm <$> find_section_for_address ctxt imm

  show_section_relative a sec@(segment,section,a0,_,_) = (label,[(a, section_label segment section a0, a - a0)])
   where
    label
      | not_part_of_larger_expression = NASM_Addr_Label (macro_name segment section a0 (a - a0)) $ Nothing
      | otherwise                     = NASM_Addr_Label (section_label segment section a0)       $ Just (a - a0)

  try_internal a = (\sym -> (NASM_Addr_Symbol sym,[])) <$> (IM.lookup (fromIntegral a) $ IM.filter is_internal_symbol $ ctxt_symbol_table ctxt)

  try_at_end_of_section a = end_of_section_label <$> (find (is_end_of_section a) $ si_sections $ ctxt_sections ctxt)

  is_end_of_section a (_,_,a0,sz,_) = a0 + sz == a

  add_annot str = (NASM_Addr_Label str Nothing,[(imm,str,0)])




-- see if address matches an external symbol loaded at linking time
relocatable_symbol ctxt a = (IM.lookup (fromIntegral a) (ctxt_symbol_table ctxt) >>= mk_symbol) `orTry` (find (reloc_for a) (ctxt_relocs ctxt) >>= mk_reloc)
 where
  mk_symbol sym@(PointerToLabel  l _)             = Nothing -- error $ "Reading GOT entry of address " ++ showHex a 
  mk_symbol sym@(PointerToObject o _)             = Just (NASM_Addr_Symbol sym,[(a,mk_safe_label o a,0)])
  mk_symbol sym@(AddressOfLabel  l _)             = Just (NASM_Addr_Symbol sym,[(a,mk_safe_label l a,0)])
  mk_symbol sym@(AddressOfObject o _)             = Just (NASM_Addr_Symbol sym,[(a,mk_safe_label o a,0)])
  mk_symbol sym@(Relocated_ResolvedObject str a1) = Just (NASM_Addr_Symbol sym,[(a,mk_safe_label str a,0)])

  mk_reloc (Relocation a0 a1) = 
    let label = block_label ctxt 0 a0 0 in
      Just (NASM_Addr_Label label Nothing, [(a,label,0)])

reloc_for a (Relocation a0 a1) = a == a0

rip_relative_to_immediate i (AddressImm imm)                                     = Just $ imm
rip_relative_to_immediate i (AddressPlus  (AddressStorage RIP) (AddressImm imm)) = Just $ fromIntegral (addressof i) + fromIntegral (sizeof i) + fromIntegral imm
rip_relative_to_immediate i (AddressMinus (AddressStorage RIP) (AddressImm imm)) = Just $ fromIntegral (addressof i) + fromIntegral (sizeof i) - fromIntegral imm
rip_relative_to_immediate i (AddressPlus  (AddressImm imm) (AddressStorage RIP)) = Just $ fromIntegral (addressof i) + fromIntegral (sizeof i) + fromIntegral imm
rip_relative_to_immediate i _                                                    = Nothing

is_segment_register r = r `elem` [CS,DS,ES,FS,GS,SS]






----- DATA SECTIONS -----
mk_macros ctxt = intercalate "\n" $ macros ++ [""] ++ internals
 where
  -- macros used to compute addresses relative to beginning of sections
  -- EXAMPLE:
  --    %define RELA_.rodata_0x11000(offset) (L_.rodata_0x11000 + offset)
  macros = map mk_macro $ filter (is_data_section ||| is_ro_data_section ||| is_bss_data_section) $ si_sections $ ctxt_sections ctxt
  -- internal symbols that are defined outside of existing sections
  -- EXAMPLE:
  --    %define __TMC_END__ RELA_.data_0x17000(0x4)
  internals = concatMap mk_internal $ IM.assocs $ internal_labels_outside_of_sections ctxt

  mk_internal (a,(sym,segment,section,a0,offset)) = ["%define " ++ sym ++ " " ++ show (macro_name segment section a0 offset)]

  mk_macro (segment,section,a0,sz,_) = "%define " ++ show_macro_name segment section a0 ++ "(offset) (" ++ show (section_label segment section a0) ++ " + offset)"


is_internal_symbol (AddressOfLabel _ False) = True
is_internal_symbol _                        = False

internal_labels_outside_of_sections ctxt = IM.mapMaybeWithKey mk $ IM.filterWithKey is_outside_section $ IM.filter is_internal_symbol $ ctxt_symbol_table ctxt
 where
  is_outside_section a _ = find_section_for_address ctxt (fromIntegral a) == Nothing

  mk a (AddressOfLabel sym False) = 
    case find_preceding_section a of
      Nothing -> Nothing
      Just (segment,section,a0,si,_) -> Just (sym,segment,section,a0,fromIntegral a - a0)
    
  find_preceding_section a =
    case sortBy (distance a) $ filter (is_after a) (si_sections $ ctxt_sections ctxt) of
      []      -> Nothing
      (sec:_) -> Just sec

  distance a (_,_,a0,si,_) (_,_,a0',si',_) = compare (fromIntegral a - a0 - si) (fromIntegral a - a0' - si')
  is_after a (_,_,a0,si,_) = fromIntegral a >= a0 + si





-- TODO to Base
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p q a = p a || q a
infixr 2 |||





ro_data_section ctxt = generic_data_section ctxt is_ro_data_section binary_read_ro_data
data_section ctxt    = generic_data_section ctxt is_data_section    binary_read_data

generic_data_section ctxt pick_section read_from = 
  map mk_data_section $ filter pick_section $ si_sections $ ctxt_sections ctxt
 where
  mk_data_section (segment,section,a0,sz,align) =
    let (entries,labels) = runState (mk_data_entries 0 segment section a0 sz) IM.empty in
      NASM_DataSection (segment,section,a0) (fromIntegral align) (add_label 0 (section_label segment section a0) labels) entries


  --mk_data_section (segment,section,a0,sz,align) = intercalate "\n" $
  -- [ "section " ++ section ++ " align=" ++ show align
  -- , section_label segment section a0 ++ ":" ]
  -- ++
  -- mk_data_entries 0 segment section a0 sz
  -- ++
  -- ["\n\n"] 

  mk_data_entries offset segment section a0 sz 
    | offset > sz  = return []
    | offset == sz = do
      modify $ add_label offset (end_of_section_label (segment,section,0,0,0))
      return []
    | otherwise = 
      case takeWhileString offset a0 sz of
        []  -> mk_data_entries_no_string offset segment section a0 sz
        str -> let offset' = offset + fromIntegral (length str) in
                 if offset' < sz && read_byte offset' a0 == 0 then do
                   entries <- mk_data_entries (offset' + 1) segment section a0 sz
                   return $ (DataEntry_String (a0 + offset) str) : entries
                   -- ["db `" ++ concat str ++ "`" ++ ", 0"] ++ mk_data_entries (offset' + 1) segment section a0 sz
                 else
                   mk_data_entries_no_string offset segment section a0 sz

  mk_data_entries_no_string offset segment section a0 sz =
    case find_reloc offset a0 of
      Just (Relocation _ a1) -> do
        entries <- mk_data_entries (offset+8) segment section a0 sz
        modify $ add_label offset (mk_reloc_label (a0 + offset))
        return $ (DataEntry_Pointer (a0 + offset) $ try_symbolize_imm ctxt a1) : entries
      --  [mk_reloc_label (a0 + offset) ++ ":"] ++ ["dq " ++ try_symbolize_imm ctxt a1] ++ mk_data_entries (offset+8) segment section a0 sz
      _ -> case IM.lookup (fromIntegral $ a0 + offset) $ ctxt_symbol_table ctxt of
             Just (PointerToLabel sym _) -> do
               entries <- mk_data_entries (offset+8) segment section a0 sz
               return $ (DataEntry_Pointer (a0 + offset) (NASM_Addr_Symbol (PointerToLabel sym False),[])) : entries
             -- Just sym -> error $ show (showHex (a0+offset),sym)
             _ -> do
               entries <- mk_data_entries (offset+1) segment section a0 sz
               return $ (DataEntry_Byte (a0 + offset) $ read_byte offset a0) : entries
               -- ["db 0" ++ showHex (read_byte offset a0) ++ "h"] ++ mk_data_entries (offset+1) segment section a0 sz


  mk_reloc_label a0 = block_label ctxt 0 a0 0

  takeWhileString offset a0 sz
    | find_reloc offset a0 /= Nothing        = []
    | offset >= sz                           = []
    | valid_char (w2c $ read_byte offset a0) = (read_byte offset a0) : takeWhileString (offset+1) a0 sz
    | otherwise                              = []

  valid_char c = c `elem` "!@#$%^&*()_-+={}[]:;|/?<>,. ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890'\"" ++ escape_chars
  escape_chars = "`\\\n\t"


  add_label offset label = IM.insertWith S.union (fromIntegral offset) (S.singleton label)


  find_reloc offset a0 = find (reloc_for $ fromIntegral (a0 + offset)) $ ctxt_relocs ctxt
  read_byte  offset a0 = 
    case read_from (ctxt_binary ctxt) (fromIntegral $ a0 + offset) 1 of
      Just [v] -> v

try_symbolize_imm ctxt a1 = 
  case symbolize_immediate ctxt Nothing False $ fromIntegral a1 of
    Just (str,annot) -> if "RELA_.text" `isPrefixOf` (show str) then traceShow ("ERROR: UNTRANSLATED ENTRY ADDRESS " ++ showHex a1) (str,annot) else (str,annot) -- show str ++ "    ; " ++ render_annot annot
    Nothing          -> error $ "ERROR: could not symbolize relocated immediate value 0x" ++ showHex a1

bss_data_section ctxt = 
  map mk_bss_data_section $ filter is_bss_data_section $ si_sections $ ctxt_sections ctxt
 where
  mk_bss_data_section (segment,section,a0,sz,align) = 
    let (entries,labels) = runState (mk_bss segment section a0 sz) IM.empty in
      NASM_DataSection (segment,section,a0) (fromIntegral align) (IM.insert 0 (S.singleton $ section_label segment section a0) labels) entries
-- "section " ++ section ++ " align=" ++ show align ++ "\n" ++ section_label segment section a0 ++ ":\n" ++ mk_bss segment section a0 sz

  mk_bss segment section a0 sz =
    case sort $ map get_addr $ filter (was_relocated_and_in a0 sz) $ S.toList $ ctxt_relocs ctxt of
      [] -> do
        modify $ addlabel sz (end_of_section_label (segment,section,0,0,0))
        return $ [DataEntry_BSS $ fromIntegral sz]
      (a:_) -> do
        modify $ addlabel (a - a0) (mk_reloc_label a)
        entries <- mk_bss segment section (fromIntegral a) (sz + a0 - fromIntegral a)
        return $ (DataEntry_BSS (fromIntegral $ a - a0)) : entries

     -- [] ->  "resb " ++ show sz ++ "\n" ++ end_of_section_label (segment,section,0,0,0) ++ ":"
     -- (a:_) -> "resb " ++ show (fromIntegral a - a0) ++ "\n" ++ mk_reloc_label a ++ ":\n" ++ mk_bss segment section (fromIntegral a) (sz + a0 - fromIntegral a)

  mk_reloc_label a0 = block_label ctxt 0 a0 0

  addlabel offset label = IM.insertWith S.union (fromIntegral offset) (S.singleton label)

  get_addr (Relocation a0 _) = a0
  was_relocated_and_in a0 sz (Relocation a a1) = fromIntegral a0 < a && a < fromIntegral (a0 + sz)-- TODO note strict inequality here



-- Get an overview of all indirections (entry,cfg,i) where entry is the entry-point of the function, cfg is the CFG of the function and i is an in instruction that performs an indirection
get_indirections_per_function ctxt = concatMap get $ S.toList $ ctxt_get_function_entries ctxt
 where
  get entry =
    let Just cfg = IM.lookup entry (ctxt_cfgs ctxt) in
      map (\i -> (entry,cfg,i)) $ filter (indirection_in_cfg cfg) $ IM.assocs $ ctxt_inds ctxt
  indirection_in_cfg cfg (a,_) = any (any (\i -> addressof i == fromIntegral a)) $ cfg_instrs cfg

get_terminals_per_function ctxt = concatMap get $ S.toList $ ctxt_get_function_entries ctxt
 where
  get entry =
    let Just cfg = IM.lookup entry (ctxt_cfgs ctxt) in
      filter (is_terminal_call ctxt entry) $ concat $ IM.elems $ cfg_instrs cfg


mk_jump_table ctxt (entry,cfg,(a,Indirection_JumpTable (JumpTable index bnd trgt tbl))) = intercalate "\n" $ 
  [ "; JUMP TABLE: entry == " ++ showHex entry ++ ", instr@" ++ showHex a
  , "section .bss"
  , show (label_jump_table_temp_storage entry a 0) ++ ":"
  , "resb 8"
  , show (label_jump_table_temp_storage entry a 1) ++ ":"
  , "resb 8"
  , "section .rodata"
  , show (label_jump_table_redirect_data entry a) ++ ":"]
  ++
  map mk_entry (sortBy (compare `on` fst) $ IM.assocs tbl)
 where
  mk_entry (idx,trgt) = 
    case symbolize_immediate ctxt (Just (entry,cfg)) False trgt of
      Just (str,annot) -> "dq " ++ show str ++ " ; index " ++ show idx ++ "    ; " ++ render_annot annot
      Nothing          -> "ERROR: cannot symbolize jump target:" ++ showHex trgt
mk_jump_table ctxt (entry,cfg,(a,_)) = []








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

reg_of_size RAX  8 = RAX
reg_of_size RBX  8 = RBX
reg_of_size RCX  8 = RCX
reg_of_size RDX  8 = RDX
reg_of_size R8   8 = R8
reg_of_size R9   8 = R9
reg_of_size R10  8 = R10
reg_of_size R11  8 = R11
reg_of_size R12  8 = R12
reg_of_size R13  8 = R13
reg_of_size R14  8 = R14
reg_of_size R15  8 = R15
reg_of_size RAX  4 = EAX
reg_of_size RBX  4 = EBX
reg_of_size RCX  4 = ECX
reg_of_size RDX  4 = EDX
reg_of_size R8   4 = R8D
reg_of_size R9   4 = R9D
reg_of_size R10  4 = R10D
reg_of_size R11  4 = R11D
reg_of_size R12  4 = R12D
reg_of_size R13  4 = R13D
reg_of_size R14  4 = R14D
reg_of_size R15  4 = R15D
reg_of_size RAX  1 = AL
reg_of_size RBX  1 = BL
reg_of_size RCX  1 = CL
reg_of_size RDX  1 = DL
reg_of_size R8   1 = R8B
reg_of_size R9   1 = R9B
reg_of_size R10  1 = R10B
reg_of_size R11  1 = R11B
reg_of_size R12  1 = R12B
reg_of_size R13  1 = R13B
reg_of_size R14  1 = R14B
reg_of_size R15  1 = R15B

reg_of_size reg si = error $ "Make register " ++ show reg ++ " of size " ++ show si 

find_element_not_in (a:as) x = if a `elem` x then find_element_not_in as x else a


find_unused_register :: [Register] -> [Instruction] -> Register
find_unused_register regs instrs = 
  let used_regs = concatMap (regs_of_ops . get_ops) instrs in
    find_element_not_in regs used_regs
 where
  get_ops i@(Instruction addr pre op Nothing ops annot) = ops




-- | There is one specific symbol frequently encountered for which we cannot find the appropiate library to load.
-- It is related to debugging information (the -g option of GCC).
-- We therefore pvodie our own implementation: just a dummy, which is what the real function seems to do as well.
__gmon_start_implementation = "void __gmon_start__ () { return; }"

