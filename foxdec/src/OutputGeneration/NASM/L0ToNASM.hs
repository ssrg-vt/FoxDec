{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StrictData #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : L0ToNASM
Description : Lift the L0 representation of the binary to symbolized and recompilable NASM.
-}



module OutputGeneration.NASM.L0ToNASM where --  (lift_L0_to_NASM, render_NASM, __gmon_start_implementation, NASM) where


import Base
import Config

import OutputGeneration.NASM.NASM

import Data.CFG
import Data.Size
import Data.SPointer
import Data.SValue

import Binary.FunctionNames
import Binary.Elf
import WithAbstractPredicates.ControlFlow
import Conventions

import Data.X86.Opcode
import Data.X86.Instruction
import Data.X86.Register

import Binary.Generic

import Data.L0
import Data.JumpTarget
import Data.Symbol
import Data.Indirection

import WithAbstractSymbolicValues.Class



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

import GHC.Base hiding (Symbol,foldr)




type LiftedC bin = Lifting bin (Sstate SValue SPointer) (FInit SValue SPointer) SValue


-- | Lift an L0 representation to position-independent NASM
lift_L0_to_NASM :: BinaryClass bin => LiftedC bin -> NASM
lift_L0_to_NASM l@(bin,_,l0) = NASM mk_externals mk_globals mk_sections' $ mk_jump_tables ++ [mk_temp_storage] 
 where
  mk_externals        = externals l
  mk_globals          = with_start_global bin $ S.difference (S.map strip_GLIBC $ binary_get_global_symbols bin) mk_externals

  mk_text_sections    = map (entry_to_NASM l) $ map fromIntegral $ S.toList $ l0_get_function_entries l0
  mk_ro_data_section  = NASM_Section_Data $ ro_data_section l
  mk_data_section     = NASM_Section_Data $ data_section l
  mk_bss_section      = NASM_Section_Data $ bss_data_section l
  mk_resolved_relocs  = NASM_Section_Data $ resolved_relocs_section l
  mk_undefined_labels = undefined_internal_global_labels l

  mk_sections         = mk_text_sections ++ [mk_ro_data_section, mk_data_section, mk_bss_section, mk_resolved_relocs] ++ mk_undefined_labels
  get_annots          = mk_annots l mk_sections
  mk_sections'        = map (add_labels_to_data_sections get_annots mk_externals) mk_sections

  mk_temp_storage     = "section .bss\nLtemp_storage_foxdec:\nresb 8"
  mk_jump_tables      = filter ((/=) []) $ map (mk_jump_table l) $ get_indirections_per_function l


with_start_global bin = 
  case head (binary_entry bin) of
    0     -> id
    entry -> S.insert "_start" 

resolved_relocs_section l@(bin,_,l0) =
  case filter is_relocation $ IM.assocs $ binary_get_symbol_table bin of
    [] -> []
    relocs -> map mk_reloc relocs
 where
  is_relocation (_,Relocated_ResolvedObject _ _) = True
  is_relocation _ = False

  mk_reloc (a0,Relocated_ResolvedObject str a1) = 
    NASM_DataSection ("",".data",fromIntegral a0) 0 [(fromIntegral a0, DataEntry_Label $ Label (fromIntegral a0) $ strip_GLIBC str), (fromIntegral a0, DataEntry_Pointer (try_symbolize_imm l a1)) ]


undefined_internal_global_labels l@(bin,_,l0) =
  case filter is_global_and_internal_and_outside_of_any_section $ IM.assocs $ binary_get_symbol_table bin of
    [] -> []
    ls -> map mk_datasection ls
 where
  is_global_and_internal_and_outside_of_any_section (a,sym) = and
    [ is_internal_symbol sym
    , symbol_to_name sym `S.member` binary_get_global_symbols bin
    , is_not_defined_elsewhere a ]

  mk_datasection (a,sym) = NASM_Section_Data [ NASM_DataSection ("",".bss",fromIntegral a) 8 $ mk_entries (fromIntegral a) sym ]
  mk_entries a sym =
    [ (a,DataEntry_Label $ Label a (symbol_to_name sym) )
    , (a,DataEntry_BSS 1) ]

  is_not_defined_elsewhere a = (find (is_section_for a) $ si_sections $ binary_get_sections_info bin) == Nothing

  is_section_for a (segment,section,a0,sz,_) = a0 <= fromIntegral a && fromIntegral a <= a0+sz


-- | Rendering NASM to a String
render_NASM l (NASM exts globals sections footer) = intercalate "\n\n\n" $ [
    render_externals,
    render_globals,
    "default rel" ]
    ++ render_sections
    ++ footer
    -- ++ [ render_annots ]
 where
  render_annots = intercalate "\n" $
    [ "; TEMP_OBJECTs are memory locations inserted by FoxDec not present in the original binary"
    , "; EXT_OBJECTs are external objects such as stderr and stdout"
    , "; TERMINAL_CALLs are addresses of instructions in the original binary that always terminate and do not return"
    , "; The remainder is a mapping from original addresses to internal labels"
    , "ifdef COMMENT"
    , intercalate "\n" $ map show_temp_object $ Label 0 "Ltemp_storage_foxdec" : (concatMap get_temp_object $ get_indirections_per_function l)
    , intercalate "\n" $ map render_terminal $ get_terminals_per_function l
    , intercalate "\n" $ map show_external_object $ external_objects l
    , show_annots $ M.toList $ mk_annots l sections
    , "endif"
    ]


  render_terminal i = "TERMINAL_CALL 0x" ++ showHex (inAddress i) 

  render_externals = intercalate "\n" $ map ((++) "extern ") $ S.toList $ exts -- S.difference exts (ctxt_get_globals ctxt)
  render_sections  = map render_section sections
  render_globals   = intercalate "\n" $ map ((++) "global ") $ S.toList $ globals -- S.difference (ctxt_get_globals ctxt) exts

  render_section (NASM_Section_Text ts) = show ts
  render_section (NASM_Section_Data ds) = intercalate "\n\n" (map show ds) ++ "\n\n"




  show_external_object str = "EXT_OBJECT " ++ str
  
  get_temp_object (entry,cfg,(a,inds)) = concatMap (get_temp_object_for_ind entry a) inds

  get_temp_object_for_ind entry a (Indirection_JumpTable (JumpTable index bnd trgt tbl)) =
    [
     label_jump_table_temp_storage entry a 0
    ,label_jump_table_temp_storage entry a 1
    ]
  get_temp_object_for_ind entry a _ = []

  show_temp_object obj = "TEMP_OBJECT " ++ show obj







-- | get the external functions and objects 
externals l@(bin,_,l0) = S.insert "exit" $ S.fromList $ map (strip_GLIBC . symbol_to_name) $ filter is_relocation $ IM.elems $ binary_get_symbol_table bin
 where
  is_relocation (PointerToLabel str ex)  = ex && str /= ""
  is_relocation (PointerToObject str ex) = ex && str /= ""
  is_relocation (AddressOfObject str ex) = ex && str /= ""
  is_relocation (AddressOfLabel str ex) = ex && str /= ""
  is_relocation _ = False

-- | get the external objects
external_objects l@(bin,_,l0) = map (strip_GLIBC . symbol_to_name) $ filter is_relocation $ IM.elems $ binary_get_symbol_table bin
 where
  is_relocation (PointerToObject str ex) = ex && str /= ""
  is_relocation (AddressOfObject str ex) = ex && str /= ""
  is_relocation _ = False





is_address_of_symbol (AddressOfObject str ex) = str /= ""
is_address_of_symbol (AddressOfLabel str ex)  = str /= ""
is_address_of_symbol _                        = False

is_address_of_internal_symbol (AddressOfObject str ex) = str /= "" && not ex
is_address_of_internal_symbol (AddressOfLabel str ex)  = str /= "" && not ex
is_address_of_internal_symbol _                        = False

-- | Creating labels
-- Given the entry address of the current function, the blockID of the current basic block,
-- map an address to a label.
-- First, try to see if it matches the _start symbol.
-- Then, try to map the address to a known internal synbol (unstripped binaries may have such symbols available)
-- Then, try to see if at the address a relocation is stored, and use that lavel if so.
-- Otherwise, make a new custom label.
block_label l@(bin,_,l0) entry a blockID = (try_start_symbol `orTry` try_symbol `orTry` try_relocation_label) `orElse` custom_label
 where
  -- Try if the address matches the entry point of the ELF
  try_start_symbol
    | head (binary_entry bin) /= 0 && a == head (binary_entry bin) = Just $ Label a "_start"
    | otherwise = Nothing
  -- Try if the address matches a known symbol
  try_symbol = do
    sym  <- (IM.lookup (fromIntegral a) $ IM.filter is_address_of_symbol $ binary_get_symbol_table bin)
    let name = symbol_to_name sym
    if {--nasm_with_safe_labels (ctxt_config ctxt) &&--} blockID /= 0 then
      return $ Label a $ "L" ++ showHex entry ++ "_" ++ name
    else
      return $ Label a name
  -- Try to see if the address stores a relocation
  try_relocation_label = reloc_label <$> find (reloc_for a) (binary_get_relocations bin)

  -- Make a new label based on the entry and blockID
  custom_label = Label a $ "L" ++ showHex entry ++ "_" ++ show blockID
  -- Make a label for relocation
  reloc_label (Relocation a0 a1) = Label a $ "L_reloc_0x" ++ showHex a0 ++ "_0x" ++ showHex a1


-- Make a label for the start of a section
section_label l@(bin,_,l0) segment section a0 addr = try_symbol `orElse` (Label addr $ "L" ++ segment ++ "_" ++ section ++ "_0x" ++ showHex addr)
 where
  try_symbol = do
    sym  <- (IM.lookup (fromIntegral addr) $ IM.filter is_address_of_internal_symbol $ binary_get_symbol_table bin)
    let name = symbol_to_name sym
    return $ Label addr name
 
-- Make a label for the end of a section
-- TODO: should be same as section_label (i.e., lookup symbol first)?
end_of_section_label l@(bin,_,l0) (segment,section,a0,sz,_) = try_symbol `orElse` (Label (a0+sz) $ "L" ++ segment ++ "_" ++ section ++ "_END")
 where
  try_symbol
    | find_section_for_address bin (a0+sz) == Nothing = do
      sym  <- IM.lookup (fromIntegral $ a0+sz) $ IM.filter is_address_of_symbol $ binary_get_symbol_table bin
      let name = symbol_to_name sym
      return $ Label (fromIntegral $ a0+sz) name
    | otherwise = Nothing


-- Make a "safe" label (no duplicates)
mk_safe_label str a = Label a $ strip_GLIBC str -- ++ "_0x" ++ showHex a

-- Make a label indicating that this block is expected to terminate
terminating_label l entry i blockID =
  let Label a lab = block_label l entry (inAddress i) blockID in
    Label a $ "FOXDEC_TERMINATING_" ++ lab
-- Make a label indicating that this block is a DONTCARE
dontcare_label l entry a_end blockID =
  let Label a lab = block_label l entry a_end blockID in
    Label a $ "FOXDEC_DONTCARE_" ++ lab
-- Make a label indicating that this instruction is halting
halting_label l entry a blockID =
  let Label a' lab = block_label l entry a blockID in
    Label a' $ lab ++ "_HLT"







-- | convert a given function entry to a NASM text section
entry_to_NASM :: BinaryClass bin => LiftedC bin -> Word64 -> NASM_Section
entry_to_NASM l@(bin,_,l0) entry = NASM_Section_Text $ NASM_TextSection mk_header mk_blocks mk_control_flow
 where
  Label _ mk_header = block_label l (fromIntegral entry) (fromIntegral entry) 0 -- function_name_of_entry ctxt entry
  Just cfg          = IM.lookup (fromIntegral entry) (l0_get_cfgs l0)
  mk_control_flow   = cfg_edges cfg
  mk_blocks         = 
    let blocks      = IM.keys $ cfg_blocks cfg in
      cfg_blocks_to_NASM l entry cfg $ (sortBy (start_address_le cfg) blocks)

  start_address_le cfg blockID1 blockID2 = 
    case (start_address_of_block cfg blockID1, start_address_of_block cfg blockID2) of
      (Just a1, Just a2) -> compare a1 a2
      (Just a1, _)       -> LT
      (_, Just a2)       -> GT
      _ -> EQ


show_annots :: [(Word64, S.Set NASM_Label)] -> String
show_annots = intercalate "\n" . map show_entry
 where
  show_entry (a,ls) = "0x" ++ showHex a ++ ": " ++ (show_labels $ S.toList ls)
  show_labels []  = ""
  show_labels [l] = show l
  show_labels ls  = show ls

mk_annots l@(bin,_,l0) sections = M.fromListWith S.union $ map (\(a,l) -> (a,S.singleton l)) $ block_mapping ++ annot_from_labels
 where
  block_mapping = concatMap (mk_block_mapping_for_entry) $ S.toList $ l0_get_function_entries l0
  annot_from_labels = concatMap get_annots_section sections

  mk_block_mapping_for_entry entry = 
    let cfg = l0_get_cfgs l0 IM.! entry in
      map (mk_block_mapping_for_block entry cfg) (IM.keys $ cfg_blocks cfg)

  mk_block_mapping_for_block entry cfg blockID = 
    let a = inAddress $ head $ get_block_instrs cfg blockID in
      (a, block_label l entry a blockID)
  get_block_instrs cfg blockID = cfg_instrs cfg IM.! blockID 


  get_annots_section (NASM_Section_Text sec)  = get_annots_textsection sec
  get_annots_section (NASM_Section_Data secs) = concatMap get_annots_datasection secs

  get_annots_textsection (NASM_TextSection _ blocks _) = concatMap get_annots_line $ concat $ map snd blocks

  get_annots_line (NASM_Line (NASM_Instruction _ _ _ _ annot _)) = annot
  get_annots_line _ = []

  get_annots_datasection (NASM_DataSection _ _ es) = concat $ mapMaybe get_annot_from_data_entry es

  get_annot_from_data_entry (a,DataEntry_Pointer (_,annot)) = Just annot
  get_annot_from_data_entry _ = Nothing




-- | convert a list of basic blocks to NASM instructions
-- Note that the order dictates if additional jumps need to be inserted
cfg_blocks_to_NASM :: BinaryClass bin => LiftedC bin -> Word64 -> CFG -> [Int] -> [(Int, [NASM_Line])]
cfg_blocks_to_NASM l entry cfg []                             = []
cfg_blocks_to_NASM l entry cfg [blockID0]                     = [(blockID0,cfg_block_to_NASM l entry cfg blockID0 $ Nothing)]
cfg_blocks_to_NASM l entry cfg (blockID0:blocks@(blockID1:_)) = (blockID0,cfg_block_to_NASM l entry cfg blockID0 $ Just blockID1) : cfg_blocks_to_NASM l entry cfg blocks



-- | convert a single basic block to NASM instructions
-- A block is translated to a header and its instructions
cfg_block_to_NASM :: BinaryClass bin => LiftedC bin -> Word64 -> CFG -> Int -> Maybe Int -> [NASM_Line]
cfg_block_to_NASM l@(bin,_,l0) entry cfg blockID blockID1 = block_header : mk_block
 where
  -- header
  block_header          = NASM_Comment 0 $ "Entry " ++ showHex entry ++ "; block " ++ show blockID ++ "; address " ++ showHex (inAddress $ head block_instrs)
  -- instructions: the label and the body
  mk_block              = block_label' ++ insert_nop_if_empty block_body

  -- the label
  block_label'          = [NASM_Label $ block_label l entry (inAddress $ head block_instrs) blockID] 
  -- the body:
  --   in case of a block ending in a jump table, instructions are inserted at beginning and end of the block
  --   in case of a block ending in an indirection resolved to a single target, translate the last instruction accordingly
  --   in case of a block ending in an unresolved indirection resolved to a single target, translate as normal but annotate
  --   in case of no indirection, translate normally but insert an extra jump to the next block if neccessary
  block_body
    | block_instrs == [] = mk_block_instrs block_instrs ++ block_end block_instrs
    | otherwise =
      case IM.lookup (fromIntegral $ inAddress $ last block_instrs) $ l0_indirections l0 of
        Nothing -> mk_block_instrs block_instrs ++ block_end block_instrs
        Just inds -> block_body_with_indirection inds

  block_body_with_indirection inds =
    case partition isJumpTable $ S.toList inds of
      ([t],_) -> block_body_with_jump_table t
      ([],inds) -> if Indirection_Unresolved `elem` inds then block_body_with_unresolved else block_body_with_resolved inds

  block_body_with_jump_table (Indirection_JumpTable (tbl@(JumpTable index _ _ _))) =
    let ByteSize si = operand_size index
        reg  = reg_of_size (find_unused_register register_set block_instrs) si
        reg' = reg_of_size (find_unused_register [ r | r <- register_set, real_reg r /= real_reg reg] block_instrs) 8 in
      jmp_table_init tbl reg reg' (last block_instrs) ++ mk_block_instrs (init block_instrs) ++ jmp_table_end tbl reg reg' (last block_instrs)

  block_body_with_unresolved = mk_block_instrs block_instrs ++ unresolved_end ++ block_end block_instrs

  block_body_with_resolved inds = 
    let trgts = nub $ map get_resolved_target inds in
      mk_block_instrs (init block_instrs) ++ resolved_jump trgts (last block_instrs) ++ block_end block_instrs


  insert_nop_if_empty [] = [NASM_Line $ mk_nasm_instr NOP [] `withComment` "NOP inserted"]
  insert_nop_if_empty b  = b

  isJumpTable (Indirection_JumpTable _) = True
  isJumpTable _ = False

  get_resolved_target (Indirection_Resolved trgt) = trgt 


  -- make a block of regular non-control-flow instructions
  mk_block_instrs instrs = concatMap (instr_to_NASM l entry cfg blockID) $ filter_unnecessary_jumps instrs

  -- for a normal (non-indirection) block, see if we need to insert an additional jump to the next block
  -- Necessary when that next block is not the next to be translated.
  block_end instrs
    | instrs == [] = []
    | is_proper_block_end_instruction (inOperation $ last instrs) = []
    --  | blockID1 /= Nothing && (IM.lookup blockID (cfg_edges cfg) == Just (IS.singleton $ fromJust blockID1) = []
    | otherwise = mk_extra_jmp


  -- A jump table is implemented (TODO more comments)
  jmp_table_init t@(JumpTable index bound trgt tbl) reg reg' i =
    let (trgt_str,annot) = operand_to_NASM l entry cfg empty_instr False index in
      [ NASM_Comment 2 $ "Resolved indirection:"
      , NASM_Comment 2 $ show t
      , NASM_Line $ mk_nasm_instr MOV  [label_to_mem_operand (8,False) $ label_jump_table_temp_storage entry (inAddress i) 0, NASM_Operand_Reg (real_reg reg)]
      , NASM_Line $ mk_nasm_instr MOV  [label_to_mem_operand (8,False) $ label_jump_table_temp_storage entry (inAddress i) 1, NASM_Operand_Reg (real_reg reg')]
      , NASM_Line $ mk_nasm_instr MOV  [NASM_Operand_Reg reg, trgt_str] `withAnnot` annot
      ]
      ++
      (if regSize reg < ByteSize 4 then [NASM_Line $ mk_nasm_instr MOVZX [NASM_Operand_Reg $ reg_of_size (real_reg reg) 4, NASM_Operand_Reg reg]] else [])
      ++
      [ NASM_Comment 2 $ "Start of block"  ]


  jmp_table_end t@(JumpTable index bound trgt tbl) reg reg' last_instr =
    let (trgt_str,annot) = operand_to_NASM l entry cfg empty_instr False trgt 
        a_end            = inAddress last_instr in
      [ NASM_Comment 2 $ "End of block"
      , NASM_Line $ mk_nasm_instr LEA [NASM_Operand_Reg reg', label_to_eff_operand $ label_jump_table_redirect_data entry a_end ]
      , NASM_Line $ mk_nasm_instr LEA [NASM_Operand_Reg reg', NASM_Operand_EffectiveAddress $ NASM_Addr_Compute $ empty_address {nasm_base = Just reg', nasm_index = Just (reg_of_size (real_reg reg) 8), nasm_scale = 8} ]
      ]
      ++
      (if isMemory trgt then
        [ NASM_Line $ mk_nasm_instr MOV [NASM_Operand_Reg (real_reg reg'), NASM_Operand_Memory (8,True) $ NASM_Addr_Compute $ empty_address {nasm_base = Just reg' }]  `withAnnot` annot
        , NASM_Line $ mk_nasm_instr MOV [trgt_str, NASM_Operand_Reg (real_reg reg')] ]
      else
        [ NASM_Line $ mk_nasm_instr MOV [trgt_str, NASM_Operand_Memory (8,True) $ NASM_Addr_Compute $ empty_address {nasm_base = Just reg' }] `withAnnot` annot ])
      ++
      [ NASM_Line $ mk_nasm_instr MOV [NASM_Operand_Reg (real_reg reg), label_to_mem_operand (8,False) $ label_jump_table_temp_storage entry a_end 0 ]
      , NASM_Line $ mk_nasm_instr MOV [NASM_Operand_Reg (real_reg reg'), label_to_mem_operand (8,False) $ label_jump_table_temp_storage entry a_end 1 ]
      , NASM_Comment 2 $ "Executing resolved indirect jump"
      , NASM_Label $ dontcare_label l entry a_end blockID
      ]
      ++
      mk_jmp_call_instr l entry cfg blockID last_instr

  isMemory (Op_Mem _ _ _ _ _ _ _) = True
  isMemory (Op_Near op) = isMemory op
  isMemory (Op_Far op) = isMemory op
  isMemory _ = False


  -- A resolved jump to a single known target
  resolved_jump [External f] i@(Instruction addr pre op Nothing ops annot) =
    [ NASM_Comment 2 $ "Resolved indirection: " ++ show (head ops) ++ " --> " ++ f 
    , NASM_Line $ NASM_Instruction (mk_NASM_prefix pre) (opcode_to_NASM op) [NASM_Operand_Address $ NASM_Addr_Symbol $ PointerToLabel f True] "" [] (Just i)]
  resolved_jump [ImmediateAddress imm] i@(Instruction addr pre op Nothing ops annot) = 
    [ NASM_Comment 2 $ "Resolved indirection: " ++ show (head ops) ++ " --> " ++ showHex imm ]
    ++
    mk_jmp_call_instr l entry cfg blockID i
  resolved_jump trgts i@(Instruction addr pre op Nothing ops annot) =
    [ NASM_Comment 2 $ "Resolved indirection: " ++ show (head ops) ++ " --> " ++ show trgts ]
    ++
    instr_to_NASM l entry cfg blockID i


  -- An unresolved indirection annotation
  unresolved_end = [ NASM_Comment 2 $ "Unresolved indirection" ]

  filter_unnecessary_jumps instrs
    | instrs == [] = []
    | otherwise    = (filter (not . isJump . inOperation) $ init instrs) ++ [last instrs]
  is_proper_block_end_instruction i = isRet i || isJump i || isHalt i

  Just block_instrs = IM.lookup blockID $ cfg_instrs cfg

  empty_instr :: Instruction
  empty_instr = Instruction 0 [] NOP Nothing [] 0

  mk_extra_jmp =
    case next_rips l (Just $ last block_instrs) of -- TODO assumes fall through is last/second
      UnresolvedTarget -> []
      JustRips []      -> mk_extra_hlt $ last block_instrs
      JustRips [a]     -> mk_extra_jump_maybe a
      JustRips [_,a]   -> mk_extra_jump_maybe a
      Terminal         -> mk_extra_hlt $ last block_instrs
      x                -> [NASM_Comment 0 $ "JMP ERROR CANNOT DETERMINE NEXT BLOCK" ++ show x]

  mk_extra_hlt last_instr
    | isCall (inOperation last_instr) =
      let a = inAddress last_instr + (fromIntegral $ inSize last_instr) in
        if is_start_of_block_anywhere l a then
          []
        else  
          let label = halting_label l entry a blockID in
            [ 
              NASM_Label label,
              NASM_Line $ mk_nasm_instr HLT [] `withComment` "should never be reached" `withAnnot` [(a,label)]
            ]
    | otherwise = []

  mk_extra_jump_maybe a
    | blockID1 /= Nothing && Just a == start_address_of_block cfg (fromJust blockID1) = []
    | otherwise = [NASM_Line $ mk_nasm_instr JMP [NASM_Operand_Address $ fst $ fromJust $ symbolize_immediate l (Just (fromIntegral entry,cfg)) False $ fromIntegral a] `withComment` "jump is inserted"]



start_address_of_block cfg blockID = fromIntegral . inAddress . head <$> (IM.lookup blockID $ cfg_instrs cfg)

label_jump_table_temp_storage  entry a n = Label 0 $ "L_jmp_tbl_temp_storage_" ++ showHex entry ++ "_" ++ showHex a ++ (if n == 0 then "_0" else "_1")
label_jump_table_redirect_data entry a = Label 0 $ "L_jmp_tbl_" ++ showHex entry ++ "_" ++ showHex a


is_start_of_block_anywhere l@(bin,_,l0) a =
  let cfgs   = IM.elems $ l0_get_cfgs l0
      blocks = concatMap (IM.elems . cfg_instrs) cfgs in
    any (\block -> inAddress (head block) == a) blocks
  


-- | convert an instruction to a NASM instruction
instr_to_NASM l entry cfg blockID i@(Instruction addr pre op Nothing ops annot)
 | op == NOP                             = []
 | op == ENDBR64                         = []
 | no_ops pre op                         = [NASM_Line $ mk_normal_instr l entry cfg $ Instruction addr pre op Nothing [] annot]
 | is_cf op                              = mk_jmp_call_instr l entry cfg blockID i
 | some_operand_reads_GOT_entry l i ops  && op /= LEA = map NASM_Line $ mk_GOT_entry_instr l entry cfg i
 | otherwise                             = [NASM_Line $ mk_normal_instr l entry cfg i]
 where
  is_cf op      = isCall op || isJump op || isCondJump op
  no_ops pre op = op `elem` [STOSB,STOSD,STOSQ, SCASB,SCASD, CMPSB] || is_string_mov op


  is_string_mov op
    | op `elem` [MOVSB, MOVSW, MOVSD, MOVSQ] =
      not (length ops == 2 && (operand_size (ops!!0) == ByteSize 16 || operand_size (ops!!1) == ByteSize 16))
      -- TODO
      --   REP MOVSQ QWORD PTR [RDI,8], QWORD PTR [RSI,8]
      --   MOVSD QWORD PTR [RSP+0x8],xmm0
    | otherwise = False


-- Make a normal instruction
mk_normal_instr l entry cfg i@(Instruction addr pre op Nothing ops annot) = mk_instr
 where
  mk_instr = NASM_Instruction (mk_NASM_prefix pre) (opcode_to_NASM op) (map fst mk_ops) "" mk_annot (Just i)
  mk_ops = map (operand_to_NASM l entry cfg i False) ops
  mk_annot = concat $ filter ((/=) []) $ map snd mk_ops

-- Make a JUMP/CALL instruction
mk_jmp_call_instr :: BinaryClass bin => LiftedC bin -> Word64 -> CFG -> Int -> Instruction -> [NASM_Line]
mk_jmp_call_instr l@(bin,_,l0) entry cfg blockID i@(Instruction addr pre op Nothing [op1] annot) =
  add_label_if_terminating $ use_jump_target $ jump_target_for_instruction bin i
 where
  use_jump_target j@(External sym)      = NASM_Line $ NASM_Instruction Nothing (opcode_to_NASM op) [NASM_Operand_Address $ NASM_JumpTarget $ j] "" []  (Just i)
  use_jump_target j@(ExternalDeref sym) = NASM_Line $ NASM_Instruction Nothing (opcode_to_NASM op) [NASM_Operand_Address $ NASM_JumpTarget $ j] "PointerToObject" [] (Just i)
  use_jump_target (ImmediateAddress a') = 
        let (op1_str,annot) = operand_to_NASM l entry cfg i True (Op_Jmp $ Immediate (BitSize 64) a') in
          NASM_Line $ NASM_Instruction Nothing (opcode_to_NASM op) [op1_str] "" annot (Just i)
  use_jump_target Unresolved            =
    let (op1_str,annot) = operand_to_NASM l entry cfg i True op1 in
      NASM_Line $ NASM_Instruction Nothing (opcode_to_NASM op) [op1_str] "" annot (Just i)


  add_label_if_terminating label
    | is_terminal_call l i = [NASM_Line $ mk_nasm_instr NOP [], NASM_Label $ terminating_label l entry i blockID, label]
    | otherwise            = [label]


is_terminal_call l i
  | isCall (inOperation i) || isJump (inOperation i) = 
    case resolve_call l i of
      Terminal    -> True
      _           -> False
  | otherwise = False

-- Make an instruction with an operand reading a GOT entry
-- TODO MOV reg, [rip+imm] can be more efficient
mk_GOT_entry_instr l entry cfg i@(Instruction addr pre op Nothing ops annot) = 
  let [f] = name_of_external_function
      r   = find_unused_register register_set [i] in
    [ mov_reg_to_temp r
    , lea_external_function r f
    , the_actual_instr r
    , mov_temp_to_reg r ]
 where
  mov_reg_to_temp r = mk_nasm_instr MOV [ label_to_mem_operand (8,True) $ Label 0 "Ltemp_storage_foxdec", fst $ operand_to_NASM l entry cfg i True $ Op_Reg r ] `withComment` "inserted"

  lea_external_function r f = mk_nasm_instr LEA [ fst $ operand_to_NASM l entry cfg i True $ Op_Reg r,  NASM_Operand_EffectiveAddress $ NASM_Addr_Symbol f ]

  the_actual_instr r = mk_normal_instr l entry cfg (Instruction addr pre op Nothing (replace_mem_op r ops) annot)

  mov_temp_to_reg r = mk_nasm_instr MOV [ fst $ operand_to_NASM l entry cfg i True $ Op_Reg r, label_to_mem_operand (8,True) $ Label 0 "Ltemp_storage_foxdec"] `withComment` "inserted"

  replace_mem_op r (Op_Mem _ _ _ _ _ _ _: ops) = Op_Reg r : ops
  replace_mem_op r (op:ops)       = op:replace_mem_op r ops

  name_of_external_function = mapMaybe (try_operand_reads_GOT_entry l i) ops
{--
-- TODO: this is only relevant when doing non-RIP-relative binaries and translating instructions of the form:
--    MOV dst, imm
-- where imm is (maybe) an address
--mk_instr ctxt entry cfg i@(Instruction addr pre op@MOV Nothing ops@[dst,Immediate imm] annot)
--  | is_instruction_address l imm = mk_fake_lea ctxt entry cfg i
--  | otherwise = [NASM_Instruction $ concat
--      [ pre
--      , opcode_to_NASM op
--      , " "
--      ,  intercalate ", " $ map (operand_to_NASM l entry cfg i False) ops ]]

--}




some_operand_reads_GOT_entry l i = any (\op -> try_operand_reads_GOT_entry l i op /= Nothing)

try_operand_reads_GOT_entry l@(bin,_,l0) i addr =
  case rip_relative_to_immediate i addr of
    Nothing -> Nothing
    Just a  -> find_relocated_function a
 where
  find_relocated_function a = 
    case find (is_relocated_function $ fromIntegral a) $ IM.assocs $ binary_get_symbol_table bin of
      Just (a',sym@(PointerToLabel _ _))  -> Just sym
      --Just (a',sym@(PointerToObject _ _)) -> Just sym
      _ -> Nothing

  is_relocated_function a (a',PointerToLabel  str _) = a == a'
  --is_relocated_function a (a',PointerToObject str _) = a == a'
  is_relocated_function a _                          = False


is_instruction_address l@(bin,_,l0) a = and
  [ address_has_instruction bin a
  , a `elem` all_instruction_addresses ]
 where
  all_instruction_addresses = S.unions $ map (S.fromList . map inAddress . concat . IM.elems . cfg_instrs) $ IM.elems $ l0_get_cfgs l0



-- TODO: this is only relevant when doing non-RIP-relative binaries and translating instructions of the form:
--   MOV dst, imm
-- where imm is (maybe) an address
{--
mk_fake_lea ctxt entry cfg i@(Instruction addr pre op@MOV Nothing ops@[dst@(Storage r),Immediate imm] annot) =
    let left = fromJust $ symbolize_immediate l (Just (entry,cfg)) True imm in
      [NASM_Instruction $ concat
        [ pre
        , "LEA "
        , operand_to_NASM l entry cfg i True dst
        , ", [" ++ left ++ "]"
        , " ; 0x" ++ showHex imm ++ " --> " ++ left ]]
mk_fake_lea ctxt entry cfg i@(Instruction addr pre op@MOV Nothing ops@[dst@(Memory a si),Immediate imm] annot) =
    let left = fromJust $ symbolize_immediate l (Just (entry,cfg)) True imm
        r = find_unused_register register_set [i] in
      [ NASM_Comment 2 "inserted"
      , NASM_Instruction $ concat
        [ "MOV qword [Ltemp_storage_foxdec], " 
        , operand_to_NASM l entry cfg i True $ Storage r ]
      , NASM_Instruction $ concat
        [ pre
        , "LEA "
        , operand_to_NASM l entry cfg i True $ Storage r
        , ", [" ++ l ++ "]"
        , " ; 0x" ++ showHex imm ++ " --> " ++ left ] 
      , NASM_Instruction $ concat
        [ "MOV " 
        , operand_to_NASM l entry cfg i True dst
        , ", "
        , operand_to_NASM l entry cfg i True $ Storage r ]
      , NASM_Instruction $ concat
        [ "MOV qword " 
        , operand_to_NASM l entry cfg i True $ Storage r
        , ", [Ltemp_storage_foxdec]"]
      , NASM_Comment 2 "done" ]
--}


-- | convert an operand to a NASM operand
-- Return both the translation (a String) as well as an optional annotation
operand_to_NASM l entry cfg i is_addr (Op_Reg r)               = (NASM_Operand_Reg r,[])
operand_to_NASM l entry cfg i is_addr a@(Op_Mem _ _ _ _ _ _ _) = mem_operand_to_NASM l entry cfg i a
operand_to_NASM l entry cfg i is_addr a@(Op_Const imm)         = (NASM_Operand_Immediate $ Immediate (BitSize 64) $ fromIntegral imm, [])
operand_to_NASM l entry cfg i is_addr a@(Op_Near op)           = operand_to_NASM l entry cfg i is_addr op
operand_to_NASM l entry cfg i is_addr a@(Op_Imm imm)           = operand_to_NASM l entry cfg i is_addr (Op_Jmp imm)
operand_to_NASM l entry cfg i is_addr a@(Op_Jmp imm) 
  | is_addr   = 
    let (address,annot) = symbolize_address l entry cfg i a in
      (NASM_Operand_Address address, annot)
  | otherwise = (NASM_Operand_Immediate imm, [])
operand_to_NASM l entry cfg i is_addr op = error $ show op

mem_operand_to_NASM l entry cfg i a@(Op_Mem (BitSize si) _ _ _ _ _ _) =
  let (address,annot) = symbolize_address l entry cfg i a in
    case inOperation i of
      LEA -> (NASM_Operand_EffectiveAddress address, annot)
      _   -> (NASM_Operand_Memory (size_directive_to_NASM i (si `div` 8)) address, annot)



-- | convert the address of an operand to a NASM address
-- -- TODO remove this
address_to_NASM (Op_Mem _ _ reg idx scale displ seg) = NASM_Address_Computation (RegSeg <$> seg) (mk idx) (fromIntegral scale) (mk reg) (mk' displ)
 where
  mk RegNone = Nothing 
  mk r = Just r

  mk' 0 = Nothing
  mk' i = Just $ fromIntegral i






-- | convert size directive of an operand to a NASM size directive
size_directive_to_NASM :: Instruction -> Int -> (Int,Bool) 
size_directive_to_NASM _ 1  = (1,True) -- "byte"
size_directive_to_NASM _ 2  = (2,True) -- "word"
size_directive_to_NASM _ 4  = (4,True) -- "dword"
size_directive_to_NASM i 8  
  | inOperation i `elem` [ADDSD,SUBSD,DIVSD,MULSD,COMISD,UCOMISD,MINSD,MAXSD] = (8,False) -- NASM does not want size directives for these instructions
  | otherwise = (8,True) --  "qword"
size_directive_to_NASM _ 10 = (10,True) -- "tword"
size_directive_to_NASM i 16
  | inOperation i == COMISD = (16,False)
  | inOperation i == COMISS = (16,False)
  | otherwise          = (16,True) --  "oword" -- BUG in Capstone
size_directive_to_NASM i x  = error $ "Unknown size directive " ++ show x ++ " in instruction: " ++ show i

-- | convert opcode to a NASM opcode
opcode_to_NASM MOVABS = Just MOV
opcode_to_NASM opcode = Just opcode


mk_NASM_prefix ps
  | PrefixRep `elem` ps = Just PrefixRep
  | PrefixRepNE `elem` ps = Just PrefixRepNE
  | PrefixLock `elem` ps = Just PrefixLock
  | otherwise = Nothing





----- SYMBOLIZATION -----


----- TEXT SECTIONS -----
-- | Symbolization of an address of an operand
-- Return an optional annotation as well.
symbolize_address l@(bin,_,l0) entry cfg i a =
  case symbolize_immediate l (Just (fromIntegral entry,cfg)) (isCall $ inOperation i) <$> fromIntegral <$> rip_relative_to_immediate i a of
    Just (Just sym) -> sym 
    _               -> (NASM_Addr_Compute $ address_to_NASM a, [])
      --case try_symbolize_base_of_address ctxt i a of
      --  Just str -> error "TODO" 
      --  _        -> (NASM_Addr_Compute $ address_to_NASM i a, [])

{--
try_symbolize_base_of_address ctxt i addr = find_base addr
 where
  find_base (Op_Mem _ _ )                  = try_symbolize_base l True imm
  find_base _                                 = Nothing
--}


-- | Symbolization of an immediate value that is used as an address
symbolize_immediate :: BinaryClass bin => LiftedC bin -> Maybe (Int,CFG) -> Bool -> Word64 -> Maybe (NASM_Address,Annot)
symbolize_immediate l@(bin,_,l0) entry_cfg is_call a =
  (add_annot <$> first) `orTry` (add_annot <$> second) `orTry` relocatable_symbol l a `orTry` try_symbolize_base l True a
 where
  (first,second) 
    | is_call   = (find_outside_cfg,find_inside_cfg entry_cfg)
    | otherwise = (find_inside_cfg entry_cfg,find_outside_cfg)

  -- search for a block in the current cfg that starts at @a@, and if found, make a label for it
  find_inside_cfg Nothing            = Nothing 
  find_inside_cfg (Just (entry,cfg)) = ((block_label l entry a . fst) <$> find block_starts_at (IM.toList $ cfg_instrs cfg))
  -- search for a block outside of the current cfg
  find_outside_cfg  = ((\a -> block_label l a a 0) <$> find ((==) a) (map fromIntegral $ S.toList $ l0_get_function_entries l0))

  block_starts_at (blockId, instrs) = instrs /= [] && inAddress (head instrs) == fromIntegral a

  add_annot str = (NASM_Addr_Label str,[(a,str)])


-- | Symbolize (try to) an immediate address falling into the range of a section
try_symbolize_base :: BinaryClass bin => LiftedC bin -> Bool -> Word64 -> Maybe (NASM_Address,Annot)
try_symbolize_base l@(bin,_,l0) not_part_of_larger_expression imm = within_section `orTry` (try_internal imm) `orTry` (add_annot <$> try_at_end_of_section imm)
 where
  within_section    = show_section_relative imm <$> find_section_for_address bin imm

  show_section_relative a sec@(segment,section,a0,_,_) = (NASM_Addr_Label label,[(a, label)])
   where
    label = section_label l segment section a0 a

  try_internal a = (\sym -> (NASM_Addr_Symbol sym,[])) <$> (IM.lookup (fromIntegral a) $ IM.filter is_internal_symbol $ binary_get_symbol_table bin)

  try_at_end_of_section a = end_of_section_label l <$> (find (is_end_of_section a) $ si_sections $ binary_get_sections_info bin)

  is_end_of_section a (_,_,a0,sz,_) = a0 + sz == a

  add_annot str = (NASM_Addr_Label str,[(imm,str)])




-- see if address matches an external symbol loaded at linking time
relocatable_symbol l@(bin,_,l0) a = (IM.lookup (fromIntegral a) (binary_get_symbol_table bin) >>= mk_symbol) `orTry` (find (reloc_for a) (binary_get_relocations bin) >>= mk_reloc)
 where
  mk_symbol sym@(PointerToLabel  l _)             = Nothing --  error $ "Reading PLT entry of address " ++ showHex a -- TODO check if this actually happens. If so, return Nothing.
  mk_symbol sym@(PointerToObject o _)             = Just (NASM_Addr_Symbol sym,[(a,mk_safe_label o a)])
  mk_symbol sym@(AddressOfLabel  l _)             = Just (NASM_Addr_Symbol sym,[(a,mk_safe_label l a)])
  mk_symbol sym@(AddressOfObject o _)             = Just (NASM_Addr_Symbol sym,[(a,mk_safe_label o a)])
  mk_symbol sym@(Relocated_ResolvedObject str a1) = Just (NASM_Addr_Symbol sym,[(a,mk_safe_label str a)])

  mk_reloc (Relocation a0 a1) = 
    let label = block_label l 0 a0 0 in
      Just (NASM_Addr_Label label, [(a,label)])

reloc_for a (Relocation a0 a1) = a == a0


rip_relative_to_immediate i (Op_Mem _ _ (Reg64 RIP) RegNone _ displ Nothing) = Just $ fromIntegral (inAddress i) + inSize i + displ
rip_relative_to_immediate i (Op_Mem _ _ RegNone RegNone 0 displ Nothing) = Just $ displ
rip_relative_to_immediate i (Op_Mem _ _ (Reg64 RIP) _ _ _ _) = error (show i)
rip_relative_to_immediate i (Op_Reg (Reg64 RIP)) = error (show i)
rip_relative_to_immediate i (Op_Imm (Immediate _ imm)) = Just $ fromIntegral imm
rip_relative_to_immediate i (Op_Jmp (Immediate _ imm)) = Just $ fromIntegral imm
rip_relative_to_immediate i (Op_Const c) = Just $ c
rip_relative_to_immediate i (Op_Near op) = rip_relative_to_immediate i op
rip_relative_to_immediate i (Op_Far op) = rip_relative_to_immediate i op

rip_relative_to_immediate i (Op_Reg _) = Nothing
rip_relative_to_immediate i (Op_Mem _ _ _ _ _ _ _) = Nothing







----- DATA SECTIONS -----

is_internal_symbol (AddressOfLabel _ False) = True
is_internal_symbol _                        = False





ro_data_section ctxt = generic_data_section ctxt is_ro_data_section binary_read_ro_data
data_section ctxt    = generic_data_section ctxt is_data_section    binary_read_data


nub_data_section_entries = rmdups -- sortBy compareFst . S.toList . S.fromList TODO expensive
 where
  compareFst (a,e0) (b,e1) = 
    case compare a b of
      EQ  -> compareEntry e0 e1 
      cmp -> cmp

  compareEntry (DataEntry_Label l0) (DataEntry_Label l1) = compare l0 l1
  compareEntry (DataEntry_Label l0) _ = LT
  compareEntry _ (DataEntry_Label l0) = GT
  compareEntry e0 e1 = compare e0 e1

  rmdups :: Ord a => [a] -> [a]
  rmdups = rmdups' S.empty

  rmdups' _ [] = []
  rmdups' a (b : c) = if S.member b a
    then rmdups' a c
    else b : rmdups' (S.insert b a) c


add_labels_to_data_sections :: M.Map Word64 (S.Set NASM_Label) -> S.Set String -> NASM_Section -> NASM_Section
add_labels_to_data_sections annots externals ts@(NASM_Section_Text _) = ts
add_labels_to_data_sections annots externals (NASM_Section_Data ds)   = NASM_Section_Data $ map (add_labels_to_datasection annots') ds
 where 
  annots' :: M.Map Word64 (S.Set NASM_Label)
  annots' = M.filter (not . S.null) $ M.map (S.filter (not . isExternalLabel)) annots
  isExternalLabel (Label _ str) = str `S.member` externals

  add_labels_to_datasection annots (NASM_DataSection n@(seg,sec,a0) align es) = NASM_DataSection n align $ nub_data_section_entries $ insert_annots annots $ splits_where_needed es


  splits_where_needed es = M.foldrWithKey split_where_needed es annots

  split_where_needed split ls [] = []
  split_where_needed split ls (e@(a,DataEntry_BSS sz):es)
    | a < split && split < a + fromIntegral sz = (a,DataEntry_BSS $ fromIntegral $ split-a) : (split,DataEntry_BSS $ sz - (fromIntegral $ split-a)) : es
    | otherwise = e : split_where_needed split ls es
  split_where_needed split ls (e@(a,DataEntry_String str zero):es)
    | a < split && split < a + fromIntegral (length str) + (if zero then 1 else 0) =
      let (str0,str1) = splitAt (fromIntegral $ split-a) str in
        (a,DataEntry_String str0 False)
      : (split,DataEntry_String str1 zero)
      : es
    | otherwise = e : split_where_needed split ls es
  split_where_needed split ls (e:es) = e : split_where_needed split ls es

  insert_annots annots [] = []
  insert_annots annots (e@(a,DataEntry_Label _):es) = e:insert_annots annots es
  insert_annots annots ((a,e):es) =
    let (curr,rest) = span (\(a',_) -> a==a') es in
      insert_annot_at annots a ++ (a,e):curr ++ insert_annots annots rest

  insert_annot_at annots a = 
    case M.lookup a annots of 
      Nothing -> []
      Just ls -> map (\l -> (a,DataEntry_Label l)) $ S.toList ls

generic_data_section l@(bin,_,l0) pick_section read_from = 
  map mk_data_section $ filter pick_section $ si_sections $ binary_get_sections_info bin
 where
  mk_data_section s@(segment,section,a0,sz,align) =
    let entries  = mk_data_entries 0 s
        entries' = (a0,DataEntry_Label $ section_label l segment section a0 a0) : entries in
      NASM_DataSection (segment,section,a0) (fromIntegral align) $ nub_data_section_entries entries'

  mk_data_entries offset s@(segment,section,a0,sz,align) 
    | offset > sz  = []
    | offset == sz = [(a0+sz, DataEntry_Label $ end_of_section_label l s)]
    | otherwise    = 
      case takeWhileString offset a0 sz of
        []  -> mk_data_entries_no_string offset s
        str -> let offset' = offset + fromIntegral (length str) in
                 if offset' < sz && read_byte offset' a0 == 0 then
                   (a0 + offset, DataEntry_String str True) : mk_data_entries (offset' + 1) s
                 else
                   mk_data_entries_no_string offset s

  mk_data_entries_no_string offset s@(segment,section,a0,sz,align) =
    case find_reloc offset a0 of
      Just (Relocation _ a1) ->
        [ (a0+offset, DataEntry_Label $ mk_reloc_label (a0 + offset))
        , (a0+offset, DataEntry_Pointer $ try_symbolize_imm l a1) ]
        ++
        mk_data_entries (offset+8) s
      _ -> case IM.lookup (fromIntegral $ a0 + offset) $ binary_get_symbol_table bin of
             Just (PointerToLabel sym _) -> 
               let nasm_sym = (NASM_Addr_Symbol (PointerToLabel sym False),[]) in
                 (a0+offset, DataEntry_Pointer nasm_sym) : mk_data_entries (offset+8) s
             _ -> (a0+offset, DataEntry_Byte $ read_byte offset a0) : mk_data_entries (offset+1) s


  mk_reloc_label a0 = block_label l 0 a0 0

  takeWhileString offset a0 sz
    | find_reloc offset a0 /= Nothing        = []
    | offset >= sz                           = []
    | valid_char (w2c $ read_byte offset a0) = (read_byte offset a0) : takeWhileString (offset+1) a0 sz
    | otherwise                              = []

  valid_char c = c `elem` "!@#$%^&*()_-+={}[]:;|/?<>,. ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890'\"" ++ escape_chars
  escape_chars = "`\\\n\t"




  find_reloc offset a0 = find (reloc_for $ fromIntegral (a0 + offset)) $ binary_get_relocations bin
  read_byte  offset a0 = 
    case read_from bin (fromIntegral $ a0 + offset) 1 of
      Just [v] -> v

try_symbolize_imm :: BinaryClass bin => LiftedC bin -> Word64 -> (NASM_Address,Annot)
try_symbolize_imm l@(bin,_,l0) a1 = 
  case symbolize_immediate l Nothing False a1 of
    Just (str,annot) -> if "RELA_.text" `isPrefixOf` (show str) then traceShow ("ERROR: UNTRANSLATED ENTRY ADDRESS " ++ showHex a1) (str,annot) else (str,annot) -- show str ++ "    ; " ++ render_annot annot
    Nothing          -> error $ "ERROR: could not symbolize relocated immediate value 0x" ++ showHex a1

bss_data_section l@(bin,_,l0) = 
  map mk_bss_data_section $ filter is_bss_data_section $ si_sections $ binary_get_sections_info bin
 where
  mk_bss_data_section (segment,section,a0,sz,align) = 
    let entries  = mk_bss segment section a0 sz
        entries' = (a0,DataEntry_Label $ section_label l segment section a0 a0) : entries in
      NASM_DataSection (segment,section,a0) (fromIntegral align) $ nub_data_section_entries entries'


  mk_bss segment section a0 sz =
    case sort $ map get_addr $ filter (was_relocated_and_in a0 sz) $ S.toList $ binary_get_relocations bin of
      [] -> [ (a0,      DataEntry_BSS $ fromIntegral sz)
            , (a0 + sz, DataEntry_Label $ end_of_section_label l (segment,section,0,0,0)) ]
      (a:_) -> [ (a0, DataEntry_BSS $ fromIntegral $ a - a0)
               , (a,  DataEntry_Label $ mk_reloc_label a) ]
               ++
               mk_bss segment section (fromIntegral a) (sz + a0 - fromIntegral a)

  mk_reloc_label a0 = block_label l 0 a0 0


  get_addr (Relocation a0 _) = a0
  was_relocated_and_in a0 sz (Relocation a a1) = fromIntegral a0 < a && a < fromIntegral (a0 + sz)-- TODO note strict inequality here



-- Get an overview of all indirections (entry,cfg,i) where entry is the entry-point of the function, cfg is the CFG of the function and i is an in instruction that performs an indirection
get_indirections_per_function l@(bin,_,l0) = concatMap get $ S.toList $ l0_get_function_entries l0
 where
  get entry =
    let Just cfg = IM.lookup entry (l0_get_cfgs l0) in
      map (\i -> (entry,cfg,i)) $ filter (indirection_in_cfg cfg) $ IM.assocs $ l0_indirections l0
  indirection_in_cfg cfg (a,_) = any (any (\i -> inAddress i == fromIntegral a)) $ cfg_instrs cfg

get_terminals_per_function l@(bin,_,l0) = concatMap get $ S.toList $ l0_get_function_entries l0
 where
  get entry =
    let Just cfg = IM.lookup entry (l0_get_cfgs l0) in
      filter (is_terminal_call l) $ concat $ IM.elems $ cfg_instrs cfg


mk_jump_table l (entry,cfg,(a,inds)) = concatMap mk $ S.toList inds
 where
  mk (Indirection_JumpTable (JumpTable index bnd trgt tbl)) = intercalate "\n" $ 
    [ "; JUMP TABLE: entry == " ++ showHex entry ++ ", instr@" ++ showHex a
    , "section .bss"
    , show (label_jump_table_temp_storage entry a 0) ++ ":"
    , "resb 8"
    , show (label_jump_table_temp_storage entry a 1) ++ ":"
    , "resb 8"
    , "section .data.rel.ro"
    , show (label_jump_table_redirect_data entry a) ++ ":"]
    ++
    map mk_entry (sortBy (compare `on` fst) $ IM.assocs tbl)
  mk _ = []

  mk_entry (idx,trgt) = 
    case symbolize_immediate l (Just (entry,cfg)) False trgt of
      Just (str,annot) -> "dq " ++ show str ++ " ; index " ++ show idx ++ "    ; " ++ render_annot annot
      Nothing          -> "ERROR: cannot symbolize jump target:" ++ showHex trgt





regs_of_ops = concatMap regs_of_op

regs_of_op (Op_Mem _ _ reg idx _ _ Nothing) = [reg,idx]
regs_of_op (Op_Mem _ _ reg idx _ _ (Just seg)) = [reg,idx,RegSeg seg]
regs_of_op (Op_Reg r) = [r]
regs_of_op (Op_Near op) = regs_of_op op
regs_of_op (Op_Far op) = regs_of_op op
regs_of_op _ = []


register_set = map Reg64 [RAX,RBX,RCX,RDX,R8,R9,R10,R11,R12,R13,R14,R15]


reg_of_size (Reg64 r) 8 = Reg64 r
reg_of_size (Reg64 r) 4 = Reg32 r
reg_of_size (Reg64 r) 2 = Reg16 r
reg_of_size (Reg64 r) 1 = Reg8 r HalfL


reg_of_size reg si = error $ "Make register " ++ show reg ++ " of size " ++ show si 

find_element_not_in (a:as) x = if a `elem` x then find_element_not_in as x else a


find_unused_register :: [Register] -> [Instruction] -> Register
find_unused_register regs instrs = 
  let used_regs = concatMap (map real_reg . regs_of_ops . get_ops) instrs in
    find_element_not_in regs used_regs
 where
  get_ops i@(Instruction addr pre op Nothing ops annot) = ops




-- | There is one specific symbol frequently encountered for which we cannot find the appropiate library to load.
-- It is related to debugging information (the -g option of GCC).
-- We therefore pvodie our own implementation: just a dummy, which is what the real function seems to do as well.
__gmon_start_implementation = "void __gmon_start__ () { return; }"


