{-# LANGUAGE PartialTypeSignatures , FlexibleContexts #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : L0ToNASM
Description : Lift the L0 representation of the binary to symbolized and recompilable NASM.


-}



module NASM.L0ToNASM (lift_L0_to_NASM, render_NASM, __gmon_start_implementation, NASM) where


import Base

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

import Data.JumpTarget

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
import Data.Bits (testBit)
import Data.List.Split (splitOn)
import Data.ByteString.Internal (w2c)
import Data.Function (on)

import Control.Monad.State.Strict
import Data.Functor.Identity
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Unsafe

import Text.RawString.QQ

import Debug.Trace

import GHC.Base hiding (Symbol)


-- | Each NASM line is either an instruction, a label, or a comment
data NASM_Line = 
    NASM_Instruction String -- ^ An instruction
  | NASM_Label String       -- ^ A label
  | NASM_Comment Int String -- ^ A comment with an indentation level (number of spaces)

-- | A NASM text section contains a header (usually just some comments), and a graph of basic blocks
data NASM_TextSection = NASM_TextSection  {
  nasm_section_header :: String,
  nasm_blocks         :: IM.IntMap [NASM_Line], -- ^ A mapping of blockIDs to instructions
  nasm_edges          :: IM.IntMap (IS.IntSet)  -- ^ A mapping of blockIDs to sets of blocKIDs
 }

-- | A NASM section is either a NASM text section or NASM data section
-- A NASM data section is simply a String
data NASM_Section = NASM_Section_Text NASM_TextSection | NASM_Section_Data String

-- | NASM contains external symbols, sections, and a footer
data NASM = NASM {
  nasm_externals :: S.Set String,
  nasm_sections  :: [NASM_Section],
  nasm_footer    :: [String]
 }


-- | Lift an L0 representation to position-independent NASM
lift_L0_to_NASM ctxt = NASM mk_externals mk_sections mk_jump_tables
 where
  mk_externals       = externals ctxt
  mk_sections        = mk_text_sections ++ [mk_ro_data_section, mk_data_section, mk_bss_section]
  mk_text_sections   = map (entry_to_NASM ctxt) $ S.toList $ ctxt_get_function_entries ctxt

  mk_ro_data_section = NASM_Section_Data $ ro_data_section ctxt
  mk_data_section    = NASM_Section_Data $ data_section ctxt
  mk_bss_section     = NASM_Section_Data $ bss_data_section ctxt
  
  mk_jump_tables     = map (mk_jump_table ctxt) $ get_indirections_per_function ctxt






-- | Rendering NASM to a String
render_NASM :: Context -> NASM -> String
render_NASM ctxt (NASM exts sections footer) = intercalate "\n\n\n" $ [render_externals] ++ ["global _start", "default rel", mk_macros ctxt] ++ render_sections ++ footer
 where
  render_externals = intercalate "\n" $ map ((++) "extern ") $ S.toList exts
  render_sections  = map render_section sections

  render_section (NASM_Section_Text (NASM_TextSection hdr blocks edges)) = hdr ++ intercalate "\n\n" (map render_block $ IM.assocs blocks) ++ "\n\n"
  render_section (NASM_Section_Data str) = str ++ "\n\n"

  render_block (blockID,lines) = intercalate "\n" $ map render_line lines

  render_line (NASM_Instruction str) = "  " ++ str
  render_line (NASM_Label str) = str ++ ":"
  render_line (NASM_Comment indent str) = replicate indent ' ' ++ comment str
  

-- making comments
comment str = "; " ++ str

comment_block strs =
   intercalate "\n" $ comment_block_delim : (map comment strs ++ [comment_block_delim,""])
 where
  comment_block_delim = comment $ replicate (length max) '-' 
  max = maximumBy compare_lengths strs
  compare_lengths str0 str1 = compare (length str0) (length str1)






-- | get the external function symbols 
externals ctxt = S.fromList $ map (strip_GLIBC . fromJust . symbol_to_name) $ filter is_relocation $ IM.elems $ ctxt_symbol_table ctxt
 where
  is_relocation (Relocated_Function str) = str /= ""
  is_relocation (Relocated_Label str) = str /= ""
  is_relocation _ = False





-- | Creating labels
-- Given the entry address of the current function, the blockID of the current basic block,
-- map an address to a label.
-- First, try to see if it matches the _start symbol.
-- Then, try to map the address to a known internal synbol (unstripped binaries may have such symbols available)
-- Then, try to see if at the address a relocation is stored, and use that lavel if so.
-- Otherwise, make a new custom label.
block_label ctxt entry a blockID = (try_start_symbol `orTry` try_internal  `orTry` try_relocation_label) `orElse` custom_label
 where
  -- For the entry point of the binary, introduce the _start label
  try_start_symbol
    | a == fromIntegral (ctxt_start ctxt) = Just "_start"
    | otherwise = Nothing
  -- Try if the address matches a known internal symbol
  try_internal = do
    sym  <- (IM.lookup (fromIntegral a) $ IM.filter is_internal_symbol $ ctxt_symbol_table ctxt)
    name <- symbol_to_name sym
    return $ name -- "L" ++ showHex entry ++ "_" ++ name
  -- Try to see if the address stores a relocation
  try_relocation_label = reloc_label <$> find (reloc_for a) (ctxt_relocs ctxt)

  -- Make a new label based on the entry and blockID
  custom_label = "L" ++ showHex entry ++ "_" ++ show blockID
  -- Make a label for relocation
  reloc_label (Relocation a0 a1) = "L_reloc_0x" ++ showHex a0 ++ "_0x" ++ showHex a1


-- Make a label for the start of a section
section_label segment section addr =  "L" ++ segment ++ "_" ++ section ++ "_0x" ++ showHex addr 
-- Make a label for the end of a section
end_of_section_label (segment,section,a0,sz) = "L" ++ segment ++ "_" ++ section ++ "_END"  
-- Make a macro name
macro_name segment section a0 =  "RELA" ++ segment ++ "_" ++ section ++ "_0x" ++ showHex a0





-- | Information on sections
-- TODO: get from Binary interface
is_ro_data_section ("",".rodata",_,_) = True
is_ro_data_section ("",".init_array",_,_) = True
is_ro_data_section ("",".fini_array",_,_) = True
is_ro_data_section ("",".data.rel.ro",_,_) = True
is_ro_data_section ("__DATA","__const",_,_) = True
is_ro_data_section _ = False

is_data_section ("__DATA","__data",_,_) = True
is_data_section ("",".data",_,_) = True
is_data_section _ = False

is_bss_data_section ("__DATA","__bss",_,_) = True
is_bss_data_section ("__DATA","__common",_,_) = True
is_bss_data_section ("",".bss",_,_) = True
is_bss_data_section _ = False






-- | convert a given function entry to a NASM text section
entry_to_NASM ctxt entry = NASM_Section_Text $ NASM_TextSection mk_header mk_blocks mk_edges
 where
  mk_header   = comment_block ["Function: " ++ function_name_of_entry ctxt entry]
  mk_blocks   = IM.fromList cfg_to_NASM
  mk_edges    = cfg_edges cfg
  Just cfg    = IM.lookup entry (ctxt_cfgs ctxt)
  cfg_to_NASM = cfg_blocks_to_NASM ctxt entry cfg $ IM.keys $ cfg_blocks cfg

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
  mk_block              = block_label' ++ block_body 

  -- the label
  block_label'          = [NASM_Label $ block_label ctxt entry (addressof $ head block_instrs) blockID] 
  -- the body:
  --   in case of a block ending in a jump table, instructions are inserted at beginning and end of the block
  --   in case of a block ending in an indirection resolved to a single target, translate the last instruction accordingly
  --   in case of a block ending in an unresolved indirection resolved to a single target, translate as normal but annotate
  --   in case of no indirection, translate normally but insert an extra jump to the next block if necessary
  block_body =
    case try_indirect_block block_instrs of
      Just (Indirection_JumpTable tbl@(JumpTable index _ _ _)) -> 
        let si   = operand_size index
            reg  = reg_of_size (find_unused_register register_set block_instrs) si
            reg' = reg_of_size (find_unused_register [ r | r <- register_set, real r /= real reg] block_instrs) 8 in
          jmp_table_init tbl reg reg' (last block_instrs) ++ mk_block_instrs (init block_instrs) ++ jmp_table_end tbl reg reg' (last block_instrs)
      Just (Indirection_Resolved trgts) -> mk_block_instrs (init block_instrs) ++ resolved_jump (S.toList trgts) (last block_instrs)
      Just (Indirection_Unresolved)     -> mk_block_instrs block_instrs ++ unresolved_end ++ block_end block_instrs
      Nothing                           -> mk_block_instrs block_instrs ++ block_end block_instrs


  -- if the block ends in an indirection, retrieve that indirection
  try_indirect_block instrs
    | instrs == []  = Nothing
    | is_unresolved = Just Indirection_Unresolved
    | otherwise     = IM.lookup (fromIntegral $ addressof $ last instrs) $ ctxt_inds ctxt
  is_unresolved = node_info_of ctxt cfg blockID == UnresolvedIndirection
  


  -- make a block of regular non-control-flow instructions
  mk_block_instrs instrs = concatMap (instr_to_NASM ctxt entry cfg) $ filter_unnecessary_jumps instrs

  -- for a normal (non-indirection) block, see if we need to insert an additional jump to the next block
  -- Necessary when that next block is not the next to be translated.
  block_end instrs
    | instrs == [] = []
    | is_proper_block_end_instruction (Instr.opcode $ last instrs) = []
    | blockID1 /= Nothing && IM.lookup blockID (cfg_edges cfg) == Just (IS.singleton $ fromJust blockID1) = []
    | otherwise = mk_extra_jmp


  -- A jump table is implemented (TODO more comments)
  jmp_table_init t@(JumpTable index bound trgt tbl) reg reg' i =
      [ NASM_Comment 2 $ "Resolved indirection:"
      , NASM_Comment 2 $ show t
      , NASM_Instruction $ "MOV [" ++ label_jump_table_temp_storage entry (addressof i) ++ "], " ++ show (real reg)
      , NASM_Instruction $ "MOV [" ++ label_jump_table_temp_storage entry (addressof i) ++ " + 8], " ++ show (real reg')
      , NASM_Instruction $ "MOV " ++ show reg ++ ", " ++ operand_to_NASM ctxt entry cfg empty_instr False index ]
      ++
      (if sizeof reg < 4 then [NASM_Instruction $ "MOVZX " ++ show(reg_of_size (real reg) 4) ++ ", " ++ show reg] else [])
      ++
      [ NASM_Comment 2 $ "Start of block"  ]

  jmp_table_end t@(JumpTable index bound trgt tbl) reg reg' last_instr =
    [ NASM_Comment 2 $ "End of block"
    , NASM_Instruction $ "LEA " ++ show reg' ++ ", [" ++ label_jump_table_redirect_data entry (addressof last_instr) ++ "]"
    , NASM_Instruction $ "LEA " ++ show reg' ++ ", [" ++ show reg' ++ " + 8*" ++ show(reg_of_size (real reg) 8) ++ "]"
    , NASM_Instruction $ "MOV " ++ operand_to_NASM ctxt entry cfg empty_instr False trgt ++ ", qword [" ++ show reg' ++ "]"
    , NASM_Instruction $ "MOV " ++ show (real reg)  ++ ", [" ++ label_jump_table_temp_storage entry (addressof last_instr) ++ "]"
    , NASM_Instruction $ "MOV " ++ show (real reg') ++ ", [" ++ label_jump_table_temp_storage entry (addressof last_instr) ++ " + 8]"
    , NASM_Comment 2 $ "Executing resolved indirect jump"
    , mk_jmp_call_instr ctxt entry cfg last_instr ]



  -- A resolved jump to a single known target
  resolved_jump [External f] (Instruction addr pre op Nothing ops annot) =
    [ NASM_Comment 2 $ "Resolved indirection: " ++ show (head ops) ++ " --> " ++ f 
    , NASM_Instruction $ concat [ prefix_to_NASM pre, opcode_to_NASM op, " ", f, " wrt ..plt" ]]
  resolved_jump [ImmediateAddress imm] i = [mk_jmp_call_instr ctxt entry cfg i]

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
    case unsafePerformIO $ stepA ctxt entry (fromIntegral $ addressof $ last block_instrs) of -- TODO maybe unnecessary, also TODO assumes fall through is last/second
      Right []        -> []
      Right [(a,_)]   -> [NASM_Instruction $ "JMP " ++ (fromJust $ symbolize_immediate ctxt (Just (entry,cfg)) False $ fromIntegral a) ++ "     ; inserted"]
      Right [_,(a,_)] -> [NASM_Instruction $ "JMP " ++ (fromJust $ symbolize_immediate ctxt (Just (entry,cfg)) False $ fromIntegral a) ++ "     ; inserted"]
      x               -> [NASM_Instruction $ "JMP ERROR CANNOT DETERMINE NEXT BLOCK" ++ show x]


label_jump_table_temp_storage  entry a = "L_jmp_tbl_temp_storage_" ++ showHex entry ++ "_" ++ showHex a
label_jump_table_redirect_data entry a = "L_jmp_tbl_" ++ showHex entry ++ "_" ++ showHex a



-- | convert an instruction to a NASM instruction
instr_to_NASM ctxt entry cfg i@(Instruction addr pre op Nothing ops annot)
 | op == NOP     = []
 | op == ENDBR64 = []
 | no_ops pre op = [mk_instr ctxt entry cfg $ Instruction addr pre op Nothing [] annot]
 | is_cf op      = [mk_jmp_call_instr ctxt entry cfg i]
 | otherwise     = [mk_instr ctxt entry cfg i]
 where
  is_cf op      = isCall op || isJump op || isCondJump op
  no_ops pre op = op `elem` [STOSQ, SCASB, CMPSB, STOSD] || (pre /= Nothing && op `elem` [MOVSB, MOVSW, MOVSD, MOVSQ])

mk_instr ctxt entry cfg i@(Instruction addr pre op Nothing ops annot) = NASM_Instruction $ concat
  [ prefix_to_NASM pre
  , opcode_to_NASM op
  , " "
  ,  intercalate ", " $ map (operand_to_NASM ctxt entry cfg i False) ops ]


mk_jmp_call_instr ctxt entry cfg i@(Instruction addr pre op Nothing [op1] annot) = (mk_external <$> try_external op1) `orElse` mk_call_to_imm 
 where
  try_external (Memory a si) = do
    a_v <- rip_relative_to_immediate i a
     -- see if address matches a symbol
    sym <- IM.lookup (fromIntegral a_v) $ IM.filter is_external_symbol $ ctxt_symbol_table ctxt
    name <- symbol_to_name sym
    return $ strip_GLIBC name
  try_external _ = Nothing

  mk_external f = NASM_Instruction $ concat
    [ prefix_to_NASM pre
    , opcode_to_NASM op
    , " "
    , f
    , " wrt ..plt" ]

  mk_call_to_imm =  NASM_Instruction $ concat
    [ prefix_to_NASM pre
    , opcode_to_NASM op
    , " "
    , operand_to_NASM ctxt entry cfg i True op1 ]



-- | convert an operand to a NASM operand
operand_to_NASM ctxt entry cfg i is_addr (Storage r)          = show r
operand_to_NASM ctxt entry cfg i is_addr (EffectiveAddress a) = "[" ++ symbolize_address ctxt entry cfg i a ++ "]"
operand_to_NASM ctxt entry cfg i is_addr (Memory a si)        = size_directive_to_NASM i si ++ " [" ++ symbolize_address ctxt entry cfg i a ++ "]"
operand_to_NASM ctxt entry cfg i is_addr (Immediate imm)
  | is_addr      = symbolize_address ctxt entry cfg i (AddressImm imm)
  | otherwise    = "0x" ++ showHex imm


-- | convert the address of an operand to a NASM address
address_to_NASM i a@(AddressPlus (AddressStorage r) a') = if is_segment_register r then show r ++ ":" ++ address_to_NASM' i a' else address_to_NASM' i a 
address_to_NASM i a                                     = address_to_NASM' i a

address_to_NASM' i a = 
  case rip_relative_to_immediate i a of
    Nothing  -> address_to_NASM'' a
    Just imm -> "0x" ++ showHex imm
 where
  address_to_NASM'' (AddressStorage r) = show r
  address_to_NASM'' (AddressImm i) = show i
  address_to_NASM'' (AddressPlus (AddressStorage r) (AddressImm i))  = show r ++ show_nasm_address_plus_imm i
  address_to_NASM'' (AddressMinus (AddressStorage r) (AddressImm i)) = show r ++ show_nasm_address_plus_imm (0 -i)
  address_to_NASM'' (AddressPlus (AddressStorage r) (AddressStorage r1)) = show r ++ " + " ++ show r1
  address_to_NASM'' (AddressPlus (AddressPlus (AddressStorage r) (AddressStorage r1)) (AddressImm i)) = show r ++ " + " ++ show r1 ++ show_nasm_address_plus_imm i
  address_to_NASM'' (AddressPlus (AddressStorage r) (AddressPlus (AddressStorage r1) (AddressImm i))) = show r ++ " + " ++ show r1 ++ show_nasm_address_plus_imm i
  address_to_NASM'' (AddressPlus (AddressStorage r) (AddressMinus (AddressStorage r1) (AddressImm i))) = show r ++ " + " ++ show r1 ++ show_nasm_address_plus_imm (0-i)
  address_to_NASM'' (AddressPlus (AddressTimes (AddressStorage r) (AddressImm i)) (AddressImm i1)) = show r ++ " * " ++ show i ++ show_nasm_address_plus_imm i1
  address_to_NASM'' (AddressPlus (AddressStorage r) (AddressTimes (AddressStorage r1) (AddressImm i))) = show r ++ " + " ++ show r1 ++ " * " ++ show i
  address_to_NASM'' (AddressPlus (AddressStorage r) (AddressPlus (AddressTimes (AddressStorage r1) (AddressImm i)) (AddressImm i1))) = show r ++ " + " ++ show r1 ++ " * " ++ show i ++ show_nasm_address_plus_imm i1
  address_to_NASM'' (AddressPlus (AddressStorage r) (AddressMinus (AddressTimes (AddressStorage r1) (AddressImm i)) (AddressImm i1))) = show r ++ " + " ++ show r1 ++ " * " ++ show i ++ show_nasm_address_plus_imm (0-i1)
  address_to_NASM'' (AddressTimes (AddressStorage r) (AddressImm i))  = show r ++ " * " ++ show i
  address_to_NASM'' (AddressMinus (AddressTimes (AddressStorage r) (AddressImm i)) (AddressImm i1)) = show r ++ " * " ++ show i ++ show_nasm_address_plus_imm (0-i1)
  address_to_NASM'' a           = error $ "TODO: " ++ show a

  show_nasm_address_plus_imm i = if testBit (fromIntegral i::Word64) 63 then " - 0x" ++ showHex (0 - i) else " + 0x" ++ showHex i



-- | convert size directive of an operand to a NASM size directive
size_directive_to_NASM _ 1  = "byte"
size_directive_to_NASM _ 2  = "word"
size_directive_to_NASM _ 4  = "dword"
size_directive_to_NASM i 8  
  | opcode i `elem` [ADDSD,SUBSD,DIVSD,MULSD,COMISD,UCOMISD] = "" -- NASM does not want size directives for these instructions
  | otherwise = "qword"
size_directive_to_NASM _ 10 = "tword"
size_directive_to_NASM i 16
  | opcode i == COMISD = ""
  | opcode i == COMISS = ""
  | otherwise          = "oword" -- BUG in Capstone
size_directive_to_NASM i x  = error $ "Unknown size directive " ++ show x ++ " in instruction: " ++ show i

-- | convert opcode to a NASM opcode
opcode_to_NASM MOVABS = "MOV"
opcode_to_NASM opcode = show opcode

-- | convert prefix to a NASM prefix
prefix_to_NASM Nothing    = ""
prefix_to_NASM (Just pre) = show pre ++ " "







----- SYMBOLIZATION -----


----- TEXT SECTIONS -----
-- | Symbolization of an address of an operand
symbolize_address :: Context -> Int -> CFG -> Instruction -> Address -> String
symbolize_address ctxt entry cfg i a =
  case symbolize_immediate ctxt (Just (entry,cfg)) (isCall $ Instr.opcode i) <$> fromIntegral <$> rip_relative_to_immediate i a of
    Just (Just str) -> str
    _               -> 
      case try_symbolize_base_of_address ctxt i a of
        Just str -> str
        _        -> address_to_NASM i a


try_symbolize_base_of_address ctxt i addr = find_base addr
 where
  find_base (AddressImm imm)                  = try_symbolize_base ctxt True imm
  --find_base (AddressPlus (AddressImm imm) a') = mk_add a' <$> (try_symbolize_base ctxt False imm)
  --find_base (AddressPlus a' (AddressImm imm)) = mk_add a' <$> (try_symbolize_base ctxt False imm)
  find_base _                                 = Nothing

  mk_add a' str = str ++ " + " ++ address_to_NASM' i a'

-- | Symbolization of an immediate value that is used as an address
symbolize_immediate :: Context -> Maybe (Int,CFG) -> Bool -> Word64 -> Maybe String
symbolize_immediate ctxt entry_cfg is_call a =
  first `orTry` second `orTry` relocatable_symbol ctxt a `orTry` try_symbolize_base ctxt True a
 where
  (first,second) 
    | is_call   = (find_outside_cfg,find_inside_cfg entry_cfg)
    | otherwise = (find_inside_cfg entry_cfg,find_outside_cfg)

  -- search for a block in the current cfg that starts at @a@, and if found, make a label for it
  find_inside_cfg Nothing            = Nothing 
  find_inside_cfg (Just (entry,cfg)) = ((block_label ctxt entry a . fst) <$> find block_starts_at (IM.toList $ cfg_instrs cfg))
  -- seach for a block outside of the current cfg
  find_outside_cfg  = ((\a -> block_label ctxt a a 0) <$> find ((==) a) (map fromIntegral $ IM.keys $ ctxt_calls ctxt))

  block_starts_at (blockId, instrs) = instrs /= [] && addressof (head instrs) == fromIntegral a


-- | Symbolize (try to) an immediate address falling into the range of a section
try_symbolize_base ctxt not_part_of_larger_expression imm = within_section `orTry` try_internal imm `orTry` try_at_end_of_section imm
 where
  within_section    = show_section_relative imm <$> find_section_for_address ctxt imm

  show_section_relative a sec@(segment,section,a0,_)
    | not_part_of_larger_expression = macro_name segment section a0 ++ "(0x" ++ showHex (a - a0) ++ ")"
    | otherwise                     = section_label segment section a0 ++ " + 0x" ++ showHex (a - a0)

  try_internal a = (\(Internal_Label str) -> str) <$> (IM.lookup (fromIntegral a) $ IM.filter is_internal_symbol $ ctxt_symbol_table ctxt)

  try_at_end_of_section a = end_of_section_label <$> (find (is_end_of_section a) $ si_sections $ ctxt_sections ctxt)

  is_end_of_section a (_,_,a0,sz) = a0 + sz == a





-- see if address matches an external symbol loaded at linking time
relocatable_symbol ctxt a = (IM.lookup (fromIntegral a) (ctxt_symbol_table ctxt) >>= mk_symbol) `orTry` (find (reloc_for a) (ctxt_relocs ctxt) >>= mk_reloc)
 where
  mk_symbol (Relocated_Function str) = Just $ strip_GLIBC str ++ " wrt ..plt"
  mk_symbol (Relocated_Label str)    = Just $ strip_GLIBC str
  mk_symbol (Internal_Label a)       = Nothing

  mk_reloc (Relocation a0 a1) = Just $ block_label ctxt 0 a0 0

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
  internals = concatMap mk_internal $ filter is_outside_section $ IM.assocs $ IM.filter is_internal_symbol $ ctxt_symbol_table ctxt

  is_outside_section (a,_) = find_section_for_address ctxt (fromIntegral a) == Nothing

  mk_internal :: (Int,Symbol) -> [String]
  mk_internal (a,Internal_Label sym) = 
    case find_preceding_section a of
      Nothing -> []
      Just (segment,section,a0,si) -> ["%define " ++ sym ++ " " ++ macro_name segment section a0 ++ "(0x" ++ showHex (fromIntegral a - a0) ++ ")"]
    
  find_preceding_section a =
    case sortBy (distance a) $ filter (is_after a) (si_sections $ ctxt_sections ctxt) of
      []      -> Nothing
      (sec:_) -> Just sec

  distance a (_,_,a0,si) (_,_,a0',si') = compare (fromIntegral a - a0 - si) (fromIntegral a - a0' - si')
  is_after a (_,_,a0,si) = fromIntegral a >= a0 + si



  mk_macro (segment,section,a0,sz) = "%define " ++ macro_name segment section a0 ++ "(offset) (" ++ section_label segment section a0 ++ " + offset)"

-- TODO to Base
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p q a = p a || q a
infixr 2 |||





ro_data_section ctxt = generic_data_section ctxt is_ro_data_section binary_read_ro_data
data_section ctxt    = generic_data_section ctxt is_data_section    binary_read_data

generic_data_section ctxt pick_section read_from = 
  concatMap mk_data_section $ filter pick_section $ si_sections $ ctxt_sections ctxt
 where
  mk_data_section (segment,section,a0,sz) = intercalate "\n" $
   [ "section " ++ section
   , section_label segment section a0 ++ ":" ]
   ++
   mk_data_entries 0 segment section a0 sz
   ++
   ["\n\n"] 

  mk_data_entries offset segment section a0 sz 
    | offset > sz = []
    | offset == sz = [end_of_section_label (segment,section,0,0) ++ ":"]
    | otherwise = 
      case takeWhileString offset a0 sz of
        []  -> mk_data_entries_no_string offset segment section a0 sz
        str -> let offset' = offset + fromIntegral (length str) in
                 if offset' < sz && read_byte offset' a0 == 0 then
                   ["db `" ++ concat str ++ "`" ++ ", 0"] ++ mk_data_entries (offset' + 1) segment section a0 sz
                 else
                   mk_data_entries_no_string offset segment section a0 sz

  mk_data_entries_no_string offset segment section a0 sz =
    case find_reloc offset a0 of
      Just (Relocation _ a1) ->  [mk_reloc_label (a0 + offset) ++ ":"] ++ ["dq " ++ try_symbolize_imm a1] ++ mk_data_entries (offset+8) segment section a0 sz
      _ -> ["db 0" ++ showHex (read_byte offset a0) ++ "h"] ++ mk_data_entries (offset+1) segment section a0 sz


  mk_reloc_label a0 = block_label ctxt 0 a0 0

  takeWhileString offset a0 sz
    | find_reloc offset a0 /= Nothing        = []
    | offset >= sz                           = []
    | valid_char (w2c $ read_byte offset a0) = [escape $ w2c $ read_byte offset a0] ++ takeWhileString (offset+1) a0 sz
    | otherwise                              = []

  valid_char c = c `elem` "!@#$%^&*()_-+={}[]:;|/?<>,. ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890'\"" ++ escape_chars

  escape_chars = "`\\\n\t"

  escape '\\' = "\\\\"
  escape '`'  = "\\`"
  escape '\n'  = "\\n"
  escape '\t'  = "\\t"
  escape c    = [c]



  find_reloc offset a0 = find (reloc_for $ fromIntegral (a0 + offset)) $ ctxt_relocs ctxt
  read_byte  offset a0 = 
    case read_from (ctxt_binary ctxt) (fromIntegral $ a0 + offset) 1 of
      Just [v] -> v

  try_symbolize_imm a1 = 
    case symbolize_immediate ctxt Nothing False $ fromIntegral a1 of
      Just str -> str
      Nothing  -> "ERROR: could not symbolize relocated immediate value 0x" ++ showHex a1

bss_data_section ctxt = 
  concatMap mk_bss_data_section $ filter is_bss_data_section $ si_sections $ ctxt_sections ctxt
 where
  mk_bss_data_section (segment,section,a0,sz) = "section " ++ section ++ "\n" ++ section_label segment section a0 ++ ":\n" ++ mk_bss segment section a0 sz

  mk_bss segment section a0 sz =
    case sort $ map get_addr $ filter (was_relocated_and_in a0 sz) $ S.toList $ ctxt_relocs ctxt of
      [] ->  "resb " ++ show sz ++ "\n" ++ end_of_section_label (segment,section,0,0) ++ ":"
      (a:_) -> "resb " ++ show (fromIntegral a - a0) ++ "\n" ++ mk_reloc_label a ++ ":\n" ++ mk_bss segment section (fromIntegral a) (sz + a0 - fromIntegral a)

  mk_reloc_label a0 = block_label ctxt 0 a0 0



  get_addr (Relocation a0 _) = a0
  was_relocated_and_in a0 sz (Relocation a a1) = fromIntegral a0 < a && a < fromIntegral (a0 + sz)-- TODO note strict inequality here



-- Get an overview of all indirections (entry,cfg,i) where entry is the entry-point of the function, cfg is the CFG of the function and i is an in instruction that performs an indirection
get_indirections_per_function ctxt = concatMap get $ S.toList $ ctxt_get_function_entries ctxt
 where
  get entry =
    let Just cfg = IM.lookup entry (ctxt_cfgs ctxt) in
      map (\i -> (entry,cfg,i)) $ filter (indirection_in_cfg cfg) $ IM.assocs $ ctxt_inds ctxt
  indirection_in_cfg cfg (a,_) = any (any (\i -> addressof i == fromIntegral a)) $ cfg_instrs cfg



mk_jump_table ctxt (entry,cfg,(a,Indirection_JumpTable (JumpTable index bnd trgt tbl))) = intercalate "\n" $ 
  [ "; JUMP TABLE: entry == " ++ showHex entry ++ ", instr@" ++ showHex a
  , "section .bss"
  , label_jump_table_temp_storage entry a ++ ":"
  , "resb 16"
  , "section .rodata"
  , label_jump_table_redirect_data entry a ++ ":"]
  ++
  map mk_entry (sortBy (compare `on` fst) $ IM.assocs tbl)
 where
  mk_entry (idx,trgt) = 
    case symbolize_immediate ctxt (Just (entry,cfg)) False trgt of
      Just str -> "dq " ++ str ++ " ; index " ++ show idx
      Nothing  -> "ERROR: cannot symbolize jump target:" ++ showHex trgt
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

