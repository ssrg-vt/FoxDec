{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StrictData, DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : NASM
Description : A datastructure for storing NASM code.
-}



module OutputGeneration.NASM.NASM where

import Base
import Data.X86.Opcode
import Data.X86.Instruction
import Data.Symbol
import Data.Size
import Data.JumpTarget
import Binary.Generic
import Data.X86.Register


import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put,encode)

import qualified Data.Set as S
import Data.Word
import Data.List
import Data.Bits (testBit)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.ByteString.Internal (w2c)
import Data.Char(toLower)


toLowerCase :: String -> String
toLowerCase = map toLower










-- | NASM contains external symbols, sections, and a footer
data NASM = NASM {
  nasm_externals :: S.Set String,
  nasm_globals   :: S.Set String,
  nasm_sections  :: [NASM_Section],
  nasm_footer    :: [String]
 }
 deriving (Generic)

data NASM_Label = Label Word64 String -- ^ Normal label
 deriving (Eq,Ord,Generic)


-- | A NASM section is either a NASM text section or NASM data section
data NASM_Section = NASM_Section_Text NASM_TextSection | NASM_Section_Data [NASM_DataSection]
  deriving Generic

-- | An annotation consists of an address that is being symbolized to a label.
-- For example:
--   0x1016 --> L1000_2
-- Annotations translate to NASM comments, and have no effect on the actual NASM itself.
type Annot = [(Word64,NASM_Label)]

-- | A NASM text section contains a name and an **ordered** list of basic blocks.
-- Each basic block has an ID and a list of lines.
data NASM_TextSection = NASM_TextSection  {
  nasm_function_name  :: String,
  nasm_blocks         :: [(Int,[NASM_Line])], -- ^ A mapping of blockIDs to instructions
  nasm_cfg            :: IM.IntMap (IS.IntSet)
 }
  deriving Generic


-- | A NASM line is either a comment, an instruction or a label
data NASM_Line =
    NASM_Comment Int String    -- ^ A comment with an indentation level (number of spaces)
  | NASM_Line NASM_Instruction -- ^ An instruction with an annotation
  | NASM_Label NASM_Label      -- ^ A label
 deriving (Eq,Generic)


-- | An instruction consists of a prefix, an opcode, a list of operands, a comment (possibly empty) and an annotation (possibly empty).
data NASM_Instruction = NASM_Instruction
  {
    nasm_prefix   :: Maybe Prefix
  , nasm_mnemonic :: Maybe Opcode
  , nasm_operands :: [NASM_Operand]
  , nasm_comment  :: String
  , nasm_annot    :: Annot
  }
 deriving (Eq,Generic)


-- | A size directive for a memory operand in bytes. For example (4,True) is the size directive for a 4-byte memory operand.
-- It can be `False` to indicate that the operand should not be rendered (e.g, in case of an LEA instruction).
type NASM_SizeDir = (Int,Bool)

data NASM_Operand =
    NASM_Operand_Address NASM_Address
  | NASM_Operand_EffectiveAddress NASM_Address
  | NASM_Operand_Reg Register
  | NASM_Operand_Memory NASM_SizeDir NASM_Address
  | NASM_Operand_Immediate Immediate
 deriving (Eq,Generic)


-- | An address can either be a computation or some symbol.
data NASM_Address = NASM_Addr_Compute NASM_Address_Computation | NASM_Addr_Symbol Symbol | NASM_Addr_Label NASM_Label | NASM_JumpTarget ResolvedJumpTarget
 deriving (Eq,Generic,Ord)


-- | An address computation within an operand. The computation is: segment + [base + index*scale + disp]
data NASM_Address_Computation = NASM_Address_Computation
  {
    nasm_segment  :: Maybe Register
  , nasm_index    :: Maybe Register
  , nasm_scale    :: Word64
  , nasm_base     :: Maybe Register
  , nasm_displace :: Maybe Word64
  }
 deriving (Eq,Generic,Ord)



-- | A data section consists of a list of data section entries.
-- Each DataEntry stores its address and a value.
data NASM_DataEntry =
    DataEntry_Byte Word8                   -- ^ A single byte
  | DataEntry_String [Word8] Bool          -- ^ A string of characters (Word8) with a Bool indicating a zero at the end
  | DataEntry_Pointer (NASM_Address,Annot) -- ^ A pointer (a label or external symbol)
  | DataEntry_BSS Int                      -- ^ A BSS section with a given size in bytes
  | DataEntry_Label NASM_Label             -- ^ A label
 deriving (Generic,Ord,Eq)

instance Show NASM_DataEntry where
  show (DataEntry_Byte b)              = ".byte 0x" ++ showHex b
  show (DataEntry_String str zero)     = if zero then ".asciz \"" ++ word8s_to_string str ++ "\"" else ".ascii \"" ++ word8s_to_string str ++ "\""
  show (DataEntry_Pointer (ptr,annot)) = ".quad " ++ show ptr ++ "    # " ++ render_annot annot
  show (DataEntry_BSS sz)              = ".space " ++ show sz
  show (DataEntry_Label l)             = show l ++ ":"

-- | A data section then consists of:
data NASM_DataSection = NASM_DataSection {
  nasm_data_section :: (String,String,Word64),        -- ^ (segment,section,address)
  nasm_data_section_align :: Int,                     -- ^ The alignment (0 if unknown)
  nasm_data_section_data :: [(Word64, NASM_DataEntry)] -- ^ A list of DataEntries
}
 deriving (Generic)



instance Cereal.Serialize NASM_Label
instance Cereal.Serialize NASM_Section
instance Cereal.Serialize NASM_TextSection
instance Cereal.Serialize NASM_Line
instance Cereal.Serialize NASM_Instruction
instance Cereal.Serialize NASM_Operand
instance Cereal.Serialize NASM_Address
instance Cereal.Serialize NASM_Address_Computation
instance Cereal.Serialize NASM_DataEntry
instance Cereal.Serialize NASM_DataSection
instance Cereal.Serialize NASM






label_to_operand l = NASM_Operand_Address $ NASM_Addr_Label l
label_to_mem_operand sizedir l = NASM_Operand_Memory sizedir $ NASM_Addr_Label l
label_to_eff_operand l = NASM_Operand_EffectiveAddress $ NASM_Addr_Label l


mk_nasm_instr m ops = NASM_Instruction Nothing (Just m) ops "" []


empty_address =  NASM_Address_Computation Nothing Nothing 1 Nothing Nothing



(withAnnot) instr annot = instr { nasm_annot = nasm_annot instr ++ annot }
(withComment) instr comment = instr {nasm_comment = nasm_comment instr ++ comment }




-- Pretty printing
instance Show NASM_DataSection where
  show (NASM_DataSection (seg,sec,a0) align entries) = ".section " ++ sec ++ show_align align ++ " # @" ++ showHex a0 ++ "\n"  ++ (intercalate "\n" $ map show_entry entries)
   where
    show_entry (a,e@(DataEntry_String _ _)) = show e ++ " # @ " ++ showHex a
    show_entry (a,e) = show e

    show_align 0 = ""
    show_align n = "\n.align=" ++ show n

word8s_to_string = concatMap (escape . w2c)
 where
  escape '\\' = "\\\\"
  escape '`'  = "\\`"
  escape '\n' = "\\n"
  escape '\t' = "\\t"
  escape '"'  = "\\\""
  escape c    = [c]

instance Show NASM_TextSection where
  show (NASM_TextSection f blocks _) = comment_block ["Function: " ++ f] ++ intercalate "\n\n" (map render_block blocks) ++ "\n\n"
   where
    render_block (blockID,lines) = intercalate "\n" $ map show lines

    comment str = "# " ++ str

    comment_block strs = intercalate "\n" $ comment_block_delim strs : (map comment strs ++ [comment_block_delim strs,""])
    comment_block_delim strs = comment $ replicate (length $ max strs) '-'
    max strs = maximumBy compare_lengths strs
    compare_lengths str0 str1 = compare (length str0) (length str1)



instance Show NASM_Line where
  show (NASM_Line i) = "  " ++ show i
  show (NASM_Label str) = show str ++ ":"
  show (NASM_Comment indent str) = replicate indent ' ' ++ "# " ++ str


instance Show NASM_Instruction where
  show (NASM_Instruction pre m ops comment annot) = concat
    [ intercalate " " $ filter ((/=) "") [ show_prefix pre, show_mnemonic m ++ show_suffix ops, star ++ show_ops ops]
    , mk_comment
    ]
   where
    show_prefix Nothing  = ""
    show_prefix (Just PrefixRep) = "repz"
    show_prefix (Just PrefixRepNE) = "repne"
    show_prefix (Just p) = toLowerCase $ show p

    show_suffix :: [NASM_Operand] -> String
    show_suffix [NASM_Operand_Memory (si, _) a, NASM_Operand_Immediate _] = size_to_suffix si
    show_suffix [NASM_Operand_Memory (si, _) a] = size_to_suffix si
    show_suffix [NASM_Operand_Reg r, NASM_Operand_Memory (si, _) a] = if m == Just MOVZX || m == Just MOVSX then size_to_suffix si ++ bytesize_to_suffix (regSize r) else ""
    show_suffix [NASM_Operand_Reg r1, NASM_Operand_Reg r2] = if m == Just MOVZX || m == Just MOVSX then bytesize_to_suffix (regSize r2) ++ bytesize_to_suffix (regSize r1) else ""
    show_suffix _ = ""

    size_to_suffix 1 = "b"
    size_to_suffix 2 = "w"
    size_to_suffix 4 = "l"
    size_to_suffix 8 = "q"
    size_to_suffix 10 = "t"
    size_to_suffix 16 = "o"
    size_to_suffix _ = error "size_to_suffix: invalid size"

    bytesize_to_suffix (ByteSize 1) = "b"
    bytesize_to_suffix (ByteSize 2) = "w"
    bytesize_to_suffix (ByteSize 4) = "l"
    bytesize_to_suffix (ByteSize 8) = "q"
    bytesize_to_suffix (ByteSize 10) = "t"
    bytesize_to_suffix (ByteSize 16) = "o"
    bytesize_to_suffix _ = error "bytesize_to_suffix: invalid size"

    show_mnemonic Nothing  = ""
    -- NOT NEEDED, JUST FOR EASY DIFF
    show_mnemonic (Just JZ)     = "je"
    show_mnemonic (Just JNZ)    = "jne"
    show_mnemonic (Just SETNBE) = "seta"
    show_mnemonic (Just CMOVZ)  = "cmove"
    show_mnemonic (Just CMOVNZ) = "cmovne"
    show_mnemonic (Just SETNLE) = "setg"
    show_mnemonic (Just MOVZX) = "movz"
    show_mnemonic (Just MOVSX) = "movs"
    show_mnemonic (Just MOVSD) = "movsl"
    show_mnemonic (Just p) = toLowerCase $ show p

    show_ops = intercalate ", " . reverse . map show_op

    star =
      case (m, ops) of
      (Just CALL, [NASM_Operand_Memory _ _]) -> "*"
      (Just CALL, [NASM_Operand_Reg _]) -> "*"
      (Just JMP, [NASM_Operand_Memory _ _]) -> "*"
      (Just JMP, [NASM_Operand_Reg _]) -> "*"
      _ -> ""

    mk_comment =
      let str = render_annot annot in
        if comment == [] && str == [] then ""
        else if comment /= [] && str /= [] then " # " ++ comment ++ "    # " ++ str
        else if comment == [] then "    # " ++ str
        else " # " ++ comment

    instr_op_size =
      case partition isImmediate ops of
        (_,(op:_))  -> operand_size op
        ([imm],[])  -> operand_size imm

    isImmediate (NASM_Operand_Immediate _) = True
    isImmediate _                          = False


    show_op (NASM_Operand_Reg r)              = "%" ++ toLowerCase (show r)
    show_op (NASM_Operand_Address a)          = show a
    show_op (NASM_Operand_EffectiveAddress (NASM_Addr_Label l)) = show l ++ "(%rip)"
    show_op (NASM_Operand_EffectiveAddress (NASM_Addr_Symbol (AddressOfLabel  l _))) = l ++ "(%rip)"
    show_op (NASM_Operand_EffectiveAddress (NASM_Addr_Symbol (AddressOfObject  l _))) = l ++ "(%rip)"
    show_op (NASM_Operand_EffectiveAddress a) = show a
    show_op (NASM_Operand_Memory _ (NASM_Addr_Label l)) = show l ++ "(%rip)"
    show_op (NASM_Operand_Memory _ (NASM_Addr_Symbol (AddressOfLabel  l _))) = l ++ "(%rip)"
    show_op (NASM_Operand_Memory _ (NASM_Addr_Symbol (AddressOfObject l _))) = l ++ "(%rip)"
    show_op (NASM_Operand_Memory _ a)   = show a
    show_op (NASM_Operand_Immediate (Immediate (BitSize si) imm)) =
      case (instr_op_size,si) of
        (ByteSize 16,64) -> "$0x" ++ showHex imm
        (ByteSize 16,32) -> "$0x" ++ showHex imm
        (ByteSize 16,16) -> "$0x" ++ showHex imm
        (ByteSize 16, 8) -> "$0x" ++ showHex imm

        (ByteSize 8,64) -> "$0x" ++ showHex imm
        (ByteSize 8,32) -> "$0x" ++ showHex (sextend_32_64 imm)
        (ByteSize 8,16) -> "$0x" ++ showHex (sextend_16_64 imm)
        (ByteSize 8, 8) -> "$0x" ++ showHex (sextend_8_64 imm)

        (ByteSize 4, 64) -> "$0x" ++ showHex imm
        (ByteSize 4, 32) -> "$0x" ++ showHex imm
        (ByteSize 4, 16) -> "$0x" ++ showHex (sextend_16_32 imm)
        (ByteSize 4,  8) -> "$0x" ++ showHex (sextend_8_32 imm)

        (ByteSize 2, 64) -> "$0x" ++ showHex imm
        (ByteSize 2, 32) -> "$0x" ++ showHex imm
        (ByteSize 2, 16) -> "$0x" ++ showHex imm
        (ByteSize 2,  8) -> "$0x" ++ showHex (sextend_8_16 imm)

        (ByteSize 1,  64) -> "$0x" ++ showHex imm
        (ByteSize 1,  32) -> "$0x" ++ showHex imm
        (ByteSize 1,  16) -> "$0x" ++ showHex imm
        (ByteSize 1,   8) -> "$0x" ++ showHex imm


        (ByteSize si0,si1) -> error $ show (si0,si1)

    operand_size (NASM_Operand_Reg r) = regSize r
    operand_size (NASM_Operand_Address a) = ByteSize 8
    operand_size (NASM_Operand_EffectiveAddress a) = ByteSize 8
    operand_size (NASM_Operand_Memory (si,_) a) = ByteSize si
    operand_size (NASM_Operand_Immediate (Immediate (BitSize si) imm)) = ByteSize $ si `div` 8

--"0x" ++ showHex imm
  --  show_op (NASM_Operand_Immediate (Immediate (BitSize 32) imm)) = "0x" ++ showHex imm -- showHex (sextend_32_64 imm)
   -- show_op (NASM_Operand_Immediate (Immediate (BitSize 16) imm)) = "0x" ++ showHex (sextend_16_64 imm)
    --show_op (NASM_Operand_Immediate (Immediate (BitSize 8)  imm)) = "0x" ++ showHex (sextend_8_64 imm)





render_annot :: Annot -> String
render_annot [] = ""
render_annot m  = intercalate "," (map render_annot_elmt m)
 where
  render_annot_elmt (a,l) = "0x" ++ showHex a ++ " --> " ++ show l


-- TODO: Do we need the below code?
instance Show NASM_Operand where
  show (NASM_Operand_Reg r)              = show r
  show (NASM_Operand_Address a)          = show a
  show (NASM_Operand_EffectiveAddress a) = show a
  show (NASM_Operand_Memory sizedir a)   = show a -- show_nasm_sizedir sizedir ++ show a
  show (NASM_Operand_Immediate (Immediate (BitSize 64) imm)) = "$0x" ++ showHex imm
  show (NASM_Operand_Immediate (Immediate (BitSize 32) imm)) = "$0x" ++ showHex imm -- showHex (sextend_32_64 imm)
  show (NASM_Operand_Immediate (Immediate (BitSize 16) imm)) = "$0x" ++ showHex (sextend_16_64 imm)
  show (NASM_Operand_Immediate (Immediate (BitSize 8)  imm)) = "$0x" ++ showHex (sextend_8_64 imm)


instance Show NASM_Label where
  show (Label _ str) = str

show_macro_name segment section a0 = "RELA" ++ section_name segment section a0

section_name segment section a0 = segment ++ "_" ++ section ++ "_0x" ++ showHex a0

instance Show NASM_Address_Computation where
 show (NASM_Address_Computation Nothing Nothing _ Nothing (Just 0)) = "%ds:0"
 show (NASM_Address_Computation Nothing Nothing _ Nothing Nothing)  = "%ds:0"
 show (NASM_Address_Computation seg ind sc base displ) =
   let str0 = show_seg seg
       str1 = intercalate ", " $ filter ((/=) "") [show_base base, show_index_scale ind sc]
       str1' = if null (show_base base) && not (null str1) then ", " ++ str1 else str1
       str1'' = if not (null str1) then "(" ++ str1' ++ ")" else str1'
       str2 = show_displacement str1 displ in
     concat [str0,str2,str1'']
  where
   show_seg Nothing  = ""
   show_seg (Just r) = "%" ++ toLowerCase (show r) ++ ":"

   show_base Nothing = ""
   show_base (Just r) = "%" ++ toLowerCase (show r)

   show_index_scale Nothing _ = ""
   show_index_scale (Just r) 0 = ""
   show_index_scale (Just r) 1 = "%" ++ toLowerCase (show r) ++ ", 1"
   show_index_scale (Just r) imm = "%" ++ toLowerCase (show r) ++ ", " ++ show imm

show_displacement _ Nothing     = ""
show_displacement "" (Just 0)   = ""
show_displacement "" (Just imm) = "0x" ++ showHex imm
show_displacement _  (Just imm)
  | testBit (fromIntegral imm::Word64) 63 = " -0x" ++ showHex (0 - imm)
  | otherwise =  " 0x" ++ showHex imm


instance Show NASM_Address where
  show (NASM_Addr_Compute addr)               = show addr
  show (NASM_Addr_Symbol sym)                 = show_symbol sym
  show (NASM_JumpTarget (External sym))       = sym ++ "@PLT"
  show (NASM_JumpTarget (ExternalDeref sym))  = "[ " ++ sym ++ " ]"
  show (NASM_Addr_Label l)                    = show l


show_symbol (PointerToLabel  l True)       = l ++ "@PLT"
show_symbol (PointerToLabel  l False)      = l
show_symbol (PointerToObject l True)       = l ++ "@GOTPCREL(%rip)"
show_symbol (PointerToObject l False)      = l ++ "@GOTPCREL(%rip)"
show_symbol (AddressOfLabel  l _)          = l
show_symbol (AddressOfObject l _)          = l
show_symbol (Relocated_ResolvedObject l _) = l
