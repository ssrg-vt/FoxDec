{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict, DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : NASM
Description : A datastructure for storing NASM code.
-}



module NASM.NASM where

import Base
import Analysis.Context
import qualified X86.Register as X86
import qualified X86.Prefix as X86
import qualified X86.Opcode as X86
import Data.Symbol



import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)

import qualified Data.Set as S
import Data.Word
import Data.List
import Data.Bits (testBit)
import qualified Data.IntMap as IM
import Data.ByteString.Internal (w2c)


-- | NASM contains external symbols, sections, and a footer
data NASM = NASM {
  nasm_externals :: S.Set String,
  nasm_globals   :: S.Set String,
  nasm_sections  :: [NASM_Section],
  nasm_footer    :: [String]
 }



-- | A NASM label is either a string, or a macro. The latter is used when referring to addresses within data sections.
-- For example:
--
--    Macro "" ".data" 0x4000 0x23
--
-- refers to segment "", and the section ".data" that starts at immediate address 0x4000. So the real adress was 0x4023.
data NASM_Label = 
    Label Word64 String -- ^ Normal label
  | Macro String String Word64 Word64 -- ^ Macro into data section (segment, section, a0, offset)
 deriving (Eq,Ord)


-- | A NASM section is either a NASM text section or NASM data section
data NASM_Section = NASM_Section_Text NASM_TextSection | NASM_Section_Data [NASM_DataSection]

-- | An annotation consists of an address that is being symbolized to a label and an offset.
-- The offset will often be 0.
-- For example:
--   0x1016 --> L1000_2
--   0x4028 --> L_.bss_0x4020 + 0x8
-- Annotations translate to NASM comments, and have no effect on the actual NASM itself.
type Annot = [(Word64,NASM_Label,Word64)]

-- | A NASM text section contains a name and an **ordered** list of basic blocks.
-- Each basic block has an ID and a list of lines.
data NASM_TextSection = NASM_TextSection  {
  nasm_function_name  :: String,
  nasm_blocks         :: [(Int,[NASM_Line])] -- ^ A mapping of blockIDs to instructions
 }


-- | A NASM line is either a comment, an instruction or a label
data NASM_Line = 
    NASM_Comment Int String    -- ^ A comment with an indentation level (number of spaces)
  | NASM_Line NASM_Instruction -- ^ An instruction with an annotation
  | NASM_Label NASM_Label      -- ^ A label
 deriving Eq


-- | An instruction consists of a prefix, an opcode, a list of operands, a comment (possibly empty) and an annotation (possibly empty).
data NASM_Instruction = NASM_Instruction
  {
    nasm_prefix   :: Maybe X86.Prefix
  , nasm_mnemonic :: Maybe X86.Opcode
  , nasm_operands :: [NASM_Operand]
  , nasm_comment  :: String
  , nasm_annot    :: Annot
  }
 deriving Eq


-- | A size directive for a memory operand in bytes. For example (4,True) is the size directive for a 4-byte memory operand.
-- It can be `False` to indicate that the operand should not be rendered (e.g, in case of an LEA instruction).
type NASM_SizeDir = (Int,Bool)

data NASM_Operand = 
    NASM_Operand_Address NASM_Address
  | NASM_Operand_EffectiveAddress NASM_Address
  | NASM_Operand_Memory NASM_SizeDir NASM_Address
  | NASM_Operand_Immediate Word64
 deriving Eq


-- | An address can either be a computation or some symbol.
data NASM_Address = NASM_Addr_Compute NASM_Address_Computation | NASM_Addr_Symbol Symbol | NASM_Addr_Label NASM_Label (Maybe Word64)
 deriving Eq


-- | An address computation within an operand. The computation is: segment + [base + index*scale + disp]
data NASM_Address_Computation = NASM_Address_Computation
  {
    nasm_segment  :: Maybe X86.Register
  , nasm_index    :: Maybe X86.Register
  , nasm_scale    :: Word64
  , nasm_base     :: Maybe X86.Register
  , nasm_displace :: Maybe Word64
  }
 deriving Eq



-- | A data sexction consists of a list of data section entries.
-- Each DataEntry stores its address and a value.
data NASM_DataEntry =
    DataEntry_Byte Word64 Word8                   -- ^ A single byte
  | DataEntry_String Word64 [Word8]               -- ^ A string of characters (Word8) 
  | DataEntry_Pointer Word64 (NASM_Address,Annot) -- ^ A pointer (a label or external symbol)
  | DataEntry_BSS Int                             -- ^ A BSS section with a given size in bytes

-- | A data section then consists of:
data NASM_DataSection = NASM_DataSection {
  nasm_data_section :: (String,String,Word64),              -- ^ (segment,section,address)
  nasm_data_section_align :: Int,                           -- ^ The alignment (0 if unknown)
  nasm_data_section_labels :: IM.IntMap (S.Set NASM_Label), -- ^ Used internally only
  nasm_data_section_data :: [NASM_DataEntry]                -- ^ A list of DataEntries
}


deriving instance Generic NASM_Label
deriving instance Generic NASM_Section
deriving instance Generic NASM_TextSection
deriving instance Generic NASM_Line
deriving instance Generic NASM_Instruction
deriving instance Generic NASM_Operand
deriving instance Generic NASM_Address
deriving instance Generic NASM_Address_Computation
deriving instance Generic NASM_DataEntry
deriving instance Generic NASM_DataSection
deriving instance Generic NASM

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






label_to_operand l = NASM_Operand_Address $ NASM_Addr_Label l Nothing
label_to_mem_operand sizedir l = NASM_Operand_Memory sizedir $ NASM_Addr_Label l Nothing
label_to_eff_operand l = NASM_Operand_EffectiveAddress $ NASM_Addr_Label l Nothing


mk_nasm_instr m ops = NASM_Instruction Nothing (Just m) ops "" []

reg_to_operand reg = NASM_Operand_Address $ NASM_Addr_Compute $ empty_address {nasm_base = Just reg }

empty_address =  NASM_Address_Computation Nothing Nothing 1 Nothing Nothing



(withAnnot) instr annot = instr { nasm_annot = nasm_annot instr ++ annot }
(withComment) instr comment = instr {nasm_comment = nasm_comment instr ++ comment }




-- Pretty printing
instance Show NASM_DataSection where
  show (NASM_DataSection (seg,sec,a0) align labels entries) = "section " ++ sec ++ show_align align ++ "\n"  ++ show_data 0 entries
   where
    show_data n []     = show_label n
    show_data n (e:es) = show_label n ++ show_entry e ++ "\n" ++ show_data (n + entry_length e) es 

    entry_length (DataEntry_Byte _ _)      = 1
    entry_length (DataEntry_String _ str)  = length str + 1
    entry_length (DataEntry_Pointer _ _)   = 8
    entry_length (DataEntry_BSS sz)        = sz

    show_label n =
      case IM.lookup n labels of
        Nothing     -> ""
        Just labels -> concatMap (\l -> show l ++ ":\n") $ S.toList labels

    show_entry (DataEntry_Byte _ b)              = "db 0" ++ showHex b ++ "h"
    show_entry (DataEntry_String _ str)          = "db `" ++ word8s_to_string str ++ "`" ++ ", 0"
    show_entry (DataEntry_Pointer _ (ptr,annot)) = "dq " ++ show ptr ++ "    ; " ++ render_annot annot
    show_entry (DataEntry_BSS sz)                = "resb " ++ show sz

    show_align 0 = ""
    show_align n = " align=" ++ show n

word8s_to_string = concatMap (escape . w2c) 
 where
  escape '\\' = "\\\\"
  escape '`'  = "\\`"
  escape '\n'  = "\\n"
  escape '\t'  = "\\t"
  escape c    = [c]

instance Show NASM_TextSection where
  show (NASM_TextSection f blocks) = comment_block ["Function: " ++ f] ++ intercalate "\n\n" (map render_block blocks) ++ "\n\n"
   where
    render_block (blockID,lines) = intercalate "\n" $ map show lines

    comment str = "; " ++ str

    comment_block strs = intercalate "\n" $ comment_block_delim strs : (map comment strs ++ [comment_block_delim strs,""])
    comment_block_delim strs = comment $ replicate (length $ max strs) '-' 
    max strs = maximumBy compare_lengths strs
    compare_lengths str0 str1 = compare (length str0) (length str1)



instance Show NASM_Line where
  show (NASM_Line i) = "  " ++ show i
  show (NASM_Label str) = show str ++ ":"
  show (NASM_Comment indent str) = replicate indent ' ' ++ "; " ++ str


instance Show NASM_Instruction where
  show (NASM_Instruction pre m ops comment annot) = concat
    [ intercalate " " $ filter ((/=) "") [ show_prefix pre, show_mnemonic m, show_ops ops]
    , mk_comment
    ]
   where
    show_prefix Nothing  = ""
    show_prefix (Just X86.NOTRACK) = ""
    show_prefix (Just p) = show p

    show_mnemonic Nothing  = ""
    show_mnemonic (Just p) = show p
 
    show_ops = intercalate ", " . map show

    mk_comment =
      let str = render_annot annot in
        if comment == [] && str == [] then ""
        else if comment /= [] && str /= [] then " ; " ++ comment ++ "    ; " ++ str
        else if comment == [] then "    ; " ++ str
        else " ; " ++ comment




render_annot :: Annot -> String
render_annot [] = ""
render_annot m  = intercalate "," (map render_annot_elmt m)
 where
  render_annot_elmt (a,l,offset) = "0x" ++ showHex a ++ " --> " ++ show l ++ (if offset == 0 then "" else " + 0x" ++ showHex offset)
    


instance Show NASM_Operand where
  show (NASM_Operand_Address a)          = show a
  show (NASM_Operand_EffectiveAddress a) = "[" ++ show a ++ "]"
  show (NASM_Operand_Memory sizedir a)   = show_nasm_sizedir sizedir ++ " [" ++ show a ++ "]"
  show (NASM_Operand_Immediate imm)      = "0x" ++ showHex imm

show_nasm_sizedir (_,False) = ""
show_nasm_sizedir (1,_) = "byte"
show_nasm_sizedir (2,_) = "word"
show_nasm_sizedir (4,_) = "dword"
show_nasm_sizedir (8,_) = "qword"
show_nasm_sizedir (10,_) = "tword"
show_nasm_sizedir (16,_) = "oword"

instance Show NASM_Label where
  show (Label _ str) = str
  show (Macro segment section a0 offset) = show_macro_name segment section a0 ++ show_offset offset
   where
    show_offset n = "(0x" ++ showHex offset ++ ")"

show_macro_name segment section a0 = "RELA" ++ section_name segment section a0

section_name segment section a0 = segment ++ "_" ++ section ++ "_0x" ++ showHex a0

instance Show NASM_Address_Computation where
 show (NASM_Address_Computation Nothing Nothing 1 Nothing (Just 0)) = "ds:0" 
 show (NASM_Address_Computation seg ind sc base displ) = 
   let str0 = show_seg seg
       str1 = intercalate " + " $ filter ((/=) "") [show_base base, show_index_scale ind sc] 
       str2 = show_displacement str1 displ in
     concat [str0,str1,str2]
  where
   show_seg Nothing  = ""
   show_seg (Just r) = show r ++ ":"

   show_base Nothing = ""
   show_base (Just r) = show r

   show_index_scale Nothing _ = ""
   show_index_scale (Just r) 0 = ""
   show_index_scale (Just r) 1 = show r
   show_index_scale (Just r) imm = show r ++ " * " ++ show imm

show_displacement _ Nothing     = ""
show_displacement "" (Just 0)   = ""
show_displacement "" (Just imm) = "0x" ++ showHex imm
show_displacement _  (Just imm) 
  | testBit (fromIntegral imm::Word64) 63 = " - 0x" ++ showHex (0 - imm)
  | otherwise =  " + 0x" ++ showHex imm


instance Show NASM_Address where
  show (NASM_Addr_Compute addr)   = show addr
  show (NASM_Addr_Symbol sym)     = show_symbol sym
  show (NASM_Addr_Label l displ)  = show l ++ show_displacement (show l) displ


show_symbol (PointerToLabel  l True)       = l ++ " wrt ..plt"
show_symbol (PointerToLabel  l False)      = l
show_symbol (PointerToObject l True)       = l ++ " wrt ..got"
show_symbol (PointerToObject l False)      = l ++ " wrt ..got"
show_symbol (AddressOfLabel  l _)          = l
show_symbol (AddressOfObject l _)          = l
show_symbol (Relocated_ResolvedObject l _) = l
