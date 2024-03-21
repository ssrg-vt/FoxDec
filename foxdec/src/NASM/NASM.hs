{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : L0ToNASM
Description : Lift the L0 representation of the binary to symbolized and recompilable NASM.
-}



module NASM.NASM where

import Base
import qualified X86.Register as X86
import qualified X86.Prefix as X86
import qualified X86.Opcode as X86

import qualified Data.Set as S
import Data.Word
import Data.List
import Data.Bits (testBit)
import qualified Data.IntMap as IM
import Data.ByteString.Internal (w2c)


-- | NASM contains external symbols, sections, and a footer
data NASM = NASM {
  nasm_externals :: S.Set String,
  nasm_sections  :: [NASM_Section],
  nasm_footer    :: [String]
 }


-- | A NASM label is either a string, or a macro. The latter is used when referring to addresses within data sections.
data NASM_Label = 
    Label String -- ^ Normal label
  | Macro String String Word64 Word64 -- ^ Macro into data section (segment, section, a0, offset)
 deriving (Eq,Ord)


-- | A NASM section is either a NASM text section or NASM data section
-- A NASM data section is simply a String
data NASM_Section = NASM_Section_Text NASM_TextSection | NASM_Section_Data [NASM_DataSection]

-- | An annotation consists of an address that is being symbolized to a label and an offset.
-- The offset will often be 0.
-- For example:
--   0x1016 --> L1000_2
--   0x4028 --> L_.bss_0x4020 + 0x8
type Annot = [(Word64,NASM_Label,Word64)]

-- | A NASM text section contains a header (usually just some comments), and a graph of basic blocks
data NASM_TextSection = NASM_TextSection  {
  nasm_function_name  :: String,
  nasm_blocks         :: [(Int,[NASM_Line])] -- ^ A mapping of blockIDs to instructions
 }


-- | A line is either a comment, an instruction or a label
data NASM_Line = 
    NASM_Comment Int String    -- ^ A comment with an indentation level (number of spaces
  | NASM_Line NASM_Instruction -- ^ An instruction with an annotation
  | NASM_Label NASM_Label      -- ^ A label
 deriving Eq

data NASM_Instruction = NASM_Instruction
  {
    nasm_prefix   :: Maybe X86.Prefix
  , nasm_mnemonic :: Maybe X86.Opcode
  , nasm_operands :: [NASM_Operand]
  , nasm_comment  :: String
  , nasm_annot    :: Annot
  }
 deriving Eq

type NASM_SizeDir = (Int,Bool)

data NASM_Operand = 
    NASM_Operand_Address NASM_Address
  | NASM_Operand_EffectiveAddress NASM_Address
  | NASM_Operand_Memory NASM_SizeDir NASM_Address
  | NASM_Operand_Immediate Word64
 deriving Eq



data NASM_Address = NASM_Address
  {
    nasm_segment  :: Maybe X86.Register
  , nasm_index    :: Maybe X86.Register
  , nasm_scale    :: Word64
  , nasm_base     :: Maybe X86.Register
  , nasm_displace :: Maybe Word64
  , nasm_label    :: Maybe NASM_Label
  , nasm_function :: Maybe String
  }
 deriving Eq

data NASM_DataEntry = DataEntry_Byte Word64 Word8 | DataEntry_String Word64 [Word8] | DataEntry_Pointer Word64 (NASM_Address,Annot) | DataEntry_BSS Int

data NASM_DataSection = NASM_DataSection {
  nasm_data_section :: (String,String,Word64), -- ^ (segment,section,address)
  nasm_data_section_align :: Int,
  nasm_data_section_labels :: IM.IntMap (S.Set NASM_Label),
  nasm_data_section_data :: [NASM_DataEntry]
}

instance Show NASM_DataSection where
  show (NASM_DataSection (seg,sec,a0) align labels entries) = "section " ++ sec ++ " align=" ++ show align ++ "\n"  ++ show_data 0 entries
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


word8s_to_string = concatMap (escape . w2c) 
 where
  escape '\\' = "\\\\"
  escape '`'  = "\\`"
  escape '\n'  = "\\n"
  escape '\t'  = "\\t"
  escape c    = [c]



mk_nasm_instr m ops = NASM_Instruction Nothing (Just m) ops "" []

reg_to_operand reg = NASM_Operand_Address $ empty_address {nasm_base = Just reg }

label_to_operand = NASM_Operand_Address . mk_address_label
label_to_mem_operand sizedir = NASM_Operand_Memory sizedir . mk_address_label
label_to_eff_operand = NASM_Operand_EffectiveAddress . mk_address_label

empty_address = NASM_Address Nothing Nothing 1 Nothing Nothing Nothing Nothing

mk_address_label str = empty_address {nasm_label = Just str }
mk_address_function str = empty_address {nasm_function = Just str }

(withAnnot) instr annot = instr { nasm_annot = nasm_annot instr ++ annot }
(withComment) instr comment = instr {nasm_comment = nasm_comment instr ++ comment }




-- Pretty printing

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
  show (Label str) = str
  show (Macro segment section a0 offset) = show_macro_name segment section a0 ++ show_offset offset
   where
    show_offset n = "(0x" ++ showHex offset ++ ")"

show_macro_name segment section a0 = "RELA" ++ section_name segment section a0

section_name segment section a0 = segment ++ "_" ++ section ++ "_0x" ++ showHex a0

instance Show NASM_Address where
 show (NASM_Address Nothing Nothing 1 Nothing (Just 0) Nothing Nothing) = "ds:0" 
 show (NASM_Address seg ind sc base displ label function) = 
   let str0 = show_seg seg
       str1 = intercalate " + " $ filter ((/=) "") [show_base base, show_index_scale ind sc, show_label label, show_function function] 
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

   show_label Nothing  = ""
   show_label (Just s) = show s

   show_function Nothing = ""
   show_function (Just f) = f ++ " wrt ..plt"

