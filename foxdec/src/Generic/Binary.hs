{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict, StandaloneDeriving, DeriveGeneric, ExistentialQuantification #-}

module Generic.Binary where

import Base


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Word 
import Data.List (intercalate)

import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)


-- |  Information on the sections in the binary
data SectionsInfo = SectionsInfo {
  si_sections    :: [(String,String,Word64,Word64)], -- ^ Sections: segment names, section names, addresses and sizes.
  si_min_address :: Word64,
  si_max_address :: Word64
 }
 deriving (Show,Generic,Eq)


-- | An address a0 can have a symbol.
--
-- Relocated_Function:
-- E.g:
-- 		0xcfe0 --> malloc
-- Means that reading 8 bytes from address 0xcfe0 procudes a pointer to malloc.
-- Thus an instruction: "CALL qword ptr [0xcfe0]" can be seen as "CALL malloc".
--
-- Relocated_Object:
-- E.g.:
--    0xd0a8 --> stdout
-- Means that "mov rdi,QWORD PTR [0xd0a8]" can be seen as "mov rdi, QWORD PTR [stdout]"
--
-- Relocated_Address:
-- E.g.:
--    0xaaa -> 0xbbbb
-- Means that reading 8 bytes from address 0xaaaa produces the value 0xbbbb
data Symbol = 
    Relocated_Function String -- ^ Address a0 is a pointer to memory sotring the entry of an external function
  | Relocated_Label    String -- ^ Address a0 can be replaced by the string, e.g., "stdout" or "optind"
  | Internal_Label     String -- ^ Address a0 can be replaced by the string.
  deriving (Generic,Eq,Show)

data SymbolTable = SymbolTable (IM.IntMap Symbol)
  deriving (Generic,Eq)

instance Show SymbolTable where
  show (SymbolTable tbl) = intercalate "\n" $ map show_entry $ IM.assocs tbl
   where
    show_entry (a0,Relocated_Function f) = showHex a0 ++ " --> " ++ f ++ " (external function)"
    show_entry (a0,Relocated_Label l)    = showHex a0 ++ " --> " ++ l ++ " (external object)"
    show_entry (a0,Internal_Label f)     = showHex a0 ++ " --> " ++ f ++ " (internal)"


is_internal_symbol (Internal_Label _) = True
is_internal_symbol _ = False

is_external_symbol (Relocated_Function _) = True
is_external_symbol (Relocated_Label _) = True
is_external_symbol _ = False

symbol_to_name (Relocated_Function str) = Just str
symbol_to_name (Relocated_Label str) = Just str
symbol_to_name (Internal_Label str) = Just str


data Relocation = 
  Relocation Word64 Word64 -- ^ 8: At address a0, address a1 has been written, i.e., qword ptr[a0] == a1
  deriving (Generic,Eq,Ord)

instance Show Relocation where
  show (Relocation a0 a1)  = showHex a0 ++ " --> " ++ showHex a1



class BinaryClass a where
  binary_read_ro_data :: a -> Word64 -> Int -> Maybe [Word8]
  binary_read_data :: a -> Word64 -> Int -> Maybe [Word8]
  binary_get_sections_info :: a -> SectionsInfo
  binary_get_symbols :: a -> SymbolTable
  binary_get_relocations :: a -> S.Set Relocation
  binary_pp :: a -> String
  binary_entry :: a -> Word64
  binary_text_section_size :: a -> Int



data Binary = forall b . BinaryClass b => Binary b


instance BinaryClass Binary where
  binary_read_ro_data (Binary b) = binary_read_ro_data b
  binary_read_data (Binary b) = binary_read_data b
  binary_get_sections_info (Binary b) = binary_get_sections_info b
  binary_get_symbols (Binary b) = binary_get_symbols b
  binary_get_relocations (Binary b) = binary_get_relocations b
  binary_pp (Binary b) = binary_pp b
  binary_entry (Binary b) = binary_entry b
  binary_text_section_size (Binary b) = binary_text_section_size b




