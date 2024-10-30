{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict, StandaloneDeriving, DeriveGeneric, ExistentialQuantification #-}

module Generic.Binary where

import Base
import Data.Symbol

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
  si_sections    :: [(String,String,Word64,Word64,Word64)], -- ^ Sections: segment names, section names, addresses, sizes, and alignment.
  si_min_address :: Word64,
  si_max_address :: Word64
 }
 deriving (Show,Generic,Eq)





data SymbolTable = SymbolTable (IM.IntMap Symbol) (S.Set String)
  deriving (Generic,Eq)

instance Show SymbolTable where
  show (SymbolTable tbl globals) = (intercalate "\n" $ map show_entry $ IM.assocs tbl) ++ "\n" ++ (intercalate "\n" $ map show_global $ S.toList globals)
   where
    show_entry (a0,PointerToLabel f b)           = showHex a0 ++ " --> " ++ f ++ show_in_ex b "label"
    show_entry (a0,PointerToObject l b)          = showHex a0 ++ " --> " ++ l ++ show_in_ex b "object"
    show_entry (a0,AddressOfObject l b)          = showHex a0 ++ " === " ++ l ++ show_in_ex b "object"
    show_entry (a0,AddressOfLabel f b)           = showHex a0 ++ " === " ++ f ++ show_in_ex b "label"
    show_entry (a0,Relocated_ResolvedObject l a) = showHex a0 ++ " (" ++ l ++ ") --> " ++ showHex a ++ " (external object, but internally resolved)"

    show_global f = f ++ " (global)"

    show_in_ex True  ty = " (external " ++ ty ++ ")" 
    show_in_ex False ty = " (internal " ++ ty ++ ")" 



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





symbol_to_name (PointerToLabel f b)           = f
symbol_to_name (PointerToObject l b)          = l
symbol_to_name (AddressOfObject l b)          = l
symbol_to_name (AddressOfLabel f b)           = f
symbol_to_name (Relocated_ResolvedObject l a) = l
