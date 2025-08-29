{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StandaloneDeriving, DeriveGeneric, BangPatterns, Strict, ExistentialQuantification #-}

module Binary.Generic where

import Base
import Data.Symbol
import Data.X86.Instruction

import Conventions

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.ByteString.Lazy as BS

import Data.Word 
import Data.List (intercalate, find)
import qualified Data.Serialize as Cereal hiding (get,put)
import Debug.Trace

import GHC.Generics


-- | Section Flags
data SectionFlag
    = SectionIsWritable     -- ^ Section contains writable data
    | SectionIsAllocated    -- ^ Section is allocated in memory image of program
    | SectionIsExecutable   -- ^ Section contains executable instructions
    | SectionHasFlag Int    -- ^ Processor- or environment-specific flag
    deriving (Generic,Eq)

instance Show SectionFlag where
  show SectionIsWritable    = "W"
  show SectionIsAllocated   = "A"
  show SectionIsExecutable  = "X"
  show (SectionHasFlag i)   = "EXT(" ++ show i ++ ")"

-- |  Information on the sections in the binary
data SectionsInfo = SectionsInfo {
  si_sections    :: ![(String,String,Word64,Word64,Word64,[SectionFlag])], -- ^ Sections: segment names, section names, addresses, sizes, and alignment.
  si_min_address :: !Word64,
  si_max_address :: !Word64
 }
 deriving (Show,Generic,Eq)

instance Cereal.Serialize SectionFlag
instance Cereal.Serialize SectionsInfo



data SymbolTable = SymbolTable {
  symboltable_symbols :: !(IM.IntMap Symbol),
  symboltable_exports :: !(IM.IntMap String)
  }
  deriving (Generic,Eq)

instance Show SymbolTable where
  show (SymbolTable tbl exports) = (intercalate "\n" $ map show_entry $ IM.assocs tbl) ++ "\n" ++ (intercalate "\n" $ map show_export $ IM.toList exports)
   where
    show_entry (a0,PointerToExternalFunction f)    = showHex a0 ++ " --> " ++ f
    show_entry (a0,PointerToInternalFunction f a1) = showHex a0 ++ " --> " ++ f ++ "@0x" ++ showHex a1
    show_entry (a0,PointerToObject l b)            = showHex a0 ++ " --> " ++ l ++ show_in_ex b "object"
    show_entry (a0,AddressOfObject l b)            = showHex a0 ++ " === " ++ l ++ show_in_ex b "object"
    show_entry (a0,AddressOfLabel f b)             = showHex a0 ++ " === " ++ f ++ show_in_ex b "label"
    show_entry (a0,Relocated_ResolvedObject l a)   = showHex a0 ++ " (" ++ l ++ ") --> " ++ showHex a ++ " (external object, but internally resolved)"

    show_export (a,f) = showHex a ++ ": " ++ f ++ " (exported)"

    show_in_ex True  ty = " (external " ++ ty ++ ")" 
    show_in_ex False ty = " (internal " ++ ty ++ ")" 

instance Cereal.Serialize SymbolTable


data Relocation = 
  Relocation Word64 Word64 -- ^ 8: At address a0, address a1 has been written, i.e., qword ptr[a0] == a1
  deriving (Generic,Eq,Ord)

instance Show Relocation where
  show (Relocation a0 a1)  = showHex a0 ++ " --> " ++ showHex a1

instance Cereal.Serialize Relocation


class BinaryClass a where
  binary_read_bytestring :: a -> Word64 -> Int -> Maybe BS.ByteString
  binary_read_ro_data :: a -> Word64 -> Int -> Maybe [Word8]
  binary_read_data :: a -> Word64 -> Int -> Maybe [Word8]
  binary_get_sections_info :: a -> SectionsInfo
  binary_get_symbols :: a -> SymbolTable
  binary_get_relocations :: a -> S.Set Relocation
  binary_pp :: a -> String
  binary_entry :: a -> [Word64] -- The first element is the entry point of the binary (or 0 if shared object)
  binary_text_section_size :: a -> Int
  binary_dir_name :: a -> String
  binary_file_name :: a -> String
  fetch_instruction :: a -> Word64 -> Maybe Instruction


data Binary = forall b . BinaryClass b => Binary b


instance BinaryClass Binary where
  binary_read_bytestring (Binary b) = binary_read_bytestring b
  binary_read_ro_data (Binary b) = binary_read_ro_data b
  binary_read_data (Binary b) = binary_read_data b
  binary_get_sections_info (Binary b) = binary_get_sections_info b
  binary_get_symbols (Binary b) = binary_get_symbols b
  binary_get_relocations (Binary b) = binary_get_relocations b
  binary_pp (Binary b) = binary_pp b
  binary_entry (Binary b) = binary_entry b
  binary_text_section_size (Binary b) = binary_text_section_size b
  binary_dir_name (Binary b) = binary_dir_name b
  binary_file_name (Binary b) = binary_file_name b
  fetch_instruction (Binary b) = fetch_instruction b

binary_get_symbol_table bin =
  case binary_get_symbols bin of
    (SymbolTable tbl globals) -> tbl


binary_get_exported_functions bin =
  case binary_get_symbols bin of
    (SymbolTable tbl exports) -> exports

symbol_to_name (PointerToExternalFunction f)   = f
symbol_to_name (PointerToInternalFunction f a) = f
symbol_to_name (PointerToObject l b)           = l
symbol_to_name (AddressOfObject l b)           = l
symbol_to_name (AddressOfLabel f b)            = f
symbol_to_name (Relocated_ResolvedObject l a)  = l



-- | Is the immediate roughly in range to be an address?
is_roughly_an_address ::
  BinaryClass bin => 
     bin     -- ^ The binary
  -> Word64  -- ^ An address
  -> Bool
is_roughly_an_address bin a = 
  let !si = binary_get_sections_info bin in
    a >= si_min_address si && a <= si_max_address si

-- | Find a section for an address (see @`SectionsInfo`@)
find_section_for_address ::
  BinaryClass bin => 
     bin     -- ^ The binary
  -> Word64 -- ^ An address
  -> Maybe (String, String, Word64, Word64,Word64,[SectionFlag])
find_section_for_address bin a =
  if is_roughly_an_address bin a then
    find (address_in_section a) (si_sections $ binary_get_sections_info bin)
  else
    Nothing
 where
  address_in_section a (_,_,a0,si,_,_) = a0 <= a && a < a0 + si



-- | Reading from a read-only data section.
--
-- Reads maximally up to 8 bytes. Returns @Nothing@ if the given address is out-of-range.
read_from_ro_datasection ::
  BinaryClass bin => 
     bin           -- ^ The binary
  -> Word64        -- ^ An address
  -> Int           -- ^ Size, i.e., the number of bytes to read
  -> Maybe Word64
read_from_ro_datasection bin a si = bytes_to_word <$> binary_read_ro_data bin a si





-- | Find a section ending at address (see @`SectionsInfo`@)
find_section_ending_at ::
  BinaryClass bin => 
     bin           -- ^ The binary
   -> Word64       -- ^ An address
   -> Maybe (String, String, Word64, Word64, Word64,[SectionFlag])
find_section_ending_at bin a = find (address_ends_at_section a) (si_sections $ binary_get_sections_info bin)
 where
  address_ends_at_section a (_,_,a0,si,_,_) = a == a0 + si





address_has_instruction ::
  BinaryClass bin => 
     bin
  -> Word64  -- ^ An address
  -> Bool 
address_has_instruction bin a =
  case fetch_instruction bin a of
    Nothing -> False
    _ -> True



undefined_internal_global_labels bin = filter is_global_and_internal_and_outside_of_any_section $ IM.assocs $ binary_get_symbol_table bin
 where
  is_global_and_internal_and_outside_of_any_section (a,sym) = and
    [ is_internal_symbol sym
    -- , symbol_to_name sym `elem` IM.elems (binary_get_global_symbols bin)
    , is_not_defined_elsewhere a ]

  is_not_defined_elsewhere a = (find (is_section_for a) $ si_sections $ binary_get_sections_info bin) == Nothing

  is_section_for a (segment,section,a0,sz,_,_) = a0 <= fromIntegral a && fromIntegral a <= a0+sz

  is_internal_symbol (AddressOfLabel _ False) = True
  is_internal_symbol _                        = False

