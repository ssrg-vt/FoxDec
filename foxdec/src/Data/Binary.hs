{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict, StandaloneDeriving, DeriveGeneric, ExistentialQuantification #-}

module Data.Binary where

import Base


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Word 

import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)


-- |  Information on the sections in the binary
data SectionsInfo = SectionsInfo {
  si_sections    :: [(String,String,Word64,Word64)], -- ^ Sections: segment names, section names, addresses and sizes.
  si_min_address :: Word64,
  si_max_address :: Word64
 }
 deriving (Show,Generic,Eq)


-- | Relocations
data Relocation = R_X86_64_RELATIVE Word64 Word64 -- ^ At address a0, address a1 has been written
 deriving (Show,Generic,Eq,Ord)


pp_reloc (R_X86_64_RELATIVE a0 a1) = showHex a0 ++ " --> " ++ showHex a1


-- | Symbol Table
type SymbolTable = IM.IntMap String



class BinaryClass a where
  binary_read_ro_data :: a -> Word64 -> Int -> Maybe [Word8]
  binary_read_data :: a -> Word64 -> Int -> Maybe [Word8]
  binary_get_sections_info :: a -> SectionsInfo
  binary_get_relocs :: a -> [Relocation]
  binary_get_symbols :: a -> SymbolTable
  binary_pp :: a -> String
  binary_entry :: a -> Word64



data Binary = forall b . BinaryClass b => Binary b


instance BinaryClass Binary where
  binary_read_ro_data (Binary b) = binary_read_ro_data b
  binary_read_data (Binary b) = binary_read_data b
  binary_get_sections_info (Binary b) = binary_get_sections_info b
  binary_get_relocs (Binary b) = binary_get_relocs b
  binary_get_symbols (Binary b) = binary_get_symbols b
  binary_pp (Binary b) = binary_pp b
  binary_entry (Binary b) = binary_entry b




