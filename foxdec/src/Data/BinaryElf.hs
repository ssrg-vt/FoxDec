{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict, DeriveGeneric, StandaloneDeriving #-}

module Data.BinaryElf (elf_read_file) where

import Base
import Data.Binary
import Data.Elf

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Word 
import Data.List
import Data.Bits
import Data.Maybe (fromJust)
import Data.List.Extra (firstJust)
import qualified Data.ByteString as BS

import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)

import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

deriving instance Generic ElfMachine
deriving instance Generic ElfSegmentType
deriving instance Generic ElfSegmentFlag
deriving instance Generic ElfSegment
deriving instance Generic ElfType
deriving instance Generic ElfOSABI
deriving instance Generic ElfClass
deriving instance Generic ElfData
deriving instance Generic ElfSectionFlags
deriving instance Generic ElfSectionType
deriving instance Generic ElfSection
deriving instance Generic Elf

instance Cereal.Serialize ElfMachine
instance Cereal.Serialize ElfSegmentType
instance Cereal.Serialize ElfSegmentFlag
instance Cereal.Serialize ElfSegment
instance Cereal.Serialize ElfType
instance Cereal.Serialize ElfOSABI
instance Cereal.Serialize ElfClass
instance Cereal.Serialize ElfData
instance Cereal.Serialize ElfSectionFlags
instance Cereal.Serialize ElfSectionType
instance Cereal.Serialize ElfSection
instance Cereal.Serialize Elf




-- | Overview of sections with read only data.
sections_ro_data = [
   ("",".text"),
   ("",".init"),
   ("",".fini"),
   ("",".rodata"),
   ("",".plt"),
   ("",".plt.got"),
   ("",".plt.sec"),
   ("",".data.rel.ro"),
   ("",".init_array"),
   ("",".fini_array")
 ]

sections_data = [
   ("",".data")
 ]

sections_bss = [
   ("",".bss")
 ]

isRelevantElfSection section = ("",elfSectionName section) `elem` sections_ro_data ++ sections_data ++ sections_bss

isAllocated section = SHF_ALLOC `elem` elfSectionFlags section



-- reading bytes from sections. 
read_bytes_section a si section = BS.unpack $ BS.take si $ BS.drop (fromIntegral $ a - elfSectionAddr section) $ elfSectionData section

contains_address a si section = 
  let a0  = elfSectionAddr section
      si0 = elfSectionSize section in
    a0 <= a && a < a0 + si0

elf_read_ro_data :: Elf -> Word64 -> Int -> Maybe [Word8]
elf_read_ro_data elf a si =
  case filter isRelevant $ filter (contains_address a si) $ elfSections elf of
    [] -> Nothing
    [section] -> Just $ read_bytes_section a si section
 where
  isRelevant section
    |  ("",elfSectionName section) `elem` sections_ro_data = True
    | otherwise = False

elf_read_data :: Elf -> Word64 -> Int -> Maybe [Word8]
elf_read_data elf a si =
  case filter isRelevant $ filter (contains_address a si) $ elfSections elf of
    [] -> Nothing
    [section] -> Just $ read_bytes_section a si section
 where
  isRelevant section 
    | ("",elfSectionName section) `elem` sections_data = True
    | otherwise = False



elf_get_relocs elf = map mk_reloc $ filter is_reloc $ concatMap elfRelSectRelocations $ parseRelocations elf
 where
  is_reloc rel = elfRelType rel == 8 -- R_X86_64_RELATIVE
  mk_reloc rel = R_X86_64_RELATIVE (elfRelOffset rel) (fromIntegral $ fromJust $ elfRelSymAddend rel)


-- TODO: perhaps R_X86_64_GLOB_DAT entries should be part of the relocs, not the symbol table




-- retrieve a mapping of addresses to symbols (external)
-- E.g:
-- 		0xcfe0 --> malloc
-- Means that reading 8 bytes from address 0xcfe0 procudes a pointer to malloc.
-- Thus an instruction: "CALL qword ptr [0xcfe0]" can be seen as "CALL malloc".
--
-- Similarly, global external variables are found, such as "stdout" or "optind".
-- E.g.:
--    0xd0a8 --> stdout
-- Means that "mov rdi,QWORD PTR [0xd0a8]" can be seen as "mov rdi, QWORD PTR [stdout]"
elf_get_symbol_table elf = IM.filter ((/=) "") $ IM.fromList $ plt_relocations  ++ external_variables
 where
  -- go through all section that contain relocations
  plt_relocations = concatMap mk_symbol_from_reloc_section $ parseRelocations elf
  -- for each such section, filter out relocations with type 6 (R_X86_64_GLOB_DAT) or 7 (R_X86_64_JUMP_SLOT)
  -- for each such relocation, the offset is the address at which a relocation happens
  -- the RelSymbol provides an index into a lookup table that contains the name of the symbol
  mk_symbol_from_reloc_section sec = 
    let relocs = filter isJumptTableRelocType $ elfRelSectRelocations sec in
      map (mk_symbol_from_reloc $ elfRelSectSymbolTable sec) relocs 
  isJumptTableRelocType = flip elem [6,7] . elfRelType 
  mk_symbol_from_reloc table sym_reloc = (fromIntegral $ elfRelOffset sym_reloc, get_string_from_steName $ steName $ table !! (fromIntegral $ elfRelSymbol sym_reloc))

  -- go through all symbol tables
  -- each symbol table entry that has as type STTObject with binding /= Local is considered external and that is not hidden
  -- its value is the address at which a relocation happens
  external_variables = map mk_symbol_entry $ filter is_external_var_symbol_entry $ concat $ parseSymbolTables elf
  is_external_var_symbol_entry sym_entry = steType sym_entry == STTObject && steBind sym_entry `elem` [STBGlobal, STBWeak] && not (isHiddenSymEntry sym_entry)
  mk_symbol_entry sym_entry = (fromIntegral $ steValue sym_entry, get_string_from_steName $ steName sym_entry)



isHiddenSymEntry sym_entry = steOther sym_entry .&. 0x3 == 0x2

-- retrieve a mapping of addresses to symbols (internal)
elf_get_symbol_table_internal elf = IM.filter ((/=) "") $ IM.fromList $ internal_symbols 
 where
  -- this is how to find internal symbols:
  -- go through all symbol tables
  internal_symbols = map mk_symbol_entry $ filter is_internal_symbol_entry $ concat $ parseSymbolTables elf
  -- each symbol table entry that has a section associated to it is internal
  -- its value is the the address at which a relocation happens
  is_internal_symbol_entry sym_entry = steEnclosingSection sym_entry /= Nothing && steType sym_entry /= STTObject
  mk_symbol_entry sym_entry = (fromIntegral $ steValue sym_entry, get_string_from_steName $ steName sym_entry)
  


-- get the name from a symbol table entry
get_string_from_steName (_, Just name) = T.unpack $ T.decodeUtf8 name
get_string_from_steName _ = ""

elf_min_address elf = minimum $ map elfSectionAddr $ filter isRelevantElfSection $ elfSections elf

elf_max_address elf = maximum $ map get_max_address $ filter isRelevantElfSection $ elfSections elf
 where
  get_max_address section = elfSectionAddr section + elfSectionSize section - 1


elf_read_file = parseElf


pp_elf_section section = "[" ++ intercalate ", " [elfSectionName section, show $ elfSectionType section, showHex (elfSectionAddr section), showHex (elfSectionSize section)] ++ "]" 
pp_elf elf = intercalate "\n" $ pp_sections ++ pp_boundaries ++ pp_relocs ++ pp_symbols ++ pp_symbols_internal ++ pp_all_relocs ++ pp_all_symbols ++ pp_entry
 where
  pp_sections = map pp_elf_section $ elfSections elf
  pp_boundaries = ["Address range: " ++ showHex (elf_min_address elf) ++ " --> " ++ showHex (elf_max_address elf)]
  pp_relocs = map  pp_reloc $ elf_get_relocs elf
  pp_symbols = ["Symbol table:\n" ++ show (elf_get_symbol_table elf)] 
  pp_symbols_internal = ["Symbol table (internal):\n" ++ show (elf_get_symbol_table_internal elf)]


  pp_all_relocs  = "Complete relocation list:" : map show (concatMap elfRelSectRelocations $ parseRelocations elf)
  pp_all_symbols = "Complete symbol table:" : map show_symbol_entry (concat $ parseSymbolTables elf)
  show_symbol_entry sym_entry = intercalate "; " [ show (steName sym_entry), show (steType sym_entry) , showHex (steValue sym_entry), show $ steIndex sym_entry, show $ steBind sym_entry ]


  pp_entry = ["Entry: " ++ (showHex $ elfEntry elf)]
  

elf_get_sections_info elf = SectionsInfo (map mk_section_info $ filter isRelevantElfSection $ elfSections elf) (elf_min_address elf) (elf_max_address elf)
 where
  mk_section_info section = ("",elfSectionName section,elfSectionAddr section,elfSectionSize section)

instance BinaryClass Elf 
  where
    binary_read_ro_data = elf_read_ro_data
    binary_read_data = elf_read_data
    binary_get_sections_info = elf_get_sections_info
    binary_get_relocs = elf_get_relocs
    binary_get_symbols = elf_get_symbol_table
    binary_pp = pp_elf
    binary_entry = elfEntry

