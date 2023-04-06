{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict, DeriveGeneric, StandaloneDeriving #-}

module Instantiation.BinaryElf (elf_read_file) where

import Base

import Generic.Binary

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


sections_text = [
   ("",".text")
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
  case filter isBss $ filter (contains_address a si) $ elfSections elf of
    [section] -> Just $ replicate si 0
    [] -> try_read_data
 where 
  try_read_data =
    case filter isData $ filter (contains_address a si) $ elfSections elf of
      [] -> Nothing
      [section] -> Just $ read_bytes_section a si section

  isData section 
    | ("",elfSectionName section) `elem` sections_data = True
    | otherwise = False
  isBss section 
    | ("",elfSectionName section) `elem` sections_bss = True
    | otherwise = False



elf_get_relocs elf = S.fromList $ mk_relocs
 where
  -- go through all relocations
  mk_relocs = concatMap mk_reloc $ parseRelocations elf
  
  mk_reloc sec = concatMap (try_mk_reloc sec) (elfRelSectRelocations sec)

  try_mk_reloc sec reloc
   | elfRelType reloc == 8 =
     -- R_X86_64_RELATIVE
     -- The SymAddend provides the relocation address
      [Relocation (fromIntegral $ elfRelOffset reloc) (fromIntegral $ fromJust $ elfRelSymAddend reloc)]
   | otherwise = []


elf_get_symbol_table elf = SymbolTable $ IM.fromList $ filter ((/=) (Just "") . symbol_to_name . snd) $ symbols_from_ELF_symbol_tables ++ symbols_from_relocations
 where
  -- go through all relocations
  symbols_from_relocations = concatMap mk_symbol_table_for_reloc_section $ parseRelocations elf
  
  mk_symbol_table_for_reloc_section sec = concatMap (try_mk_symbol_entry sec) (elfRelSectRelocations sec)

  try_mk_symbol_entry sec reloc
    | elfRelType reloc == 7 =
      -- R_X86_64_JUMP_SLOT
      -- the RelSymbol provides an index into a lookup table that contains the name of the symbol
      [(fromIntegral $ elfRelOffset reloc, Relocated_Function $ get_name_from_reloc sec reloc)]
    | elfRelType reloc == 6 =
      -- R_X86_64_GLOB_DAT, objects
      -- the RelSymbol provides an index into a lookup table that contains the name of the symbol
      let typ = if get_symbol_type_of_reloc sec reloc `elem` [STTObject,STTCommon] then Relocated_Label else Relocated_Function in
        [(fromIntegral $ elfRelOffset reloc, typ $ get_name_from_reloc sec reloc)]
   | elfRelType reloc == 5 =
      -- R_X86_64_COPY
      -- the RelSymbol provides an index into a lookup table that contains the name of the symbol
      [(fromIntegral $ elfRelOffset reloc, Relocated_Label $ get_name_from_reloc sec reloc)]
   | otherwise = []


  -- go through all ELF symbol tables
  -- each symbol table entry that has as type STTObject with binding /= Local is considered external and that is not hidden
  -- its value is the address at which a relocation happens
  symbols_from_ELF_symbol_tables = concatMap mk_symbol_entry $ concat $ parseSymbolTables elf

  mk_symbol_entry sym_entry
    --   | is_external_var_symbol_entry sym_entry = [(fromIntegral $ steValue sym_entry, Relocated_Label $ get_string_from_steName $ steName sym_entry)]
    | is_internal_symbol_entry sym_entry     = [(fromIntegral $ steValue sym_entry, Internal_Label $ get_string_from_steName $ steName sym_entry)]
    | otherwise = []


  -- external_variables = map mk_symbol_entry $ filter is_external_var_symbol_entry $ concat $ parseSymbolTables elf
  is_external_var_symbol_entry sym_entry = steType sym_entry `elem` [STTObject,STTCommon] && steBind sym_entry `elem` [STBGlobal, STBWeak] && not (isHiddenSymEntry sym_entry)
  
  is_internal_symbol_entry sym_entry = or
    [ steEnclosingSection sym_entry /= Nothing && steType sym_entry `notElem` [STTObject,STTCommon]
    , is_hidden sym_entry ]


  get_name_from_reloc sec reloc = get_string_from_steName $ steName $ (elfRelSectSymbolTable sec) !! (fromIntegral $ elfRelSymbol reloc)

  get_symbol_type_of_reloc sec reloc = steType $ (elfRelSectSymbolTable sec) !! (fromIntegral $ elfRelSymbol reloc)

  isHiddenSymEntry sym_entry = steOther sym_entry .&. 0x3 == 0x2
  is_hidden sym_entry = steType sym_entry `elem` [STTObject,STTCommon] && steBind sym_entry `elem` [STBGlobal, STBWeak] && isHiddenSymEntry sym_entry

  
{--
  plt_relocations = concatMap mk_entries_relocated_function $ parseRelocations elf
  -- for each such section, filter out relocations with type 7 (R_X86_64_JUMP_SLOT)
  mk_entries_relocated_function sec = 
    let relocs = filter (((==) 7) . elfRelType) $ elfRelSectRelocations sec in
      filter ((/=) "") $ map (mk_entry_relocated_function $ elfRelSectSymbolTable sec) relocs 

  mk_entry_relocated_function table sym_reloc = (fromIntegral $ elfRelOffset sym_reloc, Relocated_Function $ get_name_from_reloc table sym_reloc)

  external_variables = concatMap mk_entries_external_variable $ parseRelocations elf
  -- for each such section, filter out relocations with type 6 (R_X86_64_GLOB_DAT)
  -- for each such relocation, the offset is the address at which a relocation happens
  -- the RelSymbol provides an index into a lookup table that contains the name of the symbol
  mk_entries_external_variable sec = 
    let relocs = filter (((==) 6) . elfRelType) $ elfRelSectRelocations sec in
      filter ((/=) "") $ map (mk_entry_relocated_function $ elfRelSectSymbolTable sec) relocs 

  mk_entry_relocated_function table sym_reloc = (fromIntegral $ elfRelOffset sym_reloc, Relocated_Label $ get_name_from_reloc table sym_reloc)





  -- go through all symbol tables
  -- each symbol table entry that has as type STTObject with binding /= Local is considered external and that is not hidden
  -- its value is the address at which a relocation happens
  external_variables = map mk_symbol_entry $ filter is_external_var_symbol_entry $ concat $ parseSymbolTables elf
  is_external_var_symbol_entry sym_entry = steType sym_entry == STTObject && steBind sym_entry `elem` [STBGlobal, STBWeak] && not (isHiddenSymEntry sym_entry)
  mk_symbol_entry sym_entry = (fromIntegral $ steValue sym_entry, get_string_from_steName $ steName sym_entry)




-- retrieve a mapping of addresses to symbols (internal)
elf_get_symbol_table_internal elf = IM.filter ((/=) "") $ IM.fromList $ internal_symbols 
 where
  -- this is how to find internal symbols:
  -- go through all symbol tables
  internal_symbols = map mk_symbol_entry $ filter is_internal_symbol_entry $ concat $ parseSymbolTables elf
  -- each symbol table entry that has a section associated to it is internal
  -- its value is the the address at which a relocation happens
  -- hidden symbols are considered inernal as well
  is_internal_symbol_entry sym_entry = or
    [ steEnclosingSection sym_entry /= Nothing && steType sym_entry /= STTObject
    , is_hidden sym_entry ]
  mk_symbol_entry sym_entry = (fromIntegral $ steValue sym_entry, get_string_from_steName $ steName sym_entry)
  
  is_hidden sym_entry = steType sym_entry == STTObject && steBind sym_entry `elem` [STBGlobal, STBWeak] && isHiddenSymEntry sym_entry
--}


-- get the name from a symbol table entry
get_string_from_steName (_, Just name) = T.unpack $ T.decodeUtf8 name
get_string_from_steName _ = ""

elf_min_address elf = minimum $ map elfSectionAddr $ filter isRelevantElfSection $ elfSections elf

elf_max_address elf = maximum $ map get_max_address $ filter isRelevantElfSection $ elfSections elf
 where
  get_max_address section = elfSectionAddr section + elfSectionSize section - 1


elf_read_file = parseElf


pp_elf_section section = "[" ++ intercalate ", " [elfSectionName section, show $ elfSectionType section, showHex (elfSectionAddr section), showHex (elfSectionSize section)] ++ "]" 
pp_elf elf = intercalate "\n" $ pp_sections ++ pp_boundaries ++ pp_symbols ++ pp_relocs ++ pp_all_relocs ++ pp_all_symbols ++ pp_entry
 where
  pp_sections = map pp_elf_section $ elfSections elf
  pp_boundaries = ["Address range: " ++ showHex (elf_min_address elf) ++ " --> " ++ showHex (elf_max_address elf)]
  pp_symbols = ["Symbol table:\n" ++ show (elf_get_symbol_table elf)] 
  pp_relocs = ["Relocations:\n" ++ show (elf_get_relocs elf)] 


  pp_all_relocs  = "Complete relocation list:" : map show (concatMap elfRelSectRelocations $ parseRelocations elf)
  pp_all_symbols = "Complete symbol table:" : map show_symbol_entry (concat $ parseSymbolTables elf)
  show_symbol_entry sym_entry = intercalate "; " [ show (steName sym_entry), show (steType sym_entry) , showHex (steValue sym_entry), show $ steIndex sym_entry, show $ steBind sym_entry ]


  pp_entry = ["Entry: " ++ (showHex $ elfEntry elf)]
  

elf_get_sections_info elf = SectionsInfo (map mk_section_info $ filter isRelevantElfSection $ elfSections elf) (elf_min_address elf) (elf_max_address elf)
 where
  mk_section_info section = ("",elfSectionName section,elfSectionAddr section,elfSectionSize section)



elf_text_section_size = sum . map (fromIntegral . elfSectionSize) . filter isTextSection . elfSections
 where
  isTextSection sec = ("",elfSectionName sec) `elem` sections_text

instance BinaryClass Elf 
  where
    binary_read_ro_data = elf_read_ro_data
    binary_read_data = elf_read_data
    binary_get_sections_info = elf_get_sections_info
    binary_get_symbols = elf_get_symbol_table
    binary_get_relocations = elf_get_relocs
    binary_pp = pp_elf
    binary_entry = elfEntry
    binary_text_section_size = elf_text_section_size

