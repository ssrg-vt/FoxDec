{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, DeriveGeneric, StrictData, StandaloneDeriving #-}

module Binary.Elf where

import Base

import Binary.Generic

import Data.Binary.Get as G

import Data.Elf
import Data.Symbol

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
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import Data.X86.Instruction

import Control.Monad.State.Strict

import Debug.Trace

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
deriving instance Generic NamedElf

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
instance Cereal.Serialize NamedElf




data NamedElf = NamedElf {
  elf :: !Elf,
  elf_dir_name :: !String,
  elf_file_name :: !String,
  elf_sections_info :: !SectionsInfo,
  elf_symbol_table :: !SymbolTable,
  elf_relocs :: !(S.Set Relocation),
  elf_instructions :: !(IM.IntMap Instruction)
 }



-- | Overview of sections with read only data.
sections_ro_data = [
   ("",".text"),
   ("",".rodata"),
   ("",".got"),
   ("",".got.plt"),
   ("",".plt"),
   ("",".plt.got"),
   ("",".plt.sec"),
   ("",".data.rel.ro"),
   ("",".init_array"),
   ("",".fini_array"),
   ("",".ctors"),
   ("",".dtors"),
   ("","__sancov_guards")
 ]

sections_data = [
   ("",".data")
 ]

sections_bss = [
   ("",".bss")
 ]


sections_text = [
   ("",".text"),
   ("",".init"),
   ("",".fini")
 ]



isRelevantElfSection section = ("",elfSectionName section) `elem` sections_ro_data ++ sections_data ++ sections_bss ++ sections_text

isAllocated section = SHF_ALLOC `elem` elfSectionFlags section



-- reading bytes from sections. 
read_bytes_section a si section = BS.unpack $ BS.take si $ BS.drop (fromIntegral $ a - elfSectionAddr section) $ elfSectionData section

contains_address a section = 
  let a0  = elfSectionAddr section
      si0 = elfSectionSize section in
    a0 <= a && a < a0 + si0


elf_read_bytestring :: Elf -> Word64 -> Int -> Maybe LBS.ByteString
elf_read_bytestring elf a si =
  case find (contains_address a) $ elfSections elf of
    Nothing -> Nothing
    Just section -> Just $ LBS.fromStrict $ BS.take si $ BS.drop (fromIntegral $ a - elfSectionAddr section) $ elfSectionData section
 where
  isRelevant section
    |  ("",elfSectionName section) `elem` sections_ro_data = True
    | otherwise = False




elf_read_ro_data :: Elf -> Word64 -> Int -> Maybe [Word8]
elf_read_ro_data elf a si =
  case filter isRelevant $ filter (contains_address a) $ elfSections elf of
    [] -> Nothing
    [section] -> Just $ read_bytes_section a si section
 where
  isRelevant section
    |  ("",elfSectionName section) `elem` sections_ro_data = True
    | otherwise = False

elf_read_data :: Elf -> Word64 -> Int -> Maybe [Word8]
elf_read_data elf a si =
  case filter isBss $ filter (contains_address a) $ elfSections elf of
    [section] -> Just $ replicate si 0
    [] -> try_read_data
 where 
  try_read_data =
    case filter isData $ filter (contains_address a) $ elfSections elf of
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
   | elfRelType reloc `elem` [8,37] =
     -- R_X86_64_RELATIVE
     -- The SymAddend provides the relocation address
     case elfRelSymAddend reloc of
       Nothing     -> [Relocation (fromIntegral $ elfRelOffset reloc) 0] -- TODO implicit addend?
       Just addend -> [Relocation (fromIntegral $ elfRelOffset reloc) (fromIntegral $ addend)]
   | otherwise = []



elf_get_symbol_table elf = SymbolTable mk_symbols mk_exports
 where
  mk_symbols = IM.fromList $ filter ((/=) "" . symbol_to_name . snd) $ symbols_from_ELF_symbol_tables ++ symbols_from_relocations

  mk_exports = IM.fromList $ map mk_sym_entry $ filter isExportedAndInternallyDefined $ concat $ parseSymbolTables elf
  mk_sym_entry sym_entry = (fromIntegral $ steValue sym_entry, get_string_from_steName $ steName sym_entry)

  dynsym = parseDynSym elf

  isExportedAndInternallyDefined sym_entry = and
    [ steIndex sym_entry /= SHNUndef
    , steBind sym_entry == STBGlobal
    , get_string_from_steName (steName sym_entry) /= ""
    , steType sym_entry /= STTLoOS
    , M.lookup (steName sym_entry) dynsym /= Just (Just "GLIBC_PRIVATE") ]



  -- go through all relocations
  symbols_from_relocations = concatMap mk_symbol_table_for_reloc_section $ parseRelocations elf
  
  mk_symbol_table_for_reloc_section sec = concatMap (try_mk_symbol_entry sec) (elfRelSectRelocations sec)

  try_mk_symbol_entry sec reloc
    | elfRelType reloc `elem` [6,7] =
      -- R_X86_64_GLOB_DAT, objects
      -- R_X86_64_JUMP_SLOT
      -- the RelSymbol provides an index into a lookup table that contains the name of the symbol
      let symbol_table_entry      = (elfRelSectSymbolTable sec) !! (fromIntegral $ elfRelSymbol reloc)
          name_of_reloc_trgt      = get_name_and_inex_from_sym_entry reloc $ symbol_table_entry
          symb_type_of_reloc_trgt = steType $ symbol_table_entry
          value                   = steValue symbol_table_entry 
          reloc_address           = fromIntegral $ elfRelOffset reloc in
        if symb_type_of_reloc_trgt `elem` [STTObject,STTCommon] then
          let symbol_table_entry = (elfRelSectSymbolTable sec) !! (fromIntegral $ elfRelSymbol reloc)
              bind               = steBind symbol_table_entry in
            if any (\sec -> any (\reloc -> elfRelOffset reloc == value) $ elfRelSectRelocations sec) $ parseRelocations elf then
              [(reloc_address, (Relocated_ResolvedObject (fst name_of_reloc_trgt) value))]
            else if not (snd name_of_reloc_trgt) then -- internal object
              [(reloc_address, (Relocated_ResolvedObject (fst name_of_reloc_trgt) value))]  --error $ show name_of_reloc_trgt++ (show reloc) ++ show sym
            else
              [(reloc_address, (uncurry PointerToObject) name_of_reloc_trgt)]
        else 
          if not (snd name_of_reloc_trgt) then -- internal label 
              [(reloc_address, PointerToInternalFunction (fst name_of_reloc_trgt) value)]
          else
            [(reloc_address, PointerToExternalFunction (fst name_of_reloc_trgt))]
   | elfRelType reloc == 5 =
      -- R_X86_64_COPY
      -- the RelSymbol provides an index into a lookup table that contains the name of the symbol
      [(fromIntegral $ elfRelOffset reloc, AddressOfObject (fst $ get_name_and_inex_from_reloc sec reloc) True)]
   | elfRelType reloc == 1 =
      -- R_X86_64_64
      -- the RelSymbol provides an index into a lookup table that contains the name of the symbol
      -- RelSymAddend should be zero
      let symbol_table_entry      = (elfRelSectSymbolTable sec) !! (fromIntegral $ elfRelSymbol reloc)
          name_of_reloc_trgt      = get_name_and_inex_from_sym_entry reloc symbol_table_entry
          symb_type_of_reloc_trgt = steType $ symbol_table_entry
          value                   = steValue symbol_table_entry
          reloc_address           = fromIntegral $ elfRelOffset reloc in
        if symb_type_of_reloc_trgt == STTFunc then
          if not (snd name_of_reloc_trgt) then -- internal label 
            if value == 0 then
              error $ show (reloc,symbol_table_entry,symb_type_of_reloc_trgt) 
            else 
              [(reloc_address, PointerToInternalFunction (fst name_of_reloc_trgt) value)]
          else
            [(reloc_address, PointerToExternalFunction (fst name_of_reloc_trgt))]
        else
          [] -- error $ show (reloc,symbol_table_entry,symb_type_of_reloc_trgt) -- TODO very likely this is exactly the same as elfRelType reloc == 6 (see libc)
   | otherwise = []


  is_symbol_for name sym_entry = (get_string_from_steName $ steName sym_entry) == name

  -- go through all ELF symbol tables
  -- each symbol table entry that has as type STTObject with binding /= Local is considered external and that is not hidden
  -- its value is the address at which a relocation happens
  symbols_from_ELF_symbol_tables = concatMap mk_symbol_entry $ concat $ parseSymbolTables elf

  mk_symbol_entry sym_entry
    -- | is_external_var_symbol_entry sym_entry = [(fromIntegral $ steValue sym_entry, AddressOfLabel (get_string_from_steName $ steName sym_entry) True)]
    | is_internal_symbol_entry sym_entry     = [(fromIntegral $ steValue sym_entry, AddressOfLabel (get_string_from_steName $ steName sym_entry) False)]
    | otherwise = []


  -- external_variables = map mk_symbol_entry $ filter is_external_var_symbol_entry $ concat $ parseSymbolTables elf
  is_external_var_symbol_entry sym_entry = steType sym_entry `elem` [STTObject,STTCommon] && steBind sym_entry `elem` [STBGlobal, STBWeak] && not (isHiddenSymEntry sym_entry)
  
  is_internal_symbol_entry sym_entry = or
    [ steEnclosingSection sym_entry /= Nothing && steType sym_entry `notElem` [STTObject,STTCommon]
    , is_hidden sym_entry ]


  get_name_and_inex_from_reloc sec reloc =
    let sym_entry     = (elfRelSectSymbolTable sec) !! (fromIntegral $ elfRelSymbol reloc) in
      get_name_and_inex_from_sym_entry reloc sym_entry

  get_name_and_inex_from_sym_entry reloc sym_entry =
    let name          = get_string_from_steName $ steName sym_entry
        where_defined = steIndex sym_entry
        is_external   = case where_defined of
                          SHNUndef   -> True
                          SHNIndex _ -> False
    in
      (name,is_external)

  get_symbol_type_of_reloc sec reloc = steType $ (elfRelSectSymbolTable sec) !! (fromIntegral $ elfRelSymbol reloc)

  isHiddenSymEntry sym_entry = steOther sym_entry .&. 0x3 == 0x2
  is_hidden sym_entry = steType sym_entry `elem` [STTObject,STTCommon] && steBind sym_entry `elem` [STBGlobal, STBWeak] && isHiddenSymEntry sym_entry

  

-- get the name from a symbol table entry
get_string_from_steName (_, Just name) = T.unpack $ T.decodeUtf8 name
get_string_from_steName _ = ""

elf_min_address elf = minimum $ map elfSectionAddr $ filter isRelevantElfSection $ elfSections elf

elf_max_address elf = maximum $ map get_max_address $ filter isRelevantElfSection $ elfSections elf
 where
  get_max_address section = elfSectionAddr section + elfSectionSize section - 1


elf_read_file = parseElf


pp_elf_section section = "[" ++ intercalate ", " [elfSectionName section, show $ elfSectionType section, showHex (elfSectionAddr section), showHex (elfSectionSize section), showHex (elfSectionAddrAlign section)] ++ "]" 
pp_elf elf = intercalate "\n" $ pp_sections ++ pp_boundaries ++ pp_symbols ++ pp_relocs ++ pp_all_relocs ++ pp_all_symbols ++ pp_type ++ pp_entry 
 where
  pp_sections = map pp_elf_section $ elfSections elf
  pp_boundaries = ["Address range: " ++ showHex (elf_min_address elf) ++ " --> " ++ showHex (elf_max_address elf)]
  pp_symbols = ["Symbol table:\n" ++ show (elf_get_symbol_table elf)] 
  pp_relocs = ["Relocations:\n" ++ show (elf_get_relocs elf)] 


  pp_all_relocs  = "Complete relocation list:" : map show (concatMap elfRelSectRelocations $ parseRelocations elf)
  pp_all_symbols = "Complete symbol table:" : map (show_symbol_entry parse_dynsym) (zip [0..] $ concat $ parseSymbolTables elf)
  show_symbol_entry dynsym (ind,sym_entry) = intercalate "; " [ show ind, show (steName sym_entry), show (steType sym_entry) , show $ steBind sym_entry, show $ steOther sym_entry, showHex (steValue sym_entry), show $ steIndex sym_entry, show_dynsym $ M.lookup (steName sym_entry) dynsym]

  parse_dynsym = parseDynSym elf
  show_dynsym (Just (Just str)) = str
  show_dynsym _ = ""

  pp_type = ["Type: " ++ (show $ elfType elf)]
  pp_entry = ["Entry: " ++ (showHex $ elfEntry elf)]
  

elf_get_sections_info elf = SectionsInfo (map mk_section_info $ filter isRelevantElfSection $ elfSections elf) (elf_min_address elf) (elf_max_address elf)
 where
  mk_section_info section = ("",elfSectionName section,elfSectionAddr section,elfSectionSize section, elfSectionAddrAlign section, map mk_flag $ elfSectionFlags section)

  mk_flag SHF_WRITE     = SectionIsWritable
  mk_flag SHF_ALLOC     = SectionIsAllocated
  mk_flag SHF_EXECINSTR = SectionIsExecutable
  mk_flag (SHF_EXT i)   = (SectionHasFlag i)



elf_text_section_size = sum . map (fromIntegral . elfSectionSize) . filter isTextSection . elfSections
 where
  isTextSection sec = ("",elfSectionName sec) `elem` sections_text


elf_get_entry_points elf = [elfEntry elf] ++ entry_of ".init" elf ++ entry_of ".fini" elf
 where
  entry_of name elf =
    case find (\sec -> elfSectionName sec == name) $ elfSections elf of
      Just s -> [elfSectionAddr s]
      _ -> []

instance BinaryClass NamedElf 
  where
    binary_read_bytestring = \(NamedElf elf _ _ _ _ _ _) -> elf_read_bytestring elf
    binary_read_ro_data = \(NamedElf elf _ _ _ _ _ _) -> elf_read_ro_data elf
    binary_read_data = \(NamedElf elf _ _ _ _ _ _) -> elf_read_data elf
    binary_get_sections_info = \(NamedElf elf _ _ si _ _ _) -> si
    binary_get_symbols = \(NamedElf elf _ _ _ t _ _) -> t
    binary_get_relocations = \(NamedElf elf _ _ _ _ r _) -> r
    binary_pp = \(NamedElf elf _ _ _ _ _ _) -> pp_elf elf
    binary_entry = \(NamedElf elf _ _ _ _ _ _) -> elf_get_entry_points elf
    binary_text_section_size = \(NamedElf elf _ _ _ _ _ _) -> elf_text_section_size elf
    binary_dir_name = \(NamedElf _ d _ _ _ _ _) -> d
    binary_file_name = \(NamedElf _ _ n _ _ _ _) -> n
    fetch_instruction = \(NamedElf _ _ _ _ _ _ is) a -> IM.lookup (fromIntegral a) is




-- | Information on sections
-- TODO: get from Binary interface
is_ro_data_section ("",".rodata",_,_,_,_) = True
is_ro_data_section ("",".got",_,_,_,_) = True
is_ro_data_section ("",".init_array",_,_,_,_) = True
is_ro_data_section ("",".fini_array",_,_,_,_) = True
is_ro_data_section ("",".ctors",_,_,_,_) = True
is_ro_data_section ("",".dtors",_,_,_,_) = True
is_ro_data_section ("",".data.rel.ro",_,_,_,_) = True
is_ro_data_section ("","__sancov_guards",_,_,_,_) = True
is_ro_data_section ("__DATA","__const",_,_,_,_) = True
is_ro_data_section _ = False

is_data_section ("__DATA","__data",_,_,_,_) = True
is_data_section ("",".data",_,_,_,_) = True
is_data_section _ = False

is_bss_data_section ("__DATA","__bss",_,_,_,_) = True
is_bss_data_section ("__DATA","__common",_,_,_,_) = True
is_bss_data_section ("",".bss",_,_,_,_) = True
is_bss_data_section _ = False






-- https://refspecs.linuxfoundation.org/LSB_3.0.0/LSB-PDA/LSB-PDA.junk/symversion.html
--
data GNU_Version_d = GNU_Version_d {
  vd_version :: [Word8],
  vd_flags :: [Word8],
  vd_ndx :: [Word8],
  vd_cnt :: [Word8],
  vd_hash :: [Word8],
  vda_name :: [Word8],
  vda_next :: [Word8]
 }

parse_gnu_version_d elf =
  case filter (\s -> elfSectionName s == ".gnu.version_d") (elfSections elf) of
    [] -> []
    [sec] -> evalState get_symbols (elfSectionData sec)
 where
  get_symbols = do
    bs <- get
    if BS.length bs < entry_size then
      return []
    else do
      vd_version <- consume 2
      vd_flags <- consume 2
      vd_ndx <- consume 2
      vd_cnt <- consume 2
      vd_hash <- consume 4
      vd_aux <- consume 4

      let vd_aux_value = fromIntegral $ bytes_to_word $ vd_aux
      let vda_name = BS.unpack $ BS.take 4 $ BS.drop vd_aux_value bs
      let vda_next = BS.unpack $ BS.take 4 $ BS.drop (vd_aux_value+4) bs

      vd_next <- consume 4
      let vd_next_value = fromIntegral $ bytes_to_word $ vd_next
      rest <- if vd_next_value /= 0 then do
                _ <- consume $ vd_next_value - 20
                get_symbols
              else
                return []
      return $ GNU_Version_d vd_version vd_flags vd_ndx vd_cnt vd_hash vda_name vda_next : rest
      
  consume n = do
    (read,rest) <- gets $ BS.splitAt n
    put rest
    return $ BS.unpack read

  -- TODO for 64 bit ELF only
  entry_size = 2+2+2+2+4+4+4

parse_gnu_version elf =
  case filter (\s -> elfSectionName s == ".gnu.version") (elfSections elf) of
    [] -> []
    [gnu_version] -> split_bytestring (elfSectionData gnu_version) (fromIntegral $ elfSectionEntSize gnu_version)





-- the .dynsym section contains entries of size $elfSectionEntSize dnysym$ (in bytes)
-- the .gnu_version contains entries of size $elfSectionEntSize gnu_version$ (in bytes)
--    gnu_version_entry == 0 --> The symbol is local, not available outside the object.
--    gnu_version_entry == 1 --> The symbol is defined in this object and is globally available.
--
--    Result maps steName from symbols to version strings
parseDynSym :: Elf -> M.Map (Word32,Maybe BS.ByteString) (Maybe String)
parseDynSym elf =
  let [dynsym]            = filter ((`elem` [SHT_DYNSYM]) . elfSectionType) (elfSections elf)
      gnu_version_entries = parse_gnu_version elf
      gnu_version_count   = length gnu_version_entries 
      [dyn_sym_symbols]   = filter (\symbols -> length symbols == gnu_version_count) $ parseSymbolTables elf
      gnu_version_d       = parse_gnu_version_d elf
      symbols             = zip dyn_sym_symbols gnu_version_entries
  in
    M.fromList $ map (mk_symbol gnu_version_d) symbols
    --error $ (intercalate "\n" $ map (show_symbol gnu_version_d) symbols) ++ "\n\n" ++ intercalate "\n"  (map show_gnu_version_d_entry gnu_version_d)
 where
  mk_symbol gnu_version_d (sym,gnu_version) = 
    let gnu_version_value   = (fromIntegral $ bytes_to_word $ [BS.head $ gnu_version]) - 1
        lookup_vda_name     = vda_name <$> (gnu_version_d !? gnu_version_value)
        vda_name_index      = (fromIntegral . bytes_to_word) <$> lookup_vda_name
        vda_symbol          = find_symbol_by_index elf =<< vda_name_index in
      --if gnu_version_value == 1 then
      --  error $ show (steName sym, steName <$> vda_symbol, BS.last $ gnu_version)
      --else
      (steName sym, (get_string_from_steName . steName) <$> vda_symbol)



find_symbol_by_index elf index = find symbol_has_index $ concat $ parseSymbolTables elf
 where
  symbol_has_index sym = (fst $ steName sym) == index
 

split_bytestring bs n
  | BS.null bs = []
  | otherwise = 
    case BS.splitAt n bs of
      (bs0,bs1) -> bs0 : split_bytestring bs1 n




{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
