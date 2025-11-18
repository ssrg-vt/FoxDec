{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures , FlexibleContexts, DeriveGeneric, StrictData, StandaloneDeriving #-}

module Binary.Elf where

import Base

import Binary.Generic
import Binary.Disassemble
import qualified Binary.ELLF as ELLF
import Parser.ByteStringReader

import Data.Binary.Get as G

import Data.Elf
import Data.Symbol

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Int
import Data.Word 
import Data.List
import Data.Bits
import Data.Maybe (fromJust,mapMaybe)
import Data.List.Extra (firstJust)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS (fromString)

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
  elf_signs :: M.Map String FunctionSignature
 }



-- | Overview of sections with read only data.
{--
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
   ("","__sancov_guards"),
   ("","__libc_IO_vtables")
 ]--}



sections_bss = [
   ("",".bss")
 ]



isRelevantElfSection section = isExecutable section || is_ro_data_section ("",elfSectionName section) || is_data_section ("",elfSectionName section) || ("",elfSectionName section) `elem` sections_bss 

isAllocated section = SHF_ALLOC `elem` elfSectionFlags section


-- reading bytes from sections. 
read_bytes_section a si section = BS.unpack $ BS.take si $ BS.drop (fromIntegral $ a - elfSectionAddr section) $ elfSectionData section



elf_read_bytestring :: Elf -> Word64 -> Int -> Maybe LBS.ByteString
elf_read_bytestring elf a si =
  case find (contains_address a) $ elfSections elf of
    Nothing -> Nothing
    Just section -> Just $ LBS.fromStrict $ BS.take si $ BS.drop (fromIntegral $ a - elfSectionAddr section) $ elfSectionData section




elf_read_ro_data :: Elf -> Word64 -> Int -> Maybe [Word8]
elf_read_ro_data elf a si =
  case filter isRelevant $ filter (contains_address a) $ elfSections elf of
    [] -> Nothing
    [section] -> Just $ read_bytes_section a si section
 where
  isRelevant section = is_ro_data_section ("",elfSectionName section)

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
    | is_data_section ("",elfSectionName section) = True
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

  mk_exports = map mk_sym_entry $ filter isExportedAndInternallyDefined $ concat $ parseSymbolTables elf
  mk_sym_entry sym_entry = (fromIntegral $ steValue sym_entry, get_string_from_steName $ steName sym_entry)

  dynsym = parseDynSym elf

  isExportedAndInternallyDefined sym_entry = and
    [ steValue sym_entry /= 0
    , steIndex sym_entry /= SHNUndef
    , steBind sym_entry `elem` [ STBGlobal, STBWeak ]
    , get_string_from_steName (steName sym_entry) /= ""
    , steType sym_entry `notElem` [STTLoOS, STTHiOS, STTObject]
    , M.lookup (steName sym_entry) dynsym /= Just (Just "GLIBC_PRIVATE") ]



  -- go through all relocations
  symbols_from_relocations = concatMap mk_symbol_table_for_reloc_section $ parseRelocations elf
  
  mk_symbol_table_for_reloc_section sec = concatMap (try_mk_symbol_entry sec) (elfRelSectRelocations sec)

  try_mk_symbol_entry sec reloc
    | elfRelType reloc `elem` [6,7,1] =
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
          error $ show (reloc,symbol_table_entry,symb_type_of_reloc_trgt) -- TODO very likely this is exactly the same as elfRelType reloc == 6 (see libc)
    | elfRelType reloc `elem` [18] = 
      -- R_X86_64_TPOFF64
      let symbol_table_entry      = (elfRelSectSymbolTable sec) !! (fromIntegral $ elfRelSymbol reloc)
          name_of_reloc_trgt      = get_name_and_inex_from_sym_entry reloc symbol_table_entry
          reloc_address           = fromIntegral $ elfRelOffset reloc in
      [(reloc_address, TLS_Relative (fst name_of_reloc_trgt))]
   | otherwise = []


  is_symbol_for name sym_entry = (get_string_from_steName $ steName sym_entry) == name

  -- go through all ELF symbol tables
  -- each symbol table entry that has as type STTObject with binding /= Local is considered external and that is not hidden
  -- its value is the address at which a relocation happens
  symbols_from_ELF_symbol_tables = concatMap mk_symbol_entry $ concat $ parseSymbolTables elf

  mk_symbol_entry sym_entry
    -- | is_external_var_symbol_entry sym_entry = [(fromIntegral $ steValue sym_entry, AddressOfLabel (get_string_from_steName $ steName sym_entry) True)]
    | is_ifunc_symbol_entry sym_entry      = [(fromIntegral $ steValue sym_entry, AddressOfLabel (get_string_from_steName $ steName sym_entry) True)]
    | is_internal_symbol_entry sym_entry   = [(fromIntegral $ steValue sym_entry, AddressOfLabel (get_string_from_steName $ steName sym_entry) False)]
    | otherwise = []


  is_ifunc_symbol_entry sym_entry = steType sym_entry `elem` [STTLoOS, STTHiOS] 

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
pp_elf elf = intercalate "\n" $ pp_sections ++ pp_boundaries ++ pp_symbols ++ pp_relocs ++ pp_all_relocs ++ pp_all_symbols ++ pp_type ++ pp_entry -- ++ pp_ehframe (parse_ehframe elf show)
 where
  pp_sections = map pp_elf_section $ elfSections elf
  pp_boundaries = ["Address range: " ++ showHex (elf_min_address elf) ++ " --> " ++ showHex (elf_max_address elf)]
  pp_symbols = ["Symbol table:\n" ++ show (elf_get_symbol_table elf)] 
  pp_relocs = ["Relocations:\n" ++ show (elf_get_relocs elf)] 
  pp_ehframe (directives,tables,_) = ["CFI directives:"] ++ concatMap pp_directive (IM.toList directives) ++ IM.elems tables

  pp_directive (a,d) = ["0x" ++ showHex a ++ ":"] ++ d


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


elf_text_section_size = sum . map (fromIntegral . elfSectionSize) . filter isExecutable . elfSections


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
    binary_get_needed_libs = \(NamedElf elf _ _ _ _ _ _) -> parseDynamicNeeded elf
    address_has_instruction = \(NamedElf elf _ _ _ _ _ _) a -> elf_address_has_instruction elf a
    fetch_instruction = \(NamedElf elf _ _ _ _ _ _) a -> elf_disassemble_instruction elf a
    function_signatures = \(NamedElf _ _ _ _ _ _ signs) -> signs
    get_elf = \(NamedElf elf _ _ _ _ _ _) -> Just elf




-- | Information on sections
-- TODO: get from Binary interface
is_ro_data_section ("",".rodata") = True
is_ro_data_section ("",".got") = True
is_ro_data_section ("",".got.plt") = True
is_ro_data_section ("",".init_array") = True
is_ro_data_section ("",".fini_array") = True
is_ro_data_section ("",".ctors") = True
is_ro_data_section ("",".dtors") = True
is_ro_data_section ("",".data.rel.ro") = True
is_ro_data_section ("","__sancov_guards") = True
is_ro_data_section ("","__libc_IO_vtables") = True
is_ro_data_section ("__DATA","__const") = True
is_ro_data_section ("",sec) = ".rodata" `isPrefixOf` sec
is_ro_data_section _ = False

is_data_section ("__DATA","__data") = True
is_data_section ("",".data") = True
is_data_section ("",".gcc_except_table") = True
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
    [sec] -> sectionReader get_symbols sec
 where
  get_symbols = do
    (bs,_) <- get
    if BS.length bs < entry_size then
      return []
    else do
      vd_version <- read_bytes 2
      vd_flags <- read_bytes 2
      vd_ndx <- read_bytes 2
      vd_cnt <- read_bytes 2
      vd_hash <- read_bytes 4
      vd_aux <- read_bytes 4

      let vd_aux_value = fromIntegral $ bytes_to_word $ vd_aux
      let vda_name = BS.unpack $ BS.take 4 $ BS.drop vd_aux_value bs
      let vda_next = BS.unpack $ BS.take 4 $ BS.drop (vd_aux_value+4) bs

      vd_next <- BS.unpack <$> consume 4
      let vd_next_value = fromIntegral $ bytes_to_word $ vd_next
      rest <- if vd_next_value /= 0 then do
                _ <- consume $ vd_next_value - 20
                get_symbols
              else
                return []
      return $ GNU_Version_d vd_version vd_flags vd_ndx vd_cnt vd_hash vda_name vda_next : rest
  -- TODO for 64 bit ELF only
  entry_size = 2+2+2+2+4+4+4    




-- parse the .gnu_version section
parse_gnu_version elf =
  case filter (\s -> elfSectionName s == ".gnu.version") (elfSections elf) of
    [] -> []
    [gnu_version] -> split_bytestring (elfSectionData gnu_version) (fromIntegral $ elfSectionEntSize gnu_version)




sectionReader r s = evalState r $ (elfSectionData s, fromIntegral $ elfSectionAddr s)


-- parse the .dynamic section, specifically, the NEEDED libraries
-- returns a set of names of needed libraries
parseDynamicNeeded :: Elf -> S.Set String
parseDynamicNeeded elf =
  let [dynstr] = filter (\s -> elfSectionName s == ".dynstr") (elfSections elf) in
    S.unions $ map (sectionReader (get_needed dynstr)) $ filter (\s -> elfSectionType s == SHT_DYNAMIC) (elfSections elf) 
 where
  get_needed dynstr = do
    (bs,_) <- get
    if BS.length bs < 8 then
      return S.empty
    else do
      d_tag <- bytes_to_word <$> read_bytes 8
      d_union <- bytes_to_word <$> read_bytes 8
      if d_tag == 0 then -- DT_NULL
        return S.empty
      else if d_tag == 1 then do -- DT_NEEDED
        let n = T.unpack $ T.decodeUtf8 $ read_string dynstr $ (fromIntegral d_union::Word64)
        ns <- get_needed dynstr
        return $ S.insert n ns
      else
        get_needed dynstr

  read_string dynstr idx = BS.takeWhile ((/=) 0) $ BS.drop (fromIntegral idx) $ elfSectionData dynstr


-- parse the .dynsym section
-- the .dynsym section contains entries of size $elfSectionEntSize dnysym$ (in bytes)
-- the .gnu_version contains entries of size $elfSectionEntSize gnu_version$ (in bytes)
--    gnu_version_entry == 0 --> The symbol is local, not available outside the object.
--    gnu_version_entry == 1 --> The symbol is defined in this object and is globally available.
--
--    Result maps steName from symbols to version strings
parseDynSym :: Elf -> M.Map (Word32,Maybe BS.ByteString) (Maybe String)
parseDynSym elf =
  let gnu_version_entries = parse_gnu_version elf
      gnu_version_count   = length gnu_version_entries 
      gnu_version_d       = parse_gnu_version_d elf in
    case (find_dynsym, find_dynsym_symbols gnu_version_count) of
      ([dynsym], [dyn_sym_symbols]) ->
        let symbols = zip dyn_sym_symbols gnu_version_entries in
          M.fromList $ map (mk_symbol gnu_version_d) symbols
      _ -> M.empty
 where
  find_dynsym = filter ((`elem` [SHT_DYNSYM]) . elfSectionType) (elfSections elf)
  find_dynsym_symbols gnu_version_count = filter (\symbols -> length symbols == gnu_version_count) $ parseSymbolTables elf

  mk_symbol gnu_version_d (sym,gnu_version) = 
    let gnu_version_value   = (fromIntegral $ bytes_to_word $ [BS.head $ gnu_version]) - 1
        lookup_vda_name     = vda_name <$> (gnu_version_d !? gnu_version_value)
        vda_name_index      = (fromIntegral . bytes_to_word) <$> lookup_vda_name
        vda_symbol          = find_symbol_by_index elf =<< vda_name_index in
      (steName sym, (get_string_from_steName . steName) <$> vda_symbol)



find_symbol_by_index elf index = find symbol_has_index $ concat $ parseSymbolTables elf
 where
  symbol_has_index sym = (fst $ steName sym) == index
 

split_bytestring bs n
  | BS.null bs = []
  | otherwise = 
    case BS.splitAt n bs of
      (bs0,bs1) -> bs0 : split_bytestring bs1 n





data CIE_Aug_Data = Personality Word8 Address | LSDA_Encoding Word8 | FDE_Encoding Word8
 deriving (Eq,Ord)

instance Show CIE_Aug_Data where
  show (Personality enc ptr) = "Personality: " ++ show ptr ++ ", enc: 0x" ++ showHex enc
  show (LSDA_Encoding enc) = "LSDA_Encoding: 0x" ++ showHex enc
  show (FDE_Encoding enc) = "FDE_Encoding: 0x" ++ showHex enc

data CIE = CIE {
  cie_version :: Word8,
  cie_aug_string :: String,
  cie_code_align_factor :: Word64,
  cie_data_align_factor :: Int64,
  cie_return_address_register :: Word64,
  cie_aug_data :: [CIE_Aug_Data],
  cie_instructions :: [CFI_Instruction]
 }
 deriving (Show,Eq,Ord)

data FDE = FDE {
  cie_ptr  :: Address,
  pc_begin :: Address,
  pc_range :: Address,
  lsda_ptr :: Maybe Address,
  fde_instructions :: [CFI_Instruction]
 }
 deriving (Show,Eq,Ord)

data CFI_Frame = CFI_CIE CIE | CFI_FDE FDE
 deriving (Eq,Ord)

instance Show CFI_Frame where
  show (CFI_CIE cie) = show cie
  show (CFI_FDE fde) = show fde

data Address = Absolute Word64 | Indirect Word64
 deriving (Eq,Ord)

instance Show Address where
  show (Absolute w) = "0x" ++ showHex w
  show (Indirect w) = "@0x" ++ showHex w

type CFI = (Address -> String) -> (IM.IntMap [String], IM.IntMap String, IS.IntSet)


get_fde_encoding (CIE _ _ _ _ _ dats _) = firstJust get_fde dats
 where
  get_fde (FDE_Encoding enc) = Just enc
  get_fde _  = Nothing

get_lsda_ptr (CFI_FDE (FDE _ pc_begin _ (Just p) _)) = Just (pc_begin,p)
get_lsda_ptr _ = Nothing

 


parse_ehframe :: Elf -> CFI
parse_ehframe elf showAddress =
  let [ehframe] = filter (\s -> elfSectionName s == ".eh_frame") (elfSections elf)
      cfi = sectionReader (get_entries IM.empty) ehframe
      lsda_ptrs = IM.elems $ IM.mapMaybe get_lsda_ptr cfi
      cfid = mk_cfi_directives showAddress cfi
      ts = IM.fromList $ map (parse_gcc_except_table elf showAddress) lsda_ptrs
      rendered_ts = IM.map (render_gcc_except_table showAddress) ts

      cfi_addresses = IS.unions (IM.keysSet cfi : (IM.keysSet ts : map get_addresses_from_gcc_except_table (IM.elems ts))) in
    (cfid,rendered_ts,cfi_addresses)
    -- error $ (intercalate "\n" $ map show $ IM.toList cfid) ++ "\n\n" ++  (intercalate "\n" $ map (parse_gcc_except_table elf) lsda_ptrs)
 where
  get_entries entries = do
    (bs,pos0) <- get
    if BS.length bs < 4 then
      return entries
    else do
      length <- read_uint32
      if length == 0 then
        return entries
      else do
        length' <- if length == 0xffffffff then read_uint64 else return $ fromIntegral length
        (_,pos) <- get
        bs <- consume $ fromIntegral length'
        let entry = evalState (get_entry entries) (bs,pos)
        get_entries $ IM.insert pos0 entry entries

  get_entry entries = do
    (_,pos0) <- get
    cie_id <- read_uint32
    if cie_id == 0 then
      get_cie 
    else
      get_fde entries (fromIntegral pos0 - fromIntegral cie_id)

  get_cie = do
    version <- read_uint8
    aug_string <- read_string
    if "eh" `isInfixOf` aug_string then do
      _ <- consume 8 -- assumes 64 bit arch
      return ()
    else
      return ()
    code_align_factor <- read_uleb128
    data_align_factor <- read_sleb128
    return_address_register <- read_uleb128
    aug <- parse_aug aug_string
    cie_instructions <- parse_cfi_instructions code_align_factor data_align_factor

    return $ CFI_CIE $ CIE version aug_string code_align_factor data_align_factor return_address_register aug cie_instructions

  parse_aug aug_string
    | head aug_string == 'z' = do
      (aug_data_length::Word64) <- read_uleb128
      (_,pos) <- get
      aug_data <- consume $ fromIntegral aug_data_length
      return $ evalState (parse_aug_data $ tail aug_string) (aug_data,pos)
    | otherwise = return $ []

  parse_aug_data [] = return []
  parse_aug_data ('P':str) = do
    ptr_encoding <- read_uint8
    bs <- get
    personality <- read_encoded ptr_encoding
    let aug = Personality ptr_encoding personality
    augs <- parse_aug_data str
    return $ aug:augs
  parse_aug_data ('R':str) = do
    ptr_encoding <- read_uint8
    let aug = FDE_Encoding ptr_encoding
    augs <- parse_aug_data str
    return $ aug:augs
  parse_aug_data ('L':str) = do
    ptr_encoding <- read_uint8
    let aug = LSDA_Encoding ptr_encoding
    augs <- parse_aug_data str
    return $ aug:augs

  -- parsing an FDE
  get_fde entries cie_ptr = do
    let CFI_CIE cie = entries IM.! fromIntegral cie_ptr
    let Just enc = get_fde_encoding cie 
    pc_begin <- read_encoded enc
    pc_range <- read_encoded enc

    let aug_string = cie_aug_string cie
    lsda_pointer <- parse_lsda_pointer enc aug_string

    cfi_instructions <- parse_cfi_instructions (cie_code_align_factor cie) (cie_data_align_factor cie)
    return $ CFI_FDE $ FDE (Absolute cie_ptr) pc_begin pc_range lsda_pointer cfi_instructions

  parse_lsda_pointer enc aug_string
    | aug_string /= [] && head aug_string == 'z' = do
      (aug_data_length::Word64) <- read_uleb128
      (_,pos) <- get
      aug_data <- consume $ fromIntegral aug_data_length
      if 'L' `elem` aug_string then
        return $ Just $ evalState (read_encoded enc) (aug_data,pos)
      else
        return Nothing
    | otherwise = return Nothing





-- https://refspecs.linuxfoundation.org/LSB_2.1.0/LSB-Core-generic/LSB-Core-generic/dwarfehencoding.html
read_encoded :: Word8 -> State ByteStringAt Address 
read_encoded encoding = do
  let fmt = encoding .&. 0x0F
  let app = encoding .&. 0x70
  let ind = encoding .&. 0x80

  (_,pos) <- get
  v  <- read_value fmt app
  v' <- apply app v pos
  if ind == 0 then
    return $ Absolute v'
  else
    return $ Indirect v'
 where
  read_value 0x1 app = read_uleb128 -- DW_EH_PE_uleb128
  read_value 0xB app = fromIntegral <$> read_sint32 -- DW_EH_PE_sdata4
  read_value fmt app = error $ "Unknown fmt: " ++ showHex fmt  ++ ", app: " ++ showHex app ++ " in encoding " ++ showHex encoding 

  apply 0x0  v pos = return v -- DW_EH_PE_absptr
  apply 0x10 v pos = return $ (fromIntegral pos) + v -- DW_EH_PE_pcrel
  apply app v pos = error $ "Unknown app: " ++ showHex app ++ " in encoding " ++ showHex encoding 




data CFI_Instruction = 
    DW_CFA_nop
  | DW_CFA_advance_loc Int
  | DW_CFA_undefined Word64
  | DW_CFA_offset Word64 Int64 
  | DW_CFA_restore Word64
  | DW_CFA_def_cfa Word64 Word64
  | DW_CFA_def_cfa_offset Word64
  | DW_CFA_def_cfa_register Word64
  | DW_CFA_def_cfa_expression BS.ByteString
  | DW_CFA_expression Word64 BS.ByteString
  | DW_CFA_register Word64 Word64
 deriving (Show,Eq,Ord)


-- Figure 40 of https://dwarfstd.org/doc/DWARF4.pdf
parse_cfi_instructions :: Word64 -> Int64 -> State ByteStringAt [CFI_Instruction]
parse_cfi_instructions code_align_factor data_align_factor = do
  (bs,pos) <- get
  if BS.null bs then
    return []
  else do
    byte <- read_uint8
    i <- mk (primary_opcode byte) (extended_opcode byte)
    is <- parse_cfi_instructions code_align_factor data_align_factor
    return $ if i == DW_CFA_nop then is else i:is
 where
  primary_opcode byte = byte `shiftR` 6
  extended_opcode byte = byte .&. 0x3F


  mk 0x1 delta = do
    return $ DW_CFA_advance_loc $ fromIntegral delta * fromIntegral code_align_factor
  mk 0x2 reg = do
    (offset::Word64) <- read_uleb128
    return $ DW_CFA_offset (fromIntegral reg) (fromIntegral offset*data_align_factor)
  mk 0x3 reg = do
    return $ DW_CFA_restore $ fromIntegral reg
  mk 0x0 0x0 = do
    return $ DW_CFA_nop
  mk 0x0 0x2 = do
    delta <- read_uint8
    return $ DW_CFA_advance_loc $ fromIntegral delta * fromIntegral code_align_factor
  mk 0x0 0x3 = do
    delta <- read_uint16
    return $ DW_CFA_advance_loc $ fromIntegral delta * fromIntegral code_align_factor
  mk 0x0 0x4 = do
    delta <- read_uint32
    return $ DW_CFA_advance_loc $ fromIntegral delta * fromIntegral code_align_factor
  mk 0x0 0x7 = do
    reg <- read_uleb128
    return $ DW_CFA_undefined reg 
  mk 0x0 0x9 = do
    reg0 <- read_uleb128
    reg1 <- read_uleb128
    return $ DW_CFA_register reg0 reg1
  mk 0x0 0xa = do
    reg <- read_uleb128
    (len::Word64)  <- read_uleb128
    bs <- consume (fromIntegral len)
    return $ DW_CFA_expression reg bs
  mk 0x0 0xb = do
    reg <- read_uleb128
    offset <- read_sleb128
    return $ DW_CFA_offset reg (offset*data_align_factor)
  mk 0x0 0xc = do
    reg <- read_uleb128
    offset <- read_uleb128
    return $ DW_CFA_def_cfa reg offset
  mk 0x0 0xd = do
    reg <- read_uleb128
    return $ DW_CFA_def_cfa_register reg
  mk 0x0 0xe = do
    offset <- read_uleb128
    return $ DW_CFA_def_cfa_offset offset
  mk 0x0 0xf = do
    (len::Word64)  <- read_uleb128
    bs <- consume (fromIntegral len)
    return $ DW_CFA_def_cfa_expression bs
    

  mk p e = error $ "Cannot parse CFI instruction: " ++ showHex p ++ "," ++ showHex e

mk_cfi_directives :: (Address -> String) -> IM.IntMap CFI_Frame -> IM.IntMap [String]
mk_cfi_directives show_address cfi = IM.fromListWith (++) $ concatMap mk cfi
 where
  mk (CFI_CIE _ ) = []
  mk (CFI_FDE (FDE (Absolute cie_ptr) (Absolute a) _ lsda_ptr instrs)) = 
    let (CFI_CIE cie) = cfi IM.! fromIntegral cie_ptr
        personality = mk_personality a cie lsda_ptr in
      (mk_instrs a cie $ cie_instructions cie ++ instrs) ++ personality

  mk_instrs a cie [] = []
  mk_instrs a cie (DW_CFA_advance_loc offset:instrs) = mk_instrs (a+fromIntegral offset) cie instrs
  mk_instrs a cie instrs =
    let (curr,rest) = break isAdvanceLoc instrs in
      (fromIntegral a,map cfi_instruction_to_cfi_directive curr) : mk_instrs a cie rest

  isAdvanceLoc (DW_CFA_advance_loc _) = True
  isAdvanceLoc _ = False


  mk_personality a cie Nothing = []
  mk_personality a cie (Just lsda) = 
    let lsda_encoding     = mk_lsda_encoding cie
        (enc,personality) = mk_personality_ptr cie in
      [ (fromIntegral a, [".cfi_personality " ++ enc ++ ", " ++ show_address personality, ".cfi_lsda 0x" ++ showHex lsda_encoding ++ ", " ++ show_address lsda])]

  mk_lsda_encoding cie = 
    case find is_lsda_encoding $ cie_aug_data cie of
      Nothing -> 0x9b -- Sane default
      Just (LSDA_Encoding enc) -> enc
      
  mk_personality_ptr cie = 
    case find is_personality $ cie_aug_data cie of
      Just (Personality enc a) -> ("0x" ++ showHex enc, a)


  is_lsda_encoding (LSDA_Encoding _) = True
  is_lsda_encoding _ = False

  is_personality (Personality _ _) = True
  is_personality _ = False




cfi_instruction_to_cfi_directive = mk
 where
  mk (DW_CFA_undefined reg) = ".cfi_undefined " ++ cfi_reg reg 
  mk (DW_CFA_offset reg offset) = ".cfi_offset " ++ cfi_reg reg ++ ", " ++ show offset 
  mk (DW_CFA_restore reg) = ".cfi_restore " ++ cfi_reg reg 
  mk (DW_CFA_def_cfa reg offset) = ".cfi_def_cfa " ++ cfi_reg reg ++ ", " ++ show offset 
  mk (DW_CFA_def_cfa_offset offset) = ".cfi_def_cfa_offset " ++ show offset 
  mk (DW_CFA_def_cfa_register reg) = ".cfi_def_cfa_register " ++ cfi_reg reg 
  mk (DW_CFA_register reg0 reg1) = ".cfi_register " ++ cfi_reg reg0 ++ cfi_reg reg1
  mk (DW_CFA_def_cfa_expression bytes) = ".cfi_escape 0x0f, " ++ showBlock bytes ++ " # DW_CFA_def_cfa_expression" 
  mk (DW_CFA_expression reg bytes) = ".cfi_escape 0x0a, " ++ cfi_reg reg ++ ", " ++ showBlock bytes ++ " # DW_CFA_expression" 

  showBlock bytes = "0x" ++ showHex (BS.length bytes) ++ ", " ++ intercalate "," (map (\b -> "0x" ++ showHex b) $ BS.unpack bytes)

cfi_reg 0 =  "%rax"
cfi_reg 1 =  "%rdx"
cfi_reg 2 =  "%rcx"
cfi_reg 3 =  "%rbx"
cfi_reg 4 =  "%rsi"
cfi_reg 5 =  "%rdi"
cfi_reg 6 =  "%rbp"
cfi_reg 7 =  "%rsp"
cfi_reg 8 =  "%r8"
cfi_reg 9 =  "%r9"
cfi_reg 10 =  "%r10"
cfi_reg 11 =  "%r11"
cfi_reg 12 =  "%r12"
cfi_reg 13 =  "%r13"
cfi_reg 14 =  "%r14"
cfi_reg 15 =  "%r15"
cfi_reg 16 =  "%rip"
cfi_reg 17 =  "%xmm0"
cfi_reg 18 =  "%xmm1"
cfi_reg 19 =  "%xmm2"
cfi_reg 20 =  "%xmm3"
cfi_reg 21 =  "%xmm4"
cfi_reg 22 =  "%xmm5"
cfi_reg 23 =  "%xmm6"
cfi_reg 24 =  "%xmm7"
cfi_reg 25 =  "%xmm8"
cfi_reg 26 =  "%xmm9"
cfi_reg 27 =  "%xmm10"
cfi_reg 28 =  "%xmm11"
cfi_reg 29 =  "%xmm12"
cfi_reg 30 =  "%xmm13"
cfi_reg 31 =  "%xmm14"
cfi_reg 32 =  "%xmm15"
cfi_reg 33 =  "%st(0)"
cfi_reg 34 =  "%st(1)"
cfi_reg 35 =  "%st(2)"
cfi_reg 36 =  "%st(3)"
cfi_reg 37 =  "%st(4)"
cfi_reg 38 =  "%st(5)"
cfi_reg 39 =  "%st(6)"
cfi_reg 40 =  "%st(7)"
cfi_reg 41 =  "%mm0"
cfi_reg 42 =  "%mm1"
cfi_reg 43 =  "%mm2"
cfi_reg 44 =  "%mm3"
cfi_reg 45 =  "%mm4"
cfi_reg 46 =  "%mm5"
cfi_reg 47 =  "%mm6"
cfi_reg 48 =  "%mm7"
cfi_reg 49 =  "%rflags"
cfi_reg 50 =  "%es"
cfi_reg 51 =  "%cs"
cfi_reg 52 =  "%ss"
cfi_reg 53 =  "%ds"
cfi_reg 54 =  "%fs"
cfi_reg 55 =  "%gs"
cfi_reg 58 =  "%fs_base"
cfi_reg 59 =  "%gs_base"





get_addresses_from_gcc_except_table t = IS.unions $ (map get_addresses_call_site (call_sites t)) ++ [IS.fromList $ map to_value $ type_infos t]
 where
  get_addresses_call_site (GCC_Except_Table_CallSite (start1,start0) (len1,len0) (lp1,lp0) action) = IS.fromList $ map to_value [start1,start0,len1,len0,lp1,lp0]
  to_value (Absolute a) = fromIntegral a
  to_value (Indirect a) = fromIntegral a

data GCC_Except_Table_Action = GCC_Except_Table_Action {
  gcc_except_table_action_type_filter ::Int64,
  gcc_except_table_action_type_next_action ::Int64
 }
 deriving Show

data GCC_Except_Table_CallSite = GCC_Except_Table_CallSite (Address,Address) (Address,Address) (Address,Address) Word64
 deriving Show

data GCC_Except_Table = GCC_Except_Table {
  function_entry :: Word64,
  gcc_address :: Word64,
  lp_start_enc :: Word8,
  lp_start :: Maybe Address,
  ttype_enc :: Word8,
  ttype_len :: Word64,
  cs_enc :: Word8,
  cs_len :: Word64,
  call_sites :: [GCC_Except_Table_CallSite],
  actions :: [GCC_Except_Table_Action],
  type_infos :: [Address]
 }
  deriving Show


render_gcc_except_table mk_address (GCC_Except_Table function_entry gcc_address lp_start_enc lp_start ttype_enc ttype_len cs_enc cs_len call_sites actions type_infos) = intercalate "\n" $ header ++ lp ++ ttype ++ call_site ++ action_records actions ++ ttable ttype_enc ++ mk_end_label
 where
  header =
    [ "# .gcc_except_table"
    , "# LSDA for location 0x" ++ showHex function_entry ++ " is stored at 0x" ++ showHex gcc_address
    , ".section .gcc_except_table,\"a\",@progbits"
    , ".p2align 2"
    , mk_address (Absolute gcc_address) ++ ":"
    ]

  lp =
    [ "  .byte 0x" ++ showHex lp_start_enc ++ " # lp_start encoding" ] ++
    case lp_start of
      Nothing -> []

  ttype =
    [ "  .byte 0x" ++ showHex ttype_enc ++ " # ttype encoding" ]
    ++ if ttype_enc /= 0xFF then ["  .uleb128 " ++ end_label ++ " - " ++ cs_start_label ++ " # length = " ++ show ttype_len] else []

  call_site = 
    [ cs_start_label ++ ":"
    , "  .byte 0x" ++ showHex cs_enc ++ " # callsite encoding"
    , "  .uleb128 " ++ cs_end_label ++ " - " ++ cs_entries_start_label ++ " # call site table length = " ++ show cs_len
    , cs_entries_start_label ++ ":"
    ]
    ++ concatMap mk_call_site call_sites ++
    [ cs_end_label ++ ":"
    ]

  mk_call_site (GCC_Except_Table_CallSite (start1,start0) (len1,len0) (lp1,lp0) action) =
    [ "  .uleb128 " ++ mk_address start1 ++ " - " ++ mk_address start0
    , "  .uleb128 " ++ mk_address len1   ++ " - " ++ mk_address len0
    , "  .uleb128 " ++ mk_address lp1    ++ " - " ++ mk_address lp0
    , "  .uleb128 0x" ++ showHex action
    ]


  action_records [] = []
  action_records actions = 
    [ "# Action records"
    , "#   typeinfo: indices into typeinfo table"
    , "#   next action: relative pointer to next action record"
    ]
    ++ concatMap mk_action (zip [1..] actions)

  mk_action (idx,GCC_Except_Table_Action type_filter next_action) =
    [ "# Action record " ++ show idx
    ,   "  .sleb128 " ++ show type_filter ++ " # typeinfo"
    ,   "  .sleb128 " ++ show next_action ++ " # next action"
    ] 

  ttable enc
    | enc == 0xFF = []
    | otherwise = 
      [ ".p2align 2"
      , "# typeinfo table"
      ]
      ++ concatMap mk_type_info (zip [0..] type_infos)

  mk_type_info (idx,Absolute 0) = [".long 0"]
  mk_type_info (idx,a) = 
    [ typeInfo_label idx ++ ":"
    , "  .long " ++ mk_address a ++ " - " ++ typeInfo_label idx ++ " # " ++ show a ]


  mk_end_label = [end_label ++ ":"]


  end_label              = ".L_LSDA_0x" ++ showHex gcc_address ++ "_END"
  cs_start_label         = ".L_LSDA_0x" ++ showHex gcc_address ++ "_CALLSITE_TABLE_HEADER_START"
  cs_end_label           = ".L_LSDA_0x" ++ showHex gcc_address ++ "_CALLSITE_TABLE_END"
  cs_entries_start_label = ".L_LSDA_0x" ++ showHex gcc_address ++ "_CALLSITE_TABLE_START"
  typeInfo_label idx     = ".L_LSDA_0x" ++ showHex gcc_address ++ "_typeinfo_" ++ show idx


parse_gcc_except_table elf showAddress (Absolute pc_begin,Absolute a) = 
  let [s] = filter (\s -> elfSectionName s == ".gcc_except_table") (elfSections elf) in
    if elfSectionAddr s <= a && a < elfSectionAddr s + elfSectionSize s then
      let t = evalState go $ (BS.drop (fromIntegral $ a-elfSectionAddr s) $ elfSectionData s, fromIntegral a) in
        (fromIntegral a,t)
    else
      error $ "LSDA pointer 0x" ++ showHex a ++ " does not fall within a .gcc_except_table section."
 where
  go = do
    -- LPStart
    lp_start_enc <- read_uint8
    lp_start <- do
      if lp_start_enc /= 0xFF then do -- 0xFF == DW_EH_PE_omit
        lp_start <- read_encoded lp_start_enc
        return $ Just lp_start
      else
        return Nothing
    
    -- TType
    ttype_enc <- read_uint8
    ttype_len <- do
      case ttype_enc of
        0xFF -> return 0 -- 0xFF == DW_EH_PE_omit
        0x9B -> read_uleb128
        enc  -> error $ "Unknown ttype_enc: 0x" ++ showHex enc
      
    (_,pos) <- get
    let lsda_end = fromIntegral pos + fromIntegral ttype_len


    -- Call site table
    cs_enc <- read_uint8
    cs_len <- read_uleb128
    if cs_enc /= 0x01 then -- 0x01 == DW_EH_PE_uleb128
      error $ "Unknown encoding of call site."
    else
      return ()
    css <- read_call_sites cs_enc $ fromIntegral cs_len

    -- Action records
    let max_action = maximumWith get_action_of_call_site css
    (_,pos) <- get
    actions <- do
      if max_action /= 0 then
        read_until_at (pos + fromIntegral max_action+1) read_action -- TODO + 1?
      else
        return []

    -- Types table
    type_infos <- do
      case ttype_enc of
        0xFF -> return [] -- 0xFF == DW_EH_PE_omit
        _ -> do
          let max_type_index = maximumWith gcc_except_table_action_type_filter actions
          (bs,pos) <- get
          let new_pos = lsda_end - max_type_index*4
          put (BS.take (fromIntegral max_type_index*4) $ BS.drop (fromIntegral new_pos-pos) bs, fromIntegral new_pos)
          read_repeat_until_empty 0 (\_ -> read_type_info)

    return $ GCC_Except_Table pc_begin a lp_start_enc lp_start ttype_enc ttype_len cs_enc cs_len css actions type_infos


  read_call_sites cs_enc 0 = return []
  read_call_sites cs_enc cs_len = do
    (_,pos0) <- get
    start <- read_encoded cs_enc
    length <- read_encoded cs_enc
    lp <- read_encoded cs_enc
    action <- read_uleb128
    (_,pos1) <- get
    css <- read_call_sites cs_enc (cs_len-(pos1-pos0))


    case (start, length,lp) of
      (Absolute start, Absolute length,Absolute lp) -> do
        let start_rel  = (Absolute $ start + pc_begin, Absolute pc_begin)
        let length_rel = (Absolute $ length + start + pc_begin, Absolute $ start + pc_begin)
        let lp_rel     = (Absolute $ lp + pc_begin, Absolute pc_begin)
        return $ (GCC_Except_Table_CallSite start_rel length_rel lp_rel action) : css


  read_action = do
    type_filter <- read_sleb128
    next_offset <- read_sleb128
    return $ GCC_Except_Table_Action type_filter  next_offset


  read_type_info = do
    (_,pos) <- get
    type_info <- read_sint32
    if type_info == 0 then
      return $ Absolute 0
    else
      return $ Absolute $ fromIntegral $ fromIntegral type_info + pos


  get_action_of_call_site (GCC_Except_Table_CallSite _ _ _ action) = action --TODO

maximumWith f [] = 0
maximumWith f as =
  let as' = map (\a -> (a,f a)) as in
    snd $ maximumBy compareSnd as'
 where
  compareSnd (_,i) (_,j) = compare i j

