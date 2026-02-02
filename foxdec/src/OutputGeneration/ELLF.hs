{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, DeriveGeneric, StrictData, StandaloneDeriving #-}


module OutputGeneration.ELLF where


import Base
import Config

import Binary.Generic
import Binary.FunctionNames
import Binary.ELLF
import Binary.Elf
import Parser.ByteStringReader

import Data.X86.Instruction
import Data.X86.Opcode
import Data.X86.Register
import Data.Size
import Data.CFI
import Data.Symbol
import Data.JumpTarget

import qualified Data.Map as M hiding (insertWith)
import qualified Data.Map.Strict as M (insertWith)
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.ByteString.Builder
import qualified Data.ByteString as BS
import Data.ByteString.Internal (w2c)
import Data.Word 
import Data.List
import Data.List.Extra (firstJust)
import Data.Functor ((<&>))
import Data.Int
import Data.Maybe
import Data.Elf
import Data.Bits (testBit)
import Data.Char (showLitChar,isAscii,isHexDigit,toLower,isSpace)


import Data.Serialize.LEB128.Lenient 
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Char8 as BSC (unpack)
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Serialize.Get as S
import qualified Data.Serialize as Cereal hiding (get,put)
import GHC.Generics

import Data.Elf
import System.Demangle.Pure

import Debug.Trace
import System.IO.Unsafe



-- TODO LIST
-- Improve speed (use state monad to keep track of defined and undefined labels)
-- Remove excessive labels
-- Use .ellf.globals properly
-- Split into separate object files
-- Check whether LOOP/LOOPNE operands should be treated specifically.





-- Main function: read the ELLF metadata and lift to symbolized assembly
read_and_lift_ellf bin = with_elf $ get_elf bin
 where
  with_elf Nothing    = putStrLn $ "Given binary is not a valid ELF file."
  with_elf (Just elf) = with_ellf elf $ read_ellf elf 

  with_ellf elf Nothing     = putStrLn $ "Given binary is not a valid ELLF file."
  with_ellf elf (Just ellf) = do
    -- putStrLn $ show ellf
    let dirname  = binary_dir_name bin
    let name     = binary_file_name bin
    let fname    = dirname ++ name ++ ".S" 

    txt <- lift_ellf bin elf ellf
    LBS.writeFile fname txt


  

lift_ellf bin elf ellf = do
  let cfi = parse_ehframe elf (symbolize bin ellf)
  let txt = render_ellf bin elf ellf cfi 
  return txt
 where
  -- TODO use the right object here instead of 0
  symbolize bin ellf (Absolute a) = mk_label ellf 0 a
  symbolize bin ellf (Indirect a) = mk_label ellf 0 a


flatten = concatMap (\(a,bs) -> zip (repeat a) bs)





-- SYMBOLIZATION
-- make a label for address a
mk_label ellf object a = mk_label_strictly_within_object ellf object a `orTry` try_the_symbol a `orTry` try_symbol a `orElse` default_label a
 where
  try_the_symbol a = do
    sym <- firstJust (try_get_the_symbol a) $ zip [0..] $ ellf_symb_map ellf 
    return $ ellf_sym_name sym
  try_get_the_symbol a (object,m) = do
    symbs <- IM.lookup (fromIntegral a) m
    find (is_the_symbol object) symbs
  is_the_symbol object sym = ellf_sym_name sym /= ellf_section_name ((ellf_sections ellf !! object) !! (fromIntegral $ ellf_sym_section_idx sym))

  try_symbol a = do
    sym <- firstJust (try_get_symbol a) $ ellf_symb_map ellf 
    return $ ellf_sym_name sym
  try_get_symbol a m = do
    symbs <- IM.lookup (fromIntegral a) m
    return $ S.findMin symbs

-- TODO refactor so that it stores defined and undefined variables instead of this ugly hack
default_label a = "\1.L_0x" ++ showHex a

-- Make a label given the current object, otherwise fail
mk_label_strictly_within_object ellf object a = do
  symbs <- IM.lookup (fromIntegral a) (ellf_symb_map ellf !! object)
  sym <- find is_the_symbol_with_object symbs
  return $ ellf_sym_name sym 
 where
  is_the_symbol_with_object sym = ellf_sym_name sym /= ellf_section_name ((ellf_sections ellf !! object) !! (fromIntegral $ ellf_sym_section_idx sym))

mk_label_from_symbol ellf object sym = mk_label ellf object $ ellf_sym_address sym

mk_label_from_pointer ellf object ptr = mk_label_strictly_within_object ellf object $ ellf_ptr_address ptr

mk_all_labels ellf _ a = -- Always generate all labels of all objects.
  case S.toList $ S.unions $ map (S.map ellf_sym_name . IM.findWithDefault S.empty (fromIntegral a)) $ ellf_symb_map ellf of
    [] -> [default_label a]
    ls -> ls
mk_all_labels ellf (Just object) a = 
  case IM.lookup (fromIntegral a) $ ellf_symb_map ellf !! object of
    Nothing -> [default_label a]
    Just symbs -> S.toList $ S.map (mk_label_from_symbol ellf object) symbs



-- Fetching a basic block.
fetch_basic_block bin ellf object f bb@(ELLF_Basic_Block _ offset si a) = go a (fromIntegral si)
 where
  go a 0 = []
  go a si =
    case unsafePerformIO $ fetch_instruction bin (fromIntegral a) of
      Nothing -> error $ mk_error_msg $ "Address 0x" ++ showHex a ++ " does not have an instruction."
      Just i  ->
        if si < inSize i then
          error $ mk_error_msg $ "Basic block leading to the middle of an instruction: " ++ show i
        else case maybe_jump i si of 
          Nothing -> i : go (a + fromIntegral (inSize i)) (si - inSize i)
          Just a' -> i : go (a + fromIntegral (inSize i)) (si - inSize i) --  error $ "Jump in middle of basic block: " ++ show i -- mk_NOP i : go a' (si - inSize i)

  maybe_jump i si
    | isJump (inOperation i) && si > inSize i =
      case inOperands i of
        [Op_Imm (Immediate _ imm)] -> Just (inAddress i + imm)
        _ -> Nothing
    | otherwise = Nothing

  mk_NOP i = Instruction (inAddress i) [] NOP [] [] (inSize i)

  mk_error_msg msg =
    let f'   = (ellf_functions ellf !! object) !! fromIntegral (ellf_bb_function bb) in
      intercalate "\n" $
        [ msg
        , show bb
        , "Object = " ++ show object
        , "Function = " ++ show f
        , "Function[" ++ show (ellf_bb_function bb) ++ "] = " ++ show f'
        ]


--RENDERING
-- We render the entire binary possibly twice.
-- If the ELLF metadata misses basic blocks, the rendered assembly may have undefined labels.
-- During rendering, we prefix all labels that may possibly be undefined with a char '\1'.
-- We check if all these labels are defined or not in the rendered assembly.
-- If there are undefined labels, render the assembly a second time with these known new labels.
-- After that, some labels may still be undefined, if they do not correspond to any address within any basic block.
-- We declare those labels as external, with an annotation at the beginning of the assembly file.
render_ellf bin elf ellf cfi = 
  let txt    = render_ellf' bin elf ellf cfi IS.empty
      undefs = find_undefined_labels txt in
    if IS.null undefs then do
      let header  = BLU.fromString (mk_compilation_instructions bin) in
        mappend header $ BL.filter ((/=) 1) txt
    else let txt'    = render_ellf' bin elf ellf cfi undefs
             undefs' = find_undefined_labels txt' 
             txt''   = subst_undefs undefs' txt'
             header  = BLU.fromString (mk_compilation_instructions bin ++ mk_undefs_header undefs') in
           mappend header txt''
 where
  subst_undefs undefs bs
    | IS.null undefs = BL.filter ((/=) 1) bs
    | otherwise = go (IS.toList undefs) bs
  go undefs bs
    | BL.null bs      = bs
    | BL.head bs /= 1 = 
      let (bs0,bs1) = BL.span ((/=) 1) bs in
        mappend bs0 $ go undefs bs1
    | any (startsWithUndef bs) undefs =
      let Just undef = find (startsWithUndef bs) undefs
          old_label  = default_label undef
          new_label  = BLU.fromString $ undef_to_symbol undef in
        mappend new_label $ go undefs $ BL.drop (fromIntegral $ length old_label) bs
    | otherwise = go undefs $ BL.tail bs

  startsWithUndef bs undef = mk_undef undef `BL.isPrefixOf` bs
  mk_undef undef = BLU.fromString $ default_label undef

  undef_to_symbol undef =
    case IM.lookup (fromIntegral undef) $ binary_get_symbol_table bin of
      Just (AddressOfLabel f _) -> f
      -- Just sym -> error $ show "Undefined symbol: " ++ show sym
      _ -> tail $ default_label undef 

  mk_undefs_header undefs
    | IS.null undefs = ""
    | otherwise = (intercalate "\n" $ undef_msg : map mk_undef_extern (IS.toList undefs)) ++ "\n\n"

  undef_msg = "# The following functions should exist within this binary and are called or referenced, but are undefined as they are not in the metadata:"

  mk_undef_extern undef = ".extern " ++ undef_to_symbol undef

mk_compilation_instructions bin =
  let needed = binary_get_needed_libs bin
      is_cpp = binary_is_cpp bin
      name   = binary_file_name bin in
    "# " ++ (if is_cpp then "g++" else "gcc") ++ " -o " ++ name ++ "_ " ++ name ++ ".S " ++ intercalate " " (concatMap show_needed $ S.toList needed) ++ "\n\n"
 where
  show_needed lib
    | any (\p -> p `isPrefixOf` lib) ["libc.", "libgcc", "libstdc++.", "ld-"] = []
    | otherwise = ["-l" ++ (takeWhile ((/=) '.') $ drop 3 lib)]




find_undefined_labels bs =
  let (def,undef) = go bs in
    IS.difference undef def
 where
  go bs
    | BL.null bs      = (IS.empty,IS.empty)
    | BL.head bs /= 1 = go $ BL.dropWhile ((/=) 1) bs
    | otherwise       =
      let bs'           = BL.tail bs
          (label,rest)  = BL.break (\w -> w2c w `elem` [' ' , ':' , '+' , '\n' , ',' , ')' , ']']) bs'
          is_definition = not (BL.null rest) && w2c (BL.head rest) == ':'
          (def,undef)   = go rest
          address       = label_to_address $ BLU.toString label in
        if is_definition then (IS.insert address def,undef) else (def,IS.insert address undef)

  label_to_address ('.':'L':'_':'0':'x':label) = readHex' label



mk_llvm_mapping bin ellf (object,fs) = concatMap mk_llvm_mapping_fun fs
 where
  mk_llvm_mapping_fun (ELLF_Function f_name first_bb last_bb f_address) = 
    let bbs = take (fromIntegral last_bb+1-fromIntegral first_bb) $ drop (fromIntegral first_bb) $ ellf_basic_blocks ellf !! object in
      concatMap (mk_llvm_mapping_bb f_name) $ zip [0..] bbs
  mk_llvm_mapping_bb f_name (idx,bb@(ELLF_Basic_Block func_idx offset si a)) = map (mk_equality (f_name ++ "_BB" ++ show idx)) $ mk_all_labels ellf (Just object) a
  mk_equality l0 l1 = string8 $ "# " ++ l0 ++ " == " ++ l1


-- Render the CFGs if the binary contains a .ellf.cfg section
render_ellf_cfg bin ellf = 
  case ellf_cfgs ellf of
    Nothing -> string8 $ "ELLF metadata does not contain CFGs."
    Just cfgs -> 
      let mapping = foldr add_edges M.empty $ zip [0..] cfgs in
        render_list "\n" $ concat
          [ [string8 "### ELLF CFG EDGES BEGIN ###"]
          , map render_entry $ M.assocs mapping
          , [string8 "### ELLF CFG EDGES END ###"] ]
 where
  render_entry (from,tos) = string8 $ "# " ++ from ++ " --> " ++ (intercalate ", " $ S.toList tos)

  add_edges (object,edges) m = foldr (add_edge object) m edges

  add_edge object (ELLF_CFG_Edge from to) m =
    let l0 = bb_idx_to_label object from
        l1 = bb_idx_to_label object to in
      M.insertWith S.union l0 (S.singleton l1) m

  bb_idx_to_label object bb_idx =
    let bb = (ellf_basic_blocks ellf !! object) !! bb_idx
        f  = (ellf_functions ellf !! object) !! fromIntegral (ellf_bb_function bb)
        a  = ellf_bb_address bb in
      "0x" ++ showHex a
      -- head $ mk_all_labels ellf (Just object) a



render_ellf' bin elf ellf cfi@(CFI cfi_directives cfi_lsda_tables cfi_addresses) addresses =
  let cfi'             = (cfi_directives,cfi_lsda_tables,IS.union addresses cfi_addresses)
      header           = render_header bin elf ellf

      text_section     = render_list "\n\n\n" $ map (render_functions bin ellf cfi') $ zip [0..] $ ellf_functions ellf
      data_sections    = render_elf_data_sections bin elf ellf cfi' $ get_sections_from_globals elf $ concat $ ellf_globals ellf
      data_sections'   = render_elf_data_sections bin elf ellf cfi' $ nub $ concatMap (get_section_by_name elf) [".init_array", ".fini_array"]
      --data_sections    = render_list "\n\n\n" $ concatMap (render_data_section_from_globals bin elf ellf cfi') $ zip [0..] $ ellf_globals ellf
      cfi_data_section = [render_list "\n\n\n" $ map string8 $ map (render_gcc_except_table (symbolize bin ellf)) $ IM.elems cfi_lsda_tables]
      no_exec_stack    = [string8 $ "# Ensure non-executable stack\n" ++ withIndent ".section .note.GNU-stack,\"\",@progbits\n"] 
      llvm_mapping     = [render_list "\n" $ [string8 "### FOXDEC LLVM BASIC BLOCK MAPPING ###"] ++ concatMap (mk_llvm_mapping bin ellf) (zip [0..] $ ellf_functions ellf) ++ [string8 "### FOXDEC LLVM BASIC BLOCK MAPPING END ###"]]
      cfgs             = [render_ellf_cfg bin ellf] in
    toLazyByteString $ render_list "\n\n\n" $ [header,text_section,data_sections,data_sections'] ++ cfi_data_section ++ no_exec_stack -- ++ llvm_mapping ++ cfgs
 where
  -- TODO use the right object here instead of 0
  symbolize bin ellf (Absolute a) = mk_label ellf 0 a
  symbolize bin ellf (Indirect a) = mk_label ellf 0 a





render_header bin elf ellf = render_list "\n\n" [mk_intel_syntax, mk_externals]
 where
  mk_intel_syntax = string8 ".intel_syntax"

  mk_externals = render_list "\n" (string8 "# External functions" : map mk_external (S.toList $ externals bin))
  mk_external f = string8 $ ".extern " ++ f


  mk_globals = render_list "\n" (map mk_global $ binary_get_exported_functions bin)
  mk_global (_,f) = string8 $ ".globl " ++ f

-- | get the external functions and objects 
externals bin = S.fromList $ map symbol_to_name $ filter is_relocation $ IM.elems $ binary_get_symbol_table bin
 where
  is_relocation (PointerToExternalFunction str) = str /= ""
  is_relocation (PointerToObject str ex _ _) = ex && str /= ""
  is_relocation (AddressOfObject str ex) = ex && str /= ""
  is_relocation (AddressOfLabel str ex) = ex && str /= ""
  is_relocation _ = False

-- | get the external objects
external_objects l@(bin,_,l0) = map symbol_to_name $ filter is_relocation $ IM.elems $ binary_get_symbol_table bin
 where
  is_relocation (PointerToObject str ex _ _) = ex && str /= ""
  is_relocation (AddressOfObject str ex) = ex && str /= ""
  is_relocation _ = False


get_section_by_name elf name = filter (\s -> elfSectionName s == name) $ elfSections elf





-- RENDERING DATA SECTIONS
{--
-- Render data sections provided by ELLF globals
render_data_section_from_globals bin elf ellf cfi (object,globals) = render_data_sections_merged $ group overlaps $ sortBy compareStartAddress globals
 where
  render_data_sections_merged [] = []
  render_data_sections_merged (g:gs) = 
    (render_data_section bin elf ellf cfi (Just object) $ merge g)
    ++
    render_data_sections_merged gs

  overlaps g0 g1 = region_overlap (ellf_global_address g0) (ellf_global_size g0) (ellf_global_address g1) (ellf_global_size g1)
  merge gs =
     let min = minimumBy compareStartAddress gs
         max = maximumBy compareEndAddress gs in
       (ellf_global_address min, ellf_global_address max + ellf_global_size max - ellf_global_address min)

  compareStartAddress g0 g1 = compare (ellf_global_address g0) (ellf_global_address g1)
  compareEndAddress g0 g1 = compare (ellf_global_address g0 + ellf_global_size g0) (ellf_global_address g1 + ellf_global_size g1)

  region_overlap a0 si0 a1 si1 = (a0 <= a1 && a1 < a0 + si0) || (a1 <= a0 && a0 < a1 + si1)

  -- TODO does this group sufficiently? Should use transitive closure of relation r
  group r [] = [] 
  group r (g:gs) =
    let (matches,rest) = mk_group r [g] gs in
      matches : group r rest

  mk_group r as [] = (as,[])
  mk_group r as (b:bs)
    | any (r b) as = mk_group r (b:as) bs
    | otherwise = 
      let (matches,rest) = mk_group r as bs in
        (matches,b:rest)
    --}


get_sections_from_globals elf globals = nub $ concatMap toSection globals ++ concatMap find_section_by_name [".data.rel.ro", ".bss", ".data", ".rodata"]
 where
  find_section_by_name name =
    case find (\s -> elfSectionName s == name) $ elfSections elf of
      Nothing -> []
      Just s  -> [s]
  toSection g =
    case find (contains_address (ellf_global_address g)) $ elfSections elf of
      Nothing -> []
      Just s  -> [s]
  contains_address a section =
    let a0  = elfSectionAddr section
        si0 = elfSectionSize section in
      a0 <= a && a < a0 + si0



-- Render entire data sections provided by the ELF itself
render_elf_data_sections bin elf ellf cfi sections = render_list "\n\n\n" $ concatMap (render_data_section bin elf ellf cfi Nothing) sections

-- Consider non-executable sections except for the CFI related ones
is_relevant_data_section section = and
    [ SHF_ALLOC `elem` elfSectionFlags section || is_bss section
    , not (SHF_EXECINSTR `elem` elfSectionFlags section)
    , not (isInfixOf "gcc_except_table" $ elfSectionName section)
    , elfSectionName section `notElem` [".eh_frame", ".eh_frame_hdr"]
    ]
 where
  is_bss section = elfSectionName section `elem` [".bss", ".tbss"]


-- Render a data section given a starting address and a size
render_data_section bin elf ellf cfi@(_,_,cfi_addresses) optional_object section
  | elfSectionAddr section == 0 = mempty
  | elfSectionSize section == 0 = mempty
  | not (is_relevant_data_section section) = mempty
  | otherwise = [render_section section a si]
 where
  a  = elfSectionAddr section
  si = elfSectionSize section


  is_bss section        = elfSectionName section `elem` [".bss", ".tbss"]
  is_func_array section = elfSectionName section `elem` [".init_array", ".fini_array"]

  -- Render a data section
  render_section section a si = render_list "\n" $ (mk_section_header section a : mk_align section : mk_data_section section (is_bss section) (is_func_array section) a si) ++ mk_section_end_label section

  -- Render the header
  mk_section_header section a = string8 $ "# " ++ mk_object optional_object ++ "@0x" ++ showHex a ++ " (" ++ show si ++ " bytes)\n" ++ withIndent ".section " ++ (elfSectionName section) 

  -- Render the object
  mk_object Nothing    = ""
  mk_object (Just obj) = "object " ++ show obj ++ ", "

  -- Render the alignment
  mk_align section = string8 $ withIndent ".align 0x" ++ showHex (elfSectionAddrAlign section)

  -- Render the end label
  mk_section_end_label section = 
    let a = elfSectionAddr section + elfSectionSize section in
      case find (contains_address a 1) $ elfSections elf of
        Nothing -> [string8 $ "# additional end-label @0x" ++ showHex a] ++ address_to_labels Nothing a
        Just _  -> []



  -- Render the data
  -- Two things happen here: relocations from the .ellf.pointers sections are symbolized, and labels are inserted.
  -- If the section is an array of function pointers (.init_array, .fini_array) make sure that only funciton pointers are rendered
  mk_data_section section is_bss is_funptr_array (a::Word64) (si::Word64) =
    case find_next_pointer is_bss a si of
       (Nothing,Nothing,Nothing) -> raw_data section is_bss is_funptr_array a si
       (Just (object,ptr),_,_) -> 
          let ptr_a  = ellf_ptr_address ptr
              pte    = (ellf_pointees ellf  !! fromIntegral object) !! (fromIntegral $ ellf_ptr_pointee_idx ptr)
              pte_si = pointee_size ptr pte
              part0  = raw_data section is_bss is_funptr_array a (ptr_a - a)
              part1  = if is_funptr_array then [] else address_to_labels optional_object ptr_a 
              part2  = [render_pointee section object ptr_a pte pte_si]
              part3  = mk_data_section section is_bss is_funptr_array (ptr_a+pte_si) (si + a - ptr_a - pte_si) in
            part0 ++ part1 ++ part2 ++ part3
       (Nothing,Just (Relocation ptr_a a1),_) ->
          let pte_si = 8
              part0  = raw_data section is_bss is_funptr_array a (ptr_a - a)
              part1  = if is_funptr_array then [] else address_to_labels optional_object ptr_a 
              part2  = mk_reloc is_funptr_array a1 
              part3  = mk_data_section section is_bss is_funptr_array (ptr_a+pte_si) (si + a - ptr_a - pte_si) in
            part0 ++ part1 ++ part2 ++ part3
       (Nothing,Nothing,Just (ptr_a,sym)) ->
          let pte_si = 8
              part0  = raw_data section is_bss is_funptr_array a (fromIntegral ptr_a - a)
              part1  = if is_funptr_array then [] else address_to_labels optional_object (fromIntegral ptr_a)
              part2  = mk_symbol sym
              part3  = mk_data_section section is_bss is_funptr_array (fromIntegral ptr_a+pte_si) (si + a - fromIntegral ptr_a - pte_si) in
            part0 ++ part1 ++ part2 ++ part3


  -- Within .init_array or .fini_array not all relocs need to be included (e.g. frame_dummy)
  -- the tail makes it not add \1 (see default_label)
  mk_reloc is_funptr_array a1
    | is_funptr_array && not (is_function_entry a1) = [string8 $ withIndent "# .quad "   ++ tail (head (mk_all_labels ellf optional_object a1)) ++ " RELOC NOT IN ELLF METADATA"] 
    | is_function_entry a1 || in_data_section a1    = [string8 $ withIndent ".quad "     ++       head (mk_all_labels ellf optional_object a1)  ++ " # DEVIATION AS RELOC IS NOT IN ELLF METADATA"]
    | not is_funptr_array                           = [string8 $ withIndent ".quad 0 # " ++ tail (head (mk_all_labels ellf optional_object a1)) ++ " RELOC NOT IN ELLF METADATA"] 
    | otherwise                                     = [string8 $ withIndent "# .quad "   ++ tail (head (mk_all_labels ellf optional_object a1)) ++ " RELOC NOT IN ELLF METADATA"]

  mk_symbol (PointerToExternalFunction f)   = [ string8 $ withIndent ".quad " ++ f ]
  mk_symbol (PointerToObject o _ addend _)  = [ string8 $ withIndent ".quad " ++ o ++ (if addend==0 then "" else "+0x" ++ showHex addend)]
  mk_symbol (Relocated_ResolvedObject o _)  = [ string8 $ withIndent ".quad " ++ o ]
  mk_symbol (PointerToInternalFunction f _) = [ string8 $ withIndent ".quad " ++ f ]

  is_function_entry a = a `elem` map ellf_func_address (concat $ ellf_functions ellf)
  in_data_section a =
    case find (\s -> elfSectionAddr s <= a && a < elfSectionAddr s + elfSectionSize s) $ elfSections elf of
      Nothing -> False
      Just s  -> "bss" `isInfixOf` elfSectionName s || "data" `isInfixOf` elfSectionName s


  -- Find the next entry in .ellf.pointer, or the next relocation
  find_next_pointer is_bss (a::Word64) (si::Word64) =
    let first_GE_symbol = if is_bss then Nothing else IM.lookupGE (fromIntegral a) $ IM.filterWithKey (symbol_is_reloc_below (fromIntegral $ a+si)) $ binary_get_symbol_table bin 
        bnd0 = case first_GE_symbol of
                 Nothing       -> a + si
                 Just (a0,sym) -> fromIntegral a0
        first_GE_reloc  = if is_bss then Nothing else first_GE a bnd0 (\(Relocation a0 _) -> a0) $ S.toList $ binary_get_relocations bin
        bnd1 = case first_GE_reloc of
                 Nothing                -> bnd0
                 Just (Relocation a0 _) -> a0
        first_ELLF_pointer = first_GE a bnd1 (ellf_ptr_address . snd) $ all_ellf_pointers optional_object in
      (first_ELLF_pointer,first_GE_reloc,first_GE_symbol)

  all_ellf_pointers Nothing    = flatten $ zip [0..] $ ellf_pointers ellf
  all_ellf_pointers (Just obj) = flatten $ [(obj,ellf_pointers ellf !! fromIntegral obj)]

  symbol_is_reloc_below a' a0 (Relocated_ResolvedObject _ _)  = a0 < a'
  symbol_is_reloc_below a' a0 (PointerToExternalFunction _)   = a0 < a'
  symbol_is_reloc_below a' a0 (PointerToInternalFunction _ _) = a0 < a'
  symbol_is_reloc_below a' a0 (PointerToObject _ _ _ _)       = a0 < a'
  symbol_is_reloc_below a' a0 _ = False


  -- Render raw data, but insert labels of symbols from .ellf.symbols as well as from the CFI directives.
  raw_data section is_bss is_funptr_array a 0  = []
  --raw_data section is_bss True            _ _  = []
  raw_data section is_bss is_funptr_array a si = 
    let bytes = if is_bss then RD_BSS $ fromIntegral si else RD_ByteString $ BS.take (fromIntegral si) $ BS.drop (fromIntegral $ a - elfSectionAddr section) $ elfSectionData section in
      raw_data_with_symbols (fromIntegral a) (fromIntegral si) is_funptr_array bytes
 
  raw_data_with_symbols a si is_funptr_array bytes
   --  | raw_data_is_null bytes = []
    | otherwise =
      let symbols = {--if is_funptr_array then [] else --}takeWhile (\a' -> a' < a + si || (a' == a && fromIntegral a' == elfSectionAddr section)) $ dropWhile (\a' -> a' < a) $ IS.toAscList $ IS.union cfi_addresses $ all_ellf_symbol_addresses optional_object in
        raw_data_insert_symbols (fromIntegral a) symbols bytes

  all_ellf_symbol_addresses Nothing    = IS.unions $ map IM.keysSet $ ellf_symb_map ellf
  all_ellf_symbol_addresses (Just obj) = IM.keysSet $ ellf_symb_map ellf !! fromIntegral obj


  raw_data_insert_symbols a [] bytes = render_bytes a bytes
  raw_data_insert_symbols a (sym:syms) bytes
    | a  < sym = render_bytes a (raw_data_take (sym - a) bytes) ++ raw_data_insert_symbols sym (sym:syms) (raw_data_drop (sym - a) bytes)
    | a == sym = {--section_alignment (fromIntegral a) ++--}  address_to_labels optional_object (fromIntegral sym) ++ raw_data_insert_symbols a syms bytes
    | otherwise = error $ show a ++  " == 0x" ++ showHex a ++ "\n" ++ show (sym:syms)

  address_to_labels Nothing a = map (\l -> string8 (l ++ ":")) $ mk_all_labels ellf Nothing a
  address_to_labels (Just obj) a =
    case mk_label_strictly_within_object ellf obj a of
      Nothing -> []
      Just l  -> [string8 (l ++ ":")]

  section_alignment a = [] {--
    case find (\s -> ellf_section_address s == a) $ concat $ ellf_sections ellf of
      Nothing -> []
      Just s  -> [string8 $ withIndent ".align 0x" ++ showHex (ellf_section_align s) ] --}
  

  -- Render bytes. If it is 0-terminated ASCII, render as string using .asciz
  render_bytes a (RD_BSS si) = [string8 $ withIndent $ ".skip " ++ show si ]
  render_bytes a (RD_ByteString bytes) 
   -- RENDERING WITHOUT PRETTY PRINTING
   -- = [(string8 $ withIndent ".byte ") <> (render_list "," (map (\c -> string8 $ "0x" ++ showHex c) $ BS.unpack bytes))]i
    | BS.null bytes = []
    | otherwise =
      case takeWhileString bytes of
        Nothing -> 
          let (part0,part1) = bs_takeUntilZero bytes in
            [(string8 $ withIndent ".byte ") <> (render_list "," (map (\c -> string8 $ "0x" ++ showHex c) $ BS.unpack part0))] <> render_bytes (a+BS.length part0) (RD_ByteString part1)
        Just (str,rest) -> [string8 (withIndent ".asciz \"") <> escape_string (concatMap (\c -> showLitChar c "") str) <> string8 ("\"") ] <> render_bytes (a+length str+1) (RD_ByteString rest)

  takeWhileString bytes =
    let (bs0,bs1) = BS.span valid_char bytes in
       if not (BS.null bs0) && not (BS.null bs1) && BS.head bs1 == 0 then
         Just $ (map w2c $ BS.unpack bs0,BS.tail bs1)
       else
         Nothing

  bs_takeUntilZero bs =
    let (bs0,bs1) = BS.span ((/=) 0) bs in
      if BS.null bs1 then
        (bs0,bs1)
      else
        (BS.snoc bs0 (BS.head bs1), BS.tail bs1)

  valid_char c = c /= 0 && ((c >= 32 && c < 127) || c `elem` [9,10,13])



  contains_address a si section =
    let a0  = elfSectionAddr section
        si0 = elfSectionSize section in
      a0 <= a && a + si <= a0 + si0

  pointee_size ptr pte@(ELLF_Pointee base 0 _)
    | testBit (ellf_ptr_flags ptr) 2 = 4 -- A pointer diff even though target is 0
    | otherwise = 8 -- A pointer
  pointee_size ptr pte@(ELLF_Pointee base target _) = 4 -- A pointer diff

  -- Rendering a pointer
  render_pointee elf_section object ptr_a pte@(ELLF_Pointee base 0 addend) 8 = 
    let base_symbol  = (ellf_symbols ellf !! fromIntegral object) !! fromIntegral base
        base_address = ellf_sym_address base_symbol in
      if base_address == 1 then -- ELLF_EXTERN
        string8 $ (try_render_reloc_for ptr_a `orTry` try_render_symbol_at ptr_a `orElse` ("ERROR: *[0x" ++ showHex ptr_a ++ "]"))
      else case try_render_reloc_for ptr_a of
        Just str -> string8 $ str ++ " # RELOC"
        Nothing  -> string8 $ withIndent ".quad " ++ ellf_sym_name base_symbol ++ mk_offset addend
  -- The following case should never happen, but is a sane default in case the ellf metadata is wrong
  render_pointee elf_section object ptr_a pte@(ELLF_Pointee base 0 addend) 4 =
     let --sym1  = (ellf_symbols ellf !! fromIntegral object) !! fromIntegral base
         --base_address = ellf_sym_address sym1

         bytes = read_bytes elf_section ptr_a 4
         diff  = evalState read_sint32 (bytes,0)
         -- trgt_address = base_address - fromIntegral diff in
         
         trgt_address = (fromIntegral <$> IS.lookupLE (fromIntegral ptr_a) all_LEA_addresses) `orElse` fromIntegral ptr_a
         base_address = trgt_address + diff in
         string8 $ concat 
           -- [ withIndent ".long " ++ (mk_label ellf object base_address ++ mk_offset addend) ++ " - ."
           [ withIndent ".long " ++ (mk_label ellf object base_address) ++ " - " ++ (mk_label ellf object trgt_address)
           , " # DEVIATION FROM ELLF METADATA AS TARGET==0. PICKING AS BASE 0x" ++ showHex base_address ++ " AND AS TARGET 0x" ++ showHex trgt_address
           -- , " DEBUG: base == 0x" ++ showHex base_address ++ ", trgt == 0x" ++ showHex trgt_address ++ ", bytes = 0x" ++ showHex diff ]
           ]
  -- Rendering a pointer diff
  render_pointee elf_section object ptr_a pte@(ELLF_Pointee base target addend) 4 =
     let sym1  = (ellf_symbols ellf !! fromIntegral object) !! fromIntegral base
         sym2  = (ellf_symbols ellf !! fromIntegral object) !! fromIntegral target
         bytes = read_bytes elf_section ptr_a 4
         diff  = evalState read_sint32 (bytes,0) in
       if ellf_sym_address sym1 + fromIntegral addend - ellf_sym_address sym2 == fromIntegral diff then
         string8 $ withIndent ".long " ++ (mk_label_from_symbol ellf object sym1) ++ mk_offset addend ++ " - " ++ (mk_label_from_symbol ellf object sym2)
       else let base_address = ellf_sym_address sym2 + fromIntegral diff in
         -- the following case happens: the ellf metadata has the wrong base. We compute the right base and annotate the assembly.
         string8 $ concat 
           [ withIndent ".long " ++ (mk_label ellf object base_address) ++ " - " ++ (mk_label_from_symbol ellf object sym2)
           , " # DEVIATION FROM ELLF METADATA: "
           , (mk_label_from_symbol ellf object sym1) ++ mk_offset addend
           , " (0x" ++ showHex (fromIntegral (ellf_sym_address sym1) + addend) ++ ") - "
           , mk_label_from_symbol ellf object sym2
           -- , " DEBUG: " ++ showHex base_address ++ ", " ++ showHex (ellf_sym_address sym2) ++ ", " ++ showHex diff ]
           ]

  read_bytes elf_section a si = BS.take (fromIntegral si) $ BS.drop (fromIntegral $ a - elfSectionAddr elf_section) $ elfSectionData elf_section

  all_LEA_addresses = get_all_LEA_addresses bin ellf


  try_render_reloc_for a = do
    Relocation _ a1 <- find (reloc_for $ fromIntegral a) $ binary_get_relocations bin
    return $ withIndent ".quad " ++ head (mk_all_labels ellf optional_object a1)

  try_render_symbol_at :: Word64 -> Maybe String
  try_render_symbol_at a = do
    sym <- IM.lookup (fromIntegral a) $ binary_get_symbol_table bin
    case sym of
      PointerToExternalFunction f -> return $ withIndent ".quad " ++ f 
      PointerToObject obj _ addend _ -> return $ withIndent ".quad " ++ obj ++ (if addend==0 then "" else "+0x" ++ showHex addend)
      Relocated_ResolvedObject _ a1 -> do
        sym1 <- IM.lookup (fromIntegral a1) $ binary_get_symbol_table bin
        return $ withIndent ".quad " ++ symbol_to_name sym1
      sym -> return $ "TODO: 0x" ++ showHex a ++ " lookup in symbol table = " ++ show sym

  mk_offset offset
    | offset == 0 = ""
    | offset < 0 = " - 0x" ++ showHex (0 - offset)
    | otherwise = " + 0x" ++ showHex offset


data RawData = RD_ByteString BS.ByteString | RD_BSS Int

raw_data_is_null (RD_ByteString bs) = BS.null bs
raw_data_is_null (RD_BSS si) = si == 0

raw_data_take n (RD_ByteString bs) = RD_ByteString $ BS.take n bs
raw_data_take n (RD_BSS si) = RD_BSS $ min n si

raw_data_drop n (RD_ByteString bs) = RD_ByteString $ BS.drop n bs
raw_data_drop n (RD_BSS si) = RD_BSS $ si - n




-- RENDERING TEXT SECTIONS

-- Rendering a list of functions
render_functions bin ellf cfi (object,fs) = render_list "\n\n\n" $ map (render_function bin ellf cfi object) fs


get_all_LEA_addresses :: BinaryClass bin => bin -> ELLF -> IS.IntSet
get_all_LEA_addresses bin ellf = IS.unions $ map get_LEAs_funcs $ zip [0..] $ ellf_functions ellf
 where
  get_LEAs_funcs (object,fs) = IS.unions $ map (get_LEAs_func object) fs
  get_LEAs_func object f@(ELLF_Function symb_idx first_bb last_bb f_address) = 
    let bbs = take (fromIntegral last_bb+1-fromIntegral first_bb) $ drop (fromIntegral first_bb) $ ellf_basic_blocks ellf !! object in
      IS.unions $ map (get_LEAs_bb object f) bbs
  get_LEAs_bb object f bb =
    let instrs = fetch_basic_block bin ellf object f bb in
      IS.fromList $ mapMaybe get_LEA_operand_address $ filter is_LEA instrs
  is_LEA i = inOperation i == LEA

  get_LEA_operand_address i = 
    case get_RIP_relative (inOperands i !! 1) of
      Nothing    -> Nothing
      Just displ -> Just $ fromIntegral $ fromIntegral (inAddress i) + fromIntegral (inSize i) + displ

  get_RIP_relative (Op_Mem si (Reg64 RIP) RegNone _ displ Nothing _) = Just displ
  get_RIP_relative _ = Nothing



-- Rendering a function
render_function bin ellf cfi object f@(ELLF_Function f_name first_bb last_bb f_address) =
  let bbs      = take (fromIntegral last_bb+1-fromIntegral first_bb) $ drop (fromIntegral first_bb) $ ellf_basic_blocks ellf !! object 
      first_i  = ellf_bb_address (head bbs) 
      last_a = case fetch_basic_block bin ellf object f $ last bbs of
                 [] -> ellf_bb_address $ last bbs
                 is -> inAddress (last is) + fromIntegral (inSize $ last is) in
    render_list "\n"
      [ render_header f_name bbs
      , render_list "\n\n" $ map (render_basic_block bin ellf cfi object f f_name bbs) $ zip [0..] bbs
      , render_post f_name bbs first_i last_a
      ]
 where
  render_header f_name bbs = render_list "\n" $ map string8 $ catMaybes
    [ Just $ "# Function " ++ f_name
    , if binary_is_cpp bin then ("# Demangled " ++) <$> (demangle f_name) else Nothing
    , Just $ withIndent ".text"
    , Just $ withIndent ".globl " ++ f_name -- TODO only if exported
    , Just $ withIndent ".weak " ++ f_name -- TODO only if exported
    , Just $ withIndent ".p2align 4"
    , Just $ withIndent ".type " ++ f_name ++ ",@function"
    , Just $ withIndent ".cfi_startproc"
    , if ellf_bb_size (head bbs) == 0 && address_is_start_of_bb ellf bbs False (ellf_bb_address (head bbs)) then Just $ f_name ++ "_BB0:" else Nothing
    ]
  render_post f_name bbs first_i last_a = render_list "\n" $ map string8
    [ -- mk_end_label f_name bbs first_i last_a ++ ":"
      withIndent ".size " ++ f_name ++ ", " ++ " . " {-- mk_end_label f_name bbs first_i last_a --} ++ " - " ++ (f_name ++ "_BB0") -- mk_label ellf object (inAddress first_i)
    , withIndent ".cfi_endproc"
    , "# End of function " ++ f_name
    ]



  -- Only if the end address of this function is not already inserted as the start of another, insert an end-label (prevent duplicate labels)
  mk_end_label f_name bbs first_i a
    | address_is_start_of_bb ellf bbs True a = ".L_" ++ f_name ++ "_0x" ++ showHex first_i ++ "_END_LABEL"
    | otherwise = mk_label ellf object a 


address_is_start_of_bb ellf bbs can_be_empty a = find (basic_block_starting_at a) bbs /= Nothing || find (function_starting_at a) (concat $ ellf_functions ellf) /= Nothing
 where
  basic_block_starting_at a bb = (can_be_empty || ellf_bb_size bb /= 0) && ellf_bb_address bb == a
  function_starting_at a f = ellf_func_address f == a


address_is_start_of_bb_anywhere ellf a = any (basic_block_starts_at a) $ concat $ ellf_basic_blocks ellf
 where
  basic_block_starts_at a bb = ellf_bb_address bb == a

 
-- Rendering a basic block
render_basic_block bin ellf cfi object f f_name bbs (idx,bb@(ELLF_Basic_Block func_idx _ 0 a)) =
  if not $ address_is_start_of_bb ellf bbs False a then
    render_basic_block' bin ellf cfi object f f_name bbs (idx,bb)
  else
    mempty -- string8 $ show (object,idx,bb,x)
render_basic_block bin ellf cfi object f f_name bbs (idx,bb) = render_basic_block' bin ellf cfi object f f_name bbs (idx,bb)
    


render_basic_block' bin ellf cfi object f f_name bbs (idx,bb@(ELLF_Basic_Block func_idx _ si a)) = 
  let is = fetch_basic_block bin ellf object f bb in
    render_list "\n" 
      [ render_basic_block_header
      , render_basic_block_label 
      , if si == 0 then mempty else render_instructions bin ellf object cfi is 
      , if si == 0 then mempty else render_basic_block_end_label (a + si) ]
 where
  render_basic_block_header = string8 $ "# Basic block " ++ f_name ++ "@0x" ++ showHex a
  render_basic_block_label = string8 $ intercalate "\n" $ map mk_label_def $ mk_all_labels ellf (Just object) a ++ [f_name ++ "_BB" ++ show idx] -- TODO LLVM BB label is temporary
  mk_label_def l = l ++ ":"
  render_basic_block_end_label a
    -- | address_is_start_of_bb ellf bbs True a = mempty
    | address_is_start_of_bb_anywhere ellf a = mempty
    | otherwise =  string8 $ intercalate "\n" $ map mk_label_def $ mk_all_labels ellf (Just object) a

-- Rendering a list of instructions
render_instructions bin ellf object cfi = render_list "\n" . map (render_instruction bin ellf object cfi) . zip [0..]
  

-- Rendering an instruction:
-- First match operands to the GAS syntax
render_instruction bin ellf object cfi (n,i) = render_GAS_instruction bin ellf object cfi (n,mk_GAS i)
 where
  -- TODO double check this, and expand
  mk_GAS (Instruction addr pre NOP   _ info si)         = Instruction addr pre NOP   []    info si
  mk_GAS (Instruction addr pre FSTP  [op0,op1] info si) = Instruction addr pre FSTP  [op0] info si
  mk_GAS (Instruction addr pre FISTP [op0,op1] info si) = Instruction addr pre FISTP [op0] info si
  mk_GAS (Instruction addr pre FIST  [op0,op1] info si) = Instruction addr pre FISTP [op0] info si
  mk_GAS (Instruction addr pre FLD   [op0,op1] info si) = Instruction addr pre FLD   [op1] info si
  mk_GAS (Instruction addr pre FILD  [op0,op1] info si) = Instruction addr pre FILD  [op1] info si
  mk_GAS (Instruction addr pre FCOM  [op0,op1] info si) = Instruction addr pre FCOM  [op1] info si
  mk_GAS (Instruction addr pre FCOMP [op0,op1] info si) = Instruction addr pre FCOMP [op1] info si
  mk_GAS (Instruction addr pre FADDP [op0,op1] info si) = Instruction addr pre FADDP [op0] info si
  mk_GAS (Instruction addr pre FXCH  [op0,op1] info si) = Instruction addr pre FXCH  [op1] info si
  mk_GAS (Instruction addr pre FMUL  [op0,op1] info si)
    | isMem op1 = Instruction addr pre FMUL [op1] info si
    | otherwise = i
  mk_GAS i@(Instruction addr pre FDIV [op0,op1] info si)
    | isMem op1 = Instruction addr pre FDIV [op1] info si
    | otherwise = i
  mk_GAS (Instruction addr pre FSUBR [Op_Reg (RegFPU ST0) _,op1] info si) = Instruction addr pre FSUBR [op1] info si
  mk_GAS i = i

isMem (Op_Mem _ _ _ _ _ _ _) = True
isMem _                      = False


-- Rendering an instruction:
-- 1.) render the corresponding CFI directives, if any
-- 2.) if the address of this instruction is referenced in a CFI directive, insert a label
-- 3.) render the instruction
render_GAS_instruction bin ellf object (cfi_dirs,_,cfi_addresses) (n,i@(Instruction addr pre op ops info si)) = render_cfi (IM.lookup (fromIntegral addr) cfi_dirs) <> render_cfi_label n <> render_instr
 where
  render_cfi Nothing    = mempty
  render_cfi (Just cfi) = render_list "\n" (map string8 (cfi ++ [""]))

  render_cfi_label 0 = mempty
  render_cfi_label n
    | fromIntegral addr `IS.member` cfi_addresses = string8 $ mk_label ellf object addr ++ ":\n"
    | otherwise = mempty

  render_instr = string8 (withIndent "") <> render_list "" 
    [ render_prefix pre
    , render_mnemonic op
    , render_operands bin ellf object i op ops
    ]


render_prefix :: [Prefix] -> Builder
render_prefix ps
  | PrefixRep `elem` ps   = stringUtf8 "REP "
  | PrefixRepNE `elem` ps = stringUtf8 "REPNE "
  | PrefixLock `elem` ps  = stringUtf8 "LOCK "
  | otherwise             = mempty

render_mnemonic :: Opcode -> Builder
render_mnemonic (InvalidOpcode op) = string8 $ op ++ " "
render_mnemonic op = string8 $ show op ++ " "

render_operands :: BinaryClass bin => bin -> ELLF -> Int -> Instruction -> Opcode -> [Operand] -> Builder
render_operands bin ellf object i mnemonic = render_list ", " . map (render_operand bin ellf object i)

-- Rendering an operand
-- For JUMPs and CALLs, the immeidate must be interpreted RIP-relative and thus symbolized.
-- Otherwise, symbolize the operand if it is RIP relative
render_operand :: BinaryClass bin => bin -> ELLF -> Int -> Instruction -> Operand -> Builder
render_operand bin ellf object i op
  -- TODO also for LOOP/LOOPE/LOOPNE?
  | isCall (inOperation i) || isJump (inOperation i) || isCondJump (inOperation i) || isLoop (inOperation i) = render_control_flow_operand bin ellf i op
  | isMem op =
    case find (\ptr -> testBit (ellf_ptr_flags ptr) 1 && inAddress i <= ellf_ptr_address ptr && ellf_ptr_address ptr < inAddress i + fromIntegral (inSize i)) $ ellf_pointers ellf !! object of
      Just ptr -> render_TPOFF_operand bin ellf object ptr op
      Nothing  -> render_operand_normal bin ellf i op
  | otherwise = render_operand_normal bin ellf i op
 where
  render_control_flow_operand bin ellf i (Op_Imm (Immediate _ imm)) = 
    case jump_target_for_instruction bin i of
      External sym      -> string8 $ sym ++ "@PLT"
      ExternalDeref sym -> string8 $ "*" ++ sym ++ "@PLT" --TODO
      _                 -> string8 $ symbolize_address bin ellf object True (inAddress i + imm)
  render_control_flow_operand bin ellf i op = render_operand_normal bin ellf i op

  render_operand_normal bin ellf i op =
    case symbolize_rip_relative_operand bin ellf object i op of
      Nothing -> string8 $ render_operand_GAS op
      Just s  -> string8 $ s


render_TPOFF_operand bin ellf object ptr op@(Op_Mem si reg idx scale displ seg _) =
  let pte = (ellf_pointees ellf  !! fromIntegral object) !! (fromIntegral $ ellf_ptr_pointee_idx ptr)
      sym = (ellf_symbols ellf !! fromIntegral object) !! fromIntegral (ellf_pte_base pte) in
    string8 $ with_size_directive (show_seg seg) si $ show_reg reg ++ ellf_sym_name sym ++ "@TPOFF"
   where
    show_seg Nothing  = ""
    show_seg (Just r) = "%" ++ show r ++ ":"
    show_reg RegNone = ""
    show_reg reg     = show_register reg ++ " + " 


render_operand_GAS (Op_Reg r _) = show_register r
render_operand_GAS (Op_Imm imm) = show imm
render_operand_GAS (Op_Mem si reg idx scale displ seg _) = 
  let part = show_reg reg ++ show_idx_scale reg idx scale in
    (with_size_directive (show_seg seg) si $ part ++ show_displ part displ)
   where
    show_seg Nothing  = ""
    show_seg (Just r) = "%" ++ show r ++ ":"
    show_reg RegNone = ""
    show_reg reg     = show_register reg
    show_idx_scale _ RegNone 0 = ""
    show_idx_scale _ RegNone 1 = ""
    show_idx_scale _ _       0 = error "todo"
    show_idx_scale RegNone idx scale = show_reg idx ++ "*" ++ showHex scale
    show_idx_scale _       idx scale = " + " ++ show_reg idx ++ "*" ++ showHex scale
    show_displ "" displ = if displ < 0 then " - 0x" ++ showHex (fromIntegral (0-displ)) else "0x" ++ showHex displ
    show_displ _  displ = if displ < 0 then " - 0x" ++ showHex (fromIntegral (0-displ)) else " + 0x" ++ showHex displ


-- Render registers in GAS syntax
show_register (RegFPU ST0) = "%ST(0)"
show_register (RegFPU ST1) = "%ST(1)"
show_register (RegFPU ST2) = "%ST(2)"
show_register (RegFPU ST3) = "%ST(3)"
show_register (RegFPU ST4) = "%ST(4)"
show_register (RegFPU ST5) = "%ST(5)"
show_register (RegFPU ST6) = "%ST(6)"
show_register (RegFPU ST7) = "%ST(7)"
show_register reg = "%" ++ show reg

with_size_directive seg (BitSize si) s = mk_size_directive si ++ seg ++ "[" ++ s ++ "]"
 where
  mk_size_directive 0   = ""
  mk_size_directive 8   = "byte ptr "
  mk_size_directive 16  = "word ptr "
  mk_size_directive 32  = "dword ptr "
  mk_size_directive 64  = "qword ptr "
  mk_size_directive 80  = "tbyte ptr "
  mk_size_directive 128 = "oword ptr "
  mk_size_directive 256 = "ymmword ptr "
  mk_size_directive 512 = "zword ptr "


-- Symbolization of a RIP-relative operand
symbolize_rip_relative_operand bin ellf object i (Op_Mem si (Reg64 RIP) RegNone _ displ Nothing _) = Just $ with_size_directive "" si $ symbolize_address bin ellf object False $ fromIntegral (fromIntegral (inAddress i) + fromIntegral (inSize i) + displ)
symbolize_rip_relative_operand bin ellf object i (Op_Mem _ (Reg64 RIP) _ _ _ _ _) = error (show i)
symbolize_rip_relative_operand bin ellf object i (Op_Mem _ _ (Reg64 RIP) _ _ _ _) = error (show i)
symbolize_rip_relative_operand bin ellf object i (Op_Reg (Reg64 RIP) _) = error (show i)
symbolize_rip_relative_operand bin ellf object _ _ = Nothing


-- Symbolize address $a$.
-- If the address is within a .text section, simply make the label.
-- Otherwise:
--
-- 1.) it may be the case that at address $a$, a pointer to a function (or external object) is stored (a relocation):
--	*a == &f
-- In this case, symbolize $a$ to "rip + f@GOTPCRL" if we are currently symbolizing the operand of an instruction, and simply to "f" if we are symbolizing data in a data section.
--
-- 2.) it may be the case that address $a$ has a symbol:
--  &sym == a
-- In this case, symbolize $a$ to "rip + sym" if we are currently symbolizing the operand of an instruction, and simply to "sym" if we are symbolizing data in a data section.
-- It may also be the case that address $a$ is equal to a symbol + offset.
symbolize_address :: BinaryClass bin => bin -> ELLF -> Int -> Bool -> Word64 -> String
symbolize_address bin ellf object in_data_section a =
  case find_section_for_address bin a of
    Nothing -> symbolize_address_in_data_section a -- "***ERROR UNKNOWN ADDRESS 0x" ++ showHex a ++ "***"
    Just (seg,sec,a0,si,align,flgs) -> symbolize flgs a
 where
  symbolize flgs a
    | SectionIsExecutable `elem` flgs = withRIP ++ mk_label ellf object a
    | otherwise = symbolize_address_in_data_section a


  withRIP = (if in_data_section then "" else "%rip + ")

  -- TODO: for the last case, check if it is text or data section, then just generate label, otherwise fail
  symbolize_address_in_data_section a = try_ellf_within_global a `orTry` try_GOT_entry a `orTry` try_symbol a `orTry` try_reloc a `orElse` mk_default_label a

  mk_default_label a = withRIP ++ mk_label ellf object a -- ++ mk_error_message a

  try_ellf_within_global a = do
    g <- find (\g -> ellf_global_address g <= a &&  a < ellf_global_address g + ellf_global_size g) $ ellf_globals ellf !! object
    return $ try_symbol_LE a

  try_GOT_entry a =
    case IM.lookup (fromIntegral a) $ binary_get_symbol_table bin of
      Just (sym@(PointerToExternalFunction f))   -> if in_data_section then Nothing else Just $ withRIP ++ f ++ "@GOTPCREL"
      Just (sym@(PointerToInternalFunction _ _)) -> error $ "TODO: symbolize 0x" ++ showHex a
      Just (sym@(PointerToObject o _ 0 _))       -> if in_data_section then Nothing else Just $ withRIP ++ o ++ "@GOTPCREL"
      Just (sym@(TLS_Relative f))                -> Just $ withRIP ++ f ++ "@GOTTPOFF"
      Just (sym@(Relocated_ResolvedObject _ _))  -> error $ "TODO: symbolization of 0x" ++ showHex a ++ ": " ++ show sym

      -- TODO
      Just (sym@(AddressOfObject o True))        -> if in_data_section then Nothing else Just $ withRIP ++ o
      Just (sym@(AddressOfLabel l True))         -> if in_data_section then Nothing else Just $ withRIP ++ l
      _ -> Nothing

  try_reloc a = do
    sec <- find (contains_address a) $ elfSections $ fromJust $ get_elf bin
    if isInfixOf ".got" $ elfSectionName sec then do
      Relocation a0 a1 <- find (\(Relocation a0 a1) -> a0 == a) $ binary_get_relocations bin
      return $ withRIP ++ mk_label ellf object a1 ++ "@GOTPCREL"
    else
      Nothing


  try_symbol a = do
    sym <- firstJust (try_get_symbol a) $ ellf_symb_map ellf 
    return $ withRIP ++ mk_label_from_symbol ellf object sym
  try_get_symbol a m = do
    symbs <- IM.lookup (fromIntegral a) m
    return $ S.findMin symbs


  try_symbol_LE a =
    let sym = find_symbol_LE (fromIntegral a) Nothing $ ellf_symb_map ellf in
      withRIP ++ mk_label_from_symbol ellf object sym ++ mk_offset (a - ellf_sym_address sym)

  find_symbol_LE a Nothing          [] = error $ "Address 0x" ++ showHex a ++ " does not have a symbol."
  find_symbol_LE a (Just (a0,sym0)) [] = S.findMin sym0
  find_symbol_LE a curr (m:ms) = 
    case (curr,IM.lookupLE a m) of
      (Just (a0,sym0),Just (a1,sym1)) -> find_symbol_LE a (if a0 > a1 then Just (a0,sym0) else Just (a1,sym1)) ms
      (Nothing,Just (a1,sym1))        -> find_symbol_LE a (Just (a1,sym1)) ms
      (Just (a0,sym0),Nothing)        -> find_symbol_LE a (Just (a0,sym0)) ms
      (Nothing,Nothing)               -> find_symbol_LE a Nothing ms
    

  mk_offset offset
    | offset == 0 = ""
    | (fromIntegral offset :: Int64) < 0 = " - 0x" ++ showHex (0 - offset)
    | otherwise = " + 0x" ++ showHex offset


  mk_error_message a = "ADDRESS 0x" ++ showHex a ++ " HAS NO ENCOMPASSING GLOBAL"

  in_text_section a = 
    case find (contains_address a) $ elfSections $ fromJust $ get_elf bin of
      Just s  -> SHF_EXECINSTR `elem` elfSectionFlags s
      Nothing -> False


  is_reloc_loc a' (Relocation a0 _) = a0 == a'


  contains_address a section =
    let a0  = elfSectionAddr section
        si0 = elfSectionSize section in
      a0 <= a && a < a0 + si0

substitutions :: (String -> Maybe (String,String)) -> (String -> String) -> String -> String
substitutions pattern replacement [] = []
substitutions pattern replacement s =
  case pattern s of
    Nothing -> head s : substitutions pattern replacement (tail s)
    Just (str0,str1) -> replacement str0 ++ substitutions pattern replacement str1

escape_string :: String -> Builder
escape_string cs = foldMap escape cs
  where
    escape '\"' = charUtf8 '\\' <> charUtf8 '\"'
    escape c    = charUtf8 c


render_list :: String -> [Builder] -> Builder
render_list s []     = mempty
render_list s (b:bs) = b <> mconcat [ stringUtf8 s <> b' | b' <- bs ]

first_GE :: Word64 -> Word64 -> (a -> Word64) -> [a] -> Maybe a
first_GE x y f [] = Nothing
first_GE x y f (a:as)
  | f a >= x && f a < y = Just $ first_GE' x y a as
  | otherwise = first_GE x y f as
 where
  first_GE' x y a [] = a
  first_GE' x y a (b:bs) 
    | x <= f b && f b < y && f b < f a = first_GE' x y b bs
    | otherwise                        = first_GE' x y a bs



first_LE :: Word64 -> (a -> Word64) -> [a] -> Maybe a
first_LE x f [] = Nothing
first_LE x f (a:as)
  | f a <= x  = Just $ first_LE' x a as
  | otherwise = first_LE x f as
 where
  first_LE' x a [] = a
  first_LE' x a (b:bs) 
    | f b <= x && f b > f a = first_LE' x b bs
    | otherwise             = first_LE' x a bs

reloc_for a (Relocation a0 a1) = a == a0


