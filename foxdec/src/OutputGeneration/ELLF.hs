{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, DeriveGeneric, StrictData, StandaloneDeriving #-}


module OutputGeneration.ELLF where


import Base
import Config

import Binary.Generic
import Binary.FunctionNames
import Binary.ELLF
import Binary.Elf

import Data.X86.Instruction
import Data.X86.Opcode
import Data.X86.Register
import Data.Size
import Data.Symbol
import Data.JumpTarget

import Parser.ParserCFI

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.ByteString.Builder
import qualified Data.ByteString as BS
import Data.ByteString.Internal (w2c)
import Data.Word 
import Data.List
import Data.Functor ((<&>))
import Data.Int
import Data.Maybe
import Data.Elf
import Data.Char (showLitChar,isAscii,isHexDigit)


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


import Debug.Trace


-- Main function: read the ELLF metadata and lift to symbolized assembly
read_and_lift_ellf bin = with_elf $ get_elf bin
 where
  with_elf Nothing    = putStrLn $ "Given binary is not a valid ELF file."
  with_elf (Just elf) = with_ellf elf $ read_ellf elf

  with_ellf elf Nothing     = putStrLn $ "Given binary is not a valid ELLF file."
  with_ellf elf (Just ellf) = do
    putStrLn $ show ellf
    let dirname  = binary_dir_name bin
    let name     = binary_file_name bin
    let fname    = dirname ++ name ++ ".S" 

    cfi <- get_cfi_directives bin
    let txt = render_ellf bin elf ellf cfi
    LBS.writeFile fname txt





-- SYMBOLIZATION
mk_label ellf a = try_the_symbol a `orTry` try_symbol a `orElse` default_label a
 where
  try_the_symbol a = ellf_sym_name <$> (find is_the_symbol $ concat $ ellf_symbols ellf)
  is_the_symbol sym = ellf_sym_address sym == Just a && ellf_sym_name sym /= ellf_section_name (concat (ellf_sections ellf) !! (fromIntegral $ ellf_sym_section_idx sym))

  try_symbol a = ellf_sym_name <$> (find (\sym -> ellf_sym_address sym == Just a) $ concat $ ellf_symbols ellf)


default_label a = "\1.L_0x" ++ showHex a


mk_label_from_symbol ellf sym = 
  case ellf_sym_address sym of
    Nothing -> ellf_sym_name sym
    Just a  -> mk_label ellf a

mk_label_from_pointer ellf ptr = mk_label ellf $ fromJust (ellf_ptr_base ptr) + ellf_ptr_offset ptr

mk_all_labels ellf a = 
  case S.toList $ S.fromList $ map ellf_sym_name $ filter (\sym -> ellf_sym_address sym == Just a) $ concat $ ellf_symbols ellf of
    [] -> [default_label a]
    ls -> ls



fetch_basic_block bin f_address (ELLF_Basic_Block _ offset si) = go (f_address+offset) (fromIntegral si)
 where
  go a 0 = []
  go a si =
    case fetch_instruction bin (fromIntegral a) of
      Nothing -> error $ "Address 0x" ++ showHex a ++ " does not have an instruction."
      Just i  ->
        if si < inSize i then
          error $ show (i,si)
        else
          i : go (a + fromIntegral (inSize i)) (si - inSize i)



--RENDERING
-- We render the entire binary possibly twice.
-- If the ELLF metadata misses basic blocks, the rendered assembly may have undefined labels.
-- During rendering, we prefix all labels that may possibly be undefined with a char '\1'.
-- We check if all these labels are defined or not in the rendered assembly.
-- If there are undefined labels, render the assembly a second time with these known new labels.
-- After that, some labels may still be undefined, if they do not correspond to any address within any basic block.
-- We declare those labels as external, with an annotation at the beginning of the assembly file.
render_ellf bin elf ellf cfi =
  let txt    = render_ellf' bin elf ellf cfi S.empty
      undefs = find_undefined_labels txt in
    if S.null undefs then
      BL.filter ((/=) 1) txt
    else let txt'    = render_ellf' bin elf ellf cfi undefs
             undefs' = find_undefined_labels txt' 
             txt''   = subst_undefs undefs' txt'
             header  = BLU.fromString (mk_compilation_instructions bin ++ mk_undefs_header undefs') in
           mappend header txt''
 where
  subst_undefs undefs bs
    | S.null undefs = bs
    | otherwise = go undefs bs
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
      Nothing -> tail $ default_label undef 

  mk_undefs_header undefs
    | S.null undefs = ""
    | otherwise = (intercalate "\n" $ undef_msg : map mk_undef_extern (S.toList undefs)) ++ "\n\n"

  undef_msg = "# The following functions should exist within this binary and are called or referenced, but are undefined as they are not in the metadata:"

  mk_undef_extern undef = ".extern " ++ undef_to_symbol undef


mk_compilation_instructions bin =
  let needed = binary_get_needed_libs bin
      is_cpp = any (isInfixOf "libstdc++") needed
      name   = binary_file_name bin in
    "# " ++ (if is_cpp then "g++" else "gcc") ++ " -o " ++ name ++ "_ " ++ name ++ ".S " ++ intercalate " " (concatMap show_needed $ S.toList needed) ++ "\n\n"
 where
  show_needed lib
    | any (\p -> p `isPrefixOf` lib) ["libc.", "libgcc", "libstdc++."] = []
    | otherwise = ["-l:" ++ lib]




find_undefined_labels bs =
  let (def,undef) = go bs in
    S.difference undef def
 where
  go bs
    | BL.null bs      = (S.empty,S.empty)
    | BL.head bs /= 1 = go $ BL.dropWhile ((/=) 1) bs
    | otherwise       =
      let bs'           = BL.tail bs
          (label,rest)  = BL.break (\w -> w2c w `elem` [' ' , ':' , '+' , '\n' , ',' , ')' , ']']) bs'
          is_definition = not (BL.null rest) && w2c (BL.head rest) == ':'
          (def,undef)   = go rest
          address       = label_to_address $ BLU.toString label in
        if is_definition then (S.insert address def,undef) else (def,S.insert address undef)

  label_to_address ('.':'L':'_':'0':'x':label) = readHex' label





render_ellf' bin elf ellf cfi addresses =
  let cfi_addresses    = S.union addresses $ S.fromList $ map fromIntegral $ IM.keys (cfi_directives cfi) ++ cfi_get_addresses (cfi_data cfi)
      
      header           = render_header bin elf ellf
      text_section     = render_list "\n\n\n" $ map (render_functions bin ellf cfi cfi_addresses) $ zip [0..] $ ellf_functions ellf
      data_sections    = concatMap (render_data_section bin elf ellf cfi_addresses) $ ellf_layout ellf
      cfi_data_section = [string8 $ cfi_symbolize bin ellf $ cfi_data cfi]
      no_exec_stack    = [string8 $ "# Ensure non-executable stack\n.section .note.GNU-stack,\"\",@progbits"]  in
    toLazyByteString $ render_list "\n\n\n" $ (header:text_section:data_sections) ++ cfi_data_section ++ no_exec_stack


render_header bin elf ellf = render_list "\n\n" [mk_intel_syntax, mk_externals]
 where
  mk_intel_syntax = string8 ".intel_syntax noprefix"

  mk_externals = render_list "\n" (string8 "# External functions" : map mk_external (S.toList $ externals bin))
  mk_external f = string8 $ ".extern " ++ f


  mk_globals = render_list "\n" (map mk_global $ binary_get_exported_functions bin)
  mk_global (_,f) = string8 $ ".globl " ++ f

-- | get the external functions and objects 
externals bin = S.fromList $ map symbol_to_name $ filter is_relocation $ IM.elems $ binary_get_symbol_table bin
 where
  is_relocation (PointerToExternalFunction str) = str /= ""
  is_relocation (PointerToObject str ex) = ex && str /= ""
  is_relocation (AddressOfObject str ex) = ex && str /= ""
  is_relocation (AddressOfLabel str ex) = ex && str /= ""
  is_relocation _ = False

-- | get the external objects
external_objects l@(bin,_,l0) = map symbol_to_name $ filter is_relocation $ IM.elems $ binary_get_symbol_table bin
 where
  is_relocation (PointerToObject str ex) = ex && str /= ""
  is_relocation (AddressOfObject str ex) = ex && str /= ""
  is_relocation _ = False



-- RENDERING DATA SECTIONS
render_data_section bin elf ellf cfi_addresses l@(ELLF_Layout object a si section_name) =
  case find (contains_address a si) $ elfSections elf of
    Nothing -> [string8 $ "ERROR: Cannot find section for: " ++ show l]
    Just section -> if is_relevant_data_section section then [render_section section a si] else []
 where
  -- Consider non-exexutable sections except for the CFI related ones
  is_relevant_data_section section = not (SHF_EXECINSTR `elem` elfSectionFlags section) && not (isInfixOf "__gxx_personality" section_name) && not (isInfixOf "gcc_except_table" section_name)


  -- Render a data section
  render_section section a si = render_list "\n" $ mk_section_header a : mk_align : mk_data_section section a si

  -- Render the header
  mk_section_header a = string8 $ "# object " ++ show object ++ ", @0x" ++ showHex a ++ " (" ++ show si ++ " bytes)\n.section " ++ (ellf_layout_section l) 

  -- Render the alignment
  mk_align = 
    case find (\s -> section_name == ellf_section_name s) $ ellf_sections ellf  !! fromIntegral object of -- TODO  && si == ellf_section_size s
      Just section -> string8 $ ".align " ++ show (ellf_section_align section)
      Nothing -> string8 "# UNKNOWN ALIGNMENT" 

  -- Render the data
  -- Two things happen here: relocations from the .ellf.pointers sections are symbolized, and labels are inserted.
  mk_data_section section a si =
    case find_next_pointer a si of
       Nothing  -> raw_data section a si
       Just ptr -> 
          let ptr_a  = ptr_to_address ptr
              pte    = (ellf_pointees ellf  !! fromIntegral object) !! (fromIntegral $ ellf_ptr_pointee_idx ptr)
              pte_si = pointee_size pte
              part0  = raw_data section a (ptr_a - a)
              part1  = [string8 $ mk_label_from_pointer ellf ptr ++ ":"]
              part2  = [render_pointee ptr_a pte]
              part3  = mk_data_section section (ptr_a+pte_si) (si + a - ptr_a - pte_si) in
            part0 ++ part1 ++ part2 ++ part3

  -- Find the next entry in .ellf.pointer
  find_next_pointer a si =
    case first_GE a ptr_to_address (ellf_pointers ellf !! fromIntegral object) of
      Nothing  -> Nothing
      Just ptr -> if ptr_to_address ptr < a + si then Just ptr else Nothing

  -- Render raw data, but insert labels of symbols from .ellf.symbols as well as from the CFI directives.
  raw_data section a 0  = []
  raw_data section a si = 
    let dat   = BS.take (fromIntegral si) $ BS.drop (fromIntegral $ a - elfSectionAddr section) $ elfSectionData section
        bytes = BS.unpack dat in
      raw_data_with_symbols a si bytes
 
  raw_data_with_symbols a si [] = []
  raw_data_with_symbols a si bytes =
    let symbols0 = filter (\sym -> a <= symbol_to_address sym && symbol_to_address sym < a + si) $ ellf_symbols ellf !! fromIntegral object
        symbols = cfi_symbols a si ++ symbols0 -- TODO prevent duplicates
        sorted  = sortBy (\sym0 sym1 -> compare (symbol_to_address sym0) (symbol_to_address sym1)) symbols in
      raw_data_insert_symbols a sorted bytes

  raw_data_insert_symbols a [] bytes = render_bytes bytes
  raw_data_insert_symbols a (sym:syms) bytes
    | a  < symbol_to_address sym = render_bytes (take (fromIntegral $ symbol_to_address sym - a) bytes) ++ raw_data_insert_symbols (symbol_to_address sym) (sym:syms) (drop (fromIntegral $ symbol_to_address sym - a) bytes)
    | a == symbol_to_address sym = string8 (mk_label_from_symbol ellf sym ++ ":") : raw_data_insert_symbols a syms bytes
    | otherwise = error $ show a ++  " == "++showHex a ++ "\n" ++ show (sym:syms)

  cfi_symbols a si = map (\a -> ELLF_Symbol 0 (Just a) $ mk_label ellf a) $ filter (\a' -> a <= a' && a' < a + si) $ S.toList cfi_addresses

  -- Render bytes. If it is 0-terminated ASCII, render as string using .asciz
  render_bytes []    = []
  render_bytes bytes =
    case takeWhileString bytes of
      Nothing -> [(string8 "  .byte ") <> (render_list "," (map (\c -> string8 $ "0x" ++ showHex c) bytes))]
      Just (str,rest) -> [string8 "  .asciz \"" <> escape_string (concatMap (\c -> showLitChar c "") str) <> string8 "\""] <> render_bytes rest

  takeWhileString bytes =
    let (bs0,bs1) = span valid_char bytes in
       if bs0 /= [] && bs1 /= [] && head bs1 == 0 then
         Just $ (map w2c bs0,tail bs1)
       else
         Nothing

  valid_char c = c /= 0 && ((c >= 32 && c < 127) || c `elem` [9,10])-- (isAscii $ w2c c) 




  ptr_to_address ptr = 
    case ellf_ptr_base ptr of
      Nothing -> 0
      Just b  -> b + ellf_ptr_offset ptr


  contains_address a si section =
    let a0  = elfSectionAddr section
        si0 = elfSectionSize section in
      a0 <= a && a + si <= a0 + si0

  pointee_size pte@(ELLF_Pointee base 0 _) = 8 -- A pointer
  pointee_size pte@(ELLF_Pointee base target 0) = 4 -- A pointer diff

  render_pointee ptr_a pte@(ELLF_Pointee base 0 addend) = string8 $ 
    try_render_reloc_for ptr_a 
    `orTry` try_render_symbol_at ptr_a 
    `orTry` try_render_ellf_symbol base addend
    `orElse` ("ERROR: *[0x" ++ showHex ptr_a ++ "]")
  render_pointee ptr_a pte@(ELLF_Pointee base target 0) =
     let sym1 = (ellf_symbols ellf !! fromIntegral object) !! fromIntegral base
         sym2 = (ellf_symbols ellf !! fromIntegral object) !! fromIntegral target in
       string8 $ "  .long " ++ (mk_label_from_symbol ellf sym1) ++ "-" ++ (mk_label_from_symbol ellf sym2)
  render_pointee ptr_a pte = error $ "DON'T KNOW HOW TO RENDER POINTEE @ 0x" ++ showHex ptr_a ++ ": " ++ show pte


  try_render_reloc_for :: Word64 -> Maybe String
  try_render_reloc_for a = do
    Relocation _ a1 <- find (reloc_for $ fromIntegral a) $ binary_get_relocations bin
    return $ "  .quad " ++ symbolize_address bin ellf True a1

  try_render_symbol_at :: Word64 -> Maybe String
  try_render_symbol_at a = do
    sym <- IM.lookup (fromIntegral a) $ binary_get_symbol_table bin
    case sym of
      PointerToExternalFunction f -> return $ "  .quad " ++ f -- ++ mk_offset addend
      PointerToObject obj _ -> return $ "  .quad " ++ obj -- ++ mk_offset addend
      Relocated_ResolvedObject _ a1 -> do
        sym1 <- IM.lookup (fromIntegral a1) $ binary_get_symbol_table bin
        return $ "  .quad " ++ symbol_to_name sym1 -- ++ mk_offset addend
      sym -> return $ "TODO: 0x" ++ showHex a ++ " lookup in symbol table = " ++ show sym

  try_render_ellf_symbol :: Word64 -> Int64 -> Maybe String
  try_render_ellf_symbol base addend = do
    let sym = (ellf_symbols ellf !! fromIntegral object) !! fromIntegral base
    return $ "  .quad " ++ ellf_sym_name sym ++ mk_offset addend 

  mk_offset offset
    | offset == 0 = ""
    | offset < 0 = " - 0x" ++ showHex (0 - offset)
    | otherwise = " + 0x" ++ showHex offset

-- RENDERING TEXT SECTIONS

-- Rendering a list of functions
render_functions bin ellf cfi cfi_addresses (object,fs) = render_list "\n\n\n" $ map (render_function bin ellf cfi cfi_addresses object) fs

-- Rendering a function
render_function bin ellf cfi cfi_addresses object f@(ELLF_Function symb_idx first_bb last_bb f_address) =
  let f_symbol = (ellf_symbols ellf !! object) !! fromIntegral symb_idx
      f_name   = ellf_sym_name f_symbol
      bbs      = take (fromIntegral last_bb+1-fromIntegral first_bb) $ drop (fromIntegral first_bb) $ ellf_basic_blocks ellf !! object 
      first_i  = head $ fetch_basic_block bin f_address $ head bbs
      last_i   = last $ fetch_basic_block bin f_address $ last bbs in
    render_list "\n"
      [ render_header f_name
      , render_list "\n\n" $ map (render_basic_block bin ellf cfi cfi_addresses f_name f_address) bbs
      , render_post f_name first_i last_i
      ]
 where
  render_header f_name = render_list "\n" $ map string8
    [ "# Function " ++ f_name
    , ".text"
    , ".globl " ++ f_name -- TODO only if exported
    , ".p2align 4"
    , ".type " ++ f_name ++ ",@function"
    , ".cfi_startproc"
    ]
  render_post f_name first_i last_i = render_list "\n" $ map string8
    [ mk_end_label (inAddress last_i + fromIntegral (inSize last_i))
    , ".size " ++ f_name ++ ", " ++ mk_label ellf (inAddress last_i + fromIntegral (inSize last_i)) ++ " - " ++ mk_label ellf (inAddress first_i)
    , ".cfi_endproc"
    , "# End of function " ++ f_name
    ]

  -- Only if the end address of this function is not already inserted as the start of another, insert an end-label (prevent duplicate labels)
  mk_end_label a =
    case find (\f -> ellf_func_address f == a) $ concat $ ellf_functions ellf of
      Nothing -> mk_label ellf a  ++ ":"
      Just _  -> "# No end-label required"


-- Rendering a basic block
render_basic_block bin ellf cfi cfi_addresses f_name f_address bb@(ELLF_Basic_Block func_idx offset si) = 
  let is = fetch_basic_block bin f_address bb in
    render_list "\n" 
      [ render_basic_block_header
      , render_basic_block_label 
      , render_instructions bin ellf cfi cfi_addresses is ]
 where
  render_basic_block_header = string8 $ "# Basic block " ++ f_name ++ "@0x" ++ showHex (f_address + offset)
  render_basic_block_label = string8 $ intercalate "\n" $ map mk_label $ mk_all_labels ellf (f_address + offset)
  mk_label l = l ++ ":"

-- Rendering a list of instructions
render_instructions bin ellf cfi cfi_addresses = render_list "\n" . map (render_instruction bin ellf cfi cfi_addresses) . zip [0..]
  

-- Rendering an instruction:
-- First match operands to the GAS syntax
render_instruction bin ellf cfi cfi_addresses (n,i) = render_GAS_instruction bin ellf cfi cfi_addresses (n,mk_GAS i)
 where
  -- TODO double check this, and expand
  mk_GAS (Instruction addr pre FSTP  [op0,op1] info si) = Instruction addr pre FSTP  [op0] info si
  mk_GAS (Instruction addr pre FISTP [op0,op1] info si) = Instruction addr pre FISTP [op0] info si
  mk_GAS (Instruction addr pre FLD   [op0,op1] info si) = Instruction addr pre FLD   [op1] info si
  mk_GAS (Instruction addr pre FILD  [op0,op1] info si) = Instruction addr pre FILD  [op1] info si
  mk_GAS (Instruction addr pre FCOM  [op0,op1] info si) = Instruction addr pre FCOM  [op1] info si
  mk_GAS (Instruction addr pre FCOMP [op0,op1] info si) = Instruction addr pre FCOMP [op1] info si
  mk_GAS (Instruction addr pre FADDP [op0,op1] info si) = Instruction addr pre FADDP [op0] info si
  mk_GAS (Instruction addr pre FMUL  [op0,op1] info si)
    | isMem op1 = Instruction addr pre FMUL [op1] info si
    | otherwise = i
  mk_GAS i@(Instruction addr pre FDIV [op0,op1] info si)
    | isMem op1 = Instruction addr pre FDIV [op1] info si
    | otherwise = i
  mk_GAS i = i

  isMem (Op_Mem _ _ _ _ _ _ _) = True
  isMem _                      = False


-- Rendering an instruction:
-- 1.) render the corresponding CFI directives, if any
-- 2.) if the address of this instruction is referenced in a CFI directive, insert a label
-- 3.) render the instruction
render_GAS_instruction bin ellf (CFI dirs _) cfi_addresses (n,i@(Instruction addr pre op ops info si)) = render_cfi (IM.lookup (fromIntegral addr) dirs) <> render_cfi_label n <> render_instr
 where
  render_cfi Nothing    = mempty
  render_cfi (Just cfi) = string8 $ (cfi_symbolize bin ellf cfi) ++ "\n"

  render_cfi_label 0 = mempty
  render_cfi_label n
    | addr `S.member` cfi_addresses = string8 $ mk_label ellf addr ++ ":\n"
    | otherwise = mempty

  render_instr = render_list " " 
    [ string8 $ indent 2
    , render_prefix pre
    , render_mnemonic op
    , render_operands bin ellf i op ops
    ]


render_prefix :: [Prefix] -> Builder
render_prefix ps
  | PrefixRep `elem` ps   = stringUtf8 "REP"
  | PrefixRepNE `elem` ps = stringUtf8 "REPNE"
  | PrefixLock `elem` ps  = stringUtf8 "LOCK"
  | otherwise             = mempty

render_mnemonic :: Opcode -> Builder
render_mnemonic (InvalidOpcode op) = string8 op
render_mnemonic op = string8 $ show op 

render_operands :: BinaryClass bin => bin -> ELLF -> Instruction -> Opcode -> [Operand] -> Builder
render_operands bin ellf i mnemonic = render_list ", " . map (render_operand bin ellf i)

-- Rendering an operand
-- For JUMPs and CALLs, the immeidate must be interpreted RIP-relative and thus symbolized.
-- Otherwise, symbolize the operand if it is RIP relative
render_operand :: BinaryClass bin => bin -> ELLF -> Instruction -> Operand -> Builder
render_operand bin ellf i op
  -- TODO also for LOOP/LOOPE/LOOPNE?
  | isCall (inOperation i) || isJump (inOperation i) || isCondJump (inOperation i) || isLoop (inOperation i) = render_control_flow_operand bin ellf i op
  | otherwise = render_operand_normal bin ellf i op
 where
  render_control_flow_operand bin ellf i (Op_Imm (Immediate _ imm)) = 
    case jump_target_for_instruction bin i of
      External sym      -> string8 $ sym ++ "@PLT"
      ExternalDeref sym -> string8 $ "*" ++ sym ++ "@PLT" --TODO
      _                 -> string8 $ symbolize_address bin ellf True (inAddress i + imm)
  render_control_flow_operand bin ellf i op = render_operand_normal bin ellf i op

  render_operand_normal bin ellf i op =
    case symbolize_rip_relative_operand bin ellf i op of
      Nothing -> string8 $ render_operand_GAS op
      Just s  -> string8 $ s


render_operand_GAS (Op_Reg r _) = show_register r
render_operand_GAS (Op_Imm imm) = show imm
render_operand_GAS (Op_Mem si reg idx scale displ seg _) = with_size_directive si $ show_seg seg ++ show_reg reg ++ show_idx_scale idx scale ++ show_displ displ
   where
    show_seg Nothing  = ""
    show_seg (Just r) = show r ++ ":"
    show_reg RegNone = ""
    show_reg reg     = show_register reg
    show_idx_scale RegNone 0 = ""
    show_idx_scale RegNone 1 = ""
    show_idx_scale _  0      = error "todo"
    show_idx_scale idx scale = " + " ++ show idx ++ "*" ++ showHex scale
    show_displ displ = if displ < 0 then " - 0x" ++ showHex (fromIntegral (0-displ)) else " + 0x" ++ showHex displ


-- Render registers in GAS syntax
show_register (RegFPU ST0) = "ST(0)"
show_register (RegFPU ST1) = "ST(1)"
show_register (RegFPU ST2) = "ST(2)"
show_register (RegFPU ST3) = "ST(3)"
show_register (RegFPU ST4) = "ST(4)"
show_register (RegFPU ST5) = "ST(5)"
show_register (RegFPU ST6) = "ST(6)"
show_register (RegFPU ST7) = "ST(7)"
show_register reg = show reg

with_size_directive (BitSize si) s = mk_size_directive si ++ "[" ++ s ++ "]"
 where
  mk_size_directive 0   = ""
  mk_size_directive 8   = "byte ptr "
  mk_size_directive 16  = "word ptr "
  mk_size_directive 32  = "dword ptr "
  mk_size_directive 64  = "qword ptr "
  mk_size_directive 80  = "tword ptr "
  mk_size_directive 128 = "oword ptr "
  mk_size_directive 256 = "yword ptr "
  mk_size_directive 512 = "zword ptr "


-- Symbolization of a RIP-relative operand
symbolize_rip_relative_operand bin ellf i (Op_Mem si (Reg64 RIP) RegNone _ displ Nothing _) = Just $ with_size_directive si $ symbolize_address bin ellf False $ fromIntegral (fromIntegral (inAddress i) + fromIntegral (inSize i) + displ)
symbolize_rip_relative_operand bin ellf i (Op_Mem _ (Reg64 RIP) _ _ _ _ _) = error (show i)
symbolize_rip_relative_operand bin ellf i (Op_Mem _ _ (Reg64 RIP) _ _ _ _) = error (show i)
symbolize_rip_relative_operand bin ellf i (Op_Reg (Reg64 RIP) _) = error (show i)
symbolize_rip_relative_operand bin ellf _ _ = Nothing


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
symbolize_address :: BinaryClass bin => bin -> ELLF -> Bool -> Word64 -> String
symbolize_address bin ellf in_data_section a =
  case find_section_for_address bin a of
    Nothing -> error $ "Cannot find section for address 0x" ++ showHex a ++ "\n" ++ show (binary_get_sections_info bin)
    Just (seg,sec,a0,si,align,flgs) -> symbolize flgs a
 where
  symbolize flgs a
    | SectionIsExecutable `elem` flgs = (if in_data_section then "" else "rip + ") ++ mk_label ellf a
    | otherwise = symbolize_address_in_data_section a


  symbolize_address_in_data_section a = try_GOT_entry a `orElse` try_symbol_LE a

  try_GOT_entry a =
    case find (is_relocated_function $ fromIntegral a) $ IM.assocs $ binary_get_symbol_table bin of
      Just (a',sym@(PointerToExternalFunction f))    -> if in_data_section then Nothing else Just $ "rip + " ++ f ++ "@GOTPCREL"
      Just (a',sym@(PointerToInternalFunction _ _))  -> error $ "TODO: symbolize 0x" ++ showHex a
      Just (a',sym@(PointerToObject o _)) -> if in_data_section then Nothing else Just $ "rip + " ++ o ++ "@GOTPCREL"
      _ -> Nothing

  is_relocated_function a (a',PointerToExternalFunction str) = a == a'
  is_relocated_function a (a',PointerToInternalFunction str _) = a == a'
  is_relocated_function a (a',PointerToObject str _) = a == a'
  is_relocated_function a _                          = False



  try_symbol_LE a = 
    case first_LE a symbol_to_address (concat $ ellf_symbols ellf) of
      Just sym -> (if in_data_section then "" else "rip + ") ++ mk_label_from_symbol ellf sym ++ mk_offset (a - symbol_to_address sym)
      Nothing  -> error $ "Address 0x" ++ showHex a ++ " does not have a symbol."

  mk_offset offset
    | offset == 0 = ""
    | (fromIntegral offset :: Int64) < 0 = " - 0x" ++ showHex (0 - offset)
    | otherwise = " + 0x" ++ showHex offset

symbol_to_address sym =
  case ellf_sym_address sym of
    Nothing -> 0
    Just a  -> a



-- Symbolize the CFI directives
-- Replace each occurrence of @0xDEADBEEF with its symbolization.
cfi_symbolize bin ellf = substitutions split_at_hex symbolize
 where
  symbolize a = symbolize_address bin ellf True (readHex' a)

split_at_hex ('@':'0':'x':str) = Just $ span isHexDigit str
split_at_hex _ = Nothing

-- In the plain-text CFI directives, each occurrence of @0xDEADBEEF is an address.
cfi_get_addresses [] = []
cfi_get_addresses s =
  case split_at_hex s of
    Nothing -> cfi_get_addresses (tail s)
    Just (str0,str1) -> readHex' str0 : cfi_get_addresses str1




substitutions :: (String -> Maybe (String,String)) -> (String -> String) -> String -> String
substitutions pattern replacement [] = []
substitutions pattern replacement s =
  case pattern s of
    Nothing -> head s : substitutions pattern replacement (tail s)
    Just (str0,str1) -> replacement str0 ++ substitutions pattern replacement str1

escape_string :: String -> Builder
escape_string cs = foldMap escape cs
  where
    --escape '\\' = charUtf8 '\\' <> charUtf8 '\\'
    escape '\"' = charUtf8 '\\' <> charUtf8 '\"'
    --escape '\n' = charUtf8 '\\' <> charUtf8 'n'
    --escape '\t' = charUtf8 '\\' <> charUtf8 't'
    escape c    = charUtf8 c


render_list :: String -> [Builder] -> Builder
render_list s []     = mempty
render_list s (b:bs) = b <> mconcat [ stringUtf8 s <> b' | b' <- bs ]

indent n = replicate n ' ' 

first_GE :: Word64 -> (a -> Word64) -> [a] -> Maybe a
first_GE x f [] = Nothing
first_GE x f (a:as)
  | f a >= x  = Just $ first_GE' x a as
  | otherwise = first_GE x f as
 where
  first_GE' x a [] = a
  first_GE' x a (b:bs) 
    | x <= f b && f b < f a = first_GE' x b bs
    | otherwise             = first_GE' x a bs



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


