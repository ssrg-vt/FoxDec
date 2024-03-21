{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : NASMToC
Description : Lift the L0 representation of the binary to symbolized and recompilable NASM.
-}



module NASM.NASMToC where


import Base
import Config

import X86.Opcode
import X86.Register (real, Register)
import Generic.HasSize

import NASM.NASM
import Analysis.Context (ctxt_name)


import qualified Data.Set as S
import Data.Word
import Data.List
import Data.Maybe (mapMaybe,fromJust)
import Data.Bits (testBit)
import Data.List.Split (chunksOf)
import Data.ByteString.Internal (w2c)


render_NASM ctxt (NASM externals sections footer) = 
  let (ts,ds) = partition isTextSection sections
      h       = intercalate "\n\n" $ render_externals externals : map render_section ds
      c       = intercalate "\n\n" $ mk_includes : map render_section ts in
    (c, h)
 where
  isTextSection (NASM_Section_Text _) = True
  isTextSection _                     = False


  mk_includes = intercalate "\n"
    [ "#include \"foxdec.h\""
    , "#include \"" ++ ctxt_name ctxt ++ ".h\"" ]

render_externals externals = "" -- intercalate "\n" $ map render_external $ S.toList externals
 where
  render_external f = "void* " ++ f ++ ";"

render_section (NASM_Section_Text text) = render_textsection text
render_section (NASM_Section_Data dat)  = concatMap render_datasection dat

mk_section_name (segment,section,a0) = mk_C_name $ section_name segment section a0

render_datasection (NASM_DataSection sec align labels [DataEntry_BSS si]) = "uint8_t " ++ mk_section_name sec ++ "[" ++ show si ++ "] = {0};"
render_datasection (NASM_DataSection sec align labels dat) = 
  let fields      = mk_fields dat 
      struct      = mk_struct fields
      struct_init = mk_struct_init fields in
    intercalate "\n" 
      [ "// " ++ mk_name
      , struct
      , struct_init
      , "uint8_t* const " ++ mk_name ++ " = (uint8_t*)&" ++ mk_name ++ "_init;"
      -- TODO , show labels
      , "\n\n" ]
 where
  mk_fields [] = []
  mk_fields (DataEntry_Byte a b:dat) =
    let (bs,dat') = takeWhileBytes dat in
      (DataEntry_Byte a b:bs) : mk_fields dat'
  mk_fields (e : dat) = [e] : mk_fields dat


  mk_struct fields = intercalate "\n" 
    [ "struct __attribute__((packed)) " ++ mk_name
    , "{"
    , intercalate "\n" $ map mk_struct_fields fields
    , "};"
    ]
  mk_struct_fields [DataEntry_String a s]    = "  char " ++ mk_name ++ "_0x" ++ showHex a ++ "[" ++ show (length s + 1) ++ "];"
  mk_struct_fields [DataEntry_Pointer a ptr] = "  void* " ++ mk_name ++ "_0x" ++ showHex a ++ ";"
  mk_struct_fields bs@(DataEntry_Byte a _:_) = "  uint8_t " ++ mk_name ++ "_0x" ++ showHex a ++ "[" ++ show (length bs) ++ "];"


  mk_struct_init fields = "struct " ++ mk_name ++ " " ++ mk_name ++ "_init = {\n" ++ intercalate "\n" (map mk_struct_init_fields fields) ++ "\n};"
  mk_struct_init_fields [DataEntry_String _ s]      = "  \"" ++ word8s_to_string s ++ "\","
  mk_struct_init_fields [DataEntry_Pointer _ (a,_)] = "  &" ++ show a ++ ","
  mk_struct_init_fields bs                          = "  {" ++ intercalate "," (map show_byte bs) ++ "},"

  show_byte (DataEntry_Byte _ b) = "0x" ++ showHex b

  takeWhileBytes (DataEntry_Byte a b:dat) =
    let (bs,dat') = takeWhileBytes dat in
      (DataEntry_Byte a b:bs,dat')
  takeWhileBytes dat = ([],dat)

  mk_name = mk_section_name sec

  word8s_to_string = concatMap (escape . w2c) 
  escape '\\' = "\\\\"
  escape '\n'  = "\\n"
  escape '\t'  = "\\t"
  escape c    = [c]


mk_C_name = map repl
 where
  repl '.' = '_'
  repl c   = c

render_textsection (NASM_TextSection f blocks) = intercalate "\n"
  [ "void " ++ mk_C_function_name f ++ "() {"
  , render_blocks blocks
  , "}\n" ]
 where
  mk_C_function_name f
    | head f `elem` "0123456789" = "FUN_" ++ f
    | otherwise = f

render_blocks = intercalate "\n\n" . map render_block 

render_block (blockID,lines) = intercalate "\n" $ map render_line lines

render_line (NASM_Comment _ comment) = "// " ++ comment
render_line (NASM_Label label) = show label ++ ":"
render_line (NASM_Line i@(NASM_Instruction pre (Just m) ops comment annot)) =
  case mnemonic_to_C m ops of
    Just strs -> intercalate "\n" $ map add_colon strs
    Nothing  -> "/// " ++ show i

 where
  add_colon str = "  " ++ str ++ ";"



data TypeMode = UInt | SInt | Double | Pointer | Unknown
 deriving (Eq,Show)

get_size :: NASM_Operand -> Maybe Int
get_size (NASM_Operand_Address (NASM_Address Nothing Nothing 1 (Just base) Nothing Nothing Nothing))  = Just $ sizeof base
get_size (NASM_Operand_Address (NASM_Address Nothing Nothing 1 Nothing Nothing Nothing (Just fun)))   = Just $ 8
get_size (NASM_Operand_Address (NASM_Address Nothing Nothing 1 Nothing Nothing (Just label) Nothing)) = Just $ 8
get_size (NASM_Operand_EffectiveAddress _) = Just 8
get_size (NASM_Operand_Memory (si,_) _) = Just si
get_size (NASM_Operand_Immediate _) = Nothing
get_size a = error $ show a

 


mnemonic_to_C m = mk m
 where
  mk MOV    [op0,op1] = Just $ mk_assign (nasm_operand_to_c_destination op0) UInt (render_operand op1 UInt)
  mk MOVAPS [op0,op1] = Just $ mk_assign (nasm_operand_to_c_destination op0) UInt (render_operand op1 UInt)
  mk LEA    [op0,op1] = Just $ mk_assign (nasm_operand_to_c_destination op0) Pointer (render_operand op1 Pointer)

  mk MOVZX  [op0,op1] = Just $ mk_assign (nasm_operand_to_c_destination op0) UInt (render_operand op1 UInt `castToType` (UInt,fromJust $ get_size op0))
  mk MOVSX  [op0,op1] = Just $ mk_assign (nasm_operand_to_c_destination op0) SInt (render_operand op1 SInt `castToType` (SInt,fromJust $ get_size op0))
  mk CDQE    []       = Just $ [ "RAX.s = (int64_t)(read32(RAX).s)" ]

  mk SETNE  [op0]     = Just $ mk_assign (nasm_operand_to_c_destination op0) UInt (flag_to_byte "!ZF")

  mk ADD    [op0,op1] = Just $ mk_src_from_dests "+"  (nasm_operand_to_c_destination op0) op0 op1 UInt
  mk SUB    [op0,op1] = Just $ mk_src_from_dests "-"  (nasm_operand_to_c_destination op0) op0 op1 UInt
  mk AND    [op0,op1] = Just $ mk_src_from_dests "&"  (nasm_operand_to_c_destination op0) op0 op1 UInt 
  mk SHR    [op0,op1] = Just $ mk_src_from_dests ">>" (nasm_operand_to_c_destination op0) op0 op1 UInt
  mk SAR    [op0,op1] = Just $ mk_src_from_dests ">>" (nasm_operand_to_c_destination op0) op0 op1 SInt
  mk XOR    [op0,op1] 
    | op0 == op1 = Just $ mk_assign (nasm_operand_to_c_destination op0) UInt "0"
    | otherwise  = Just $ mk_src_from_dests "^" (nasm_operand_to_c_destination op0) op0 op1 UInt

  mk PUSH    [op0] = Just $ mk_push op0
  mk POP     [op0] = Just $ mk_pop op0
  mk LEAVE   []    = Just $ mk_leave
  mk RET     []    = Just $ mk_ret


  mk CALL    [op0] = Just $ mk_call op0
  mk JMP     [op0] = Just $ ["goto " ++ render_operand op0 Unknown]
  mk JE      [op0] = Just $ [mk_cond_jump "ZF" op0 ]
  mk JNE     [op0] = Just $ [mk_cond_jump "!ZF" op0 ]
  mk JG      [op0] = Just $ [mk_cond_jump "GF" op0 ]
  mk JLE     [op0] = Just $ [mk_cond_jump "!GF" op0 ]
  mk JL      [op0] = Just $ [mk_cond_jump "!GF && !ZF" op0 ]

  mk CMP     [op0,op1] = Just $ mk_cmp op0 op1
  mk TEST    [op0,op1] = Just $ mk_test op0 op1

  mk NOP     _         = Just $ []
  mk HLT     _         = Just $ ["abort()"]
  mk _ _               = Nothing


data C_Dest = Register Register | Flag String | Operand NASM_Operand

nasm_operand_to_c_destination (NASM_Operand_Address (NASM_Address Nothing Nothing 1 (Just base) Nothing Nothing Nothing)) = Register base
nasm_operand_to_c_destination a = Operand a

mk_call (NASM_Operand_Address (NASM_Address Nothing Nothing 1 Nothing Nothing Nothing (Just fun)))   = ["CALL(" ++ fun ++ ")"]
mk_call (NASM_Operand_Address (NASM_Address Nothing Nothing 1 Nothing Nothing (Just label) Nothing)) = ["RSP.u -= 8", show label ++ "()"]
mk_call op0 = ["//TODO: call to " ++ render_operand op0 Unknown]

mk_push op0 =
  let Just si = get_size op0 in
    [ "RSP.u -= " ++ show si
    , "*((uint" ++ show (si*8) ++ "_t*) RSP.ptr) = " ++ render_operand op0 UInt
    ]

mk_pop op0 =
  let Just si = get_size op0 in
    [ render_operand op0 UInt ++ " = *((uint" ++ show (si*8) ++ "_t*) RSP.ptr)"
    , "RSP.u += " ++ show si
    ]

mk_leave =
  [ "RSP.u = RBP.u"
  , "RBP.u = *((uint64_t*) RSP.ptr)"
  , "RSP.u += 8" ]

mk_ret = 
  [ "RSP.u += 8" ]

mk_assign (Register reg) tm src
  | real reg == reg || sizeof reg == 16 = [ reg `regWithTypeMode` tm ++ " = " ++ src ]
  | otherwise = [ write_to_reg reg src ]
mk_assign (Operand dst) tm src = [ render_operand dst tm ++ " = " ++ src ]
mk_assign (Flag flg) tm src = [ flg ++ " = " ++ src ]

mk_src_from_dests operation dst op0 op1 tm = mk_assign dst tm (render_operand op0 tm ++ " " ++ operation ++ " " ++ render_operand op1 tm)

mk_cmp op0 op1 = map mk
  [ ("ZF", "==", UInt)
  , ("GF", ">", SInt)]
 where
  mk (flg,op,tm) = concat $ mk_src_from_dests op (Flag flg) op0 op1 tm


mk_test op0 op1
  | op0 == op1 = [ "ZF = (" ++ render_operand op0 UInt ++ " == 0)" ]

flag_to_byte flg = "(uint8_t)(" ++ flg ++ " ? 1 : 0)"

mk_cond_jump flg trgt = "if (" ++ flg ++ ") goto " ++ render_operand trgt Pointer


write_to_reg reg val = "write" ++ show (8 * sizeof reg) ++ "(" ++ show (real reg) ++ ", (" ++ castTo val ("union d" ++ show (8 * sizeof reg)) ++ "))"
read_from_reg reg = "read" ++ show (8 * sizeof reg) ++ "(" ++ show (real reg) ++ ")"


-- Produce a C expression that is of the type coorespodning to the type mode
render_operand (NASM_Operand_Address (NASM_Address Nothing Nothing 1 (Just base) Nothing Nothing Nothing))    tm = base `regWithTypeMode` tm
render_operand (NASM_Operand_Address (NASM_Address Nothing Nothing 1 Nothing  Nothing Nothing (Just fun)))    tm = fun `castToType` (tm,8)
render_operand (NASM_Operand_Address (NASM_Address Nothing Nothing 1 Nothing  Nothing (Just label) Nothing))  tm = show label

--render_operand (NASM_Operand_Memory _ (NASM_Address Nothing Nothing 1 Nothing  Nothing (Just label) Nothing)) tm = label
render_operand (NASM_Operand_EffectiveAddress a) tm = render_address a
render_operand (NASM_Operand_Memory (si,_) a) tm    = "(*" ++ (render_address a `castToTypePtr` (tm,si)) ++ ")"
render_operand (NASM_Operand_Immediate imm) tm      = ("0x" ++ showHex imm) -- `castToType` tm
render_operand op tm = error $ show op

regWithTypeMode reg tm
  | real reg /= reg && sizeof reg /= 16 = read_from_reg reg ++ accessor tm
  | otherwise                           = show reg ++ accessor tm
 where
  accessor UInt    = ".u"
  accessor SInt    = ".s"
  accessor Pointer = ".ptr"
  accessor Double  = ".d"
  accessor Unknown = ""

castToType a (UInt,si)    = castTo a $ mk_uint_ty si
castToType a (SInt,si)    = castTo a $ mk_sint_ty si
castToType a (Pointer,si) = castTo a $ "void*"
castToType a (Double,si)  = castTo a $ "double"
castToType a (Unknown,si) = a

castToTypePtr a (UInt,si)    = castTo a $ mk_uint_ty si ++ "*"
castToTypePtr a (SInt,si)    = castTo a $ mk_sint_ty si ++ "*"
castToTypePtr a (Pointer,si) = castTo a $ "void**"
castToTypePtr a (Double,si)  = castTo a $ "double*"
castToTypePtr a (Unknown,si) = a

mk_uint_ty si = "uint" ++ show (si*8) ++ "_t"
mk_sint_ty si = "int" ++ show (si*8) ++ "_t"

castTo a ty = "(" ++ ty ++ ")(" ++ a ++ ")"


render_address a@(NASM_Address Nothing Nothing 1 (Just base) displ Nothing Nothing)           = base `regWithTypeMode` Pointer ++ show_displacement displ
render_address a@(NASM_Address Nothing Nothing 1 Nothing Nothing (Just label) Nothing)        = render_label label
render_address a@(NASM_Address Nothing Nothing 1 Nothing Nothing Nothing (Just fun))          = fun
render_address a@(NASM_Address (Just seg) indx scale base displ label fun)                    = seg `regWithTypeMode` Pointer ++ show_displacement displ
render_address a@(NASM_Address Nothing (Just indx) scale Nothing Nothing Nothing Nothing)     = (indx `regWithTypeMode` SInt ++ "*" ++ show scale) `castToType` (Pointer,8)
render_address a@(NASM_Address Nothing (Just indx) scale (Just base) Nothing Nothing Nothing) = base `regWithTypeMode` Pointer ++ " + " ++ ((indx `regWithTypeMode` SInt) ++ "*" ++ show scale) `castToType` (Pointer,8)

render_address a@(NASM_Address seg indx scale base displ label fun) = error $ show a


show_displacement Nothing     = ""
show_displacement (Just 0)   = ""
show_displacement (Just imm) 
  | testBit (fromIntegral imm::Word64) 63 = " - 0x" ++ showHex (0 - imm)
  | otherwise =  " + 0x" ++ showHex imm


render_label (Label str) = str
render_label (Macro segment section a0 offset) = mk_section_name (segment,section,a0) ++ show_offset offset
 where
  show_offset 0 = ""
  show_offset offset = " + " ++ show offset

