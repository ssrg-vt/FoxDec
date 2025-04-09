{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StrictData, DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : NASM
Description : A datastructure for storing NASM code.
-}



module OutputGeneration.NASM.Abstract_ASM where

import Base
import Data.X86.Opcode
import Data.X86.Instruction
import Data.Symbol
import Data.Size
import Data.JumpTarget
import Binary.Generic
import Data.X86.Register

import OutputGeneration.NASM.NASM

import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put,encode)

import qualified Data.Set as S
import Data.Word
import Data.List
import Data.Bits (testBit)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.ByteString.Internal (w2c)



ai_show_NASM l (NASM exts globals sections footer) = intercalate "\n\n\n" $ filter ((/=) []) render_sections
 where
  render_sections  = map render_section sections

  render_section (NASM_Section_Text ts) = ai_show_NASM_TextSection ts
  render_section (NASM_Section_Data ds) = []


ai_show_NASM_DataSection (NASM_DataSection (seg,sec,a0) align entries) = "section " ++ sec ++ show_align align ++ " ; @" ++ showHex a0 ++ "\n"  ++ (intercalate "\n" $ map show_entry entries)
 where
  show_entry (a,e@(DataEntry_String _ _)) = show e ++ "; @ " ++ showHex a
  show_entry (a,e) = show e

  show_align 0 = ""
  show_align n = " align=" ++ show n



ai_show_NASM_TextSection (NASM_TextSection f blocks _) = comment_block ["Function: " ++ f] ++ intercalate "\n\n" (map render_block blocks) ++ "\n\n"
 where
  render_block (blockID,lines) = intercalate "\n" $ map ai_show_NASM_Line lines

  comment str = "; " ++ str

  comment_block strs = intercalate "\n" $ comment_block_delim strs : (map comment strs ++ [comment_block_delim strs,""])
  comment_block_delim strs = comment $ replicate (length $ max strs) '-'
  max strs = maximumBy compare_lengths strs
  compare_lengths str0 str1 = compare (length str0) (length str1)



ai_show_NASM_Line (NASM_Line i) = "  " ++ ai_show_NASM_Instruction i
ai_show_NASM_Line (NASM_Label str) = show str ++ ":"
ai_show_NASM_Line (NASM_Comment indent str) = replicate indent ' ' ++ "; " ++ str


ai_show_NASM_Instruction i@(NASM_Instruction pre m ops comment annot Nothing)     = ai_show_instruction i
ai_show_NASM_Instruction i@(NASM_Instruction pre (Just m) ops comment annot (Just orig))
    | isRelevant m = ai_show_instruction i
    | otherwise =
      case canonicalize orig of
        [ Instruction _ _ _ Nothing _ _ ] -> "NOP ;" ++ show m
        [ Instruction _ _ _ (Just dst) _ _ ] -> show dst ++ " <- Top ;" ++ show m
        _ -> error $ show i
 where
  isRelevant m = isJump m || isCall m || isCondJump m || m `S.member` relevantMnemonics || take 3 (show m) == "MOV" || take 3 (show m) == "CMOV"
  relevantMnemonics = S.fromList [PUSH,POP,MOV,ADD,MOV,LEA,LEAVE,CMP,TEST,SUB,RET]



ai_show_instruction (NASM_Instruction pre m ops comment annot _) = concat
  [ intercalate " " $ filter ((/=) "") [ show_prefix pre, show_mnemonic m, show_ops ops]
  , mk_comment ]
 where
    show_prefix Nothing  = ""
    show_prefix (Just PrefixRep) = "REPZ"
    show_prefix (Just PrefixRepNE) = "REPNE"
    show_prefix (Just p) = show p

    show_mnemonic Nothing  = ""
    -- NOT NEEDED, JUST FOR EASY DIFF
    show_mnemonic (Just JZ) = "JE"
    show_mnemonic (Just JNZ) = "JNE"
    show_mnemonic (Just SETNBE) = "SETA"
    show_mnemonic (Just CMOVZ) = "CMOVE"
    show_mnemonic (Just CMOVNZ) = "CMOVNE"
    show_mnemonic (Just SETNLE) = "SETG"
    show_mnemonic (Just p) = show p

    show_ops = intercalate ", " . map show_op

    mk_comment =
      let str = render_annot annot in
        if comment == [] && str == [] then ""
        else if comment /= [] && str /= [] then " ; " ++ comment ++ "    ; " ++ str
        else if comment == [] then "    ; " ++ str
        else " ; " ++ comment

    instr_op_size =
      case partition isImmediate ops of
        (_,(op:_))  -> operand_size op
        ([imm],[])  -> operand_size imm

    isImmediate (NASM_Operand_Immediate _) = True
    isImmediate _                          = False

    show_op (NASM_Operand_Reg r)              = show r
    show_op (NASM_Operand_Address a)          = show a
    show_op (NASM_Operand_EffectiveAddress a) = "[" ++ show a ++ "]"
    show_op (NASM_Operand_Memory sizedir a)   = show_nasm_sizedir sizedir ++ " [" ++ show a ++ "]"
    show_op (NASM_Operand_Immediate (Immediate (BitSize si) imm)) =
      case (instr_op_size,si) of
        (ByteSize 16,64) -> "0x" ++ showHex imm
        (ByteSize 16,32) -> "0x" ++ showHex imm
        (ByteSize 16,16) -> "0x" ++ showHex imm
        (ByteSize 16, 8) -> "0x" ++ showHex imm

        (ByteSize 8,64) -> "0x" ++ showHex imm
        (ByteSize 8,32) -> "0x" ++ showHex (sextend_32_64 imm)
        (ByteSize 8,16) -> "0x" ++ showHex (sextend_16_64 imm)
        (ByteSize 8, 8) -> "0x" ++ showHex (sextend_8_64 imm)

        (ByteSize 4, 64) -> "0x" ++ showHex imm
        (ByteSize 4, 32) -> "0x" ++ showHex imm
        (ByteSize 4, 16) -> "0x" ++ showHex (sextend_16_32 imm)
        (ByteSize 4,  8) -> "0x" ++ showHex (sextend_8_32 imm)

        (ByteSize 2, 64) -> "0x" ++ showHex imm
        (ByteSize 2, 32) -> "0x" ++ showHex imm
        (ByteSize 2, 16) -> "0x" ++ showHex imm
        (ByteSize 2,  8) -> "0x" ++ showHex (sextend_8_16 imm)

        (ByteSize 1,  64) -> "0x" ++ showHex imm
        (ByteSize 1,  32) -> "0x" ++ showHex imm
        (ByteSize 1,  16) -> "0x" ++ showHex imm
        (ByteSize 1,   8) -> "0x" ++ showHex imm


        (ByteSize si0,si1) -> error $ show (si0,si1)

    operand_size (NASM_Operand_Reg r) = regSize r
    operand_size (NASM_Operand_Address a) = ByteSize 8
    operand_size (NASM_Operand_EffectiveAddress a) = ByteSize 8
    operand_size (NASM_Operand_Memory (si,_) a) = ByteSize si
    operand_size (NASM_Operand_Immediate (Immediate (BitSize si) imm)) = ByteSize $ si `div` 8

    show_nasm_sizedir (_,False) = ""
    show_nasm_sizedir (1,_) = "byte"
    show_nasm_sizedir (2,_) = "word"
    show_nasm_sizedir (4,_) = "dword"
    show_nasm_sizedir (8,_) = "qword"
    show_nasm_sizedir (10,_) = "tword"
    show_nasm_sizedir (16,_) = "oword"
