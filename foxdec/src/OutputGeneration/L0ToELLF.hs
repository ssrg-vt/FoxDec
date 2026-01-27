{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StrictData #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : L0ToELLF
Description : Lift the L0 representation of the binary to ELLF
-}



module OutputGeneration.L0ToELLF where 

import Base
import Config


import Data.CFG
import Data.Size
import Data.SPointer
import Data.SValue
import Data.Int

import Binary.FunctionNames
import Binary.Elf
import Binary.ELLF

import OutputGeneration.ELLF

import Data.X86.Instruction
import Data.L0
import Data.JumpTarget
import Data.Symbol
import Data.Indirection
import Data.X86.Opcode

import WithAbstractSymbolicValues.Class
import Binary.Generic


import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List.Extra (firstJust,replace)
import Data.List 
import Data.Word
import Data.Foldable
import Data.Char 
import Data.Bits (testBit, (.&.))
import Data.List.Split (splitOn)
import Data.ByteString.Internal (w2c)
import Data.Function (on)
import Data.Elf

import Control.Monad.State.Strict
import Data.Functor.Identity
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Unsafe


import Debug.Trace

import GHC.Base hiding (Symbol,foldr)




type LiftedC bin = Lifting bin (Sstate SValue SPointer) (FInit SValue SPointer) SValue

-- | Lift an L0 representation to ELLF
lift_L0_to_ELLF :: BinaryClass bin => LiftedC bin -> ELLF
lift_L0_to_ELLF l@(bin,_,l0) = 
  let Just elf = get_elf bin
      ellf0 = ELLF [[]] [[]] [[]] [[]] [[]] [[ellf_text_section]] [] [[]] Nothing [IM.empty] 
      ellf1 = execState (mk_ellf elf) ellf0 in
    ellf1 { ellf_symb_map = ellf_mk_symb_map (ellf_symbols ellf1) }
 where
  mk_ellf elf = do
    -- Make all functions
    mapM_ (l0_to_ellf_entry l) $ map fromIntegral $ S.toAscList $ l0_get_function_entries l0
    -- Add some symbols
    l0_to_ellf_data_section_symbols elf bin
    -- Add jump tables
    l0_to_ellf_jump_tables l

    -- Make the symbol map
    modify (\ellf -> ellf { ellf_symb_map = ellf_mk_symb_map (ellf_symbols ellf) } )

  ellf_text_section = ELLF_Section 8 0 0 "ELLF_Section" 


add_symbol :: ELLF_Symbol -> State ELLF Word64
add_symbol sym = do
  ellf <- get
  let syms = head (ellf_symbols ellf)
  case elemIndex sym syms of
    Nothing -> do
      put $ ellf { ellf_symbols = [syms ++ [sym]] }
      return $ fromIntegral $ length syms
    Just indx -> return $ fromIntegral indx

add_pointee :: ELLF_Pointee -> State ELLF Word64
add_pointee pte = do
  ellf <- get
  let indx = length $ head $ ellf_pointees ellf
  put $ ellf {ellf_pointees = [head (ellf_pointees ellf) ++ [pte]] }
  return $ fromIntegral indx

add_pointer :: ELLF_Pointer -> State ELLF () 
add_pointer ptr = do
  ellf <- get
  put $ ellf {ellf_pointers = [head (ellf_pointers ellf) ++ [ptr]] }



l0_to_ellf_jump_tables l@(bin,_,l0) = mapM_ mk_jump_table $ S.unions $ IM.elems $ l0_indirections l0
 where
  mk_jump_table (Indirection_JumpTable (JumpTable indx bound trgt tbl base)) = mapM_ (mk_jump_table_entry base) [0..fromIntegral bound]
  mk_jump_table _ = return ()


  mk_jump_table_entry base n = do
    let a = base + 4*n
    let trgt = ELLF_Symbol 0 base (".L_JumpTable_0x" ++ showHex base) 0
    trgt_idx <- add_symbol trgt

    let pte = ELLF_Pointee trgt_idx trgt_idx 0 -- TODO read diff and compute base symbol
    indx <- add_pointee pte
    let ptr = ELLF_Pointer 0 a indx 0
    add_pointer ptr




l0_to_ellf_data_section_symbols elf bin = mapM_ add_symbol data_labels
 where
  data_labels = map mk_ellf_symbol $ filter is_data_label $ IM.assocs $ binary_get_symbol_table bin

  mk_ellf_symbol (a,AddressOfObject o _) = ELLF_Symbol 0 (fromIntegral a) o (fromIntegral a)
  mk_ellf_symbol (a,AddressOfLabel l _)  = ELLF_Symbol 0 (fromIntegral a) l (fromIntegral a)

  is_data_label (a,AddressOfObject o False) = not (is_ellf_special_symbol o) && (is_within_data_section $ fromIntegral a)
  is_data_label (a,AddressOfLabel l False)  = not (is_ellf_special_symbol l) && (is_within_data_section $ fromIntegral a)
  is_data_label _ = False

  is_within_data_section a =
    case find (contains_address a) $ elfSections elf of
      Nothing -> False
      Just s  -> is_relevant_data_section s
  contains_address a section =
    let a0  = elfSectionAddr section
        si0 = elfSectionSize section in
      a0 <= a && a < a0 + si0


  
l0_to_ellf_entry :: BinaryClass bin => LiftedC bin -> Word64 -> State ELLF ()
l0_to_ellf_entry l@(bin,_,l0) entry = do
  ellf <- get
  let bbs0    = ellf_basic_blocks ellf
  let fs0     = ellf_functions ellf
  let f_indx  = length $ head fs0
  let curr_bb = length $ head bbs0


  let Just cfg      = IM.lookup (fromIntegral entry) (l0_get_cfgs l0)
  let ellf_bbs      = map (mk_ellf_bb f_indx) $ sort_bbs $ filter (not . bb_already_present (head bbs0)) $ concatMap split_jumps $ IM.assocs $ cfg_instrs cfg
  let end_bb        = curr_bb + (length ellf_bbs) - 1
  let ellf_function = mk_ellf_function curr_bb $ fromIntegral end_bb 


  if end_bb >= curr_bb && (not $ IM.null $ cfg_instrs cfg) then do
    put $ ellf { ellf_basic_blocks = [head bbs0 ++ ellf_bbs], ellf_functions = [head fs0 ++ [ellf_function]] }
    let sym = ELLF_Symbol 0 entry mk_fun_name entry
    add_symbol sym
    return ()
  else
    return ()
 where
  bb_already_present bbs bb = (inAddress $ head $ snd bb) `elem` map ellf_bb_address bbs

  split_jumps bb@(blockID,instrs) =
    case break (\i -> isJump (inOperation i)) instrs of
      (_, [])     -> [bb]
      (_, [_])    -> [bb]
      (is0,i:is1) -> [(blockID,is0++[i]), (blockID,is1)]


  mk_fun_name =
    let str = function_name_of_entry bin entry in
      if entry == head (binary_entry bin) then
        "_start"
      else if "0x" `isPrefixOf` str then
        "Fun" ++ str
      else
        str

  mk_ellf_function curr_bb end_bb = ELLF_Function mk_fun_name (fromIntegral curr_bb) end_bb entry

  mk_ellf_bb f_indx (blockID,instrs) =
    let a      = inAddress $ head instrs
        offset = a - entry 
        si     = fromIntegral $ sum $ map inSize instrs in
      ELLF_Basic_Block (fromIntegral f_indx) offset si a



  sort_bbs bbs = 
    let bbs0        = sortBy compareAddress bbs
        (bbs1,bbs2) = span beforeEntry bbs0 in
      bbs2 ++ bbs1
  compareAddress (_,is0) (_,is1) = compare (inAddress (head is0)) (inAddress (head is1))
  beforeEntry (_,is) = inAddress (head is) < entry

-- | There is one specific symbol frequently encountered for which we cannot find the appropiate library to load.
-- It is related to debugging information (the -g option of GCC).
-- We therefore pvodie our own implementation: just a dummy, which is what the real function seems to do as well.
__gmon_start_implementation = "void __gmon_start__ () { return; }"



