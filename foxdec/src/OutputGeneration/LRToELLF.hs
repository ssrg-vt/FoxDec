{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StrictData #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : LRToELLF
Description : Lift the L0 representation of the binary to ELLF
-}



module OutputGeneration.LRToELLF where 

import Config
import Base

import Binary.FunctionNames
import Binary.Elf
import Binary.ELLF


import Data.X86.Instruction
import Data.JumpTarget
import Data.Symbol
import Data.Indirection

import Binary.Generic
import InputLifting.Lift
import InputLifting.Types
import InputLifting.ControlFlowGraph

import OutputGeneration.ELLF

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
import Control.Monad.Reader


import Debug.Trace




-- | Lift an LR to ELLF
lift_LR_to_ELLF :: BinaryClass bin => LiftedRepresentationFunctions bin -> ELLF
lift_LR_to_ELLF l =
  let funcs    = lrf_cfgs l
      ellf0    = ELLF [[]] [[]] [[]] [[]] [[]] [[ellf_text_section]] [] [[]] Nothing [IM.empty] 
      ellf1    = execState (mk_ellf funcs) ellf0 in
    ellf1 { ellf_symb_map = ellf_mk_symb_map (ellf_symbols ellf1) }
 where
  mk_ellf funcs = do
    -- Make all functions
    mapM_ (lr_to_ellf_entry l) $ IM.toAscList funcs
    -- Add some symbols
    lr_to_ellf_data_section_symbols l
    -- Add jump tables
    lr_to_ellf_jump_tables l

    -- Make the symbol map
    modify (\ellf -> ellf { ellf_symb_map = ellf_mk_symb_map (ellf_symbols ellf) } )

  ellf_text_section = ELLF_Section 8 0 0 "ELLF_Section" 


add_symbol :: BinaryClass bin => LiftedRepresentationFunctions bin -> ELLF_Symbol -> State ELLF Word64
add_symbol l sym = do
  ellf <- get
  let syms = head (ellf_symbols ellf)
  case elemIndex sym syms of
    Nothing -> do
      put $ ellf { ellf_symbols = [syms ++ [sym]] }
      return $ fromIntegral $ length syms
    Just indx -> return $ fromIntegral indx

add_pointee :: BinaryClass bin => LiftedRepresentationFunctions bin -> ELLF_Pointee -> State ELLF Word64
add_pointee l pte = do
  ellf <- get
  let indx = length $ head $ ellf_pointees ellf
  put $ ellf {ellf_pointees = [head (ellf_pointees ellf) ++ [pte]] }
  return $ fromIntegral indx

add_pointer :: BinaryClass bin => LiftedRepresentationFunctions bin -> ELLF_Pointer -> State ELLF () 
add_pointer l ptr = do
  ellf <- get
  put $ ellf {ellf_pointers = [head (ellf_pointers ellf) ++ [ptr]] }


lr_to_ellf_jump_tables l = do
  let inds = lrf_indirections l
  mapM_ mk_jump_table $ IM.toAscList inds
 where
  mk_jump_table (a,ResolvedJumpTable base bound) = mapM_ (mk_jump_table_entry base) [0..bound]
  mk_jump_table _ = return ()


  mk_jump_table_entry base n = do
    let a = base + 4*n
    let trgt = ELLF_Symbol 0 base (".L_JumpTable_0x" ++ showHex base) 0
    trgt_idx <- add_symbol l trgt

    let pte = ELLF_Pointee trgt_idx trgt_idx 0 -- TODO read diff and compute base symbol
    indx <- add_pointee l pte
    let ptr = ELLF_Pointer 0 a indx 0
    add_pointer l ptr



lr_to_ellf_data_section_symbols l = mapM_ (add_symbol l) data_labels
 where
  data_labels = map mk_ellf_symbol $ filter is_data_label $ IM.assocs $ binary_get_symbol_table bin

  mk_ellf_symbol (a,AddressOfObject o _) = ELLF_Symbol 0 (fromIntegral a) o (fromIntegral a)
  mk_ellf_symbol (a,AddressOfLabel l _)  = ELLF_Symbol 0 (fromIntegral a) l (fromIntegral a)

  is_data_label (a,AddressOfObject o False) = not (is_ellf_special_symbol o) && (is_within_data_section $ fromIntegral a)
  is_data_label (a,AddressOfLabel l False)  = not (is_ellf_special_symbol l) && (is_within_data_section $ fromIntegral a)
  is_data_label _ = False

  is_within_data_section a =
    case find (elf_section_contains_address a) $ elfSections elf of
      Nothing -> False
      Just s  -> is_relevant_data_section s
 
  Just elf = get_elf bin
  bin = lrf_binary l


  
lr_to_ellf_entry :: BinaryClass bin => LiftedRepresentationFunctions bin ->(Int,ControlFlowGraph) -> State ELLF ()
lr_to_ellf_entry l (entry,cfg) = do
  ellf       <- get
  let bin     = lrf_binary l
  let bbs0    = ellf_basic_blocks ellf
  let fs0     = ellf_functions ellf
  let f_indx  = length $ head fs0
  let curr_bb = length $ head bbs0

  if IM.null $ cfg_basic_blocks cfg then
    return ()
  else do
    let (bbs_before,Just bb_at,bbs_after) = IM.splitLookup entry $ cfg_basic_blocks cfg
    let bbs_ordered = [(entry,bb_at)] ++ IM.toAscList bbs_after ++ IM.toAscList bbs_before
    new_bbs    <- mapM (mk_ellf_bb f_indx) bbs_ordered
    let end_bb  = curr_bb + (length new_bbs) - 1
    let fname   = mk_fun_name bin $ concat fs0
    let new_function = mk_ellf_function fname curr_bb $ fromIntegral end_bb 

    if end_bb >= curr_bb && (not $ IM.null $ cfg_basic_blocks cfg) then do
      put $ ellf { ellf_basic_blocks = [head bbs0 ++ new_bbs], ellf_functions = [head fs0 ++ [new_function]] }
      let sym = ELLF_Symbol 0 (fromIntegral entry) fname $ fromIntegral entry
      add_symbol l sym
      return ()
    else
      return ()
 where
  mk_fun_name bin fs0 =
    let str = function_name_of_entry bin $ fromIntegral entry in
      if fromIntegral entry == head (binary_entry bin) then
        "_start"
      else if "0x" `isPrefixOf` str then
        "Fun" ++ str
      else if already_a_function_name fs0 str then
        str ++ "_0x" ++ showHex entry
      else
        str

  already_a_function_name fs f = any (\(ELLF_Function name _ _ _) -> name == f) fs

  mk_ellf_function fname curr_bb end_bb = ELLF_Function fname (fromIntegral curr_bb) end_bb $ fromIntegral entry

  mk_ellf_bb f_indx (blockID,instrs) = do
    let a      = fromIntegral $ inAddress $ head instrs
    let offset = a - fromIntegral entry 
    let si     = fromIntegral $ sum $ map inSize instrs
    let new_bb = ELLF_Basic_Block (fromIntegral f_indx) offset si a
    return new_bb


-- | There is one specific symbol frequently encountered for which we cannot find the appropiate library to load.
-- It is related to debugging information (the -g option of GCC).
-- We therefore pvodie our own implementation: just a dummy, which is what the real function seems to do as well.
__gmon_start_implementation = "void __gmon_start__ () { return; }"



