{-# LANGUAGE DeriveGeneric, StrictData, StandaloneDeriving #-}

module Data.CFI where

import Base

import Data.Binary.Get as G


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Int
import Data.Word 
import Data.List
import Data.List.Extra (firstJust)

import qualified Data.ByteString as BS


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

data CFI = CFI {
  cfi_directives :: IM.IntMap [String], -- ^ Mapping from addresses to rendered CFI directives
  cfi_gcc_except_tables :: IM.IntMap GCC_Except_Table, -- ^ Mapping from addresses to gcc except tables
  cfi_all_addresses :: IS.IntSet -- ^ All addresses used in the CFI directives + tables
 }

get_fde_encoding (CIE _ _ _ _ _ dats _) = firstJust get_fde dats
 where
  get_fde (FDE_Encoding enc) = Just enc
  get_fde _  = Nothing

get_lsda_ptr (CFI_FDE (FDE _ pc_begin _ (Just p) _)) = Just (pc_begin,p)
get_lsda_ptr _ = Nothing

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
  | DW_CFA_remember_state
  | DW_CFA_restore_state
  | DW_CFA_Escape_0x2E Word64 -- likely DW_CFA_GNU_args_size
 deriving (Show,Eq,Ord) 



data GCC_Except_Table_Action = GCC_Except_Table_Action {
  gcc_except_table_action_type_filter ::Int64,
  gcc_except_table_action_type_next_action ::Int64,
  gcc_except_table_action_type_size :: Int,
  gcc_except_table_action_type_address :: Word64,
  gcc_except_table_action_type_next_address :: Word64
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


get_addresses_from_gcc_except_table t = IS.unions $ (map get_addresses_call_site (call_sites t)) ++ [IS.fromList $ map to_value $ type_infos t]
 where
  get_addresses_call_site (GCC_Except_Table_CallSite (start1,start0) (len1,len0) (lp1,lp0) action) = IS.fromList $ map to_value [start1,start0,len1,len0,lp1,lp0]
  to_value (Absolute a) = fromIntegral a
  to_value (Indirect a) = fromIntegral a

get_landing_pads_from_gcc_except_table t = IS.unions $ map get_landing_pads_call_site (call_sites t)
 where
  get_landing_pads_call_site (GCC_Except_Table_CallSite (start1,start0) (len1,len0) (lp1,lp0) action) 
    | lp0 == lp1 = IS.empty
    | otherwise = IS.fromList $ map to_value [lp1]
  to_value (Absolute a) = fromIntegral a
  to_value (Indirect a) = fromIntegral a


get_callsite_regions_from_gcc_except_table t = map get_region_from_call_site (call_sites t)
 where
  get_region_from_call_site (GCC_Except_Table_CallSite (start1,start0) (len1,len0) (lp1,lp0) action) = (to_value len1,to_value len0, to_value lp1,action)
  to_value (Absolute a) = fromIntegral a
  to_value (Indirect a) = fromIntegral a

get_callsite_region_starts_from_gcc_except_table = IS.fromList . map (\(_,start,_,_) -> start) . get_callsite_regions_from_gcc_except_table
get_callsite_region_ends_from_gcc_except_table = IS.fromList . map (\(end,_,_,_) -> end) . get_callsite_regions_from_gcc_except_table
