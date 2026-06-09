{-# LANGUAGE DeriveGeneric #-}

module OutputGeneration.GlobalMemAnalysis where

import Base


import WithAbstractSymbolicValues.Class

import Data.Size
import Data.X86.Register
import Data.X86.Instruction
import Data.GlobalMem
import Data.VerificationCondition

import Binary.Elf
import Binary.Generic

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import qualified Data.Set.NonEmpty as NES
import Data.List
import Data.Word

import Control.Monad.State.Strict hiding (join)

import GHC.Generics (Generic)
import Control.DeepSeq
import qualified Data.Serialize as Cereal
import Debug.Trace




data RegionInfo = Variable Int | RegionInfo Bool Int [(Int,Int)]


show_region_info (a, Variable si) = "VAR_" ++ showHex a ++ "_" ++ show si
show_region_info (a,RegionInfo isDirty si mem_accesses) = "[" ++ showHex a ++ "," ++ show si ++ "]" ++ show_isDirty isDirty ++ ":" ++ insert_new_line (show_mem_accesses mem_accesses)
 where
  show_isDirty True  = "*"
  show_isDirty False = ""

  show_mem_accesses mem_accesses = intercalate "\n" $ map show_mem_access mem_accesses
  show_mem_access (a,si) = "   [" ++ showHex a ++ "," ++ show si ++ "]" 

  insert_new_line "" = ""
  insert_new_line str = "\n" ++ str

analyze_gmem :: WithAbstractSymbolicValues ctxt bin v p => ctxt -> Word64 -> Int -> [GlobalMem v] -> IM.IntMap RegionInfo
analyze_gmem ctxt a0 si0 gmem =
  let gmem_structure  = sget_gmem_structure ctxt
      gmem_structure' = gmem_structure_with gmem_structure $ IS.unions (map gmem_get_structure gmem) 
      gmem'           = IM.unions (map gmem_get_mem_accesses gmem)

      gmem_within_sections           = IM.filterWithKey is_within_section gmem'
      gmem_structure_within_sections = IM.filterWithKey is_within_section gmem_structure'

      regions                        = IM.mapWithKey (get_region_info gmem_structure_within_sections gmem_within_sections) $ gmem_structure_within_sections
      introduce_varables             = IM.mapWithKey mk_variables regions in
    introduce_varables
 where
  is_within_section a _ = fromIntegral a0 <= a && a < fromIntegral a0 + si0
    
  get_region_info gmem_structure gmem a isDirty =
    let si           = get_region_size gmem_structure a
        mem_accesses = get_mem_accesses a si gmem in
      RegionInfo isDirty si mem_accesses

  get_region_size gmem_structure a =
    case IM.lookupGT a gmem_structure of
      Just (a',_) -> a' - a
      Nothing -> (fromIntegral a0+si0)-a

  get_mem_accesses a si gmem = map (\(a,(v,si)) -> (a,si)) $ IM.assocs $ IM.filterWithKey (overlapsWith a si) gmem

  overlapsWith a si a0 (v0,si0) = (a <= a0 && a0 < a + si) || (a0 <= a && a < a0 + si0)

  mk_variables a r@(RegionInfo False si [])
    | si `elem` [1,2,4,8,16] = Variable si
    | otherwise = r
  mk_variables a r@(RegionInfo False si [(a',si')])
    | a == a' && si == si' && si `elem` [1,2,4,8,16] = Variable si
    | otherwise = r
  mk_variables a r = r 




