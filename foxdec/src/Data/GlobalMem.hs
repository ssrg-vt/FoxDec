{-# LANGUAGE DeriveGeneric #-}


module Data.GlobalMem where


import Config
import Base


import Data.Word
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

import qualified Data.Serialize as Cereal hiding (get,put)
import Control.DeepSeq
import GHC.Generics

type GMemStructure = IM.IntMap Bool

data GlobalMem v = GlobalMem (IM.IntMap (v,Int)) IS.IntSet
  deriving (Eq,Ord,Generic)



show_gmem_entry a (v,si) = "[" ++ showHex a ++ "," ++ show si ++ "] := " ++ show v

gmem_structure_with gmem_structure as = IM.unionWith (||) (IM.fromSet (const True) as) gmem_structure

instance (Show v) => Show (GlobalMem v) where
  show m = show_gmem m IM.empty

show_gmem (GlobalMem m as) gmem_structure = intercalate "\n" $ map show_entry $ sortBy compare_entry $ IM.assocs (IM.mapWithKey mk_entry' (gmem_structure_with gmem_structure as)) ++ IM.assocs (IM.mapWithKey show_gmem_entry m)
  where
   compare_entry (a0,v0) (a1,v1)
     | a0 == a1  = labelsFirst v0 v1
     | otherwise = compare a0 a1

   labelsFirst v0 v1
     | head v0 == '[' && head v1 /= '[' = GT
     | head v0 /= '[' && head v1 == '[' = LT
     | otherwise = compare v0 v1
    

   mk_entry' a True    = showHex a ++ "(*):"
   mk_entry' a False   = showHex a ++ ":"

   show_entry (a,str) = str  


instance (Cereal.Serialize v) => Cereal.Serialize (GlobalMem v)

instance (NFData v) => NFData (GlobalMem v)


empty_gmem_structure = IM.empty

gmem_get_structure    (GlobalMem m ls) = ls
gmem_get_mem_accesses (GlobalMem m ls) = m
