{-# LANGUAGE DeriveGeneric #-}


module Data.GlobalMem where


import Config
import Base


import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.Word
import Data.List

import qualified Data.Serialize as Cereal hiding (get,put)
import Control.DeepSeq
import GHC.Generics

data MemAccess p v = Stores v Int | SpansTo Int
 deriving (Eq, Ord, Generic)

instance (Show p, Show v) => Show (MemAccess p v) where
  show (Stores v si) = "Stores " ++ show v ++ "  " ++ show si
  show (SpansTo a)   = "SpansTo " ++ showHex a

data GlobalMem p v = GlobalMem (IM.IntMap (MemAccess p v))
  deriving (Eq,Ord,Generic)


instance (Show p,Show v) => Show (GlobalMem p v) where
  show (GlobalMem m) = intercalate "\n" $ map show_entry $ IM.toList m
   where
    show_entry (a,Stores v si) = "[" ++ showHex a ++ "," ++ show si ++ "] := " ++ show v 
    show_entry (a,SpansTo a') = "[" ++ showHex a ++ " --> " ++ showHex a' ++ "]" --  stores at " ++ show p ++ ": " ++ show v



instance (Cereal.Serialize v,Cereal.Serialize p) => Cereal.Serialize (MemAccess p v)
instance (Cereal.Serialize v,Cereal.Serialize p) => Cereal.Serialize (GlobalMem p v)

instance (NFData v, NFData p) => NFData (MemAccess p v)
instance (NFData v, NFData p) => NFData (GlobalMem p v)


show_gmem_structure (GlobalMem gmem) = intercalate "\n" $ map show_entry $ IM.assocs gmem
 where
  show_entry (a,Stores v si) = "[" ++ showHex a ++ "," ++ show si ++ "]"
  show_entry (a, SpansTo a') = "[" ++ showHex a ++ " --> " ++ showHex a' ++ "]"
