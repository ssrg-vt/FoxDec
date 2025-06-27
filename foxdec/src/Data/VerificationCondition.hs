{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module Data.VerificationCondition where

import Base

import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Data.Word
import Data.List

import qualified Data.Serialize as Cereal hiding (get,put)
import Control.DeepSeq
import GHC.Generics


data PointerAnalysisResult v = PointerAnalysisResult {
  pa_mem_write :: [Maybe v],
  pa_mem_reads :: [Maybe v]
 }
  deriving (Generic,Eq,Ord)

instance Show v => Show (PointerAnalysisResult v) where
  show (PointerAnalysisResult []        rs) = show_pars rs
  show (PointerAnalysisResult [Nothing] rs) = "_ <-- " ++ show_pars rs
  show (PointerAnalysisResult [Just w]  rs) = show w ++ " <-- " ++ show_pars rs
  show (PointerAnalysisResult ws rs)        = show_pars ws ++ " <-- " ++ show_pars rs


show_pars pars = "[" ++ intercalate "," (map show_pa pars) ++ "]"
 where
  show_pa Nothing  = "_"
  show_pa (Just v) = show v

data VerificationCondition v =
    FunctionPointers      Word64 IS.IntSet -- ^ A set of function pointers passed to a function
  | PointerAnalysis       Word64 (PointerAnalysisResult v) -- ^ A pointer analysis result for the instruction at the address
  deriving (Generic,Eq,Ord)

instance Show v => Show (VerificationCondition v) where
  show (FunctionPointers a ptrs) = "@" ++ showHex a ++ ": function pointer loaded " ++ showHex_set ptrs
  show (PointerAnalysis a r)     = "@" ++ showHex a ++ ": " ++ show r



instance Cereal.Serialize v => Cereal.Serialize (PointerAnalysisResult v)
instance Cereal.Serialize v => Cereal.Serialize (VerificationCondition v)

instance NFData v => NFData (PointerAnalysisResult v)
instance NFData v => NFData (VerificationCondition v)

type VCS v = S.Set (VerificationCondition v)



gather_pa_results :: VCS v -> IM.IntMap (PointerAnalysisResult v)
gather_pa_results = foldr gather IM.empty
 where
  gather (PointerAnalysis a par) = IM.insert (fromIntegral a) par
  gather _  = id
