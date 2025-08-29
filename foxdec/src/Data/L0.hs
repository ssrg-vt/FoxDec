{-# LANGUAGE DeriveGeneric #-}


module Data.L0 where


import Config
import Base

import Data.Indirection
import Data.CFG
import Data.VerificationCondition
import Data.GlobalMem 

import Binary.Generic


import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Word
import Data.List
import Data.Maybe

import qualified Data.Serialize as Cereal hiding (get,put)
import Control.DeepSeq
import GHC.Generics


data Postcondition pred = ReturnsWith pred | HasUnresolvedIndirections [Int] | Terminates | TimeOut | VerificationError [(Int,pred)]
  deriving (Generic,Eq)

instance Show pred => Show (Postcondition pred) where
  show (ReturnsWith p) = "returns with postcondition\n" ++ show p
  show (HasUnresolvedIndirections blockIDs) = "has unresolved indirections at blocks " ++ show blockIDs
  show (Terminates) = "terminates"
  show (TimeOut) = "TimeOut"
  show (VerificationError bs) = " returns with a verification error:\n" ++ intercalate "\n" (map show_error bs)
   where
    show_error (blockID,p) = "BlockID: " ++ show blockID ++ "\n" ++ show p 


data FResult pred v = FResult {
  result_cfg   :: CFG,                                 -- The control flow graph
  result_post  :: Postcondition pred,                  -- The postcondition
  result_join  :: Maybe pred,                          -- The join of all postconditions, even if terminating or unresolved
  result_calls :: S.Set Word64,                        -- All function calls of the current function
  result_vcs   :: S.Set (VerificationCondition v),     -- Side effects observed during verification
  result_pa    :: IM.IntMap (PointerAnalysisResult v)  -- The results of the pointer analysis per instruciton address (as Int)
 }
 deriving Generic


data L0 pred finit v = L0 {
  l0_functions :: IM.IntMap (finit,Maybe (FResult pred v)),
  l0_indirections :: IM.IntMap Indirections,
  l0_gmem_structure :: GMemStructure,
  l0_time :: String
 }
 deriving Generic

l0_insert_new_entry entry finit (L0 fs inds gmem_structure time) = L0 (IM.alter alter entry fs) inds gmem_structure time
 where
  alter Nothing           = Just (finit,Nothing)
  alter (Just (_,result)) = Just (finit,result)

l0_adjust_result entry result (L0 fs inds gmem_structure time) = L0 (IM.adjust (\(finit,_) -> (finit,result)) (fromIntegral entry) fs) inds gmem_structure time

l0_lookup_entry entry (L0 fs inds gmem_structure time) = IM.lookup (fromIntegral entry) fs


l0_set_gmem_structure gmem_structure (L0 fs inds _ time) = L0 fs inds gmem_structure time

l0_lookup_indirection a (L0 fs inds gmem_structure time) = IM.lookup (fromIntegral a) inds

l0_insert_indirection a ind (L0 fs inds gmem_structure time) = L0 fs (IM.insert (fromIntegral a) ind inds) gmem_structure time

l0_lookup_join (L0 fs inds gmem_structure time) entry = fromJust $ result_join $ fromJust $ snd $ fs IM.! entry 

l0_lookup_finit (L0 fs inds gmem_structure time) entry = fst $ fs IM.! entry

l0_lookup_result entry (L0 fs inds gmem_structure time) =
  case IM.lookup (fromIntegral entry) fs of
    Nothing -> Nothing
    Just (_,r) -> r


empty_result :: FResult pred v
empty_result = FResult (init_cfg 0) TimeOut Nothing S.empty S.empty IM.empty





l0_get_cfgs :: L0 pred finit v -> IM.IntMap CFG
l0_get_cfgs = IM.map get_cfg . l0_functions
 where
  get_cfg (_,Just (FResult cfg _ _ _ _ _)) = cfg

l0_get_function_entries :: L0 pred finit v -> S.Set Int
l0_get_function_entries = S.fromList . IM.keys . l0_functions

l0_get_pars :: L0 pred finit v -> IM.IntMap (IM.IntMap (PointerAnalysisResult v))
l0_get_pars = IM.map get_par . l0_functions
 where
  get_par (_,Just (FResult _ _ _ _ _ pa)) = pa



type Lifting bin pred finit v = (bin, Config, L0 pred finit v)
type LiftingEntry bin pred finit v = (bin, Config, L0 pred finit v,Word64)

instance (Cereal.Serialize pred) => Cereal.Serialize (Postcondition pred)
instance (Cereal.Serialize v,Cereal.Serialize pred,Ord v) => Cereal.Serialize (FResult pred v)
instance (Cereal.Serialize pred, Cereal.Serialize finit,Cereal.Serialize v, Ord v) => Cereal.Serialize (L0 pred finit v)

instance (NFData pred) => NFData (Postcondition pred)
instance (NFData v, NFData pred) => NFData (FResult pred v)
instance (NFData pred,NFData finit,NFData v) => NFData (L0 pred finit v)

show_indirections :: IM.IntMap Indirections -> String
show_indirections = intercalate "\n" . map show_entry . IM.assocs
 where
  show_entry (a,inds) 
    | S.size inds == 1 = showHex a ++ ": " ++ (show $ S.findMin inds)
    | otherwise        = showHex a ++ ": " ++ (show $ S.toList inds)
