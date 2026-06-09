{-# LANGUAGE DeriveGeneric#-}


module Data.FInit where

import Base


import Data.X86.Register

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.IntMap.Internal as IM (lookupLT, lookupGT)
import qualified Data.Set.NonEmpty as NES
import Data.Word
import Data.List

import GHC.Generics (Generic)
import Control.Monad.State.Strict hiding (join)

import qualified Data.Serialize as Cereal hiding (get,put)
import Control.DeepSeq
import GHC.Generics



-- | A statepart is either a register or a region in memory
data SStatePart p =
    SSP_Reg Register -- ^ A register
  | SSP_Mem p Int    -- ^ A region with a symbolic address and an immediate size.
 deriving (Eq, Ord, Generic)


instance (Cereal.Serialize p) => Cereal.Serialize (SStatePart p)
instance (NFData p) => NFData (SStatePart p)



instance Show p => Show (SStatePart p) where
  show (SSP_Reg r)        = show r
  show (SSP_Mem a si)     = "[" ++ show a ++ ", " ++ show si ++ "]"



-- | A function initialisation consists of a mapping of state parts to values, and memory relations
data MemRelation = Separate | Aliassing | Unknown
  deriving (Generic,Eq,Ord,Show)


data FInit v p = FInit (S.Set (SStatePart p,v)) (M.Map (SStatePart p,SStatePart p) MemRelation)
  deriving (Generic,Eq,Ord)


instance Cereal.Serialize MemRelation
instance (Cereal.Serialize v,Cereal.Serialize p, Ord p,Ord v) => Cereal.Serialize (FInit v p)

instance NFData MemRelation
instance (NFData p,NFData v) => NFData (FInit v p)


empty_finit = FInit S.empty M.empty



-- | Show function initialisation
instance (Eq v, Show v, Show p) => Show (FInit v p) where
 show (FInit sps m) = intercalate "\n" $ filter ((/=) []) $ 
  [ intercalate "\n" $ map show_sp_v $ S.toList sps
  , intercalate "\n" $ map show_entry $ M.toList m ]
  where
    show_sp_v (sp,v) = show_sp sp ++ " === " ++ show v
    show_sp (SSP_Reg r) = show r
    show_sp (SSP_Mem a si) = "*[" ++ show a ++ "," ++ show si ++ "]"
    show_entry ((sp0,sp1),r) = show (sp0,sp1) ++ ": " ++ show r


