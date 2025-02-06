{-# LANGUAGE DeriveGeneric, DefaultSignatures, StrictData, StandaloneDeriving, BangPatterns #-}

{-|
Module      : SValue
Description : 
-}

module Data.SValue where

import Data.SymbolicExpression

import Base

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Word 
import Data.List
import GHC.Generics
import qualified Data.Serialize as Cereal
import Data.Bits (testBit)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Foldable as F
import Control.DeepSeq




data SValue = SConcrete (NES.NESet SimpleExpr) | SAddends (NES.NESet SimpleExpr) | Top
 deriving (Generic,Eq,Ord)



-- | A `symbolic value` is either a `pointer value` (high certainty that it is actually a pointer),
-- or a non-deterministic set of concrete expressions, or computed from a set of possible addends.
--data SValue      = SPointer (NES.NESet PtrValue) | SConcrete (NES.NESet SimpleExpr) | SAddends (NES.NESet SAddend) | Top
--  deriving (Eq,Ord,Generic)

instance Cereal.Serialize SValue
instance NFData SValue
  

instance Show SValue where
  show Top             = "top" 
  show (SConcrete es) 
    | NES.size es == 1 = show $ NES.findMin es 
    | otherwise        = show_set $ neSetToList es
  show (SAddends adds) = "{" ++ (intercalate "," $ map show_adds $ neSetToList adds) ++"}"
   where
    show_adds a = "(" ++ show a ++ "+..)"






