{-# LANGUAGE DeriveGeneric, DefaultSignatures, Strict, StandaloneDeriving, BangPatterns #-}

{-|
Module      : SPointer
Description : 
-}

module Data.SPointer where

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

data SPointer = 
    Ptr_Concrete SimpleExpr
  | Ptr_Base SimpleExpr
  | Ptr_Top
  deriving (Eq,Ord,Generic)


instance Show SPointer where
  show (Ptr_Concrete a) = show a
  show (Ptr_Base b)     = show b ++ "+..."
  show Ptr_Top          = "top"







instance Cereal.Serialize SPointer
instance NFData SPointer
