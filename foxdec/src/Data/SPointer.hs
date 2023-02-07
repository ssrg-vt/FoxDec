{-# LANGUAGE DeriveGeneric, DefaultSignatures, Strict, StandaloneDeriving, BangPatterns #-}

{-|
Module      : SPointer
Description : 
-}

module Data.SPointer where

import Data.SymbolicExpression

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
    Concrete (NES.NESet SimpleExpr)
  | Bases (NES.NESet PointerBase)
  | Sources (NES.NESet BotSrc)
  | Top
  deriving (Eq,Ord,Generic)

--TODO move to Base
show_set :: (Foldable t,Show a) => t a -> String
show_set as = "{" ++ intercalate ", " (fmap show $ F.toList as) ++ "}" 

instance Show SPointer where
  show (Concrete es)
    | NES.size es == 1  = show $ NES.findMin es
    | otherwise         = show_set es ++ "C"
  show (Bases bs)       = show_set bs ++ "B"
  show (Sources srcs)   = show_set srcs ++ "S"
  show Top              = "top"



isConcrete (Concrete _) = True
isConcrete _            = False




instance Cereal.Serialize SPointer
instance NFData SPointer
