{-# LANGUAGE DeriveGeneric, DefaultSignatures, Strict, StandaloneDeriving #-}

{-|
Module      : SValue
Description : 
-}

module Data.SValue2 where

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


data PtrValue  =
    Base_StackPointer String
  | Base_Section Word64
  | Base_Malloc (Maybe Word64) (Maybe String)
  | Base_TLS
  | Base_StatePart StatePart
  deriving (Eq,Ord,Generic)

data SValue = SPointer (NES.NESet PtrValue) | SConcrete (NES.NESet SimpleExpr) | SAddends (NES.NESet StatePart) | Top
  deriving (Eq,Ord,Generic)

instance Cereal.Serialize PtrValue
instance NFData PtrValue
instance Cereal.Serialize SValue
instance NFData SValue

instance Show PtrValue where
  show (Base_StackPointer f) = "RSP_" ++ f ++ " + ..."
  show (Base_Section i)      = "Section@0x" ++ showHex i
  show (Base_Malloc id h)    = (show $ SE_Malloc id h) ++ " + ..."
  show (Base_StatePart sp)   = show sp ++ "_0" ++ " + ..."
  show (Base_TLS)            = "&TLS" ++ " + ..."


instance Show SValue where
  show (SPointer ptrs) = show_set (map show $ neSetToList ptrs)
  show (SConcrete es)  = show_set (map show $ neSetToList es)
  show (SAddends adds) = "{" ++ intercalate "+" (map show$ neSetToList adds) ++ "+...}" 
  show Top             = "top"

show_set [str] = str
show_set strs  = "{" ++ intercalate "," strs ++ "}"



isImmediate (SConcrete es)  = all isImmediateExpr es
isImmediate _ = False

isImmediateExpr (SE_Immediate _) = True
isImmediateExpr _ = False


isConcrete (SConcrete es) = True
isConcrete _ = False

isPointer (SPointer _) = True
isPointer _ = False

isSectionPtr (Base_Section a0) = True
isSectionPtr _ = False


isStackPointer (Base_StackPointer f) = True
isStackPointer _ = False

