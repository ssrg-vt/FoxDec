{-# LANGUAGE DeriveGeneric, DefaultSignatures, Strict, StandaloneDeriving, BangPatterns #-}

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

data PtrOffset   = PtrOffset Word64 | UnknownOffset
  deriving (Eq,Ord,Generic)

data PtrValue     =
    Base_StackPointer String PtrOffset
  | Base_Immediate Word64 -- TODO REMOVE!
  | Base_Section Word64
  | Base_Malloc (Maybe Word64) (Maybe String) PtrOffset
  | Base_FunctionPtr Word64 String
  | Base_ReturnAddr String
  | Base_TLS PtrOffset
  | Base_StatePart StatePart PtrOffset
  deriving (Eq,Ord,Generic)

data SValue      = SPointer (NES.NESet PtrValue) | SConcrete (NES.NESet SimpleExpr) | SAddends (NES.NESet StatePart) | Top
  deriving (Eq,Ord,Generic)

instance Cereal.Serialize PtrOffset
instance NFData PtrOffset
instance Cereal.Serialize PtrValue
instance NFData PtrValue
instance Cereal.Serialize SValue
instance NFData SValue
  
instance Show PtrOffset where
 show (PtrOffset i)
   | i == 0       = ""
   | testBit i 63 = " - 0x" ++ showHex (0 - i)
   | otherwise    = " + 0x" ++ showHex i
 show _           = " + Top"

instance Show PtrValue where
  show (Base_StackPointer f  offset) = "RSP_" ++ f ++ show offset
  show (Base_Immediate i)            = "0x" ++ showHex i
  show (Base_Section i)              = "Section@0x" ++ showHex i
  show (Base_Malloc id h     offset) = (show $ SE_Malloc id h) ++ show offset
  show (Base_FunctionPtr _ f)        = "&" ++ f
  show (Base_ReturnAddr f)           = "ReturnAddress_" ++ f
  show (Base_StatePart sp    offset) = show sp ++ "_0" ++ show offset
  show (Base_TLS             offset) = "&TLS" ++ show offset


instance Show SValue where
  show (SPointer ptrs) = show_set (map show $ neSetToList ptrs)
  show (SConcrete es)  = show_set (map show $ neSetToList es)
  show (SAddends adds) = "{" ++ intercalate "+" (map show$ neSetToList adds) ++ "+...}" 
  show Top             = "top"

show_set [str] = str
show_set strs  = "{" ++ intercalate "," strs ++ "}"



isImmediate (SPointer ptrs) = all isImmediatePtr ptrs
isImmediate (SConcrete es)  = all isImmediateExpr es
isImmediate _ = False

isImmediatePtr (Base_Immediate i)     = True
isImmediatePtr _ = False

isImmediateExpr (SE_Immediate _) = True
isImmediateExpr _ = False


isConcrete (SConcrete es) = True
isConcrete _ = False

isPointer (SPointer _) = True
isPointer _ = False

isSectionPtr (Base_Section a0) = True
isSectionPtr _ = False




has_unknown_offset (Base_StackPointer f  UnknownOffset) = True
has_unknown_offset (Base_Section i)                     = True
has_unknown_offset (Base_Malloc id h     UnknownOffset) = True
has_unknown_offset (Base_StatePart sp    UnknownOffset) = True
has_unknown_offset (Base_TLS             UnknownOffset) = True
has_unknown_offset _                                    = False

liftOffsetMod m UnknownOffset = UnknownOffset
liftOffsetMod m (PtrOffset i) = PtrOffset $ m i

mod_offset (Base_StackPointer f offset) m = Base_StackPointer f $ liftOffsetMod m $ offset 
mod_offset (Base_Section i)             m = Base_Section i
mod_offset (Base_Malloc id h    offset) m = Base_Malloc id h $ liftOffsetMod m $ offset 
mod_offset (Base_FunctionPtr a f)       m = Base_Section a
mod_offset (Base_StatePart sp   offset) m = Base_StatePart sp $ liftOffsetMod m $ offset 
mod_offset (Base_TLS            offset) m = Base_TLS $ liftOffsetMod m $ offset 
mod_offset b                            m = error $ "Modding offset of: " ++ show b ++ "   " ++ show (m 42)
