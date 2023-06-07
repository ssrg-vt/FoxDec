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


-- | A `pointer value` may have a `pointer offset` that is either a known fixed immediate or unknown
data PtrOffset   = PtrOffset Word64 | UnknownOffset
  deriving (Eq,Ord,Generic)

-- | A `pointer value` consists of a `pointer base` and a `pointer offset`
data SPointer    =
    Base_StackPointer String PtrOffset                   -- ^ The stackpointer of the given function
  | Base_Immediate Word64 PtrOffset                      -- ^ An immediate pointer
  | Base_Malloc (Maybe Word64) (Maybe String) PtrOffset  -- ^ A malloc return value
  | Base_FunctionPtr Word64 String PtrOffset             -- ^ A function pointer (external)
  | Base_ReturnAddr String                               -- ^ The return address of the given function
  | Base_TLS PtrOffset                                   -- ^ The Thread Local Storage
  | Base_StatePart StatePart PtrOffset                   -- ^ The value initially stored in some statepart
  | Base_FunctionReturn String PtrOffset                 -- ^ The return value of a function
  deriving (Eq,Ord,Generic)

-- | A `pointer addend` consists of a `pointer base`, some constant relativ to the initial state, or the return value
-- of some function.
data SAddend =
    SAddend_StackPointer String
  | SAddend_Immediate Word64
  | SAddend_Malloc (Maybe Word64) (Maybe String)
  | SAddend_FunctionPtr Word64 String
  | SAddend_ReturnAddr String
  | SAddend_TLS 
  | SAddend_StatePart StatePart 
  | SAddend_FunctionReturn String
 deriving (Generic,Eq,Ord)


data SValue = SConcrete (NES.NESet SimpleExpr) | SAddends (NES.NESet (NES.NESet SAddend)) | Top
 deriving (Generic,Eq,Ord)



-- | A `symbolic value` is either a `pointer value` (high certainty that it is actually a pointer),
-- or a non-deterministic set of concrete expressions, or computed from a set of possible addends.
--data SValue      = SPointer (NES.NESet PtrValue) | SConcrete (NES.NESet SimpleExpr) | SAddends (NES.NESet SAddend) | Top
--  deriving (Eq,Ord,Generic)

instance Cereal.Serialize PtrOffset
instance NFData PtrOffset
instance Cereal.Serialize SPointer
instance NFData SPointer
instance Cereal.Serialize SAddend
instance NFData SAddend
instance Cereal.Serialize SValue
instance NFData SValue
  
instance Show PtrOffset where
 show (PtrOffset i)
   | i == 0       = ""
   | testBit i 63 = " - 0x" ++ showHex (0 - i)
   | otherwise    = " + 0x" ++ showHex i
 show _           = " + Top"


instance Show SPointer where
  show (Base_StackPointer f  offset)  = "RSP_" ++ f ++ show offset
  show (Base_Immediate i     offset)  = "0x" ++ showHex i ++ show offset
  show (Base_Malloc id h     offset)  = (show $ SE_Malloc id h) ++ show offset
  show (Base_FunctionPtr _ f offset)  = "&" ++ f
  show (Base_ReturnAddr f)            = "ReturnAddress_" ++ f
  show (Base_StatePart sp    offset)  = show sp ++ "_0" ++ show offset
  show (Base_TLS             offset)  = "&TLS" ++ show offset
  show (Base_FunctionReturn f offset) = f ++ "()" ++ show offset

instance Show SAddend where
  show (SAddend_StackPointer f)   = "RSP_" ++ f
  show (SAddend_Immediate i)      = "P0x" ++ showHex i
  show (SAddend_Malloc id h)      = "P"++(show $ SE_Malloc id h)
  show (SAddend_FunctionPtr _ f)  = "&" ++ f
  show (SAddend_ReturnAddr f)     = "ReturnAddress_" ++ f
  show (SAddend_StatePart sp)     = show sp ++ "_0"
  show (SAddend_TLS)              = "&TLS"
  show (SAddend_FunctionReturn f) = f ++ "()"




instance Show SValue where
  show (SConcrete es)  = show_set (map show $ neSetToList es)
  show (SAddends adds) = show_set (map show_addend $ neSetToList adds)
   where
    show_addend adds = intercalate "+" (map show $ neSetToList adds) ++ "+..."
  show Top             = "top"

show_set [str] = str
show_set strs  = "{" ++ intercalate "," strs ++ "}"

isImmediateBase (SAddend_Immediate _) = True
isImmediateBase _ = False


isConcrete (SConcrete es) = True
isConcrete _ = False






has_unknown_offset (Base_StackPointer f   UnknownOffset) = True
has_unknown_offset (Base_Immediate i      UnknownOffset) = False
has_unknown_offset (Base_Malloc id h      UnknownOffset) = True
has_unknown_offset (Base_FunctionPtr _ _  UnknownOffset) = True
has_unknown_offset (Base_ReturnAddr f)                   = False
has_unknown_offset (Base_TLS              UnknownOffset) = True
has_unknown_offset (Base_StatePart sp     UnknownOffset) = True
has_unknown_offset (Base_FunctionReturn f UnknownOffset) = True
has_unknown_offset _                                     = False

liftOffsetMod m UnknownOffset = UnknownOffset
liftOffsetMod m (PtrOffset i) = PtrOffset $ m i

mod_offset m (Base_StackPointer f   offset) = Base_StackPointer f $ liftOffsetMod m $ offset 
mod_offset m (Base_Malloc id h      offset) = Base_Malloc id h $ liftOffsetMod m $ offset 
mod_offset m (Base_FunctionPtr a f  offset) = Base_FunctionPtr a f $ liftOffsetMod m $ offset
mod_offset m (Base_TLS              offset) = Base_TLS $ liftOffsetMod m $ offset 
mod_offset m (Base_StatePart sp     offset) = Base_StatePart sp $ liftOffsetMod m $ offset 
mod_offset m (Base_FunctionReturn f offset) = Base_FunctionReturn f $ liftOffsetMod m $ offset 
mod_offset m b                              = error $ "Modding offset of: " ++ show b ++ "   " ++ show (m 42)



