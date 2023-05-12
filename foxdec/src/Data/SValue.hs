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


-- | A `pointer value` may have an `pointer offset` that is either a known fixed immediate or unknown
data PtrOffset   = PtrOffset Word64 | UnknownOffset
  deriving (Eq,Ord,Generic)

-- | A `pointer value` consists of a `pointer base` and a `pointer offset`
data PtrValue    =
    Base_StackPointer String PtrOffset                   -- ^ The stackpointer of the given function
  | Base_Immediate Word64                                -- ^ An immediate pointer
  | Base_Section Word64                                  -- ^ A pointer to anywhere in the given section
  | Base_Malloc (Maybe Word64) (Maybe String) PtrOffset  -- ^ A malloc return value
  | Base_FunctionPtr Word64 String PtrOffset             -- ^ A function pointer (external)
  | Base_ReturnAddr String                               -- ^ The return address of the given function
  | Base_TLS PtrOffset                                   -- ^ The Thread Local Storage
  | Base_StatePart StatePart PtrOffset                   -- ^ The value initially stored in some statepart
  deriving (Eq,Ord,Generic)

data PtrBase =
    PtrBase_StackPointer String
  | PtrBase_Immediate Word64
  | PtrBase_Section Word64
  | PtrBase_Malloc (Maybe Word64) (Maybe String)
  | PtrBase_FunctionPtr Word64 String
  | PtrBase_ReturnAddr String
  | PtrBase_TLS 
  | PtrBase_StatePart StatePart 
  | PtrBaseUnknown  -- TODO add SimpleExpr
 deriving (Generic,Eq,Ord)


data SAddend = Addend_Base PtrBase | Addend_StatePart StatePart
  deriving (Eq,Ord,Generic)

-- | A `symbolic value` is either a `pointer value` (high certainty that it is actually a pointer),
-- or a non-deterministic set of concrete expressions, or computed from a set of possible addends.
data SValue      = SPointer (NES.NESet PtrValue) | SConcrete (NES.NESet SimpleExpr) | SAddends (NES.NESet SAddend) | Top
  deriving (Eq,Ord,Generic)

instance Cereal.Serialize PtrOffset
instance NFData PtrOffset
instance Cereal.Serialize PtrValue
instance NFData PtrValue
instance Cereal.Serialize PtrBase
instance NFData PtrBase
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


instance Show PtrValue where
  show (Base_StackPointer f  offset) = "PRSP_" ++ f ++ show offset
  show (Base_Immediate i)            = "P0x" ++ showHex i
  show (Base_Section i)              = "Section@0x" ++ showHex i
  show (Base_Malloc id h     offset) = "P"++(show $ SE_Malloc id h) ++ show offset
  show (Base_FunctionPtr _ f offset) = "&" ++ f
  show (Base_ReturnAddr f)           = "ReturnAddress_" ++ f
  show (Base_StatePart sp    offset) = show sp ++ "_0" ++ show offset
  show (Base_TLS             offset) = "&TLS" ++ show offset

instance Show PtrBase where
  show (PtrBase_StackPointer f)  = "RSP_" ++ f
  show (PtrBase_Immediate i)     = "P0x" ++ showHex i
  show (PtrBase_Section i)       = "Section@0x" ++ showHex i
  show (PtrBase_Malloc id h)     = "P"++(show $ SE_Malloc id h)
  show (PtrBase_FunctionPtr _ f) = "&" ++ f
  show (PtrBase_ReturnAddr f)    = "ReturnAddress_" ++ f
  show (PtrBase_StatePart sp)    = show sp ++ "_0"
  show (PtrBase_TLS)             = "&TLS"
  show PtrBaseUnknown            = "PtrBaseUnknown"



instance Show SAddend where
  show (Addend_Base b)       = show b
  show (Addend_StatePart sp) = show sp ++ "_0"


instance Show SValue where
  show (SPointer ptrs) = show_set (map show $ neSetToList ptrs)
  show (SConcrete es)  = show_set (map show $ neSetToList es)
  show (SAddends adds) = "{" ++ intercalate "+" (map show$ neSetToList adds) ++ "+...}" 
  show Top             = "top"

show_set [str] = str
show_set strs  = "{" ++ intercalate "," strs ++ "}"

isAddendBase (Addend_Base _) = True
isAddendBase _ = False

isImmediate (SPointer ptrs) = all isImmediatePtr ptrs
isImmediate (SConcrete es)  = all isImmediateExpr es
isImmediate _ = False

isImmediatePtr (Base_Immediate i) = True
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
has_unknown_offset (Base_Immediate i)                   = False
has_unknown_offset (Base_Section i)                     = True
has_unknown_offset (Base_Malloc id h     UnknownOffset) = True
has_unknown_offset (Base_FunctionPtr _ _ UnknownOffset) = True
has_unknown_offset (Base_ReturnAddr f)                  = False
has_unknown_offset (Base_TLS             UnknownOffset) = True
has_unknown_offset (Base_StatePart sp    UnknownOffset) = True
has_unknown_offset _                                    = False

liftOffsetMod m UnknownOffset = UnknownOffset
liftOffsetMod m (PtrOffset i) = PtrOffset $ m i

mod_offset m (Base_StackPointer f  offset) = Base_StackPointer f $ liftOffsetMod m $ offset 
mod_offset m (Base_Section i)              = Base_Section i
mod_offset m (Base_Malloc id h     offset) = Base_Malloc id h $ liftOffsetMod m $ offset 
mod_offset m (Base_FunctionPtr a f offset) = Base_FunctionPtr a f $ liftOffsetMod m $ offset
mod_offset m (Base_TLS             offset) = Base_TLS $ liftOffsetMod m $ offset 
mod_offset m (Base_StatePart sp    offset) = Base_StatePart sp $ liftOffsetMod m $ offset 
mod_offset m b                             = error $ "Modding offset of: " ++ show b ++ "   " ++ show (m 42)

get_base_of_spointer (Base_StackPointer f _)  = PtrBase_StackPointer f
get_base_of_spointer (Base_Immediate i)       = PtrBase_Immediate i
get_base_of_spointer (Base_Section i)         = PtrBase_Section i
get_base_of_spointer (Base_Malloc id h _)     = PtrBase_Malloc id h
get_base_of_spointer (Base_FunctionPtr a f _) = PtrBase_FunctionPtr a f
get_base_of_spointer (Base_TLS _)             = PtrBase_TLS
get_base_of_spointer (Base_ReturnAddr f)      = PtrBase_ReturnAddr f
get_base_of_spointer (Base_StatePart sp _)    = PtrBase_StatePart sp

svalue_to_bases fctxt (SPointer ptrs) = NES.map get_base_of_spointer ptrs
svalue_to_bases fctxt (SAddends adds) = 
  let bs = NES.filter isPtrBase adds in
    if S.null bs then NES.singleton PtrBaseUnknown else NES.unsafeFromSet $ S.map getPtrBase bs
 where
  getPtrBase (Addend_Base b) = b
  isPtrBase (Addend_Base b) = True
  isPtrBase _ = False
svalue_to_bases _ _ = NES.singleton PtrBaseUnknown


