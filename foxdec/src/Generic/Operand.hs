{-# LANGUAGE DeriveGeneric #-}

module Generic.Operand (GenericOperand(..)) where

import           Generic.Address (GenericAddress)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import qualified Data.Serialize as Cereal
import           Base (showHex)
import           Typeclasses.HasSize (HasSize(sizeof))

-- | A generic statepart, based on polymorphic type `storage`.
data GenericOperand storage =
    Memory (GenericAddress storage) Int -- ^ A region in memory, whose address is stored in the given state part and whose size in bytes is given in the Int
  | EffectiveAddress (GenericAddress storage)     -- ^ An address itself, but not the value stored at the address.
  | Storage storage                      -- ^ A storage location such as a register or a variable
  | Immediate Word64                       -- ^ An immediate value
  deriving (Eq, Ord, Generic)

instance (Cereal.Serialize storage) => Cereal.Serialize (GenericOperand storage)

instance Show storage => Show (GenericOperand storage) where
  show (EffectiveAddress addr) = "[" ++ show addr ++ "]"
  show (Storage st) = show st
  show (Immediate imm) = showHex imm
  show (Memory addr si) =
    showSize si ++ " [" ++ show addr ++ "," ++ show si ++ "]"
    where
      showSize 1 = "BYTE PTR"
      showSize 2 = "WORD PTR"
      showSize 4 = "DWORD PTR"
      showSize 8 = "QWORD PTR"
      showSize 16 = "XMMWORD PTR"
      showSize 32 = "YMMWORD PTR"
      showSize si = show (si * 8) ++ " PTR"

instance (HasSize storage) => HasSize (GenericOperand storage) where
  sizeof (Storage r) = sizeof r
  sizeof (Memory _ si) = si
  sizeof (EffectiveAddress _) = 8
  sizeof (Immediate _) = 8

instance Functor GenericOperand where
  fmap f (Memory addr size) = Memory (f <$> addr) size
  fmap f (EffectiveAddress addr) = EffectiveAddress $ f <$> addr
  fmap f (Storage s) = Storage $ f s
  fmap _ (Immediate imm) = Immediate imm