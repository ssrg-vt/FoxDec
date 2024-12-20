{-# LANGUAGE DeriveGeneric #-}

module Generic.Address (GenericAddress(..), AddressWord64(..)) where

import           Data.Word (Word64)
import           GHC.Generics (Generic)
import qualified Data.Serialize as Cereal
import           Base (showHex)
import           Control.DeepSeq

-- | A type for encapsulating an immediate (allows to always show hex)
newtype AddressWord64 = AddressWord64 Word64
  deriving (Eq, Ord, Generic)

instance Cereal.Serialize AddressWord64
instance NFData AddressWord64


-- | An unresolved address, within the operand of an instruction, based on polymorphic type `storage`.
data GenericAddress storage =
    AddressStorage storage                                         -- ^ Reading a pointer from a storage
  | AddressImm Word64                                              -- ^ Immediate value 
  | AddressMinus (GenericAddress storage) (GenericAddress storage) -- ^ Minus
  | AddressPlus (GenericAddress storage) (GenericAddress storage)  -- ^ Plus
  | AddressTimes (GenericAddress storage) (GenericAddress storage) -- ^ Times
  deriving (Eq, Ord, Generic)

instance (Cereal.Serialize storage) => Cereal.Serialize (GenericAddress storage)
instance (NFData storage) => NFData (GenericAddress storage)

instance Show storage => Show (GenericAddress storage) where
  show (AddressStorage st) = show st
  show (AddressImm imm) = showHex imm
  show (AddressMinus a0 a1) = show a0 ++ " - " ++ show a1
  show (AddressPlus a0 a1) = show a0 ++ " + " ++ show a1
  show (AddressTimes a0 a1) = show a0 ++ " * " ++ show a1

instance Show AddressWord64 where
  show (AddressWord64 a) = showHex a

instance Functor GenericAddress where
  fmap f (AddressStorage s) = AddressStorage $ f s
  fmap f (AddressImm val) = AddressImm val
  fmap f (AddressMinus a1 a2) = AddressMinus (f <$> a1) (f <$> a2)
  fmap f (AddressPlus a1 a2) = AddressPlus (f <$> a1) (f <$> a2)
  fmap f (AddressTimes a1 a2) = AddressTimes (f <$> a1) (f <$> a2)
