{-# LANGUAGE DeriveGeneric #-}

module Generic.Address (GenericAddress(..)) where

import           Data.Word (Word64)
import           GHC.Generics (Generic)
import qualified Data.Serialize as Cereal
import           Base (showHex)

-- | An unresolved address, within the operand of an instruction, based on polymorphic type `storage`.
data GenericAddress storage =
    AddressStorage storage                                          -- ^ Reading a pointer from a storage
  | AddressImm Word64                                               -- ^ Immediate value 
  | AddressMinus (GenericAddress storage) (GenericAddress storage)  -- ^ Minus
  | AddressPlus (GenericAddress storage) (GenericAddress storage)  -- ^ Plus
  | AddressTimes (GenericAddress storage) (GenericAddress storage)  -- ^ Times
  deriving (Eq, Ord, Generic)

instance (Cereal.Serialize storage) => Cereal.Serialize (GenericAddress storage)

instance Show storage => Show (GenericAddress storage) where
  show (AddressStorage st) = show st
  show (AddressImm imm) = showHex imm
  show (AddressMinus a0 a1) = show a0 ++ " - " ++ show a1
  show (AddressPlus a0 a1) = show a0 ++ " + " ++ show a1
  show (AddressTimes a0 a1) = show a0 ++ " * " ++ show a1