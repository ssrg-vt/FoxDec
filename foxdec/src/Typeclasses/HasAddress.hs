module Typeclasses.HasAddress (HasAddress(addressof)) where

import           Data.Word (Word64)

class HasAddress a where
  addressof :: a -> Word64