{-# LANGUAGE DeriveGeneric #-}


module Data.Size where

import qualified Data.Serialize as Cereal hiding (get,put)
import Control.DeepSeq
import GHC.Generics


data BitSize = BitSize Int
  deriving (Show, Eq, Ord, Generic)

data ByteSize = ByteSize Int
  deriving (Show, Eq, Ord, Generic)

byteSize (ByteSize si) = si









instance Cereal.Serialize BitSize
instance Cereal.Serialize ByteSize

instance NFData BitSize
instance NFData ByteSize

