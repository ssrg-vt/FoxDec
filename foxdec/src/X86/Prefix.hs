{-# LANGUAGE DeriveGeneric #-}

module X86.Prefix (Prefix(..)) where

import GHC.Generics (Generic)
import qualified Data.Serialize as Cereal
import Control.DeepSeq

data Prefix = InvalidPrefix | REP | REPZ | REPNE | LOCK | BND | NOTRACK
  deriving (Show,Eq,Ord,Read,Generic)

instance Cereal.Serialize Prefix
instance NFData Prefix
