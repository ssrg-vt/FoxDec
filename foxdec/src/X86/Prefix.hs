{-# LANGUAGE DeriveGeneric #-}

module X86.Prefix (Prefix(..)) where

import GHC.Generics (Generic)
import qualified Data.Serialize as Cereal

data Prefix = InvalidPrefix | REP | REPZ | REPNE | LOCK | BND
  deriving (Show,Eq,Ord,Read,Generic)

instance Cereal.Serialize Prefix