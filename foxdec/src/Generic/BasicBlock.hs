{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Generic.BasicBlock (GenericBasicBlock(..)) where

import           Data.List (intercalate)
import           GHC.Generics (Generic)
import qualified Data.Serialize as Cereal

newtype GenericBasicBlock instr = BasicBlock { instructions :: [instr] }
  deriving (Eq, Ord, Functor, Foldable, Traversable, Applicative, Monad
          , Generic)

instance (Cereal.Serialize instr) => Cereal.Serialize (GenericBasicBlock instr)

instance (Show instr) => Show (GenericBasicBlock instr) where
  show BasicBlock { instructions } = intercalate "\n" (show <$> instructions)
