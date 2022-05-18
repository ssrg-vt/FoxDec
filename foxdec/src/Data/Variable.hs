{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Variable (Variable(..), VariableConversion(..), isSSA) where

import           Data.Maybe (isJust)
import           GHC.Generics (Generic)
import           Base (orElse)
import qualified Data.Serialize as Cereal

-- | A mutable or immutable variable
data Variable =
  Variable { name :: !String       -- ^ Name of the variable
           , index :: !(Maybe Int) -- ^ Index for immutable (SSA) variables, Nothing otherwise
           , size :: !Int          -- ^ Size in byte
           }
  deriving (Eq, Ord, Generic)

instance Cereal.Serialize Variable

-- | Copies the value of one variable to another while extracting (or extending) the correct bits
data VariableConversion =
  VariableConversion { source :: !Variable      -- ^ The variable to copy from
                     , destination :: !Variable -- ^ The variable to copy to
                     , isLow :: !Bool           -- ^ Used when we don't want to extract the lowest 8 bits, but rather the highest 8 of the lowest 16 bits
                     }
  deriving (Eq, Ord, Generic)

instance Cereal.Serialize VariableConversion

isSSA :: Variable -> Bool
isSSA Variable { index } = isJust index

instance Show Variable where
  show (Variable name index size) =
    "( " ++ name ++ (show <$> index) `orElse` "" ++ ": " ++ show size ++ ")"

instance Show VariableConversion where
  show (VariableConversion src dst isLow) = show dst
    ++ " = conv"
    ++ (if isLow
        then ""
        else "_high")
    ++ show src