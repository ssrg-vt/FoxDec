module Data.Phi (Phi(..), phiForVariable) where

import           Data.Variable (Variable)
import qualified Data.Serialize as Cereal
import           Data.List (intercalate)

-- | Phi node with one destination and multiple sources
data Phi = Phi { destination :: !Variable, sources :: ![Variable] }
  deriving (Eq, Ord)

-- TODO
-- instance Cereal.Serialize Phi
-- | Creates an initial Phi node for the variable with n incoming edges
phiForVariable :: Variable -> Int -> Phi
phiForVariable var n = Phi var $ replicate n var

instance Show Phi where
  show (Phi dst srcs) =
    show dst ++ " = Φ(" ++ intercalate "," (show <$> srcs) ++ ")"