module Generic.HasSize (HasSize(..)) where

-- | Class for things that have a size
class HasSize a where
  sizeof :: a -> Int -- ^ size, in bytes
