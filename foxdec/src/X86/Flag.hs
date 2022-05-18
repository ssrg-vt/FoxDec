module X86.Flag (Flag(..)) where

-- | Flags
data Flag = ZF
          | CF
          | SF
          | OF
          | PF
          | InvalidFlag
  deriving (Show, Eq, Ord)