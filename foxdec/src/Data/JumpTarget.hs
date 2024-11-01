{-# LANGUAGE DeriveGeneric, DefaultSignatures, StrictData #-}

{-|
Module      : JumpTarget
Description : A datatype for resolving the operand of a jump/call
-}

module Data.JumpTarget where

import Base
import Data.Word
import GHC.Generics (Generic)


-- | Resolving the operand of a jump/call can produce one of the following.
data ResolvedJumpTarget =
   Unresolved               -- ^ An indirect branch that has not been resolved yet
 | External String          -- ^ A call to external function f
 | ImmediateAddress Word64  -- ^ An internal call to the given address
 deriving (Eq,Generic,Ord)


instance Show ResolvedJumpTarget
 where
  show Unresolved             = "Unresolved"
  show (External f)           = f
  show (ImmediateAddress imm) = "0x" ++ showHex imm
