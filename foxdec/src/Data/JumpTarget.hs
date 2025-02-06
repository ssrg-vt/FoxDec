{-# LANGUAGE DeriveGeneric, DefaultSignatures, StrictData #-}

{-|
Module      : JumpTarget
Description : A datatype for resolving the operand of a jump/call
-}

module Data.JumpTarget where

import Base

import Data.Word

import qualified Data.Serialize as Cereal hiding (get,put)
import Control.DeepSeq
import GHC.Generics (Generic)

-- | Resolving the operand of a jump/call can produce one of the following.
data ResolvedJumpTarget =
   Unresolved               -- ^ An indirect branch that has not been resolved yet
 | External String          -- ^ A call to external function f
 | ExternalDeref String     -- ^ A call to external function whose entry address is stored at the label: *[l,8] = fptr
 | ImmediateAddress Word64  -- ^ An internal call to the given address
 | Returns Bool             -- ^ The function returns (treat as a nop) or terminates
 deriving (Eq,Generic,Ord)


instance Show ResolvedJumpTarget
 where
  show Unresolved             = "Unresolved"
  show (External f)           = f
  show (ExternalDeref l)      = "*" ++ l
  show (ImmediateAddress imm) = "0x" ++ showHex imm
  show (Returns True)         = "returns"
  show (Returns False)        = "terminates"

instance Cereal.Serialize ResolvedJumpTarget

instance NFData ResolvedJumpTarget

