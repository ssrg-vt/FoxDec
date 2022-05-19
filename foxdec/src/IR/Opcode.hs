{-# LANGUAGE DeriveGeneric #-}

module IR.Opcode (Opcode(..)) where

import qualified X86.Opcode as X86
import           GHC.Generics (Generic)
import qualified Data.Serialize as Cereal

data Opcode = OpcodeX86 X86.Opcode
            | OpcodePhi
            | OpcodeConvert
  deriving (Eq, Ord, Generic)

instance Cereal.Serialize Opcode

instance Show Opcode where
  show (OpcodeX86 x86) = show x86
  show OpcodePhi = "Φ"
  show OpcodeConvert = "conv"