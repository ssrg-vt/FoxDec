{-# LANGUAGE FlexibleInstances #-}

module IR.Instruction where

import           Generic.Instruction (GenericInstruction(Instruction))
import           Generic.Address (AddressWord64(AddressWord64))
import           Data.Variable (Variable)
import           X86.Prefix (Prefix)
import           IR.Opcode (Opcode)
import           Data.Void (Void)
import           Typeclasses.HasAddress (HasAddress(addressof))

type Instruction = GenericInstruction AddressWord64 Variable Prefix Opcode Void

instance HasAddress Instruction where
  addressof (Instruction (AddressWord64 a) _ _ _ _ _) = a