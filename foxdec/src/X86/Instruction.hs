{-# LANGUAGE FlexibleInstances #-}

module X86.Instruction (Instruction, addr, GenericInstruction(..)) where

import           Base (orElse)
import           Data.Word (Word64)
import           Generic.Address (AddressWord64(..))
import           Generic.Instruction (GenericInstruction(..))
import           Typeclasses.HasSize (HasSize(sizeof))
import           X86.Opcode (Opcode)
import           X86.Prefix (Prefix)
import           X86.Register (Register)

type Instruction = GenericInstruction AddressWord64 Register Prefix Opcode Int

addr :: Instruction -> Word64
addr (Instruction (AddressWord64 a) _ _ _ _ _) = a

instance HasSize Instruction where
  sizeof i = annot i `orElse` 0