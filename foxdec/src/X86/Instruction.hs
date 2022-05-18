{-# LANGUAGE FlexibleInstances #-}

module X86.Instruction where

import qualified Generic_Datastructures as GD
import           X86.Register (Register)
import           X86.Prefix (Prefix)
import           X86.Opcode (Opcode)
import           Data.Word (Word64)
import           Base (orElse)
import           Typeclasses.HasSize (HasSize(sizeof))
import Generic.Address (AddressWord64(..))

type Instruction = GD.Instruction AddressWord64 Register Prefix Opcode Int

instr_addr :: Instruction -> Word64
instr_addr (GD.Instruction (AddressWord64 a) _ _ _ _ _) = a

instance HasSize Instruction where
  sizeof i = GD.instr_annot i `orElse` 0