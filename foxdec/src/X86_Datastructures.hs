{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

{-|
Module      : X86_Datastructures
Description : Datastructures for storing x86-64 instructions.
-}

module X86_Datastructures where

import Data.List
import Data.Word (Word64)
import Base
import qualified Data.Map as M
import Generic_Datastructures
import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)
import X86.Register (Register)
import qualified X86.Register as Reg
import X86.Prefix (Prefix)
import X86.Opcode (Opcode)
import Typeclasses.HasSize (sizeof)

-- | An x86 instruction
-- labels are integers, storages are registers, the annotation is the instruction size
type X86_Instruction = Instruction AddressWord64 Register Prefix Opcode Int
type X86_Operand     = GenericOperand Register
type X86_Address     = GenericAddress Register



instr_size :: X86_Instruction -> Int
instr_size i = instr_annot i `orElse` 0

instr_addr :: X86_Instruction -> Word64
instr_addr (Instruction (AddressWord64 a) _ _ _ _ _) = a


-- | The size of the operand, in bytes
operand_size :: X86_Operand -> Int
operand_size (Storage r)          = sizeof r
operand_size (Memory _ si)        = si
operand_size (EffectiveAddress _) = 8
operand_size (Immediate _)        = 8
