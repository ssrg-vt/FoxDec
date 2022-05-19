module IR.SSA (Label, Storage, Instruction, Program, fromPreSSA) where

import qualified IR.PreSSA as PreSSA
import           X86.Prefix (Prefix)
import           X86.Opcode (Opcode)
import           Generic.Address (AddressWord64)
import           Generic.Instruction (GenericInstruction)
import           Data.Variable (Variable, VariableConversion)
import           Data.Phi (Phi)
import           Generic.Program (GenericProgram)

--------------------------------------------------------------------------------
-- DATA
--------------------------------------------------------------------------------
type Label = AddressWord64

type Storage = Variable

type Instruction = GenericInstruction Label Storage Prefix Opcode Int

type Program = GenericProgram Instruction

--------------------------------------------------------------------------------
-- PreSSA -> SSA
--------------------------------------------------------------------------------
fromPreSSA :: PreSSA.Program -> Program
fromPreSSA = undefined

instructionFromPreSSA :: PreSSA.Instruction -> Instruction
instructionFromPreSSA = undefined
