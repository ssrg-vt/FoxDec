module IR.PreSSA
    ( Label
    , Storage
    , Instruction
    , Program
    , verifySSA) where

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
-- VERIFIER
--------------------------------------------------------------------------------
-- | Returns Just the program if it is in valid SSA form
-- | Returns Nothing if the program is not in valid SSA form
verifySSA :: Program -> Maybe Program
verifySSA = return -- TODO
