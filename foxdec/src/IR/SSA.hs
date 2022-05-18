module IR.SSA
    ( Label
    , Storage
    , Special
    , Instruction
    , Statement
    , Program
    , fromPreSSA) where

import           IR.Generic (mapI, mapP)
import qualified IR.Generic as Generic
import qualified IR.PreSSA as PreSSA
import           X86.Prefix (Prefix)
import           X86.Opcode (Opcode)
import           Generic.Address (AddressWord64)
import           Generic.Instruction (GenericInstruction)
import           Data.Variable (Variable, VariableConversion)
import           Data.Phi (Phi)

--------------------------------------------------------------------------------
-- DATA
--------------------------------------------------------------------------------
type Label = AddressWord64

type Storage = Variable

data Special = SpecialPhi Phi
             | SpecialConversion VariableConversion

type Instruction = GenericInstruction Label Storage Prefix Opcode Int

type Statement = Generic.Statement Label Storage Prefix Opcode Int Special

type Program = Generic.Program Label Storage Prefix Opcode Int Special

--------------------------------------------------------------------------------
-- PreSSA -> SSA
--------------------------------------------------------------------------------
fromPreSSA :: PreSSA.Program -> Program
fromPreSSA = mapP (map instructionFromPreSSA) specialFromPreSSA

specialFromPreSSA :: PreSSA.Special -> Special
specialFromPreSSA (PreSSA.SpecialPhi phi) = SpecialPhi phi
specialFromPreSSA (PreSSA.SpecialConversion c) = SpecialConversion c

instructionFromPreSSA :: PreSSA.Instruction -> Instruction
instructionFromPreSSA = mapI id
