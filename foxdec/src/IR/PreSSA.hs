module IR.PreSSA
    ( Label
    , Storage
    , Special(..)
    , Instruction
    , Statement
    , Program
    , verifySSA) where

import qualified IR.Generic as Generic
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
-- VERIFIER
--------------------------------------------------------------------------------
-- | Returns Just the program if it is in valid SSA form
-- | Returns Nothing if the program is not in valid SSA form
verifySSA :: Program -> Maybe Program
verifySSA = return -- TODO
