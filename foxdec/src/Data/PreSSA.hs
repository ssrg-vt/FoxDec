module Data.PreSSA
    ( Label
    , Storage
    , Special(..)
    , Instruction
    , Statement
    , Program
    , verifySSA) where

import qualified Data.Generic as Generic
import           Generic_Datastructures (AddressWord64)
import qualified Generic_Datastructures as GD
import           X86_Datastructures (Opcode, Prefix)

--------------------------------------------------------------------------------
-- DATA
--------------------------------------------------------------------------------
type Label = AddressWord64

type Storage = Generic.Variable

data Special = SpecialPhi (Generic.Phi Storage)
             | SpecialConversion (Generic.VariableConversion Storage)

type Instruction = GD.Instruction Label Storage Prefix Opcode Int

type Statement = Generic.Statement Label Storage Prefix Opcode Int Special

type Program = Generic.Program Label Storage Prefix Opcode Int Special

--------------------------------------------------------------------------------
-- VERIFIER
--------------------------------------------------------------------------------
-- | Returns Just the program if it is in valid SSA form
-- | Returns Nothing if the program is not in valid SSA form
verifySSA :: Program -> Maybe Program
verifySSA = return -- TODO