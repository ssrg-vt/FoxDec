module IR.SSA
    ( Label
    , Storage
    , Special
    , Instruction
    , Statement
    , Program
    , fromPreSSA) where

import           IR.Generic (ImmutableVariable, Phi(..), Variable(..), mapI
                           , mapP)
import qualified IR.Generic as Generic
import qualified IR.PreSSA as PreSSA
import           X86.Prefix (Prefix)
import           X86.Opcode (Opcode)
import           Generic.Address (AddressWord64)
import           Generic.Instruction (GenericInstruction)

--------------------------------------------------------------------------------
-- DATA
--------------------------------------------------------------------------------
type Label = AddressWord64

type Storage = ImmutableVariable

data Special = SpecialPhi (Generic.Phi Storage)
             | SpecialConversion (Generic.VariableConversion Storage)

type Instruction = GenericInstruction Label Storage Prefix Opcode Int

type Statement = Generic.Statement Label Storage Prefix Opcode Int Special

type Program = Generic.Program Label Storage Prefix Opcode Int Special

--------------------------------------------------------------------------------
-- PreSSA -> SSA
--------------------------------------------------------------------------------
fromPreSSA :: PreSSA.Program -> Program
fromPreSSA = mapP (map instructionFromPreSSA) specialFromPreSSA

specialFromPreSSA :: PreSSA.Special -> Special
specialFromPreSSA (PreSSA.SpecialPhi (Phi args)) =
  SpecialPhi $ Phi $ storageFromPreSSA <$> args
specialFromPreSSA (PreSSA.SpecialConversion c) =
  SpecialConversion $ storageFromPreSSA <$> c

storageFromPreSSA :: PreSSA.Storage -> Storage
storageFromPreSSA (ImmutableVar v) = v
storageFromPreSSA (MutableVar _) =
  error "Only SSA verified PreSSA programs may be translated"

instructionFromPreSSA :: PreSSA.Instruction -> Instruction
instructionFromPreSSA = mapI storageFromPreSSA
