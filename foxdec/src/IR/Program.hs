module IR.Program where

import           Generic.Program (GenericProgram)
import qualified IR.Instruction as IR

type Program = GenericProgram IR.Instruction