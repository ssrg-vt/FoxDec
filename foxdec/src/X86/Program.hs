module X86.Program (Program(..)) where

import           Generic.Program (GenericProgram)
import qualified X86.Instruction as X86

type Program = GenericProgram X86.Instruction