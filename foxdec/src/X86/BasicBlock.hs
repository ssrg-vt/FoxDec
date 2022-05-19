module X86.BasicBlock where

import           Generic.BasicBlock (GenericBasicBlock)
import qualified X86.Instruction as X86

type BasicBlock = GenericBasicBlock X86.Instruction