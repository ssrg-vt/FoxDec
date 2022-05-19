module IR.BasicBlock where

import           Generic.BasicBlock (GenericBasicBlock)
import qualified IR.Instruction as IR

type BasicBlock = GenericBasicBlock IR.Instruction