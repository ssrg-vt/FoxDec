module Pass.Reg2Var (reg2var) where

import qualified IR.PreSSA as PreSSA
import qualified IR.X86 as X86
import           Data.Void (absurd)
import           Data.List (delete)
import           X86.Register (Register(..))
import qualified X86.Register as Reg
import           Generic.Operand (GenericOperand(Storage))
import qualified Generic.Instruction as Instr

--------------------------------------------------------------------------------
-- TRANSFORMATIONS
--------------------------------------------------------------------------------
-- | Replaces all registers by variables.
-- | This is almost a trivial translation where every register is replaced by exactly one mutable variable.
-- | But registers have the aliasing semantics where the value of `eax` depends on `rax`.
-- | Variables usually don't have these aliasing semantics, so we introduce explicit conversion nodes.
reg2var :: X86.Program -> PreSSA.Program
reg2var = undefined 