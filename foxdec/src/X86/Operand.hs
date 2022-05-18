module X86.Operand (Operand, GenericOperand(..)) where

import           X86.Register (Register)
import           Generic.Operand (GenericOperand)

type Operand = GenericOperand Register