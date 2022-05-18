module X86.Operand (Operand) where

import           Generic_Datastructures (GenericOperand)
import           X86.Register (Register)

type Operand = GenericOperand Register