module X86.Address (Address) where

import           X86.Register (Register)
import           Generic.Address (GenericAddress(..))

type Address = GenericAddress Register