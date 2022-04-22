module Pass.Reg2Var
    ( reg2var
    ) where

import           Data.Generic                   ( MutableVariable
                                                    ( MutableVariable
                                                    )
                                                , Variable(MutableVar)
                                                , mapI
                                                , mapP
                                                )
import qualified Data.PreSSA                   as PreSSA
import           Data.Void                      ( absurd )
import qualified Data.X86                      as X86


reg2var :: X86.Program -> PreSSA.Program
reg2var = mapP (map reg2varInstr) absurd

reg2varInstr :: X86.Instruction -> PreSSA.Instruction
reg2varInstr = mapI reg2varStorage

reg2varStorage :: X86.Storage -> PreSSA.Storage
reg2varStorage reg = MutableVar (MutableVariable (show reg))
