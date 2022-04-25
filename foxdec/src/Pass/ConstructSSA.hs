module Pass.ConstructSSA where

import qualified IR.PreSSA                   as PreSSA
import           IR.PreSSA                    ( verifySSA )
import qualified IR.SSA                      as SSA

constructSSA :: PreSSA.Program -> Maybe SSA.Program
constructSSA prog = SSA.fromPreSSA <$> (verifySSA . constructPartialSSA) prog

-- | Actual SSA implementation, needs a boolean verifier
constructPartialSSA :: PreSSA.Program -> PreSSA.Program
constructPartialSSA = renameVariables . insertPhiNodes

insertPhiNodes :: PreSSA.Program -> PreSSA.Program
insertPhiNodes = undefined

renameVariables :: PreSSA.Program -> PreSSA.Program
renameVariables = undefined
