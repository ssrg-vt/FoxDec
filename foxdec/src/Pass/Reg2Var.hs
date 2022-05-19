module Pass.Reg2Var (reg2var) where

import           Data.Maybe (maybeToList)
import qualified Data.Variable as Variable
import qualified Generic.Instruction as Instr
import           Generic.Instruction (GenericInstruction(..))
import           Generic.BasicBlock (GenericBasicBlock(..))
import           Generic.Operand (GenericOperand(..))
import           Generic.Program (mapBasicBlocks)
import           X86.Register (Register)
import qualified X86.Register as Register
import qualified X86.Instruction as X86
import qualified X86.Program as X86
import qualified X86.Operand as X86
import qualified IR.Instruction as IR
import qualified IR.Program as IR
import qualified IR.Operand as IR
import           IR.Opcode (Opcode(..))

reg2var :: X86.Program -> IR.Program
reg2var = mapBasicBlocks (>>= BasicBlock <$> instrReg2Var)

instrReg2Var :: X86.Instruction -> [IR.Instruction]
instrReg2Var (Instruction label prefix opcode dest srcs annot) =
  mkInstr (sourcesToVariables srcs) <$> destinationToVariables dest
  where
    mkInstr srcs' dst' =
      Instruction label prefix (OpcodeX86 opcode) (Just dst') srcs' Nothing

sourcesToVariables :: [X86.Operand] -> [IR.Operand]
sourcesToVariables srcs = fmap Variable.fromRegister <$> srcs

destinationToVariables :: Maybe X86.Operand -> [IR.Operand]
destinationToVariables Nothing = []
destinationToVariables (Just dest) = fmap Variable.fromRegister <$> allDests
  where
    allDests = dest:overlappingDestinations dest

overlappingDestinations :: X86.Operand -> [X86.Operand]
overlappingDestinations (Storage reg) = Storage <$> Register.overlapping reg
overlappingDestinations _ = []


