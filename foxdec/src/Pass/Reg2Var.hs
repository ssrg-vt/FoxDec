module Pass.Reg2Var (reg2var) where

import           Data.Generic (MutableVariable(..), Variable(..), mapI, mapP
                             , bindP, Statement(..), variableFromRegister
                             , VariableConversion(..))
import qualified Data.PreSSA as PreSSA
import qualified Data.X86 as X86
import           Data.Void (absurd)
import           Generic_Datastructures (Instruction(..), GenericOperand(..))
import           X86_Datastructures (Register(..), real_reg, operand_size)
import           Data.List (delete)
import           Data.PreSSA (Special(SpecialConversion))

--------------------------------------------------------------------------------
-- TRANSFORMATIONS
--------------------------------------------------------------------------------
-- | Replaces all registers by variables.
-- | This is almost a trivial translation where every register is replaced by exactly one mutable variable.
-- | But registers have the aliasing semantics where the value of `eax` depends on `rax`.
-- | Variables usually don't have these aliasing semantics, so we introduce explicit conversion nodes.
reg2var :: X86.Program -> PreSSA.Program
reg2var = bindP reg2varStmt

reg2varStmt :: X86.Statement -> [PreSSA.Statement]
reg2varStmt (StmtInstruction is) = is >>= reg2varInstr
reg2varStmt (StmtSpecial s) = absurd s

reg2varInstr :: X86.Instruction -> [PreSSA.Statement]
reg2varInstr
  i = StmtInstruction [mapI reg2varStorage i]:otherDestinationsToStatements i

reg2varStorage :: X86.Storage -> PreSSA.Storage
reg2varStorage = variableFromRegister

-- Makes sure all variables for aliasing registers are in sync
-- This will for all instructions that
-- * Have a register as destination:
--   Create conversion statements for all aliased registers.
--   For example, for `MOV rax, rbx`, this generates
--   `%eax <- 64to32 %rax; %ax <- 64to16 %rax; %al <- 64to08low %rax; %ah <- 64to08high %rax`
-- * Have no registers as destination (or none at all);
--   Create nothing.
--
-- TODO: Right now, the decision on high/low for the 8-bit registers is the wrong way around.
-- This will cause the high 8-bit registers to have the wrong value.
otherDestinationsToStatements :: X86.Instruction -> [PreSSA.Statement]
otherDestinationsToStatements i = case instr_dest i of
  Just (Storage reg) -> mkConversion reg . variableFromRegister <$> otherRegisters reg
  _ -> []
  where
    mkConversion :: Register -> PreSSA.Storage -> PreSSA.Statement
    mkConversion orig var = StmtSpecial
      $ PreSSA.SpecialConversion
      $ VariableConversion { conversionFrom = variableFromRegister orig
                           , conversionTo = var
                           , conversionLow = isNot8BitHigh orig
                           }

--------------------------------------------------------------------------------
-- REGISTER UTIL
--------------------------------------------------------------------------------
isNot8BitHigh :: Register -> Bool
isNot8BitHigh AH = False
isNot8BitHigh BH = False
isNot8BitHigh CH = False
isNot8BitHigh DH = False
isNot8BitHigh _ = True

-- | All registers that are aliased by this register
otherRegisters :: Register -> [Register]
otherRegisters r = let others = otherPotentialRegisters r
                   in case opposingRegister r of
                        Just op -> delete r others
                        Nothing -> others

-- | All registers that may be aliased by this register
otherPotentialRegisters :: Register -> [Register]
otherPotentialRegisters = delete <*> registerGroup . real_reg

-- | The partial registers that don't alias (AL and AH, BL and BH, ...)
opposingRegister :: Register -> Maybe Register
opposingRegister AL = Just AH
opposingRegister AH = Just AL
opposingRegister BL = Just BH
opposingRegister BH = Just BL
opposingRegister CL = Just CH
opposingRegister CH = Just CL
opposingRegister DL = Just DH
opposingRegister DH = Just DL
opposingRegister _ = Nothing

-- | All registers that belong to the "real register"
registerGroup :: Register -> [Register]
registerGroup RAX = [RAX, EAX, AX, AH, AL]
registerGroup RBX = [RBX, EBX, BX, BH, BL]
registerGroup RCX = [RCX, ECX, CX, CH, CL]
registerGroup RDX = [RDX, EDX, DX, DH, DL]
registerGroup RDI = [RDI, EDI, DI, DIL]
registerGroup RSI = [RSI, ESI, SI, SIL]
registerGroup RSP = [RSP, ESP, SP, SPL]
registerGroup RBP = [RBP, EBP, BP, BPL]
registerGroup R15 = [R15, R15D, R15W, R15B]
registerGroup R14 = [R14, R14D, R14W, R14B]
registerGroup R13 = [R13, R13D, R13W, R13B]
registerGroup R12 = [R12, R12D, R12W, R12B]
registerGroup R11 = [R11, R11D, R11W, R11B]
registerGroup R10 = [R10, R10D, R10W, R10B]
registerGroup R9 = [R9, R9D, R9W, R9B]
registerGroup R8 = [R8, R8D, R8W, R8B]
registerGroup CS = [CS, DS, ES, FS, GS, SS]
registerGroup EIZ = [EIZ, RIZ]
registerGroup ST0 = [ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7]
registerGroup YMM0 =
  [ YMM0
  , YMM1
  , YMM2
  , YMM3
  , YMM4
  , YMM5
  , YMM6
  , YMM7
  , YMM8
  , YMM9
  , YMM10
  , YMM11
  , YMM12
  , YMM13
  , YMM14
  , YMM15]
registerGroup XMM0 =
  [ XMM0
  , XMM1
  , XMM2
  , XMM3
  , XMM4
  , XMM5
  , XMM6
  , XMM7
  , XMM8
  , XMM9
  , XMM10
  , XMM11
  , XMM12
  , XMM13
  , XMM14
  , XMM15]
registerGroup reg = error $ show reg ++ " is not a \"real register\""
