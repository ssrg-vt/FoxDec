{-# LANGUAGE PartialTypeSignatures, Strict #-}

{-|
Module      : FunctionNames
Description : Provides functions to 

Contains function relating to control flow, including functions for
resolving the targets of jumps and calls.
-}



module Analysis.FunctionNames where

import Analysis.Context
import Base
import Data.JumpTarget
import X86.Conventions
import Data.Symbol


import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.List
import Data.List.Split (chunksOf)
import Data.Word (Word64)
import Control.Monad ((>=>))
import Debug.Trace
import Numeric (readHex)
import System.IO.Unsafe (unsafePerformIO)
import X86.Register (Register(..))
import X86.Opcode (Opcode(JMP,ENDBR64), isCall, isJump)
import X86.Instruction
import qualified X86.Operand as X86
import Generic.HasSize (sizeof)
import Generic.Address (GenericAddress(..))
import Generic.Operand (GenericOperand(..))
import Generic.Instruction (GenericInstruction(Instruction))
import Generic.SymbolicConstituents
import qualified Generic.Instruction as Instr
import Control.Applicative ((<|>))


-- | Returns true iff a symbol is associated with the address.
address_has_external_symbol ctxt a =
  case IM.lookup (fromIntegral a) $ ctxt_symbol_table ctxt of
    Just (PointerToLabel _ ex)  -> ex
    Just (PointerToObject _ ex) -> ex
    Just (AddressOfObject _ ex) -> ex
    Just (AddressOfLabel _ ex)  -> ex
    _ -> False

-- | Returns true iff a symbol is associated with the address.
find_external_symbol_for_address ctxt a =
  case IM.lookup (fromIntegral a) $ ctxt_symbol_table ctxt of
    Just (PointerToLabel str ex)  -> onlyWhen ex str 
    Just (PointerToObject str ex) -> onlyWhen ex str
    Just (AddressOfObject str ex) -> onlyWhen ex str
    Just (AddressOfLabel str ex)  -> onlyWhen ex str
    _ -> Nothing


-- | many operands can statically be resolved, even though technically they are indirect (relative to RIP).
-- Examples:
--
-- @10005464e: call RIP + 1751660@ resolves to an immediate jump target by resolving the RIP-relative addressing.
--
-- @10005464e: call qword ptr [RIP + 1751660]@ read from address 1002000c0, but address has a symbol associated to it. This function call will resolve to an external function.
operand_static_resolve ::
  Context               -- ^ The context
  -> Instruction    -- ^ The instruction
  -> X86.Operand        -- ^ The operand of the instruction to be resolved
  -> ResolvedJumpTarget
operand_static_resolve ctxt i (Immediate a')                                                         = ImmediateAddress a'
operand_static_resolve ctxt i (EffectiveAddress (AddressPlus (AddressStorage RIP) (AddressImm imm))) = ImmediateAddress $ addressof i + fromIntegral (sizeof i) + imm
operand_static_resolve ctxt i (EffectiveAddress (AddressPlus (AddressImm imm) (AddressStorage RIP))) = ImmediateAddress $ addressof i + fromIntegral (sizeof i) + imm
operand_static_resolve ctxt i (Memory (AddressPlus  (AddressStorage RIP) (AddressImm imm)) si)       = static_resolve_rip_expr ctxt i (\rip -> rip + imm) si
operand_static_resolve ctxt i (Memory (AddressPlus  (AddressImm imm) (AddressStorage RIP)) si)       = static_resolve_rip_expr ctxt i (\rip -> rip + imm) si
operand_static_resolve ctxt i (Memory (AddressMinus (AddressStorage RIP) (AddressImm imm)) si)       = static_resolve_rip_expr ctxt i (\rip -> rip - imm) si
operand_static_resolve ctxt i _                                                                      = Unresolved

static_resolve_rip_expr :: Context -> Instruction -> (Word64 -> Word64) -> Int -> ResolvedJumpTarget
static_resolve_rip_expr ctxt i f si =
  let rip     = addressof i + (fromIntegral $ sizeof i)
      a'      = f rip in
    try_relocated_function a' `orElse` immediate_address a'
 where
  try_relocated_function a' =
    case IM.lookup (fromIntegral a') $ ctxt_symbol_table ctxt of
      -- Example:
      --   Instruction 10005464e: CALL 64 ptr [RIP + 1751660] 6 read from address 1002000c0 which has symbol _objc_msgSend producing address 0
      --   Address *[1002000c0,8] is treated as an external function call       
      Just (PointerToLabel s True)  -> Just $ External s
      Just (PointerToLabel s False) -> find_address_of_label s
      _ -> Nothing


  immediate_address a
    | not (address_has_instruction ctxt a) = Unresolved
    | otherwise = error $ "Does this happen?" ++ showHex a -- Should NOT return this! ImmediateAddress a


  find_address_of_label l = (ImmediateAddress . fromIntegral . fst) <$> (find (\(a,symbol) -> symbol == AddressOfLabel l False) $ IM.assocs $ ctxt_symbol_table ctxt)
      


{--
      (Nothing, Just a'') ->
        -- Example:
        --   Instruction 10011e093: CALL 64 ptr [RIP + 1098831] 6 read from address 10022a4e8 producing address 100131d63
        --   Address 100131d63 is returned as that is the function pointer to be called
        -- trace ("Instruction " ++ show i ++ " read from address " ++ showHex a' ++ " producing address " ++ showHex a'') $
        ImmediateAddress a''
--}


-- | Tries to retrieve a function name with an entry address.
-- If the entry matches a known symbol, return that.
-- Otherwise, simply return the entry address itself in hexadecimal notation.
-- However, there is one exception: 
-- 	if the first instruction at the entry address immediately jumps to an external function,
-- 	return the name of that external function instead. This happens in a @.got@ section.
try_plt_target_for_entry ::
  Context  -- ^ The context
  -> Int   -- ^ The entry address
  -> Maybe ResolvedJumpTarget
try_plt_target_for_entry ctxt a =
  case IM.lookup a $ ctxt_symbol_table ctxt of
    Just (PointerToLabel sym True) -> Just $ External $ strip_GLIBC sym
    _ ->
      case unsafePerformIO $ fetch_instruction ctxt (fromIntegral a) of -- TODO. However, should be safe as result is immutable.
        Just i@(Instruction _ _ JMP Nothing [op1] _)  ->
          case operand_static_resolve ctxt i op1 of
            External sym -> Just $ External $ strip_GLIBC sym
            ImmediateAddress a' -> Just $ ImmediateAddress a'
            _ -> Nothing
        Just i@(Instruction _ _ ENDBR64 Nothing _ (Just si))  ->
          case unsafePerformIO $ fetch_instruction ctxt (fromIntegral a + fromIntegral si) of -- TODO. However, should be safe as result is immutable.
            Just i@(Instruction _ _ JMP Nothing [op1] _)  ->
              case operand_static_resolve ctxt i op1 of
                External sym -> Just $ External $ strip_GLIBC sym
                ImmediateAddress a' -> Just $ ImmediateAddress a'
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing

function_name_of_entry ::
  Context  -- ^ The context
  -> Int   -- ^ The entry address
  -> String
function_name_of_entry ctxt a =
  case try_plt_target_for_entry ctxt a of
    Nothing                    -> "0x" ++ showHex a
    Just (External sym)        -> sym
    Just (ImmediateAddress a') -> "0x" ++ showHex a'




-- | Given an instruction that calls or jumps, try to find a jump target
jump_target_for_instruction ::
  Context            -- ^ The context
  -> Instruction -- ^ The instruction
  -> ResolvedJumpTarget
jump_target_for_instruction ctxt i@(Instruction _ _ _ _ ops _) =
  case operand_static_resolve ctxt i (head ops) of
    External sym       -> External sym
    ImmediateAddress a -> (try_plt_target_for_entry ctxt $ fromIntegral a) `orElse` ImmediateAddress a
    Unresolved         -> Unresolved


-- | Tries to retrieve a function name for a @call@-instruction (see @`function_name_of_entry`@).
--
-- Returns the empty string if the given instruction is not a call or a jump.
function_name_of_instruction ::
  Context            -- ^ The context
  -> Instruction -- ^ The instruction
  -> String
function_name_of_instruction ctxt i@(Instruction _ _ _ _ ops _) =
  if isCall (Instr.opcode i) || isJump (Instr.opcode i) then
    case jump_target_for_instruction ctxt i of
      External sym       -> sym
      ImmediateAddress a -> "0x" ++ showHex a
      Unresolved         -> "indirection@" ++ showHex (addressof i)
  else
    ""




