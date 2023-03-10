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
import Generic.Binary

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
import X86.Opcode (Opcode(JMP), isCall, isJump)
import X86.Instruction
import qualified X86.Operand as X86
import Generic.HasSize (sizeof)
import Generic.Address (GenericAddress(..))
import Generic.Operand (GenericOperand(..))
import Generic.Instruction (GenericInstruction(Instruction))
import Generic.SymbolicConstituents
import qualified Generic.Instruction as Instr


-- | Returns true iff a symbol is associated with the address.
address_has_external_symbol ctxt a =
  case IM.lookup (fromIntegral a) $ ctxt_symbol_table ctxt of
    Just (Relocated_Function _) -> True
    Just (Relocated_Label _) -> True
    _ -> False


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
      a'      = f rip
      syms    = ctxt_symbol_table ctxt in
    case (IM.lookup (fromIntegral a') syms,read_from_ro_datasection ctxt a' si) of
      (Just (Relocated_Function s),  a'') ->
        -- Example:
        --   Instruction 10005464e: CALL 64 ptr [RIP + 1751660] 6 read from address 1002000c0 which has symbol _objc_msgSend producing address 0
        --   Address 1002000c0 is returned and treated as an external function call       
        -- trace ("Instruction " ++ show i ++ " read from address " ++ showHex a' ++ " which has symbol " ++ s ++ " producing address " ++ showHex_option a'') $ 
        External s
{--
      (Nothing, Just a'') ->
        -- Example:
        --   Instruction 10011e093: CALL 64 ptr [RIP + 1098831] 6 read from address 10022a4e8 producing address 100131d63
        --   Address 100131d63 is returned as that is the function pointer to be called
        -- trace ("Instruction " ++ show i ++ " read from address " ++ showHex a' ++ " producing address " ++ showHex a'') $
        ImmediateAddress a''
--}
      _   ->
        Unresolved


-- | Tries to retrieve a function name with an entry address.
-- If the entry matches a known symbol, return that.
-- Otherwise, simply return the entry address itself in hexadecimal notation.
-- However, there is one exception: 
-- 	if the first instruction at the entry address immediately jumps to an external function,
-- 	return the name of that external function instead. This happens in a @.got@ section.
function_name_of_entry ::
  Context  -- ^ The context
  -> Int   -- ^ The entry address
  -> String
function_name_of_entry ctxt a =
  case IM.lookup a $ ctxt_symbol_table ctxt of
    Just (Relocated_Function sym) -> sym
    _ ->
      case unsafePerformIO $ fetch_instruction ctxt (fromIntegral a) of -- TODO. However, should be safe as result is immutable.
        Just i@(Instruction _ _ JMP Nothing [op1] _)  ->
          case operand_static_resolve ctxt i op1 of
            External sym -> sym
            _ -> "0x" ++ showHex a
        _ -> "0x" ++ showHex a

-- | Tries to retrieve a function name for a @call@-instruction (see @`function_name_of_entry`@).
--
-- Returns the empty string if the given instruction is not a call or a jump.
function_name_of_instruction ::
  Context            -- ^ The context
  -> Instruction -- ^ The instruction
  -> String
function_name_of_instruction ctxt i@(Instruction _ _ _ _ ops _) =
  if isCall (Instr.opcode i) || isJump (Instr.opcode i) then
    case operand_static_resolve ctxt i (head ops) of
      External sym       -> sym
      ImmediateAddress a -> function_name_of_entry ctxt $ fromIntegral a
      Unresolved         -> "indirection@" ++ showHex (addressof i)
  else
    ""




