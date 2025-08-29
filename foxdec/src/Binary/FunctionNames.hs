{-# LANGUAGE PartialTypeSignatures, StrictData #-}

{-|
Module      : FunctionNames
Description : Provides functions to 

Contains function relating to control flow, including functions for
resolving the targets of jumps and calls.
-}



module Binary.FunctionNames where

import Binary.Generic


import Base
import Data.JumpTarget
import Conventions
import Data.X86.Opcode
import Data.X86.Instruction
import Data.X86.Register
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
import Control.Applicative ((<|>))






-- | Tries to retrieve a function name with an entry address.
-- If the entry matches a known symbol, return that.
-- Otherwise, simply return the entry address itself in hexadecimal notation.
-- However, there is one exception: 
-- 	if the first instruction at the entry address immediately jumps to an external function,
-- 	return the name of that external function instead. This happens in a @.got@ section.
try_plt_target_for_entry ::
  BinaryClass bin => 
     bin
  -> Word64 -- ^ The entry address
  -> Maybe ResolvedJumpTarget
try_plt_target_for_entry bin a = jmps_to_external_function a `orTry` endbr64_jmps_to_external_function a `orTry` nop_jmps_to_external_function a
  --firstJusts [ {--is_external_function a,--} jmps_to_external_function a, endbr64_jmps_to_external_function a ]
 where
{--
  is_external_function a = do
    let SymbolTable syms _ = binary_get_symbols $ fst bin
    let sym_a = IM.lookup (fromIntegral a) syms
    case sym_a of
      Just (PointerToLabel sym True) -> Just $ External $ strip_GLIBC sym
      _                              -> Nothing 
--}
  jmps_to_external_function a =
    case fetch_instruction bin a of
      Just i@(Instruction _ _  JMP [op1] _ _) -> resolve_operand i op1
      _                                               -> Nothing

  endbr64_jmps_to_external_function a =
    case fetch_instruction bin a of
      Just (Instruction _ _ ENDBR64 _ _ si) -> jmps_to_external_function (a + fromIntegral si)
      _                                             -> Nothing

  nop_jmps_to_external_function a =
    case fetch_instruction bin a of
      Just (Instruction _ _ NOP _ _ si) -> jmps_to_external_function (a + fromIntegral si)
      _                                   -> Nothing


  resolve_operand i op1 =
    case operand_static_resolve bin i op1 of
      External sym -> Just $ External sym
      ExternalDeref sym -> Just $ ExternalDeref sym
      ImmediateAddress a' -> Just $ ImmediateAddress a'
      _ -> Nothing


-- | Given an instruction that calls or jumps, try to find a jump target
jump_target_for_instruction ::
  BinaryClass bin =>
     bin
  -> Instruction -- ^ The instruction
  -> ResolvedJumpTarget
jump_target_for_instruction bin i@(Instruction _ _ SYSCALL _ _ _) = Unresolved
jump_target_for_instruction bin i@(Instruction _ _ _ ops _ _) =
  case operand_static_resolve bin i (head ops) of
    External sym       -> External sym
    ExternalDeref sym  -> ExternalDeref sym
    Unresolved         -> Unresolved
    ImmediateAddress a ->
      case try_plt_target_for_entry bin a of
        Just trgt -> trgt
        Nothing   -> ImmediateAddress a



-- | many operands can statically be resolved, even though technically they are indirect (relative to RIP).
-- Examples:
--
-- @10005464e: call RIP + 1751660@ resolves to an immediate jump target by resolving the RIP-relative addressing.
--
-- @10005464e: call qword ptr [RIP + 1751660]@ read from address 1002000c0, but address has a symbol associated to it. This function call will resolve to an external function.
operand_static_resolve ::
  BinaryClass bin =>
     bin
  -> Instruction    -- ^ The instruction
  -> Operand        -- ^ The operand of the instruction to be resolved
  -> ResolvedJumpTarget
operand_static_resolve bin i (Op_Imm (Immediate _ v)) = ImmediateAddress (inAddress i + v)
operand_static_resolve bin i (Op_Mem _ (Reg64 RIP) RegNone 0 displ Nothing _) = try_read_function_pointer bin i $ fromIntegral (fromIntegral (inAddress i) + fromIntegral (inSize i) + displ)
operand_static_resolve bin i (Op_Mem _ (Reg64 RIP) RegNone scale displ Nothing _) = error $ show (i,fromIntegral (fromIntegral (inAddress i) + fromIntegral (inSize i) + displ))
operand_static_resolve bin i _            = Unresolved



-- If *[a',8] = fptr, then return that fptr
try_read_function_pointer bin i a' = try_symbol a' `orTry` try_relocation a' `orElse` immediate_address a'
 where
  try_symbol a' = do
    let SymbolTable syms _ = binary_get_symbols bin
    case IM.lookup (fromIntegral a') syms of
      -- Example:
      --   Instruction 10005464e: CALL 64 ptr [RIP + 1751660] 6 read from address 1002000c0 which has symbol _objc_msgSend producing address 0
      --   Address *[1002000c0,8] is treated as an external function call       
      Just (PointerToExternalFunction s) -> Just $ External $ strip_GLIBC s
      Just (PointerToInternalFunction s a1) -> Just $ ImmediateAddress a1
      Just (AddressOfLabel s True)  -> Just $ ExternalDeref $ strip_GLIBC s
      Just (AddressOfObject s True) -> Just $ ExternalDeref $ strip_GLIBC s
      Just (Relocated_ResolvedObject f a1) -> Just $ ImmediateAddress a1

      --Just s -> error $ show (i,showHex a',s)
      _ -> Nothing

  try_relocation a' =
    case find (is_reloc_for a') $ binary_get_relocations bin of
      Nothing -> Nothing
      Just (Relocation _ a1) -> Just $ ImmediateAddress a1 --TODO what if a1 is a symbol?
        --case try_symbol a1 of
        --  Nothing -> Just $ ImmediateAddress a1
        --  Just sym -> Just sym -- error $ show (a',a1,sym)
    

  is_reloc_for a' (Relocation a _) = a == a'

  find_address_of_label syms l = (ImmediateAddress . fromIntegral . fst) <$> (find (\(a,symbol) -> symbol == AddressOfLabel l False) $ IM.assocs syms)

  immediate_address a
    | address_has_instruction bin a = error $ "Does this happen?" ++ showHex a -- Should NOT return this! ImmediateAddress a
    | otherwise = Unresolved



function_name_of_entry ::
  BinaryClass bin =>
     bin
  -> Word64 -- ^ The entry address
  -> String
function_name_of_entry bin a = try_symbol a `orElse` try_plt a
 where
  -- Try if the address matches a known symbol
  try_symbol a = do
    sym  <- (IM.lookup (fromIntegral a) $ IM.filter is_address_of_symbol $ binary_get_symbol_table bin)
    return $ symbol_to_name sym

  try_plt a =
    case try_plt_target_for_entry bin a of
      Nothing                    -> "0x" ++ showHex a
      Just (External sym)        -> sym
      Just (ExternalDeref  sym)  -> "*" ++ sym
      Just (ImmediateAddress a') -> "0x" ++ showHex a'


is_address_of_symbol (AddressOfObject str ex) = str /= ""
is_address_of_symbol (AddressOfLabel str ex)  = str /= ""
is_address_of_symbol _                        = False

is_address_of_internal_symbol (AddressOfObject str ex) = str /= "" && not ex
is_address_of_internal_symbol (AddressOfLabel str ex)  = str /= "" && not ex
is_address_of_internal_symbol _                        = False




-- | Tries to retrieve a function name for a @call@-instruction (see @`function_name_of_entry`@).
--
-- Returns the empty string if the given instruction is not a call or a jump.
function_name_of_instruction ::
  BinaryClass bin =>
     bin
  -> Instruction -- ^ The instruction
  -> String
function_name_of_instruction bin i@(Instruction _ _ _ ops _ _)
  | isSyscall (inOperation i) = "syscall@" ++ showHex (inAddress i)
  |  isCall (inOperation i) || isJump (inOperation i) =
    case jump_target_for_instruction bin i of
      External sym       -> sym
      ExternalDeref sym  -> "*" ++ sym
      ImmediateAddress a -> "0x" ++ showHex a
      Unresolved         -> "indirection@" ++ showHex (inAddress i)
  | otherwise = ""



