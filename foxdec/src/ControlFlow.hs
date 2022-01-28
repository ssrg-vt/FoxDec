{-# LANGUAGE PartialTypeSignatures, Strict #-}

{-|
Module      : ControlFlow
Description : Functions for resolving jump targets.

Contains function relating to control flow, including functions for
resolving the targets of jumps and calls.
-}



module ControlFlow (
   ResolvedJumpTarget(..),
   post,
   fetch_block,
   address_has_instruction,
   address_has_symbol,
   address_is_external,
   operand_static_resolve,
   resolve_jump_target,
   get_internal_addresses,
   instruction_jumps_to_external,
   show_block,
   show_invariants,
   function_name_of_entry,
   function_name_of_instruction
 )
 where

import Base
import Context
import SimplePred
import Conventions
import SCC

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.List
import Data.List.Split (chunksOf)
import Data.Word (Word64)
import X86_Datastructures
import Control.Monad ((>=>))
import Debug.Trace
import Numeric (readHex)
import System.IO.Unsafe (unsafePerformIO)



-- | The set of next blocks from the given block
post g blockId =
  case IM.lookup blockId (cfg_edges g) of
    Nothing -> IS.empty
    Just ns -> ns



-- | Fetching an instruction list given a block ID
fetch_block :: 
  CFG    -- ^ The CFG
  -> Int -- ^ The blockID
  -> [Instr]
fetch_block g blockId =
  case IM.lookup blockId $ cfg_instrs $ g of
    Nothing -> error $ "Block with ID" ++ show blockId ++ " not found in cfg."
    Just b -> b









-- | Returns true iff an instruction can be fetched from the address.
address_has_instruction ctxt a =
  case find_section_for_address ctxt $ fromIntegral a of
    Nothing                    -> False
    Just (segment,section,_,_) -> (segment,section) `elem` sections_with_instructions
  --case unsafePerformIO $ fetch_instruction ctxt $ fromIntegral a of -- TODO. However, should be safe as result is immutable.
  --  Nothing -> False
  --  Just i  -> True

-- | Returns true iff a symbol is associated with the address.
address_has_symbol ctxt a =
  case IM.lookup (fromIntegral a) $ ctxt_syms ctxt of
    Nothing  -> False
    Just sym -> True

-- | Returns truee if the adress is external, i.e., has no instruction or has a symbol.
address_is_external ctxt a = address_has_symbol ctxt a || not (address_has_instruction ctxt a)


-- | Resolving the operand of a jump/call can produce one of the following.
data ResolvedJumpTarget = 
   Unresolved               -- ^ An indirect branch that has not been resolved yet
 | External String          -- ^ A call to external function f
 | ImmediateAddress Word64  -- ^ An internal call to the given address
 deriving (Eq,Show)

-- | many operands can statically be resolved, even though technically they are indirect (relative to RIP).
-- Examples:
--
-- @10005464e: call RIP + 1751660@ resolves to an immediate jump target by resolving the RIP-relative addressing.
--
-- @10005464e: call qword ptr [RIP + 1751660]@ read from address 1002000c0, but address has a symbol associated to it. This function call will resolve to an external function.
operand_static_resolve :: 
  Context           -- ^ The context
  -> Instr          -- ^ The instruction
  -> Maybe Operand  -- ^ The operand of the instruction to be resolved
  -> ResolvedJumpTarget
operand_static_resolve ctxt i (Just (Immediate a'))                                                 = ImmediateAddress a'
operand_static_resolve ctxt i (Just (Address (AddrPlus (FromReg RIP) (AddrImm imm))))               = ImmediateAddress $ fromIntegral (i_addr i) + fromIntegral (i_size i) + fromIntegral imm
operand_static_resolve ctxt i (Just (Address (AddrPlus (AddrImm imm) (FromReg RIP))))               = ImmediateAddress $ fromIntegral (i_addr i) + fromIntegral (i_size i) + fromIntegral imm
operand_static_resolve ctxt i (Just (Address (SizeDir si (AddrPlus  (FromReg RIP) (AddrImm imm))))) = static_resolve_rip_expr ctxt i (\rip -> rip + imm) si
operand_static_resolve ctxt i (Just (Address (SizeDir si (AddrPlus  (AddrImm imm) (FromReg RIP))))) = static_resolve_rip_expr ctxt i (\rip -> rip + imm) si
operand_static_resolve ctxt i (Just (Address (SizeDir si (AddrMinus (FromReg RIP) (AddrImm imm))))) = static_resolve_rip_expr ctxt i (\rip -> rip - imm) si
operand_static_resolve ctxt i _                                                                     = Unresolved

static_resolve_rip_expr ctxt i f si =
  let rip     = i_addr i + i_size i
      a'      = fromIntegral $ f rip
      syms    = ctxt_syms    ctxt in
    case (IM.lookup (fromIntegral a') syms,read_from_datasection ctxt a' si) of
      (Just s,  a'')      -> 
        -- Example:
        --   Instruction 10005464e: CALL 64 ptr [RIP + 1751660] 6 read from address 1002000c0 which has symbol _objc_msgSend producing address 0
        --   Address 1002000c0 is returned and treated as an external function call       
        -- trace ("Instruction " ++ show i ++ " read from address " ++ showHex a' ++ " which has symbol " ++ s ++ " producing address " ++ showHex_option a'') $ 
        External s
      (Nothing, Just a'') ->
        -- Example:
        --   Instruction 10011e093: CALL 64 ptr [RIP + 1098831] 6 read from address 10022a4e8 producing address 100131d63
        --   Address 100131d63 is returned as that is the function pointer to be called
        -- trace ("Instruction " ++ show i ++ " read from address " ++ showHex a' ++ " producing address " ++ showHex a'') $
        ImmediateAddress a''
      (Nothing,Nothing)   ->
        Unresolved

-- | Resolves the first operand of a call or jump instruction.
-- First tries to see if the instruction is an indirection, that has already been resolved.
-- If not, try to statically resolve the first operand using @`operand_static_resolve`@.
-- If that resolves to an external symbol @libc_start_main@, then this call is actually an indirection (and thus currently unresolved).
-- If that resolves to an immediate value, see if that immediate value corresponds to an external function or an internal function.
--
-- Returns a list of @`ResolvedJumpTarget`@, since an indirection may be resolved to multiple targets.
resolve_jump_target ::
  Context        -- ^ The context
  -> Instr       -- ^ The instruction
  -> [ResolvedJumpTarget]
resolve_jump_target ctxt i =
  case IM.lookup (i_addr i) $ ctxt_inds ctxt of
    Just as -> map (ImmediateAddress . fromIntegral) $ filter (not . address_is_external ctxt) $ IS.toList as -- already resolved indirection
    Nothing -> 
      case operand_static_resolve ctxt i (i_op1 i) of
        Unresolved -> [Unresolved] -- unresolved indirection
        External sym ->
          if "libc_start_main" `isInfixOf` sym then
            [Unresolved] -- An indirection: register RDI holds the pointer of the main function
          else
            [External sym]
        ImmediateAddress a  ->
          case IM.lookup (fromIntegral a) $ ctxt_syms ctxt of
            Just sym -> [External sym]
            Nothing  -> if not (address_has_instruction ctxt a) then [External $ showHex a] else [ImmediateAddress a]

-- | Returns true iff the instruction resolves to external targets only.
instruction_jumps_to_external ::
  Context        -- ^ The context
  -> Instr       -- ^ The instruction
  -> Bool
instruction_jumps_to_external ctxt i = 
  all resolve_is_external $ resolve_jump_target ctxt i
 where
  resolve_is_external (External _) = True
  resolve_is_external _            = False

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
  case IM.lookup a $ ctxt_syms ctxt of
    Just sym -> sym
    Nothing  ->
      case unsafePerformIO $ fetch_instruction ctxt a of -- TODO. However, should be safe as result is immutable.
        Just i@(Instr _ _ JMP op1 _ _ _ _)  ->
          case operand_static_resolve ctxt i op1 of
            External sym -> sym
            _ -> "0x" ++ showHex a
        _ -> "0x" ++ showHex a

-- | Tries to retrieve a function name for a @call@-instruction (see @`function_name_of_entry`@).
--
-- Returns the empty string if the given instruction is not a call or a jump.
function_name_of_instruction :: 
  Context  -- ^ The context
  -> Instr -- ^ The instruction
  -> String
function_name_of_instruction ctxt i =
  if is_call (i_opcode i) || is_jump (i_opcode i) then
    case operand_static_resolve ctxt i (i_op1 i) of
      External sym       -> sym
      ImmediateAddress a -> function_name_of_entry ctxt $ fromIntegral a
      Unresolved         -> "indirection@" ++ showHex (i_addr i)
  else
    ""





-- | Given a resolved jump target, get a possibly empty list of internal addresses to which the jump target can jump.
get_internal_addresses ::
  ResolvedJumpTarget -- ^ A resolved jump target
  -> [Int]
get_internal_addresses (External _)         = []
get_internal_addresses Unresolved           = []
get_internal_addresses (ImmediateAddress a) = [fromIntegral a]


    

        


-- | Shows the block associated to the givern blockID.
show_block ::
  CFG    -- ^ The CFG
  -> Int -- ^ The blockID
  -> String
show_block g b = 
  let instrs = im_lookup ("show_block: Block " ++ show b ++ "in cfg.") (cfg_blocks g) b in
       show b ++ " ["
    ++ showHex (head instrs)
    ++ ","
    ++ showHex (last instrs)
    ++ "]" 

-- | Shows invariants.
show_invariants 
  :: CFG        -- ^ The CFG
  -> Invariants -- ^ The invariants
  -> String
show_invariants g invs = intercalate "\n\n" $ map show_entry $ IM.toList $ invs
 where
  show_entry (blockId, p) =  "Block " ++ show_block g blockId ++ ":\n" ++ show p







instance IntGraph CFG where
  intgraph_post = post
  intgraph_V    = IM.keysSet . cfg_blocks
