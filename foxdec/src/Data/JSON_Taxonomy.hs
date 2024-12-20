{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict, DeriveGeneric#-}
{-# OPTIONS_HADDOCK prune  #-}


{-|
Module      : JSON_Taxonomy
Description : Provides a taxonomy for the generated output.

Provides a taxonomy for output generated by FoxDec. The output is a JSON object, whose schema is defined by the `JSON` datatype below. That datatype provides its fields, such as `instructions` and `control_flow`.
This is an overview of some of the information retrievable from FoxDec.
For example, we have omitted control flow graphs and per-instruction invariants to keep the size of the JSON object manageable.
However, if more information is required, or if there is a request for information in a different form, then let us know.

On the right hand side, unfold the __synopsis__ for an overview.
-}

module Data.JSON_Taxonomy where


import Base

import Data.SValue
import Data.SymbolicExpression

import Generic.Binary
import Generic.SymbolicConstituents

import Analysis.Context

import qualified Generic.Address as GA
import qualified Generic.Operand as GO
import qualified Generic.Instruction as GI
import qualified X86.Instruction as X86
import X86.Opcode
import X86.Prefix
import X86.Register
import Generic.HasSize (HasSize(sizeof))


import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES


import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List 
import Data.Foldable
import Data.Word
import Data.Aeson
import GHC.Generics





-- * Taxonomy

-- | The top-level JSON datastructure consists of the following fields:
--
--     * `instructions`:  All instructions with their addresses and sizes.
--     * `control_flow`:  For each instruction address, this provides the set of successor instruction addresses. This includes indirections, if they were resolved.
--     * `function_boundaries`:  For each function entry address, this provides an overview of all instruction addresses belonging to that function.
--     * `function_summaries`:  For each function entry address, this provides a pre- and postcondition.
--     * `invariants`: For each each instruction address, for each function address that contains the instruction, this provides an invariant.
--     * `pointer_domains`: For each each instruction address, for each function address that contains the instruction, this provides a symbolic expression for the pointer of each memory operand.
--
-- Example of the entire JSON output:
--
-- > { "instructions": [...], "control_flow": [...], ... }

data JSON = JSON {
  instructions        :: [Instruction],                    -- ^ A list of `Instruction`s.
  control_flow        :: ControlFlow,                      -- ^ The __`ControlFlow`__ essentially is graph with as nodes instruction addresses.
  function_boundaries :: [(Word64,FunctionBoundary)],      -- ^ A mapping from function entry addresses to pretty-printed  __`FunctionBoundary`__s.
  function_summaries  :: [(Word64,FunctionSummary)],       -- ^ A mapping from function entry addresses to __`FunctionSummary`__s.
  invariants          :: [(Word64,[Invariant])],           -- ^ A mapping from instruction addresses to __`Invariant`__s.
  pointer_domains     :: [(Word64, Word64,[Maybe SValue])] -- ^ Per instruction address, per function entry, the __`PointerDomain`__ for each memory operand.

  }
  deriving Generic


-- | The __ControlFlow__ is provided as a mapping from instruction addresses to lists of instruction addresses.
--
-- Example of JSON output:
-- 
--  > [5907,[5909,5929]]
--
-- This states the instruction at address 5907 has two possible successor instruction addresses (5909 and 5929).
type ControlFlow = [(Word64, [Word64])]


-- | The __FunctionBoundary__ is provided as a pretty-printed representation of all instruction addresses covered by the function.
--
-- Example of JSON output:
-- 
--  > "1200-->1238 ; 1280-->1284"
--
-- This __Function Boundary__ states that the function spans two non-consecutive ranges of instruction addresses.

type FunctionBoundary = String


-- | A __FunctionSummary__ contains:
--
--   * a preconditon: a __`Predicate`__ that holds whenever the function is called within the binary.
--   * a postconditon: a __`Postcondition`__ that holds whenever the function is returning, if the function returns normally.
--
--
-- Example of JSON output:
--
-- > {"precondition" : [...], "postcondition" : [...]}
data FunctionSummary = FunctionSummary {
    precondition :: String, -- TODO
    postcondition :: Postcondition
  }
  deriving Generic

-- | An __Invariant__ contains:
--
--   * the entry address of the function in which the instruction occurs. Note that (rarely) the same instruction can occur in multiple functions.
--     Therefore, we provide an invariant per entry.
--   * an invariant: a `Predicate`.
--
-- Example of JSON output:
--
-- >  [4320,[[{"SP_Reg":"RIP"},{"SE_Immediate":4324}],[{"SP_Reg":"RAX"},...]]]
--
-- The instruction occurs in the function with entry address @4320@. Register @RIP@ is set to @4324@. Register @RAX@ is always equal to iths initial value.
type Invariant = (Word64,Data.JSON_Taxonomy.Predicate) -- TODO





-- | A __Postcondition__ provides information on the return status of a function.
-- 
-- Example of JSON output:
--
-- > {"Terminating" : [] }
data Postcondition =
    Terminating              -- ^ The function does never return
  | ReturningWith Data.JSON_Taxonomy.Predicate  -- ^ The function returns in a state satisfying the __`Predicate`__ TODO
  | UnknownRetBehavior       -- ^  It is unknown whether the function returns or not
 deriving Generic


-- | A __Predicate__ is a mapping from state parts (registers, memory, flags) to symbolic expressions. TODO
-- 
-- Example of JSON output:
--
--  > [
--  >   [{"SP_Reg":"RDI"},{"Bottom":{"FromPointerBases":[{"Malloc":[4420,""]}]}}],
--  >   [{"SP_Reg":"RSI"},{"SE_Immediate":8217}]
--  > ]
--
--  This predicate states that register @RDI@ is a pointer with as base the return value of @malloc@, called at address @4420@.
--  Register @RSI@ contains an immediate value.
type Predicate = M.Map StatePart SValue



-- | An __Instruction__ has an address, a prefix (which may also be @null@), a mnemonic/opcode, a list of operands (possibly empty) and a size in bytes.
--
-- Example of JSON output:
--
-- > {"size":7,"prefix":"BND","addr":4420,"opcode":"JMP","operands":[{"Memory":[{"AddressImm":11869},8]}]}
data Instruction = Instruction {
  addr     :: Word64,            -- ^ address
  prefix   :: Maybe Prefix,      -- ^ prefix, e.g., lock or repz
  opcode   :: Opcode,            -- ^ opcode/mnemonic
  operands :: [Operand],         -- ^ possibly empty list of operands 
  size     :: Int                -- ^ size of instruction
 }
 deriving Generic


-- | An __Operand__ is either a memory operand, an effective address (in case of @LEA@), a register or an immediate value.
--
-- Example of JSON output:
--
-- > {"Memory":[{"AddressPlus":[{"AddressRegister":"RIP"},{"AddressImm":11869}]},8]}
data Operand =
    Memory Address Int           -- ^ A region in memory, with an `Address` and a size in bytes
  | EffectiveAddress Address     -- ^ An address itself, but not the value stored at the address.
  | Register Register            -- ^ A storage location such as a register or a variable
  | Immediate Word64             -- ^ An immediate value
  deriving Generic



-- | An __Address__ is the unresolved address computation in the memory operand of an instruction.
--
-- Example of JSON output:
--
-- > {"AddressPlus":[{"AddressRegister":"RIP"},{"AddressImm":11869}]}
data Address =
    AddressRegister Register      -- ^ Reading a pointer from a storage
  | AddressImm Word64             -- ^ Immediate value 
  | AddressPlus Address Address   -- ^ Plus
  | AddressMinus Address Address  -- ^ Minus
  | AddressTimes Address Address  -- ^ Times
  deriving Generic




mk_json_address (GA.AddressStorage r) = AddressRegister r
mk_json_address (GA.AddressImm i) = AddressImm i
mk_json_address (GA.AddressPlus a0 a1) = AddressPlus (mk_json_address a0) (mk_json_address a1)
mk_json_address (GA.AddressMinus a0 a1) = AddressMinus (mk_json_address a0) (mk_json_address a1)
mk_json_address (GA.AddressTimes a0 a1) = AddressTimes (mk_json_address a0) (mk_json_address a1)

mk_json_operand (GO.Memory a si) = Memory (mk_json_address a) si
mk_json_operand (GO.EffectiveAddress a) = EffectiveAddress (mk_json_address a)
mk_json_operand (GO.Storage r) = Register r
mk_json_operand (GO.Immediate i) = Immediate i

mk_json_instruction i@(GI.Instruction (GA.AddressWord64 a) p op Nothing srcs _) = Instruction a p op (map mk_json_operand srcs) (sizeof i)

mk_json_predicate :: Analysis.Context.Predicate -> Data.JSON_Taxonomy.Predicate
mk_json_predicate (Sstate regs mem flg) = M.mapKeys mk_reg regs
 where
  mk_reg r = SP_Reg r

mk_json_post Nothing                                    = Data.JSON_Taxonomy.UnknownRetBehavior 
mk_json_post (Just Analysis.Context.UnknownRetBehavior) = Data.JSON_Taxonomy.UnknownRetBehavior
mk_json_post (Just Analysis.Context.Terminating)        = Data.JSON_Taxonomy.Terminating
mk_json_post (Just (Analysis.Context.ReturningWith q))  = Data.JSON_Taxonomy.ReturningWith $ mk_json_predicate q  

