-----------------------------------------------------------------------------
-- |
-- In this file, we eumerate assumptions made on calling conventions and over external functions.
-----------------------------------------------------------------------------

module Conventions where

import X86_Datastructures
import Base
import qualified Data.Map as M




-- | A list of function names of functions that never return.
exiting_function_calls = [
  "exit", "_exit", "__exit", "___exit",
  "error", "_error","__error", 
  "__stack_chk_fail", "___stack_chk_fail",
  "__overflow", 
  "abort", "_abort",
  "_fail",
  "halt",
  "_assert_fail", "__assert_fail", "___assert_fail", "___assert_rtn",
  "err", "verr", "errc", "verrc", "errx", "verrx", 
  "_err", "_verr", "_errc", "_verrc", "_errx", "_verrx"
 ]


-- | An overview of which sections can be modified by external functions.
-- Other sections are assumed NOT to be modified by external functions.
sections_modifiable_by_external_functions = [
 ]

-- | Overview of sections with instructions.
sections_with_instructions = [
   ("__TEXT","__text"), -- TODO ELF
   ("__TEXT","__stubs"),
   ("__TEXT","__stub_helper"),
   ("__DATA_CONST","__got"),
   ("__DATA","__la_symbol_ptr"),
   ("__DATA","__nl_symbol_ptr"),

   ("",".text"),
   ("",".init"),
   ("",".plt"),
   ("",".fini")
 ]

-- | Sections in the following list are assumed not to be modifiable during execution, i.e., constant.
section_is_unwritable s@(segname,sect_name) = 
  segname `elem` ["__TEXT","__DATA_CONST"]
  ||
  s `elem` [
    ("__DATA","__got"),
    ("__DATA","__const")
  ]



-- | A list if registers that are non-volatile, i.e., that must be preserved by a function (callee-saved)
callee_saved_registers = [RBX, RBP, RSP, R12, R13, R14, R15]


-- | A list of registers that may be used for return values
return_registers = [RAX, XMM0]



-- | A list of registers used as parameters
parameter_registers = [RDI, RSI, RDX, RCX, R8, R9]


-- | An overapproximation of the maximum number of entries in a jump table.
-- Does not affect soundness, but if the value is set too low, then more indirections may be left unresolved.
max_jump_table_size = 20000
