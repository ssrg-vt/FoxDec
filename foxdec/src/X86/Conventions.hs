{-|
Module      : Conventions 
Description : In this file, we eumerate assumptions made on calling conventions and over external functions.
-}

module X86.Conventions where

import Base
import qualified Data.Map as M
import Debug.Trace
import X86.Register (Register(..))




-- | A list of function names of functions that never return.
is_exiting_function_call f =
  let f' = strip_GLIBC f in 
    f' `elem` [
      "exit", "_exit", "__exit", "___exit",
      "error", "_error","__error", 
      "__stack_chk_fail", "___stack_chk_fail",
      "__overflow", 
      "abort", "_abort",
      "_fail",
      "halt",
      "_assert_fail", "__assert_fail", "___assert_fail", "___assert_rtn",
      "err", "verr", "errc", "verrc", "errx", "verrx", 
      "_err", "_verr", "_errc", "_verrc", "_errx", "_verrx",
      "obstack_alloc_failed_handler"
    ]
 where
  strip_GLIBC = takeUntilString "@GLIBC"



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
   ("",".plt.got"),
   ("",".plt.sec"),
   ("",".fini")
 ]

-- | Sections in the following list are assumed not to be modifiable during execution, i.e., constant.
section_is_unwritable s@(segname,sect_name) = 
  or [
    segname `elem` ["__TEXT","__DATA_CONST"],
    s `elem` [ ("__DATA","__got"), ("__DATA","__const"), ("",".rodata") ],
    s `elem` sections_with_instructions
  ]



-- | A list if registers that are non-volatile, i.e., that must be preserved by a function (callee-saved)
callee_saved_registers = [RBX, RBP, RSP, R12, R13, R14, R15]


-- | A list of registers that may be used for return values
return_registers = [RAX]



-- | A list of registers used as parameters
parameter_registers = [RDI, RSI, RDX, RCX, R8, R9]



