{-|
Module      : Conventions 
Description : In this file, we eumerate assumptions made on calling conventions and over external functions.
-}

module Conventions where

import Base
import qualified Data.Map as M
import Debug.Trace

import Data.X86.Register


-- | A list of function names of functions that never return.
is_exiting_function_call f =
  let f' = strip_GLIBC f in 
    f' `elem` [
      "exit", "_exit", "__exit", "___exit",
      "_error","__error", 
      "__stack_chk_fail", "___stack_chk_fail",
      -- "__overflow", 
      "abort", "_abort",
      "_fail",
      "halt",
      "_assert_fail", "__assert_fail", "___assert_fail", "___assert_rtn",
      "err", "verr", "errc", "verrc", "errx", "verrx", 
      "_err", "_verr", "_errc", "_verrc", "_errx", "_verrx",
      "obstack_alloc_failed_handler", "isc_assertion_failed", "isc_error_fatal",
      "PyExc_SystemExit", "pthread_exit", "error_exit",
      "__longjmp_chk", "siglongjmp"
    ]

strip_GLIBC = takeUntilString "@GLIBC" . takeUntilString "@@GLIBC"


-- TODO syscalls differ per OS
is_terminal_syscall i = i `elem`
  [ --   1 -- exit (FREEBSD)
    -- , 431 -- thr_exit (FREEBSD)
    15 -- rt_sigreturn
  , 60 -- exit
  , 231 -- exit_group
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
   ("",".iplt"),
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
callee_saved_registers = map Reg64 [RBX, RBP, RSP, R12, R13, R14, R15]


-- | A list of registers that may be used for return values
return_registers = map Reg64 [RAX]



-- | A list of registers used as parameters
parameter_registers = map Reg64 [RDI, RSI, RDX, RCX, R8, R9]



