{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, FlexibleInstances, StrictData #-}

{-|
Module      : SymbolicPropagation
Description : Provides an instantiation of all functions necessary to do symbolic propagation
-}
module InputLifting.ExternalFunctions where


import Base
import Config

import Conventions

import Data.X86.Register




data ExternalFunctionOutput = FreshPointer | UnknownReturnValue | Input Register

data ExternalFunctionBehavior = ExternalFunctionBehavior {
  f_inputs :: [Register],
  f_output :: ExternalFunctionOutput
 }


param 0 = Reg64 RDI 
param 1 = Reg64 RSI
param 2 = Reg64 RDX
param 3 = Reg64 RCX
param 4 = Reg64 R8
param 5 = Reg64 R9


pure_and_fresh = ExternalFunctionBehavior [] FreshPointer
pure_and_unknown = ExternalFunctionBehavior [] UnknownReturnValue

external_function_behavior :: String -> ExternalFunctionBehavior
-- | a list of some function that return a heap-pointer through RAX.
-- The pointer is assumed to  be fresh.
external_function_behavior "_malloc" = pure_and_fresh
external_function_behavior "malloc" = pure_and_fresh
external_function_behavior "_malloc_create_zone" = pure_and_fresh
external_function_behavior "_malloc_default_zone" = pure_and_fresh
external_function_behavior "_malloc_zone_malloc" = pure_and_fresh
external_function_behavior "isc__mem_allocate" = pure_and_fresh
external_function_behavior "isc_mem_allocate" = pure_and_fresh
external_function_behavior "PyType_GenericAlloc" = pure_and_fresh
external_function_behavior "PyType_GenericNew" = pure_and_fresh
external_function_behavior "PySys_GetObject" = pure_and_fresh
external_function_behavior "aligned_alloc" = pure_and_fresh
external_function_behavior "_calloc" = pure_and_fresh
external_function_behavior "calloc" = pure_and_fresh
external_function_behavior "_malloc_zone_calloc" = pure_and_fresh
external_function_behavior "_mmap" = pure_and_fresh
external_function_behavior "_av_mallocz" = pure_and_fresh
external_function_behavior "___error" = pure_and_fresh
external_function_behavior "_localeconv" = pure_and_fresh
external_function_behavior "localeconv" = pure_and_fresh
external_function_behavior "strerror" = pure_and_fresh
external_function_behavior "_strerror" = pure_and_fresh
external_function_behavior "_strerror_r" = pure_and_fresh
external_function_behavior "_wcserror" = pure_and_fresh
external_function_behavior "__wcserror" = pure_and_fresh
external_function_behavior "_EVP_CIPHER_CTX_new" = pure_and_fresh
external_function_behavior "strdup" = pure_and_fresh
external_function_behavior "_strdup" = pure_and_fresh
external_function_behavior "_getenv" = pure_and_fresh
external_function_behavior "getenv" = pure_and_fresh
external_function_behavior "_open" = pure_and_fresh
external_function_behavior "_fts_read$INODE64" = pure_and_fresh
external_function_behavior "_fts_open$INODE64" = pure_and_fresh
external_function_behavior "_opendir$INODE64" = pure_and_fresh
external_function_behavior "fopen" = pure_and_fresh
external_function_behavior "_fopen" = pure_and_fresh
external_function_behavior "_fdopen" = pure_and_fresh
external_function_behavior "_wfdopen" = pure_and_fresh
external_function_behavior "_fgetln" = pure_and_fresh
external_function_behavior "fgetln" = pure_and_fresh
external_function_behavior "_setlocale" = pure_and_fresh
external_function_behavior "_wsetlocale" = pure_and_fresh
external_function_behavior "__ctype_b_loc" = pure_and_fresh
external_function_behavior "dcgettext" = pure_and_fresh
external_function_behavior "nl_langinfo" = pure_and_fresh
external_function_behavior "setlocale" = pure_and_fresh
external_function_behavior "__errno_location" = pure_and_fresh
external_function_behavior "_popen" = pure_and_fresh
external_function_behavior "__ctype_tolower_loc" = pure_and_fresh
external_function_behavior "__ctype_toupper_loc" = pure_and_fresh
external_function_behavior "readdir" = pure_and_fresh
external_function_behavior "getmntent" = pure_and_fresh
external_function_behavior "setmntent" = pure_and_fresh
external_function_behavior "dlsym" = pure_and_fresh
external_function_behavior "dlopen" = pure_and_fresh
external_function_behavior "dlerror" = pure_and_fresh
-- | A list of some functions that are assumed not to change the state in any significant way, and that return an unknown bottom value through RAX
external_function_behavior "feof" = pure_and_unknown
external_function_behavior "_feof" = pure_and_unknown
external_function_behavior "_getc" = pure_and_unknown
external_function_behavior "getc" = pure_and_unknown
external_function_behavior "fgetc" = pure_and_unknown
external_function_behavior "_fgetc" = pure_and_unknown
external_function_behavior "_fgetwc" = pure_and_unknown
external_function_behavior "fgetwc" = pure_and_unknown
external_function_behavior "_fnmatch" = pure_and_unknown
external_function_behavior "_fputc" = pure_and_unknown
external_function_behavior "fputc" = pure_and_unknown
external_function_behavior "_close" = pure_and_unknown
external_function_behavior "close" = pure_and_unknown
external_function_behavior "fwrite" = pure_and_unknown
external_function_behavior "_fwrite" = pure_and_unknown
external_function_behavior "_fflush" = pure_and_unknown
external_function_behavior "___maskrune" = pure_and_unknown
external_function_behavior "_getbsize" = pure_and_unknown
external_function_behavior "_printf" = pure_and_unknown
external_function_behavior "printf" = pure_and_unknown
external_function_behavior "vprintf" = pure_and_unknown
external_function_behavior "_fprintf" = pure_and_unknown
external_function_behavior "fprintf" = pure_and_unknown
external_function_behavior "vfprintf" = pure_and_unknown
external_function_behavior "_fprintf_l" = pure_and_unknown
external_function_behavior "fwprintf" = pure_and_unknown
external_function_behavior "_fwprintf_l" = pure_and_unknown
external_function_behavior "__fprintf_chk" = pure_and_unknown
external_function_behavior "__printf_chk" = pure_and_unknown
external_function_behavior "_putchar" = pure_and_unknown
external_function_behavior "_puts" = pure_and_unknown
external_function_behavior "fputs" = pure_and_unknown
external_function_behavior "_fputs" = pure_and_unknown
external_function_behavior "_btowc" = pure_and_unknown
external_function_behavior "btowc" = pure_and_unknown
external_function_behavior "mbtowc" = pure_and_unknown
external_function_behavior "_mbtowc" = pure_and_unknown
external_function_behavior "_mbrtowc" = pure_and_unknown
external_function_behavior "mbrtowc" = pure_and_unknown
external_function_behavior "_atof" = pure_and_unknown
external_function_behavior "atof" = pure_and_unknown
external_function_behavior "_strcmp" = pure_and_unknown
external_function_behavior "_strncmp" = pure_and_unknown
external_function_behavior "strcmp" = pure_and_unknown
external_function_behavior "strncmp" = pure_and_unknown
external_function_behavior "strlen" = pure_and_unknown
external_function_behavior "_ilogb" = pure_and_unknown
external_function_behavior "_atoi" = pure_and_unknown
external_function_behavior "_getopt" = pure_and_unknown
external_function_behavior "getopt_long" = pure_and_unknown
external_function_behavior "_free" = pure_and_unknown
external_function_behavior "_warn" = pure_and_unknown
external_function_behavior "_warnx" = pure_and_unknown
external_function_behavior "__errno_location" = pure_and_unknown
external_function_behavior "__libc_start_main" = pure_and_unknown
external_function_behavior "__cxa_finalize" = pure_and_unknown
external_function_behavior "perror" = pure_and_unknown
external_function_behavior "fclose" = pure_and_unknown
external_function_behavior "free" = pure_and_unknown
external_function_behavior "unlink" = pure_and_unknown
external_function_behavior "unlinkat" = pure_and_unknown
external_function_behavior "strspn" = pure_and_unknown
external_function_behavior "utimensat" = pure_and_unknown
external_function_behavior "fdatasync" = pure_and_unknown
external_function_behavior "fsync" = pure_and_unknown
external_function_behavior "isatty" = pure_and_unknown
external_function_behavior "strcspn" = pure_and_unknown
external_function_behavior "memcmp" = pure_and_unknown
external_function_behavior "_memcmp" = pure_and_unknown
external_function_behavior "isprint" = pure_and_unknown
external_function_behavior "iswprint" = pure_and_unknown
external_function_behavior "_isprint_l" = pure_and_unknown
external_function_behavior "_iswprint_l" = pure_and_unknown
external_function_behavior "__cxa_atexit" = pure_and_unknown
external_function_behavior "towlower" = pure_and_unknown
external_function_behavior "towupper" = pure_and_unknown
external_function_behavior "iswalnum" = pure_and_unknown
external_function_behavior "fseeko" = pure_and_unknown
external_function_behavior "fflush" = pure_and_unknown
external_function_behavior "_fclose" = pure_and_unknown
external_function_behavior "_fgets" = pure_and_unknown
external_function_behavior "_ferror" = pure_and_unknown
external_function_behavior "_strtol" = pure_and_unknown
external_function_behavior "_strtoul" = pure_and_unknown
external_function_behavior "_munmap" = pure_and_unknown
external_function_behavior "fread_unlocked" = pure_and_unknown



-- | A list of some functions that return bottom and write to pointers passed by parameters
--external_function_behavior "_sysctlbyname" = ExternalFunctionBehavior [param 2, param 4] UnknownReturnValue
--external_function_behavior "_fstat$INODE64" = ExternalFunctionBehavior [param 1] UnknownReturnValue
--external_function_behavior "_fstatfs$INODE64" = ExternalFunctionBehavior [param 1] UnknownReturnValue
--external_function_behavior "_statfs$INODE64" = ExternalFunctionBehavior [param 1] UnknownReturnValue
external_function_behavior "snprintf"             = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "_snprintf"            = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "_snprintf_l"          = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "_snwprintf"           = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "_snwprintf_l"         = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "__snprintf_chk"       = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "_vsnprintf"           = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "sprintf"              = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "_sprintf"             = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "___bzero"             = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "sigprocmask"          = ExternalFunctionBehavior [param 2] UnknownReturnValue
external_function_behavior "__strcat_chk"         = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "strcat"               = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "strlcpy"              = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "___strlcpy_chk"       = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "sigemptyset"          = ExternalFunctionBehavior [param 0] UnknownReturnValue
external_function_behavior "sigaction"            = ExternalFunctionBehavior [param 2] UnknownReturnValue
external_function_behavior "localtime"            = ExternalFunctionBehavior [param 0] FreshPointer
external_function_behavior "memset"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "_memset"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "__memset_chk"         = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "___memset_chk"        = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "_index"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_rindex"              = ExternalFunctionBehavior [] $ Input $ param 0

-- A list of functions that return a pointer given to them by a parameter
external_function_behavior "_realloc"             = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "reallocarray"         = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_malloc_zone_realloc" = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_recallocarray"       = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "realloc"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "mremap"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_strcpy"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "__strcpy_chk"         = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "_strncpy"             = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "strcpy"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "strncpy"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "stpcpy"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "memcpy"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "_memcpy"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "__memcpy_chk"         = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "___memcpy_chk"        = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "__memmove_chk"        = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "memmove"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "_memmove"             = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "strcat"               = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "_strcat"              = ExternalFunctionBehavior [param 0] $ Input $ param 0
external_function_behavior "strchr"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_strchr"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "strrchr"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_strrchr"             = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_memchr"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "memchr"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "strstr"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_strstr"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_strpbrk"             = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "strpbrk"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_strtok"              = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "strtok"               = ExternalFunctionBehavior [] $ Input $ param 0
external_function_behavior "_strlen"              = ExternalFunctionBehavior [] $ Input $ param 0


external_function_behavior f
 | is_exiting_function_call f = pure_and_unknown
 | otherwise                  = ExternalFunctionBehavior [] UnknownReturnValue -- trace ("Unknown external function: " ++ f) $ 


