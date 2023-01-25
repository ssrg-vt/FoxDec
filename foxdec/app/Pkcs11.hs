{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}


{-|
Module      : Pkcs11
Description : A small program for obtaining entry addresses from a library that only exports a @C_GetFunctionList@ function.
See http://docs.oasis-open.org/pkcs11/pkcs11-base/v2.40/os/pkcs11-base-v2.40-os.html#_Toc416959728


We run function C_GetFunctionList on an empty intial state. The postcondition should contain:

  [RDI0,8] == a_t

Here a_t is the address of a table of 8-byte pointers stored in a data-section.
We read the table and output a list of read pointers, which are all entry addresses of functions exposed by the library.

Usage:
1.) first, dump the MachO library including its data-sections. For example:

  ./scripts/dump_macho.sh /Applications/Firefox.app/Contents/MacOS/libnssckbi.dylib libnssckbi 1

Note the extra "1" at the end, which ensures that data is also dumped.

2.) Run this program using the entry point of the C_GetFunctionList function. For example:

  foxdec-pkcs11-exe 9c0 examples/firefox_libnssckbi/ libnssckbi

One can double check whether this entry address corresponds to this symbol by executing:

  nm --defined-only /Applications/Firefox.app/Contents/MacOS/libnssckbi.dylib

  00000000000009c0 T _C_GetFunctionList

3.) The produced list of entry addresses can be copy-pased into the .entry file (except for the first entry, which seems to be something different).

-}
module Pkcs11 where



import Base
import Config

import Analysis.Context
import Analysis.ControlFlow

import Data.SymbolicExpression

import Parser.ParserDump
import Parser.ParserSymbols
import Parser.ParserSections
import Parser.ParserIndirections
import Parser.ParserCalls 

import Generic.SymbolicConstituents
import Instantiation.SymbolicPropagation

-- import Pass.ACode_Gen
import Generic.Propagation
import Data.CallGraph
import X86.Conventions
import X86.Register (Register(..))
import Generic.Address (AddressWord64(AddressWord64))

import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List (intercalate,delete,isInfixOf,partition)
import Data.Foldable
import qualified Data.Serialize as Cereal hiding (get,put)
import qualified Data.ByteString as BS (readFile,writeFile) 
import Data.IORef

import Debug.Trace
import System.IO
import System.Process (callCommand)
import System.Timeout (timeout)
import System.Directory (doesFileExist,createDirectoryIfMissing)
import Data.Functor.Identity
import System.Console.ArgParser



-- the main algorithm
run_with_ctxt :: Int -> StateT Context IO ()
run_with_ctxt entry = do
  -- read the dump
  -- ctxt_read_dump
  ctxt <- get
  fctxt <- ctxt_mk_fcontext entry

  -- generate CFG for entry
  (new_calls,g) <- liftIO $ cfg_gen ctxt entry
  -- generate invariants for the entry function
  let p          = init_pred fctxt "main" IM.empty S.empty S.empty
  let (invs,vcs) = do_prop fctxt g 0 p

  case IM.toList invs of
    [(0,p)] -> do
      -- retrieve the postcondition
      let s = fst $ sexec_block fctxt (fetch_block g 0) Nothing p
      -- lookup region [RDI0,8]
      case stry_deterministic fctxt $ evalSstate (sread_mem fctxt (mk_svalue fctxt (SE_Var $ SP_Reg RDI)) (simmediate fctxt 8)) s of
        Just (SE_Immediate a) -> read_pointer_table ctxt a
        _                     -> error $ "Failed to read _C_GetFunctionList function from binary."
    _ -> error $ "Failed to read _C_GetFunctionList function from binary."

read_pointer_table ctxt a = do
  liftIO $ putStrLn $ "Reading pointer table at address " ++ showHex a
  let as = do_read a
  liftIO $ putStrLn $ intercalate "\n" $ map (\a -> "0x" ++ showHex a) as
 where
  do_read a =
    case read_from_ro_datasection ctxt a 8 of
      Nothing  -> []
      Just 0   -> []
      Just ptr -> ptr:do_read (a+8)


ctxt_mk_fcontext :: Int -> StateT Context IO FContext
ctxt_mk_fcontext entry = do
  ctxt        <- get
  return $ mk_fcontext ctxt entry


data Args =  Args String String String
  deriving (Show)

argsParser = Args
  `parsedBy` reqPos "entry"    `Descr` "Entry address of function _C_GetFunctionList (hexadecimal)."
  `andBy`    optPos [] "dirname"  `Descr` "Name of directory (including ending /)"
  `andBy`    optPos [] "filename" `Descr` "Basename of file (without directory) without dot and without file-extension."

run (Args entry_str dirname name) = do
  config <- parse_config "./config/config.dhall"

  let entry = readHex' entry_str
  let dirname' = if last dirname  == '/' then dirname else dirname ++ "/"
  binary <- read_binary dirname' name
  case binary of
    Nothing -> putStrLn $ "Cannot read binary file: " ++ dirname' ++ name
    Just b  -> do
                 ioref <- newIORef IM.empty
                 evalStateT (run_with_ctxt entry) $! init_context b ioref config dirname' name

  
-- Parse the command line arguments and run
main = do
  withParseResult argsParser run
