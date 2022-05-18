{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

{-|
Module      : VerificationReportInterface
Description : The interface to the @.report@ generated after running FoxDec.

The interface to the @.report@ generated after running FoxDec.
After running FoxDec, a \"verification report\" (an object of type @"Context"@) can be retrieved from the generated .report file (see function @ctxt_read_report@).
Essentially, this module provides hooks into some of the information retrieved and derived from the binary,
including instructions, invariants, function entry points, etc.

A verification report is represented by the type @"Context"@, as internally it
is just the context passed around and maintained during verification.

The main flow is to read the .report file and use these functions to retrieve information.
The following example reads in a .report file provided as first command-line parameter and outputs the function entries:

>  main = do
>    args <- getArgs
>    ctxt <- ctxt_read_report $ head args
>    putStrLn $ show $ ctxt_get_function_entries ctxt

Some of the information is automatically also exported in plain-text format, for easy access.
-}


module VerificationReportInterface
  (
    Retrieve,FunctionEntry,InstructionAddress,
    ctxt_read_report,
    retrieve_io,
    ctxt_get_function_entries,
    ctxt_get_instruction_addresses,
    ctxt_get_indirections,
    ctxt_get_instruction,
    ctxt_get_invariant,
    ctxt_get_internal_function_calls,
    ctxt_get_cfg,
    ctxt_get_function_init,
    ctxt_get_postcondition
  )
where

import Analysis.Context
import Analysis.SymbolicExecution
import Base
import Data.CallGraph
import Data.ControlFlow
import Data.Pointers
import Data.SimplePred
import Pass.CFG_Gen

import qualified Data.Serialize as Cereal hiding (get,put)
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS (readFile,writeFile) 
import System.Exit (die)
import qualified X86.Instruction as X86







-- | The return type when retrieving information from a verification report: either an error message or a result.
type Retrieve a = Context -> Either String a
-- | Function Entries are simply integers
type FunctionEntry      = Int
-- | Instruction Addresses are simply integers
type InstructionAddress = Int



-- | Read in the .report file from a file with the given file name.
--   May produce an error if no report can be read from the file.
--   Returns the verification report stored in the .report file.
ctxt_read_report :: 
   String        -- ^ The filename
   -> IO Context 
ctxt_read_report fname = do
  fcontents <- BS.readFile fname
  case Cereal.decode fcontents of
    Left err   -> error $ "Could not read verification report in file " ++ fname 
    Right ctxt -> return ctxt

-- | Retrieve information from a @"Context"@ read from a .report file, or die with an error message.
-- For example:
--
-- > do
-- >   ctxt <- ctxt_read_report filename
-- >   retrieve_io $ ctxt_get_instruction a ctxt
--
-- This code reads in a .report file with the given @filename@,  and reads the instruction at address @a@ if any.
retrieve_io :: Either String a -> IO a
retrieve_io retrieve_result = do
  case retrieve_result of
    Left err -> die err
    Right result -> return result

 




-- | Retrieve all function entries.
--
-- Returns a set of funtion entries.
ctxt_get_function_entries :: Retrieve (S.Set FunctionEntry) 
ctxt_get_function_entries = Right . S.fromList . IM.keys . ctxt_calls


-- | Retrieve all instruction addresses.
--
-- Returns a set of instruction addresses.
ctxt_get_instruction_addresses :: Retrieve (S.Set InstructionAddress)
ctxt_get_instruction_addresses ctxt =
  Right $ S.unions $ map cfg_to_addresses $ IM.elems $ ctxt_cfgs ctxt
 where
  cfg_to_addresses g = S.fromList $ concat $ IM.elems $ cfg_blocks g

-- | Retrieve all indirections
--
-- Returns a mapping that provides for some instruction addresses a set of jump targets.
ctxt_get_indirections :: Retrieve Indirections
ctxt_get_indirections = Right . ctxt_inds


-- | Retrieve instruction for a given instruction address, both as datastructure and pretty-printed
ctxt_get_instruction :: InstructionAddress -> Retrieve (X86.Instruction,String)
ctxt_get_instruction a ctxt =
  case unsafePerformIO $ fetch_instruction ctxt a of -- Should be safe as result is immutable.
    Nothing -> Left $ "Could not disassemble instruction at address: " ++ showHex a
    Just i  -> Right (i,pp_instruction ctxt i)

-- | Retrieve invariant for a given function entry and instruction address
--
-- An invariant is a predicate provding information over registers, memory, flags, and verification conditions.
ctxt_get_invariant :: FunctionEntry -> InstructionAddress -> Retrieve Pred
ctxt_get_invariant entry a ctxt =
  let fctxt = mk_fcontext ctxt entry in
    case get_invariant fctxt a of
      Nothing -> Left $ "Cannot retrieve invariant for function entry " ++ showHex entry ++ " and instruction address " ++ showHex a ++ " in verification report."
      Just p  -> Right p

-- | Retrieve all internal function calls for a given function entry
--
-- Returns a set of function entries.
ctxt_get_internal_function_calls :: FunctionEntry -> Retrieve (S.Set FunctionEntry)
ctxt_get_internal_function_calls entry ctxt = 
  ctxt_get_cfg entry ctxt >>> (Right . toSet . calls_of_cfg ctxt)


-- | Retrieve a CFG for a given function entry
ctxt_get_cfg :: FunctionEntry -> Retrieve CFG
ctxt_get_cfg = ctxt_get ctxt_cfgs


-- | Retrieve a function initialization for a given function entry
ctxt_get_function_init :: FunctionEntry -> Retrieve FInit
ctxt_get_function_init = ctxt_get ctxt_finits


-- | Retrieve a function initialization for a given function entry
ctxt_get_postcondition :: FunctionEntry -> Retrieve FReturnBehavior
ctxt_get_postcondition = ctxt_get ctxt_calls


-- | Retrieve verification conditions for a given function entry, both as a datastructure and pretty-printed
--ctxt_get_vcs :: FunctionEntry -> Retrieve (S.Set VerificationCondition, String)
--ctxt_get_vcs entry ctxt = 
--  ctxt_get ctxt_vcs entry ctxt >>> (\vcs -> Right (vcs, summarize_verification_conditions ctxt entry))









(>>>) :: Either a b -> (b -> Either a c) -> Either a c
(>>>) (Left a) _  = Left a
(>>>) (Right b) e = e b

ctxt_get :: (Context -> IM.IntMap a) -> FunctionEntry -> Retrieve a
ctxt_get lens entry ctxt =
  case IM.lookup entry $ lens ctxt of
    Nothing -> Left $ "Cannot find function entry " ++ showHex entry ++ " in verification report."
    Just r  -> Right r



toSet :: IS.IntSet -> S.Set Int
toSet = S.fromList . IS.toList
