{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

-----------------------------------------------------------------------------
-- |
--
-- After running FoxDec, a \"verification report\" (an object of type @"Context"@) can be retrieved from the generated .report file (see function @ctxt_read_report@).
-- Essentially, this module provides hooks into some of the information retrieved and derived from the binary,
-- including instructions, invariants, function entry points, etc.
--
-- A verification report is represented by the type @"Context"@, as internally it
-- is just the context passed around and maintained during verification.
--
-- The main flow is to read the .report file and use these functions to retrieve information.
-- The following example reads in a .report file provided as first command-line parameter and outputs the function entries:
-- 
-- >  main = do
-- >    args <- getArgs
-- >    ctxt <- ctxt_read_report $ head args
-- >    putStrLn $ show $ ctxt_get_function_entries ctxt
-- 
--
-- Some of the information is automatically also exported in plain-text format, for easy access.
-----------------------------------------------------------------------------


module VerificationReportInterface
  (
    Retrieve,FunctionEntry,InstructionAddress,
    ctxt_read_report,
    ctxt_get_function_entries,
    ctxt_get_instruction_addresses,
    ctxt_get_indirections,
    ctxt_get_instruction,
    ctxt_get_invariant,
    ctxt_get_internal_function_calls,
    ctxt_get_cfg
  )
where

import Base
import Context
import SimplePred
import X86_Datastructures
import CallGraph

import qualified Data.Serialize as Cereal hiding (get,put)
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS (readFile,writeFile) 








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
  

-- | Retrieve all function entries.
--
-- Returns a set of funtion entries.
ctxt_get_function_entries :: Retrieve (S.Set FunctionEntry) 
ctxt_get_function_entries = Right . S.fromList . IM.keys . ctxt_report


-- | Retrieve all instruction addresses.
--
-- Returns a set of instruction addresses.
ctxt_get_instruction_addresses :: Retrieve (S.Set InstructionAddress)
ctxt_get_instruction_addresses ctxt =
  Right $ S.unions $ map entry_to_addresses $ IM.elems $ ctxt_report ctxt
 where
  entry_to_addresses (Report g _ _ _ _) = S.fromList $ concat $ IM.elems $ cfg_blocks g

-- | Retrieve all indirections
--
-- Returns, a mapping that provides for some instruction addresses a set of jump targets.
ctxt_get_indirections :: Retrieve Indirections
ctxt_get_indirections = Right . ctxt_inds


-- | Retrieve instruction for a given instruction address, both as datastructure and pretty-printed
ctxt_get_instruction :: InstructionAddress -> Retrieve (Instr,String)
ctxt_get_instruction a ctxt =
  case unsafePerformIO $ fetch_instruction ctxt a of -- Should be safe as result is immutable.
    Nothing -> Left $ "Could not disassemble instruction at address: " ++ showHex a
    Just i  -> Right (i,pp_instruction ctxt i)

-- | Retrieve invariant for a given function entry and instruction address
--
-- An invariant is a predicate provding information over registers, memory, flags, and verification conditions.
ctxt_get_invariant :: FunctionEntry -> InstructionAddress -> Retrieve Pred
ctxt_get_invariant entry a ctxt =
  ctxt_get_report ctxt entry >>> report_get_invariant a


-- | Retrieve all internal function calls for a given function entry
--
-- Returns a set of function entries.
ctxt_get_internal_function_calls :: FunctionEntry -> Retrieve (S.Set FunctionEntry)
ctxt_get_internal_function_calls entry ctxt = 
  ctxt_get_report ctxt entry >>> report_get_cfg >>> (Right . toSet . calls_of_cfg ctxt)


-- | Retrieve a CFG for a given function entry
ctxt_get_cfg :: FunctionEntry -> Retrieve CFG
ctxt_get_cfg entry ctxt =
  ctxt_get_report ctxt entry >>> report_get_cfg


-- | Retrieve verification conditions for a given function entry, both as a datastructure and pretty-printed
ctxt_get_vcs :: FunctionEntry -> Retrieve (S.Set VerificationCondition, String)
ctxt_get_vcs entry ctxt = 
  ctxt_get_report ctxt entry >>> (\r -> Right (report_vcs r, summarize_verification_conditions ctxt $ report_vcs r))









(>>>) :: Either a b -> (b -> Either a c) -> Either a c
(>>>) (Left a) _  = Left a
(>>>) (Right b) e = e b

ctxt_get_report :: Context -> FunctionEntry -> Either String Report 
ctxt_get_report ctxt entry =
  case IM.lookup entry $ ctxt_report ctxt of
    Nothing -> Left $ "Cannot find function entry " ++ showHex entry ++ " in verification report."
    Just r  -> Right r

report_get_invariant :: InstructionAddress -> Report -> Either String Pred
report_get_invariant a r =
  case IM.lookup a $ report_invs r of
    Nothing -> Left $ "Cannot find instruction address " ++ showHex a ++ " in verification report."
    Just p  -> Right p

report_get_cfg :: Report -> Either String CFG
report_get_cfg = Right . report_cfg


toSet :: IS.IntSet -> S.Set Int
toSet = S.fromList . IS.toList