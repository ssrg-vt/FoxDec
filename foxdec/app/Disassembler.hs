{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}

module Disassembler where


import Base
import SimplePred
import Context
import X86_Datastructures
import SymbolicExecution
import MachineState
import CFG_Gen
import VerificationReportInterface

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List 
import Data.Foldable

import Control.Monad.State.Strict
import Data.Functor.Identity
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)


-- | Read in first command-line argument, check whether it is a .report file.
-- Read the file, and use the "VerificationReportInterface" to get an overview of the instructions.
main = do
  args <- getArgs
  if args == [] then
    putStrLn $ "Usage:\n\n  foxdec-disassembler-exe NAME.report\n\nHere NAME refers to the NAME used when running foxdec-exe.\nRun this program from the same directory foxdec-exe was run."
  else do
    exists <- doesFileExist $ head args
    if exists then do
      ctxt <- ctxt_read_report $ head args
      ctxt_disassemble ctxt
    else
      putStrLn $ "File: " ++ show (head args) ++ " does not exist."

-- | First retrieve all instruction addresses, and for each address get the instruction.
ctxt_disassemble :: Context -> IO ()
ctxt_disassemble ctxt = do
  addresses <- retrieve_io $ ctxt_get_instruction_addresses ctxt
  mapM_ ctxt_disassemble_address addresses
 where
  ctxt_disassemble_address a = do
    (i,s) <- retrieve_io $ ctxt_get_instruction a ctxt
    putStrLn s


retrieve_io :: Either String a -> IO a
retrieve_io retrieve_result = do
  case retrieve_result of
    Left err -> die err
    Right result -> return result



