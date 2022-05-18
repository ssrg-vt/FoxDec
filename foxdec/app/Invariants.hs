{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}

module Invariants where


import Base
import Pass.CFG_Gen
import Analysis.Context
import VerificationReportInterface
import Analysis.Propagation
import Generic.Address (AddressWord64(AddressWord64))

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List 
import Data.Foldable

import Control.Monad.State.Strict
import Control.Monad.Extra (concatMapM)
import Data.Functor.Identity
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)

usage_msg = "Usage:\n\n  foxdec-invariants-exe NAME.report ADDRESS\n\nHere NAME refers to the NAME used when running foxdec-exe and ADDRESS is a hexadecimal address prefixed with 0x.\nRun this program from the same directory foxdec-exe was run."

-- | Read in first command-line argument, check whether it is a .report file.
-- Read the file, and use the "VerificationReportInterface" to get an overview of the instructions.
main = do
  args <- getArgs
  if length args /= 2 then
    putStrLn usage_msg
  else do
    exists <- doesFileExist $ head args
    if exists then do
      a     <- read_address $ args !! 1
      ctxt  <- ctxt_read_report $ head args
      ctxt_get_inv ctxt a
    else
      putStrLn $ "File: " ++ show (head args) ++ " does not exist."
 where
  read_address str = do
    if take 2 str == "0x" then
      return $ readHex' $ drop 2 str 
    else do
      putStrLn usage_msg
      error $ "Invalid second argument."

-- | Given an address @a@, retrieve all invariants (the address may occur in mulitple entries).
-- Produce the supremum of all found invariants.
ctxt_get_inv :: Context -> Int -> IO () 
ctxt_get_inv ctxt a = do
  entries <- retrieve_io $ ctxt_get_function_entries ctxt
  invs    <- concatMapM get_invariant_per_entry $ S.toList entries
  if invs == [] then do
    putStrLn $ "Address " ++ showHex a ++ " has no invariant."
  else do
    putStrLn $ "Invariant at address " ++ showHex a
    putStrLn $ show invs
 where
  get_invariant_per_entry entry = do
    case ctxt_get_invariant entry a ctxt of 
      Left _    -> return $ []
      Right inv -> return $ [inv]


