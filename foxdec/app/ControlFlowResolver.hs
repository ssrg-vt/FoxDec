{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}

module ControlFlowResolver where


import Base
import CFG_Gen
import Context
import X86_Datastructures
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

usage_msg = "Usage:\n\n  foxdec-controlflow-exe NAME.report ADDRESS\n\nHere NAME refers to the NAME used when running foxdec-exe and ADDRESS is a hexadecimal address prefixed with 0x.\nRun this program from the same directory foxdec-exe was run."

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

      entries <- retrieve_io $ ctxt_get_function_entries ctxt
      mapM_ (report_per_entry ctxt a) entries
    else
      putStrLn $ "File: " ++ show (head args) ++ " does not exist."
 where
  read_address str = do
    if take 2 str == "0x" then
      return $ readHex' $ drop 2 str 
    else do
      putStrLn usage_msg
      error $ "Invalid second argument."


  -- Per function entry, check if @a@ is an instruction address occuring in that function.
  report_per_entry ctxt a entry = do
    case IM.lookup entry (ctxt_cfgs ctxt) of
      Nothing  -> return ()
      Just cfg -> do
        when (a `elem` concat (IM.elems $ cfg_blocks cfg)) $ do
           -- If yes, retrieve the post-set and the invariant
          putStrLn $ "FUNCTION ENTRY: " ++ showHex entry ++ "\n"
          post <- stepA ctxt entry a
          case post of
            Left _     -> putStrLn $ "Cannot retrieve post-set for address " ++ showHex a ++ "\n"
            Right nxts -> putStrLn $ "CONTROL FLOW:\n" ++ showHex a ++ " --> " ++ showHex_list (map fst nxts) ++"\n"
          case ctxt_get_invariant entry a ctxt of
            Left _    -> return ()
            Right inv -> putStrLn $ "INVARIANT:\n" ++ show inv
          putStrLn "\n"



