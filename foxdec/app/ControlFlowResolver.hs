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

usage_msg = "Usage:\n\n  foxdec-disassembler-exe NAME.report ADDRESS\n\nHere NAME refers to the NAME used when running foxdec-exe and ADDRESS is a hexadecimal address prefixed with 0x.\nRun this program from the same directory foxdec-exe was run."

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
      posts <- ctxt_get_posts ctxt a
      putStrLn $ showHex a ++ " --> " ++ showHex_set posts
    else
      putStrLn $ "File: " ++ show (head args) ++ " does not exist."
 where
  read_address str = do
    if take 2 str == "0x" then
      return $ readHex' $ drop 2 str 
    else do
      putStrLn usage_msg
      error $ "Invalid second argument."

ctxt_get_posts :: Context -> Int -> IO IS.IntSet
ctxt_get_posts ctxt a = do
  entries <- retrieve_io $ ctxt_get_function_entries ctxt
  posts   <- mapM get_post_per_entry $ S.toList entries
  return $ IS.unions posts
 where
  get_post_per_entry entry = do
    post <- stepA ctxt entry a
    case post of
      Left _     -> return $ IS.empty
      Right nxts -> return $ IS.fromList $ map fst nxts


