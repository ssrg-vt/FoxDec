{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}

module ControlFlowResolver where


import Base
import Pass.CFG_Gen
import Analysis.Context
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

usage_msg = "Usage:\n\n  foxdec-controlflow-exe NAME.report ADDRESS\n\nHere NAME refers to the NAME used when running foxdec-exe and ADDRESS (optional) is a hexadecimal address prefixed with 0x. If no ADDRESS is provided, all instructions are considered."

-- | Read in first command-line argument, check whether it is a .report file.
-- Read the file, and use the "VerificationReportInterface" to get an overview of the instructions.
main = do
  args <- getArgs
  if length args /= 1 && length args /= 2 then do
    putStrLn usage_msg
  else do
    exists <- doesFileExist $ head args
    if exists then do
      ctxt  <- ctxt_read_report $ head args
      if length args == 1 then do
        as <- retrieve_io $ ctxt_get_instruction_addresses ctxt
        mapM_ (put_control_flow ctxt) as
      else do
        a  <- read_address $ args !! 1
        put_control_flow ctxt a
    else do
      putStrLn $ "File: " ++ show (head args) ++ " does not exist."
 where
  put_control_flow ctxt a = do
    posts <- ctxt_get_posts ctxt a
    putStrLn $ showHex a ++ " --> " ++ showHex_set posts

  read_address str = do
    if take 2 str == "0x" then
      return $ readHex' $ drop 2 str 
    else do
      putStrLn usage_msg
      error $ "Invalid second argument."

-- | Given an address @a@, retrieve the set of next addresses.
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


