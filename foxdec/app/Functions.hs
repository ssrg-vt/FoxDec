{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}

module Functions where


import Base
import Data.SimplePred
import Analysis.Context
import X86_Datastructures
import Data.CallGraph
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


-- | Read in first command-line argument, check whether it is a .report file.
-- Read the file, and use the "VerificationReportInterface" to get an overview of the instructions.
main = do
  args <- getArgs
  if args == [] then
    putStrLn $ "Usage:\n\n  foxdec-functions-exe NAME.report\n\nHere NAME refers to the NAME used when running foxdec-exe.\nRun this program from the same directory foxdec-exe was run."
  else do
    exists <- doesFileExist $ head args
    if exists then do
      ctxt <- ctxt_read_report $ head args
      ctxt_functions ctxt
    else
      putStrLn $ "File: " ++ show (head args) ++ " does not exist."

-- | First retrieve all instruction addresses, and for each address get the instruction.
ctxt_functions :: Context -> IO ()
ctxt_functions ctxt = do
  entries <- retrieve_io $ ctxt_get_function_entries ctxt
  mapM_ ctxt_function entries
 where
  ctxt_function entry = do
    cfg   <- retrieve_io $ ctxt_get_cfg entry ctxt
    let finit = ctxt_get_function_init entry ctxt
    let addresses = concat $ IM.elems $ cfg_blocks cfg

    putStrLn $ "FUNCTION ENTRY: " ++ showHex entry
    putStrLn $ "BOUNDARY: " ++ (intercalate "\n" $ map show_chunk $ mk_consecutive_chunks addresses)
    show_lr_finit finit
    putStrLn $ ""

  show_chunk [i]   = showHex i ++ " (single instruction)"
  show_chunk chunk = showHex (head chunk) ++ "-->" ++ showHex (last chunk)

  show_lr_finit (Right finit) = do
    putStrLn $ summarize_finit $ Just finit
  show_lr_finit _ = return ()



mk_consecutive_chunks :: [Int] -> [[Int]]
mk_consecutive_chunks = split_consecutives . sort
 where
  split_consecutives :: [Int] -> [[Int]]
  split_consecutives []         = []
  split_consecutives [i]        = [[i]]
  split_consecutives (i0:i1:is) = if i1 < i0 + 16 then add_to_hd i0 $ split_consecutives (i1:is) else [i0] : split_consecutives (i1:is)


  add_to_hd :: Int -> [[Int]] -> [[Int]]
  add_to_hd i []       = [[i]]
  add_to_hd i (is:iss) = (i:is) : iss

