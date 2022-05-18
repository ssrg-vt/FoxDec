{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}

module Postconditions where


import Base
import Data.SimplePred
import Analysis.Context
import Data.CallGraph
import VerificationReportInterface
import Analysis.Propagation
import Data.Pointers
import Generic.Address (AddressWord64(AddressWord64))

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
main = do
  args <- getArgs
  if args == [] then
    putStrLn $ "Usage:\n\n  foxdec-postconditions-exe NAME.report\n\nHere NAME refers to the NAME used when running foxdec-exe.\nRun this program from the same directory foxdec-exe was run."
  else do
    exists <- doesFileExist $ head args
    if exists then do
      ctxt <- ctxt_read_report $ head args
      ctxt_functions ctxt
    else
      putStrLn $ "File: " ++ show (head args) ++ " does not exist."

-- | First retrieve all instruction addresses, and for each address get the postcondition.
ctxt_functions :: Context -> IO ()
ctxt_functions ctxt = do
  entries <- retrieve_io $ ctxt_get_function_entries ctxt
  mapM_ ctxt_function entries
 where
  ctxt_function entry = do
    putStr $  "Function entry " ++ showHex entry
    case IM.lookup entry $ ctxt_calls ctxt of
      Nothing                 -> putStr $ " has no known postconditions." 
      Just UnknownRetBehavior -> putStr $ " has unknown return behavior."
      Just Terminating        -> putStr $ " is terminal."
      Just (ReturningWith q)  -> putStr $ " returns with postcondition:\n" ++ pp_pred q 
    putStrLn "\n\n"
    
