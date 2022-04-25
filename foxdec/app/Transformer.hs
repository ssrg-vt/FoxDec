{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict #-}

module Transformer where


import Base
import SimplePred
import Context
import X86_Datastructures
import VerificationReportInterface
import SymbolicExecution
import Pointers
import qualified IR.X86 as X86
import IR.Generic (programControlFlow)

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.List 

import qualified Data.Graph.Dom as G
import qualified Data.Tree as T

import Control.Monad.State.Strict
import Data.Functor.Identity
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Environment (getArgs)




usage_msg = "Usage:\n\n  foxdec-transformer-exe NAME.report ADDRESS\n\nHere NAME refers to the NAME used when running foxdec-exe ADDRESS is a hexadecimal entry address prefixed with 0x."

-- | Read in first command-line argument, check whether it is a .report file.
-- Read the file, and use the "VerificationReportInterface" to get an overview of the instructions.
main = do
  args <- getArgs
  if length args /= 2 then
    putStrLn usage_msg
  else do
    exists <- doesFileExist $ head args
    if exists then do
      entry <- read_address $ args !! 1
      ctxt <- ctxt_read_report $ head args
      ctxt_transform ctxt entry
    else
      putStrLn $ "File: " ++ show (head args) ++ " does not exist."
 where
  read_address str = do
    if take 2 str == "0x" then
      return $ readHex' $ drop 2 str
    else do
      putStrLn usage_msg
      error $ "Invalid second argument.\n\n" ++ usage_msg 



-- | Run a transformation
-- We obtain L0 from the .report file, and then transform it into L1
ctxt_transform :: Context -> Int -> IO ()
ctxt_transform ctxt entry = do
  -- obtain L0
  let l0 = X86.fromContext ctxt entry

  -- Obtain the control flow graph of L0
  let (root,g) = programControlFlow l0

  -- Compute the dominance tree and the dominance frontiers
  let (_,tree) = G.asGraph $ G.domTree (root,g)
  putStrLn $ "\nDOMINANCE FRONTIERS:"
  forM_ (IM.keys g) (\v ->
    putStrLn $ show (v,dominance_frontier g tree v)
   )

  -- explicitize dataflow
  putStrLn $ "\nMAKING DATAFLOWS EXPLICIT:"
  let l0' = X86.canonicalize l0
  putStrLn $ show l0'


  -- Per block, look at the modified registers
  {--
  putStrLn $ "\nVARIABLES PER BLOCK:"
  forM_ (IM.keys g) (\v ->
    putStrLn $ show $ (v,variables_of_block ctxt entry v)
   )
 --}

  -- An incorrect first version of introducing SSA, going form L0 to L1
  {--
  putStrLn $ "\nSSA"
  let l1 = to_ssa RDI $ l0_to_l1 l0
  putStrLn $ show l1
  --}





-- use symbolic exeution to obtain a set of stateparts written to by a basic block
-- returns only register-stateparts
variables_of_block :: Context -> Int ->  Int -> [StatePart]
variables_of_block ctxt entry blockID = 
  let fctxt             = mk_fcontext ctxt entry
      cfg               = ctxt_cfgs ctxt IM.! entry
      instructions      = cfg_instrs cfg IM.! blockID
      precondition      = init_pred fctxt IM.empty S.empty
      (postcondition,_) = tau_block fctxt instructions Nothing precondition
      Predicate p _     = precondition
      Predicate q _     = postcondition in
    filter (is_modified_register p q) (M.keys q)
 where
  is_modified_register p q sp@(SP_Reg r) = M.lookup sp p /= M.lookup sp q
  is_modified_register p q _             = False





-- Computes the dominance frontier
-- see https://www.ed.tus.ac.jp/j-mune/keio/m/ssa2.pdf
dominance_frontier :: G.Graph -> G.Graph -> Int -> IS.IntSet
dominance_frontier g tree n = IS.union df_local df_up
 where
  -- the local part DF_{local}
  -- "idom y = n" is determined by looking up the edge (n,y) in the dominance tree
  df_local = IS.filter (not . is_edge tree n) $ succ n g
  -- the up part DF_{up}
  df_up = IS.unions $ get_df_up_child <$> (IS.toList $ succ n tree)
  get_df_up_child c = 
    let df_children = dominance_frontier g tree c in
      IS.filter (not . strictly_dominates n) df_children
  -- does n strictly dominate w?
  -- See if w is reachable from n in the dominance tree, i.e., if n is an ancestor of w.
  dominates n w = n == w || (let post = succ n tree in IS.member w post || any (\n' -> dominates n' w) (IS.toList post))
  strictly_dominates n w = n /= w && dominates n w
  -- the successors of vertex v in graph g
  succ v g = IM.lookup v g `orElse` IS.empty
  -- is (v,v') an edge in the graph?
  is_edge g v v' =
    case IM.lookup v g of
      Nothing -> False
      Just vs -> IS.member v' vs



