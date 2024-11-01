{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, DeriveGeneric, DefaultSignatures, FlexibleContexts, StrictData #-}

{-|
Module      : Propagation
Description : A generic abstract interpretation algorithm for propagating postcondition-transformations through a control flow graph.

We assume a class where we can do predicate transformation through function @tau@,
and we can merge two predicates through function @join@.
Moreover, we assume an implementation of a function @implies@ that implements symbolic implication.
Given these functions, we provide a generic abstract interpretation algorithm.
-}



module Generic.SymbolicPropagation (
  Propagator(..),
  do_prop
 ) where

import Base
import Analysis.ControlFlow
import Analysis.Context
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Foldable (find)

import Control.Monad.State.Strict hiding (join)
import Debug.Trace
import qualified X86.Instruction as X86

import Generic.SymbolicConstituents 


-- | A class that allows propagation of predicates over a CFG.
class (Show pred) => Propagator ctxt pred where
  -- | Predicate transformation for an edge in in a CFG, over a basic blocks.
  tau     :: ctxt -> [X86.Instruction] -> Maybe [X86.Instruction] -> pred -> (pred,S.Set VerificationCondition)
  -- | A lattice-join
  join    :: ctxt -> String -> pred -> pred -> pred
  -- | Symbolic implication
  implies :: ctxt -> pred -> pred -> Bool



-- The set of edges starting in $v$
out_edges g v = S.fromList $ zip (repeat v) $ IS.toList $ post g v



propstate_m   (m,bag,vcs) = m
propstate_bag (m,bag,vcs) = bag
propstate_vcs (m,bag,vcs) = vcs

-- pick edges from the bag, preferring edges to already visited nodes
pick_edge_from_bag :: State (IM.IntMap pred, S.Set (Int,Int), S.Set VerificationCondition) (Maybe ((Int,Int), S.Set (Int,Int)))
pick_edge_from_bag = do
  (m,bag,_) <- get
  case find (\(v0,v1) -> IM.member v1 m) bag of
    Just edge -> return $ Just (edge, S.delete edge bag)
    Nothing   -> return $ S.minView bag
  

-- propagation
-- The state consists of 
-- 		1.) the current mapping of addresses to predicates 
-- 		2.) the current bag (a set of edges to be explored)
prop :: Propagator ctxt pred => ctxt -> CFG -> State (IM.IntMap pred, S.Set (Int,Int), S.Set VerificationCondition) ()
prop ctxt g = do
  pick <- pick_edge_from_bag
  case pick of
    Nothing ->
      return ()
    Just ((v0,v1),bag') -> do
      -- take an edge (v0,v1) out of the bag
      m <- gets propstate_m
      -- do predicate transformation on the currently available precondition of v0
      let p = im_lookup "v0 must have predicate" m v0
      -- this produces q: the precondition for v1
      let (q,vcs') = tau ctxt (fetch_block g v0) (Just $ fetch_block g v1) p
      -- add verification conditions
      modify (\(m,_,vcs) -> (m,bag', S.union vcs vcs'))
      -- store q
      add_predicate q v1
      -- continue
      prop ctxt g
 where
  add_predicate q v1 = do
   (m,bag,vcs) <- get
   case IM.lookup v1 m of
     Nothing -> do
       -- first time visit, store q and explore all outgoing edges
       put (IM.insert v1 q m, S.union bag $ out_edges g v1, vcs)
     Just p -> do
       if implies ctxt p q then
         -- previously visited, no need for further exploration
         return () -- put (IM.insert v1 j m, bag, vcs)
       else do
         let j = join ctxt "prop" p q
         -- previously visited, need to weaken invariant by joining
         put (IM.insert v1 j m,S.union bag $ out_edges g v1, vcs)

-- | Start propagation at the given entry address with the given initial predicate.
-- Returns a set of invariants, i.e., a mapping of instruction addresses to predicates.
do_prop :: Propagator ctxt pred => 
  ctxt      -- ^ The context
  -> CFG    -- ^ The CFG
  -> Int    -- ^ The entry address
  -> pred   -- ^ The initial predicate
  -> (IM.IntMap pred, S.Set VerificationCondition)
do_prop ctxt g entry p = 
  let (m,_,vcs) = execState (prop ctxt g) $ (IM.singleton entry p, out_edges g entry, S.empty) in
    (m,vcs)





