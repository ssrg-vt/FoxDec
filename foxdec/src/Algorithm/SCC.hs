{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_HADDOCK hide #-}


module Algorithm.SCC where

import Base

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad
import Data.List
import Debug.Trace
import Data.Ord (comparing)


{--
 - Generating strongly connected components.
 - https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm

 - The only modification is that this algorithm considers SCCs wrt to some \emph{frontier}.
 - A frontier is a set of nodes that is not passed, i.e., we consider a subgraph bounded by 
 - (up to not including) the frontier.
 -
 - We assume the existence of a function post :: G -> Int -> IS.IntSet that returns, 
 - given the graph and the current node, a set of next nodes.
 - We also assume the existence of a function V :: G -> IS.IntSet that returns all vertices.
 -
 -
 - I could get none of the Data.Graph functions to work properly, hence this reimplementation.
--}


data SCC_state = SCC_State {
  scc_indices  :: IM.IntMap Int,
  scc_lowlinks :: IM.IntMap Int,
  scc_index :: Int,
  scc_stack :: [Int],
  scc_return :: [IS.IntSet]
 }

set_index_of v x s = s { scc_indices = IM.insert v (x s) (scc_indices s) }

set_lowlink_of v x s = s { scc_lowlinks = IM.insert v (x s) (scc_lowlinks s) }

set_index x s = s { scc_index = x s }

push v s = s { scc_stack = v:scc_stack s }

pop_and_return v s = do
  let stack        = scc_stack s
      (scc,stack') = break ((==) v) stack in
    s { scc_stack = tail stack', scc_return = (IS.fromList (v:scc) : scc_return s) }


strongconnect :: IntGraph g => g -> Int -> IS.IntSet -> State SCC_state () 
strongconnect g v frontier = do
   modify $ set_index_of   v scc_index
   modify $ set_lowlink_of v scc_index
   modify $ set_index      (((+) 1) . scc_index)
   modify $ push v

   forM_ (IS.toList $ intgraph_post g v) (\w -> do 
     when (not $ w `IS.member` frontier) (do 
       lookup_w_index <- gets (IM.lookup w . scc_indices)
       case lookup_w_index of
         Nothing -> do
           strongconnect g w frontier
           modify $ set_lowlink_of v (\s -> min (scc_lowlinks s IM.! v) (scc_lowlinks s IM.! w))
         Just w_index -> do
           stack <- gets scc_stack
           when (w `elem` stack) $
             modify $ set_lowlink_of v (\s -> min (scc_lowlinks s IM.! v) (scc_indices s IM.! w))
      )
    )

   s <- get
   when (scc_lowlinks s IM.! v == scc_indices s IM.! v) $
     modify $ pop_and_return v


compute_all_sccs :: IntGraph g => g -> IS.IntSet -> State SCC_state ()
compute_all_sccs g frontier = do
  forM_ (IS.toList $ intgraph_V g) (\v -> do
    lookup_v_index <- gets (IM.lookup v . scc_indices)
    when (lookup_v_index == Nothing) $
      strongconnect g v frontier
   )

init_scc_state = SCC_State IM.empty IM.empty 0 [] []

-- Start SCC generation at a given root vertex.
-- Reaches only those vertices reachable from the root.
scc_of :: IntGraph g => g -> Int -> IS.IntSet -> [IS.IntSet]
scc_of g v frontier = scc_return $ execState (strongconnect g v frontier) init_scc_state

-- SCC generation over all vertices.
all_sccs :: IntGraph g => g -> IS.IntSet -> [IS.IntSet]
all_sccs g frontier = scc_return $ execState (compute_all_sccs g frontier) init_scc_state



-- | retrieve a non-trivial SCC, if any exists
graph_nontrivial_scc g@(Edges es) =
  let sccs             = all_sccs g IS.empty
      nontrivial_sccs  = filter is_non_trivial sccs
      nontrivial_scc   = maximumBy (comparing IS.size) sccs in
    nontrivial_scc -- trace ("Found SCC of mutually recursive function entries: " ++ showHex_set nontrivial_scc) nontrivial_scc
 where
  is_non_trivial :: IS.IntSet -> Bool
  is_non_trivial scc = IS.size scc > 1 || graph_is_edge g (head $ IS.toList scc) (head $ IS.toList scc)



-- | find next vertex to consider: either a terminal vertex (if any) or the head of an SCC
graph_find_next :: Graph -> Maybe Int
graph_find_next g@(Edges es) =
  if IM.null es then
    Nothing
  else case find (IS.disjoint (IM.keysSet es) . snd) $ IM.toList es of
    Nothing    -> Just $ head $ IS.toList $ graph_nontrivial_scc g -- no terminal vertex
    Just (v,_) -> Just v                               -- terminal vertex




