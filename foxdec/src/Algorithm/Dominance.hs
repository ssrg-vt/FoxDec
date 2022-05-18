module Algorithm.Dominance
    (domFrontier) where

import qualified Data.Graph.Dom                as G
import           Data.IntMap                    ( (!) )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import qualified Data.IntMap as IM
import Base (orElse)

-- Computes the dominance frontier
-- see https://www.ed.tus.ac.jp/j-mune/keio/m/ssa2.pdf
domFrontier :: G.Graph -> G.Graph -> Int -> IS.IntSet
domFrontier g tree n = IS.union df_local df_up
 where
  -- the local part DF_{local}
  -- "idom y = n" is determined by looking up the edge (n,y) in the dominance tree
  df_local = IS.filter (not . is_edge tree n) $ succ n g
  -- the up part DF_{up}
  df_up = IS.unions $ get_df_up_child <$> IS.toList (succ n tree)
  get_df_up_child c =
    let df_children = domFrontier g tree c in
      IS.filter (not . strictly_dominates n) df_children
  -- does n strictly dominate w?
  -- See if w is reachable from n in the dominance tree, i.e., if n is an ancestor of w.
  dominates n w = n == w || (let post = succ n tree in IS.member w post || any (`dominates` w) (IS.toList post))
  strictly_dominates n w = n /= w && dominates n w
  -- the successors of vertex v in graph g
  succ v g = IM.lookup v g `orElse` IS.empty
  -- is (v,v') an edge in the graph?
  is_edge g v v' =
    case IM.lookup v g of
      Nothing -> False
      Just vs -> IS.member v' vs