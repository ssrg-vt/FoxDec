module Algo.Dominance
    () where
import qualified Data.Graph.Dom                as G
import           Data.IntMap                    ( (!) )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS

domFrontier :: G.Graph -> G.Graph -> Int -> IntSet
domFrontier graph domTree node = localFrontier `IS.union` globalFrontier
  where
    localFrontier :: IntSet
    localFrontier =
        IS.filter (not . isSucc domTree node) $ successors graph node
    globalFrontier :: IntSet
    globalFrontier = undefined

-- | Successor nodes
successors :: G.Graph -> Int -> IntSet
successors graph node = graph ! node

isSucc :: G.Graph -> Int -> Int -> Bool
isSucc graph node maybeSucc = IS.member node $ successors graph node
