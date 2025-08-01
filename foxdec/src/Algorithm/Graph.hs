{-|
Module      : Algorithm.Graph
Description : Graph searching functions
-}

module Algorithm.Graph where

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Tree.View as TV
import Data.Maybe
import Data.List.Extra (firstJust)

import Base

import Control.Monad.State
import Control.Monad (forM_,msum)
import Control.Monad.Extra (concatMapM, firstJustM)

----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- Some graph related functions
----------------------------------------------------------------------------
----------------------------------------------------------------------------

data NodeData = Unfinished Int | Vertices [Int]
  deriving Eq

instance Show NodeData where
  show (Unfinished v)    = show v ++ " ..."
  show (Vertices vs)     = show vs


-- Generate a spanning tree for the given graph
-- Example:
--
-- [0,1]
-- ├╴[2,5,4]
-- │  ├╴[3]
-- │  └╴5 ...
-- └╴3 ...
dfs_spanning_tree :: IntGraph g => g -> Int -> State IS.IntSet (T.Tree NodeData)
dfs_spanning_tree g v = do
  -- let nexts        = IM.lookup v (g_edges g)
  let nexts        = intgraph_post g v
  visited         <- get
  let is_visited   = v `IS.member` visited
  put $ IS.insert v visited
  
  if is_visited then
    return $ T.Node (Unfinished v) []
  else case IS.toList nexts of
    []  -> return $ T.Node (Vertices [v]) []
    [b] -> add_to_root v <$> dfs_spanning_tree g b
    bs  -> T.Node (Vertices [v]) <$> (mapM (dfs_spanning_tree g) bs)
 where
  add_to_root v (T.Node (Vertices bs) t)        = T.Node (Vertices $ v:bs) t
  add_to_root v n@(T.Node (Unfinished _) [])    = T.Node (Vertices [v]) [n]



-- Given a spanning tree, generate paths that repeat cycles a certain number of times
spanning_tree_to_cycles :: Int -> T.Tree NodeData -> [[Int]]
spanning_tree_to_cycles reptition = mk_paths []
 where
  mk_paths path (T.Node (Unfinished b) [])
    | b `elem` path = [path ++ (concat $ replicate reptition $ skipUntil b path)]
    | otherwise     = [path]

  mk_paths path (T.Node (Vertices bs) [])   = [path ++ bs]
  mk_paths path (T.Node (Vertices bs) nxts) = concatMap (mk_paths (path ++ bs)) nxts



-- Given a path, extend the path so that it reaches an end node
finish_path :: IntGraph g => g -> [Int] -> Maybe [Int]
finish_path g p = 
  let finish = evalState (path_from_node_to_finish g $ last p) IS.empty in
    case finish of
      Nothing -> Nothing -- error $ "Cannot find path to finish."
      Just p' -> Just $ p ++ tail p'
 where
  path_from_node_to_finish :: IntGraph g => g -> Int -> State IS.IntSet (Maybe [Int])
  path_from_node_to_finish g v = do
    let nexts        = intgraph_post g v
    visited         <- get
    let is_visited   = v `IS.member` visited
    put $ IS.insert v visited

    if is_visited then
      return Nothing
    else case IS.toList nexts of
      [] -> return $ Just [v]
      bs -> 
        if bs == [v] then
          return $ Just [v]
        else do
          path <- firstJustM (path_from_node_to_finish g) bs
          return $ ((:) v) <$> path





-- For the given graph: produce a set of paths towards end-points
-- We compute the spanning tree thorugh a depth-first-search.
-- Given that spanning tree, we repeat some cycles a couple of times.
-- Then we finish the path to some exit-point.
produce_set_of_paths :: IntGraph g => g -> Int -> S.Set [Int]
produce_set_of_paths g reptition = do
  let srcs = IS.toList $ intgraph_sources g in
    S.unions $ map mk_paths srcs
 where
  mk_paths src =
    let tree   = evalState (dfs_spanning_tree g src) IS.empty
        paths  = spanning_tree_to_cycles reptition tree
        paths' = catMaybes $ map (finish_path g) paths in
      S.fromList paths'



-- Find a path from a source to the given vertex
find_path_from_source_to :: IntGraph g => g -> Int -> Maybe [Int]
find_path_from_source_to g trgt_v = firstJust (\v -> evalState (go v) IS.empty) $ IS.toList $ intgraph_sources g
 where
  go :: Int -> State IS.IntSet (Maybe [Int])
  go v = do
    visited <- get
    if v `IS.member` visited then
      return Nothing
    else if v == trgt_v then
      return $ Just [v]
    else do
      modify $ IS.insert v
      let nexts = intgraph_post g v
      path <- firstJustM go $ IS.toList nexts
      return $ ((:) v) <$> path



-- | Traverse graph upwards from a node
-- Produces all visited nodes
graph_traverse_upwards :: IntGraph g => g -> Int -> IS.IntSet
graph_traverse_upwards g v0 = execState (go v0) IS.empty
 where
  go :: Int -> State IS.IntSet ()
  go v = do
    visited <- get
    if v `IS.member` visited then
      return ()
    else do
      modify $ IS.insert v
      let parents = IS.toList $ intgraph_pre g v
      mapM_ go parents


-- find a path that:
-- 1.) at least contains one vertex that satisfies $p$
-- 2.) ends in a nodes satisfying $q$
-- The path is broken down in two parts: one ending in a $p$-vertex, the other starting after the first.
-- The first part is thus always non-empty.
find_path_visiting_resulting_in :: IntGraph g => g -> (Int -> Bool) -> (Int -> Bool) -> Maybe ([Int],[Int])
find_path_visiting_resulting_in g p q =
  let to_be_visited = IS.toList $ IS.filter p $ intgraph_V g
      p_vertices    = catMaybes $ map add_path_to_finish to_be_visited in
    firstJust add_path_to_vertex p_vertices
 where
  add_path_to_vertex (v,p1) =
    case find_path_from_source_to g v of
      Nothing -> Nothing
      Just p0 -> Just (p0,p1)



  add_path_to_finish v =
    case evalState (path_from_node_to_finish v) IS.empty of
      Nothing -> Nothing
      Just p  -> Just (v,p)

  path_from_node_to_finish :: Int -> State IS.IntSet (Maybe [Int])
  path_from_node_to_finish v = do
    let nexts        = intgraph_post g v
    visited         <- get
    let is_visited   = v `IS.member` visited
    put $ IS.insert v visited

    if is_visited then
      return Nothing
    else if q v then
      return $ Just [v]
    else case IS.toList nexts of
      [] -> return Nothing
      bs -> do
        path <- find_path_from_blocks bs 
        return $ ((:) v) <$> path

  find_path_from_blocks []     = return Nothing
  find_path_from_blocks (b:bs) = do
    path <- path_from_node_to_finish b
    case path of
      Nothing -> find_path_from_blocks bs
      Just p  -> return $ Just p


-- find a path that:
-- 0.) start in v0
-- 1.) does not contain nodes satisfying $p$
-- 2.) ends in a nodes satisfying $q$
-- The path is broken down in two parts: one ending in a $p$-vertex, the other starting after the first.
-- The first part is thus always non-empty.
find_path_satisfying_resulting_in :: IntGraph g => g -> Int -> (Int -> Bool) -> (Int -> Bool) -> Maybe [Int]
find_path_satisfying_resulting_in g v0 p q = evalState (go v0) IS.empty
 where
  go :: Int -> State IS.IntSet (Maybe [Int])
  go v = do
    let nexts        = intgraph_post g v
    visited         <- get
    let is_visited   = v `IS.member` visited
    put $ IS.insert v visited

    if is_visited then
      return Nothing
    else if not $ p v then
      return Nothing
    else if q v then
      return $ Just [v]
    else case IS.toList nexts of
      [] -> return Nothing
      bs -> do
        path <- firstJustM go bs 
        return $ ((:) v) <$> path




