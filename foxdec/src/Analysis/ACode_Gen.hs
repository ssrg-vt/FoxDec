{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_HADDOCK hide #-}

module Analysis.ACode_Gen where

import Base
import Analysis.Context
import Algorithm.SCC 
import Analysis.ControlFlow

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad
import Data.List
import Debug.Trace




{--
-- given a cfg, a node and the frontier, returns the SCC of the current node
get_scc_without :: IntGraph g => g -> Int -> IS.IntSet -> IS.IntSet
get_scc_without g v frontier =
 let sccs = scc_of g v $ intset_to_set frontier in
   case find (IS.member v) sccs of
     Nothing -> IS.empty
     Just scc -> scc

-- return the set of exits, i.e., edges (v0,v1) such that v0 is in the SCC and v1 is not.
exits g scc =
  S.toList $ S.unions $ map exits_of $ IS.toList scc
 where
  exits_of v = S.fromList $ map (\n -> (v,n)) $ IS.toList (IS.difference (intgraph_post g v) scc)
--}


{--
convergence_points :: CFG -> Int -> IS.IntSet -> Forest Int
concergence_points g v frontier =
  let paths = evalState (all_ayclic_paths v frontier) frontier

  -- partition according to endings, assuming shortest paths come first
  partition [] = []
  partition (p:ps) = 
    

  

  -- a spanning tree from vertex f upto (including) the frontier
  all_acyclic_paths :: Int -> IS.IntSet -> S.Set [Int]
  all_acyclic_paths v frontier =
    if v `IS.member` frontier || IS.null (post g v) || is_cyclic v frontier then
      S.singleton [v]
    else do
      paths <- forM (post g v) all_ayclic_paths
      return $ map (\p -> v:p) paths


-- is v cyclic (a path from v to v) wrt to the frontier?
is_cyclic :: Int -> IS.IntSet -> Bool
is_cyclic v frontier = evalState (anyM (is_reachable_from v) (post g v)) frontier

-- is v' reachable from v wrt to the frontier?
is_reachable_from :: Int -> Int -> State IS.IntSet Bool
is_reachable_from v v' = do
  visited <- gets $ IS.member v'
  if v == v' then
    True
  else if visited then
    False
  else do
    modify $ IS.insert v'
    anyM (is_reachable_from v) (post g v')
    



{-- ACode (Abstract Code)
 -
 - ACode consists of blocks (identified by an ID of type Int), sequential execution,
 - case splits (i.e., if statements) and while loops. Also, it can be a "Break ID"
 - which is a normal loop break annotated with the ID of the block that is executed next.
--}
data ACode =
    ACode_Skip
  | ACode_Block Int
  | ACode_Break Int
  | ACode_Seq ACode ACode
  | ACode_Cases (Int, [ACode])
  | ACode_While ACode [(Int,Int,ACode)]
 deriving Eq

show_acode indent ACode_Skip      = indent ++ "skip"
show_acode indent (ACode_Block v) = indent ++ show v
show_acode indent (ACode_Break v) = indent ++ "Break " ++ show v
show_acode indent (ACode_Seq a0 a1) = show_acode indent a0 ++ ";\n" ++ show_acode indent a1
show_acode indent (ACode_Cases (v,[ACode_Skip,case1])) =
  show_acode indent (ACode_Block v) ++ ";\n"
  ++ indent ++ "If ___ Then\n"
  ++ show_acode (indent ++ "  ") case1
  ++ "\n" ++ indent ++ "EndIf"
show_acode indent (ACode_Cases (v,[case0,ACode_Skip])) = show_acode indent (ACode_Cases (v,[ACode_Skip,case0]))
show_acode indent (ACode_Cases (v,[case0,case1])) =
  show_acode indent (ACode_Block v) ++ ";\n"
  ++ indent ++ "If ___ Then\n"
  ++ show_acode (indent ++ "  ") case0
  ++ "\n" ++ indent ++ "Else\n"
  ++ show_acode (indent ++ "  ") case1
  ++ "\n" ++ indent ++ "EndIf"
show_acode indent (ACode_Cases (v,cases)) =
   show_acode indent (ACode_Block v) ++ ";\n"
   ++ indent ++ "Cases {\n"
   ++ (intercalate "\n" $ map (\(i,acode) -> indent ++ show i ++ ":\n" ++ show_acode (indent ++ "  ") acode) (zip [0..] cases))
   ++ "\n" ++ indent ++ "}"
show_acode indent (ACode_While body resumes) =
  indent ++ "Loop\n"
  ++ show_acode (indent ++ "  ") body
  ++ "\n" ++ indent ++ "EndLoop"
  ++ "\n" ++ indent ++ "Resumes"
  ++ "\n" ++ (intercalate "\n" $ map (\(v0,v1,acode) -> indent ++ show v0 ++ "->" ++ show v1  ++ ":\n" ++ show_acode (indent ++ "  ") acode) resumes)
  ++ "\n" ++ indent ++ "EndResumes"

instance Show ACode where
  show = show_acode ""
  


cfg_to_acode g v frontier =
  let scc_v = get_scc_without g v frontier
      next  = IS.toList $ post g v in
    if v `IS.member` frontier then
      ACode_Break v
    else if next == [] then
      ACode_Block v
    else if (IS.size scc_v >= 2 || (IS.size scc_v == 1 && v `elem` next))
            && v `IS.member` scc_v then
      let exs     = exits g scc_v
          ns      = map (\v' -> cfg_to_acode g v' (IS.insert v frontier `IS.union` (IS.fromList $ map snd exs))) next
          rs      = map (\(v0,v1) -> (v0, v1, cfg_to_acode g v1 frontier)) exs
          body    = if length ns == 1 then ACode_Seq (ACode_Block v) (head ns) else ACode_Cases (v, ns) in
        ACode_While body rs
    else if length next > 1 then
      let ns = map (\v' -> cfg_to_acode g v' frontier) next in
        ACode_Cases (v,ns)
    else
      ACode_Seq (ACode_Block v) (cfg_to_acode g (head next) frontier)

    




-- is a0 included in a1?
acode_in a0 a1 = a0 == a1 || acode_strict_in a0 a1

acode_strict_in a0 (ACode_Block b)    = False
acode_strict_in a0 (ACode_Break b)    = False
acode_strict_in a0 (ACode_Seq a1 a1') = a0 `acode_in` a1 || a0 `acode_in` a1'
acode_strict_in a0 (ACode_Cases cs)   = any (acode_in a0) (snd cs)
acode_strict_in a0 (ACode_While b rs) = a0 `acode_in` b || any (\(_,_,a1) -> a0 `acode_in` a1) rs


-- is a0 at the end of a1?
at_end_of a0 a1 = a0 == a1 || acode_strict_at_end_of a0 a1

acode_strict_at_end_of a0 (ACode_Block b)    = False
acode_strict_at_end_of a0 (ACode_Break b)    = False
acode_strict_at_end_of a0 (ACode_Seq _ a1)   = a0 `at_end_of` a1
acode_strict_at_end_of a0 (ACode_Cases cs)   = all (\a1 -> a0 `at_end_of` a1) (snd cs)
acode_strict_at_end_of a0 (ACode_While b rs) = all (\(_,_,a1) -> a0 /= a1 && a0 `at_end_of` a1) rs


-- assuming a0 is at the end of a1, remove a0 from a1
remove_from_end a0 a1 =
  if a0 == a1 then
    ACode_Skip 
  else
    remove_from_end_strict a0 a1
 where
  remove_from_end_strict a0 (ACode_Seq a1 a1')   = if a0 == a1' then a1 else ACode_Seq a1 $ remove_from_end a0 a1'
  remove_from_end_strict a0 (ACode_Cases (v,cs)) = if all ((==) a0) cs then ACode_Block v else ACode_Cases (v,map (remove_from_end a0) cs)
  remove_from_end_strict a0 (ACode_While b rs)   = if all (\(_,_,a) -> a == a0) rs then error "Should not happen" else ACode_While b (map (\(v0,v1,a) -> (v0,v1,remove_from_end a0 a)) rs)
  remove_from_end_strict a0 a1 = error $ "Cannot remove " ++ show a0 ++ " from " ++ show a1

-- do a0 and a1 have shared endings
acode_shared_ending a0 a1 =
  if a0 `at_end_of` a1 then
    Just a0
  else
    case acode_tail a0 of
      Nothing -> Nothing
      Just a  -> acode_shared_ending a a1
 where
  acode_tail (ACode_Skip)       = Nothing
  acode_tail (ACode_Block b)    = Nothing
  acode_tail (ACode_Break b)    = Nothing
  acode_tail (ACode_Seq _ a1)   = Just a1
  acode_tail (ACode_Cases cs)   = Nothing 
  acode_tail (ACode_While b rs) = Nothing 

acode_shared_endings [a] = Just a
acode_shared_endings (a0:a1:as) =
  case acode_shared_ending a0 a1 of
    Nothing -> Nothing
    Just a  -> acode_shared_endings (a:as)


-- a simplification step
-- whenever we find code that have shared endings, remove the duplication and introduce a sequence
acode_simp (ACode_Cases (v,cs)) =
  let cs'     = map acode_simp cs in
    case acode_shared_endings cs' of
      Nothing -> ACode_Cases (v,cs')
      Just ending ->
        let beginnings = map (remove_from_end ending) cs'
            new_code  = ACode_Seq (ACode_Cases (v,beginnings)) ending in
          new_code
acode_simp (ACode_Seq a0 a1) = ACode_Seq (acode_simp a0) (acode_simp a1)
acode_simp (ACode_While b rs) =
  let rs'  = map (\(v0,v1,a) -> (v0,v1,acode_simp a)) rs
      as   = map (\(v0,v1,a) -> a) rs' in
    case acode_shared_endings as of
      Nothing -> ACode_While (acode_simp b) rs'
      Just ending ->
        if ending `elem` as then
          ACode_While (acode_simp b) rs'
        else 
          let beginnings = map (\((v0,v1,_),a) -> (v0,v1,remove_from_end ending a)) $ zip rs' as in
            ACode_Seq (ACode_While (acode_simp b) beginnings) ending
acode_simp a = a





-- the first block executed by the acode
acode_first (ACode_Block blockId)      = blockId
acode_first (ACode_Break blockId)      = blockId
acode_first (ACode_Seq a0 a1)          = acode_first a0
acode_first (ACode_Cases (blockId,cs)) = blockId
acode_first (ACode_While body rs)      = acode_first body

-- the last blocks executed by the acode
acode_lasts (ACode_Skip)          = []
acode_lasts (ACode_Block blockId) = [blockId]
acode_lasts (ACode_Break blockId) = []
acode_lasts (ACode_Seq a0 a1)     =
  case acode_lasts a1 of
    [] -> acode_lasts a0
    bs -> bs
acode_lasts (ACode_Cases (v,cs))  = concatMap last_cases cs
 where
  last_cases a = 
    case acode_lasts a of
      [] -> [v] 
      bs -> bs
acode_lasts (ACode_While b rs) = concatMap last_while rs
 where
  last_while (v0,v1,a) = 
    case acode_lasts a of
      [] -> error $ "Should not happen: resume without a last block."
      bs -> bs
 




     --}
