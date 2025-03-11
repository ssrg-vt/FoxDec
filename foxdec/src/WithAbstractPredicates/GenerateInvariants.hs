{-# LANGUAGE DeriveGeneric, StrictData, FlexibleContexts, ScopedTypeVariables #-}

module WithAbstractPredicates.GenerateInvariants where

import Base
import Config

import WithAbstractPredicates.ControlFlow

import Data.X86.Opcode
import Data.CFG
import Data.GlobalMem
import Data.VerificationCondition
import Data.L0
import Data.Indirection
import Data.X86.Instruction
import Data.X86.Opcode
import Data.X86.Register

import WithAbstractPredicates.Class

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Maybe
import Data.List
import Control.Monad.State.Strict

-- | Start propagation at the given entry address with the given initial predicate.
-- Returns a set of invariants, i.e., a mapping of instruction addresses to predicates.
--
-- Assumes blockID 0 is start of function
generate_invariants :: WithAbstractPredicates bin pred finit v => LiftingEntry bin pred finit v -> CFG -> finit -> (IM.IntMap pred,GMemStructure,VCS v)
generate_invariants l@(_,_,l0,_) cfg finit =
  let p = finit_to_init_pred l finit
      gmem_structure = l0_gmem_structure l0
      (invs,_,gem_structure',vcs) = execState (propagate l cfg) (IM.singleton 0 p, out_edges cfg 0, gmem_structure, S.empty)
      blocks = IM.assocs $ cfg_instrs cfg
      end_blocks = filter (\(blockID,_) -> is_end_node cfg blockID) blocks
      vcs' = map (\(blockID,instrs) -> snd $ get_postcondition_for_block l blockID instrs invs) end_blocks in -- TODO use updated GMemStructure
    (invs,gem_structure',S.unions $ vcs:vcs')





invs_to_PA :: WithAbstractPredicates bin pred finit v => LiftingEntry bin pred finit v -> CFG -> IM.IntMap pred -> IM.IntMap (PointerAnalysisResult v)
invs_to_PA l@(bin,_,l0,_) cfg invs =
  let blocks = IM.assocs $ cfg_instrs cfg
      pa     = IM.unions $ map get_pa blocks in
    pa
 where
  get_pa (blockID,instrs) = 
    let inv     = invs IM.! blockID
        (_,vcs) = execState (symbolically_execute l True instrs Nothing) (inv,S.empty) in
      gather_pa_results vcs



invs_to_post :: WithAbstractPredicates bin pred finit v => LiftingEntry bin pred finit v -> CFG -> IM.IntMap pred -> Postcondition pred
invs_to_post l@(bin,_,l0,_) cfg invs =
  let blocks = IM.assocs $ cfg_instrs cfg
      end_blocks = filter (\(blockID,_) -> is_end_node cfg blockID) blocks
      posts = map (get_post l invs) end_blocks in
    join_postconditions posts
 where
  join_postconditions posts
    | all ((==) Terminates . snd) posts = Terminates
    | any ((==) TimeOut . snd) posts = TimeOut
    | any (is_unresolved_indirection . snd) posts = join_unresolved_indirections $ map snd posts
    | otherwise =
      let errors = filter is_error posts in
        case errors of
          [] -> ReturnsWith $ foldr1 (join_preds l) $ mapMaybe get_return_pred $ map snd posts
          _  -> VerificationError $ map (\(blockID,ReturnsWith pred) -> (blockID,pred)) errors

  is_error (blockID, ReturnsWith pred) = not $ verify_postcondition l pred
  is_error _ = False

  get_return_pred (ReturnsWith pred) = Just pred
  get_return_pred _ = Nothing


  join_unresolved_indirections posts = HasUnresolvedIndirections $ concatMap get_unresolved_indirections posts
  get_unresolved_indirections (HasUnresolvedIndirections blockIDs) = blockIDs
  get_unresolved_indirections _ = []
  is_unresolved_indirection (HasUnresolvedIndirections _) = True
  is_unresolved_indirection _ = False


  has_unresolved_instruction l0 (blockID,instrs) = any (is_unresolved_instruction l0) instrs
  is_unresolved_instruction l0 i
    | isCall (inOperation i) || isJump (inOperation i) = has_unresolved_indirections $ l0_lookup_indirection (inAddress i) l0
    | otherwise = False

  get_post :: WithAbstractPredicates bin pred finit v => LiftingEntry bin pred finit v -> IM.IntMap pred -> (Int,[Instruction]) -> (Int,Postcondition pred)
  get_post l@(_,_,l0,_) invs (blockID,instrs) =
    if has_unresolved_indirections $ l0_lookup_indirection (inAddress $ last instrs) l0 then
      (blockID,HasUnresolvedIndirections [blockID])
    else case next_rips (withoutEntry l) (Just $ last instrs) of
      UnresolvedTarget -> error $ show (blockID,instrs)  -- HasUnresolvedIndirections
      Terminal         -> (blockID,Terminates)
      JustRips []      -> (blockID,ReturnsWith $ fst $ get_postcondition_for_block l blockID instrs invs)


  has_unresolved_indirections (Just inds) = Indirection_Unresolved `S.member` inds
  has_unresolved_indirections Nothing = False

withEntry entry (a,b,c) = (a,b,c,entry) 
withoutEntry (a,b,c,entry) = (a,b,c)

get_postcondition_for_block :: WithAbstractPredicates bin pred finit v => LiftingEntry bin pred finit v -> Int -> [Instruction] -> IM.IntMap pred -> (pred,VCS v)
get_postcondition_for_block l blockID instrs invs =
  let pre = fromJust $ IM.lookup blockID invs in
   execState (symbolically_execute l False instrs Nothing) (pre,S.empty)



invs_to_joined_post :: WithAbstractPredicates bin pred finit v => LiftingEntry bin pred finit v -> CFG -> IM.IntMap pred -> pred
invs_to_joined_post l@(bin,_,l0,_) cfg invs =
  let blocks = IM.assocs $ cfg_instrs cfg
      end_blocks = filter (\(blockID,_) -> is_end_node cfg blockID) blocks
      posts = map (get_post l invs) end_blocks in
    foldr1 (join_preds l) posts
 where
  get_post l@(_,_,l0,_) invs (blockID,instrs) = fst $ get_postcondition_for_block l blockID instrs invs



-- PROPAGATION
--
--
-- propagation
-- The state consists of 
-- 		1.) the current mapping of addresses to predicates 
-- 		2.) the current bag (a set of edges to be explored)
--


-- TODO CALLS
add_to_gmem_structure :: Instruction -> GMemStructure -> GMemStructure
add_to_gmem_structure i@(Instruction _ _ _ maybe_dst srcs _) gmem_structure = foldr add gmem_structure (get_dst maybe_dst ++ srcs)
 where
  get_dst Nothing = []
  get_dst (Just dst) = [dst]

  add (Op_Mem si aSi (Reg64 RIP) RegNone 0 displ Nothing) gmem_structure =
    let rip   = inAddress i + (fromIntegral $ inSize i)
        a     = rip + fromIntegral displ
        dirty = inOperation i == LEA  in
      IM.insertWith (||) (fromIntegral a) dirty gmem_structure
  add (Op_Mem si aSi (Reg64 RIP) _ _ _ _) gmem_structure = error "todo"
  add _ gmem_structure = gmem_structure


add_block_to_gmem_structure = foldr add_to_gmem_structure


propagate :: WithAbstractPredicates bin pred finit v => LiftingEntry bin pred finit v -> CFG -> State (IM.IntMap pred, S.Set (Int,Int), GMemStructure, VCS v) ()
propagate l@(bin,config,l0,entry) g = do
  pick <- pick_edge_from_bag
  case pick of
    Nothing -> return ()
    Just ((v0,v1),bag') -> do
      -- take an edge (v0,v1) out of the bag
      (m,_,gmem_structure,vcs) <- get
      -- do predicate transformation on the currently available precondition of v0
      let p = m IM.! v0

      let instrs = fetch_block g v0
      let gmem_structure' = add_block_to_gmem_structure gmem_structure instrs
      put (m,bag',gmem_structure',vcs)
      let l' = (bin,config,l0_set_gmem_structure gmem_structure' l0,entry) 
      -- this produces q: the precondition for v1
      let (q,vcs') = execState (symbolically_execute l' False instrs (Just $ fetch_block g v1)) (p,S.empty)
      modify $ add_vcs vcs'
      -- store q
      add_predicate l q v1
      -- continue
      propagate l g
 where
  pick_edge_from_bag = do
    (m,bag,gmem_structure,vcs) <- get
    case find (\(v0,v1) -> IM.member v1 m) bag of
      Just edge -> return $ Just (edge, S.delete edge bag)
      Nothing   -> return $ S.minView bag
  add_vcs vcs' (m,bag,gmem_structure,vcs) = (m,bag,gmem_structure,S.union vcs vcs')

  add_predicate l q v1 = do
   (m,bag,gmem_structure,vcs) <- get
   case IM.lookup v1 m of
     Nothing -> do
       -- first time visit, store q and explore all outgoing edges
       put (IM.insert v1 q m, S.union bag $ out_edges g v1,gmem_structure,vcs)
     Just p -> do
       if is_weaker_than l p q then
         -- previously visited, no need for further exploration
         return () -- put (IM.insert v1 j m, bag, vcs)
       else do
         let j = join_preds l p q
         -- previously visited, need to weaken invariant by joining
         put (IM.insert v1 j m,S.union bag $ out_edges g v1,gmem_structure,vcs)

out_edges g v = S.fromList $ zip (repeat v) $ IS.toList $ post g v
 
