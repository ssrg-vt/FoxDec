{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}

module WithAbstractSymbolicValues.GMem where

import Base


import WithAbstractSymbolicValues.Class

import Data.Size
import Data.X86.Register
import Data.X86.Instruction
import Data.GlobalMem
import Data.VerificationCondition

import Binary.Elf
import Binary.Generic

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import qualified Data.Set.NonEmpty as NES
import Data.List

import Control.Monad.State.Strict hiding (join)

import GHC.Generics (Generic)
import Control.DeepSeq
import qualified Data.Serialize as Cereal
import Debug.Trace





-- Try an aliassing memory access
try_alias_mem_access a si action = do
  gmem@(GlobalMem m ls) <- get
  case IM.lookup a m of 
    Just (v0,si0) -> if si == si0 then action v0 else return Nothing
    Nothing -> return Nothing

-- Try a fresh memory access
try_fresh_mem_access a si v action = do
  gmem@(GlobalMem m ls) <- get
  case (IM.lookupLT a m, IM.lookupGT a m) of
    (Nothing,Nothing)               -> do_insert a si
    (Nothing,Just (a1,_))           -> if a  + si  > a1 then fail else do_insert a si
    (Just (a0,(_,si0)),Nothing)     -> if a0 + si0 > a  then fail else do_insert a si
    (Just (a0,(_,si0)),Just (a1,_)) -> if a0+si0 > a || a+si > a1 then fail else do_insert a si
 where
  fail = return Nothing
  do_insert a si = do
    gmem@(GlobalMem m ls) <- get
    let m' = IM.insert a (v,si) m
    put $ GlobalMem m' ls
    action v

-- Try an enclosing access
try_enclosing_mem_access a si action = do
  gmem@(GlobalMem m ls) <- get
  case IM.lookupLE a m of
    Just (a0,(_,si0)) -> if a0 <= a && a+si<=a0+si0 then action a0 si0 else fail
    Nothing -> fail
 where
  fail = return Nothing




-- In case of partial overlap of (perhaps multiple) memory accesses
-- precise
get_touched_mem_accesses gmem_structure (GlobalMem m ls) a (Just si) True =
  case IM.splitLookup a m of
    (below,Nothing,      above) -> IM.union (get_touched_below a below) (get_touched_above (a+si) above)
    (below,Just (v0,si0),above) -> IM.insert a (v0,si0) $ get_touched_above (a+si) above
 where
  get_touched_below a below =
    case IM.lookupMax below of
      Nothing -> IM.empty
      Just (a0,(v0,si0)) -> if a0 + si0 > a then IM.singleton a0 (v0,si0) else IM.empty
  get_touched_above a above =
    case IM.minViewWithKey above of
      Nothing -> IM.empty
      Just ((a0,(v0,si0)),above') -> if a0 < a then IM.insert a0 (v0,si0) $ get_touched_above (max (a0+si0) a) above' else IM.empty
-- imprecise
get_touched_mem_accesses gmem_structure (GlobalMem m ls) a _ False =
  case (IM.lookupLE a gmem_structure, IM.lookupGT a gmem_structure) of
    (Just (l0,_), Nothing)     -> takeGE l0 m
    (Just (l0,_), Just (l1,_)) -> takeGE_LT l0 l1 m
    (Nothing, above)           -> IM.empty -- TODO error $ show_gmem (GlobalMem m ls) gmem_structure ++ "\n\n" ++  showHex (a) ++ "\n" ++ show above
 where
  takeGE a0 m =
    case IM.splitLookup a0 m of
      (below,Nothing,      above) -> above
      (below,Just (v0,si0),above) -> IM.insert a0 (v0,si0) above

  takeGE_LT a0 a1 m =
    let above_a0 = takeGE a0 m in
      case IM.split a1 above_a0 of
        (below_a1,above_a1) ->
          case IM.maxViewWithKey below_a1 of
            Nothing -> IM.empty
            Just ((a,(v,si)),below) -> if a + si <= a1 then IM.insert a (v,si) below else below


overlapping_access ctxt a si = do
  let gmem_structure = sget_gmem_structure ctxt
  gmem@(GlobalMem m ls) <- get
  let touched = get_touched_mem_accesses gmem_structure gmem a (Just si) True
  if IM.null touched then
    return ()
  else do
    let merge = case (IM.lookupMin touched,IM.lookupMax touched) of
                  (Just (l,(_,si_l)),Just (h, (_,si_h))) -> IM.singleton (min a l) (top ctxt "",max (h+si_h) (a+si) - min a l)
    let m' = IM.union merge (IM.difference m touched) -- TODO can be more efficient
    put $ GlobalMem m' ls


add_dirty_access :: Int -> State (GlobalMem v) ()
add_dirty_access a = do
  gmem@(GlobalMem m ls) <- get
  put $ GlobalMem m $ IS.insert a ls


-- READING
read_global_mem_access :: WithAbstractSymbolicValues ctxt bin v p => ctxt -> p -> Int -> Maybe ByteSize -> Bool -> State (GlobalMem v) v
-- precise
read_global_mem_access ctxt p a si'@(Just (ByteSize si)) True =
  try_alias_mem_access a si read
   `orTryM`  try_fresh_mem_access a si (top ctxt "") read -- (smk_init_mem_value ctxt "" p si') read
   `orTryM`  try_enclosing_mem_access a si read_top
   `orElseM` (overlapping_access ctxt a si >> (return $ top ctxt ""))
 where
  read v0 = return $ Just v0
  read_top a0 si0 = return $ Just $ top ctxt ""
-- imprecise
read_global_mem_access ctxt p a _ _ = do
  gmem@(GlobalMem m ls) <- get
  let gmem_structure = sget_gmem_structure ctxt
  case IM.lookupLE a gmem_structure of
    Just (l0,True) -> return $ top ctxt ""
    Just (l0,False) -> add_dirty_access a >> (return $ top ctxt "")
    x -> return $ top ctxt "" -- TODO error $ "TODO:\n" ++ show_gmem gmem gmem_structure ++ "\n\n" ++ show (showHex a,x) ++ "\n\n" ++ show (get_touched_mem_accesses gmem_structure gmem a Nothing False)






-- WRITING 
-- precise
write_global_mem_access :: WithAbstractSymbolicValues ctxt bin v p => ctxt -> Int -> Maybe ByteSize -> Bool -> Bool -> v -> State (GlobalMem v) ()
write_global_mem_access ctxt a (Just (ByteSize si)) True do_fresh v = do
  try_alias_mem_access a si write
   `orTryM`  (if do_fresh then try_fresh_mem_access a si v ret else return Nothing)
   `orTryM`  try_enclosing_mem_access a si write_top
   `orElseM` overlapping_access ctxt a si
 where
  ret _ = return $ Just ()
  write old_v = do
    GlobalMem m ls <- get
    let m' = IM.insert a (v,si) m
    put $ GlobalMem m' ls
    return $ Just ()
  write_top a0 si0 = do
    gmem@(GlobalMem m ls) <- get
    let m' = IM.insert a0 (top ctxt "",si0) m
    put $ GlobalMem m' ls
    return $ Just ()
-- imprecise
write_global_mem_access ctxt a _ _ _ v = do
  gmem@(GlobalMem m ls) <- get
  let gmem_structure = sget_gmem_structure ctxt
  let touched = get_touched_mem_accesses gmem_structure gmem a Nothing False
  let muddled = IM.map (\(v,si) -> (top ctxt "", si)) touched -- TODO expensive
  let m' = IM.union muddled m
  put $ GlobalMem m' ls
  case IM.lookupLE a gmem_structure of
    Just (l0,True)  -> return ()
    Just (l0,False) -> add_dirty_access a
    x ->  error $ "TODO:\n" ++ show_gmem gmem gmem_structure ++ "\n\n" ++ show (showHex a,x) ++ "\n\n" ++ show (get_touched_mem_accesses gmem_structure gmem a Nothing False)



add_global_mem_access ctxt do_fresh a si v = execState (write_global_mem_access ctxt a (Just $ ByteSize si) True do_fresh v)



-- JOINING
sjoin_gmem :: WithAbstractSymbolicValues ctxt bin v p => ctxt -> GlobalMem v -> GlobalMem v -> GlobalMem v
sjoin_gmem ctxt gmem0@(GlobalMem m0 ls0) gmem1@(GlobalMem m1 ls1) = 
  let step0 = IM.foldrWithKey (join_entry gmem1) (GlobalMem IM.empty $ IS.union ls0 ls1) m0 in
    IM.foldrWithKey (join_entry gmem0) step0 m1
 where
  join_entry m1 a0 (v0,si0) j =
    let touched = get_touched_mem_accesses (sget_gmem_structure ctxt) m1 a0 (Just si0) True
        (a_j,(v_j,si_j)) = IM.foldrWithKey' join_mem_accesses (a0,(v0,si0)) touched in
      add_global_mem_access ctxt True a_j si_j v_j j


  join_mem_accesses a0 (v0,si0) (a1,(v1,si1))
    | a0==a1 && si0 == si1 = (a0, (sjoin_values ctxt "" [v0,v1], si0))
    | otherwise            = (min a0 a1, (top ctxt "", max (a0+si0) (a1+si1) - min a0 a1))







