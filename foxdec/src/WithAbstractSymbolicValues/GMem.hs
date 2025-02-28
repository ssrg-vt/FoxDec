{-# LANGUAGE DeriveGeneric, FlexibleContexts  #-}

module WithAbstractSymbolicValues.GMem where

import Base


import WithAbstractSymbolicValues.Class

import Data.Size
import Data.X86.Register
import Data.X86.Instruction
import Data.GlobalMem
import Data.VerificationCondition
import Data.SymbolicExpression (FlagStatus(..)) -- TODO


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Set.NonEmpty as NES
import Data.List

import Control.Monad.State.Strict hiding (join)

import GHC.Generics (Generic)
import Control.DeepSeq
import qualified Data.Serialize as Cereal



get_touched_mem_accesses m a si isPrecise = get_below a ++ get_exact a ++ get_above a
 where
  get_exact a = 
    case IM.lookup a m of
      Nothing -> []
      Just  r -> [(a,r)]

  get_below a =
    case IM.lookupLT a m of
      -- Nothing below 
      Nothing -> []
      -- Found below region
      Just r@(a',ma') ->
        if is_below_overlapping a' ma' a then
          -- error $ show (p,showHex a,si,isPrecise,"above",showHex a', si',v',a -a')
          [r]
        else
          []

  get_above a =
    case IM.lookupGT a m of
      -- Nothing above 
      Nothing -> []
      -- Found above region
      Just r@(a',ma') ->
        if is_above_overlapping a (if isPrecise then si else Nothing) a' then do
          (a',ma') : go_contiguous_upwards_all a' ma'
        else
          []

  go_contiguous_upwards_all a' ma' =
    case IM.lookupGT a' m of
      Nothing -> []
      Just (a'',ma'') ->
        if is_contiguous_above a' ma' a'' then
          (a'',ma'') : go_contiguous_upwards_all a'' ma''
        else
          [(a'',ma'')]

  is_below_overlapping a (Stores _ si) a' = a + si > a'
  is_below_overlapping a (SpansTo a'') a' = a <= a' && a' < a'' + 16

  is_above_overlapping a Nothing a' = a' - a < 16
  is_above_overlapping a (Just (ByteSize si)) a' = a + si > a'

  is_contiguous_above a (Stores _ si) a' = a + si >= a'
  is_contiguous_above a (SpansTo a'') a' = a' <= a'' + 16


--TODO move to Sstate
insert_global_mem_access :: WithAbstractSymbolicValues ctxt v p => ctxt -> Int -> Maybe ByteSize -> Bool -> v -> State (GlobalMem p v) (Int,MemAccess p v)
insert_global_mem_access ctxt a si isPrecise fresh_value  = do
  GlobalMem m <- get
  case get_touched_mem_accesses m a si isPrecise of
    [] -> do
      let fresh_entry = mk_fresh_entry a si isPrecise fresh_value
      put $ GlobalMem $ IM.insert a fresh_entry m
      return (a,fresh_entry)
    touched@[(a',Stores _ si')] -> 
      if a' == a && si == Just (ByteSize si') && isPrecise then
        return $ head touched
      else do
        merge_touched touched
    touched -> do
      merge_touched touched
 where
  mk_fresh_entry _ si@(Just (ByteSize s)) True  v = Stores v s
  mk_fresh_entry a si@(Just (ByteSize s)) False v = SpansTo (a + s) --p (smk_init_mem_value ctxt "" p si)
  mk_fresh_entry a si@Nothing             _     v = SpansTo a -- p (smk_init_mem_value ctxt "" p si)


  merge_touched touched = do
    GlobalMem m <- get
    let new_mem_access = mk_fresh_entry a si isPrecise $ top ctxt ""
    let (new_a,new_ma) = foldr1 (join_mem_accesses ctxt) $ (a,new_mem_access) : touched
    let m'  = IM.filterWithKey (\a' _ -> a' `notElem` map fst touched) m
    let m'' = IM.insert new_a new_ma m'
    put $ GlobalMem m''
    return (new_a,new_ma)

join_mem_accesses ctxt (a0,SpansTo a0')   (a1,SpansTo a1')   = (min a0 a1, SpansTo $ max a0' a1')
join_mem_accesses ctxt (a0,Stores _ si0)  (a1,SpansTo a1')   = (min a0 a1, SpansTo $ max (a0+si0) a1')
join_mem_accesses ctxt (a0,SpansTo a0')   (a1,Stores _ si1)  = (min a0 a1, SpansTo $ max a0' (a1+si1))
join_mem_accesses ctxt (a0,Stores v0 si0) (a1,Stores v1 si1)
  | a0==a1 && si0 == si1 = (a0, Stores (sjoin_values ctxt "" [v0,v1]) si0)
  | otherwise            = (min a0 a1, Stores (top ctxt "") (max (a0+si0) (a1+si1) - min a0 a1))




add_global_mem_access ctxt a (Stores v si) m = execState (write_global_mem_access ctxt a (Just (ByteSize si))     True v) m
add_global_mem_access ctxt a (SpansTo a')  m = execState (write_global_mem_access ctxt a (Just (ByteSize (a'-a))) False $ top ctxt "") m


write_global_mem_access ctxt a si isPrecise v = do
  (a',ma') <- insert_global_mem_access ctxt a si isPrecise v
  case (isPrecise,a==a',si,ma') of
    (True,True,Just (ByteSize si),Stores _ si') -> if si == si' then insert_mem_access a $ Stores v si else insert_mem_access a' $ Stores (top ctxt "") si'
    (_,_,_,Stores _ si') -> insert_mem_access a' $ Stores (top ctxt "") si'
    (_,_,_,SpansTo _) -> return ()
 where
  insert_mem_access a ma = do
    GlobalMem m <- get
    put $ GlobalMem $ IM.insert a ma m

read_global_mem_access ctxt p a si isPrecise = do
  (a',ma') <- insert_global_mem_access ctxt a si isPrecise (smk_init_mem_value ctxt "" p si)
  case (isPrecise,a==a',si,ma') of
    (True,True,Just (ByteSize si),Stores v si') -> if si == si' then return v else return (top ctxt "")
    _ -> return (top ctxt "")




gmem_to_structure (GlobalMem gmem) = merge_consecutive $ sortBy fstLE $ IM.assocs gmem
 where
  fstLE (a0,_) (a1,_) = compare a0 a1

  merge_consecutive [] = []
  merge_consecutive [(a,_)] = [a]
  merge_consecutive ((a0,Stores v0 si0):(a1,Stores v1 si1):es)
    | a0+si0 == a1 = merge_consecutive $ (a0,Stores v0 $ si0+si1):es
    | otherwise    = a0 : merge_consecutive ((a1,Stores v1 si1):es)
  merge_consecutive ((a,_):es) = a : merge_consecutive es
