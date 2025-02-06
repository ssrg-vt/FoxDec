{-# LANGUAGE DeriveGeneric, StrictData #-}

module WithAbstractPredicates.GenerateCFG where

import Base
import Config

import WithAbstractPredicates.Class
import WithAbstractPredicates.ControlFlow


import Binary.Generic
import Data.JumpTarget
import Data.Symbol
import Data.L0
import Data.CFG
import Data.X86.Opcode
import Data.X86.Instruction


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Word
import Data.Maybe
import Control.Monad.Extra



-- CONTROL FLOW

-- | Produce a CFG
generate_cfg ::
  WithAbstractPredicates bin pred finit v =>
     Lifting bin pred finit v
  -> Word64 -- ^ The entry point of the function
  -> CFG
generate_cfg l@(bin,_,_) entry =
  let g           = init_cfg entry
      i           = fetch_instruction bin entry
      nxt         = next_rips l i
      bag         = S.fromList $ (\(JustRips as) -> map (\a -> (entry,a)) as) nxt 
      g'          = mk_graph l entry bag g 
      g''         = cfg_add_instrs l g' in
    g''


mk_graph :: WithAbstractPredicates bin pred finit v => Lifting bin pred finit v -> Word64 -> S.Set (Word64, Word64) -> CFG -> CFG
mk_graph l@(bin,_,_) entry bag g =
  case S.minView bag of
    Nothing -> g
    Just ((a0,a1),bag) ->
      if is_edge g (fromIntegral a0) (fromIntegral a1) then 
        mk_graph l entry bag g
      else
        let g' = add_edge (fromIntegral a0) (fromIntegral a1) (is_call a0) g in
          case next_rips l (fetch_instruction bin a1) of
            JustRips as -> let bag' = S.union (S.fromList $ map (\a2 -> (a1,a2)) as) bag in
                             mk_graph l entry bag' g' 
            _           -> mk_graph l entry bag g'
 where
  is_call a =
    let i = fetch_instruction bin a in
      isCall $ inOperation $ fromJust i






-- CONTROL FLOW GRAPH

-- the algorithm below has been formally proven correct in Isabelle/HOL
split_graph' a g = 
  case IM.lookup a (cfg_addr_to_blockID g) of
    Nothing -> Just g
    Just blockID ->
      case IM.lookup blockID (cfg_blocks g) of
        Nothing -> Just g
        Just block ->
          if last block /= a then do
            let (begin,end) = break ((==) a) block
            let f           = cfg_fresh g
            let blocks'     = IM.insert blockID (begin++ [a]) $ IM.insert f (tail end) (cfg_blocks g)
            let edges'      = IM.insert blockID (IS.singleton f) $ IM.mapKeys (\k -> if k == blockID then f else k) (cfg_edges g)
            let a_to_b'     = IM.mapWithKey (\addr blockID -> if addr `elem` tail end then f else blockID) (cfg_addr_to_blockID g)
            let fresh'      = f + 1
            return $ CFG blocks' edges' a_to_b' fresh' IM.empty
          else
            return g

split_graph a g = do
  case IM.lookup a (cfg_addr_to_blockID g) of
    Nothing -> Just g
    Just blockID ->
      case IM.lookup blockID (cfg_blocks g) of
        Nothing -> Just g
        Just block ->
          if head block /= a then do
            let (begin,end) = break ((==) a) block
            let f           = cfg_fresh g
            let blocks'     = IM.insert blockID begin $ IM.insert f end  (cfg_blocks g)
            let edges'      = IM.insert blockID (IS.singleton f) $ IM.mapKeys (\k -> if k == blockID then f else k) (cfg_edges g)
            let a_to_b'     = IM.mapWithKey (\addr blockID -> if addr `elem` end then f else blockID) (cfg_addr_to_blockID g)
            let fresh'      = f + 1
            return $ CFG blocks' edges' a_to_b' fresh' IM.empty
          else
            return g

add_edge_to_graph a0 a1 g = do
  case IM.lookup a0 (cfg_addr_to_blockID g) of
    Nothing -> Just g
    Just blockID ->
      case IM.lookup blockID (cfg_blocks g) of
        Nothing -> Just g
        Just block ->
          case IM.lookup a1 (cfg_addr_to_blockID g) of
            Just blockID' -> do
              case IM.lookup blockID' (cfg_blocks g) of
                Nothing -> Just g
                Just block' -> do
                  let edges' = IM.alter (add_to_intset blockID') blockID (cfg_edges g)
                  return $ g { cfg_edges = edges' }
            Nothing -> do
              case IM.lookup blockID (cfg_edges g) of
                Nothing -> do
                  let blocks' = IM.alter (append_to_list a1) blockID (cfg_blocks g)
                  let a_to_b' = IM.insert a1 blockID (cfg_addr_to_blockID g)
                  return $ g { cfg_blocks = blocks', cfg_addr_to_blockID = a_to_b' }
                _ -> do
                  let f       = cfg_fresh g
                  let blocks' = IM.insert f [a1] (cfg_blocks g)
                  let edges'  = IM.alter (add_to_intset f) blockID (cfg_edges g)
                  let a_to_b' = IM.insert a1 f (cfg_addr_to_blockID g)
                  let fresh'  = f + 1
                  return $ CFG blocks' edges' a_to_b' fresh' IM.empty



add_to_intset a Nothing  = Just $ IS.singleton a
add_to_intset a (Just x) = Just $ IS.insert a x 

append_to_list a Nothing = Just [a]
append_to_list a (Just x) = Just (x++[a]) 

add_edge a0 a1 is_call_a0 g =
  case add_to g of 
    Nothing -> error "Could not add edge"
    Just g  -> g
 where
  add_to = split_graph' a0 >=> split_graph a1 >=> add_edge_to_graph a0 a1 >=> split_calls a0 is_call_a0
  split_calls a True  = split_graph' a
  split_calls a False = return

fromJust' instrs as Nothing = error $ showHex_list as ++ show instrs
fromJust' _ _ (Just a) = a

cfg_add_instrs ::
  WithAbstractPredicates bin pred finit v =>
     Lifting bin pred finit v
  -> CFG
  -> CFG
cfg_add_instrs l@(bin,_,_) g =
  let instrs = map block_to_instrs (IM.toList $ cfg_blocks g) in
    g { cfg_instrs = IM.fromList instrs }
 where
  block_to_instrs (a,as) =
    let instrs = map (fetch_instruction bin . fromIntegral) as in
      (a, map (fromJust' instrs as) instrs)



is_consecutive a b []      = False
is_consecutive a b [_]     = False
is_consecutive a b (c:d:x) = (a,b) == (c,d) || is_consecutive a b (d:x)

is_edge g a0 a1 =
 case lookup of
  Nothing -> False
  Just b -> b
 where
  lookup = do
    blockId  <- IM.lookup a0 (cfg_addr_to_blockID g)
    blockId' <- IM.lookup a1 (cfg_addr_to_blockID g)
    b  <- IM.lookup blockId  (cfg_blocks g)
    b' <- IM.lookup blockId' (cfg_blocks g)
    if last b == a0 then do
      edges <- IM.lookup blockId (cfg_edges g)
      return $ head b' == a1 && blockId' `IS.member` edges
    else
      return $ blockId == blockId' && is_consecutive a0 a1 b




