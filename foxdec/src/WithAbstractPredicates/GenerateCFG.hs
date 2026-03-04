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
import Data.CFI
import Data.X86.Opcode
import Data.X86.Instruction


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Word
import Data.Maybe
import Data.List
import Control.Monad.Extra
import Data.Foldable (foldrM,any)

import Debug.Trace

-- CONTROL FLOW

-- | Produce a CFG
generate_cfg ::
  WithAbstractPredicates bin pred finit v =>
     Lifting bin pred finit v
  -> Word64 -- ^ The entry point of the function
  -> IO CFG
generate_cfg l@(bin,_,_) entry = do
  let g    = init_cfg entry
  i       <- fetch_instruction bin entry
  let nxt  = next_rips l i
  let bag  = S.fromList $ (\(JustRips as) -> map (\a -> (entry,a)) as) nxt 
  g'      <- mk_graph l entry bag g
  g''     <- repeatUntilFixPointM (\g -> foldrM do_landing_pad g $ IS.toList $ get_landing_pads g) g'
  cfg_add_instrs l g''
 where
  -- Given the address of a landing pad, add the landing pad as root to the CFG, and explore from there
  -- This is iterated up to a fixpoint
  do_landing_pad lp cfg 
    | already_in_cfg cfg lp = return cfg
    | otherwise = do
      let f    = cfg_fresh cfg
      i       <- fetch_instruction bin $ fromIntegral lp
      let nxt  = next_rips l i
      let bag  = S.fromList $ get_nxt lp nxt 
      g'      <- mk_graph l entry bag $ cfg_add_block_for_landing_pad cfg lp
      return $ g' { cfg_landing_pads = IS.insert f $ cfg_landing_pads g' }

  get_nxt a0 (JustRips as) = map (\a -> (fromIntegral a0,a)) as
  get_nxt a0 Terminal = []

  -- The landing pads are retrieved from the gcc_except_tables
  get_landing_pads g = 
    let all_landing_pads = IS.unions $ map get_landing_pads_from_gcc_except_table $ get_relevant_tables bin g in
      IS.filter is_landing_pad all_landing_pads

  is_landing_pad a = address_has_instruction bin (fromIntegral a)

  already_in_cfg cfg a = any (\as -> a `elem` as) $ cfg_blocks cfg

  -- A gcc_except_table is relevant if it has a region that covers an instruction of the current CFG
  get_relevant_tables bin cfg = filter (\t -> anyIS (table_has_overlapping_region t) $ IM.keysSet $ cfg_addr_to_blockID cfg) all_tables
  all_tables = IM.elems $ cfi_gcc_except_tables $ binary_get_cfi bin
  table_has_overlapping_region t a = any (\(end,start,_,_) -> start <= a && a < end) $ get_callsite_regions_from_gcc_except_table t



repeatUntilFixPointM m a0 = do
  a1 <- m a0
  if a0 == a1 then
    return a0
  else
    repeatUntilFixPointM m a1


cfg_add_block_for_landing_pad cfg lp = 
  let f        = cfg_fresh cfg
      f'       = f + 1
      blocks'  = IM.insert f [lp] $ cfg_blocks cfg
      a_to_b'  = IM.insert (fromIntegral lp) f $ cfg_addr_to_blockID cfg in
    cfg { cfg_blocks = blocks', cfg_addr_to_blockID = a_to_b', cfg_fresh = f' }


mk_graph :: WithAbstractPredicates bin pred finit v => Lifting bin pred finit v -> Word64 -> S.Set (Word64, Word64) -> CFG -> IO CFG
mk_graph l@(bin,_,_) entry bag g =
  case S.minView bag of
    Nothing -> return g
    Just ((a0,a1),bag) ->
      if is_edge g (fromIntegral a0) (fromIntegral a1) then 
        mk_graph l entry bag g
      else do
        is_call_a0 <- is_call a0
        let g' = add_edge (fromIntegral a0) (fromIntegral a1) is_call_a0 g
        i <- fetch_instruction bin a1
        case next_rips l i of
          JustRips as -> let bag' = S.union (S.fromList $ map (\a2 -> (a1,a2)) as) bag in
                           mk_graph l entry bag' g' 
          _           -> mk_graph l entry bag g'
 where
  is_call a = do
    i <- fetch_instruction bin a 
    let op = inOperation $ fromJust i 
    return $ isCall op || isSyscall op









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
            let lps         = cfg_landing_pads g
            return $ CFG blocks' edges' a_to_b' fresh' IM.empty lps
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
            let lps         = cfg_landing_pads g
            return $ CFG blocks' edges' a_to_b' fresh' IM.empty lps
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
                  let lps     = cfg_landing_pads g
                  return $ CFG blocks' edges' a_to_b' fresh' IM.empty lps



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



cfg_add_instrs ::
  WithAbstractPredicates bin pred finit v =>
     Lifting bin pred finit v
  -> CFG
  -> IO CFG
cfg_add_instrs l@(bin,_,_) g = do
  instrs <- mapM block_to_instrs (IM.toList $ cfg_blocks g)
  return $ g { cfg_instrs = IM.fromList instrs }
 where
  block_to_instrs (a,as) = do
    instrs <- zip as <$> mapM (fetch_instruction bin . fromIntegral) as
    return (a, map (fromJust' instrs as) instrs)

  fromJust' instrs as (a,Nothing) = mk_HLT a -- error $ showHex_list as ++ show instrs
  fromJust' _ _ (a,Just i) = i

  mk_HLT a = Instruction (fromIntegral a) [] HLT [] [] 1

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



-- Break up basic blocks that contain a non-conditional jump in the middle.
cfg_split_jumps :: CFG -> CFG
cfg_split_jumps = repeatUtilFixpoint split_jumps
 where
  split_jumps cfg = foldr split_block cfg $ IM.assocs $ cfg_instrs cfg

  split_block (blockID,is) cfg =
    case break (isJump . inOperation) is of
      (_, [])     -> cfg
      (_, [_])    -> cfg
      (is0,i:(is1@(_:_))) -> 
        let f         = cfg_fresh cfg
            -- The current block gets instructions is0++[i], the new block gets instructions is1
            is1_addrs = map (fromIntegral . inAddress) is1
            blocks'   = IM.insert f is1_addrs $ IM.adjust (take (length is0+1)) blockID $ cfg_blocks cfg
            instrs'   = IM.insert f is1 $ IM.insert blockID (is0++[i]) $ cfg_instrs cfg 
            a_to_b'   = IM.union (IM.fromList $ zip is1_addrs (repeat f)) $ cfg_addr_to_blockID cfg
            -- The current block gets a single edge to the new block, the new block gets the out edges of the current block
            curr_outs = post cfg blockID
            edges'    = IM.insert f curr_outs $ IM.insert blockID (IS.singleton f) $ cfg_edges cfg
            fresh'    = f + 1 in
           CFG blocks' edges' a_to_b' fresh' instrs' (cfg_landing_pads cfg)




