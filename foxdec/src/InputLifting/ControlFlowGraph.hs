{-# LANGUAGE PartialTypeSignatures, DeriveGeneric, StrictData, ScopedTypeVariables #-}

module InputLifting.ControlFlowGraph where

import Base
import Config
import Conventions

import Data.JumpTarget
import Data.Symbol
import Data.Size
import Data.CFG
import Data.CFI
import Data.Indirection
import Data.VerificationCondition
import Data.X86.Opcode
import Data.X86.Instruction
import Data.SymbolicExpression
import Data.X86.Register

import Algorithm.Graph
import qualified Data.Tree as T
import qualified Data.Tree.View as TV

import Binary.Generic
import Binary.FunctionNames

import InputLifting.Types
import InputLifting.NextRips

import Data.Word
import Data.List
import Data.List.Extra (firstJust)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS


import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Extra




import GHC.Generics
import Debug.Trace

instance IntGraph ControlFlowGraph where
  intgraph_pre     (ControlFlowGraph _ es srcs _ _) v = xgraph_parents es v
  intgraph_post    (ControlFlowGraph _ es srcs _ _) v = xgraph_children es v
  intgraph_V       (ControlFlowGraph _ es srcs _ _)   = xgraph_all_vertices es
  intgraph_sources (ControlFlowGraph _ _  srcs _ _)   = srcs
 

instance Show ControlFlowGraph where
  show cfg@(ControlFlowGraph blocks edges srcs leaks comp) = show_trees
   where
    show_trees = intercalate "\n" $ map src_to_tree $ IS.toList srcs
    src_to_tree src = 
      let tree = evalState (dfs_spanning_tree cfg src) IS.empty in
        TV.showTree $ fmap show_nodedata tree

    show_nodedata (Unfinished v)      = showHex v ++ " ..."
    show_nodedata (Vertices blockIDs) = 
      let bs = map (\blockID -> blocks IM.! blockID) blockIDs in
        "[" ++ intercalate "," (map (showHex . inAddress . head) bs) ++ ".." ++ showHex (inAddress $ last $ last bs) ++ "]"


-- all cfg addresses
cfg_all_instruction_addresses cfg = IS.unions $ map (IS.fromList . map (fromIntegral . inAddress)) $ IM.elems $ cfg_basic_blocks cfg

-- size of a CFG
cfg_size = IM.size . cfg_basic_blocks

-- remove a block given a blockID: connect its parents to its children
cfg_remove_block blockID cfg =
  let parents  = IS.delete blockID $ intgraph_pre cfg blockID
      children = IS.delete blockID $ intgraph_post cfg blockID
      prod     = filter (\(x,y) -> x /= y) $ [(x,y) | x <- IS.toList parents, y <- IS.toList children]
      cfg0     = delete_node blockID cfg in
    foldl' add_new_edge cfg0 prod
 where
  add_new_edge (ControlFlowGraph blocks edges srcs leaks comp) (parent,child) = ControlFlowGraph blocks (xgraph_add_edge edges parent child) srcs leaks comp


  delete_node blockID (ControlFlowGraph blocks edges srcs leaks comp) = ControlFlowGraph (IM.delete blockID blocks) (xgraph_delete_node blockID edges) (IS.delete blockID srcs) leaks comp



type XLifted bin = ReaderT (bin,Config,LiftedRepresentationUnstructured,IM.IntMap ControlFlowGraph) IO

withLR :: XLifting bin a -> XLifted bin a
withLR m = do
  (bin,config,lr,cfgs) <- ask
  lift $ runReaderT (evalStateT m lr) (bin,config)



isRegionStart bin i = (fromIntegral (inAddress i) + inSize i) `IS.member` all_region_starts
 where
   all_region_starts = IS.unions $ map get_callsite_region_starts_from_gcc_except_table all_tables
   all_tables = IM.elems $ cfi_gcc_except_tables $ binary_get_cfi bin

-- Make the basic blocks for a snippet.
-- This requires a DFS where instruction adrdesses with mutiple parents or mulitple children indicate the start or end of a basic block.
-- The starting points (i.e., sources) are the entry and the landing pads.
-- We also keep track of edges where normal control flow goes to outside the given snippet (i.e., the leaks).
lr_make_function_blocks :: BinaryClass bin => FunctionEntry -> XLifted bin (S.Set FunctionEntry,Blocks,Sources)
lr_make_function_blocks entry = do
  (bin,_,lr,_) <- ask
  (lps,additional_sources) <- get_additional_sources
  let sources = IS.insert (toInt entry) additional_sources
  (blocks,entries) <- execStateT (dfs_all lps $ map (\a -> (a,a)) $ IS.toList sources) $ (IM.empty,S.empty)
  return (entries,blocks,sources) 
 where
  -- Do a depth-first-search to traverse the snippet of the entry.
  -- The extra state stores the currently known blocks, and the snippets to which they leak.
  dfs_all :: BinaryClass bin => IS.IntSet -> [(Int, Int)] -> StateT (Blocks,S.Set FunctionEntry) (XLifted bin) ()
  dfs_all lps = mapM_ (dfs lps)
  dfs lps (blockID,a) = do
    (bin,_,_,_) <- ask
    (blocks,leaking_entries) <- get
    x <- lift $ add_address blockID a blocks
    case x of
      Nothing -> return ()
      Just (blocks',blockID') -> do
        (new_entries,children0) <- lift $ withLR $ get_next_collapsed_calls (S.singleton entry) $ InstructionAddress a
        let children = S.filter (\child -> not $ toInt child `IS.member` lps) children0
        Just i <- lift $ withLR $ fetch $ InstructionAddress a
        -- Start a new basic block if there is outgoing branching
        let start_new_block = S.size children > 1 || isCall (inOperation i) || isJump (inOperation i) || isCondJump (inOperation i) || isRegionStart bin i
        let nxt = map (\child -> (if start_new_block then child else blockID',child)) $ map toInt $ S.toList children
        put (blocks',S.union new_entries leaking_entries)
        dfs_all lps nxt


  get_additional_sources = do 
    (bin,_,lr,_) <- ask
    case find (\t -> function_entry t == fromIntegral (toInt entry)) $ cfi_gcc_except_tables $ binary_get_cfi bin of
      Nothing -> return (IS.empty,IS.empty)
      Just t -> do
        lps     <- IS.fromList <$> filterM (is_source_from_entry False) (IS.toList $ get_landing_pads_from_gcc_except_table t)
        starts  <- IS.fromList <$> filterM (is_source_from_entry False) (IS.toList $ get_callsite_region_starts_from_gcc_except_table t)
        ends    <- IS.fromList <$> filterM (is_source_from_entry False) (IS.toList $ get_callsite_region_ends_from_gcc_except_table t)
        let as = xgraph_all_parents $ current_inlining lr
        inlines <- IS.fromList <$> (filterM (is_source_from_entry True) $ IS.toList as)
        return $ (lps,IS.unions [lps,starts,ends,inlines])

  is_source_from_entry :: BinaryClass bin => Bool -> Int -> XLifted bin Bool
  is_source_from_entry strict a = do
    (bin,_,lr,_) <- ask
    if IM.lookup a (current_fmap lr) == Just entry then do
      if strict then do
        parents <- withLR $ get_prev_collapsed_calls entry $ InstructionAddress a
        return $ S.null parents
      else
        return True
    else
      return False
  


  add_address :: BinaryClass bin => Int -> Int -> Blocks -> XLifted bin (Maybe (Blocks, Int))
  add_address blockID a blocks = do
    g <- withLR $ gets current_cfg
    -- Start a new basic block if there is incoming branching
    let blockID' = if IS.size (xgraph_parents g a) <= 1 && IM.lookup a blocks == Nothing then blockID else a
    Just i <- withLR $ fetch $ InstructionAddress a
    case IM.lookup blockID' blocks of
      Nothing    -> return $ Just (IM.insert blockID' [i] blocks, blockID')
      Just block -> 
        if head block == i then
          return Nothing
        else if i `elem` block then
          error $ "Should not happen:" ++ show i ++ show block
        else do
          return $ Just (IM.insert blockID' (block ++ [i]) blocks, blockID')

lr_make_function_cfg :: BinaryClass bin => S.Set FunctionEntry -> Blocks -> Sources -> XLifted bin ControlFlowGraph
lr_make_function_cfg comp blocks sources = do
  (edges,leaks) <- foldlM' (add_edge blocks) (XEdges mempty mempty,mempty) $ IM.toList blocks
  return $ ControlFlowGraph blocks edges sources leaks comp
 where
  -- mk_edge blocks (a,[])    = return (a,IS.empty)
  add_edge blocks (g,leaks) (a,block) = do
    (new_entries,children) <- withLR $ get_next_collapsed_calls comp $ InstructionAddress $ fromIntegral $ inAddress $ last block
    if any (\child -> not $ (toInt child) `IM.member` blocks) children then do
      withLR $ xtoLog $ "Should not happen: " ++ showHex a ++ " --> " ++ show block ++ " --> " ++ show children ++ " in component " ++ show comp
      -- TODO happens in boost
      return (g,leaks)
    else do
      leaking_jmp <- is_jump_to_new_entry $ last block
      when (not $ S.null new_entries) $ do 
         withLR $ xtoLog $ "Instruction 0x" ++ show (last block) ++ " leaks to " ++ show new_entries
      when  leaking_jmp $ do
         withLR $ xDebug 0 $ "Instruction 0x" ++ show (last block) ++ " jumps to different function."

      let leaks' = if leaking_jmp || not (S.null new_entries) then IS.insert (fromIntegral $ inAddress $ last block) leaks else leaks
      return (xgraph_add_edges g a (IS.fromList $ map toInt $ S.toList children), leaks')


  is_jump_to_new_entry i = do
    if isJump (inOperation i) || isCondJump (inOperation i) then do
      nxt <- withLR $ next_rips i
      case nxt of
        NxtAddresses _ as -> anyM is_new_entry $ S.toList as
        _ -> return False
    else
      return False


  is_new_entry a = do
    a_entry <- withLR $ get_entry_of_instruction a
    case a_entry of
      Nothing -> return False -- This happens when $a$ is not a valid instruction address, perhaps a wrongly computed jump target from an indirection (TODO)
      Just a_entry -> return $ not $ a_entry `S.member` comp


lr_make_functions :: BinaryClass bin => XLifted bin (IM.IntMap ControlFlowGraph)
lr_make_functions = do
  xtoLog $ "Generating function boundaries and CFGs"
  -- Get all entry points of function snippets
  all_entries <- withLR $ gets (IM.foldr (IS.insert . toInt) IS.empty . current_fmap)
  -- For each snippet, produce all blocks as well as their "leaking" edges (i.e., blocks that have normal control flow to outside of the snippet)
  -- The graph $g$ has an edge (snip0,snip1) iff snip0 leaks to snip1
  (g0,block_map) <- execStateT (get_blocks all_entries) (xgraph_empty,mempty)
  (bin,_,_,_) <- ask
  let g = foldl' (add_ehframe_edge block_map) g0 $ get_all_landing_pads bin ++ get_all_region_starts bin
  -- Partition that graph into weak components.
  -- Each component is a list [snip0,snip1,...] of mutually connected snippets
  IM.fromList <$> (concatMapM (mk_component g block_map) $ xgraph_weak_components g)
 where
  -- Let comp == [snip0,snip1,...]
  -- This component consists of snippets that leak to each other, and thus belong to the same function.
  -- This component will be turned into pair(s) of the form (f,g) with f the function entry point, and g the CFG.
  mk_component g block_map comp = do
    -- Find the snippet that is the source (that is going to be the function entrypoint)
    real_entries <- withLR $ gets current_real_entries
    let srcs  = IS.filter (is_function_entry real_entries g) comp 
    -- Partition the component if there are multiple sources
    let funcs = decompose g (IS.toList srcs) comp
    -- Each source is a function entrypoint
    mapM (mk_func block_map) funcs

  -- We consider something a function entry if during lifting it has been regsitered as a "real entry" or if it a source of the component of snippets.
  is_function_entry real_entries g a = FunctionEntry a `S.member` real_entries || xgraph_is_source g a

  mk_func block_map (f,comp) = do
    -- Function $f$ is composed from snippets $comp$
    when (IS.size comp >= 2) $ xtoLog $ "Function 0x" ++ showHex f ++ " is composed out of snippets " ++ showHex_set comp
    let all_blocks  = IM.unions $ map (\entry -> fst $ block_map IM.! entry) $ IS.toList comp
    let all_sources = IS.unions $ map (\entry -> snd $ block_map IM.! entry) $ IS.toList comp
    g <- lr_make_function_cfg (cast_intset_to_entries comp) all_blocks all_sources
    return (f,g)

  add_ehframe_edge block_map g (e,lp) 
    | fromIntegral e `IM.member` block_map && lp `IM.member` block_map = xgraph_add_edge g (fromIntegral e) lp
    | otherwise = g


  -- If the snippets have no source, then just pick one as the entry point
  decompose g []    comp       = [(IS.findMin comp,comp)]
  -- If the snippets have a single source, that is the entry point
  decompose g [src] comp       = [(src,comp)]
  -- If the snippets have mulitple sources, these are all entry points.
  -- Take all snippets reachable from src0 and make that a function.
  -- Then proceed with the rest.
  decompose g (src0:srcs) comp = 
    let reach0 = foldl' (remove_snippets_from g) (IS.intersection comp (xgraph_is_reachable g src0)) srcs
        comp'  = IS.difference comp reach0 in 
      (src0,reach0) : decompose g srcs comp'

  show_component g comp = showHex_set comp ++ showHex_set (IS.filter (xgraph_is_source g) comp)

  remove_snippets_from g comp src = IS.difference comp (xgraph_is_reachable g src)

  -- Per snippet, get all basic blocks. Store the leaking.
  get_blocks :: BinaryClass bin => IS.IntSet -> StateT (XGraph,IM.IntMap (Blocks,Sources)) (XLifted bin) ()
  get_blocks entries
    | IS.null entries = return ()
    | otherwise = do
      let Just (entry,rest) = IS.minView entries 
      -- xtoLog $ "Generating blocks for entry 0x" ++ showHex entry
      (entries',blocks,srcs) <- lift $ lr_make_function_blocks $ FunctionEntry entry
      -- The leaking, but don't leak to landing pads or region starts from other entries
      let new_entries  = IS.delete entry $ IS.fromList $ map toInt $ S.toList entries'
      (bin,_,_,_) <- ask
      let new_entries' = IS.difference new_entries (all_eh_frame_address bin)
      --let new_entries' = IS.filter (not . is_covered bin) new_entries 

      modify (\(g,all_blocks) -> (xgraph_add_edges g entry new_entries',IM.insert entry (blocks,srcs) all_blocks))
      get_blocks rest

  all_eh_frame_address bin = IS.union (IS.fromList $ map fromIntegral $ get_all_function_entries bin) (IS.fromList $ map snd $ get_all_landing_pads bin ++ get_all_region_starts bin)
  is_covered bin a = 
    case get_gcc_except_table_covering_address (binary_get_ehframe_covering bin) a of
      Just _  -> True
      Nothing -> False


cast_intset_to_entries = S.fromList . map FunctionEntry . IS.toList



-- CALL GRAPH
-- TODO move to own file, add LEA's, make callgraph with leaks

mk_callgraph :: BinaryClass bin => LiftedRepresentationFunctions bin -> XGraph
mk_callgraph l =
  let all_edges = concatMap get_edges_for_function $ IM.assocs $ lrf_cfgs l in
    foldl' (\g (a0,a1) -> xgraph_add_edge g a0 a1) (foldl' xgraph_add_vertex xgraph_empty $ IM.keys $ lrf_cfgs l) all_edges
 where
  get_edges_for_function (entry,cfg) =
    let calls = get_all_internal_calls cfg in
      zip (repeat entry) (IS.toList calls)

  get_all_internal_calls cfg = 
    let calls = map (get_calls_from_blockID l cfg) $ IM.keys $ cfg_basic_blocks cfg in
      IS.fromList $ mapMaybe get_internal_target $ map snd $ concatMap (S.toList) calls

  get_internal_target (NxtInternalCall trgt)    = Just $ toInt trgt
  get_internal_target (NxtAddresses Nothing as) = Just $ toInt $ S.findMin as
  get_internal_target _                         = Nothing
  
    


get_calls_from_blockID :: BinaryClass bin => LiftedRepresentationFunctions bin -> ControlFlowGraph -> Int -> S.Set (Instruction,Next)
get_calls_from_blockID l cfg blockID =
  let instrs = cfg_basic_blocks cfg IM.! blockID
      calls  = mapMaybe ifHasCallTarget instrs
      leaks  = map mk_leak $ IS.toList $ IS.filter (\a -> a == (fromIntegral $ inAddress $ last instrs)) $ cfg_leaks cfg in
    S.fromList $ leaks ++ calls
 where
  ifHasCallTarget i
    | isCall (inOperation i) || isJump (inOperation i) || isCondJump (inOperation i) =
      case IM.lookup (fromIntegral $ inAddress i) $ lrf_nexts l of
        Just nxt@(NxtInternalCall trgt)     -> Just (i,nxt)
        Just nxt@(NxtAddresses (Just f) as) -> Just (i,nxt)
        Just nxt@(NxtReturn    (Just f))    -> Just (i,nxt)
        Just nxt@(NxtTerminal  (Just f))    -> Just (i,nxt)
        _ -> Nothing
    | otherwise = Nothing

  mk_leak a =
    case (IM.lookup a $ lrf_instrs l, IM.lookup a $ lrf_nexts l)  of
      (Just i,Just nxt) -> (i,nxt)




