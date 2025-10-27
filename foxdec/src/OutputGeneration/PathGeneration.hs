{-# LANGUAGE PartialTypeSignatures , FlexibleContexts #-}


module OutputGeneration.PathGeneration where




import Base
import Config


import OutputGeneration.CallGraph
import Algorithm.Graph

import Data.CFG hiding (num_of_instructions)
import Data.SValue
import Data.SPointer
import Data.GlobalMem
import Data.L0

import Binary.FunctionNames
import Data.X86.Opcode
import Data.X86.Instruction
import Data.JumpTarget

import Binary.Generic

import WithNoAbstraction.Lifted
import WithAbstractPredicates.ControlFlow
import WithAbstractSymbolicValues.FInit


import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.ByteString as BS (readFile,writeFile) 
import qualified Data.Serialize as Cereal hiding (get,put)
import Data.List 
import Data.List.Extra (firstJust)
import Data.Word
import Data.Function ((&))
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as B

import Control.Monad
import Control.Monad.State.Strict

import Debug.Trace



-- A nested path is a path of basic blocks that allows function calls to be unfolded.
-- Example:
-- 6d8e0: [0,1,2,3] can be path within function 6d8e0. Let's say blocks 0 and 1 end in a call. That can be unfolded, producing:
--
-- 6d8e0: [0]
--  4f530: [0,1,3,4,6,7]
-- 6d8e0: [1]
--  59880: [0]
--   87a10: [0,1]
--  59880: [1,2,20]
-- 6d8e0: [2,3]
--
data NestedPath = NestedPath [NestedPathElement]
  deriving (Eq,Ord)

data NestedPathElement =
    PathWithinFunction Int [Int] (Maybe Int) Bool -- ^ Within a function with the given entry, a path of blocks in its CFG.
                                                  -- The Int is the number of instructions of the final block. If Nothing, execute the entire block.
                                                  -- The Bool is true iff the call in the last block is unfolded.
  | CallReturn Int Int NestedPath -- ^ A call from caller to callee, unfolded to the nested path
 deriving (Eq,Ord)


nested_path_size (NestedPath ps) = sum $ map path_within_function_size ps

path_within_function_size (PathWithinFunction _ bs _ _) = length bs
path_within_function_size (CallReturn _ _ np) = nested_path_size np


generate_paths_to_address :: Lifted -> Maybe Int -> Int -> (Int,S.Set NestedPath)
generate_paths_to_address l max_count a =
  let np1@(PathWithinFunction entry1 p1 n1 is_unfolded1) = generate_path_within_function_to_address l a
      ps = generate_nested_paths_to_function_entry l max_count entry1 in
    (entry1,S.map (append_after np1) ps)
 where
  append_after np1@(PathWithinFunction entry1 p1 n1 is_unfolded1) (NestedPath ps0) =
    case last ps0 of
      PathWithinFunction entry0 [] Nothing False ->
        if entry0 == entry1 then
          NestedPath $ init ps0 ++ [np1]
        else
          error "Error"
  


-- given an address, find the function contaiing that address.
-- generate a path from the function entry towards the address
-- The path is not unfolded.
generate_path_within_function_to_address :: Lifted -> Int -> NestedPathElement
generate_path_within_function_to_address l@(bin,_,l0) a =
  case firstJust find_function_containing_address $ IM.toList $ l0_get_cfgs l0 of
    Just (entry,cfg,blockID) -> find_path entry cfg blockID
    Nothing -> error $ "No function containing address: 0x" ++ showHex a
 where
  find_function_containing_address (entry,cfg) =
    case find block_has_address $ IM.toList $ cfg_instrs cfg of
      Just (blockID, is) -> Just (entry,cfg,blockID)
      Nothing -> Nothing

  block_has_address (blockID,is) = any instruction_has_address is
  instruction_has_address i = inAddress i == fromIntegral a

  find_path entry cfg blockID =
    case find_path_from_source_to cfg blockID of
      Just p -> 
        let (is_unfolded,n) = last_block_info l entry cfg (takeWhile (not . address_is a)) p in
          PathWithinFunction entry p n is_unfolded
  address_is a i = inAddress i == fromIntegral a


last_block_info l entry cfg take [] = (False,Nothing)
last_block_info l entry cfg take p =
  let blockID     = last p
      is          = cfg_instrs cfg IM.! blockID
      is'         = take is
      n           = if length is == length is' then Nothing else Just $ length is'
      is_unfolded = if n == Nothing then not $ block_contains_internal_call l entry blockID else True in
    (is_unfolded,n)


-- The paths are not unfolded.
generate_nested_paths_to_function_entry :: Lifted -> Maybe Int -> Int -> S.Set NestedPath
generate_nested_paths_to_function_entry l@(bin,config,l0) max_count entry =
  let -- generate the callgraph
      (callgraph,fptrs) = mk_callgraph l
      -- traverse callgraph backwards: generate paths in the call graph from function $entry$ upto sources
      vs'      = graph_traverse_upwards callgraph entry
      subgraph = graph_delete_from entry $ graph_mk_subgraph vs' callgraph
      paths    = produce_set_of_paths subgraph max_count 1
      -- turn paths in the call graph to nested paths over CFGs of the functions
      nps      = S.map (path_in_call_graph_to_nested_path l) paths in
    nps

 


{--
  --let dirname  = binary_dir_name bin
  --let name     = binary_file_name bin
  --let dot      = callgraph_to_dot l pp_finitC subgraph (Edges IM.empty)
  --let fname    = dirname ++ name ++ ".callgraph.sub.dot" 
  --writeFile fname dot
  --putStrLn $ "Sources: " ++ showHex_set (find_source_nodes callgraph)
  --putStrLn $ "Written: " ++ fname


    --putStrLn $ showHex_list path
    --print np'
    --putStrLn "\n"
--}



instance Show NestedPath where
  show p = show_with_indent 0 p
   where
    show_with_indent ind (NestedPath []) = ""
    show_with_indent ind (NestedPath (PathWithinFunction entry [] n _ :ps)) = show_indent ind ++ show_entry entry True ++ "[]\n" ++ show_with_indent ind (NestedPath ps)
    show_with_indent ind (NestedPath (PathWithinFunction entry p n unfolded:ps)) = show_indent ind ++ show_entry entry unfolded ++ show p ++ show_n n ++ "\n" ++ show_with_indent ind (NestedPath ps)
    show_with_indent ind (NestedPath (CallReturn entry0 entry1 np:ps)) = show_with_indent (ind+1) np ++ show_with_indent ind (NestedPath ps)

    show_indent ind = replicate ind ' '
    show_entry entry unfolded = showHex entry ++ (if unfolded then "" else "(folded)") ++ ": "
 
    show_n Nothing  = ""
    show_n (Just n) = " (" ++ show n ++ " instrs. of last block)"



-- given a function, find paths from its entry point to an exit node (either a return, or a jump to a function)
-- unfold all function calls within this path, producing a nested path
function_call_to_nested_paths l@(bin,config,l0) max_count entry must_return = do
  let cfg = l0_get_cfgs l0 IM.! entry
  visited_entries <- get
  let returning_paths = find_paths cfg visited_entries (block_ends_in_ret l entry)
  if S.null returning_paths && must_return then do
    let unresolved_paths = find_paths cfg visited_entries (block_ends_in_unresolved_jump l entry)
    if S.null unresolved_paths then
      error $ "Cannot find a returning path from 0x" ++ showHex entry ++ " to return with already visited: " ++ showHex_set visited_entries
    else
      mapM (unfold cfg) $ S.toList unresolved_paths
  else if must_return then
    mapM (unfold cfg) $ S.toList returning_paths
  else do
    -- TODO prefer returning and then exiting paths, if sufficiently are found
    let exiting_paths    = find_paths cfg visited_entries (block_ends_in_exit l entry)
    let unresolved_paths = find_paths cfg visited_entries (block_ends_in_unresolved_jump l entry)
    let paths            = cap $ S.unions [returning_paths, exiting_paths, unresolved_paths]
    if S.null paths then
      -- TODO find a longer path here, we now just take the initial block
      mapM (unfold cfg) [[0]]
      -- error $ "Cannot find a path from 0x" ++ showHex entry ++ " to end-node with already visited: " ++ showHex_set visited_entries
    else
      mapM (unfold cfg) $ S.toList paths
 where
  find_paths cfg visited_entries = find_paths_satisfying_resulting_in (showHex entry) cfg max_count 0 (isUnvisited visited_entries)

  isUnvisited visited_entries = not . block_contains_call_to_any_entry_from l entry visited_entries

  unfold cfg p = 
    let (is_unfolded,n) = last_block_info l entry cfg id p in
      unfold_function_calls l $ NestedPath [PathWithinFunction entry p n is_unfolded] 

  cap = S.fromList . take max_count . S.toList


-- unfold all calls in the nested path
unfold_nested_path l np = evalState (unfold_function_calls l np) IS.empty

-- unfold function calls, building a nested path
unfold_function_calls :: Lifted -> NestedPath -> State IS.IntSet NestedPath
unfold_function_calls l (NestedPath []) = return $ NestedPath []
unfold_function_calls l (NestedPath es) = do
  es' <- mapM (unfold_function_call_path_element l True) $ init es
  e'  <- unfold_function_call_path_element l False $ last es
  return $ NestedPath $ concat $ es' ++ [e']

unfold_function_call_path_element :: Lifted -> Bool -> NestedPathElement -> State IS.IntSet [NestedPathElement]
unfold_function_call_path_element l _ (CallReturn entry0 entry1 np) = do
  np' <- unfold_function_calls l np
  return $ [CallReturn entry0 entry1 np']
unfold_function_call_path_element l@(_,_,l0) must_return e@(PathWithinFunction entry p n unfolded) =
  case break (block_contains_internal_call l entry) p of
    -- the path contains no calls, no unfolding required
    (_,[])  -> return $ [e]
    -- the path ends in a call, maybe unfolding required
    (p0,[b]) -> 
      if unfolded then
        return $ [e]
      else if n /= Nothing && fromJust n < num_of_instructions (l0_get_cfgs l0 IM.! entry) b then
        return $ [e]
      else
        do_unfolding must_return p0 b False [] n unfolded
    -- the path contains a call in block b
    (p0,(b:p1)) -> do_unfolding True p0 b must_return p1 n unfolded
 where
  do_unfolding must_return0 p0 b must_return1 p1 n unfolded = do
    -- unfold the call in block b
    -- simply takes the first of the possible calls
    let Just (trgt:_) = block_get_internall_calls l entry b
    let entry' = fromIntegral trgt
    modify $ IS.insert entry'
    np' <- head <$> function_call_to_nested_paths l 1 entry' must_return0
    let e' = CallReturn entry entry' np'
    modify $ IS.delete entry'
    -- unfold the remainder of the path 
    p1' <- if p1 == [] then return [] else unfold_function_call_path_element l must_return1 $ PathWithinFunction entry p1 n unfolded
    -- the nested path contains three parts: the path leading up to block b, the unfolding of b, and the remainder
    return $ PathWithinFunction entry (p0++[b]) Nothing True : e' : p1'




-- turn a path in a call graph to a sequence of paths in the CFGs of the functions
-- The produced paths are not unfolded.
path_in_call_graph_to_nested_path :: Lifted -> [Int] -> NestedPath
path_in_call_graph_to_nested_path l@(bin,config,l0) p = NestedPath $ go p
 where
  go []                = []
  go [entry]           = [PathWithinFunction entry [] Nothing False]
  go (entry0:entry1:p) = 
    let cfg             = l0_get_cfgs l0 IM.! entry0
        blocks          = intgraph_V cfg
        relevant_blocks = IS.filter (block_contains_call_to l entry0 entry1) blocks in
      case firstJust (find_path_from_source_to cfg) $ IS.toList relevant_blocks of
        Just p0 -> PathWithinFunction entry0 p0 Nothing True : go (entry1:p)
        Nothing -> error $ show (showHex entry0,showHex entry1,cfg,relevant_blocks, map (find_path_from_source_to cfg) $ IS.toList blocks, intgraph_sources cfg)

num_of_instructions cfg blockID = length $ cfg_instrs cfg IM.! blockID


-- returns true iff the given block ends in a call/jmp to any of the entries
block_contains_call_to_any_entry_from l entry visited_entries blockID =
  case block_get_internall_calls l entry blockID of
    Nothing -> False
    Just trgts -> not $ IS.null $ IS.intersection (IS.fromList $ map fromIntegral trgts) visited_entries

-- returns true iff the given block ends in a call/jmp to entry
block_contains_call_to l entry0 entry1 blockID =
  case block_get_internall_calls l entry0 blockID of
    Nothing -> False
    Just trgts -> fromIntegral entry1 `elem` trgts

-- returns true iff the given block ends in a call/jmp
block_contains_internal_call l entry blockID =
  case block_get_internall_calls l entry blockID of
    Nothing -> False
    Just trgts -> trgts /= []

-- if the block ends in an internal call, return the targets
block_get_internall_calls l@(_,_,l0) entry blockID =
  let cfg = l0_get_cfgs l0 IM.! entry
      is  = cfg_instrs cfg IM.! blockID in
    get_trgts $ last is
 where
  get_trgts i 
    | isCall (inOperation i) || (isJump (inOperation i) && not (isCondJump $ inOperation i) && jump_is_actually_a_call l i) =
      Just $ get_known_internal_jump_targets l i
    | otherwise = Nothing

get_known_internal_jump_targets l i = concatMap get_internal_trgt $ get_known_jump_targets l i
 where
  get_internal_trgt (ImmediateAddress a) = [a]
  get_internal_trgt _ = []












data FunctionBlockID = FunctionBlockID Int Int
  deriving (Eq,Ord)

instance Show FunctionBlockID where
  show (FunctionBlockID entry blockID) = "0x" ++ showHex entry ++ "_" ++ show blockID

data TransitingState = TransitingState {
  ts_callstack :: [FunctionBlockID]
 }
  deriving (Eq,Ord)

instance Show TransitingState where
  show (TransitingState es) = show (TransitingPath es Nothing)


data TransitingPath = TransitingPath [FunctionBlockID] (Maybe (FunctionBlockID,Int))
  deriving (Eq,Ord)

instance Show TransitingPath where
  show p@(TransitingPath [] end) = showEnd end
    where
     showEnd Nothing = ""
     showEnd (Just (FunctionBlockID entry blockID,n)) = "0x" ++ showHex entry ++ ": " ++ show blockID ++ " (" ++ show n ++ ")"
  show p@(TransitingPath _  end)  =
    let (same,others) = split_transiting_path p
        str0 = showEntry (head same) ++ ": " ++ (intercalate "," $ map showBlockID same)
        str1 = show (TransitingPath others end) in
      str0 ++ (if str1 == "" then "" else "\n" ++ str1)
    where
     showBlockID (FunctionBlockID _ blockID) = show blockID
     showEntry   (FunctionBlockID entry _ )  = "0x" ++ showHex entry


split_transiting_path (TransitingPath [] _)     = ([],[])
split_transiting_path (TransitingPath (v:vs) _) = 
   let (same,others) = span (equalFunction v) vs in
     (v:same,others)
 where
  equalFunction (FunctionBlockID entry0 _) (FunctionBlockID entry1 _) = entry0 == entry1


transiting_path_length (TransitingPath p _) = length p

transiting_path_take_last n (TransitingPath p end) = TransitingPath (drop (length p - n) p) end

append_transiting_paths (TransitingPath p0 Nothing) (TransitingPath p1 end) = TransitingPath (p0 ++ p1) end

generate_transiting_paths_to_address :: Lifted -> Maybe Int -> Int -> (Int,S.Set TransitingPath)
generate_transiting_paths_to_address l@(bin,config,l0) max_count a =
  case firstJust find_function_containing_address $ IM.toList $ l0_get_cfgs l0 of
    Just (entry,cfg,blockID) -> 
      let p1  = generate_transiting_path_within_function_to_address entry cfg blockID a
          ps0 = generate_transiting_path_to_function_entry l max_count entry in
        (entry,S.map (\p0 -> append_transiting_paths p0 p1) ps0)
    Nothing -> error $ "No function containing address: 0x" ++ showHex a
 where
  find_function_containing_address (entry,cfg) =
    case find block_has_address $ IM.toList $ cfg_instrs cfg of
      Just (blockID, is) -> Just (entry,cfg,blockID)
      Nothing -> Nothing
  block_has_address (blockID,is) = any instruction_has_address is
  instruction_has_address i = inAddress i == fromIntegral a


  generate_transiting_path_within_function_to_address entry cfg blockID a =
    case find_path_from_source_to cfg blockID of
      Just p -> TransitingPath (map (FunctionBlockID entry) $ init p) (last_block_info entry cfg blockID a)

  last_block_info entry cfg blockID a =
    let is          = cfg_instrs cfg IM.! blockID
        is'         = takeWhile (not . address_is a) is in
      if length is' == 0 then Nothing else Just (FunctionBlockID entry blockID, length is')
  address_is a i = inAddress i == fromIntegral a



generate_transiting_path_to_function_entry :: Lifted -> Maybe Int -> Int -> S.Set TransitingPath
generate_transiting_path_to_function_entry l@(bin,config,l0) max_count entry =
  let -- generate the callgraph
      (callgraph,fptrs) = mk_callgraph l
      -- traverse callgraph backwards: generate paths in the call graph from function $entry$ upto sources
      vs'      = graph_traverse_upwards callgraph entry
      subgraph = graph_delete_from entry $ graph_mk_subgraph vs' callgraph
      paths    = produce_set_of_paths subgraph max_count 1
      -- turn paths in the call graph to nested paths over CFGs of the functions
      nps      = S.map (path_in_call_graph_to_transiting_path l max_count) paths in
    nps

path_in_call_graph_to_transiting_path :: Lifted -> Maybe Int -> [Int] -> TransitingPath
path_in_call_graph_to_transiting_path l@(bin,config,l0) maxcount p = TransitingPath (concat $ go p) Nothing
 where
  go []                = []
  go [entry]           = []
  go (entry0:entry1:p) = 
    let cfg             = l0_get_cfgs l0 IM.! entry0
        blocks          = intgraph_V cfg
        relevant_blocks = IS.filter (block_contains_call_to l entry0 entry1) blocks in
      case firstJust (find_path_from_source_to cfg) $ IS.toList relevant_blocks of
        Just p0 -> map (FunctionBlockID entry0) p0 : go (entry1:p)
        Nothing -> error $ show (showHex entry0,showHex entry1,cfg,relevant_blocks, map (find_path_from_source_to cfg) $ IS.toList blocks, intgraph_sources cfg)






generate_paths_to_address' :: Lifted -> Int -> Int -> (Int,S.Set TransitingPath)
generate_paths_to_address' l@(_,_,l0) max_count a =
  case firstJust find_function_containing_address $ IM.toList $ l0_get_cfgs l0 of
    Just (entry,blockID) -> (entry,generate_paths_towards entry blockID)
    Nothing -> error $ "No function containing address: 0x" ++ showHex a
 where
  find_function_containing_address (entry,cfg) =
    case find block_has_address $ IM.toList $ cfg_instrs cfg of
      Just (blockID, is) -> Just (entry,blockID)
      Nothing -> Nothing

  block_has_address (blockID,is) = any instruction_has_address is
  instruction_has_address i = inAddress i == fromIntegral a

  generate_paths_towards entry blockID = 
    let -- generate the callgraph
        (callgraph,fptrs) = mk_callgraph l
        -- traverse callgraph backwards: generate paths in the call graph from function $entry$ upto sources
        vs'      = graph_traverse_upwards callgraph entry
        subgraph = graph_delete_from entry $ graph_mk_subgraph vs' callgraph
        -- get the sources of this subgraph
        srcs = intgraph_sources subgraph

        v0 = FunctionBlockID (head $ IS.toList srcs) 0
        g  = StatefullGraph transiting_init_state transiting_nexts
        p  = \_ -> True
        paths = S.map (\p -> TransitingPath p Nothing) $ find_paths_satisfying_resulting_in' g l True v0 p (q entry blockID) in
      --if S.null paths then
      --  error $ "No paths from " ++ showHex_set srcs ++ " to " ++ showHex entry ++ ", " ++ show blockID
      --else
        paths
  q trgt_entry trgt_blocKID state (FunctionBlockID entry blockID) = entry == trgt_entry && blockID == trgt_blocKID

function_to_paths :: Lifted -> Int -> S.Set TransitingPath
function_to_paths l entry =
  let v0    = FunctionBlockID entry 0
      g     = StatefullGraph transiting_init_state transiting_nexts
      p     = \_ -> True
      paths = find_paths_satisfying_resulting_in' g l True v0 p q in
    if S.null paths then
      let paths = find_paths_satisfying_resulting_in' g l False v0 p q in
        if S.null paths then
          S.empty --error $ "Cannot find paths for function: 0x" ++ showHex entry
        else
          S.map (\p -> TransitingPath p Nothing) paths
    else
      S.map (\p -> TransitingPath p Nothing) paths
 where
  q state (FunctionBlockID entry blockID) = 
    let stack_popped  = all hasZeroBlockID $ ts_callstack state
        internal_call = block_contains_internal_call l entry blockID
        ends_in_ret   = block_ends_in_ret l entry blockID
        ends_in_ujump = block_ends_in_unresolved_jump l entry blockID
        ends_in_exit  = block_ends_in_exit l entry blockID in
     --traceShow (showHex entry, blockID, ts_callstack state, stack_popped, internal_call, ends_in_ret, ends_in_ujump, ends_in_exit) $ 
      or [
        and [ stack_popped
            , not internal_call
            , ends_in_ret || ends_in_ujump ]
       ,
        and [ not internal_call
            , ends_in_exit ]
       ]

  tr entry blockID = traceShow (showHex entry,show blockID, not (block_contains_internal_call l entry blockID), block_ends_in_ret l entry blockID, block_ends_in_unresolved_jump l entry blockID, block_ends_in_exit l entry blockID)

  hasZeroBlockID (FunctionBlockID _ blockID) = blockID == 0

transiting_init_state :: Lifted -> FunctionBlockID -> TransitingState
transiting_init_state l@(_,_,l0)  (FunctionBlockID entry _) = TransitingState [FunctionBlockID entry 0]



-- A transition function for transiting from instruction address to the set of next instruction addresses.
-- It keeps track of the current call stack so that it knows where to return.
transiting_nexts :: Lifted -> FunctionBlockID -> State TransitingState (S.Set FunctionBlockID)
transiting_nexts l@(_,_,l0) (FunctionBlockID entry blockID) = do
  (TransitingState callstack) <- get
  assert $ getEntry (head callstack) == entry
  let trgts = block_get_internall_calls l (fromIntegral entry) blockID
  proceed trgts

  --ts <- proceed trgts
  --return $ traceShow ("HELLO", showHex entry, show blockID, show ts) ts
 where
  proceed trgts
    | trgts /= Nothing && length (fromJust trgts) > 0 = proceed_with_call_to $ fromIntegral $ head $ fromJust trgts
    | block_ends_in_ret l entry blockID = proceed_return blockID
    | otherwise = proceed_normally blockID

  -- Execute a normal block
  -- Find edges in the CFG of the current function
  proceed_normally blockID = do
    (TransitingState callstack) <- get
    let cfg = l0_get_cfgs l0 IM.! entry
    let nexts = post cfg blockID
    return $ S.fromList $ map (\next -> FunctionBlockID entry next) $ IS.toList nexts

  -- Execute a RET
  -- Pop the callstack until we find a blockID not equal to 0 to return to.
  -- The transition goes to that blockID of that function.
  proceed_return blockID = do
    unwind_while_blockID_is_zero
    (TransitingState callstack) <- get
    let (FunctionBlockID _ blockID:callstack') = callstack
    put $ TransitingState callstack'
    let (FunctionBlockID caller _:_) = callstack'
    if blockID == 0 then
      return $ S.empty
    else
      return $ S.singleton $ FunctionBlockID caller blockID

  -- Execute a function call to an internal function
  -- Push to the call stack: FunctionBlockID $callee$ $blockID$
  -- where $callee$ is the entry address of the function that is called
  -- and $blockID$ is the blockID of the current function where we should return to.
  -- If the callee terminates, blockID is set to 0.
  -- The transition goes to blockID 0 (the initial block) of the callee.
  proceed_with_call_to entry' = do
    let cfg   = l0_get_cfgs l0 IM.! entry
    let nexts = IS.toList $ post cfg blockID
    let next  = if nexts == [] then 0 else head nexts
    (TransitingState callstack) <- get
    put $ TransitingState (FunctionBlockID entry' next:callstack)
    return $ S.singleton $ FunctionBlockID entry' 0

  assert True  = return ()
  assert False = error "False assertion"

  getEntry (FunctionBlockID entry _) = entry


  -- it may be the case that we do not call functions but jump into them
  -- that is registered by having the returning blockID be 0.
  -- If we then return, we should return to the returing blockID of the last call, i.e.,
  -- unwind until we find a non-zero blockID
  unwind_while_blockID_is_zero = do
    (TransitingState callstack) <- get
    case callstack of
      [_] -> return () -- error $ "Unwinding to empty call stack"
      (FunctionBlockID _ 0:callstack') -> put (TransitingState callstack') >> unwind_while_blockID_is_zero
      _ -> return ()

-- returns true iff the given block ends in a return
block_ends_in_ret l@(_,_,l0) entry blockID =
  let cfg = l0_get_cfgs l0 IM.! entry in
    case IM.lookup blockID $ cfg_instrs cfg of
      Just is -> is_return (last is) || is_jump_that_returns (last is)
 where
  is_return i = isRet $ inOperation i
  is_jump_that_returns i = isJump (inOperation i) && not (isCondJump $ inOperation i) && jump_is_actually_a_call l i


block_ends_in_unresolved_jump l@(_,_,l0) entry blockID =
  let cfg = l0_get_cfgs l0 IM.! entry in
    case IM.lookup blockID $ cfg_instrs cfg of
      Just is -> is_unresolved_jump (last is)
 where
  is_unresolved_jump i = isJump (inOperation i) && not (isCondJump $ inOperation i) && any ((==) Unresolved) (get_known_jump_targets l i)

block_ends_in_exit l@(_,_,l0) entry blockID =
  let cfg = l0_get_cfgs l0 IM.! entry in
    case IM.lookup blockID $ cfg_instrs cfg of
      Just is -> is_terminal (last is)
 where
  is_terminal i = next_rips l (Just i) == Terminal


