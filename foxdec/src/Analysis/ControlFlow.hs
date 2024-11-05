{-# LANGUAGE PartialTypeSignatures, Strict #-}

{-|
Module      : ControlFlow
Description : Contains functions pertaining to control flow graph generation.
-}

module Analysis.ControlFlow (
   cfg_gen,
   cfg_to_dot,
   is_end_node,
   node_info_of,
   stepA,
   post,
   fetch_block,
   resolve_call,
   resolve_jump_target,
   get_internal_addresses,
   jump_is_actually_a_call,
   show_block,
   show_invariants,
   isTerminal
 )
 where

import Base

import Algorithm.SCC
import Data.Pred
import Analysis.Context
import Analysis.FunctionNames

import Data.SymbolicExpression
import Data.JumpTarget
import Generic.Binary
import Generic.SymbolicConstituents

import X86.Conventions
import X86.Instruction (addressof)
import X86.Opcode (isRet, isCall, isCondJump, isJump, isHalt)
import X86.Register
import qualified X86.Instruction as X86
import Generic.HasSize (sizeof)
import qualified Generic.Instruction as Instr

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Either (fromRight,fromLeft,partitionEithers)
import Data.Maybe (fromJust,fromMaybe,isNothing)
import Data.List
import Data.List.Split (chunksOf)
import Data.Word (Word64)
import Control.Monad ((>=>))
import Debug.Trace
import Numeric (readHex)
import GHC.Float.RealFracMethods (floorDoubleInt,int2Double)


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



init_cfg a = CFG { cfg_blocks = IM.singleton 0 [fromIntegral a], cfg_edges = IM.empty, cfg_addr_to_blockID = IM.singleton (fromIntegral a) 0, cfg_fresh = 1, cfg_instrs = IM.empty }


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









-- | The set of next blocks from the given block in a CFG
post :: CFG -> IS.Key -> IS.IntSet
post g blockId = fromMaybe IS.empty (IM.lookup blockId (cfg_edges g))



-- | Fetching an instruction list given a block ID
fetch_block ::
  CFG    -- ^ The CFG
  -> Int -- ^ The blockID
  -> [X86.Instruction]
fetch_block g blockId =
  case IM.lookup blockId $ cfg_instrs $ g of
    Nothing -> error $ "Block with ID" ++ show blockId ++ " not found in cfg."
    Just b -> b

isTerminal :: CFG -> IS.Key -> Bool
isTerminal cfg b = isNothing $ IM.lookup b (cfg_edges cfg)

-- | Resolves the first operand of a call or jump instruction.
-- First tries to see if the instruction is an indirection, that has already been resolved.
-- If not, try to statically resolve the first operand using @`operand_static_resolve`@.
-- If that resolves to an immediate value, see if that immediate value corresponds to an external function or an internal function.
--
-- Returns a list of @`ResolvedJumpTarget`@, since an indirection may be resolved to multiple targets.
resolve_jump_target ::
  Context        -- ^ The context
  -> X86.Instruction       -- ^ The instruction
  -> [ResolvedJumpTarget]
resolve_jump_target ctxt i =
  case (IM.lookup (fromIntegral $ addressof i) $ ctxt_inds ctxt, Instr.srcs i) of
    (Just ind,_)    -> indirection_to_jump_target ind -- already resolved indirection
    (Nothing,[op1]) -> [jump_target_for_instruction ctxt i]
 where
  indirection_to_jump_target (Indirection_Resolved trgts) = S.toList trgts
  indirection_to_jump_target (Indirection_JumpTable (JumpTable _ _ _ tbl)) = map ImmediateAddress $ IM.elems tbl

-- | Returns true iff the JUMP instruction is actually a CALL followed by implicit RET
jump_is_actually_a_call ::
  Context        -- ^ The context
  -> X86.Instruction       -- ^ The instruction
  -> Bool
jump_is_actually_a_call ctxt i =
  any resolves_to_function_entry $ resolve_jump_target ctxt i
 where
  resolves_to_function_entry Unresolved           = False
  resolves_to_function_entry (External sym)       = True
  resolves_to_function_entry (ImmediateAddress a) = IM.lookup (fromIntegral a) (ctxt_calls ctxt) /= Nothing




-- | Given a resolved jump target, get a possibly empty list of internal addresses to which the jump target can jump.
get_internal_addresses ::
  ResolvedJumpTarget -- ^ A resolved jump target
  -> [Int]
get_internal_addresses (External _)         = []
get_internal_addresses Unresolved           = []
get_internal_addresses (ImmediateAddress a) = [fromIntegral a]







-- | Shows the block associated to the givern blockID.
show_block ::
  CFG    -- ^ The CFG
  -> Int -- ^ The blockID
  -> String
show_block g b =
  let instrs = im_lookup ("show_block: Block " ++ show b ++ "in cfg.") (cfg_blocks g) b in
       show b ++ " ["
    ++ showHex (head instrs)
    ++ ","
    ++ showHex (last instrs)
    ++ "]"

-- | Shows invariants.
show_invariants
  :: CFG        -- ^ The CFG
  -> Invariants -- ^ The invariants
  -> String
show_invariants g invs = intercalate "\n\n" $ map show_entry $ IM.toList $ invs
 where
  show_entry (blockId, p) =  "Block " ++ show_block g blockId ++ ":\n" ++ show p






instance IntGraph CFG where
  intgraph_post = post
  intgraph_V    = IM.keysSet . cfg_blocks




is_new_function_call_to_be_analyzed ctxt trgt = (IM.lookup trgt $ ctxt_calls ctxt) == Nothing || (IM.lookup trgt $ ctxt_finits ctxt) == Nothing


read_reg reg (Predicate m _) = M.lookup (SP_Reg reg) m

resolve_call :: Context -> (FContext -> Int -> Maybe Predicate) -> Int -> X86.Instruction -> _
resolve_call ctxt get_invariant entry i =
  let resolved_addresses = resolve_jump_target ctxt i in
    if any ((==) Unresolved) resolved_addresses then
      Right [(fromIntegral (addressof i) + sizeof i,True)] -- Right []
    else 
      let nexts          = map next resolved_addresses
          (lefts,rights) = partitionEithers nexts in
        if lefts == [] then
          Right $ concat rights
        else
          Left $ S.fromList $ map (\a -> (i,a)) $ concat lefts
 where
  next (External sym) =
    -- external function call
    if  is_exiting_function_call sym then
      Right []
    else if take 5 sym == "error" then -- TODO or error_at_line
      let fctxt = mk_fcontext ctxt entry in
        case get_invariant fctxt (fromIntegral $ addressof i) >>= (read_reg RDI) of
          Nothing -> Right []
          Just v ->  if show v == "0" then Right [(fromIntegral (addressof i) + sizeof i,True)] else Right []
    else
      Right [(fromIntegral (addressof i) + sizeof i,True)]
  next (ImmediateAddress a') =
    -- call to an immediate address
    if not $ is_new_function_call_to_be_analyzed ctxt (fromIntegral a') then
      -- internal function call already analyzed
      if (IM.lookup (fromIntegral a') $ ctxt_calls ctxt) == Just Terminating then
        -- verified and terminating
        Right []
      else
        -- verified and returning
        Right [(fromIntegral (addressof i) + sizeof i,True)]
    else if graph_is_edge (ctxt_entries ctxt) entry (fromIntegral a') then
      -- recursion
      Right [(fromIntegral (addressof i) + sizeof i,True)]
    else
      -- new function, stop CFG generation here
      Left [fromIntegral a']


-- | An abstract step function
--
-- Given the entry address of the function currently under investigation, and the instruction address of the current instruction,
-- try to get the set of next instruction addresses.
-- 
-- This returns either:
--   * a set of tuples @(i,a)@ where @i@ is an instruction and @a@ its address. All these instructions are function calls that need to be analyzed before this current function entry can continue.
--   * a list of tuples @(a,b)@ where @a@ is an instruction address that may follow the current instruction, and @b@ is a Bool indicating whether that address belongs to a @call@
--
--   TODO the Lefts are ignored so need no to return them
stepA :: 
     Context   -- ^ The context
  -> (FContext -> Int -> Maybe Predicate)
  -> Int       -- ^ The entry address
  -> Int       -- ^ The instruction address
  -> IO (Either (S.Set (X86.Instruction,Int)) [(Int,Bool)])
stepA ctxt get_invariant entry a = do
  instr <- fetch_instruction ctxt $ fromIntegral a
  case instr of
    Nothing -> return $ Right [] -- error $ "Cannot find instruction at addres: " ++ showHex a
    Just i -> 
      if isHalt (Instr.opcode i) then
        return $ Right []
      else if isJump (Instr.opcode i) then
        return $ Right $ map (\a -> (a,False)) $ concatMap mk_jmp_trgt $ resolve_jump_target ctxt i 
      else if isCondJump $ Instr.opcode i then
        return $ Right $ map (\a -> (a,False)) $ (concatMap get_internal_addresses $ resolve_jump_target ctxt i) ++ [a + sizeof i]
      else if isCall $ Instr.opcode i then
        return $ resolve_call ctxt get_invariant entry i
      else if isRet (Instr.opcode i) then
        return $ Right []
      else
        return $ Right [(a + sizeof i,False)]
 where
  mk_jmp_trgt Unresolved           = []
  mk_jmp_trgt (External sym)       = []
  mk_jmp_trgt (ImmediateAddress a)
    | IM.lookup (fromIntegral a) (ctxt_calls ctxt) /= Nothing = []
    | otherwise = [fromIntegral a]


mk_graph :: Context -> (FContext -> Int -> Maybe Predicate) -> Int -> S.Set ((Int,Bool), Int) -> CFG -> S.Set (X86.Instruction,Int) -> IO (S.Set (X86.Instruction,Int),CFG) 
mk_graph ctxt get_invariant entry bag g new_calls =
  case S.minView bag of
    Nothing -> return $ (new_calls,g)
    Just (((a0,is_call_a0),a1),bag) -> do
      if is_edge g a0 a1 then 
        mk_graph ctxt get_invariant entry bag g new_calls
      else do
        let g' = add_edge (fromIntegral a0) (fromIntegral a1) is_call_a0 g
        nxt <- stepA ctxt get_invariant entry a1
        case nxt of
          Left new_calls' -> do
            mk_graph ctxt get_invariant entry bag g' (S.union new_calls' new_calls)
          Right as -> do
            let bag' = S.union (S.fromList $ map (\(a2,is_call_a1) -> ((a1,is_call_a1),a2)) as) bag
            mk_graph ctxt get_invariant entry bag' g' new_calls
    

fromJust' instrs as Nothing = error $ showHex_list as ++ show instrs
fromJust' _ _ (Just a) = a

cfg_add_instrs ctxt g = do
  instrs <- mapM block_to_instrs $ IM.toList $ cfg_blocks g
  return $ g { cfg_instrs = IM.fromList instrs }
 where
    block_to_instrs (a,as) = do 
      instrs <- mapM (fetch_instruction ctxt . fromIntegral) as
      return $ (a, map (fromJust' instrs as) instrs)

-- | Produce a CFG
--
-- Given the entry point of the function, generate either a CFG, or a set of new entry points to be analyzed first.
-- The set of new entry points are function entries called by the current function, but for which we do not know yet whether they terminate or not.
-- If a CFG is returned, then all function calls in that CFG have already been analyzed.
cfg_gen ::
     Context -- ^ The context
  -> _
  -> Int     -- ^ The entry point of the function
  -> IO (S.Set (X86.Instruction,Int),CFG)
cfg_gen ctxt get_invariant  entry = do
 let g           = init_cfg entry
 nxt            <- stepA ctxt get_invariant  entry entry
 let bag         = S.fromList $ map (\(a,is_call_a) -> ((entry,False),a)) (fromRight [] nxt) -- assumes entry is not a call
 (new_calls,g') <- mk_graph ctxt get_invariant entry bag g S.empty
 g''            <- cfg_add_instrs ctxt g'
 return (new_calls, g'')




        
-- | Returns true if the given blockID is a leaf-node in the given CFG.
is_end_node ::
  CFG     -- ^ The CFG
  -> Int  -- ^ The blockID
  -> Bool
is_end_node g b = IS.null $ post g b

is_unresolved_indirection ctxt i = (isCall (Instr.opcode i) || isJump (Instr.opcode i) || isCondJump (Instr.opcode i))
                   && (any ((==) Unresolved) $ resolve_jump_target ctxt i)


-- | Returns the @`NodeInfo`@ of a given blockID.
--
-- Assumes the given blockID corresponds to a leaf-node.
node_info_of ::
     Context -- ^ The context
  -> CFG
  -> Int
  -> NodeInfo 
node_info_of ctxt g blockId =
  let a    = last (im_lookup ("C.) Block " ++ show blockId ++ " in cfg.") (cfg_blocks g) blockId)
      i    = last (im_lookup ("D.) Block " ++ show blockId ++ " in instrs.") (cfg_instrs g) blockId) in
    if is_unresolved_indirection ctxt i then
      UnresolvedIndirection
    else if IS.null (post g blockId) && (isCall (Instr.opcode i) || isHalt (Instr.opcode i)) || is_terminating_jump i then
        Terminal
    else
      Normal

 where
  is_terminating_jump i = isJump (Instr.opcode i) &&
    case resolve_jump_target ctxt i of
      [External sym] -> is_exiting_function_call sym
      _              -> False






-- | Export a CFG to .dot file
--
-- Strongly connected components get the same color.
cfg_to_dot ::
     Context -- ^ The context
  -> CFG     -- ^ The CFG
  -> String
cfg_to_dot ctxt g =
 let name  = ctxt_name ctxt
     sccs     = scc_of g 0 IS.empty in
  "diGraph " ++ name ++ "{\n"
  ++ intercalate "\n" (map (node_to_dot sccs) $ IM.keys $ cfg_blocks g)
  ++ "\n\n"
  ++ intercalate "\n" (map edge_to_dot' $ IM.toList $ cfg_edges g)
  ++ "\n}"
 where
  node_to_dot sccs blockId =
    let bgcolor = hex_color_of blockId sccs
        fgcolor = hex_color_of_text bgcolor in
       "\t" 
    ++ mk_node blockId
    ++ "  ["
    ++ "style=filled fillcolor=\"" ++ bgcolor ++ "\" fontcolor=\"" ++ fgcolor ++ "\" shape=" ++ node_shape blockId ++ " "
    ++ "label=\""
    ++ show_block g blockId
    ++ "\"]"

  edge_to_dot' (blockId, blockIds) = intercalate "\n" $ map (edge_to_dot'' blockId) $ IS.toList blockIds

  edge_to_dot'' blockId blockId' = "\t" ++ mk_node blockId ++ " -> " ++ mk_node blockId'

  mk_node v = ctxt_name ctxt ++ "_" ++ showHex v

  node_shape blockId =
    case node_info_of ctxt g blockId of
      Normal -> "oval"
      Terminal -> "invhouse"
      UnresolvedIndirection -> "box3d"

hex_color_of vertex sccs =
  case findIndex (IS.member vertex) sccs of
    Just n -> hex_colors !! (126 - (floorDoubleInt $ 127 * int2Double n / int2Double (length sccs)))
    Nothing -> "#FFFFFF"








