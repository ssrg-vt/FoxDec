{-# LANGUAGE PartialTypeSignatures, Strict #-}

{-|
Module      : ControlFlow
Description : Contains functions pertaining to control flow graph generation.
-}

module WithAbstractPredicates.ControlFlow 
 where

import Base

import Algorithm.SCC

import Binary.FunctionNames
import Data.X86.Opcode
import Data.X86.Instruction


import Data.L0
import Data.JumpTarget
import Data.Indirection
import Data.CFG
import Binary.Generic 

import Conventions


import Control.Monad.Extra


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

import GHC.Float.RealFracMethods (floorDoubleInt,int2Double)


data NextRips = JustRips [Word64] | UnresolvedTarget | Terminal | UnvisitedFunctionCall Word64
  deriving (Eq,Show)

next_rips :: (BinaryClass bin, Eq pred) =>
     Lifting bin pred finit v
  -> Maybe Instruction
  -> NextRips
next_rips l@(bin,_,_) Nothing  = JustRips []
next_rips l@(bin,_,_) (Just i) = next_rip_based_on_opcode i $ inOperation i
 where
  next_rip_based_on_opcode i op
    | isHalt op = Terminal
    | isJump op =
      if jump_is_actually_a_call l i then
        case resolve_call l i of
          JustRips _       -> JustRips []
          Terminal         -> Terminal
          UnresolvedTarget -> UnresolvedTarget
      else let trgts = get_known_jump_targets l i in
        if all ((==) Unresolved) trgts then
          UnresolvedTarget
        else
          JustRips $ concatMap mk_jmp_trgt trgts 
    | isCondJump op =
      let trgts = get_known_jump_targets l i in
        if all ((==) Unresolved) trgts then
          UnresolvedTarget
        else
          JustRips $ (concatMap mk_jmp_trgt trgts) ++ [inAddress i + fromIntegral (inSize i)]
    | isCall op = resolve_call l i
    | isRet op  = JustRips $ []
    | otherwise = JustRips $ [inAddress i + fromIntegral (inSize i)]

  mk_jmp_trgt (ImmediateAddress a) = [a]
  mk_jmp_trgt (Unresolved) = []


resolve_call :: (BinaryClass bin, Eq pred) => Lifting bin pred finit v -> Instruction -> NextRips
resolve_call l@(bin,_,l0) i =
  let resolved_addresses = get_known_jump_targets l i in
    if all ((==) Unresolved) resolved_addresses then
      case l0_lookup_indirection (inAddress i) l0 of
        Just _ -> JustRips $ [inAddress i + fromIntegral (inSize i)]
        _ -> UnresolvedTarget
    else let nexts = map next resolved_addresses in
      foldr1 gather nexts
 where
  gather n@(UnvisitedFunctionCall i) _  = n
  gather (JustRips as) (JustRips as')   = JustRips $ as++as'
  gather _ n@(UnvisitedFunctionCall i)  = n
  gather Terminal Terminal              = Terminal
  gather (JustRips as) Terminal         = JustRips as
  gather Terminal (JustRips as)         = JustRips as
  gather UnresolvedTarget (JustRips as) = JustRips as
  gather (JustRips as) UnresolvedTarget = JustRips as
  gather x y = error $ show (x,y)


  next (External sym)
    -- external function call
    | is_exiting_function_call sym = Terminal
    | sym == "error" = UnresolvedTarget
    | otherwise = JustRips $ [inAddress i + fromIntegral (inSize i)]
  next (ExternalDeref sym)
    -- external function call
    | is_exiting_function_call sym = Terminal
    | otherwise = JustRips $ [inAddress i + fromIntegral (inSize i)]
  next (ImmediateAddress a') =
    case l0_lookup_entry a' l0 of
      Nothing          -> -- Unvisited, stop CFG generation here
                          UnvisitedFunctionCall a'
      Just (_,Nothing) -> -- Recursive unanalyzed call
                          JustRips $ [inAddress i + fromIntegral (inSize i)] 
      Just (_,Just r)  -> -- Already analyzed call
                          if result_post r == Terminates then Terminal else JustRips [inAddress i + fromIntegral (inSize i)]
  next (Returns True) = JustRips [inAddress i + fromIntegral (inSize i)]
  next (Returns False) = Terminal
  next (Unresolved)    = UnresolvedTarget


-- | Returns true iff the JUMP instruction is actually a CALL followed by implicit RET
jump_is_actually_a_call ::
  BinaryClass bin =>
     Lifting bin pred finit v
  -> Instruction       -- ^ The instruction
  -> Bool
jump_is_actually_a_call l@(_,_,l0) i =
  let trgts = get_known_jump_targets l i in
    any resolves_to_function_entry trgts
 where
  resolves_to_function_entry Unresolved           = False
  resolves_to_function_entry (External sym)       = True
  resolves_to_function_entry (ExternalDeref sym)  = True
  resolves_to_function_entry (Returns _)          = True
  resolves_to_function_entry (ImmediateAddress a) = 
    case l0_lookup_entry a l0 of
      Nothing -> False
      Just _  -> True


get_known_jump_targets ::
  BinaryClass bin =>
     Lifting bin pred finit v
  -> Instruction       -- ^ The instruction
  -> [ResolvedJumpTarget]
get_known_jump_targets l@(bin,_,l0) i =
  case l0_lookup_indirection (inAddress i) l0 of
    Just r -> S.toList $ S.unions $ S.map indirection_to_jump_target r
    _      -> singleton $ jump_target_for_instruction bin i
 where
  indirection_to_jump_target (Indirection_JumpTable (JumpTable _ _ _ tbl)) = S.fromList $ map ImmediateAddress $ IM.elems tbl
  indirection_to_jump_target (Indirection_Resolved trgt) = S.singleton trgt
  indirection_to_jump_target (Indirection_Unresolved) = S.singleton Unresolved








instance IntGraph CFG where
  intgraph_V    = IM.keysSet . cfg_blocks
  intgraph_post = post
   where
    post :: CFG -> IS.Key -> IS.IntSet
    post g blockId = fromMaybe IS.empty (IM.lookup blockId (cfg_edges g))
-- | Export a CFG to .dot file
--
-- Strongly connected components get the same color.
cfg_to_dot ::
  BinaryClass bin =>
     bin
  -> FResult pred v
  -> String
cfg_to_dot bin (FResult g post _ _ _ _) =
 let name = binary_file_name bin
     sccs = scc_of g 0 IS.empty in
  "diGraph " ++ name ++ "{\n"
  ++ intercalate "\n" (map (node_to_dot g post sccs) $ IM.keys $ cfg_blocks g)
  ++ "\n\n"
  ++ intercalate "\n" (map edge_to_dot' $ IM.toList $ cfg_edges g)
  ++ "\n}"
 where
  node_to_dot g post sccs blockId =
    let bgcolor = hex_color_of blockId sccs
        fgcolor = hex_color_of_text bgcolor in
       "\t" 
    ++ mk_node blockId
    ++ "  ["
    ++ "style=filled fillcolor=\"" ++ bgcolor ++ "\" fontcolor=\"" ++ fgcolor ++ "\" shape=" ++ node_shape post blockId ++ " "
    ++ "label=\""
    ++ show_block g blockId
    ++ "\"]"

  edge_to_dot' (blockId, blockIds) = intercalate "\n" $ map (edge_to_dot'' blockId) $ IS.toList blockIds

  edge_to_dot'' blockId blockId' = "\t" ++ mk_node blockId ++ " -> " ++ mk_node blockId'

  mk_node v = binary_file_name bin ++ "_" ++ showHex v

  node_shape _ _ =  "oval" {-- TODO
  node_shape (ReturnsWith _) blockId = "oval"
  node_shape (Terminates) blockId = "terminator"
  node_shape (TimeOut) blockId = "invtriangle"
  node_shape (HasUnresolvedIndirections blockIDs) blockId 
    | blockId `elem` blockIDs = "box3d"
    | otherwise = "oval"
  node_shape (VerificationError errors) blockId 
    | blockId `elem` map fst errors = "invtriangle"
    | otherwise = "oval"--}
    


hex_color_of vertex sccs =
  case findIndex (IS.member vertex) sccs of
    Just n -> hex_colors !! (126 - (floorDoubleInt $ 127 * int2Double n / int2Double (length sccs)))
    Nothing -> "#FFFFFF"



-- | Shows the block associated to the givern blockID.
show_block ::
  CFG    -- ^ The CFG
  -> Int -- ^ The blockID
  -> String
show_block g b =
  let instrs = cfg_blocks g IM.! b in
       show b ++ " ["
    ++ showHex (head instrs)
    ++ ","
    ++ showHex (last instrs)
    ++ "]"



