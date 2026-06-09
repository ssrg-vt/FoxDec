{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, DeriveGeneric, DefaultSignatures, FlexibleContexts, StrictData #-}


{-# OPTIONS_HADDOCK hide #-}


module OutputGeneration.CallGraph2 where

import Base
import Algorithm.Graph

import InputLifting.Types
import Binary.FunctionNames

import Data.JumpTarget
import Data.VerificationCondition
import Data.Indirection
import Data.X86.Instruction
import Data.X86.Opcode
import Data.X86.Register

import Binary.Generic
import Data.SymbolicExpression



import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set.NonEmpty as NES
import Data.List
import Data.List.Split (chunksOf)
import Data.List.Extra (groupSort)
import Data.Maybe
import Data.Word



-- CALL GRAPH

-- TODO add leaks, using get_entry_of_instruction to map addresses to entries
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
  
    

mk_callgraph_LEA_edges :: BinaryClass bin => LiftedRepresentationFunctions bin -> XGraph
mk_callgraph_LEA_edges l =
  let all_edges = concatMap get_edges_for_function $ IM.assocs $ lrf_cfgs l in
    foldl' (\g (a0,a1) -> xgraph_add_edge g a0 a1) xgraph_empty all_edges
 where
  get_edges_for_function (entry,cfg) =
    let leas = get_all_LEAs cfg in
      zip (repeat entry) (IS.toList leas)

  get_all_LEAs cfg = 
    let leas = filter (\a -> a `IS.member` all_entries) $ mapMaybe get_lea_pointer $ concat $ IM.elems $ cfg_basic_blocks cfg in
      IS.fromList leas

  all_entries = IM.keysSet $ lrf_cfgs l




get_lea_pointer i@(Instruction _ _ LEA [dst,op] _ _) = mk_rip_relative (inAddress i) (inSize i) op
get_lea_pointer _ = Nothing

mk_rip_relative :: Word64 -> Int -> Operand -> Maybe Int
mk_rip_relative a si op@(Op_Mem _ (Reg64 RIP) RegNone 0 displ Nothing info) = Just $ fromIntegral $ fromIntegral a + displ + fromIntegral si
mk_rip_relative a si op@(Op_Mem _ (Reg64 RIP) _ _ _ _ _) = error $ "TODO: " ++ show op
mk_rip_relative a si op@(Op_Mem _ _ (Reg64 RIP) _ _ _ _) = error $ "TODO: " ++ show op
mk_rip_relative a si op = Nothing


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




callgraph_to_dot :: BinaryClass bin => LiftedRepresentationFunctions bin -> IM.IntMap IS.IntSet -> IM.IntMap IS.IntSet -> String
callgraph_to_dot lr es fptrs =
  "diGraph " ++ name ++ "{\n"
  ++ intercalate "\n" (map node_to_dot $ IS.toList $ IS.fromList $ IM.keys es ++ IM.keys fptrs)
  ++ "\n\n"
  ++ intercalate "\n" (map (edge_to_dot' "") $ IM.assocs es)
  ++ "\n\n"
  ++ intercalate "\n" (map (edge_to_dot' "[style=dotted]") $ IM.assocs fptrs)
  ++ "\n}"
 where
  node_to_dot v =
    let bgcolor = node_color v
        fgcolor = hex_color_of_text bgcolor in
       "\t"
    ++ mk_node v
    ++ "  [shape=plaintext,label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">"
    ++ concatMap (mk_row fgcolor bgcolor) (mk_node_lines v)
    ++ "</TABLE>>]"

  mk_node_lines v = [ function_name_of_entry bin (fromIntegral v) ]

  mk_row fgcolor bgcolor str = "<TR><TD BGCOLOR=\"" ++ bgcolor ++ "\"><FONT COLOR=\"" ++ fgcolor ++ "\">" ++ str ++ "</FONT></TD></TR>"
  

  edge_to_dot'  style (v,vs) = intercalate "\n" $ map (edge_to_dot'' style v) $ IS.toList vs
  edge_to_dot'' style v v'   = "\t" ++ mk_node v ++ " -> " ++ mk_node v'  ++ " " ++ style

  mk_node v = binary_file_name bin ++ "_" ++ showHex v

  node_shape v = "Mrecord"

  node_color v = "#90EE90" -- light green

  markup vcs = take max_limit_node_text_size_as_indicated_by_graphviz $ map replace [c | c <- vcs, c /= '|']

  replace '\n' = '|'
  replace c    = c

  max_limit_node_text_size_as_indicated_by_graphviz = 15000

  bin   = lrf_binary lr
  name  = binary_file_name bin



