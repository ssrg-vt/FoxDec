{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, DeriveGeneric, DefaultSignatures, FlexibleContexts, StrictData #-}


{-# OPTIONS_HADDOCK hide #-}


module OutputGeneration.CallGraph where

import Base
import Algorithm.Graph

import WithAbstractPredicates.ControlFlow
import Binary.FunctionNames

import Data.L0
import Data.CFG
import Data.JumpTarget
import Data.VerificationCondition
import Data.Indirection
import Data.X86.Opcode (isCall,isJump)
import Data.X86.Instruction


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
import Data.Maybe (fromJust)
import Debug.Trace


mk_callgraph l@(bin,_,l0) =
  let cfgs  = l0_get_cfgs l0
      fptrs = Edges $ IM.map get_function_pointer_intros $ l0_functions l0
      g     = Edges $ IM.map (calls_of_cfg l) cfgs in
    (g,fptrs)

get_call_graph_sources l@(bin,_,l0) = find_source_nodes $ fst $ mk_callgraph l
 

get_function_pointer_intros = IS.unions . map get_ptrs . S.toList . result_vcs . fromJust . snd
 where
  get_ptrs (FunctionPointers _ ptrs) = ptrs


calls_of_cfg l cfg = IS.unions $ map get_call_target $ IM.elems $ cfg_instrs cfg
 where
  get_call_target instrs
    | isCall (inOperation $ last instrs) = IS.fromList $ concatMap get_internal_address $ get_known_jump_targets l $ last instrs
    | isJump (inOperation $ last instrs) && jump_is_actually_a_call l (last instrs) = IS.fromList $ concatMap get_internal_address $ get_known_jump_targets l $ last instrs
    | otherwise = IS.empty
  get_internal_address (ImmediateAddress a) = [fromIntegral a]
  get_internal_address _ = []




callgraph_to_dot :: BinaryClass bin => Lifting bin pred finit v -> (finit -> String) -> Graph -> Graph -> String
callgraph_to_dot (bin,_,l0) pp_finit (Edges es) (Edges fptrs) =
 let name  = binary_file_name bin in
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

  mk_node_lines v = 
    let finit_printed = pp_finit $ finit v in
      [ function_name_of_entry bin (fromIntegral v) ] ++ lines finit_printed

  mk_row fgcolor bgcolor str = "<TR><TD BGCOLOR=\"" ++ bgcolor ++ "\"><FONT COLOR=\"" ++ fgcolor ++ "\">" ++ str ++ "</FONT></TD></TR>"
  

  edge_to_dot'  style (v,vs) = intercalate "\n" $ map (edge_to_dot'' style v) $ IS.toList vs
  edge_to_dot'' style v v'   = "\t" ++ mk_node v ++ " -> " ++ mk_node v'  ++ " " ++ style

  mk_node v = binary_file_name bin ++ "_" ++ showHex v

  node_shape v =
    case result v of
      Just (ReturnsWith _) -> "Mrecord"
      Just (Terminates)    -> "Mrecord"
      _                    -> "Mrecord"


  result v = fromJust . fmap result_post . snd <$> (IM.lookup v $ l0_functions l0)
  finit v = fromJust $ fst <$> (IM.lookup v $ l0_functions l0)

  node_color v =
    case result v of
      Nothing -> "#FF7F7F" -- light red
      Just (ReturnsWith _) -> "#90EE90" -- light green
      Just (TimeOut) -> "#FF7F7F" -- light red
      Just (Terminates) ->  "#006400" -- dark green
      Just (HasUnresolvedIndirections _) ->  "#CBC3E3" -- light purple
      Just (VerificationError _) ->  "#FF7F7F" -- light red

  entry_has_unresolved_indirections v = 
    let Just cfg = IM.lookup v $ l0_get_cfgs l0
        blocks = IM.elems $ cfg_instrs cfg in
      any block_has_unresolved_instruction blocks

  block_has_unresolved_instruction instrs = contains_unresolved_indirections $ l0_lookup_indirection (inAddress $ last instrs) l0

  contains_unresolved_indirections (Just inds) = Indirection_Unresolved `S.member` inds
  contains_unresolved_indirections Nothing = False

  markup vcs = take max_limit_node_text_size_as_indicated_by_graphviz $ map replace [c | c <- vcs, c /= '|']

  replace '\n' = '|'
  replace c    = c

  max_limit_node_text_size_as_indicated_by_graphviz = 15000
