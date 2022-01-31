{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, DeriveGeneric, DefaultSignatures, FlexibleContexts, StrictData #-}


{-# OPTIONS_HADDOCK hide #-}


module CallGraph where

import Base
import Context
import SimplePred
import X86_Datastructures
import MachineState
import ControlFlow
import Pointers

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.List
import Data.List.Split (chunksOf)
import Data.List.Extra (groupSort)
import Data.Maybe (fromJust)
import Debug.Trace

pp_bot (Bottom (FromSources srcs))    = if S.size srcs > 5 then "Bot" else intercalate "," (map pp_source $ S.toList srcs)
pp_bot (Bottom (FromPointerBases bs)) = if S.size bs   > 5 then "Bot" else intercalate "," (map pp_base $ S.toList bs)
pp_bot e                              = pp_expr e

pp_base StackPointer            = pp_expr (SE_Var $ SP_Reg RSP)
pp_base (Malloc id h)           = pp_expr $ SE_Malloc id h
pp_base (GlobalAddress a)       = showHex a
pp_base (PointerToSymbol a sym) = "&" ++ sym
pp_base (Unknown e)             = pp_expr e

pp_source (Src_Var v)       = pp_expr $ SE_Var v
pp_source (Src_Malloc id h) = pp_expr $ SE_Malloc id h
pp_source (Src_Function f)  = f


pp_statepart (SP_Mem a si)  = "[" ++ pp_bot a ++ "," ++ show si ++ "]" 
pp_statepart (SP_Reg r)     = show r

-- | Summarize preconditions
summarize_preconditions get_info show_e vcs =
  let pointer_pairs = map get_pointer_pair $ S.toList $ S.filter is_precondition vcs
      groups        = groupSort pointer_pairs in
    if groups == [] then
      ""
    else
      "PRECONDITIONS:\n" ++ (intercalate "\n" $ map show_group groups) ++ "\n"
 where
  get_pointer_pair (Precondition a0 _ a1 _) = (get_info a0, get_info a1)
  show_group (a,as) = show_e a ++ " SEP " ++ (intercalate "," $ nub $ map show_e as)

summarize_preconditions_short ctxt = summarize_preconditions (join_single ctxt) pp_bot
summarize_preconditions_long  ctxt = summarize_preconditions id show




-- | Summarize assertions
summarize_assertions get_info show_e vcs =
  let instr_assert_pairs      = map get_instr_assert_pair $ S.toList $ S.filter is_assertion vcs
      per_instruction         = groupSort instr_assert_pairs
      per_instruction_grouped = map (\(rip,pairs) -> (rip, groupSort pairs)) per_instruction in
    if per_instruction_grouped == [] then
      "" 
    else
      "ASSERTIONS:\n" ++ (intercalate "\n" $ map show_instr_group per_instruction_grouped) ++ "\n"
 where
  get_instr_assert_pair (Assertion rip a0 _ a1 _) = (rip, (a0, get_info a1))
  show_instr_group (rip,groups) = "@" ++ show_e rip ++ ": " ++ intercalate" && " (map show_group groups)
  show_group (a,as) = show_e a ++ " SEP " ++ (intercalate "," $ nub $ map show_e as)

summarize_assertions_short ctxt = summarize_assertions (join_single ctxt) pp_bot
summarize_assertions_long  ctxt = summarize_assertions id show


-- | Summarize function constraints
summarize_function_constraints_short ctxt vcs =
  let fcs = S.filter is_func_constraint vcs in
    if S.null fcs then
      ""
    else
      "FUNCTION CONSTRAINTS:\n" ++ (intercalate "\n" $ sort $ map summarize_fcs $ S.toList fcs) ++ "\n"
 where
  summarize_fcs (FunctionConstraint f i_a params sps) = "@" ++ showHex i_a ++ ": " ++ f ++ parens (intercalate "," $ map show_param params) ++ " PRESERVES " ++ show_sps sps

  show_param (r,e) = show r ++ show_param_eq_sign e ++ strip_parentheses (show_param_value e)
  show_param_value e = if not (contains_bot e) then pp_bot e else pp_bot $ join_single ctxt e
  show_param_eq_sign e = if contains_bot e then "~=" else ":="
  
  show_sps sps =
    let (stackframe,others) = partition is_stack_frame $ S.toList sps
        showed_stack_frame  = show_stack_frame stackframe in
      intercalate "," $ filter ((/=) []) $ showed_stack_frame : map pp_statepart others

  is_stack_frame (SP_Mem (SE_Op (Minus _) [SE_Var (SP_Reg RSP), SE_Immediate _]) _) = True
  is_stack_frame (SP_Mem (SE_Var (SP_Reg RSP)) _)                                   = True
  is_stack_frame sp                                                                 = False


  show_stack_frame sps =
    if sps  == [] then
      ""
    else let offset = maximum $ map get_offset sps
             top    = maximum $ map get_top sps in
      "[ RSP_0 - " ++ show offset ++ " TO RSP_0" ++ (if top == 0 then "" else " + " ++ show top) ++ " ]"

  get_offset (SP_Mem (SE_Op (Minus _) [SE_Var (SP_Reg RSP), SE_Immediate offset]) _) = offset
  get_offset (SP_Mem (SE_Var (SP_Reg RSP)) _)                                        = 0

  get_top (SP_Mem (SE_Op (Minus _) [SE_Var (SP_Reg RSP), SE_Immediate offset]) si) = si - fromIntegral offset
  get_top (SP_Mem (SE_Var (SP_Reg RSP)) si)                                        = si

summarize_function_constraints_long :: Context -> S.Set VerificationCondition -> String
summarize_function_constraints_long ctxt vcs = 
  let fcs = S.filter is_func_constraint vcs in
    if S.null fcs then
      ""
    else
      "FUNCTION CONSTRAINTS:\n" ++ (intercalate "\n" $ map show $ S.toList fcs) ++ "\n"


-- | Summarize sourceless memwrites
summarize_sourceless_memwrites_short ctxt vcs = 
  let mws = S.filter is_sourceless_memwrite vcs in
    if S.null mws then
      ""
    else
      "SOURCELESS MEM WRITES:\n" ++ intercalate "\n" (map (intercalate ",") $ chunksOf 5 $ sort $ nub $ map get_location $ S.toList mws) ++ "\n"
 where
   get_location (SourcelessMemWrite (MemWriteFunction f a sp))       = f ++ "@" ++ showHex a
   get_location (SourcelessMemWrite (MemWriteInstruction a addr a')) = "@" ++ showHex a

summarize_sourceless_memwrites_long ctxt vcs =
  let mws = S.filter is_sourceless_memwrite vcs in
    if S.null mws then
      ""
    else
      "SOURCELESS MEM WRITES:\n" ++ (intercalate "\n" $ map show $ S.toList mws) ++ "\n"


-- | Summarize function initialization
summarize_finit Nothing      = ""
summarize_finit (Just finit) = if M.null finit then "" else "INITIAL:\n" ++ intercalate "\n" (map (intercalate ",") $ chunksOf 1 $ map show_finit_eq $ M.toList finit) ++ "\n"
 where
  show_finit_eq (sp,e) = pp_statepart sp ++ " ~= " ++ pp_bot e

parens str = "(" ++ str ++ ")"

summarize_verification_conditions ctxt entry =
  let vcs        = IM.lookup entry (ctxt_vcs ctxt) `orElse` S.empty
      finit      = IM.lookup entry $ ctxt_finits ctxt
      summary    = summarize_finit finit
                   ++ summarize_preconditions_short ctxt vcs
                   -- ++ summarize_assertions_short ctxt vcs
                   ++ summarize_function_constraints_short ctxt vcs
                   ++ summarize_sourceless_memwrites_short ctxt vcs in
    butlast summary
 where
  butlast ""  = ""
  butlast str = init str



calls_of_cfg ctxt cfg = IS.unions $ map (get_call_target ctxt) $ concat $ IM.elems $ cfg_instrs cfg
 where
  get_call_target ctxt i = 
    if is_call (i_opcode i) then
      IS.fromList $ concatMap get_internal_addresses $ resolve_jump_target ctxt i
    else
      IS.empty





callgraph_to_dot :: Context -> Graph -> String
callgraph_to_dot ctxt (Edges es) =
 let name  = ctxt_name ctxt in
  "diGraph " ++ name ++ "{\n"
  ++ intercalate "\n" (map node_to_dot $ IM.keys $ es)
  ++ "\n\n"
  ++ intercalate "\n" (map edge_to_dot' $ IM.assocs es)
  ++ "\n}"
 where
  node_to_dot v =
    let bgcolor = node_color v
        fgcolor = hex_color_of_text bgcolor in
       "\t" 
    ++ mk_node v
    ++ "  ["
    ++ "style=filled fillcolor=\"" ++ bgcolor ++ "\" fontcolor=\"" ++ fgcolor ++ "\" shape=" ++ node_shape v ++ " "
    ++ "label=\"" ++ node_label v ++ "\""
    ++ "]"

  edge_to_dot' (v,vs) = intercalate "\n" $ map (edge_to_dot'' v) $ IS.toList vs
  
  edge_to_dot'' v v' = "\t" ++ mk_node v ++ " -> " ++ mk_node v' 
  
  mk_node v = ctxt_name ctxt ++ "_" ++ showHex v

  node_shape v =
    case IM.lookup v $ ctxt_results ctxt of
      Just VerificationSuccess               -> "Mrecord"
      Just VerificationSuccesWithAssumptions -> "Mrecord"
      _                                      -> "record"

  node_label v = 
    let finit = IM.lookup v $ ctxt_finits ctxt in
      case IM.lookup v $ ctxt_vcs ctxt of
        Nothing  -> function_name_of_entry ctxt v
        Just vcs -> 
          if S.null vcs then
            function_name_of_entry ctxt v
          else
            "{" ++ function_name_of_entry ctxt v ++ "|" ++ markup (summarize_verification_conditions ctxt v) ++ "}"

  node_color v =
    let verified = case IM.lookup v (ctxt_results ctxt) of
                     Nothing -> VerificationError "Verification error"
                     Just v  -> v
     in
      case verified of
        VerificationSuccess               -> "#90EE90" -- light green
        VerificationSuccesWithAssumptions -> "#89CFF0" -- light blue
        VerificationError _               -> "#FF7F7F" -- light red
        VerificationUnresolvedIndirection -> "#CBC3E3" -- light purple

        
     
 

  markup vcs = take max_limit_node_text_size_as_indicated_by_graphviz $ map replace [c | c <- vcs, c /= '|']

  replace '\n' = '|'
  replace c    = c

  max_limit_node_text_size_as_indicated_by_graphviz = 15000
