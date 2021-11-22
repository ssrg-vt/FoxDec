module CallGraph where

import Base
import Context
import SimplePred
import X86_Datastructures
import MachineState
import CFG_Gen

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.List
import Data.List.Split (chunksOf)

calls_of_cfg ctxt cfg = IS.unions $ map (get_call_target ctxt) $ concat $ IM.elems $ cfg_instrs cfg
 where
  get_call_target ctxt i = 
    if is_call (i_opcode i) then
      IS.fromList $ concatMap get_internal_addresses $ resolve_jump_target ctxt i
    else
      IS.empty


summarize_verification_conditions ctxt vcs =
  let precs   = S.filter is_precondition    vcs
      asserts = S.filter is_assertion       vcs
      fcs     = S.filter is_func_constraint vcs
      stack_size = S.lookupMax $ S.unions $ S.map get_stack_offsets vcs in

    "Stacksize == " ++ print_stack_size stack_size
    ++ (if S.null precs   then "" else "\nPRECONDITIONS:\n" ++ intercalate "\n" (map (intercalate ",") $ chunksOf 1 $ S.toList $ S.map (pick_one_to_show . get_lhs_and_rhs) $ precs))
    ++ (if S.null asserts then "" else "\nASSERTIONS:\n" ++ show_assertions asserts)
    ++ (if S.null fcs     then "" else "\nFUNCTION CONSTRAINTS:\n" ++ show_function_constraints fcs)
 where
  print_stack_size Nothing  = "unknown"
  print_stack_size (Just i) = show i


  get_stack_offsets (Precondition a0 _ a1 _) = S.union (get_stack_offset a0) (get_stack_offset a1)
  get_stack_offsets (Assertion  _ a0 _ a1 _) = S.union (get_stack_offset a0) (get_stack_offset a1)
  get_stack_offsets _                        = S.empty -- TODO params?

  get_stack_offset  (SE_Op (Minus _) [SE_Var (SP_Reg RSP), SE_Immediate i]) = S.singleton i
  get_stack_offset  _ = S.empty

  get_lhs_and_rhs (Precondition lhs _ rhs _) = (lhs,rhs)
  get_lhs_and_rhs (Assertion _  lhs _ rhs _) = (lhs,rhs)

  pick_one_to_show (a0,a1) =
    if expr_to_addr_type ctxt a0 == S.singleton Local && not (expr_to_addr_type ctxt a1 == S.singleton Local) then
      strip_parentheses $ show a1
    else if expr_to_addr_type ctxt a1 == S.singleton Local && not (expr_to_addr_type ctxt a0 == S.singleton Local) then
      strip_parentheses $ show a0
    else if contains_bot a0 && all_bot_satisfy (\typ bot -> typ == FromCall) a0 then
      strip_parentheses $ show a0
    else if contains_bot a1 && all_bot_satisfy (\typ bot -> typ == FromCall) a1 then
      strip_parentheses $ show a1
    else
      show a0 ++ " SEP " ++ show a1



  show_assertions ps = intercalate "\n" $ map (show_assertions_of_address ps) $ S.toList $ S.map (\(Assertion rip _ _ _ _) -> rip) ps
  show_assertions_of_address ps rip =
    let assertions = S.filter (\(Assertion rip' _ _ _ _) -> rip' == rip) ps in
      "@" ++ show rip ++ ": " ++ intercalate "," (S.toList $ S.map (pick_one_to_show . get_lhs_and_rhs) $ assertions)
 
  show_function_constraints fcs = intercalate "\n" $ map show $ S.toList fcs




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
    case IM.lookup v $ ctxt_report ctxt of
      Just (Report _ _ _ VerificationSuccess _) -> "Mrecord"
      Just (Report _ _ _ VerificationSuccesWithAssertions _) -> "Mrecord"
      _ -> "record"

  node_label v = 
    case IM.lookup v $ ctxt_report ctxt of
      Nothing -> function_name_of_entry ctxt v
      Just (Report _ _ _ _ vcs) -> 
        if S.null vcs then
          function_name_of_entry ctxt v
        else
          "{" ++ function_name_of_entry ctxt v ++ "|" ++ markup (summarize_verification_conditions ctxt vcs) ++ "}"

  node_color v =
    let verified = case IM.lookup v (ctxt_report ctxt) of
                     Nothing -> VerificationError
                     Just (Report _ _ _ v _) -> v
     in
      case verified of
        VerificationSuccess               -> "#90EE90" -- light green
        VerificationSuccesWithAssertions  -> "#89CFF0" -- light blue
        VerificationError                 -> "#FF7F7F" -- light red
        VerificationUnresolvedIndirection -> "#CBC3E3" -- light purple

        
     
 

  markup vcs = take max_limit_node_text_size_as_indicated_by_graphviz $ map replace [c | c <- vcs, c /= '|']

  replace '\n' = '|'
  replace c    = c

  max_limit_node_text_size_as_indicated_by_graphviz = 15000
