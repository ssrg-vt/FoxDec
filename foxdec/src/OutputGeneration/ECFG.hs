{-# LANGUAGE PartialTypeSignatures, StrictData #-}

{-|
Module      : ControlFlow
Description : Contains functions pertaining to control flow graph generation.
-}

module OutputGeneration.ECFG where

import Base

import Algorithm.SCC

import Binary.FunctionNames
import Data.Symbol
import Data.X86.Opcode
import Data.X86.Instruction
import Data.X86.Register
import WithAbstractPredicates.ControlFlow
import WithNoAbstraction.SymbolicExecutionPath

import Data.L0
import Data.JumpTarget
import Data.Indirection
import Data.CFG
import Data.CFI
import Binary.Generic 
import Data.SymbolicExpression 

import Conventions


import Control.Monad.Extra
import Control.Monad.State.Strict


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
import System.Demangle.Pure


import System.IO.Unsafe

cfg_to_ecfg l@(bin,config,l0) entry cfg =
  let cfg' = cfg_compress l entry cfg in
    intercalate "\n" $ [dot_start , dot_nodes cfg', dot_edges cfg', dot_end]
 where
  dot_start = intercalate "\n" 
    [ "digraph ECFG {"
    , "  rankdir=LR;  // left-to-right layout (optional)"
    , "  node ["
    , "  shape=plaintext,"
    , "  fontname=\"Helvetica\""
    , "  ];"
    ]

  dot_nodes cfg = intercalate "\n" $ map mk_node $ IM.assocs $ cfg_blocks cfg

  mk_node (blockID,instrs) = 
    let a          = blockID_to_address blockID
        label_elts = [mk_init blockID,  mk_landing_pad blockID, mk_region_start blockID] ++ mk_region_colors blockID ++ [mk_relevant_calls blockID,mk_region_end blockID,mk_end blockID]
        label      = mk_HTML_label $ [("0x" ++ showHex a,"")] ++ filter (\(txt,color) -> txt /= "") label_elts in
      "  " ++ show blockID ++ " [label=" ++ label ++ "];"


  mk_init 0 = ("<B>START</B>", "")
  mk_init _ = ("","")

  mk_end blockID
    | IS.null (intgraph_post cfg blockID) = 
      let i = last $ cfg_instrs cfg IM.! blockID
          f = function_name_of_instruction bin i in
        if isRet $ inOperation i then
          ("Return", "")
        else if f `elem` relevant_calls l cfg blockID then
          ("", "")
        else
          ("Call " ++  f,"")
    | otherwise = ("","")

  mk_relevant_calls blockID = (intercalate "<BR/>" $ map (annotate_relevant_call blockID) $ relevant_calls l cfg blockID, "")

  mk_landing_pad blockID =
    case block_has_address_from blockID landing_pads of
      []  -> ("","")
      [a] -> case find (\(end,start,lp,action,color) -> lp == a) $ mk_regions of
               Nothing -> ("","")
               Just (end,start,lp,0,color)      -> ("CLEANUP-ONLY<BR/>LANDING PAD", color)
               Just (end,start,lp,action,color) -> ("LANDING PAD", color)

  mk_region_start blockID =
    case block_has_address_from blockID region_starts of
      []  -> ("","")
      [a] -> case find (\(end,start,lp,action,color) -> start == a) $ mk_regions of
               Nothing -> ("","")
               Just (end,start,lp,action,color) -> ("REGION START 0x" ++ showHex a, color)
      
  mk_region_end blockID =
    case block_has_address_from blockID $ IS.delete (blockID_to_address blockID) $ region_ends of
      []  -> ("","")
      [a] -> case find (\(end,start,lp,action,color) -> end == a) $ mk_regions of
               Nothing -> ("","")
               Just (end,start,lp,action,color) -> ("REGION END 0x" ++ showHex a, color)
      as -> ("TODO","#00FF00") 


  mk_region_colors blockID = map (\(end,start,lp,action,color) -> ("REGION", color)) $ filter (not . is_region_start blockID) $ nub $ concatMap get_regions_for_address $ blockID_to_addresses blockID 

  is_region_start blockID (end,start,lp,action,color) = any (\a -> a == start) $ (block_has_address_from blockID region_starts)

  mk_HTML_label strs = "< <TABLE BORDER=\"1\" CELLBORDER=\"1\" CELLSPACING=\"0\">" ++ concatMap mk_HTML_cell strs ++ "</TABLE> >"

  mk_HTML_cell (str,"")    = "<TR><TD>" ++ str ++ "</TD></TR>"
  mk_HTML_cell (str,color) = "<TR><TD bgcolor=\"" ++ color ++ "\"><FONT COLOR=\"" ++ hex_color_of_text color ++ "\">" ++ str ++ "</FONT></TD></TR>"

  dot_edges cfg = intercalate "\n" $ map mk_dot_edge $ IM.assocs $ cfg_edges cfg


  mk_dot_edge (blockID,blockIDs) =
    case block_has_address_from blockID landing_pads of
      []  -> mk_unlabeled_edge (blockID,blockIDs)
      [a] -> case find (\(end,start,lp,action,color) -> lp == a) $ mk_regions of
               Nothing -> mk_unlabeled_edge (blockID,blockIDs)
               Just (end,start,lp,0,color)      -> mk_unlabeled_edge (blockID,blockIDs)
               Just (end,start,lp,cs_action,color) -> if cs_action > 0 then mk_labeled_edges (obtain_typeinfo (fromIntegral cs_action) blockID blockIDs) (blockID,blockIDs) else error $ "Negative call-site action"



  mk_labeled_edges info (blockID,blockIDs) = intercalate "\n" $ map (mk_labeled_edge info blockID) $ IS.toList blockIDs

  mk_labeled_edge info blockID blockID' = 
    case find (\(_,type_info,rips) -> S.size rips == 1 && type_info /= "(NULL)" && blockID_to_address blockID' `S.member` rips) info of
      Nothing -> mk_unlabeled_edge (blockID, IS.singleton blockID')
      Just (_,type_info,_) -> "  " ++ show blockID ++ " -> " ++ show blockID' ++ "[label=\"" ++ type_info ++ "\"];"

  mk_unlabeled_edge (blockID,blockIDs) = "  " ++ show blockID ++ " -> " ++ intercalate "," (map show $ IS.toList blockIDs) ++ ";"

  dot_end = "}"

  blockID_to_address blockID = fromIntegral $ inAddress $ head $ cfg_instrs cfg IM.! blockID
  blockID_to_last_address blockID = fromIntegral $ inAddress $ last $ cfg_instrs cfg IM.! blockID
  blockID_to_addresses blockID = map (fromIntegral . inAddress) $ cfg_instrs cfg IM.! blockID

  t             = find (\t -> function_entry t == entry) $ cfi_gcc_except_tables $ binary_get_cfi bin
  landing_pads  = maybe IS.empty get_landing_pads_from_gcc_except_table t
  region_starts = maybe IS.empty get_callsite_region_starts_from_gcc_except_table t
  region_ends   = maybe IS.empty get_callsite_region_ends_from_gcc_except_table t


  block_has_address_from blockID as = filter (\a -> a `IS.member` as) $ map (fromIntegral . inAddress) $ cfg_instrs cfg IM.! blockID


  get_regions_for_address a = filter (is_region_for a) mk_regions 

  is_region_for a (end,start,lp,action,color) = a >=  start &&  a < end

  mk_regions = 
    let regions = (get_callsite_regions_from_gcc_except_table <$> t) `orElse` [] in
      map (mk_callsite_region $ length regions) $ zip [0..] $ filter (\(end,start,lp,action) -> lp /= fromIntegral entry) $ regions

  mk_callsite_region total (indx,(end,start,lp,action)) = (end,start,lp,action,hex_colors !! indx)





  get_all_action_records = maybe [] actions t

  find_action_at cs_action = find_action cs_action get_all_action_records

  find_action 1         (act:_) = act
  find_action cs_action (act:acts)
    | gcc_except_table_action_type_size act <= fromIntegral cs_action = find_action (cs_action - gcc_except_table_action_type_size act) acts
    | otherwise = error $ show (cs_action,act:acts) 

  traverse_actions act
    | gcc_except_table_action_type_next_action act == 0 = [act]
    | otherwise =
      case find (\act' -> gcc_except_table_action_type_address act' == gcc_except_table_action_type_next_address act) get_all_action_records of
        Just act' -> act : traverse_actions act'

  gcc_except_table_action_to_type_info act =
    let typs      = maybe [] type_infos t
        filter    = gcc_except_table_action_type_filter act
        typs_indx = (length typs - fromIntegral filter) in
       if filter == 0 then
         (gcc_except_table_action_type_filter act,Absolute 0)
       else if typs_indx < length typs then
         (gcc_except_table_action_type_filter act,typs !! typs_indx)
       else
         error $ show (typs,act)


  obtain_typeinfo cs_action blockID blockIDs =
    let a  = blockID_to_address blockID
        as = IS.map blockID_to_address blockIDs
        l  = (bin,config,l0,fromIntegral entry) 
        indexed_types_infos = map gcc_except_table_action_to_type_info $ traverse_actions $ find_action_at cs_action in
      map (get_trgt_from_indexed_type l a as) $ indexed_types_infos

  get_trgt_from_indexed_type l a as (rdx,type_info) =
    let symstates = unsafePerformIO $ symbolically_execute_until l 0 a as (init_sym_state_with (Reg64 RDX) $ fromIntegral rdx)
        rips      = S.map read_RIP symstates in
      (a,strip_typeinfo_for $ address_to_pointee type_info,rips)


  strip_typeinfo_for str
    | "typeinfo for " `isPrefixOf` str = drop 13 str
    | otherwise = str

  address_to_pointee (Absolute 0) = "(NULL)"
  address_to_pointee (Absolute a) =
    case IM.lookup (fromIntegral a) $ binary_get_symbol_table bin of
      Just sym@(PointerToObject f _ 0 _) -> demangle f `orElse` f
      Just sym@(Relocated_ResolvedObject f _0) -> demangle f `orElse` f
      _ -> case find (\(Relocation a0 a1) -> a0 == a) $ binary_get_relocations bin of
             Just (Relocation a0 a1) -> address_to_label a1
             _ -> "&0x" ++ showHex a

  address_to_label a = 
    case IM.lookup (fromIntegral a) $ binary_get_symbol_table bin of
      Just sym@(PointerToObject _ _ _ (Just l)) -> demangle l `orElse` l
      Just sym@(AddressOfLabel l _)             -> demangle l `orElse` l
      Just sym@(AddressOfObject l _)            -> demangle l `orElse` l
      _                                         -> "0x" ++ showHex a
      

  

  annotate_relevant_call blockID "__cxa_throw" =
    let a  = blockID_to_address blockID
        as = IS.singleton $ blockID_to_last_address blockID
        l  = (bin,config,l0,fromIntegral entry) 
        symstates = unsafePerformIO $ symbolically_execute_until l 0 a as init_symstate
        rsis = S.toList $ S.map (try_get_label . evalState (sread_reg (Reg64 RSI))) symstates in
      if any ("0x" `isPrefixOf`) rsis || length rsis /= 1 then
        "throw(" ++ intercalate "," rsis ++ ")"
      else 
        "throw(" ++ mk_dot_safe (strip_typeinfo_for (head rsis)) ++ ")"

  annotate_relevant_call _ f = f 


  try_get_label (SE_Immediate imm) = address_to_label imm
  try_get_label e                  = "0x" ++ show e -- bit ugly...


  mk_dot_safe []        = []
  mk_dot_safe ('<':str) = "&lt;" ++ mk_dot_safe str
  mk_dot_safe ('>':str) = "&gt;" ++ mk_dot_safe str
  mk_dot_safe (c:str)   = [c] ++ mk_dot_safe str


-- Compresses a CFG, i.e., removes node that are irrelevant.
-- Removing a node means connecting all parents to all children
-- A node is relevant if it either:
-- 1.) is a start or an end node
-- 2.) contains calls to, e.g., __cxa_throw
-- 3.) has CFI directives
-- 4.) is the beginning or end of a call-site region 
cfg_compress :: BinaryClass bin => Lifting bin pred finit v -> Word64 -> CFG -> CFG
cfg_compress l@(bin,_,_) entry cfg =
  let t = find (\t -> function_entry t == entry) $ cfi_gcc_except_tables $ binary_get_cfi bin
      landing_pads  = maybe IS.empty get_landing_pads_from_gcc_except_table t
      region_starts = maybe IS.empty get_callsite_region_starts_from_gcc_except_table t
      region_ends   = maybe IS.empty get_callsite_region_ends_from_gcc_except_table t
      all_cfi_addresses = IS.unions [landing_pads, region_starts ]
  in
    foldr (maybe_remove_node region_ends all_cfi_addresses) cfg (IM.keys $ cfg_edges cfg)
 where
  maybe_remove_node region_ends all_cfi_addresses blockID cfg
    | IS.null (intgraph_pre  cfg blockID) = cfg
    | IS.null (intgraph_post cfg blockID) = cfg
    | relevant_calls l cfg blockID /= []  = cfg
    | block_has_cfi_directive all_cfi_addresses blockID = cfg 
    | block_has_region_end region_ends blockID = cfg
    -- TODO or end of basic block has cfi_directive end is not start of another?
    | otherwise = remove_node blockID cfg


  block_has_region_end region_ends blockID = ((fromIntegral $ inAddress $ last (cfg_instrs cfg IM.! blockID)) + (fromIntegral $ inSize $ last (cfg_instrs cfg IM.! blockID))) `IS.member` region_ends || (any (\a -> a `IS.member` region_ends) $ map (fromIntegral . inAddress) $ drop 1 $ cfg_instrs cfg IM.! blockID)

  block_has_cfi_directive all_cfi_addresses blockID = any (address_has_cfi_directive all_cfi_addresses) $ map (fromIntegral . inAddress) $ cfg_instrs cfg IM.! blockID

  address_has_cfi_directive all_cfi_addresses a = a `IS.member` all_cfi_addresses || IM.lookup a (cfi_directives $ binary_get_cfi bin) /= Nothing


  remove_node blockID cfg = 
    let parents  = IS.toList $ IS.delete blockID $ intgraph_pre cfg blockID
        children = IS.toList $ IS.delete blockID $ intgraph_post cfg blockID
        prod     = [(x,y) | x <- parents, y <- children]
        cfg0     = delete_node blockID cfg in
      foldr add_new_edge cfg0 prod

  add_new_edge (parent,child) cfg  = cfg { cfg_edges = IM.insertWith IS.union parent (IS.singleton child) (cfg_edges cfg) }

  delete_node blockID cfg = cfg { cfg_edges = IM.map (IS.delete blockID) $ IM.delete blockID $ cfg_edges cfg, cfg_blocks = IM.delete blockID $ cfg_blocks cfg }




relevant_calls l cfg blockID = get_call_target (cfg_instrs cfg IM.! blockID)
 where
  get_call_target instrs
    | isCall (inOperation $ last instrs) = concatMap get_relevant_call $ get_known_jump_targets l $ last instrs
    | isJump (inOperation $ last instrs) && jump_is_actually_a_call l (last instrs) = concatMap get_relevant_call $ get_known_jump_targets l $ last instrs
    | otherwise = []

  get_relevant_call (External f) = if (f `notElem` ["__cxa_finalize", "__cxa_allocate_exception", "__cxa_free_exception"] && "__cxa_" `isPrefixOf` f) || f `elem` ["_Unwind_Resume", "_Unwind_RaiseException"] then [f] else []
  get_relevant_call _ = []

