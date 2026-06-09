{-# LANGUAGE PartialTypeSignatures, DeriveGeneric#-}

module InputLifting.NextRips where

import Base
import Config
import Conventions

import qualified Data.JumpTarget as JT
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

import WithNoAbstraction.Pointers (expr_is_global_immediate)

import InputLifting.SymbolicExecution
import InputLifting.Types

import Data.Word
import Data.List
import Data.List.Extra (firstJust)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS


import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Extra




import GHC.Generics
import Debug.Trace



-- Explore an unresolved CALL
-- Do under-constrained symbolic execution to see if that resolves the CALL.
next_rips_unresolved_call :: BinaryClass bin => InstructionAddress -> XLifting bin Next
next_rips_unresolved_call a = do
  Just entry <- get_entry_of_instruction a
  p <- mk_path_upwards_folded_calls entry a
  case p of
    Nothing   -> do
      Just i <- fetch a
      modify $ register_resolving a $ UnresolvedCall
      snd <$> (unresolved_indirection i $ Bottom RockBottom)
    Just path -> do
      instrs'      <- mapM fetch $ map InstructionAddress path
      let instrs    = map fromJust instrs' 
      ctxt@(bin,_) <- ask
      (_,_,ss,_,_) <- liftIO $ symbolically_execute_path ctxt False False (init instrs) init_symstate


      let i         = last instrs
      let sem       = instr_to_semantics ctxt i
      let [SE_StatePart op Nothing] = operands_of sem
      let v         = resolve_operands ctxt False sem ss M.! op
      (is_resolved,nxt) <- resolve_target_based_on_symbolic_value bin i v
      modify $ register_resolving a $ if is_resolved then ResolvedCall nxt else UnresolvedCall
      return nxt

-- Explore an unresolved CALL to ERROR
-- Do under-constrained symbolic execution to see if tit returns or terminates
next_rips_error :: BinaryClass bin => InstructionAddress -> XLifting bin Next
next_rips_error a = do
  Just entry   <- get_entry_of_instruction a
  Just path    <- mk_path_upwards_folded_calls entry a
  instrs'      <- mapM fetch $ map InstructionAddress path
  let instrs    = map fromJust instrs' 
  ctxt@(bin,_) <- ask
  (_,_,ss,_,_) <- liftIO $ symbolically_execute_path ctxt False False (init instrs) init_symstate


  let i         = last instrs
  let sem       = instr_to_semantics ctxt i
  let op        = SP_Reg $ Reg64 RDI
  let v         = resolve_operands ctxt False sem ss M.! op

  case v of
    [SE_Immediate imm] -> do 
      if imm == 0 then do
        xtoLog $ "Resolved error() operand at " ++ show a ++ ": it returns"
        modify $ register_resolving a $ ResolvedCallToError True
        return $ NxtAddresses (Just "error") $ S.singleton $ instruction_next_address i
      else do 
        xtoLog $ "Resolved error() operand at " ++ show a ++ ": it terminates."
        modify $ register_resolving a $ ResolvedCallToError False
        return $ NxtTerminal $ Just "error"
    _ -> do
        xtoLog $ "Unesolved error() operand at " ++ show a ++ ": RDI == " ++ show v
        modify $ register_resolving a $ UnresolvedCallToError
        return $ NxtAddresses (Just "error") $ S.singleton $ instruction_next_address i
    

-- Explore an unresolved JUMP
-- Do under-constrained symbolic execution to see if that resolves the JUMP.
next_rips_unresolved_jump :: BinaryClass bin => InstructionAddress -> XLifting bin Next
next_rips_unresolved_jump a = do
  Just entry <- get_entry_of_instruction a
  p <- mk_path_upwards_folded_calls entry a
  case p of
    Nothing   -> do
      modify $ register_resolving a UnresolvedJump
      return $ NxtReturn Nothing -- TODO, try finding shorter paths, also for calls
    Just path -> use_path_to_resolve_jump a path

use_path_to_resolve_jump :: BinaryClass bin => InstructionAddress -> [Int] -> XLifting bin Next
use_path_to_resolve_jump a path = do
  ctxt@(bin,_) <- ask
  Just i       <- fetch a
  instrs'      <- mapM fetch $ map InstructionAddress path
  let instrs    = map fromJust instrs' 
  attempt      <- try_find_jump_table_computation 0 a instrs
  case attempt of
    (v,errmsg,Nothing) -> do
      xDebug 0 $ "Not a jump table: " ++ show i
      (is_resolved,nxt0) <- resolve_target_based_on_symbolic_value bin i v
      let nxt = case nxt0 of
                  NxtTerminal f            -> NxtTerminal f
                  NxtInternalCall trgt     -> NxtAddresses Nothing $ S.singleton trgt
                  NxtAddresses (Just f) as -> NxtReturn $ Just f
                  NxtAddresses Nothing as  -> NxtReturn Nothing -- unresolved, TODO report
                  _ -> error $ show nxt
      modify $ register_resolving a $ if is_resolved then ResolvedJump nxt else UnresolvedJump
      return nxt
    (v,errmsg,Just (idx,ss)) -> do
      let invs = symstate_invs ss
      bnd <- (return $ firstJust (find_bound idx) invs) `orTryM` (return $ bound_inherited_from_expr idx) `orElseM` failed_to_find_bound idx errmsg
      when (bnd >= 10000) $ error $ "Bound too large: " ++ show (idx,bnd) ++ errmsg
      case find_base ctxt $ head v of
        Nothing -> do
          modify $ register_resolving a $ UnresolvedJumpTable errmsg
          -- error $ "ERROR: Cannot find base!" ++ errmsg  -- TODO report error
          return $ NxtReturn Nothing
        Just base -> do
          let vs_ordered = map (\bnd -> evalState (sresolve_expr ctxt False (substE remove_take_bits idx (SE_Immediate bnd) (head v))) ss)  [0..bnd]
          let vs = S.toList $ S.fromList vs_ordered
          if all (expr_is_global_immediate bin) vs then do
            xtoLog $ "Jump table @0x" ++ showHex (inAddress i) ++ ", bound " ++ show bnd ++ ", base 0x" ++ showHex base  ++ " and targets " ++ show vs
            modify $ register_resolving a $ ResolvedJumpTable base bnd
            return $ NxtAddresses Nothing $ S.fromList $ map (\(SE_Immediate imm) -> InstructionAddress $ fromIntegral imm) vs
          else do
            modify $ register_resolving a $ UnresolvedJumpTable errmsg
            xtoLog $ "Jump table @0x" ++ showHex (inAddress i) ++ " with index " ++ show idx ++ ", bound " ++ show bnd ++ ", base 0x" ++ showHex base  ++ " and targets " ++ show (zip [0..bnd] vs_ordered) ++ " --> " ++ show (substE remove_take_bits idx (SE_Immediate 0x42) (head v))
            error $ "ERROR: Jump table has invalid targets!" ++ errmsg
 where
  mock_op = Op_Reg (Reg64 RAX) []

  try_find_jump_table_computation :: BinaryClass bin => Int -> InstructionAddress -> [Instruction] ->
                                     XLifting bin ([SimpleExpr], String, Maybe (SimpleExpr, SymState))
  try_find_jump_table_computation attemptNr a instrs = do
    ctxt@(bin,_) <- ask
    (_,_,ss,_,r) <- liftIO $ symbolically_execute_path ctxt True (attemptNr == 2) (init instrs) init_symstate

    let i         = last instrs
    let sem       = instr_to_semantics ctxt i
    let [SE_StatePart op Nothing] = operands_of sem
    let v         = resolve_operands ctxt False sem ss M.! op
    let errmsg    = "\n" ++ intercalate "\n"
                    [ "Symbolically executing:"
                    , show (head instrs) ++ " --> " ++ show (last instrs)
                    , "Produces:"
                    , r ]
    if length v > 1 then error $ show v else return ()
    
    case find_jump_table_index $ head v of
      Nothing  -> 
        if attemptNr == 0 then do
          let instrs' = reverse $ takeWhile (not . loadsImmediate) $ reverse instrs
          if head instrs' /= head instrs then
            try_find_jump_table_computation 1 a instrs'
          else
            try_find_jump_table_computation 2 a instrs
        else if attemptNr == 1 then do
          try_find_jump_table_computation 2 a instrs
        else
          return (v,errmsg,Nothing)
      Just idx -> return (v,errmsg,Just (idx,ss))



  loadsImmediate (Instruction _ _ MOV [_,Op_Imm _] _ _) = True
  loadsImmediate _ = False

  find_jump_table_index (SE_Op Plus _ es)      = firstJust find_jump_table_index es
  find_jump_table_index (SE_SExtend _ _ e)     = find_jump_table_index e
  find_jump_table_index (SE_Var (SP_Mem a si)) = find_jump_table_index_in_address a
  find_jump_table_index _                      = Nothing

  find_jump_table_index_in_address (SE_Op Plus _ es)                   = firstJust find_jump_table_index_in_address es
  find_jump_table_index_in_address (SE_Op Times _ [e0,SE_Immediate 4]) = Just $ remove_take_bits e0 -- TODO hard coded 4
  find_jump_table_index_in_address (SE_Op Times _ [SE_Immediate 4,e0]) = Just $ remove_take_bits e0
  find_jump_table_index_in_address _                                   = Nothing

  find_base ctxt (SE_Op Plus _ es) = firstJust (get_base ctxt) es
  find_base ctxt _                 = Nothing

  get_base (bin,_) e@(SE_Immediate imm)
    | expr_is_global_immediate bin e = Just imm
    | otherwise = Nothing
  get_base _ _ = Nothing

  failed_to_find_bound idx errmsg = do
    xtoLog $ "ERROR: Cannot find bound! Index = " ++ show idx ++ "\n" ++ errmsg 
    return 1

  -- normalize: remove take_bits and do not worry about when the memory was read
  remove_take_bits (SE_Bit _ e)                   = remove_take_bits e
  remove_take_bits (SE_Op op si es)               = SE_Op op si $ map remove_take_bits es
  remove_take_bits (SE_StatePart (SP_Mem a si) _) = SE_StatePart (SP_Mem (remove_take_bits a) si) Nothing
  remove_take_bits e                              = e


  find_bound (SE_Op Minus _ [a,SE_Immediate i]) inv = (\bnd -> bnd - i) <$> find_bound a inv
  find_bound (SE_Op Shr   _ [a,SE_Immediate i]) inv = (\bnd -> bnd `quot` 2^i) <$> find_bound a inv
  find_bound idx ("<=", e0,SE_Immediate imm)
    | simp' (remove_take_bits e0) == idx = Just imm
    | otherwise = Nothing
  find_bound idx ("<", e0,SE_Immediate imm)
    | imm /= 0 && simp' (remove_take_bits e0) == idx = Just (imm + 1)
    | otherwise = Nothing
  find_bound idx ("<=", e0,e1) = Nothing
  find_bound idx (">",_,_) = Nothing
  find_bound idx (">=",_,_) = Nothing
  find_bound idx (cmp,e0,e1) = error $ show (idx,cmp,e0,e1)

  bound_inherited_from_expr (SE_Op Plus  _  [a,SE_Immediate imm]) = ((+) imm) <$> bound_inherited_from_expr a
  bound_inherited_from_expr (SE_Op Times _  [a,SE_Immediate imm]) = ((*) imm) <$> bound_inherited_from_expr a
  bound_inherited_from_expr (SE_Op Xor   _  [a,SE_Immediate imm]) = max imm <$> bound_inherited_from_expr a
  bound_inherited_from_expr (SE_Op Shr   si [SE_Var (SP_Mem a 1),SE_Immediate imm]) = Just $ 2 ^(8-imm) - 1
  bound_inherited_from_expr (SE_Op Shr   si [a,SE_Immediate imm])
    | fromIntegral imm < si && si-fromIntegral imm < 5 = Just $ 2^(si-fromIntegral imm) - 1
    | otherwise                                        = Nothing
  bound_inherited_from_expr (SE_Op And   _  [a,SE_Immediate imm])
    | isPower2Minus1 imm = Just imm
    | otherwise          = Nothing
  bound_inherited_from_expr (SE_Bit n a)                         = min (2^n-1) <$> (bound_inherited_from_expr a)

  bound_inherited_from_expr _                                    = Nothing




resolve_target_based_on_symbolic_value bin i v@[SE_Immediate imm]
  | address_has_instruction bin imm = do
    xtoLog $ "Resolved operand of " ++ show i ++ " to 0x" ++ showHex imm
    return $ (True,NxtInternalCall $ InstructionAddress $ fromIntegral imm)
  | otherwise = do
    -- TODO check if is external symbol and if exitting call
    -- xtoLog $ "Resolved operand of " ++ show i ++ " to 0x" ++ showHex imm
    -- error $ show i ++ " --> " ++ showHex imm
    unresolved_indirection i v
resolve_target_based_on_symbolic_value bin i v@[SE_Var (SP_Mem (SE_Immediate a) si)] = try_resolve_symbol_at i a si `orElseM` unresolved_indirection i v
resolve_target_based_on_symbolic_value bin i v = unresolved_indirection i v


try_resolve_symbol_at i a si = do
  ctxt@(bin,_) <- ask
  case IM.lookup (fromIntegral a) $ binary_get_symbol_table bin of
    Just (PointerToInternalFunction f a1)       -> do
      xtoLog $ "Resolved operand of " ++ show i ++ " to internal function 0x" ++ showHex a1
      return $ Just (True, NxtInternalCall $ InstructionAddress $ fromIntegral a1)
    Just (PointerToExternalFunction f)          -> do
      trgt <- next_rip_external_function i f
      xtoLog $ "Resolved operand of " ++ show i ++ " to external function " ++ f
      return $ Just (True, trgt)
    Just (Relocated_ResolvedObject o a1 addend) -> error "TODO" -- internal? external? Just $ SE_Immediate $ fromIntegral $ fromIntegral a1 + addend
    Just (PointerToObject f True _ _)           -> error "TODO" -- Just $ SE_Var $ SP_Mem (SE_Immediate a) si
    Just (AddressOfObject l b)                  -> do
      trgt <- next_rip_external_function i l
      xtoLog $ "Resolved operand of " ++ show i ++ " to function stored at " ++ l
      return $ Just (True, trgt)
    -- Just x -> error $ show i ++ ": " ++ show_symbol_table_entry (a,x)
    _ -> return Nothing

unresolved_indirection i v = do
  -- xtoLog $ "WARNING: Cannot resolve " ++ show i ++ " resolves to " ++ show v
  return $ (False, NxtAddresses Nothing $ S.singleton $ instruction_next_address i)




next_rips :: BinaryClass bin => Instruction -> XLifting bin Next
next_rips i = do
  (bin,_) <- ask
  let a = InstructionAddress $ fromIntegral $ inAddress i
  nxt <- get_next_rips a
  case nxt of
    Just nxt -> return nxt
    Nothing  -> do
      nxt <- next_rip_based_on_opcode bin i a $ inOperation i
      modify $ register_next_rips a nxt
      return nxt
 where
  next_rip_based_on_opcode bin i a op
    | isHalt op    = return $ NxtTerminal Nothing
    | isSyscall op = return $ NxtSyscall
    | isRet op     = return $ NxtReturn Nothing
    | isJump op    = do
      case jump_target_for_instruction bin i of
        JT.ImmediateAddress a' -> return $ NxtAddresses Nothing $ S.singleton $ InstructionAddress $ fromIntegral a'
        JT.External sym        -> if is_exiting_function_call sym then return $ NxtTerminal $ Just sym else if sym == "error" then jump_to_error <$> next_rips_error a else return $ NxtReturn $ Just sym
        JT.Unresolved          -> next_rips_unresolved_jump (InstructionAddress $ fromIntegral $ inAddress i)
    | isCondJump op =
      case jump_target_for_instruction bin i of
        JT.ImmediateAddress a' -> return $ NxtAddresses Nothing $ S.fromList [ InstructionAddress $ fromIntegral a', instruction_next_address i ]
        JT.External sym        -> return $ NxtAddresses (Just sym) $ S.fromList [ instruction_next_address i ]
        trgt                   -> error $ show i ++ " --> " ++ show trgt
    | isCall op     =
      case jump_target_for_instruction bin i of
        JT.ImmediateAddress a'          -> return $ NxtInternalCall $ InstructionAddress $ fromIntegral a'
        JT.External sym                 -> next_rip_external_function i sym
        JT.ExternalDeref sym            -> next_rip_external_function i sym
        JT.Unresolved                   -> next_rips_unresolved_call (InstructionAddress $ fromIntegral $ inAddress i)
        x                               -> error $ show i ++ ", " ++ show x
    | otherwise = return $ NxtAddresses Nothing $ S.singleton $ instruction_next_address i

  jump_to_error (NxtTerminal f)     = NxtTerminal f
  jump_to_error (NxtAddresses f as) = NxtReturn f

next_rip_external_function i sym
  | is_exiting_function_call sym = return $ NxtTerminal $ Just sym
  | sym == "error"               = next_rips_error $ InstructionAddress $ fromIntegral $ inAddress i
  | otherwise                    = return $ NxtAddresses (Just sym) $ S.singleton $ instruction_next_address i




instruction_next_address i = InstructionAddress $ fromIntegral (inAddress i) + inSize i




mk_path_upwards_folded_calls :: BinaryClass bin => FunctionEntry -> InstructionAddress -> XLifting bin (Maybe [Int])
mk_path_upwards_folded_calls entry a = fst <$> go IS.empty a
 where
  go visited a 
    | toInt a `IS.member` visited = return (Nothing,visited)
    | otherwise = do
      parents <- get_prev_collapsed_calls entry a 
      if S.null parents then
        return (Just [toInt a], visited)
      else do
        let visited' = IS.insert (toInt a) visited
        go' visited' a $ S.toList parents

  go' visited a [] = return (Nothing,visited)
  go' visited a (parent:parents) = do
    (path,visited') <- go visited parent
    case path of
      Nothing -> go' visited' a parents
      Just p  -> return (Just $ p ++ [toInt a], visited')




-- The edges of a CFG within a function (forward). CALLs are collapsed (i.e., skip them and go the instruction address right after them)
get_next_collapsed_calls :: BinaryClass bin => S.Set FunctionEntry -> InstructionAddress -> XLifting bin (S.Set FunctionEntry,S.Set InstructionAddress)
get_next_collapsed_calls entries a = do
  fmap <- gets current_fmap 
  instrs <- gets current_instrs
  g <- gets current_cfg
  let i  = instrs IM.! toInt a
  let op = inOperation i
  if isRet op then
    return (S.empty,S.empty)
  else do
    nxt <- if isCall op then
             maybe_fall_through a
           else
             return $ S.fromList $ map InstructionAddress $ IS.toList $ xgraph_children g $ toInt a

    let (fromEntry,others) = S.partition (is_from_current_entry fmap) nxt
    let (_,notFromEntry) = S.partition (\a1 -> not $ toInt a1 `IM.member` fmap) others

    expandToNotFromEntry <- if isJump op || S.null notFromEntry then
                              return S.empty
                            else if isCondJump op then do
                              Just entry' <- get_entry_of_instruction $ instruction_next_address i
                              return $ if entry' `S.member` entries then S.empty else S.singleton $ instruction_next_address i
                            else
                              return notFromEntry
    if S.null expandToNotFromEntry then
      return (S.empty,fromEntry)
    else do
      let new_entries = S.map (\a -> fmap IM.! toInt a) expandToNotFromEntry
      xDebug 0 $ "Expanding entries " ++ show entries ++ " with " ++ show new_entries ++ " because of crossing control flow at " ++ show a
      return (new_entries,fromEntry)
 where
  is_from_current_entry fmap a1 =
    case IM.lookup (toInt a1) fmap of
      Nothing -> False
      Just entry1 -> entry1 `S.member` entries

  maybe_fall_through a0 = do
    instrs <- gets current_instrs
    let i0 = instrs IM.! toInt a0
    nxt   <- next_rips i0
    case nxt of
      NxtInternalCall trgt -> ifM (comes_from_ret $ instruction_next_address i0) (return $ S.singleton $ instruction_next_address i0) (return S.empty)
      NxtAddresses f as    -> return as 
      NxtTerminal f        -> return S.empty

  comes_from_ret :: BinaryClass bin => InstructionAddress -> XLifting bin Bool
  comes_from_ret a = do 
    g <- gets current_cfg
    let parents = IS.toList $ xgraph_parents g $ toInt a
    instrs     <- mapM fetch $ map InstructionAddress parents
    anyM instructionIsReturn $ map fromJust instrs





-- The edges of a CFG within a function (backward). CALLs are collapses (i.e., skip them and go the instruction address right before them)
get_prev_collapsed_calls :: BinaryClass bin => FunctionEntry -> InstructionAddress -> XLifting bin (S.Set InstructionAddress)
get_prev_collapsed_calls entry a = do
  cfg         <- gets current_cfg
  let parents0 = xgraph_parents cfg (toInt a) 
  parents1    <- mapM fix_RET $ map InstructionAddress $ IS.toList parents0 
  parents2    <- filterM is_from_current_entry $ S.toList $ S.unions parents1
  return $ S.fromList parents2
 where
  is_from_current_entry a0 = do
    entry0 <- get_entry_of_instruction a0
    return $ entry0 == Just entry

  fix_RET parent = do 
    Just i0 <- fetch parent
    is_return <- instructionIsReturn i0
    if is_return then do
      instrs <- gets current_instrs
      case IM.lookupLT (toInt a) instrs of
        Just (a0,i0) -> do
          if (isCall $ inOperation i0) && instruction_next_address i0 == a then do
            return $ S.singleton $ InstructionAddress a0
          else
            error $ show (show parent,show a, show a0)
    else if toInt a == toInt entry then
      return $ S.empty
    else if isCall $ inOperation i0 then do
      (bin,_) <- ask
      case jump_target_for_instruction bin i0 of
        JT.ImmediateAddress a' -> return $ S.empty -- error $ "TODO?" ++ show i0 ++ "@" ++ showHex a ++ " --> " ++ showHex a' ++ " in " ++ show entry
        _                      -> return $ S.singleton parent
    else
      return $ S.singleton parent

instructionIsReturn i = do
  nxt <- next_rips i
  case nxt of
    NxtReturn _ -> return True
    _ -> return False



