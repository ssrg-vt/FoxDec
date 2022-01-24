{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, DeriveGeneric, DefaultSignatures, FlexibleContexts, StrictData #-}



{-|
Module      : MachineState
Description : Functions for resolving symbolic memory reads and writes.

These functions are defined using the @State (Pred,VCS)@ monad.
Both the read- and write function may update the current predicate, as well as introduce new verification conditions.
-}
module MachineState (
  read_reg,
  write_reg,
  read_mem,
  write_mem,
  read_operand,
  write_operand,
  read_sp,
  write_sp,
  invalid_bottom_pointer,
  address_is_unwritable,
  resolve_address
 )
 where

import Base
import SimplePred
import Context
import Conventions
import X86_Datastructures
import ControlFlow
import Pointers

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Word 
import Data.Traversable (for)
import Control.Monad.State.Strict hiding (join)
import Control.Monad.Extra (partitionM,mapMaybeM)
import Data.List
import Data.Maybe (mapMaybe,fromJust,catMaybes)
import Debug.Trace
import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)










-- *  Registers
read_rreg :: Register -> State (Pred,VCS) SimpleExpr
read_rreg r = do
  (Predicate eqs flg muddle_status, vcs) <- get
  case M.lookup (SP_Reg r) eqs of
    Nothing -> do
      let var = SE_Var (SP_Reg r)
      put $ (Predicate (M.insert (SP_Reg r) var eqs) flg muddle_status, vcs)
      return var 
    Just e  -> return e

-- | Read from a register
read_reg :: Context -> Register -> State (Pred,VCS) SimpleExpr
read_reg ctxt r = do
  let rr = real_reg r
  v <- read_rreg rr
  if reg_size r == 8 then -- 64 bit 
    return v
  else if reg_size r == 4 then -- 32 bit
    return $ simp $ SE_Bit 32 v
  else if r `elem` reg16 then -- 16 bit
    return $ simp $ SE_Bit 16 v
  else if r `elem` reg8 then -- 8 bit low registers
    return $ simp $ SE_Bit 8 v
  else
    case v of
      SE_Immediate imm -> return $ SE_Immediate imm
      e -> return $ Bottom (FromBitMode $ srcs_of_expr ctxt e)



write_rreg :: Register -> SimpleExpr -> State (Pred,VCS) () 
write_rreg r e = 
  if take 2 (show r) == "ST" then
    return ()
  else 
    modify do_write
 where
  do_write (Predicate eqs flg muddle_status,vcs) =
    let eqs' = M.insert (SP_Reg r) (trim_expr $ simp e) eqs
        flg' = clean_flg (SP_Reg r) flg in
      (Predicate eqs' flg' muddle_status,vcs)
 
-- | Write to a register
write_reg :: Context -> Register -> SimpleExpr -> State (Pred,VCS) ()
write_reg ctxt r v = do
  if reg_size r == 8 then -- 64 bit
    write_rreg r $ simp v
  else if reg_size r == 4 then -- 32 bit
    write_rreg (real_reg r) (simp $ SE_Bit 32 v)
  else if r `elem` reg16 then do -- 16 bit 
    let rr = real_reg r
    curr_v <- read_rreg rr
    write_rreg rr (simp $ SE_Overwrite 16 curr_v (SE_Bit 16 v))
  else if r `elem` reg8 then do -- 8 bit low registers
    let rr = real_reg r
    curr_v <- read_rreg rr
    write_rreg rr (simp $ SE_Overwrite 8 curr_v (SE_Bit 8 v))
  else
    write_rreg (real_reg r) $ Bottom (FromBitMode $ srcs_of_expr ctxt v)


-- * Flags 


-- If the given StatePart is overwritten, does that taint the current flag status?
-- If so, set flagstatus to None, otherwise keep it.
clean_flg :: StatePart -> FlagStatus -> FlagStatus
clean_flg sp None               = None
clean_flg sp (FS_CMP b op1 op2) = do
  if is_tainted op1 || is_tainted op2 then
    None
  else
    FS_CMP b op1 op2
 where
  is_tainted (Reg r)       = sp == SP_Reg (real_reg r)
  is_tainted (Immediate _) = False
  is_tainted (Address a)   =
    case sp of
      SP_Mem _ _ -> True
      _          -> False





-- * Memory 

-- | Given the address of an operand of an instruction, resolve it given the current state.
resolve_address :: Context -> Address -> State (Pred,VCS) SimpleExpr
resolve_address ctxt (FromReg r)       = read_reg ctxt r
resolve_address ctxt (AddrImm i)       = return $ SE_Immediate $ fromIntegral i
resolve_address ctxt (AddrMinus a0 a1) = do
  ra0 <- resolve_address ctxt a0 
  ra1 <- resolve_address ctxt a1
  return $ simp $ SE_Op (Minus 64) [ra0,ra1]
resolve_address ctxt (AddrPlus a0 a1) = do
  ra0 <- resolve_address ctxt a0 
  ra1 <- resolve_address ctxt a1
  return $ simp $ SE_Op (Plus 64) [ra0,ra1]
resolve_address ctxt (AddrTimes a0 a1) = do
  ra0 <- resolve_address ctxt a0 
  ra1 <- resolve_address ctxt a1
  return $ simp $ SE_Op (Times 64) [ra0,ra1]
resolve_address ctxt (SizeDir _ a) = resolve_address ctxt a


-- | Can we assume that the two given symolic addresses are separate, and add a precondition?
-- Returns true iff the expressions do not contain bottom.
-- This function is called only after other checks have alreayd been done.
is_preconditionable ctxt a0 a1 = not (contains_bot a0) && not (contains_bot a1)

-- | Add a precondition to the given symbolic predicate
add_precondition a0 si0 a1 si1 (p,vcs)   = (p,S.insert (Precondition a0 si0 a1 si1) vcs)



is_assertable ctxt a0 a1 =
  let srcs0     = srcs_of_expr ctxt a0
      srcs1     = srcs_of_expr ctxt a1 
      is_local0 = rsp `S.member` srcs0
      is_local1 = rsp `S.member` srcs1 in
  or [
    contains_bot a0 && is_local0 && not (contains_bot a1) && is_local1, -- TODO this can be more precise
    
    not is_local0 && is_local1, 
    not is_local1 && is_local0, 

    pointers_from_different_global_section ctxt a0 a1
  ]
 where
  rsp = Src_Var $ SP_Reg $ RSP

-- | Add an assertion to the given symbolic predicate
add_assertion rip a0 si0 a1 si1 (p,vcs)  = (p,S.insert (Assertion rip a0 si0 a1 si1) vcs)



generate_assertion :: Context -> Maybe Address -> SimpleExpr -> State (Pred,VCS) SimpleExpr
generate_assertion ctxt Nothing a0           = return a0
generate_assertion ctxt (Just (FromReg r)) _ = do
  p <- get
  v <- read_reg ctxt r
  a <- do
    if contains_bot v then
      --if all_bot_satisfy (\typ srcs -> typ `elem` [FromSemantics] && all (not . is_function_src) srcs) v then
      --  error $ show (r,v,p)
      --else
        return $ SE_StatePart $ SP_Reg r
    else
      return $ v
  return $ a
generate_assertion ctxt (Just (AddrImm i))       _ = return $ SE_Immediate $ fromIntegral i
generate_assertion ctxt (Just (AddrMinus a0 a1)) x = do
  v0 <- generate_assertion ctxt (Just a0) x
  v1 <- generate_assertion ctxt (Just a1) x
  return $ simp $ SE_Op (Minus 64) [v0,v1]
generate_assertion ctxt (Just (AddrPlus a0 a1) ) x = do
  v0 <- generate_assertion ctxt (Just a0) x
  v1 <- generate_assertion ctxt (Just a1) x
  return $ simp $ SE_Op (Plus 64) [v0,v1]
generate_assertion ctxt (Just (AddrTimes a0 a1)) x = do
  v0 <- generate_assertion ctxt (Just a0) x
  v1 <- generate_assertion ctxt (Just a1) x
  return $ simp $ SE_Op (Times 64) [v0,v1]
generate_assertion ctxt (Just (SizeDir _ a))     x = generate_assertion ctxt (Just a) x
  



-- | An address is considered "unwritable" only if it is an immediate address that belongs to a section that is considered unwritable
-- according to Conventions (see 'section_is_unwritable')
address_is_unwritable ctxt (SE_Immediate a) =
  case find_section_for_address ctxt $ fromIntegral a of
    Nothing -> False
    Just (segname,sectname,_,_) -> section_is_unwritable (segname,sectname)
address_is_unwritable ctxt _ = False





-- | Returns true if a pointer is not suitable for writing to memory.
-- This may happen if the symbolic expression provides no information, i.e., it has @Bottom@ without known pointerbases,
-- and without any sources.
invalid_bottom_pointer ctxt e = not (is_immediate e)  && S.null (get_known_pointer_bases ctxt e) && S.null (srcs_of_expr ctxt e) 

-- | Assuming a valid pointer (see 'invalid_bottom_pointer'), produce a list of pointers that are to be written to in the memory.
expr_to_mem_addresses ctxt a si =
  if not (contains_bot a) then
    [(a,si)]
  else let bs = get_known_pointer_bases ctxt a in
    if not $ S.null bs then
      map (\bs -> (Bottom $ FromPointerBases $ S.fromList bs, 1)) $ split_per_base $ S.toList bs
    else
      map srcs_to_mem_address $ split_per_source $ S.toList $ srcs_of_expr ctxt a
 where
  srcs_to_mem_address [Src_Var (SP_Reg RSP)] = (Bottom $ FromPointerBases $ S.singleton $ StackPointer,1)
  srcs_to_mem_address [Src_Malloc i h]       = (Bottom $ FromPointerBases $ S.singleton $ Malloc i h,1)
  srcs_to_mem_address srcs                   = (Bottom $ FromSources $ S.fromList srcs,1)
    
  split_per_base [] = []
  split_per_base (bs:bss) =
    let (separate,overlapping) = partition (pointer_bases_separate bs) bss in
      (bs:overlapping) : split_per_base separate

  split_per_source [] = []
  split_per_source (src:srcs) =
    let (separate,overlapping) = partition (sources_separate src) srcs in
      (src:overlapping) : split_per_source separate






data SeparationStatus = Alias | Separated | Enclosed | Disclosed | Overlap
  deriving Eq


read_from_address :: Context -> Maybe Address -> SimpleExpr -> Int -> State (Pred,VCS) SimpleExpr
read_from_address ctxt address a si0 = do
  let as = map simp $ unfold_non_determinism a
  vs <- mapM read_from_address' as
  return $ join_exprs ("Read") ctxt vs
 where
  read_from_address' (SE_Immediate imm) = 
    case read_from_datasection ctxt imm si0 of 
      Just imm -> -- trace ("Read immediate from datasection: " ++ show (a0,si0) ++ " := " ++ show imm) $ return $ SE_Immediate imm
                  return $ SE_Immediate imm  
      Nothing  -> read_from_state $ SE_Immediate imm
  read_from_address' a0 = read_from_state a0


  read_from_state :: SimpleExpr -> State (Pred,VCS) SimpleExpr
  read_from_state a0 = do
    p@(Predicate eqs flg muddle_status,vcs) <- get
    if contains_bot a0 then
      if invalid_bottom_pointer ctxt a0 then do
        rip <- read_reg ctxt RIP
        trace ("READ FROM BASELESS POINTER @ " ++ show rip ++ ": " ++ show (a,si0)) $ return rock_bottom
      else do
        overlapping <- filterM (fmap not . has_separate_base a0) $ M.toList eqs

        if overlapping == [] then do
          let bot   = Bottom (FromUninitializedMemory $ srcs_of_expr ctxt a0)
          let sps'  = map (uncurry SP_Mem) $ expr_to_mem_addresses ctxt a0 si0
          let eqs'  = M.union (M.fromList $ map (\sp' -> (sp',bot)) sps') eqs
          put (Predicate eqs' flg muddle_status,vcs)
          return bot
        else do
          let bot = Bottom (FromOverlap $ srcs_of_exprs ctxt $ (a0 : map snd overlapping))
          return bot
    else if address_is_unwritable ctxt a0 then do
      let sp  = SP_Mem a0 si0
      let var = SE_Var sp
      return var   
    else do
      (Predicate eqs flg muddle_status,vcs) <- get
      do_read a0 $ M.toList eqs


  has_separate_base a0 (SP_Reg _,_)      = return True
  has_separate_base a0 (SP_Mem a1 si1,_) = do
    sep <- is_separate a0 si0 a1 si1
    return $ sep == Separated



  is_separate a0 si0 a1 si1 = do
    if si0 == si1 && necessarily_equal a0 a1 then
      return Alias
    else if necessarily_separate ctxt a0 si0 a1 si1 then
      return Separated
    else if necessarily_enclosed a0 si0 a1 si1 then
      --trace ("PRECONDITION (READ): ENCLOSURE BETWEEN " ++ show (a0,si0) ++ " and " ++ show (a1,si1))
       return Enclosed
    else if necessarily_enclosed a1 si1 a0 si0 then
      --trace ("PRECONDITION (READ): DISCLOSURE BETWEEN " ++ show (a0,si0) ++ " and " ++ show (a1,si1))
       return Disclosed
    else if is_preconditionable ctxt a0 a1 then do
      --trace ("PRECONDITION (READ): SEPARATION BETWEEN " ++ show (a0,si0) ++ " and " ++ show (a1,si1))
      modify $ add_precondition a0 si0 a1 si1
      return Separated
    else if is_assertable ctxt a0 a1 then do
      rip <- read_reg ctxt RIP
      assertion <- generate_assertion ctxt address a0
      modify $ add_assertion rip assertion si0 a1 si1
      --trace ("ASSERTION (READ@" ++ show rip ++ "): SEPARATION BETWEEN " ++ show (assertion,si0,a0) ++ " and " ++ show (a1,si1)) $
      return Separated
    else do
     if do_trace a0 a1 then trace ("PRECONDITION (READ): OVERLAP BETWEEN " ++ show (a0,si0) ++ " and " ++ show (a1,si1)) $ return Overlap else
       return Overlap

  do_trace a0 a1 = a0 /= a1 && a1 /= Bottom (FromPointerBases $ S.singleton StackPointer) && (expr_is_possibly_local_pointer ctxt a0 || expr_is_possibly_local_pointer ctxt a1)

  do_read :: SimpleExpr -> [(StatePart, SimpleExpr)] -> State (Pred,VCS) SimpleExpr
  do_read a0 [] = do
    (p,vcs) <- get
    let Predicate eqs flg muddle_status = p
    let sp  = SP_Mem a0 si0
    let var = --if muddle_status == Clean || -- TODO
                --   -- muddle_status == ExternalOnly then
                --SE_Var sp
                -- else 
                Bottom $ FromUninitializedMemory $ S.singleton (Src_Var sp)
    put (Predicate (M.insert sp var eqs) flg muddle_status,vcs)
    return var
  do_read a0 ((SP_Reg _,_):mem)       = do_read a0 mem
  do_read a0 ((SP_Mem a1 si1,e1):mem) = do
      sep <- is_separate a0 si0 a1 si1
      case sep of
        Alias     -> return $ e1
        Separated -> do_read a0 mem
        Enclosed  -> return $ Bottom (FromOverlap $ srcs_of_expr ctxt e1)
        _         -> do
          e0 <- do_read a0 mem
          let bot  = join_exprs ("read merge overlapping values2" ++ show (a0,si0,a1,si1)) ctxt $ filter ((/=) rock_bottom) [e0,e1]
          return $ bot 



-- | Read from memory
read_mem :: 
     Context  -- ^ The context
  -> Address  -- ^ The address of an operand of an instruction
  -> State (Pred,VCS) SimpleExpr
read_mem ctxt (SizeDir si a) = do
  resolved_address <- resolve_address ctxt a
  read_from_address ctxt (Just a) resolved_address si



add_unknown_mem_write mid (p,vcs) = (p,S.insert (SourcelessMemWrite mid) vcs)

-- | Write to memory
-- Each memory write is accomponied with a `MemWriteIdentifier` so that we can log memory writes to unknown locations.
--
write_mem :: 
  Context                -- ^ The context
  -> MemWriteIdentifier  -- ^ An identifier where the write occurs
  -> SimpleExpr          -- ^ The symbolic address
  -> Int                 -- ^ The size (in bytes)
  -> SimpleExpr          -- ^ The value to be written
  -> State (Pred,VCS) ()
write_mem ctxt mid a si0 v = do
  let as = map simp $ unfold_non_determinism a
  mapM_ (\a -> write_mem' a v) as -- TODO v should be bot?
 where
  write_mem' a0 v = do
    p@(Predicate eqs flg muddle_status,vcs) <- get

    if address_is_unwritable ctxt a0 then do
      trace ("Writing to unwritable section: " ++ show (a0,si0)) $ return ()
    else if invalid_bottom_pointer ctxt a0 && invalid_bottom_pointer ctxt a then do -- FORALL PATHS VS EXISTS PATH
      --error (show (a0,a,get_known_pointer_bases ctxt a0, srcs_of_expr ctxt a0, get_known_pointer_bases ctxt a, srcs_of_expr ctxt a))
      modify $ add_unknown_mem_write mid
    else do
      p@(Predicate eqs flg muddle_status,vcs) <- get
      eqs' <- M.fromList <$> do_write a0 si0 v (M.toList eqs)
      let flg' = clean_flg (SP_Mem a0 si0) flg
      put (Predicate eqs' flg' muddle_status,vcs)


  is_separate a0 si0 a1 si1 = do
    if si0 == si1 && necessarily_equal a0 a1 then
      return Alias
    else if necessarily_separate ctxt a0 si0 a1 si1 then
      --trace ("SEPARATION (WRITE) BETWEEN " ++ show (a0,si0,expr_to_addr_type ctxt a0) ++ " and " ++ show (a1,si1,expr_to_addr_type ctxt a1) ++ "\n") $
       return Separated
    else if necessarily_enclosed a0 si0 a1 si1 then do
      --trace ("ENCLOSURE1 (WRITE) BETWEEN " ++ show (a0,si0) ++ " and " ++ show (a1,si1)) $
       return Enclosed
    else if necessarily_enclosed a1 si1 a0 si0 then do
      --trace ("ENCLOSURE2 (WRITE) BETWEEN " ++ show (a0,si0) ++ " and " ++ show (a1,si1)) $
       return Disclosed
    else if is_preconditionable ctxt a0 a1 then do
      --trace ("PRECONDITION (WRITE): SEPARATION BETWEEN " ++ show (a0,si0,expr_to_addr_type ctxt a0) ++ " and " ++ show (a1,si1,expr_to_addr_type ctxt a1))
      modify $ add_precondition a0 si0 a1 si1
      return Separated
    else if is_assertable ctxt a0 a1 then do
      assertion <- generate_assertion ctxt (address mid) a0
      rip <- read_reg ctxt RIP
      modify $ add_assertion rip assertion si0 a1 si1
      --trace ("ASSERTION (WRITE@" ++ show rip ++ "): SEPARATION BETWEEN " ++ show (assertion,si0,a0) ++ " and " ++ show (a1,si1)) $
      return Separated
    else if a1 == SE_Var (SP_Reg RSP) then do
      rip <- read_reg ctxt RIP
      error ("ERROR (WRITE@" ++ show rip ++ "): OVERLAP BETWEEN " ++ show (si0,a0) ++ " and " ++ show (a1,si1))
    else
     if do_trace a0 a1 then trace ("PRECONDITION (WRITE): OVERLAP BETWEEN " ++ show (a0,si0) ++ " and " ++ show (a1,si1)) $ return Overlap else
       return Overlap

  address (MemWriteInstruction _ addr _) = Just addr
  address (MemWriteFunction _ _ _)       = Nothing



  do_trace a0 a1 = a0 /= a1 && a1 /= Bottom (FromPointerBases $ S.singleton StackPointer) && (expr_is_possibly_local_pointer ctxt a0 || expr_is_possibly_local_pointer ctxt a1)

  do_write :: SimpleExpr -> Int -> SimpleExpr -> [(StatePart, SimpleExpr)] -> State (Pred,VCS) [(StatePart, SimpleExpr)]
  do_write a0 si0 v []                      =  do
    let sps'  = map (uncurry SP_Mem) $ expr_to_mem_addresses ctxt a0 si0
    return $ map (\sp' -> (sp', trim_expr v)) sps'
  do_write a0 si0 v (eq@(SP_Reg _,_):mem)   = ((:) eq) <$> do_write a0 si0 v mem
  do_write a0 si0 v (eq@(SP_Mem a1 si1,e):mem) = do
    sep <- is_separate a0 si0 a1 si1
    case sep of
      Alias     -> return $ (SP_Mem a0 si0,trim_expr v) : mem
      Separated -> ((:) $ eq) <$> do_write a0 si0 v mem
      Enclosed  -> return $ (SP_Mem a1 si1,join_exprs ("write enclosure") ctxt [e,v]) : mem
      Disclosed -> do_write a0 si0 v mem
      Overlap   -> do_write (join_exprs ("write overlap adddres") ctxt [a0,a1]) si0 (join_exprs ("write overlap value") ctxt [e,v]) mem





-- * Operands  
-- | Read from an operand of an instruction
read_operand :: Context -> Operand -> State (Pred,VCS) SimpleExpr
read_operand ctxt (Reg r)       = read_reg ctxt r
read_operand ctxt (Immediate w) = return $ SE_Immediate w
read_operand ctxt (Address a)   = read_mem ctxt a

-- | Write to an operand of an instruction
write_operand ::
 Context                 -- ^ The context
 -> Int                  -- ^ The address of the instruction, used to build a `MemWriteIdentifier`
 -> Operand              -- ^ The operand
 -> SimpleExpr           -- ^ The value to be written
 -> State (Pred,VCS) ()
write_operand ctxt instr_a (Reg r) v = write_reg ctxt r v
write_operand ctxt instr_a (Address (SizeDir si a)) v = do
  resolved_address <- resolve_address ctxt a
  write_mem ctxt (MemWriteInstruction instr_a (SizeDir si a) resolved_address) resolved_address si v
write_operand ctxt instr_a op1 e = trace ("Writing to immediate operand: " ++ show (op1,e)) $ return ()-- Should not happen
 


-- * Stateparts 
-- | Read from an statepart
read_sp :: Context -> StatePart -> State (Pred,VCS) SimpleExpr
read_sp ctxt (SP_Reg r)    = simp <$> read_reg ctxt r
read_sp ctxt (SP_Mem a si) = simp <$> read_from_address ctxt Nothing a si

-- | Write to a statepart
write_sp :: 
  Context                               -- ^ The context
  -> (StatePart -> MemWriteIdentifier)  -- Given a statepart, build a memwrite identifier
  -> (StatePart,SimpleExpr)             -- The statepart and the value to be written
  -> State (Pred,VCS) ()
write_sp ctxt mk_mid (SP_Reg r,v)         = write_reg ctxt r v
write_sp ctxt mk_mid (sp@(SP_Mem a si),v) = write_mem ctxt (mk_mid sp) a si v



