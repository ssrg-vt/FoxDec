{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, DeriveGeneric, DefaultSignatures, FlexibleContexts, Strict #-}



{-|
Module      : MachineState
Description : Functions for resolving symbolic memory reads and writes.

These functions are defined using the @State (Pred,VCS)@ monad.
Both the read- and write function may update the current predicate, as well as introduce new verification conditions.
-}
module Data.MachineState (
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

import Analysis.Context
import Base
import Data.ControlFlow
import Data.Pointers
import Data.SimplePred
import X86.Conventions

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
import X86.Register (Register (..))
import qualified X86.Register as Reg
import Typeclasses.HasSize (sizeof)
import qualified X86.Address as X86
import qualified X86.Operand as X86
import Generic.Address (GenericAddress(..))
import Generic.Operand (GenericOperand(..))


-- *  Registers
read_rreg :: Register -> State (Pred,VCS) SimpleExpr
read_rreg r = do
  (Predicate eqs flg, vcs) <- get
  case M.lookup (SP_Reg r) eqs of
    Nothing -> do
      let var = SE_Var (SP_Reg r)
      put $ (Predicate (M.insert (SP_Reg r) var eqs) flg, vcs)
      return var 
    Just e  -> return e

-- | Read from a register
read_reg :: FContext -> Register -> State (Pred,VCS) SimpleExpr
read_reg ctxt r = do
  let rr = Reg.real r
  v <- read_rreg rr
  if sizeof r == 8 then -- 64 bit 
    return v
  else if sizeof r == 4 then -- 32 bit
    return $ simp $ SE_Bit 32 v
  else if r `elem` Reg.reg16 then -- 16 bit
    return $ simp $ SE_Bit 16 v
  else if r `elem` Reg.reg8 then -- 8 bit low registers
    return $ simp $ SE_Bit 8 v
  else
    case v of
      SE_Immediate imm -> return $ SE_Immediate imm
      e -> return $ Bottom (FromBitMode $ srcs_of_expr ctxt e)



write_rreg :: FContext -> MemWriteIdentifier -> Register -> SimpleExpr -> State (Pred,VCS) () 
write_rreg ctxt mid r e = 
  if take 2 (show r) == "ST" then
    return ()
  else do
    modify do_write
 where
  do_write (Predicate eqs flg,vcs) =
    let eqs' = M.insert (SP_Reg r) (trim_expr ctxt e) eqs
        flg' = clean_flg (SP_Reg r) flg in
      (Predicate eqs' flg',vcs)

get_immediate_forced (SE_Immediate imm) = fromIntegral imm
 
-- | Write to a register
write_reg :: FContext -> Word64 -> Register -> SimpleExpr -> State (Pred,VCS) ()
write_reg ctxt i_a r v = do
 let mid = mk_reg_write_identifier i_a r 
     sz  = sizeof r in
  if sz == 8 then -- 64 bit
    write_rreg ctxt mid r $ simp v
  else if sz == 4 then -- 32 bit
    write_rreg ctxt mid (Reg.real r) (simp $ SE_Bit 32 v)
  else if sz == 2 then do -- 16 bit 
    let rr = Reg.real r
    curr_v <- read_rreg rr
    write_rreg ctxt mid rr (simp $ SE_Overwrite 16 curr_v (SE_Bit 16 v))
  else if r `elem` Reg.reg8 then do -- 8 bit low registers
    let rr = Reg.real r
    curr_v <- read_rreg rr
    write_rreg ctxt mid rr (simp $ SE_Overwrite 8 curr_v (SE_Bit 8 v))
  else
    write_rreg ctxt mid (Reg.real r) $ Bottom (FromBitMode $ srcs_of_expr ctxt v)


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
  is_tainted (Storage r)          = sp == SP_Reg (Reg.real r)
  is_tainted (Immediate _)        = False
  is_tainted (EffectiveAddress _) = True -- TODO
  is_tainted (Memory a si)        =
    case sp of
      SP_Mem _ _ -> True
      _          -> False





-- * Memory 

-- | Given the address of an operand of an instruction, resolve it given the current state.
resolve_address :: FContext -> X86.Address -> State (Pred,VCS) SimpleExpr
resolve_address ctxt (AddressStorage r) = read_reg ctxt r
resolve_address ctxt (AddressImm i)     = return $ SE_Immediate $ fromIntegral i
resolve_address ctxt (AddressMinus a0 a1) = do
  ra0 <- resolve_address ctxt a0 
  ra1 <- resolve_address ctxt a1
  return $ simp $ SE_Op (Minus 64) [ra0,ra1]
resolve_address ctxt (AddressPlus a0 a1) = do
  ra0 <- resolve_address ctxt a0 
  ra1 <- resolve_address ctxt a1
  return $ simp $ SE_Op (Plus 64) [ra0,ra1]
resolve_address ctxt (AddressTimes a0 a1) = do
  ra0 <- resolve_address ctxt a0 
  ra1 <- resolve_address ctxt a1
  return $ simp $ SE_Op (Times 64) [ra0,ra1]


-- | Can we assume that the two given symbolic addresses are separate, and add a precondition?
-- Returns true iff the expressions do not contain bottom.
-- This function is called only after other checks have alreayd been done.
is_preconditionable ctxt a0 a1 = not (contains_bot a0) && not (contains_bot a1)

-- | Add a precondition to the given symbolic predicate
add_precondition a0 si0 a1 si1 (p,vcs)   = (p,S.insert (Precondition a0 si0 a1 si1) vcs)


-- | Can we assume that the two given symbolic addresses are separate, and add a assertion?
-- Even if two pointers contain bottom, we might still just assume (assert) they are separate based on their sources.
--
-- 1. If we write to an unknown region at the stackframe, we have to assert separation between the write and some essential known parts of the current stackframe.
--    These assertions are interesting, these may point to a stack overflow.
-- 2. If one is local and the other not, we assert separation.
-- 3. If the two pointers are global immediates from different global section, we assert separation.
-- 4. If one pointer is the return value of a call, and the other is different, we assert separation.
--
--
is_assertable ctxt a0 si0 a1 si1 =
  let srcs0       = srcs_of_expr ctxt a0
      srcs1       = srcs_of_expr ctxt a1
      is_local0 q = q is_stackpointer_src srcs0
      is_local1 q = q is_stackpointer_src srcs1 in
    or [
      --any (uncurry $ sources_separate_possibly ctxt) [(s0,s1) | s0 <- S.toList srcs0, s1 <- S.toList srcs1],
      separate_pointer_domains ctxt False a0 si0 a1 si1,
      --is_local0 all && not (S.null srcs1) && not (is_local1 any),
      -- is_local1 all && not (S.null srcs0) && not (is_local0 any),

      is_call srcs0 && a0 /= a1,
      is_call srcs1 && a0 /= a1,

      contains_bot a0 && is_local0 any && not (contains_bot a1) && is_local1 all -- TODO make more precise, only certain parts of a1 
     ]
 where
  is_call srcs = S.size srcs == 1 && all is_src_call srcs

  is_src_call (Src_Function _) = True
  is_src_call _                = False 

  is_stackpointer_src (Src_StackPointer _) = True  -- TODO compare stackframes
  is_stackpointer_src _                    = False


  


-- | Add an assertion to the given symbolic predicate
add_assertion rip a0 si0 a1 si1 (p,vcs)  = (p,S.insert (Assertion rip a0 si0 a1 si1) vcs)



generate_assertion :: FContext -> Maybe X86.Address -> SimpleExpr -> State (Pred,VCS) SimpleExpr
generate_assertion ctxt Nothing a0                    = return a0
generate_assertion ctxt (Just (AddressStorage r)) _   = read_reg ctxt r
generate_assertion ctxt (Just (AddressImm i))       _ = return $ SE_Immediate $ fromIntegral i
generate_assertion ctxt (Just (AddressMinus a0 a1)) x = do
  v0 <- generate_assertion ctxt (Just a0) x
  v1 <- generate_assertion ctxt (Just a1) x
  return $ simp $ SE_Op (Minus 64) [v0,v1]
generate_assertion ctxt (Just (AddressPlus a0 a1) ) x = do
  v0 <- generate_assertion ctxt (Just a0) x
  v1 <- generate_assertion ctxt (Just a1) x
  return $ simp $ SE_Op (Plus 64) [v0,v1]
generate_assertion ctxt (Just (AddressTimes a0 a1)) x = do
  v0 <- generate_assertion ctxt (Just a0) x
  v1 <- generate_assertion ctxt (Just a1) x
  return $ simp $ SE_Op (Times 64) [v0,v1]
  



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
invalid_bottom_pointer ctxt e = not (is_immediate e) && S.null (srcs_of_expr ctxt e) 

-- | Assuming a valid pointer (see 'invalid_bottom_pointer'), produce a list of pointers that are to be written to in the memory.
expr_to_mem_addresses ctxt a si =
  if not (contains_bot a) then
    [(a,si)]
  else
    let addresses = dom_to_mem_addresses $ get_pointer_domain ctxt a in
      addresses
 where 
  is_global (Src_ImmediateAddress _) = True
  is_global _ = False

  dom_to_mem_addresses (Domain_Bases bs)     = map base_to_mem_address $ split_per_base $ S.toList bs
  dom_to_mem_addresses (Domain_Sources srcs) = map srcs_to_mem_address $ split_per_source $ S.toList srcs

  base_to_mem_address bs = (Bottom $ FromPointerBases $ S.fromList bs, 1)

  srcs_to_mem_address [Src_StackPointer f] = (Bottom $ FromPointerBases $ S.singleton $ StackPointer f,1)
  srcs_to_mem_address [Src_Malloc i h]     = (Bottom $ FromPointerBases $ S.singleton $ Malloc i h,1)
  srcs_to_mem_address srcs                 = (Bottom $ FromSources $ S.fromList srcs,1)
    
  split_per_base [] = []
  split_per_base (bs:bss) =
    let (separate,overlapping) = partition (pointer_bases_separate_possibly ctxt bs) bss in
      (bs:overlapping) : split_per_base separate

  split_per_source [] = [] 
  split_per_source (src:srcs) =
    let (separate,overlapping) = partition (sources_separate_possibly ctxt src) srcs in
      (src:overlapping) : split_per_source separate






data SeparationStatus = Alias | Separated | Enclosed | Disclosed | Overlap
  deriving Eq


read_from_address :: FContext -> Maybe X86.Operand -> SimpleExpr -> Int -> State (Pred,VCS) SimpleExpr
read_from_address ctxt operand a si0 = do
  let as = map simp $ unfold_non_determinism ctxt a
  vs <- mapM read_from_address' as
  return $ join_exprs ("Read") ctxt vs
 where
  read_from_address' (SE_Immediate imm) = 
    case read_from_datasection (f_ctxt ctxt) imm si0 of 
      Just imm -> -- trace ("Read immediate from datasection: " ++ show (a0,si0) ++ " := " ++ show imm) $ return $ SE_Immediate imm
                  return $ SE_Immediate imm  
      Nothing  -> read_from_state $ SE_Immediate imm
  read_from_address' a0 = read_from_state a0


  read_from_state :: SimpleExpr -> State (Pred,VCS) SimpleExpr
  read_from_state a0 = do
    if contains_bot a0 then
      if invalid_bottom_pointer ctxt a0 then do
        --rip <- read_reg ctxt RIP
        --trace ("READ FROM BASELESS POINTER @ " ++ show rip ++ ": " ++ show (a,si0)) $
        return rock_bottom
      else do
        (Predicate eqs flg,vcs) <- get
        overlapping <- filterM (fmap not . has_separate_base a0) $ M.toList eqs

        let sps'      = map (uncurry SP_Mem) $ expr_to_mem_addresses ctxt a0  si0
        let new_eqs   = map (\sp' -> (sp',mk_uninitialized_value sp')) sps'
        if overlapping == [] then do
          let eqs'    = M.union (M.fromList new_eqs) eqs
          put (Predicate eqs' flg,vcs)
          let srcs      = srcs_of_exprs ctxt $ map snd new_eqs
          let bot       = Bottom (FromUninitializedMemory srcs)
          return bot 
        else do
          let srcs      = srcs_of_exprs ctxt $ map snd (new_eqs ++ overlapping)
          let bot       = Bottom (FromOverlap srcs)
          return bot 
    else if address_is_unwritable (f_ctxt ctxt) a0 then do
      let sp  = SP_Mem a0 si0
      let var = SE_Var sp
      return var   
    else do
      (Predicate eqs flg,vcs) <- get
      do_read a0 $ M.toList eqs


  has_separate_base a0 (SP_Reg _,_)      = return True
  has_separate_base a0 (SP_Mem a1 si1,_) = do
    sep <- is_separate a0 si0 a1 si1
    return $ sep == Separated


  mk_uninitialized_value sp@(SP_Mem a si) = 
    if not $ contains_bot a then
      Bottom (FromUninitializedMemory $ S.singleton $ Src_Var sp)
    else
      Bottom (FromUninitializedMemory $ srcs_of_expr ctxt a) -- rock_bottom 

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
    else if is_assertable ctxt a0 si0 a1 si1 then do
      rip <- read_reg ctxt RIP
      assertion <- generate_assertion ctxt (mk_address operand) a0
      modify $ add_assertion rip assertion si0 a1 si1
      --trace ("ASSERTION (READ@" ++ show rip ++ "): SEPARATION BETWEEN " ++ show (assertion,si0,a0) ++ " and " ++ show (a1,si1)) $
      return Separated
    else do
     --if do_trace a0 a1 then trace ("PRECONDITION (READ): OVERLAP BETWEEN " ++ show (a0,si0) ++ " and " ++ show (a1,si1)) $ return Overlap else
     return Overlap

  mk_address (Just (Memory a si)) = Just a
  mk_address _                    = Nothing

  do_trace a0 a1 = True--srcs_of_expr ctxt a0 /= srcs_of_expr ctxt a1 


  do_read :: SimpleExpr -> [(StatePart, SimpleExpr)] -> State (Pred,VCS) SimpleExpr
  do_read a0 [] = do
    (p,vcs) <- get
    let Predicate eqs flg = p
    let sp  = SP_Mem a0 si0
    let var = Bottom $ FromUninitializedMemory $ S.singleton (Src_Var sp)
    put (Predicate (M.insert sp var eqs) flg,vcs)
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
     FContext     -- ^ The context
  -> X86.Operand  -- ^ The address of an operand of an instruction
  -> State (Pred,VCS) SimpleExpr
read_mem ctxt (Memory a si) = do
  resolved_address <- resolve_address ctxt a
  read_from_address ctxt (Just $ Memory a si) resolved_address si



add_unknown_mem_write mid (p,vcs) = (p,S.insert (SourcelessMemWrite mid) vcs)

-- | Write to memory
-- Each memory write is accomponied with a `MemWriteIdentifier` so that we can log memory writes to unknown locations.
--
write_mem :: 
  FContext               -- ^ The context
  -> MemWriteIdentifier  -- ^ An identifier where the write occurs
  -> SimpleExpr          -- ^ The symbolic address
  -> Int                 -- ^ The size (in bytes)
  -> SimpleExpr          -- ^ The value to be written
  -> State (Pred,VCS) ()
write_mem ctxt mid a si0 v = do
  let as = map simp $ unfold_non_determinism ctxt a
  mapM_ (\a -> write_mem' a v) as -- TODO v should be bot?
 where
  write_mem' a0 v = do
    p@(Predicate eqs flg,vcs) <- get

    if address_is_unwritable (f_ctxt ctxt) a0 then do
      trace ("Writing to unwritable section: " ++ show (a0,si0)) $ return ()
    else if invalid_bottom_pointer ctxt a0 && invalid_bottom_pointer ctxt a then do -- FORALL PATHS VS EXISTS PATH
      --error (show (a0,a,get_known_pointer_bases ctxt a0, srcs_of_expr ctxt a0, get_known_pointer_bases ctxt a, srcs_of_expr ctxt a))
      modify $ add_unknown_mem_write mid
    else do
      p <- get
      (Predicate eqs _ ,_) <- get
      eqs' <- M.fromList <$> do_write a0 si0 v (M.toList eqs)
      (Predicate _ flg,vcs) <- get
      let flg' = clean_flg (SP_Mem a0 si0) flg
      put (Predicate eqs' flg',vcs)


  is_separate a0 si0 a1 si1 = do
    if si0 == si1 && necessarily_equal a0 a1 then
      return Alias
    else if necessarily_separate ctxt a0 si0 a1 si1 then
      --trace ("SEPARATION (WRITE) BETWEEN " ++ show (a0,si0) ++ " and " ++ show (a1,si1) ++ "\n") $
      return Separated
    else if necessarily_enclosed a0 si0 a1 si1 then do
      --trace ("ENCLOSURE1 (WRITE) BETWEEN " ++ show (a0,si0) ++ " and " ++ show (a1,si1)) $
      return Enclosed
    else if necessarily_enclosed a1 si1 a0 si0 then do
      --trace ("ENCLOSURE2 (WRITE) BETWEEN " ++ show (a0,si0) ++ " and " ++ show (a1,si1)) $
      return Disclosed
    else if is_preconditionable ctxt a0 a1 then do
      --trace ("PRECONDITION (WRITE): SEPARATION BETWEEN " ++ show (a0,si0) ++ " and " ++ show (a1,si1))
      modify $ add_precondition a0 si0 a1 si1
      return Separated
    else if is_assertable ctxt a0 si0 a1 si1 then do
      assertion <- generate_assertion ctxt (address mid) a0
      rip <- read_reg ctxt RIP
      modify $ add_assertion rip assertion si0 a1 si1
      --trace ("ASSERTION (WRITE@" ++ show rip ++ "): SEPARATION BETWEEN " ++ show (assertion,si0,a0) ++ " and " ++ show (a1,si1)) $
      return Separated
    else if invalid_bottom_pointer ctxt a0 && expr_is_highly_likely_local_pointer ctxt a1 then do
      assertion <- generate_assertion ctxt (address mid) a0
      rip <- read_reg ctxt RIP
      modify $ add_assertion rip assertion si0 a1 si1
      --trace ("ASSERTION (WRITE@" ++ show rip ++ "): SEPARATION BETWEEN " ++ show (assertion,si0,a0) ++ " and " ++ show (a1,si1)) $
      return Separated
    else
      --if do_trace a0 a1 then trace ("PRECONDITION (WRITE): OVERLAP BETWEEN " ++ show (a0,si0) ++ " and " ++ show (a1,si1)) $ return Overlap else
      return Overlap

  address (MemWriteInstruction _ (Memory addr si) _) = Just addr
  address _                                          = Nothing



  do_trace a0 a1 = True -- srcs_of_expr ctxt a0 /= srcs_of_expr ctxt a1 


  do_write :: SimpleExpr -> Int -> SimpleExpr -> [(StatePart, SimpleExpr)] -> State (Pred,VCS) [(StatePart, SimpleExpr)]
  do_write a0 si0 v []                      =  do
    let sps'  = map (uncurry SP_Mem) $ expr_to_mem_addresses ctxt a0 si0
    return $ map (\sp' -> (sp', trim_expr ctxt v)) sps'
  do_write a0 si0 v (eq@(SP_Reg _,_):mem)   = ((:) eq) <$> do_write a0 si0 v mem
  do_write a0 si0 v (eq@(SP_Mem a1 si1,e):mem) = do
    sep <- is_separate a0 si0 a1 si1
    case sep of
      Alias     -> return $ (SP_Mem a0 si0,trim_expr ctxt v) : mem
      Separated -> ((:) $ eq) <$> do_write a0 si0 v mem
      Enclosed  -> return $ (SP_Mem a1 si1,join_exprs ("write enclosure") ctxt [e,v]) : mem
      Disclosed -> do_write a0 si0 v mem
      Overlap   -> do_write (join_exprs ("write overlap adddres") ctxt [a0,a1]) si0 (join_exprs ("write overlap value") ctxt [e,v]) mem





-- * Operands  
-- | Read from an operand of an instruction
read_operand :: FContext -> X86.Operand -> State (Pred,VCS) SimpleExpr
read_operand ctxt (Storage r)          = read_reg ctxt r
read_operand ctxt (Immediate w)        = return $ SE_Immediate w
read_operand ctxt (EffectiveAddress a) = resolve_address ctxt a
read_operand ctxt (Memory a si)        = read_mem ctxt $ Memory a si

-- | Write to an operand of an instruction 
write_operand ::
 FContext                -- ^ The context
 -> Word64               -- ^ The address of the instruction, used to build a `MemWriteIdentifier`
 -> X86.Operand          -- ^ The operand
 -> SimpleExpr           -- ^ The value to be written
 -> State (Pred,VCS) ()
write_operand ctxt instr_a (Storage r)   v = write_reg ctxt instr_a r v
write_operand ctxt instr_a (Memory a si) v = do
  resolved_address <- resolve_address ctxt a
  write_mem ctxt (MemWriteInstruction instr_a (Memory a si) resolved_address) resolved_address si v
write_operand ctxt instr_a op1 e                      = trace ("Writing to immediate operand: " ++ show (op1,e)) $ return ()-- Should not happen
 

mk_reg_write_identifier instr_a r = MemWriteInstruction instr_a (Storage r) (SE_StatePart $ SP_Reg r)


-- * Stateparts 
-- | Read from an statepart
read_sp :: FContext -> StatePart -> State (Pred,VCS) SimpleExpr
read_sp ctxt (SP_Reg r)          = simp <$> read_reg ctxt r
read_sp ctxt (SP_Mem a si)       = simp <$> read_from_address ctxt Nothing a si

-- | Write to a statepart
write_sp :: 
  FContext                                        -- ^ The context
  -> Word64                                       -- ^ The address of the instruction
  -> (Word64 -> StatePart -> MemWriteIdentifier)  -- ^ Given a statepart, build a memwrite identifier
  -> (StatePart,SimpleExpr)                       -- ^ The statepart and the value to be written
  -> State (Pred,VCS) ()
write_sp ctxt i_a mk_mid (sp @(SP_Reg r),v)   = write_reg ctxt i_a r v
write_sp ctxt i_a mk_mid (sp@(SP_Mem a si),v) = write_mem ctxt (mk_mid i_a sp) a si v



