{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, DeriveGeneric, DefaultSignatures, FlexibleContexts, Strict #-}

{-# OPTIONS_HADDOCK hide #-}


module MachineState where

import Base
import SimplePred
import Context
import Conventions
import X86_Datastructures

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import ACode_Gen hiding (push)
import Data.Word 
import Data.Traversable (for)
import Control.Monad.State.Strict hiding (join)
import Data.List
import Data.Maybe (mapMaybe,fromJust)
import Debug.Trace
import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)


-- Given a predicate of type Pred, we define functions for reading and writing from registers, flags and memory.



-- return true if the symbolic epxression is likely a pointer to a global variable
is_global_expr ctxt (Bottom (FromNonDeterminism es) _) = all (is_global_expr ctxt) es
is_global_expr ctxt (SE_Immediate a)                   = find_section_for_address ctxt (fromIntegral a) /= Nothing
is_global_expr ctxt  _                                 = False 

-- given an expr that represents an address, return:
-- [Local] iff the address points to the local stack-frame (it is only based on RSP)
-- [Global] iff the address is an immediate
-- or the appropiate types from [Local,Global,Heap] if otherwise
expr_to_addr_type ctxt e =
  let regs = regs_of e in
    if is_global_expr ctxt e then
      S.singleton Global
    else if not (S.null regs) && regs `S.isSubsetOf` S.fromList [RSP] && not (contains_bot e) then
      S.singleton Local
    else
      S.unions $ S.map botsrc_to_addr_type $ srcs_of_expr e 
 where
  botsrc_to_addr_type (Src_SP (SP_Reg RSP)) = S.fromList $ [Local]
  botsrc_to_addr_type (Src_SP (SP_Reg _))   = S.fromList $ [Local,Global,Heap]
  botsrc_to_addr_type (Src_SP (SP_Mem _ _)) = S.fromList $ [Local,Global,Heap] 
  botsrc_to_addr_type f@(Src_Function name) = S.fromList $ [Local,Global,Heap] 





---------------
-- REGISTERS --
---------------
read_rreg :: Register -> State Pred SimpleExpr
read_rreg r = do
  Predicate eqs flg ps muddle_status <- get
  case M.lookup (SP_Reg r) eqs of
    Nothing -> do
      let var = SE_Var (SP_Reg r)
      put $ Predicate (M.insert (SP_Reg r) var eqs) flg ps muddle_status
      return var 
    Just e  -> return e

read_reg :: Register -> State Pred SimpleExpr
read_reg r = do
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
      e -> return $ Bottom FromBitMode (srcs_of_expr e)



write_rreg :: Register -> SimpleExpr -> State Pred () 
write_rreg r e = 
  if take 2 (show r) == "ST" then
    return ()
  else 
    modify do_write
 where
  do_write (Predicate eqs flg ps muddle_status) =
    let eqs' = M.insert (SP_Reg r) (trim_expr $ simp e) eqs
        flg' = clean_flg (SP_Reg r) flg in
      Predicate eqs' flg' ps muddle_status
 
write_reg :: Register -> SimpleExpr -> State Pred ()
-- write_reg r (SE_Immediate imm) = write_rreg (real_reg r) (SE_Immediate imm)
write_reg r v = do
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
    write_rreg (real_reg r) $ Bottom FromBitMode (srcs_of_expr v)


-----------
-- FLAGS --
-----------

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





------------
-- MEMORY --
------------
necessarily_enclosed a0 si0 a1 si1 =
  not (contains_bot a0) && not (contains_bot a1) && enc a0 a1
 where
  -- v0 - i0 enc v0 - i1 <==> i1 >= i0  && si0 - i0 <= si1 - i1     (WHEN si0 >= i0 && si1 >= i1)
  enc (SE_Op (Minus _) [SE_Var v0, SE_Immediate i0])
      (SE_Op (Minus _) [SE_Var v1, SE_Immediate i1]) = 
    v0 == v1 && 
      if si0 <= fromIntegral i0 && si1 <= fromIntegral i1 then
        fromIntegral i1 >= fromIntegral i0 && fromIntegral i0 - si0 >= fromIntegral i1 - si1
      else if si0 <= fromIntegral i0 && si1 > fromIntegral i1 then
        fromIntegral i1 >= fromIntegral i0
      else if si0 > fromIntegral i0 && si1 <= fromIntegral i1 then
        False
      else if si0 > fromIntegral i0 && si1 > fromIntegral i1 then
        fromIntegral i1 >= fromIntegral i0 && fromIntegral si0 - i0 <= fromIntegral si1 - i1
      else
        False
  -- v0 + i0 enc v0 + i1 <==> i0 >= i1 && i0 + si0 <= i1 + si1
  enc (SE_Op (Plus _) [SE_Var v0, SE_Immediate i0])
      (SE_Op (Plus _) [SE_Var v1, SE_Immediate i1]) = 
    v0 == v1 && i0 >= i1 && fromIntegral i0 + si0 <= fromIntegral i1 + si1
  -- v0 + i0 enc v0 <==> i0 + si0 <= si1
  enc (SE_Op (Plus _) [SE_Var v0, SE_Immediate i0])
      (SE_Var v1) =
    v0 == v1 && fromIntegral i0 + si0 <= si1
  -- immediates
  enc (SE_Immediate a0) (SE_Immediate a1) = 
    a0 >= a1 && a0 + fromIntegral si0 <= a1 + fromIntegral si1
  enc _ _ = False

necessarily_separate ctxt a0 si0 a1 si1 =
  if not (contains_bot a0) && not (contains_bot a1) then
    (a0,si0) /= (a1,si1) && sep a0 a1
  else
    compare_srcs ctxt a0 a1
 where
  -- two immediate addresses
  sep (SE_Immediate a0)
      (SE_Immediate a1) =
    fromIntegral a0 + si0 <= fromIntegral a1 || fromIntegral a1 + si1 <= fromIntegral a0
  -- v0 - i0 |x| v0 - i1 <==> v0 - i0 + si0 <= v0 - i1 || v0 - i1 + si1 <= v0 - i0 <==> i0-si0 >= i1 || i1-si1 >= i0
  sep (SE_Op (Minus _) [SE_Var v0, SE_Immediate i0])
      (SE_Op (Minus _) [SE_Var v1, SE_Immediate i1]) = 
    if v0 == v1 then
      fromIntegral i0 - si0 >= fromIntegral i1 || fromIntegral i1 - si1 >= fromIntegral i0
    else
      compare_srcs ctxt a0 a1
  -- same for registers
  sep (SE_Op (Minus _) [SE_StatePart (SP_Reg r0), SE_Immediate i0])
      (SE_Op (Minus _) [SE_StatePart (SP_Reg r1), SE_Immediate i1]) =
    if r0 == r1 then
      fromIntegral i0 - si0 >= fromIntegral i1 || fromIntegral i1 - si1 >= fromIntegral i0
    else
      compare_srcs ctxt a0 a1
  -- v0 - i0 |x| v0 <==> i0 >= si0
  sep (SE_Op (Minus _) [SE_Var v0, SE_Immediate i0])
      (SE_Var v1) = 
    if v0 == v1 then
      fromIntegral i0 >= si0 
    else
      compare_srcs ctxt a0 a1
  sep (SE_Var v0)
      (SE_Op (Minus _) [SE_Var v1, SE_Immediate i1]) = 
    if v0 == v1 then
      fromIntegral i1 >= si1
    else
      compare_srcs ctxt a0 a1 
  -- same for registers
  sep (SE_Op (Minus _) [SE_StatePart (SP_Reg r0), SE_Immediate i0])
      (SE_StatePart (SP_Reg r1)) = 
    if r0 == r1 then
      fromIntegral i0 >= si0 
    else
      compare_srcs ctxt a0 a1
  sep (SE_StatePart (SP_Reg r0))
      (SE_Op (Minus _) [SE_StatePart (SP_Reg r1), SE_Immediate i1]) = 
    if r0 == r1 then
      fromIntegral i1 >= si1
    else
      compare_srcs ctxt a0 a1 
  -- v0 + i0 |x| v0 + i1 <==> v0 + i0 + si0 <= v0 + i1 || v0 + i1 + si1 <= v0 + i0 <==> i0+si0 <= i1 || i1+si1 <= i0
  sep (SE_Op (Plus _) [SE_Var v0, SE_Immediate i0])
      (SE_Op (Plus _) [SE_Var v1, SE_Immediate i1]) = 
    if v0 == v1 then
      fromIntegral i0 + si0 <= fromIntegral i1 || fromIntegral i1 + si1 <= fromIntegral i0
    else
      compare_srcs ctxt a0 a1 
  sep (SE_Op (Plus _) [SE_StatePart (SP_Reg r0), SE_Immediate i0])
      (SE_Op (Plus _) [SE_StatePart (SP_Reg r1), SE_Immediate i1]) = 
    if r0 == r1 then
      fromIntegral i0 + si0 <= fromIntegral i1 || fromIntegral i1 + si1 <= fromIntegral i0
    else
      compare_srcs ctxt a0 a1

  -- v0 + i0 |x| v0 <==> i0 >= si1
  sep (SE_Op (Plus _) [SE_Var v0, SE_Immediate i0])
      (SE_Var v1) =
    if v0 == v1 then
      fromIntegral i0 >= si1
    else
      compare_srcs ctxt a0 a1 
  sep (SE_Var v1)
      (SE_Op (Plus _) [SE_Var v0,SE_Immediate i0]) =
    if v0 == v1 then
      fromIntegral i0 >= si1
    else
      compare_srcs ctxt a0 a1 

  -- remainder
  sep a0 a1 = compare_srcs ctxt a0 a1


-- in case we have to make a guess, we compare where the information comes from
compare_srcs ctxt a0 a1 =
  let typs0 = S.toList $ expr_to_addr_type ctxt a0
      typs1 = S.toList $ expr_to_addr_type ctxt a1 in
    if exactly_one_of typs0 typs1 Global then
      -- global is a separate memory space
      True
    else if exactly_one_of typs0 typs1 Heap then
      -- heap is a separate memory space
      True
    else if exactly_one_of typs0 typs1 Local then
      -- local is a separate memory space
      True 
    else
      {--trace ("Do not know whether two regions are necessarily separate: [" ++ show a0 ++ "," ++ show si0 ++ "] and [" ++ show a1 ++ "," ++ show si1 ++ "]")--} False
 where
   exactly_one_of typs0 typs1 typ =
        (typs0 == [typ] && typ `notElem` typs1 && typs1 /= [])
     || (typs1 == [typ] && typ `notElem` typs0 && typs0 /= [])


necessarily_equal a0 a1 = a0 == a1 && not (contains_bot a0) && not (contains_bot a1)

necessarily_equal_stateparts (SP_Reg r0) (SP_Reg r1) = r0 == r1
necessarily_equal_stateparts (SP_Mem a0 si0) (SP_Mem a1 si1) = si0 == si1 && necessarily_equal a0 a1
necessarily_equal_stateparts _ _ = False





resolve_address :: Address -> State Pred SimpleExpr
resolve_address (FromReg r)       = read_reg r
resolve_address (AddrImm i)       = return $ SE_Immediate $ fromIntegral i
resolve_address (AddrMinus a0 a1) = do
  ra0 <- resolve_address a0 
  ra1 <- resolve_address a1
  return $ simp $ SE_Op (Minus 64) [ra0,ra1]
resolve_address (AddrPlus a0 a1) = do
  ra0 <- resolve_address a0 
  ra1 <- resolve_address a1
  return $ simp $ SE_Op (Plus 64) [ra0,ra1]
resolve_address (AddrTimes a0 a1) = do
  ra0 <- resolve_address a0 
  ra1 <- resolve_address a1
  return $ simp $ SE_Op (Times 64) [ra0,ra1]
resolve_address (SizeDir _ a) = resolve_address a



read_from_datasection_expr :: Context -> SimpleExpr -> Int -> Maybe SimpleExpr
read_from_datasection_expr ctxt (SE_Immediate a) si = SE_Immediate <$> read_from_datasection ctxt a si
read_from_datasection_expr _ _ _ = Nothing



generate_assertion :: Address -> State Pred SimpleExpr
generate_assertion (FromReg r) = do
  p <- get
  v <- read_reg r
  a <- do
    if contains_bot v then
      --if all_bot_satisfy (\typ srcs -> typ `elem` [FromSemantics] && all (not . is_function_src) srcs) v then
      --  error $ show (r,v,p)
      --else
        return $ SE_StatePart $ SP_Reg r
    else
      return $ v
  return $ a
 where
  is_function_src (Src_Function _) = True
  is_function_src _ = False
generate_assertion (AddrImm i) = return $ SE_Immediate $ fromIntegral i
generate_assertion (AddrMinus a0 a1) = do
  v0 <- generate_assertion a0
  v1 <- generate_assertion a1
  return $ simp $ SE_Op (Minus 64) [v0,v1]
generate_assertion (AddrPlus a0 a1) = do
  v0 <- generate_assertion a0
  v1 <- generate_assertion a1
  return $ simp $ SE_Op (Plus 64) [v0,v1]
generate_assertion (AddrTimes a0 a1) = do
  v0 <- generate_assertion a0
  v1 <- generate_assertion a1
  return $ simp $ SE_Op (Times 64) [v0,v1]
generate_assertion (SizeDir _ a) = generate_assertion a
  


--TODO move
get_section_of_address ctxt a = find (\(_,_,a0,si0) -> a0 <= a && a < a0 + si0) $ ctxt_sections ctxt



-- An address is considered "unwritable" only if it is an immediate address that belongs to a section that is considered unwritable
-- according to Conventions.

-- address_is_unwritable ctxt (SE_Var (SP_Mem (SE_Immediate a) _)) = IM.lookup (fromIntegral a) (ctxt_syms ctxt) == Just "___stack_chk_guard"
address_is_unwritable ctxt (SE_Immediate a) =
  case get_section_of_address ctxt $ fromIntegral a of
    Nothing -> False
    Just (segname,sectname,_,_) -> section_is_unwritable (segname,sectname)
address_is_unwritable ctxt _ = False


-- An address is considered "unmodifiable_by_external_functions" only if it is an immediate address
-- that belongs to a section that is not considered modifiable by extenral functions according to Conventions.
address_is_unmodifiable_by_external_functions ctxt (SE_Immediate a) =
  case get_section_of_address ctxt $ fromIntegral a of
    Nothing -> False
    Just (segname,sectname,_,_) -> (segname,sectname) `notElem` sections_modifiable_by_external_functions
address_is_unmodifiable_by_external_functions ctxt _ = False




is_return_value_of_call a = contains_bot a && all_bot_satisfy (\typ srcs -> typ == FromCall && S.size srcs == 1) a

is_preconditionable ctxt a0 a1 =
  let no_bot   = not (contains_bot a0) && not (contains_bot a1)
      is_local = \a -> expr_to_addr_type ctxt a == S.singleton Local in
    or [
      no_bot,
      is_return_value_of_call a0 && not (contains_bot a1),
      is_return_value_of_call a1 && not (contains_bot a0)
    ]





read_from_address :: Context -> Address -> SimpleExpr -> Int -> State Pred SimpleExpr
read_from_address ctxt address (Bottom (FromNonDeterminism as) _) si = do
  -- TODO check is vs contains only one value
  vs <- mapM (\a -> read_from_address ctxt address a si) $ S.toList as
  let srcs = S.unions $ map srcs_of_expr vs
  return $ Bottom (FromNonDeterminism $ S.fromList vs) $ srcs
read_from_address ctxt address a0 si0 = do
  case read_from_datasection_expr ctxt a0 si0 of
    Just imm -> return $ imm 
                -- trace ("Read immediate from datasection: " ++ show (a0,si0) ++ " := " ++ show imm) $ return $ SE_Immediate imm
    Nothing -> do
      Predicate eqs flg ps muddle_status <- get
      do_read $ M.toList eqs
 where
  do_read :: [(StatePart, SimpleExpr)] -> State Pred SimpleExpr
  do_read [] = do
    p <- get
    let Predicate eqs flg ps muddle_status = p

    if address_is_unwritable ctxt a0 ||
            muddle_status == Clean ||
            (muddle_status == ExternalOnly && address_is_unmodifiable_by_external_functions ctxt a0) then do
      let sp  = SP_Mem a0 si0
      let var = SE_Var sp
      put $ Predicate (M.insert sp var eqs) flg ps muddle_status
      return var
    else
      return $ Bottom FromUninitializedMemory $ S.singleton (Src_SP $ SP_Mem a0 si0)
  do_read ((SP_Reg _,_):mem)      = do_read mem
  do_read ((SP_Mem a1 si1,e1):mem) =
    if si0 == si1 && necessarily_equal a0 a1 then 
      return e1
    else if necessarily_separate ctxt a0 si0 a1 si1 then
      do_read mem
    else if necessarily_enclosed a0 si0 a1 si1 || necessarily_enclosed a1 si1 a0 si0 then do
      e0 <- do_read mem
      let srcs = srcs_of_exprs e0 e1
      let bot  = Bottom FromOverlap srcs
      return $ bot
    else if is_preconditionable ctxt a0 a1 then do
        --trace ("PRECONDITION (READ): SEPARATION BETWEEN " ++ show (a0,si0,expr_to_addr_type a0) ++ " and " ++ show (a1,si1,expr_to_addr_type a1))
        modify $ add_precondition a0 si0 a1 si1
        do_read mem
    else if contains_bot a0 && not (contains_bot a1) && expr_to_addr_type ctxt a1 == S.singleton Local then do
      rip <- read_reg RIP
      assertion <- generate_assertion address
      -- trace ("ASSERTION (READ@" ++ show rip ++ "): SEPARATION BETWEEN " ++ show (assertion,si0,a0,expr_to_addr_type ctxt a0) ++ " and " ++ show (a1,si1,expr_to_addr_type ctxt a1)) $
      modify $ add_assertion rip assertion si0 a1 si1
      do_read mem
    else if not (contains_bot a0) && contains_bot a1 && expr_to_addr_type ctxt a0 == S.singleton Local then do
      do_read mem
    --else if (expr_to_addr_type a0 == S.singleton Local && expr_to_addr_type a1 == S.singleton Local)  then
    --  error ("PRECONDITION??? (READ): SEPARATION BETWEEN " ++ show (a0,si0,expr_to_addr_type a0) ++ " and " ++ show (a1,si1,expr_to_addr_type a1))
    else do
      e0 <- do_read mem
      let srcs = srcs_of_exprs e0 e1
      let bot  = Bottom FromOverlap srcs

      --trace ("Overlap: " ++ show (a0,si0,expr_to_addr_type ctxt a0) ++ " and " ++ show (a1,si1,expr_to_addr_type ctxt a1)) $
      return $ bot 

read_mem :: Context -> Address -> State Pred SimpleExpr
read_mem ctxt (SizeDir si a) = do
  resolved_address <- resolve_address a
  read_from_address ctxt a resolved_address si



remove_region :: SimpleExpr -> Int -> State Pred ()
remove_region a si = do
  Predicate eqs flg ps muddle_status <- get
  let eqs' = M.delete (SP_Mem a si) eqs
  let flg' = clean_flg (SP_Mem a si) flg
  put $ Predicate eqs' flg' ps muddle_status

write_region :: SimpleExpr -> Int -> SimpleExpr -> State Pred ()
write_region a si v = do
  Predicate eqs flg ps muddle_status <- get
  let eqs' = M.insert (SP_Mem a si) v eqs
  let flg' = clean_flg (SP_Mem a si) flg
  put $ Predicate eqs' flg' ps muddle_status


write_mem :: Context -> Address -> SimpleExpr -> Int -> SimpleExpr -> State Pred ()
write_mem ctxt address (Bottom (FromNonDeterminism as) _) si v = mapM_ (\a -> write_mem ctxt address a si v) as
write_mem ctxt address a0 si0 v = do
  Predicate eqs flg ps muddle_status <- get
  do_write True $ M.toList eqs
 where
  do_write :: Bool -> [(StatePart, SimpleExpr)] -> State Pred ()
  do_write True  []                  = write_region a0 si0 (trim_expr $ simp v)
  do_write False []                  = return ()
  do_write b ((SP_Reg _,_):mem)      = do_write b mem
  do_write b ((SP_Mem a1 si1,e):mem) =
    if address_is_unwritable ctxt a0 then do
      p <- get
      error ("Writing to unwritable section: " ++ show (a0,si0,expr_to_addr_type ctxt a0) ++ "\n" ++ show p)
    else if si0 == si1 && necessarily_equal a0 a1 then do
      write_region a1 si1 (simp v)
      do_write False mem 
    else if necessarily_separate ctxt a0 si0 a1 si1 then
      --trace ("SEPARATION (WRITE) BETWEEN " ++ show (a0,si0,expr_to_addr_type ctxt a0) ++ " and " ++ show (a1,si1,expr_to_addr_type ctxt a1) ++ "\n") $
      do_write b mem
    else if necessarily_enclosed a0 si0 a1 si1 then do
      let srcs = srcs_of_exprs e v
      let bot  = Bottom FromMemWrite srcs
      p <- get
      --trace ("ENCLOSURE1 (WRITE) BETWEEN " ++ show (a0,si0,expr_to_addr_type ctxt a0) ++ " and " ++ show (a1,si1,expr_to_addr_type ctxt a1) ++ "\n" ++ show p) $
      write_region a1 si1 bot
    else if necessarily_enclosed a1 si1 a0 si0 then do
      p <- get
      remove_region a1 si1
      --trace ("ENCLOSURE2 (WRITE) BETWEEN " ++ show (a0,si0,expr_to_addr_type ctxt a0) ++ " and " ++ show (a1,si1,expr_to_addr_type ctxt a1) ++ "\n" ++ show p) $
      do_write b mem
    else if is_preconditionable ctxt a0 a1 then do
      --trace ("PRECONDITION (WRITE): SEPARATION BETWEEN " ++ show (a0,si0,expr_to_addr_type ctxt a0) ++ " and " ++ show (a1,si1,expr_to_addr_type ctxt a1))
      modify $ add_precondition a0 si0 a1 si1
      do_write b mem
    else if contains_bot a0 && not (contains_bot a1) && expr_to_addr_type ctxt a1 == S.singleton Local then do
      rip <- read_reg RIP
      assertion <- generate_assertion address
      --trace ("ASSERTION (WRITE@" ++ show rip ++ "): SEPARATION BETWEEN " ++ show (assertion,si0,a0,expr_to_addr_type ctxt a0) ++ " and " ++ show (a1,si1,expr_to_addr_type ctxt a1)) $
      modify $ add_assertion rip assertion si0 a1 si1
      do_write b mem
    else if not (contains_bot a0) && contains_bot a1 && expr_to_addr_type ctxt a0 == S.singleton Local then do
      do_write b mem
    else do
      -- overlap, and thus overwrite with bottom
      let srcs = srcs_of_exprs e v
      let bot  = Bottom FromMemWrite srcs
      --trace ("Overlap: " ++ show (a0,si0,expr_to_addr_type ctxt a0) ++ " and " ++ show (a1,si1,expr_to_addr_type ctxt a1)) $
      write_region a1 si1 bot
      do_write b mem


--------------
-- OPERANDS --
--------------

read_operand :: Context -> Operand -> State Pred SimpleExpr
read_operand ctxt (Reg r)       = read_reg r
read_operand ctxt (Immediate w) = return $ SE_Immediate w
read_operand ctxt (Address a)   = read_mem ctxt a

write_operand :: Context -> Operand -> SimpleExpr -> State Pred ()
write_operand ctxt (Reg r) v = write_reg r v
write_operand ctxt (Address (SizeDir si a)) v = do
  resolved_address <- resolve_address a
  write_mem ctxt a resolved_address si v
write_operand ctxt op1 e = trace ("Writing to immediate operand: " ++ show (op1,e)) $ return ()-- Should not happen
 
