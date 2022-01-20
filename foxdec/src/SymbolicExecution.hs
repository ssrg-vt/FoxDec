{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, FlexibleContexts, StrictData #-}

-----------------------------------------------------------------------------
-- |
-- Symbolic execution of sequential lists of instructions of type @`Instr'@ on predicates of type @`Pred`@.
-----------------------------------------------------------------------------

module SymbolicExecution (
  tau_block,
  init_pred,
  gather_stateparts,
  is_initial,
  invariant_to_finit,
  weaken_finit,
  get_invariant
  ) where

import Base
import SimplePred
import Context
import MachineState
import Propagation
import X86_Datastructures
import Conventions
import ControlFlow
import Pointers

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Data.Maybe (fromJust)
import Data.List.Extra (firstJust)
import Data.Word 
import Data.Either (partitionEithers)
import Control.Monad.State.Strict hiding (join)
import Control.Monad.Extra (anyM,whenM,mapMaybeM)
import Data.List hiding (transpose)
import Data.Maybe (mapMaybe)
import Debug.Trace



-- | Forward transposition
-- If the current invariant states:
-- 		@r == r0 - i@
-- 		@x == r0@
-- 
-- In words, register r contains its initial value r0 minus some immediate i.
-- If a function is called, then a new r0 is introduced, r0', which models the initial value of r for that function.
-- In any expression we replace an occurence of r0 with @r0' + i@.
--
-- The new function initialisation thus becomes:
--    @r == r0'@
--    @x == r0' + i@
fw_transposition :: Context -> StatePart -> (StatePart,SimpleExpr) -> Maybe SimpleExpr
fw_transposition ctxt (SP_Reg r) (SP_Reg r1, SE_Op (Minus wsize) [SE_Var (SP_Reg r2), SE_Immediate i]) 
  | r == r1 && r1 == r2    = Just $ SE_Op (Plus wsize) [SE_Var (SP_Reg r), SE_Immediate i]
  | otherwise              = Nothing
fw_transposition _ _ _     = Nothing


transpose_fw_e :: Context -> M.Map StatePart SimpleExpr -> SimpleExpr -> Maybe SimpleExpr 
transpose_fw_e ctxt p (Bottom (FromNonDeterminism es)) = do
  es' <- mapM (fmap simp . transpose_fw_e ctxt p) $ S.toList es
  return $ join_exprs "transpose_fw" ctxt $ es'
transpose_fw_e ctxt p (Bottom (FromPointerBases bs)) = do
  bs' <- mapM (transpose_fw_base ctxt p) $ S.toList bs
  return $ Bottom (FromPointerBases $ S.fromList bs')
transpose_fw_e ctxt p   (Bottom typ)          = Nothing -- TODO what if all sources are mallocs?
transpose_fw_e ctxt p e@(SE_Malloc _ _)       = return e
transpose_fw_e ctxt p e@(SE_Immediate i)      = return e
transpose_fw_e ctxt p   (SE_Var sp)           = transpose_fw_var ctxt p sp
transpose_fw_e ctxt p   (SE_StatePart sp)     = Nothing 
transpose_fw_e ctxt p   (SE_Op op es)         = SE_Op op <$> mapM (transpose_fw_e ctxt p) es
transpose_fw_e ctxt p   (SE_Bit i e)          = SE_Bit i <$> transpose_fw_e ctxt p e
transpose_fw_e ctxt p   (SE_SExtend l h e)    = SE_SExtend l h <$> transpose_fw_e ctxt p e
transpose_fw_e ctxt p   (SE_Overwrite i a b)  = do
  a' <- transpose_fw_e ctxt p a
  b' <- transpose_fw_e ctxt p b
  return $ SE_Overwrite i a' b'

transpose_fw_var :: Context -> M.Map StatePart SimpleExpr -> StatePart -> Maybe SimpleExpr
transpose_fw_var ctxt p (SP_Mem a si) = Nothing -- TODO
transpose_fw_var ctxt p (SP_Reg r)    = firstJust (fw_transposition ctxt $ SP_Reg r) $ M.toList p

transpose_fw_sp ctxt p sp@(SP_Reg r) = Just sp
transpose_fw_sp ctxt p (SP_Mem a si) = do
  a' <- transpose_fw_e ctxt p a
  return $ SP_Mem (simp a') si


transpose_fw_base ctxt p (Unknown e)  = Unknown <$> transpose_fw_e ctxt p e
transpose_fw_base ctxt p bs           = return bs



-- | Convert the current invariant into a function initialisation
invariant_to_finit :: Context -> Pred -> FInit
invariant_to_finit ctxt (Predicate eqs _ _) = M.mapMaybeWithKey keep eqs
 where
  keep sp@(SP_Reg r)    v = if r `elem` parameter_registers && expr_is_highly_likely_local_pointer ctxt v then Just bottom_rsp else Nothing 
  keep sp@(SP_Mem a si) v = Nothing -- TODO

  bottom_rsp = Bottom (FromPointerBases $ S.singleton StackPointer)
{--
invariant_to_finit ctxt p =
  let (Predicate eqs _ _,_) = execState (push ctxt $ Immediate 42) (p,S.empty) in
    M.mapMaybeWithKey (keep eqs) eqs
 where
  keep eqs sp@(SP_Reg r)    v = if r `elem` parameter_registers then keep_pointer (simp <$> transpose_fw_e ctxt eqs v) else Nothing
  keep eqs sp@(SP_Mem a si) v = 
    if not (is_initial sp v) && expr_is_global_pointer ctxt a then
      keep_pointer (simp <$> transpose_fw_e ctxt eqs v)
    else
      Nothing -- if expr_highly_likely_pointer ctxt v && not (is_initial sp v) then trace (show ("Pruned: ",sp,v)) Nothing else Nothing -- TODO


  keep_pointer Nothing  = Nothing
  keep_pointer (Just a) = if not (contains_bot a) && expr_is_global_pointer ctxt a then Just a else Nothing
--}

-- | The join between two function initialisations
weaken_finit :: Context -> FInit -> FInit -> FInit
weaken_finit ctxt f0 f1 = M.filter keep $ M.intersectionWith weaken_finit_eq f0 f1
 where
  weaken_finit_eq e0 e1 = 
    if e0 == e1 then
      e0
    else 
      join_exprs "weaken_finit" ctxt [e0,e1]

  keep (Bottom (FromPointerBases bs)) = not (S.null bs)   
  keep (Bottom (FromSources srcs))    = not (S.null srcs) 







-- | Backward transposition
-- Let p be the current predicate and let trhe equality sp == v be from the predicate after execution of an internal function.
-- For example, p contains:
--   RSP == RSP0 - 64
--   RSI == 10
--
-- And after execution of the function, we have:
--   *[RSP0+16,8] == RSI0
--
-- Transposing this equality produces:
--   *[RSP0-40,8] == 10
transpose_bw :: Context -> String -> Int -> Pred -> (StatePart, SimpleExpr) -> Either VerificationCondition (StatePart, SimpleExpr)
transpose_bw ctxt f a p (sp,v) =
  let sp' = transpose_bw_sp ctxt p sp
      v'  = trim_expr $ simp $ transpose_bw_e ctxt p v in
    if is_invalid sp' then
      --error $ show (((sp,v),(sp',v')), p)
      Left $ SourcelessMemWrite $ MemWriteFunction f a sp
    else
      Right (sp',v')
 where
  is_invalid (SP_Reg _)   = False
  is_invalid (SP_Mem a _) = invalid_bottom_pointer ctxt a



transpose_bw_e ctxt p (Bottom (FromNonDeterminism es)) = join_exprs ("transpose_bw") ctxt $ map (transpose_bw_e ctxt p) $ S.toList es
transpose_bw_e ctxt p (Bottom (FromPointerBases bs))   = Bottom (FromPointerBases $ S.map (transpose_bw_base ctxt p) bs)
transpose_bw_e ctxt p (Bottom typ)                     = Bottom $ transpose_bw_bottyp ctxt p typ
transpose_bw_e ctxt p (SE_Malloc id hash)              = SE_Malloc id hash
transpose_bw_e ctxt p (SE_Immediate i)                 = SE_Immediate i
transpose_bw_e ctxt p (SE_StatePart sp)                = Bottom $ FromSemantics S.empty
transpose_bw_e ctxt p (SE_Var sp)                      = evalState (read_sp ctxt $ transpose_bw_sp ctxt p sp) (p,S.empty)
transpose_bw_e ctxt p (SE_Bit i e)                     = SE_Bit i $ transpose_bw_e ctxt p e
transpose_bw_e ctxt p (SE_SExtend l h e)               = SE_SExtend l h $ transpose_bw_e ctxt p e
transpose_bw_e ctxt p (SE_Op op es)                    = SE_Op op $ map (transpose_bw_e ctxt p) es
transpose_bw_e ctxt p (SE_Overwrite i a b)             = SE_Overwrite i (transpose_bw_e ctxt p a) (transpose_bw_e ctxt p b) 

transpose_bw_sp ctxt p (SP_Reg r)    = SP_Reg r
transpose_bw_sp ctxt p (SP_Mem a si) = SP_Mem (trim_expr $ simp $ transpose_bw_e ctxt p a) si

transpose_bw_bottyp ctxt p (FromSources srcs)             = FromSources $ S.unions $ S.map (transpose_bw_src ctxt p) srcs
transpose_bw_bottyp ctxt p (FromOverlap srcs)             = FromSources $ S.unions $ S.map (transpose_bw_src ctxt p) srcs
transpose_bw_bottyp ctxt p (FromMemWrite srcs)            = FromSources $ S.unions $ S.map (transpose_bw_src ctxt p) srcs
transpose_bw_bottyp ctxt p (FromSemantics srcs)           = FromSources $ S.unions $ S.map (transpose_bw_src ctxt p) srcs
transpose_bw_bottyp ctxt p (FromBitMode srcs)             = FromSources $ S.unions $ S.map (transpose_bw_src ctxt p) srcs
transpose_bw_bottyp ctxt p (FromUninitializedMemory srcs) = FromSources $ S.unions $ S.map (transpose_bw_src ctxt p) srcs
transpose_bw_bottyp ctxt p (FromCall f)                   = FromCall f

transpose_bw_src ctxt p (Src_Var sp)      = srcs_of_expr ctxt $ transpose_bw_e ctxt p (SE_Var sp)
transpose_bw_src ctxt p (Src_Malloc id h) = S.singleton $ Src_Malloc id h
transpose_bw_src ctxt p (Src_Function f)  = S.singleton $ Src_Function f



transpose_bw_base ctxt p b@StackPointer          = b
transpose_bw_base ctxt p b@(GlobalAddress _)     = b
transpose_bw_base ctxt p b@(PointerToSymbol _ _) = b
transpose_bw_base ctxt p b@(Malloc _ _)          = b
transpose_bw_base ctxt p b@(Unknown e)           = Unknown $ transpose_bw_e ctxt p e







-- | a list of some function that return a heap-pointer through RAX.
-- The pointer is assumed to  be fresh.
functions_returning_fresh_pointers = [ 
     "_malloc", "malloc", "_malloc_create_zone", "_malloc_default_zone", "_malloc_zone_malloc", "_calloc", "calloc", "_malloc_zone_calloc", "_mmap",
     "strdup", "_strdup", "___error", "_fts_read$INODE64", "_fts_open$INODE64", "_opendir$INODE64", "fopen", "_fopen", "_getenv", "_open",
     "_localeconv", "localeconv", "_setlocale", "_wsetlocale", "_fgetln", "fgetln",
     "strerror", "_strerror", "_wcserror", "__wcserror"
   ]

-- | A list of some functions that are assumed not to change the state in any significant way, and that return an unknown bottom value through RAX
functions_returning_bottom = [
     "feof", "_feof", "_getc", "getc", "fgetc", "_fgetc", "_fgetwc", "fgetwc", "_fnmatch", "_fputc",
     "_read", "read", "_fread", "fread", "_close", "_fstat$INODE64", "_fstatfs$INODE64", "_statfs$INODE64", "___maskrune", "_sysctlbyname", "_getbsize",
     "_printf", "printf", "_fprintf", "fprintf", "_fprintf_l", "fwprintf", "_fwprintf_l",
     "_putchar", "_puts", 
     "_btowc", "btowc", "mbtowc", "_mbtowc", "_mbrtowc", "mbrtowc", "_atof", "atof",
     "_strcmp", "strcmp",
     "_ilogb", "_atoi",
     "___stack_chk_fail", "_getopt", "_free",
     "_warn", "_warnx"
   ]
 ++ exiting_function_calls



-- | Executes semantics for some external functions.
-- Returns true iff the string corresponds to a known external function, and the semantics were applied.
function_semantics :: Context -> Instr -> String -> State (Pred,VCS) Bool
function_semantics ctxt i "_realloc"             = function_semantics ctxt i "realloc"
function_semantics ctxt i "_malloc_zone_realloc" = function_semantics ctxt i "realloc"
function_semantics ctxt i "realloc"              = read_reg ctxt RDI >>= write_reg ctxt RAX >> return True
function_semantics ctxt i "_strcpy"              = function_semantics ctxt i "strcpy"
function_semantics ctxt i "strcpy"               = read_reg ctxt RDI >>= write_reg ctxt RAX >> return True
function_semantics ctxt i "_strrchr"             = function_semantics ctxt i "strrchr"
function_semantics ctxt i "strrchr"              = read_reg ctxt RDI >>= (\rdi -> write_reg ctxt RAX $ SE_Op (Plus 64) [rdi,rock_bottom]) >> return True
function_semantics ctxt i f                      = 
  if f `elem` functions_returning_bottom then do
    write_reg ctxt RAX $ Bottom $ FromCall f -- TODO overwrite volatile regs as well?
    return True 
  else if f `elem` functions_returning_fresh_pointers then do
    write_rreg RAX $ SE_Malloc (Just (i_addr i)) (Just "")  -- TODO (show p))
    return True
  else
    return False




-- | Add a function constraint to the given symbolic predicate
add_function_constraint f a ps sps (p,vcs) = (p,S.insert (FunctionConstraint f a ps sps) vcs)


-- | Symbolically execute a function call
call :: Context -> Instr -> State (Pred,VCS) ()
call ctxt i = do
  let f   = function_name_of_instruction ctxt i
  let i_a = i_addr i
  known <- function_semantics ctxt i f

  when (not known) $ do
    (p@(Predicate p_eqs _ _),_) <- get
    params <- mapMaybeM (when_is_relevant_param p_eqs) parameter_registers
    let postconditions = map postcondition_of_jump_target $ resolve_jump_target ctxt i

    if postconditions == [] || any ((==) Nothing) postconditions then do
      -- an external function, or a function that produced a verification error, or with unrersolved indirections

      -- 1.) for each parameter, smudge the current state
      forM_ params (write_param f . snd)
      (q@(Predicate q_eqs _ _),_) <- get

      -- 2.) transfer stateparts that must be kept intact, and generation verification conditions if necessary
      sps <- S.unions <$> (mapM (transfer_current_statepart p q True) $ M.toList p_eqs)
      when (not $ S.null sps) $ modify $ add_function_constraint f i_a params sps

      -- 3.) For all return registers (RAX,XMM0), write some unknown return value
      forM_ return_registers (\r -> write_reg ctxt r $ Bottom $ FromCall $ function_name_of_instruction ctxt i) -- TODO flags
    else do
      -- an internal function, already verified
      sub ctxt i_a (Reg RSP) (Immediate 8)
      (p@(Predicate p_eqs _ _),vcs) <- get

      -- 1.) obtain the postcondition of the function, and do backwards transposition
      let (q@(Predicate q_eqs _ _)) = supremum ctxt $ map fromJust postconditions
      let (vcs',q_eqs_transposed)   = partitionEithers $ map (transpose_bw ctxt f i_a p) $ filter (uncurry do_transfer) $ M.toList q_eqs
      put ((Predicate M.empty None Clean),S.union vcs $ S.fromList vcs')
      mapM_ (write_sp ctxt mk_mid) q_eqs_transposed
      (q_transposed@(Predicate q_eqs _ _),vcs) <- get

      -- 2.) transfer stateparts that must be kept intact, and generation verification conditions if necessary
      sps <- S.unions <$> (mapM (transfer_current_statepart p q_transposed False) $ M.toList p_eqs)
      when (not $ S.null sps) $ modify $ add_function_constraint f i_a params sps
 where
  write_param f a =
    if address_is_unwritable ctxt a then
      return () 
    else
      let a'  = SE_Op (Plus 64) [a,rock_bottom]
          si' = 1 in
      write_mem ctxt (mk_mid $ SP_Mem a' si') a' si' (Bottom $ FromSources $ S.singleton $ Src_Function f)  


  -- let the current predicate p, before the call, contain an equation sp == v
  -- q provides the predicate after the function call.
  -- We transfer the statepart sp and sometimes forcibly keep its original value v, even if we could not prove that it was preserved.
  -- In those cases, we add annotations in the form of "Function Constraints".
  transfer_current_statepart p@(Predicate p_eqs _ _) q@(Predicate q_eqs _ _) is_external (sp,v) = do
    if sp == SP_Reg RIP then do
      -- forcibly transfer and set the value of the instruction pointer
      forced_insert_sp sp (SE_Immediate $ fromIntegral (i_addr i + i_size i))
      return S.empty
    else if all (necessarily_separate_stateparts ctxt sp) $ M.keys q_eqs then do
      -- if the function did not write to the statpart, transfer it without annotations
      forced_insert_sp sp v
      return S.empty
    else let v' = evalState (read_sp ctxt sp) (q,S.empty) in
      if sp == SP_Reg RSP && not is_external && v' /= simp (SE_Op (Plus 64) [v,SE_Immediate 8]) then do
        -- register RSP must be set to its original value + 8, force this and add an annotation
        forced_insert_sp sp (SE_Op (Plus 64) [v,SE_Immediate 8])
        return $ S.singleton sp
      else if or [
           is_external && v' /= v && statepart_preserved_after_external_function_call ctxt i sp && (must_be_preserved p_eqs sp v || is_reg_sp sp),
           not is_external && v /= v' && is_mem_sp sp && statepart_preserved_after_external_function_call ctxt i sp && must_be_preserved p_eqs sp v
          ] then do
        -- the statepart should have been preserved by the function, but that cannot be proven
        -- forcibly preserve the statepart and annotate
        forced_insert_sp sp v
        return $ S.singleton sp
      else do
        -- the statepart was not preserved by the function, but therw as no need, so use its new value
        write_sp ctxt mk_mid (sp,v')
        return S.empty
        

  -- make a memory-write-identifier: the memroy write happened during this function call
  mk_mid = MemWriteFunction (function_name_of_instruction ctxt i) (i_addr i) 

  -- forcibly insert the statepart into ther current predicate
  -- should never be done without caution, one should always use the mem_write function
  forced_insert_sp sp v = do
    (p@(Predicate p_eqs flg muddlestatus),vcs) <- get
    put (Predicate (M.insert sp v p_eqs) flg muddlestatus,vcs)

 
  must_be_preserved p_eqs (SP_Reg _)                       _ = True
  must_be_preserved p_eqs (SP_Mem (SE_Var (SP_Reg RSP)) _) _ = True
  must_be_preserved p_eqs (SP_Mem a si)                    v = if expr_is_highly_likely_local_pointer ctxt a then expr_highly_likely_pointer ctxt v || is_reg_var v || is_currently_pointer p_eqs v else True 

  is_reg_var (SE_Var (SP_Reg r)) = True
  is_reg_var _                   = False

  is_currently_pointer p_eqs a  = find (is_region_for a . fst) (M.toList p_eqs) /= Nothing
  is_region_for a (SP_Reg _)    = False
  is_region_for a (SP_Mem a' _) = a == a'

  when_is_relevant_param p_eqs r = do
    v <- read_reg ctxt r
    p <- get
    if expr_highly_likely_pointer ctxt v || (is_currently_pointer p_eqs v && not (is_immediate v)) then
      return $ Just (r,v)
    else
      return Nothing


  do_transfer sp@(SP_Reg _)                                             v = True 
  do_transfer sp@(SP_Mem (SE_Var (SP_Reg RSP))                       _) v = False
  do_transfer sp@(SP_Mem (SE_Op (Minus 64) [SE_Var (SP_Reg RSP), e]) _) v = contains_bot e
  do_transfer sp                                                        v = not $ is_initial sp v -- (not $ contains_bot_sp sp) && (

  postcondition_of_jump_target (ImmediateAddress a) =
    case IM.lookup (fromIntegral a) (ctxt_calls ctxt) of
      Just (ReturningWith q) -> Just q
      _                      -> Nothing
  postcondition_of_jump_target _                    = Nothing



statepart_preserved_after_external_function_call ::
  Context       -- ^ The context
  -> Instr      -- ^ The instruction currently symbolically executed (a @call@)
  -> StatePart  -- ^ A state part 
  -> Bool 
statepart_preserved_after_external_function_call ctxt i (SP_Reg r)   = r `elem` callee_saved_registers
statepart_preserved_after_external_function_call ctxt i (SP_Mem a _) = not (contains_bot a) && or [
      expr_is_highly_likely_local_pointer ctxt a,
      address_is_unwritable ctxt a ,
      (instruction_jumps_to_external ctxt i && address_is_unmodifiable_by_external_functions ctxt a)
     ]




is_initial :: StatePart -> SimpleExpr -> Bool
is_initial sp v = v == SE_Var sp










-- | Instruction semantics
push :: Context -> Int -> Operand -> State (Pred,VCS) ()
push ctxt i_a (Immediate imm) = do
  let si = 8
  let address = AddrMinus (FromReg RSP) (AddrImm si)
  e1 <- resolve_address ctxt address
  write_reg ctxt RSP e1
  write_mem ctxt (MemWriteInstruction i_a address e1) e1 si (SE_Immediate imm)
push ctxt i_a op1 = do
  e0 <- read_operand ctxt op1
  let si = operand_size op1
  let address = AddrMinus (FromReg RSP) (AddrImm $ fromIntegral si)
  e1 <- resolve_address ctxt address
  write_reg ctxt RSP e1
  write_mem ctxt (MemWriteInstruction i_a address e1) e1 si e0

pop :: Context -> Int -> Operand -> State (Pred,VCS) ()
pop ctxt i_a op1 = do
  let si = operand_size op1
  e0 <- read_mem ctxt (SizeDir si (FromReg RSP))
  let address = AddrPlus (FromReg RSP) (AddrImm $ fromIntegral si)
  e1 <- resolve_address ctxt address
  write_reg ctxt RSP e1
  write_operand ctxt i_a op1 e0

lea :: Context -> Int -> Operand -> Operand -> State (Pred,VCS) ()
lea ctxt i_a op1 (Address a) = do
  e <- resolve_address ctxt a
  write_operand ctxt i_a op1 e


leave :: Context -> Int -> State (Pred,VCS) ()
leave ctxt i_a = mov ctxt i_a (Reg RSP) (Reg RBP) >> pop ctxt i_a (Reg RBP)


ret ctxt i_a = pop ctxt i_a (Reg RIP)

sysret ctxt i_a = do
  e0 <- read_operand ctxt (Reg RCX)
  write_rreg RIP e0
  write_flags (\_ _ -> None) (Reg RCX) (Reg RCX)

jmp ctxt i =
  if instruction_jumps_to_external ctxt i then
    -- A jump to an external symbol is treated as a function call and implicit RET
    call ctxt i >> ret ctxt (i_addr i)
  else
    return ()



write_flags :: (Operand -> Operand -> FlagStatus) -> Operand -> Operand -> State (Pred,VCS) ()
write_flags g op1 op2 = do
  (Predicate eqs flg muddle_status,vcs) <- get
  put (Predicate eqs (g op1 op2) muddle_status,vcs)


mov_with_func_op2_to_op1 :: Context -> Int -> (SimpleExpr -> SimpleExpr) -> Operand -> Operand -> State (Pred,VCS) ()
mov_with_func_op2_to_op1 ctxt i_a f op1 op2 = do
  e1 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (f e1)

mk_bottom ctxt es = Bottom $ FromSemantics $ S.unions (map (srcs_of_expr ctxt) es)

mov_with_func1 :: Context -> Int -> (Context -> [SimpleExpr] -> SimpleExpr) -> Bool -> Operand -> State (Pred,VCS) ()
mov_with_func1 ctxt i_a f do_write_flags op1 = do
  e0 <- read_operand ctxt op1
  write_operand ctxt i_a op1 (f ctxt [e0])
  when do_write_flags (write_flags (\_ _ -> None) op1 op1)

mov_with_func :: Context -> Int -> (Context -> [SimpleExpr] -> SimpleExpr) -> Bool -> Operand -> Operand -> State (Pred,VCS) ()
mov_with_func ctxt i_a f do_write_flags op1 op2 = do
  e0 <- read_operand ctxt op1
  e1 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (f ctxt [e0,e1])
  when do_write_flags (write_flags (\_ _ -> None) op1 op2)

mov_with_func3 :: Context -> Int -> (Context -> [SimpleExpr] -> SimpleExpr) -> Bool -> Operand -> Operand -> Operand -> State (Pred,VCS) ()
mov_with_func3 ctxt i_a f do_write_flags op1 op2 op3 = do
  e0 <- read_operand ctxt op1
  e1 <- read_operand ctxt op2
  e2 <- read_operand ctxt op3
  write_operand ctxt i_a op1 (f ctxt [e0,e1,e2])
  when do_write_flags (write_flags (\_ _ -> None) op2 op3)

nop ctxt i_a = return ()

ud2 ctxt i_a = return ()

hlt ctxt i_a = return ()

wait ctxt i_a = return ()

mfence ctxt i_a = return ()

clflush ctxt i_a = return ()

mov ctxt i_a = mov_with_func_op2_to_op1 ctxt i_a id 

movzx ctxt i_a op1 op2 = do
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 e2

movsx ctxt i_a op1 op2 = do
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (SE_SExtend (8 * operand_size op2) (8 * operand_size op1) e2)

movsxd ctxt i_a op1 op2 = do
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (SE_SExtend (8 * operand_size op2) (8 * operand_size op1) e2)

movsd = mov

movss = mov

movaps = mov

movapd = mov

movups = mov

movupd = mov

movabs = mov

movdqu = mov

movdqa = mov

movlpd = mov

movlps = mov

movd   = mov

movq   = mov

cmov ctxt i_a op1 op2 = do
  e0 <- read_operand ctxt op1
  e1 <- read_operand ctxt op2
  write_operand ctxt i_a op1 $ join_exprs ("cmov") ctxt [e0,e1]

xchg :: Context -> Int -> Operand -> Operand -> State (Pred,VCS) ()
xchg ctxt i_a op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 e2
  write_operand ctxt i_a op2 e1


cmp ctxt i_a = write_flags $ FS_CMP Nothing

add ctxt i_a op1 = mov_with_func ctxt i_a (\ctxt -> SE_Op (Plus (8 * operand_size op1))) True op1

sub ctxt i_a op1 = mov_with_func ctxt i_a (\ctxt -> SE_Op (Minus (8 * operand_size op1))) True op1

neg ctxt i_a op1 = mov_with_func1 ctxt i_a (\ctxt e -> SE_Op (Minus (8 * operand_size op1)) (SE_Immediate 0 : e) ) True op1

test ctxt i_a = write_flags (\_ _ -> None) --TODO needed?

ucomisd ctxt i_a = write_flags (\_ _ -> None)

ucomiss ctxt i_a = write_flags (\_ _ -> None)

inc :: Context -> Int -> Operand -> State (Pred,VCS) ()
inc ctxt i_a op1 = do
  e1 <- read_operand ctxt op1
  write_operand ctxt i_a op1 (SE_Op (Plus (8 * operand_size op1)) [e1,SE_Immediate 1])
  write_flags (\_ _ -> None) op1 op1

dec :: Context -> Int -> Operand -> State (Pred,VCS) ()
dec ctxt i_a op1 = do
  e1 <- read_operand ctxt op1
  write_operand ctxt i_a op1 (SE_Op (Minus (8 * operand_size op1)) [e1,SE_Immediate 1])
  write_flags (\_ _ -> None) op1 op1

or' :: Context -> Int -> Operand -> Operand -> State (Pred,VCS) ()
or' ctxt i_a op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (SE_Op (Or (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2

and' :: Context -> Int -> Operand -> Operand -> State (Pred,VCS) ()
and' ctxt i_a op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (SE_Op (And (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2


not' :: Context -> Int -> Operand -> State (Pred,VCS) ()
not' ctxt i_a op1 = do
  e1 <- read_operand ctxt op1
  write_operand ctxt i_a op1 (SE_Op (Not (8 * operand_size op1)) [e1])
  write_flags (\_ _ -> None) op1 op1


xor ctxt i_a op1 op2 = do
  if op1 == op2 then
    mov_with_func ctxt i_a (\ctxt x -> SE_Immediate 0) True op1 op2
  else do
    e1 <- read_operand ctxt op1
    e2 <- read_operand ctxt op2
    write_operand ctxt i_a op1 (SE_Op (Xor (8 * operand_size op1)) [e1,e2])
    write_flags (\_ _ -> None) op1 op2

{--
setxx :: Context -> Operand -> State (Pred,VCS) ()
setxx ctxt op1 = do
  e1 <- read_operand ctxt op1
  write_operand ctxt op1 (SE_Op SetXX [e1])
  write_flags (\_ _ -> None) op1 op1
--}
setxx ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False -- TODO ND 0/1

pxor = xor --TODO flags?

pand ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pandn ctxt i_a = mov_with_func ctxt i_a mk_bottom False

por ctxt i_a = mov_with_func ctxt i_a mk_bottom False

ptest ctxt i_a = mov_with_func ctxt i_a mk_bottom True


xorpd = xor --TODO flags?


xorps = xor -- TODO flags?

andpd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

andnpd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

orpd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

subpd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

addpd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

subss ctxt i_a = mov_with_func ctxt i_a mk_bottom False

addss ctxt i_a = mov_with_func ctxt i_a mk_bottom False

mulss ctxt i_a = mov_with_func ctxt i_a mk_bottom False

divss ctxt i_a = mov_with_func ctxt i_a mk_bottom False

roundss ctxt i_a = mov_with_func ctxt i_a mk_bottom False

subsd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

addsd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

mulsd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

divsd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

roundsd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

bt :: Context -> Int -> Operand -> Operand -> State (Pred,VCS) ()
bt ctxt i_a op1 op2 = do
  write_flags (\_ _ -> None) op1 op2

btc ctxt i_a = mov_with_func ctxt i_a mk_bottom True

btr ctxt i_a = mov_with_func ctxt i_a mk_bottom True

bsr ctxt i_a op1 op2 = do
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 $ SE_Op (Bsr (8 * operand_size op1)) [e2]
  write_flags (\_ _ -> None) op1 op2

bsf ctxt i_a = mov_with_func ctxt i_a mk_bottom True

bts ctxt i_a = mov_with_func ctxt i_a mk_bottom True

paddd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

paddb ctxt i_a = mov_with_func ctxt i_a mk_bottom False

paddq ctxt i_a = mov_with_func ctxt i_a mk_bottom False

psubd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

psubb ctxt i_a = mov_with_func ctxt i_a mk_bottom False

psubq ctxt i_a = mov_with_func ctxt i_a mk_bottom False

psrld ctxt i_a = mov_with_func ctxt i_a mk_bottom False

psrlw ctxt i_a = mov_with_func ctxt i_a mk_bottom False

psrldq ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pslldq ctxt i_a = mov_with_func ctxt i_a mk_bottom False

psllq ctxt i_a = mov_with_func ctxt i_a mk_bottom False

psrlq ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pmulld ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pminud ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pminsd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pmaxud ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pmaxuq ctxt i_a = mov_with_func ctxt i_a mk_bottom False

psubusb ctxt i_a = mov_with_func ctxt i_a mk_bottom False

psubusw ctxt i_a = mov_with_func ctxt i_a mk_bottom False

packssdw ctxt i_a = mov_with_func ctxt i_a mk_bottom False

packsswb ctxt i_a = mov_with_func ctxt i_a mk_bottom False

cvtss2sd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

cvtsd2ss ctxt i_a = mov_with_func ctxt i_a mk_bottom False

cvtsi2sd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

cvtsi2ss ctxt i_a = mov_with_func ctxt i_a mk_bottom False

cvttss2si ctxt i_a = mov_with_func ctxt i_a mk_bottom False

cvttsd2si ctxt i_a = mov_with_func ctxt i_a mk_bottom False

cvttpd2dq ctxt i_a = mov_with_func ctxt i_a mk_bottom False

cvtdq2pd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

is_st_reg_operand (Reg r) = take 2 (show r) == "ST"
is_st_reg_operand _       = False

fst' ctxt i_a op1 =
  if is_st_reg_operand op1 then
    return ()
  else
    write_operand ctxt i_a op1 $ Bottom (FromSemantics $ S.empty)

fstp ctxt i_a op1 =
  if is_st_reg_operand op1 then
    return ()
  else
    write_operand ctxt i_a op1 $ Bottom (FromSemantics $ S.empty)

fld ctxt i_a op1 = return ()

fld1 ctxt i_a = return ()

fldz ctxt i_a = return ()

fild ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False

fxch ctxt i_a = return ()

fchs ctxt i_a = return ()
  
fucom ctxt i_a = return ()

fucomi ctxt i_a = return ()

fucomip ctxt i_a = return ()

fucomp ctxt i_a = return ()

fucompi ctxt i_a = return ()

fucompp ctxt i_a = return ()

finit ctxt i_a = return ()

fninit ctxt i_a = return ()

fnstcw ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False

fstcw ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False


-- We do not model state changes to ST(_) registers, hence the following semantics
-- instead of:
--    mov_with_func ctxt i_a mk_bottom False (Reg ST0) op1
fadd1 ctxt i_a op1 = return ()

fadd2 ctxt i_a op1 op2 = return ()

fmul1 ctxt i_a op1 = return ()

fmul2 ctxt i_a op1 op2 = return ()

fmulp1 ctxt i_a op1 = return ()

fmulp2 ctxt i_a op1 op2 = return ()

fdivr1 ctxt i_a op1 = return ()

fdivr2 ctxt i_a op1 op2 = return ()

fdivrp1 ctxt i_a op1 = return ()

fdivrp2 ctxt i_a op1 op2 = return ()

fisub ctxt i_a op1 = return ()

fcmovxx ctxt i_a = return ()

fisttp ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False

idiv ctxt i_a = mov_with_func1 ctxt i_a mk_bottom True

div1 :: Context -> Int -> Operand -> State (Pred,VCS) ()
div1 ctxt i_a op1 = do
  let srcs = case operand_size op1 of
               8 -> [RDX,RAX]
               4 -> [EDX,EAX]
               2 -> [DX,AX]
               1 -> [AH,AL]
  e1 <- read_operand ctxt op1
  src0 <- read_operand ctxt (Reg $ srcs !! 0)
  src1 <- read_operand ctxt (Reg $ srcs !! 1)
  
  write_operand ctxt i_a (Reg $ srcs !! 0) $ SE_Op (Div_Rem (8 * operand_size op1)) [src0,src1,e1]
  write_operand ctxt i_a (Reg $ srcs !! 1) $ SE_Op (Div (8 * operand_size op1)) [src0,src1,e1]
  write_flags (\_ _ -> None) op1 op1


cdq :: Context -> Int -> State (Pred,VCS) ()
cdq ctxt i_a = do
  e1 <- read_operand ctxt (Reg EAX)
  write_operand ctxt i_a (Reg EDX) (mk_bottom ctxt [e1])

cqo :: Context -> Int -> State (Pred,VCS) ()
cqo ctxt i_a = do
  e1 <- read_operand ctxt (Reg RAX)
  write_operand ctxt i_a (Reg RDX) (mk_bottom ctxt [e1])

cdqe :: Context -> Int -> State (Pred,VCS) ()
cdqe ctxt i_a = do
  e1 <- read_operand ctxt (Reg EAX)
  write_operand ctxt i_a (Reg RAX) (SE_SExtend 32 64 e1)

cbw ctxt i_a = mov_with_func1 ctxt i_a mk_bottom True (Reg AX)

cwde ctxt i_a = mov_with_func1 ctxt i_a mk_bottom True (Reg EAX)





shl :: Context -> Int -> Operand -> Operand -> State (Pred,VCS) ()
shl ctxt i_a op1 op2@(Immediate i) = do
  e1 <- read_operand ctxt op1
  write_operand ctxt i_a op1 (SE_Op (Times (8 * operand_size op1)) [e1,SE_Immediate $ 2^i])
  write_flags (\_ _ -> None) op1 op2
shl ctxt i_a op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (SE_Op (Shl (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2

shr :: Context -> Int -> Operand -> Operand -> State (Pred,VCS) ()
shr ctxt i_a op1 op2@(Immediate i) = do
  e1 <- read_operand ctxt op1
  write_operand ctxt i_a op1 (SE_Op (Udiv (8 * operand_size op1)) [e1,SE_Immediate $ 2^i])
  write_flags (\_ _ -> None) op1 op2
shr ctxt i_a op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (SE_Op (Shr (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2

sar ctxt i_a op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (SE_Op (Sar (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2


shld ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False -- TODO

shrd ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False -- TODO


rol :: Context -> Int -> Operand -> Operand -> State (Pred,VCS) ()
rol ctxt i_a op1 op2@(Immediate i) = do
  e1 <- read_operand ctxt op1
  write_operand ctxt i_a op1 $ SE_Op (Rol (8 * operand_size op1)) [e1,SE_Immediate $ 2^i]
  write_flags (\_ _ -> None) op1 op2
rol ctxt i_a op1 op2 = mov_with_func ctxt i_a mk_bottom True op1 op2

ror :: Context -> Int -> Operand -> Operand -> State (Pred,VCS) ()
ror ctxt i_a op1 op2@(Immediate i) = do
  e1 <- read_operand ctxt op1
  write_operand ctxt i_a op1 $ SE_Op (Ror (8 * operand_size op1)) [e1,SE_Immediate $ 2^i]
  write_flags (\_ _ -> None) op1 op2
ror ctxt i_a op1 op2 = mov_with_func ctxt i_a mk_bottom True op1 op2

{-
adc :: Context -> Operand -> Operand -> State (Pred,VCS) ()
adc ctxt op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 $ SE_Binop "adc" [e1,e2]
  write_flags (\_ _ -> None) op1 op2

sbb :: Context -> Operand -> Operand -> State (Pred,VCS) ()
sbb ctxt op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 $ SE_Binop "sbb" [e1,e2]
  write_flags (\_ _ -> None) op1 op2
-}
adc ctxt i_a = mov_with_func ctxt i_a mk_bottom True

sbb ctxt i_a = mov_with_func ctxt i_a mk_bottom True

mul1 ctxt i_a = mov_with_func1 ctxt i_a mk_bottom True --TODO 

mul2 ctxt i_a = mov_with_func ctxt i_a mk_bottom True --TODO 

imul1 :: Context -> Int -> Operand -> State (Pred,VCS) ()
imul1 ctxt i_a op1 = do
  let srcs = case operand_size op1 of
               8 -> [RDX,RAX]
               4 -> [EDX,EAX]
               2 -> [DX,AX]
               1 -> [AH,AL]
  e1 <- read_operand ctxt op1
  src0 <- read_operand ctxt (Reg $ srcs !! 0)
  src1 <- read_operand ctxt (Reg $ srcs !! 1)
  
  write_operand ctxt i_a (Reg $ srcs !! 0) $ mk_bottom ctxt [src1,e1] -- high part of multiplication
  write_operand ctxt i_a (Reg $ srcs !! 1) $ SE_Op (Times (8 * operand_size op1)) [src1,e1]
  write_flags (\_ _ -> None) op1 op1

imul2 :: Context -> Int -> Operand -> Operand -> State (Pred,VCS) ()
imul2 ctxt i_a op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (SE_Op (Times (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2

imul3 :: Context -> Int -> Operand -> Operand -> Operand -> State (Pred,VCS) ()
imul3 ctxt i_a op0 op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op0 (SE_Op (Times (8 * operand_size op0)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2



bswap :: Context -> Int -> Operand -> State (Pred,VCS) ()
bswap ctxt i_a op1 = do
  e1 <- read_operand ctxt op1
  write_operand ctxt i_a op1 $ SE_Op (Bswap (8 * operand_size op1)) [e1]
  write_flags (\_ _ -> None) op1 op1

pextrb :: Context -> Int -> Operand -> Operand -> Operand -> State (Pred,VCS) ()
pextrb ctxt i_a op0 op1 op2 = do
  e0 <- read_operand ctxt op0
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op0 (SE_Op (Pextr 8) [e0,e1,e2])

pextrd :: Context -> Int -> Operand -> Operand -> Operand -> State (Pred,VCS) ()
pextrd ctxt i_a op0 op1 op2 = do
  e0 <- read_operand ctxt op0
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op0 (SE_Op (Pextr 32) [e0,e1,e2])

pextrq :: Context -> Int -> Operand -> Operand -> Operand -> State (Pred,VCS) ()
pextrq ctxt i_a op0 op1 op2 = do
  e0 <- read_operand ctxt op0
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op0 (SE_Op (Pextr 64) [e0,e1,e2])

haddpd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pinsrq ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

pinsrd ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

pshufb ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pshufd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pshuflw ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

pclmulqdq ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

pcmpeqb ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pcmpeqd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pcmpgtb ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pcmpgtd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

movmskps ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pmovsxdq ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pmovzxdq ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pmovsxbd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pmovzxbd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

movmskpd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

unpcklps ctxt i_a = mov_with_func ctxt i_a mk_bottom False

cmpltsd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

cmpeqsd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

cmpneqsd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

punpcklqdq ctxt i_a = mov_with_func ctxt i_a mk_bottom False

punpckldq ctxt i_a = mov_with_func ctxt i_a mk_bottom False

punpcklbw ctxt i_a = mov_with_func ctxt i_a mk_bottom False

blendvps ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

extractps ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

movsd_string ctxt prefix op1 op2 = return () -- TODO 

movsq ctxt prefix op1 op2 = return () -- TODO 

x86_in ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False

x86_out ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False

cli ctxt i_a = return ()

clts ctxt i_a = return ()

cpuid ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False (Reg RAX)

invpcid ctxt i_a = return ()

lgdt ctxt i_a = return ()

lidt ctxt i_a = return ()

lldt ctxt i_a = return ()

ltr ctxt i_a = return ()

rdmsr ctxt i_a = do
  mov_with_func1 ctxt i_a mk_bottom False (Reg RAX)
  mov_with_func1 ctxt i_a mk_bottom False (Reg RDX)

wrmsr ctxt i_a = do
  mov_with_func1 ctxt i_a mk_bottom False (Reg RAX)
  mov_with_func1 ctxt i_a mk_bottom False (Reg RDX)

rdtsc ctxt i_a = do
  mov_with_func1 ctxt i_a mk_bottom False (Reg RAX)
  mov_with_func1 ctxt i_a mk_bottom False (Reg RDX)

swapgs ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False (Reg GS)

xsetbv ctxt i_a = return ()

xsaveopt ctxt i_a = return ()

xrstor ctxt i_a = return ()

wrfsbase ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False (Reg FS)

wrgsbase ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False (Reg GS)



xadd :: Context -> Int -> Operand -> Operand -> State (Pred,VCS) ()
xadd ctxt i_a op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (SE_Op (Plus (operand_size op1)) [e1,e2])
  write_operand ctxt i_a op2 e1
  write_flags (\_ _ -> None) op1 op2

cmpxchg ctxt i_a = mov_with_func ctxt i_a mk_bottom True


tau_i :: Context -> Instr -> State (Pred,VCS) ()
tau_i ctxt (Instr i_a _ PUSH     (Just op1) _          _ _ _)   = push   ctxt i_a op1
tau_i ctxt (Instr i_a _ POP      (Just op1) _          _ _ _)   = pop    ctxt i_a op1
tau_i ctxt (Instr i_a _ LEA      (Just op1) (Just op2) _ _ _)   = lea    ctxt i_a op1 op2

tau_i ctxt i@(Instr i_a _ CALL     _          _          _ _ _) = call   ctxt i
tau_i ctxt   (Instr i_a _ RET      _          _          _ _ _) = ret    ctxt i_a 
tau_i ctxt   (Instr i_a _ IRETQ    _          _          _ _ _) = ret    ctxt i_a 
tau_i ctxt   (Instr i_a _ SYSRET   _          _          _ _ _) = sysret ctxt i_a
tau_i ctxt i@(Instr i_a _ JMP      (Just op1) _          _ _ _) = jmp    ctxt i
tau_i ctxt   (Instr i_a _ LEAVE    _          _          _ _ _) = leave  ctxt i_a

tau_i ctxt (Instr i_a _       MOV      (Just op1) (Just op2) _ _ _) = mov    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _       MOVZX    (Just op1) (Just op2) _ _ _) = movzx  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _       MOVSX    (Just op1) (Just op2) _ _ _) = movsx  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _       MOVSXD   (Just op1) (Just op2) _ _ _) = movsxd ctxt i_a op1 op2
tau_i ctxt (Instr i_a _       MOVAPS   (Just op1) (Just op2) _ _ _) = movaps ctxt i_a op1 op2
tau_i ctxt (Instr i_a _       MOVAPD   (Just op1) (Just op2) _ _ _) = movapd ctxt i_a op1 op2
tau_i ctxt (Instr i_a _       MOVABS   (Just op1) (Just op2) _ _ _) = movabs ctxt i_a op1 op2
tau_i ctxt (Instr i_a _       MOVUPD   (Just op1) (Just op2) _ _ _) = movupd ctxt i_a op1 op2
tau_i ctxt (Instr i_a _       MOVUPS   (Just op1) (Just op2) _ _ _) = movups ctxt i_a op1 op2
tau_i ctxt (Instr i_a _       MOVDQU   (Just op1) (Just op2) _ _ _) = movdqu ctxt i_a op1 op2
tau_i ctxt (Instr i_a _       MOVDQA   (Just op1) (Just op2) _ _ _) = movdqa ctxt i_a op1 op2
tau_i ctxt (Instr i_a _       MOVD     (Just op1) (Just op2) _ _ _) = movd   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _       MOVQ     (Just op1) (Just op2) _ _ _) = movq   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _       MOVLPD   (Just op1) (Just op2) _ _ _) = movlpd ctxt i_a op1 op2
tau_i ctxt (Instr i_a _       MOVLPS   (Just op1) (Just op2) _ _ _) = movlps ctxt i_a op1 op2
tau_i ctxt (Instr i_a Nothing MOVSD    (Just op1) (Just op2) _ _ _) = movsd  ctxt i_a op1 op2
tau_i ctxt (Instr i_a Nothing MOVSS    (Just op1) (Just op2) _ _ _) = movss  ctxt i_a op1 op2

tau_i ctxt (Instr i_a _ CMOVO    (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVNO   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVS    (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVNS   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVE    (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVZ    (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVNE   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVNZ   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVB    (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVNAE  (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVC    (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVNB   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVAE   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVNC   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVBE   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVNA   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVA    (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVNBE  (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVL    (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVNGE  (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVG    (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVGE   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVNL   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVLE   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVNG   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVNLE  (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVP    (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVPE   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVNP   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMOVPO   (Just op1) (Just op2) _ _ _) = cmov   ctxt i_a op1 op2

tau_i ctxt (Instr i_a _ CDQ      Nothing    _          _ _ _) = cdq    ctxt i_a
tau_i ctxt (Instr i_a _ CDQE     Nothing    _          _ _ _) = cdqe   ctxt i_a
tau_i ctxt (Instr i_a _ CQO      Nothing    _          _ _ _) = cqo    ctxt i_a
tau_i ctxt (Instr i_a _ CBW      Nothing    _          _ _ _) = cbw    ctxt i_a
tau_i ctxt (Instr i_a _ CWDE     Nothing    _          _ _ _) = cwde   ctxt i_a

tau_i ctxt (Instr i_a _ ADD      (Just op1) (Just op2) _ _ _) = add    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ SUB      (Just op1) (Just op2) _ _ _) = sub    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ NEG      (Just op1) Nothing    _ _ _) = neg    ctxt i_a op1
tau_i ctxt (Instr i_a _ INC      (Just op1) _          _ _ _) = inc    ctxt i_a op1
tau_i ctxt (Instr i_a _ DEC      (Just op1) _          _ _ _) = dec    ctxt i_a op1
tau_i ctxt (Instr i_a _ SHL      (Just op1) (Just op2) _ _ _) = shl    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ SHL      (Just op1) Nothing    _ _ _) = shl    ctxt i_a op1 (Immediate 1)

tau_i ctxt (Instr i_a _ NOP      _          _          _ _ _) = nop    ctxt i_a
tau_i ctxt (Instr i_a _ UD2      _          _          _ _ _) = ud2    ctxt i_a
tau_i ctxt (Instr i_a _ HLT      _          _          _ _ _) = hlt    ctxt i_a

tau_i ctxt (Instr i_a _ WAIT     _          _          _ _ _) = wait    ctxt i_a
tau_i ctxt (Instr i_a _ MFENCE   _          _          _ _ _) = mfence  ctxt i_a
tau_i ctxt (Instr i_a _ CLFLUSH  _          _          _ _ _) = clflush ctxt i_a

tau_i ctxt (Instr i_a _ ADC      (Just op1) (Just op2) _ _ _)          = adc    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ SBB      (Just op1) (Just op2) _ _ _)          = sbb    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ ROL      (Just op1) (Just op2) _ _ _)          = rol    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ ROR      (Just op1) (Just op2) _ _ _)          = ror    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ SHR      (Just op1) (Just op2) _ _ _)          = shr    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ SHR      (Just op1) Nothing    _ _ _)          = shr    ctxt i_a op1 (Immediate 1)
tau_i ctxt (Instr i_a _ SAR      (Just op1) (Just op2) _ _ _)          = sar    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ SAR      (Just op1) Nothing    _ _ _)          = sar    ctxt i_a op1 (Immediate 1)
tau_i ctxt (Instr i_a _ MUL      (Just op1) Nothing    _ _ _)          = mul1  ctxt i_a op1
tau_i ctxt (Instr i_a _ MUL      (Just op1) (Just op2) Nothing _ _)    = mul2  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ IMUL     (Just op1) Nothing    _ _ _)          = imul1  ctxt i_a op1
tau_i ctxt (Instr i_a _ IMUL     (Just op1) (Just op2) Nothing _ _)    = imul2  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ IMUL     (Just op1) (Just op2) (Just op3) _ _) = imul3  ctxt i_a op1 op2 op3
tau_i ctxt (Instr i_a _ IDIV     (Just op1) Nothing    _ _ _)          = idiv   ctxt i_a op1
tau_i ctxt (Instr i_a _ DIV      (Just op1) Nothing    _ _ _)          = div1   ctxt i_a op1
tau_i ctxt (Instr i_a _ SHLD     (Just op1) (Just op2) (Just op3) _ _) = shld ctxt i_a op1 op2 op3
tau_i ctxt (Instr i_a _ SHRD     (Just op1) (Just op2) (Just op3) _ _) = shrd ctxt i_a op1 op2 op3

tau_i ctxt (Instr i_a _ CMP      (Just op1) (Just op2) _ _ _) = cmp    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ TEST     (Just op1) (Just op2) _ _ _) = test   ctxt i_a op1 op2

tau_i ctxt (Instr i_a _ XOR      (Just op1) (Just op2) _ _ _) = xor    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ OR       (Just op1) (Just op2) _ _ _) = or'    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ AND      (Just op1) (Just op2) _ _ _) = and'   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ NOT      (Just op1) _          _ _ _) = not'   ctxt i_a op1
tau_i ctxt (Instr i_a _ BT       (Just op1) (Just op2) _ _ _) = bt     ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ BTC      (Just op1) (Just op2) _ _ _) = btc    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ BTR      (Just op1) (Just op2) _ _ _) = btr    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ BSR      (Just op1) (Just op2) _ _ _) = bsr    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ BSF      (Just op1) (Just op2) _ _ _) = bsf    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ BTS      (Just op1) (Just op2) _ _ _) = bts    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ BSWAP    (Just op1) _          _ _ _) = bswap  ctxt i_a op1



tau_i ctxt (Instr i_a _ SETO     (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETNO    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETS     (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETNS    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETE     (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETZ     (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETNE    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETNZ    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETB     (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETNAE   (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETC     (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETNB    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETAE    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETNC    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETBE    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETNA    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETA     (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETNBE   (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETL     (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETNGE   (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETGE    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETNL    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETLE    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETNG    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETG     (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETNLE   (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETP     (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETPE    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETNP    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1
tau_i ctxt (Instr i_a _ SETPO    (Just op1) Nothing    _ _ _) = setxx  ctxt i_a op1


tau_i ctxt (Instr i_a _ XORPS     (Just op1) (Just op2) _ _ _)            = xorps      ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ MOVMSKPS  (Just op1) (Just op2) _ _ _)            = movmskps   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ UNPCKLPS  (Just op1) (Just op2) _ _ _)            = unpcklps   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ BLENDVPS  (Just op1) (Just op2) Nothing    _ _)   = blendvps   ctxt i_a op1 op2 (Reg XMM0)
tau_i ctxt (Instr i_a _ BLENDVPS  (Just op1) (Just op2) (Just op3) _ _)   = blendvps   ctxt i_a op1 op2 op3
tau_i ctxt (Instr i_a _ EXTRACTPS (Just op1) (Just op2) (Just op3) _ _)   = extractps  ctxt i_a op1 op2 op3

tau_i ctxt (Instr i_a _ XORPD    (Just op1) (Just op2) _ _ _) = xorpd    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ ANDPD    (Just op1) (Just op2) _ _ _) = andpd    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ ANDNPD   (Just op1) (Just op2) _ _ _) = andnpd   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ ORPD     (Just op1) (Just op2) _ _ _) = orpd     ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ SUBPD    (Just op1) (Just op2) _ _ _) = subpd    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ ADDPD    (Just op1) (Just op2) _ _ _) = addpd    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ HADDPD   (Just op1) (Just op2) _ _ _) = haddpd   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ MOVMSKPD (Just op1) (Just op2) _ _ _) = movmskpd   ctxt i_a op1 op2

tau_i ctxt (Instr i_a _ POR      (Just op1) (Just op2) _ _ _)            = por    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PAND     (Just op1) (Just op2) _ _ _)            = pand   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PANDN    (Just op1) (Just op2) _ _ _)            = pandn  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PXOR     (Just op1) (Just op2) _ _ _)            = pxor   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PTEST    (Just op1) (Just op2) _ _ _)            = ptest  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PUNPCKLQDQ (Just op1) (Just op2) _ _ _)          = punpcklqdq   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PUNPCKLBW  (Just op1) (Just op2) _ _ _)          = punpcklbw    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PUNPCKLDQ  (Just op1) (Just op2) _ _ _)          = punpckldq    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PMOVSXDQ   (Just op1) (Just op2) _ _ _)          = pmovsxdq     ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PMOVZXDQ   (Just op1) (Just op2) _ _ _)          = pmovzxdq     ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PMOVSXBD   (Just op1) (Just op2) _ _ _)          = pmovsxbd     ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PMOVZXBD   (Just op1) (Just op2) _ _ _)          = pmovzxbd     ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PSHUFB     (Just op1) (Just op2) _ _ _)          = pshufb       ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PSHUFD     (Just op1) (Just op2) _ _ _)          = pshufd       ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PCMPEQB    (Just op1) (Just op2) _ _ _)          = pcmpeqb      ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PCMPEQD    (Just op1) (Just op2) _ _ _)          = pcmpeqd      ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PCMPGTB    (Just op1) (Just op2) _ _ _)          = pcmpgtb      ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PCMPGTD    (Just op1) (Just op2) _ _ _)          = pcmpgtd      ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PADDD      (Just op1) (Just op2) _ _ _)          = paddd        ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PADDB      (Just op1) (Just op2) _ _ _)          = paddb        ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PADDQ      (Just op1) (Just op2) _ _ _)          = paddq        ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PSUBD      (Just op1) (Just op2) _ _ _)          = psubd        ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PSUBB      (Just op1) (Just op2) _ _ _)          = psubb        ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PSUBQ      (Just op1) (Just op2) _ _ _)          = psubq        ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PMULLD     (Just op1) (Just op2) _ _ _)          = pmulld       ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PMINSD     (Just op1) (Just op2) _ _ _)          = pminsd       ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PMINUD     (Just op1) (Just op2) _ _ _)          = pminud       ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PMAXUD     (Just op1) (Just op2) _ _ _)          = pmaxud       ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PMAXUQ     (Just op1) (Just op2) _ _ _)          = pmaxuq       ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PSRLD      (Just op1) (Just op2) _ _ _)          = psrld        ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PSRLW      (Just op1) (Just op2) _ _ _)          = psrlw        ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PSRLDQ     (Just op1) (Just op2) _ _ _)          = psrldq       ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PSLLDQ     (Just op1) (Just op2) _ _ _)          = pslldq       ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PSLLQ      (Just op1) (Just op2) _ _ _)          = psllq        ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PSRLQ      (Just op1) (Just op2) _ _ _)          = psrlq        ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PSUBUSB    (Just op1) (Just op2) _ _ _)          = psubusb      ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PSUBUSW    (Just op1) (Just op2) _ _ _)          = psubusw      ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PINSRQ     (Just op1) (Just op2) (Just op3) _ _) = pinsrq       ctxt i_a op1 op2 op3
tau_i ctxt (Instr i_a _ PINSRD     (Just op1) (Just op2) (Just op3) _ _) = pinsrd       ctxt i_a op1 op2 op3
tau_i ctxt (Instr i_a _ PEXTRB     (Just op1) (Just op2) (Just op3) _ _) = pextrb       ctxt i_a op1 op2 op3
tau_i ctxt (Instr i_a _ PEXTRD     (Just op1) (Just op2) (Just op3) _ _) = pextrd       ctxt i_a op1 op2 op3
tau_i ctxt (Instr i_a _ PEXTRQ     (Just op1) (Just op2) (Just op3) _ _) = pextrq       ctxt i_a op1 op2 op3
tau_i ctxt (Instr i_a _ PSHUFLW    (Just op1) (Just op2) (Just op3) _ _) = pshuflw      ctxt i_a op1 op2 op3
tau_i ctxt (Instr i_a _ PCLMULQDQ  (Just op1) (Just op2) (Just op3) _ _) = pclmulqdq    ctxt i_a op1 op2 op3
tau_i ctxt (Instr i_a _ PACKSSDW   (Just op1) (Just op2) _ _ _)          = packssdw     ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ PACKSSWB   (Just op1) (Just op2) _ _ _)          = packsswb     ctxt i_a op1 op2


tau_i ctxt (Instr i_a _ SUBSS    (Just op1) (Just op2) _ _ _) = subss    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ ADDSS    (Just op1) (Just op2) _ _ _) = addss    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ DIVSS    (Just op1) (Just op2) _ _ _) = divss    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ MULSS    (Just op1) (Just op2) _ _ _) = mulss    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ ROUNDSS  (Just op1) (Just op2) _ _ _) = roundss  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ UCOMISS  (Just op1) (Just op2) _ _ _) = ucomiss  ctxt i_a op1 op2

tau_i ctxt (Instr i_a _ SUBSD    (Just op1) (Just op2) _ _ _) = subsd    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ ADDSD    (Just op1) (Just op2) _ _ _) = addsd    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ DIVSD    (Just op1) (Just op2) _ _ _) = divsd    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ MULSD    (Just op1) (Just op2) _ _ _) = mulsd    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ ROUNDSD  (Just op1) (Just op2) _ _ _) = roundsd  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ UCOMISD  (Just op1) (Just op2) _ _ _) = ucomisd  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMPLTSD  (Just op1) (Just op2) _ _ _) = cmpltsd  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMPEQSD  (Just op1) (Just op2) _ _ _) = cmpeqsd  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CMPNEQSD (Just op1) (Just op2) _ _ _) = cmpneqsd ctxt i_a op1 op2



tau_i ctxt (Instr i_a _ CVTSS2SD  (Just op1) (Just op2) _ _ _) = cvtss2sd  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CVTSI2SS  (Just op1) (Just op2) _ _ _) = cvtsi2ss  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CVTSI2SD  (Just op1) (Just op2) _ _ _) = cvtsi2sd  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CVTSD2SS  (Just op1) (Just op2) _ _ _) = cvtsd2ss  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CVTTSS2SI (Just op1) (Just op2) _ _ _) = cvttss2si ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CVTTSD2SI (Just op1) (Just op2) _ _ _) = cvttsd2si ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CVTTPD2DQ (Just op1) (Just op2) _ _ _) = cvttpd2dq ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ CVTDQ2PD  (Just op1) (Just op2) _ _ _) = cvtdq2pd  ctxt i_a op1 op2

tau_i ctxt (Instr i_a _ FST      (Just op1) _          _ _ _)  = fst'     ctxt i_a op1
tau_i ctxt (Instr i_a _ FSTP     (Just op1) _          _ _ _)  = fstp     ctxt i_a op1
tau_i ctxt (Instr i_a _ FINIT    _          _          _ _ _)  = finit    ctxt i_a
tau_i ctxt (Instr i_a _ FNINIT   _          _          _ _ _)  = fninit   ctxt i_a
tau_i ctxt (Instr i_a _ FNSTCW   (Just op1) _          _ _ _)  = fnstcw   ctxt i_a op1
tau_i ctxt (Instr i_a _ FSTCW    (Just op1) _          _ _ _)  = fstcw    ctxt i_a op1
tau_i ctxt (Instr i_a _ FLD      (Just op1) _          _ _ _)  = fld      ctxt i_a op1
tau_i ctxt (Instr i_a _ FLD1     Nothing    _          _ _ _)  = fld1     ctxt i_a
tau_i ctxt (Instr i_a _ FLDZ     Nothing    _          _ _ _)  = fldz     ctxt i_a
tau_i ctxt (Instr i_a _ FILD     (Just op1) Nothing    _ _ _)  = fild     ctxt i_a op1
tau_i ctxt (Instr i_a _ FUCOM    _          _          _ _ _)  = fucom    ctxt i_a
tau_i ctxt (Instr i_a _ FUCOMI   _          _          _ _ _)  = fucomi   ctxt i_a
tau_i ctxt (Instr i_a _ FUCOMIP  _          _          _ _ _)  = fucomip  ctxt i_a
tau_i ctxt (Instr i_a _ FUCOMP   _          _          _ _ _)  = fucomp   ctxt i_a
tau_i ctxt (Instr i_a _ FUCOMPI  _          _          _ _ _)  = fucompi  ctxt i_a
tau_i ctxt (Instr i_a _ FUCOMPP  _          _          _ _ _)  = fucompp  ctxt i_a
tau_i ctxt (Instr i_a _ FCMOVB   _          _          _ _ _)  = fcmovxx  ctxt i_a
tau_i ctxt (Instr i_a _ FCMOVBE  _          _          _ _ _)  = fcmovxx  ctxt i_a
tau_i ctxt (Instr i_a _ FCMOVE   _          _          _ _ _)  = fcmovxx  ctxt i_a
tau_i ctxt (Instr i_a _ FCMOVNE  _          _          _ _ _)  = fcmovxx  ctxt i_a
tau_i ctxt (Instr i_a _ FCMOVU   _          _          _ _ _)  = fcmovxx  ctxt i_a
tau_i ctxt (Instr i_a _ FCMOVNU  _          _          _ _ _)  = fcmovxx  ctxt i_a
tau_i ctxt (Instr i_a _ FCMOVNBE _          _          _ _ _)  = fcmovxx  ctxt i_a
tau_i ctxt (Instr i_a _ FADD     (Just op1) Nothing    _ _ _)  = fadd1    ctxt i_a op1
tau_i ctxt (Instr i_a _ FADD     (Just op1) (Just op2) _ _ _)  = fadd2    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ FMUL     (Just op1) Nothing    _ _ _)  = fmul1    ctxt i_a op1
tau_i ctxt (Instr i_a _ FMUL     (Just op1) (Just op2) _ _ _)  = fmul2    ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ FMULP    (Just op1) Nothing    _ _ _)  = fmulp1   ctxt i_a op1
tau_i ctxt (Instr i_a _ FMULP    (Just op1) (Just op2) _ _ _)  = fmulp2   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ FDIVR    (Just op1) Nothing    _ _ _)  = fdivr1   ctxt i_a op1
tau_i ctxt (Instr i_a _ FDIVR    (Just op1) (Just op2) _ _ _)  = fdivr2   ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ FDIVRP   (Just op1) Nothing    _ _ _)  = fdivrp1  ctxt i_a op1
tau_i ctxt (Instr i_a _ FDIVRP   (Just op1) (Just op2) _ _ _)  = fdivrp2  ctxt i_a op1 op2
tau_i ctxt (Instr i_a _ FISUB    (Just op1) Nothing    _ _ _)  = fisub    ctxt i_a op1
tau_i ctxt (Instr i_a _ FISTTP   (Just op1) Nothing    _ _ _)  = fisttp   ctxt i_a op1
tau_i ctxt (Instr i_a _ FXCH     (Just op1) _          _ _ _)  = fxch     ctxt i_a
tau_i ctxt (Instr i_a _ FCHS     Nothing    _          _ _ _)  = fchs     ctxt i_a


tau_i ctxt (Instr i_a _ CPUID    Nothing    _          _ _ _)  = cpuid    ctxt i_a
tau_i ctxt (Instr i_a _ RDMSR    Nothing    _          _ _ _)  = rdmsr    ctxt i_a
tau_i ctxt (Instr i_a _ WRMSR    Nothing    _          _ _ _)  = wrmsr    ctxt i_a
tau_i ctxt (Instr i_a _ RDTSC    Nothing    _          _ _ _)  = rdtsc    ctxt i_a
tau_i ctxt (Instr i_a _ IN       (Just op1) _          _ _ _)  = x86_in   ctxt i_a op1
tau_i ctxt (Instr i_a _ OUT      (Just op1) _          _ _ _)  = x86_out  ctxt i_a op1
tau_i ctxt (Instr i_a _ CLI      Nothing    _          _ _ _)  = cli      ctxt i_a
tau_i ctxt (Instr i_a _ CLTS     Nothing    _          _ _ _)  = clts     ctxt i_a
tau_i ctxt (Instr i_a _ INVPCID  _          _          _ _ _)  = invpcid  ctxt i_a
tau_i ctxt (Instr i_a _ LGDT     _          _          _ _ _)  = lgdt     ctxt i_a
tau_i ctxt (Instr i_a _ LIDT     _          _          _ _ _)  = lidt     ctxt i_a
tau_i ctxt (Instr i_a _ LLDT     _          _          _ _ _)  = lldt     ctxt i_a
tau_i ctxt (Instr i_a _ LTR      _          _          _ _ _)  = ltr      ctxt i_a
tau_i ctxt (Instr i_a _ SWAPGS   _          _          _ _ _)  = swapgs   ctxt i_a
tau_i ctxt (Instr i_a _ XSETBV   _          _          _ _ _)  = xsetbv   ctxt i_a
tau_i ctxt (Instr i_a _ XSAVEOPT _          _          _ _ _)  = xsaveopt ctxt i_a
tau_i ctxt (Instr i_a _ XRSTOR   _          _          _ _ _)  = xrstor   ctxt i_a
tau_i ctxt (Instr i_a _ WRFSBASE _          _          _ _ _)  = wrfsbase ctxt i_a
tau_i ctxt (Instr i_a _ WRGSBASE _          _          _ _ _)  = wrgsbase ctxt i_a



tau_i ctxt (Instr a Nothing XCHG (Just op1) (Just op2) _ _ _)        = xchg         ctxt a op1 op2

tau_i ctxt (Instr i_a (Just LOCK) XADD    (Just op1) (Just op2) _ _ _) = xadd         ctxt i_a op1 op2
tau_i ctxt (Instr i_a (Just LOCK) CMPXCHG (Just op1) (Just op2) _ _ _) = cmpxchg      ctxt i_a op1 op2

tau_i ctxt (Instr i_a (Just pre)  MOVSD   (Just op1) (Just op2) _ _ _) = movsd_string ctxt pre op1 op2
tau_i ctxt (Instr i_a (Just pre)  MOVSQ   (Just op1) (Just op2) _ _ _) = movsq        ctxt pre op1 op2

tau_i ctxt i =
  if is_jump (i_opcode i) || is_cond_jump (i_opcode i) then
    return ()
  else
    error $ "Unsupported instruction: " ++ show i

-- | Do predicate transformation over a block of instructions.
-- Does not take into account flags, commonly function @`tau_block`@ should be used.
tau_b :: Context -> [Instr] -> State (Pred,VCS) ()
tau_b ctxt []  = return ()
tau_b ctxt (i:is) = do
  write_rreg RIP (SE_Immediate $ fromIntegral $ i_addr i + i_size i)
  tau_i ctxt i
  tau_b ctxt is



-- TODO JE, other JMP aboves and JUMP lesses
add_jump_to_pred :: Instr -> Instr -> FlagStatus -> FlagStatus
add_jump_to_pred i0@(Instr _ _ JA (Just (Immediate trgt)) _ _ _ _) i1 flg =
  case flg of
    FS_CMP b o1 o2 -> if i_addr i1 == fromIntegral trgt then FS_CMP (Just False) o1 o2 else FS_CMP (Just True) o1 o2
    _ -> flg
add_jump_to_pred i0 i1 flg = flg


-- | Do predicate transformation over a basic block in a CFG.
-- Given an edge in the CFG from none block to another, perform predicate transformation.
-- Parameter insts' is needed to set the flags properly. If @Nothing@ is supplied, the flags are overapproximatively set to @`None`@.
tau_block ::
  Context           -- ^ The context
  -> [Instr]        -- ^ The instructions of the basic block
  -> Maybe [Instr]  -- ^ Optionally, the instructions of the next block if symbolically executing an edge in a CFG.
  -> Pred           -- ^ The predicate to be transformed
  -> (Pred, VCS)
tau_block ctxt insts insts' p@(Predicate eqs flg muddle_status) = 
  if insts == [] then
    (p, S.empty)
  else let
      addr                        = i_addr $ head insts
      eqs'                        = write_rip addr eqs
      (p'',vcs'')                 = execState (tau_b ctxt insts) $ (Predicate eqs' flg muddle_status, S.empty)
      Predicate eqs'' flgs'' im'' = p'' in
    case insts' of
      Nothing -> (p'', vcs'')
      Just (i':_) ->
        let addr'  = i_addr i' in
          (Predicate (write_rip addr' eqs'') (add_jump_to_pred (last insts) i' flgs'') im'', vcs'')
 where
  write_rip addr eqs = M.insert (SP_Reg RIP) (SE_Immediate $ fromIntegral addr) eqs






join_expr ctxt e0 e1 = join_exprs "join" ctxt $ map simp [e0,e1]
join_expr' ctxt p q e0 e1 = join_exprs ("join") ctxt $ map simp [e0,e1] -- \n" ++ show p ++ "\n\n" ++ show q



-- the join of two predicates
-- 1.) keep any key-value pair (sp,v) that is in both predicates
-- 2.) for any remaining key-value pair (sp,v) in either of the predicates, add (sp,Bottom)
-- 3.) the flag expression is kept based on strict equality
--
--
-- Assumes any statepart not currently in the state is unwritten to.
join_muddle_status Muddled _      = Muddled
join_muddle_status _ Muddled      = Muddled
join_muddle_status ExternalOnly _ = ExternalOnly
join_muddle_status _ ExternalOnly = ExternalOnly
join_muddle_status _ _            = Clean

temp_trace v v' sp = id -- if S.size (srcs_of_expr v'') == 0 && S.size (srcs_of_expr v) > 0 && S.size (srcs_of_expr v') > 0 then traceShow ("temp_trace",sp,v,v',v'') v'' else v''


join_preds ctxt p@(Predicate eqs0 flg0 muddle_status0) p'@(Predicate eqs1 flg1 muddle_status1) =
  let m    = M.mapWithKey mk_entry eqs0
      flg' = if flg0 == flg1 then flg0 else None
      ms'  = join_muddle_status muddle_status0 muddle_status1
      q    = Predicate m flg' ms' in
    foldr mk_entry' q $ M.toList eqs1
 where
  mk_entry sp v = 
    case find (\(sp',v') -> necessarily_equal_stateparts sp sp') $ M.toList eqs1 of
      Nothing ->
        if is_initial sp v then
          v
        else 
          let v' = evalState (read_sp ctxt sp) (p',S.empty) in
            temp_trace v v' sp $ join_expr' ctxt p p' v v'
      Just (sp',v') -> 
        if necessarily_equal v v' || v == v' then
          v
        else
          temp_trace v v' sp $ join_expr' ctxt p p' v v'

  mk_entry' (sp',v' ) q = 
    case find (\(sp,v) -> necessarily_equal_stateparts sp sp') $ M.toList eqs0 of
      Nothing -> 
        if is_initial sp' v' then
          fst $ execState (write_sp ctxt mk_mid (sp', v')) (q,S.empty)
        else
          let v = evalState (read_sp ctxt sp') (p,S.empty) in
            fst $ execState (write_sp ctxt mk_mid (sp', temp_trace v v' sp' $ join_expr' ctxt p p' v v')) (q,S.empty)
      Just _  -> q

  mk_mid = MemWriteFunction "joining" 0 -- never used


-- TODO in case of bot, check whether sources of P1 are larger than P0
implies_preds ctxt (Predicate eqs0 flg0 muddle_status0) (Predicate eqs1 flg1 muddle_status1) = 
 muddle_status0 == muddle_status1 && flg1 `elem` [None,flg0] && (all implied_by_eqs0 $ M.toList eqs1) -- && (all_sps_in_eqs1 $ M.keys eqs0)
 where
  implied_by_eqs0 (SP_Reg RIP, _) = True -- technicality 
  implied_by_eqs0 (sp1,Bottom _)  = True
  implied_by_eqs0 (sp1,v1) =
    if contains_bot v1 then
      True
    else case find (\(sp0,v0) -> necessarily_equal_stateparts sp0 sp1) $ M.toList eqs0 of
      Nothing -> is_initial sp1 v1 
      Just (sp0,v0) -> necessarily_equal v0 v1  || join_expr ctxt v0 v1 == v1

  all_sps_in_eqs1 sps0 = all (\sp0 -> contains_bot_sp sp0 || (find (\sp1 -> necessarily_equal_stateparts sp0 sp1) $ M.keys eqs1) /= Nothing) sps0


-- | The initial predicate.
init_pred :: 
  Invariants               -- ^ The currently available invariants
  -> S.Set (NodeInfo,Pred) -- ^ The currently known postconditions
  -> FInit                 -- ^ The function initialisation
  -> Pred
init_pred curr_invs curr_posts finit = 
  let sps = S.delete (SP_Reg RIP) $ S.insert (SP_Mem (SE_Var (SP_Reg RSP)) 8) $ gather_stateparts curr_invs curr_posts
      eqs = M.fromList (map (\sp -> (sp,SE_Var sp)) $ S.toList sps) in
    Predicate (M.union finit eqs) None Clean



get_stateparts_of_preds ps = S.unions $ map get_stateparts_of_pred $ S.toList $ ps

get_stateparts_of_pred (Predicate eqs _ _) = S.filter (not . contains_bot_sp) $ M.keysSet eqs



-- | Given the currently known invariants and postconditions, gather all stateparts occurring in the current function.
gather_stateparts ::
  Invariants               -- ^ The currently available invariants
  -> S.Set (NodeInfo,Pred) -- ^ The currently known postconditions
  -> S.Set StatePart
gather_stateparts invs posts = S.union (IM.foldrWithKey accumulate_stateparts S.empty invs) (get_stateparts_of_preds (S.map snd posts))
 where
  accumulate_stateparts a p sps = S.union sps (get_stateparts_of_pred p)








-- | Get the invariant for a given instruction address for a given function entry
get_invariant :: Context -> Int -> Int -> Maybe Pred
get_invariant ctxt entry a = do
  g         <- IM.lookup entry $ ctxt_cfgs ctxt
  invs      <- IM.lookup entry $ ctxt_invs ctxt
  blockId   <- IM.lookup a $ cfg_addr_to_blockID g
  p         <- IM.lookup blockId invs
  instrs    <- IM.lookup blockId $ cfg_instrs g

  return $ fst $ tau_block ctxt (takeWhile (\i -> i_addr i /= a) instrs) Nothing p






  

instance Propagator Context Pred where
  tau     = tau_block
  join    = join_preds 
  implies = implies_preds
