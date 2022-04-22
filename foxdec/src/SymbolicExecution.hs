{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, FlexibleContexts, Strict #-}


{-|
Module      : SimplePred
Description : Symbolic execution of sequential lists of instructions of type @"X86_Instruction"@ on predicates of type @"Pred"@.
-}

module SymbolicExecution (
  tau_block,
  init_pred,
  gather_stateparts,
  invariant_to_finit,
  join_finit,
  get_invariant
  ) where

import Base
import Config
import SimplePred
import Context
import MachineState
import Propagation
import X86_Datastructures
import Generic_Datastructures
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
fw_transposition :: FContext -> StatePart -> (StatePart,SimpleExpr) -> Maybe SimpleExpr
fw_transposition ctxt (SP_Reg r) (SP_Reg r1, SE_Op (Minus wsize) [SE_Var (SP_Reg r2), SE_Immediate i]) 
  | r == r1 && r1 == r2    = Just $ SE_Op (Plus wsize) [SE_Var (SP_Reg r), SE_Immediate i]
  | otherwise              = Nothing
fw_transposition _ _ _     = Nothing


transpose_fw_e :: FContext -> M.Map StatePart SimpleExpr -> SimpleExpr -> Maybe SimpleExpr 
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

transpose_fw_var :: FContext -> M.Map StatePart SimpleExpr -> StatePart -> Maybe SimpleExpr
transpose_fw_var ctxt p (SP_Mem a si) = Nothing -- TODO
transpose_fw_var ctxt p (SP_Reg r)    = firstJust (fw_transposition ctxt $ SP_Reg r) $ M.toList p

transpose_fw_sp ctxt p sp@(SP_Reg r) = Just sp
transpose_fw_sp ctxt p (SP_Mem a si) = do
  a' <- transpose_fw_e ctxt p a
  return $ SP_Mem (simp a') si


transpose_fw_base ctxt p bs          = return bs -- NOTE: no need for backward transposition here




-- | Convert the current invariant into a function initialisation
invariant_to_finit :: FContext -> Pred -> FInit
invariant_to_finit ctxt (Predicate eqs _) = M.fromList $ mapMaybe mk_finit_entry $ filter is_suitable_for_finit $ M.assocs eqs
 where
  is_suitable_for_finit (SP_Reg r,_)    = r `notElem` [RIP,RSP,RBP]
  is_suitable_for_finit (SP_Mem a si,_) = is_immediate a

  mk_finit_entry (sp,v) = 
    if is_immediate_pointer v then
      Just (sp,v)
    else case get_pointer_domain ctxt v of
      (Domain_Bases bs) -> Just (sp,Bottom $ FromPointerBases bs)  -- NOTE: no need for forward tranposition here
      _                 -> Nothing

  is_immediate_pointer (SE_Immediate a) = find_section_for_address (f_ctxt ctxt) (fromIntegral a) /= Nothing
  is_immediate_pointer _                = False -- What if non-determinism?


-- | The join between two function initialisations
join_finit :: FContext -> FInit -> FInit -> FInit
join_finit ctxt f0 f1 = M.filter keep $ M.intersectionWith (join_expr ctxt) f0 f1
 where
  keep (Bottom (FromPointerBases _)) = True
  keep (Bottom _)                    = False 
  keep _                             = True






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
transpose_bw :: FContext -> String -> Word64 -> Pred -> (StatePart, SimpleExpr) -> Either VerificationCondition (StatePart, SimpleExpr)
transpose_bw ctxt f a p (sp,v) =
  let sp' = transpose_bw_sp ctxt p sp
      v'  = trim_expr ctxt $ simp $ transpose_bw_e ctxt p v in
    if is_invalid sp' then
      Left $ SourcelessMemWrite $ MemWriteFunction f a sp
    else
      Right (sp',v')
 where
  is_invalid (SP_Reg _)   = False
  is_invalid (SP_Mem a _) = invalid_bottom_pointer ctxt a



transpose_bw_e :: FContext -> Pred -> SimpleExpr -> SimpleExpr
transpose_bw_e ctxt p (Bottom (FromNonDeterminism es)) = join_exprs ("transpose_bw") ctxt $ map (transpose_bw_e ctxt p) $ S.toList es
transpose_bw_e ctxt p (Bottom (FromPointerBases bs))   = Bottom (FromPointerBases $ S.map (transpose_bw_base ctxt p) bs)
transpose_bw_e ctxt p (Bottom typ)                     = Bottom $ transpose_bw_bottyp ctxt p typ
transpose_bw_e ctxt p (SE_Malloc id hash)              = SE_Malloc id hash
transpose_bw_e ctxt p (SE_Immediate i)                 = SE_Immediate i
transpose_bw_e ctxt p (SE_StatePart sp)                = Bottom $ FromSemantics S.empty
transpose_bw_e ctxt p (SE_Var (SP_StackPointer f))     = SE_Var (SP_StackPointer f)
transpose_bw_e ctxt p (SE_Var sp)                      = evalState (read_sp ctxt $ transpose_bw_sp ctxt p sp) (p,S.empty)
transpose_bw_e ctxt p (SE_Bit i e)                     = SE_Bit i $ transpose_bw_e ctxt p e
transpose_bw_e ctxt p (SE_SExtend l h e)               = SE_SExtend l h $ transpose_bw_e ctxt p e
transpose_bw_e ctxt p (SE_Op op es)                    = SE_Op op $ map (transpose_bw_e ctxt p) es
transpose_bw_e ctxt p (SE_Overwrite i a b)             = SE_Overwrite i (transpose_bw_e ctxt p a) (transpose_bw_e ctxt p b) 

transpose_bw_sp ctxt p sp@(SP_Reg r)          = sp
transpose_bw_sp ctxt p sp@(SP_StackPointer _) = sp
transpose_bw_sp ctxt p sp@(SP_Mem a si)       = SP_Mem (trim_expr ctxt $ simp $ transpose_bw_e ctxt p a) si

transpose_bw_bottyp ctxt p (FromSources srcs)             = FromSources $ S.unions $ S.map (transpose_bw_src ctxt p) srcs
transpose_bw_bottyp ctxt p (FromOverlap srcs)             = FromSources $ S.unions $ S.map (transpose_bw_src ctxt p) srcs
transpose_bw_bottyp ctxt p (FromMemWrite srcs)            = FromSources $ S.unions $ S.map (transpose_bw_src ctxt p) srcs
transpose_bw_bottyp ctxt p (FromSemantics srcs)           = FromSources $ S.unions $ S.map (transpose_bw_src ctxt p) srcs
transpose_bw_bottyp ctxt p (FromBitMode srcs)             = FromSources $ S.unions $ S.map (transpose_bw_src ctxt p) srcs
transpose_bw_bottyp ctxt p (FromUninitializedMemory srcs) = FromSources $ S.unions $ S.map (transpose_bw_src ctxt p) srcs
transpose_bw_bottyp ctxt p (FromCall f)                   = FromCall f

transpose_bw_src ctxt p src@(Src_Var sp)             = srcs_of_expr ctxt $ transpose_bw_e ctxt p (SE_Var sp)
transpose_bw_src ctxt p src@(Src_StackPointer f)     = S.singleton src
transpose_bw_src ctxt p src@(Src_Malloc id h)        = S.singleton src
transpose_bw_src ctxt p src@(Src_Function f)         = S.singleton src
transpose_bw_src ctxt p src@(Src_ImmediateAddress a) = S.singleton src


transpose_bw_base ctxt p b@(StackPointer f)      = b
transpose_bw_base ctxt p b@(GlobalAddress _)     = b
transpose_bw_base ctxt p b@(PointerToSymbol _ _) = b
transpose_bw_base ctxt p b@(Malloc _ _)          = b







-- | a list of some function that return a heap-pointer through RAX.
-- The pointer is assumed to  be fresh.
functions_returning_fresh_pointers = [ 
     "_malloc", "malloc", "_malloc_create_zone", "_malloc_default_zone", "_malloc_zone_malloc", "_calloc", "calloc", "_malloc_zone_calloc", "_mmap", "_av_mallocz",
     "strdup", "_strdup", "___error", "_fts_read$INODE64", "_fts_open$INODE64", "_opendir$INODE64", "fopen", "_fopen", "_getenv", "_open",
     "_localeconv", "localeconv", "_setlocale", "_wsetlocale", "_fgetln", "fgetln",
     "strerror", "_strerror", "_wcserror", "__wcserror"
   ]

-- | A list of some functions that are assumed not to change the state in any significant way, and that return an unknown bottom value through RAX
functions_returning_bottom = [
     "feof", "_feof", "_getc", "getc", "fgetc", "_fgetc", "_fgetwc", "fgetwc", "_fnmatch", "_fputc",
     "_close", "_fstat$INODE64", "_fstatfs$INODE64", "_statfs$INODE64", "___maskrune", "_sysctlbyname", "_getbsize",
     "_printf", "printf", "_fprintf", "fprintf", "_fprintf_l", "fwprintf", "_fwprintf_l",
     "_putchar", "_puts", 
     "_btowc", "btowc", "mbtowc", "_mbtowc", "_mbrtowc", "mbrtowc", "_atof", "atof",
     "_strcmp", "strcmp",
     "_ilogb", "_atoi",
     "_getopt", "_free",
     "_warn", "_warnx", "__errno_location"
   ]



-- | Executes semantics for some external functions.
-- Returns true iff the string corresponds to a known external function, and the semantics were applied.
function_semantics :: FContext -> X86_Instruction -> String -> State (Pred,VCS) Bool
function_semantics ctxt i "_realloc"             = function_semantics ctxt i "realloc"
function_semantics ctxt i "_malloc_zone_realloc" = function_semantics ctxt i "realloc"
function_semantics ctxt i "realloc"              = read_reg ctxt RDI >>= write_reg ctxt (instr_addr i) RAX >> return True
function_semantics ctxt i "_strcpy"              = function_semantics ctxt i "strcpy"
function_semantics ctxt i "strcpy"               = read_reg ctxt RDI >>= write_reg ctxt (instr_addr i) RAX >> return True
function_semantics ctxt i "_strrchr"             = function_semantics ctxt i "strrchr"
function_semantics ctxt i "strrchr"              = read_reg ctxt RDI >>= (\rdi -> write_reg ctxt (instr_addr i) RAX $ SE_Op (Plus 64) [rdi,rock_bottom]) >> return True
function_semantics ctxt i f                      = 
  if f `elem` functions_returning_bottom then do  -- and exiting function calls?
    write_reg ctxt (instr_addr i) RAX $ Bottom $ FromCall f -- TODO overwrite volatile regs as well?
    return True 
  else if f `elem` functions_returning_fresh_pointers then do
    write_reg ctxt (instr_addr i) RAX $ SE_Malloc (Just (instr_addr i)) (Just "")
    return True
  else
    return False




-- | Add a function constraint to the given symbolic predicate
add_function_constraint f a ps sps (p,vcs) = (p,S.insert (FunctionConstraint f a ps sps) vcs)

-- | Add a function_pointer_intro to the given symbolic predicate
add_function_pointers a ptrs (p,vcs) = 
  if not $ IS.null ptrs then
    let (match,remainder) = S.partition belongs_to_a vcs in
      case S.toList match of
        []                         -> (p,S.insert (FunctionPointers a ptrs) remainder)
        [FunctionPointers _ ptrs'] -> (p,S.insert (FunctionPointers a $ IS.union ptrs ptrs') remainder)
  else
    (p,vcs)
 where
  belongs_to_a (FunctionPointers a' _) = a == a'
  belongs_to_a _                       = False




get_symbolic_function_pointers ctxt (Bottom (FromNonDeterminism es)) = IS.unions $ S.map (get_symbolic_function_pointers ctxt) es
get_symbolic_function_pointers ctxt (SE_Immediate a)                 = if address_has_instruction (f_ctxt ctxt) a then IS.singleton $ fromIntegral a else IS.empty
get_symbolic_function_pointers ctxt _                                = IS.empty



evalState_discard :: State s a -> State s a
evalState_discard ma = do
  s <- get
  return $ evalState ma s 

-- | Symbolically execute a function call
call :: FContext -> X86_Instruction -> State (Pred,VCS) ()
call ctxt i = do
  let f  = f_name ctxt
  let f' = function_name_of_instruction (f_ctxt ctxt) i
  let i_a = instr_addr i
  known <- function_semantics ctxt i $ takeUntilString "@GLIBC" f'

  when (not known) $ do
    sub ctxt i_a (Storage RSP) (Immediate 8)
    (p@(Predicate p_eqs _),_) <- get
    params <- mapMaybeM (when_is_relevant_param p_eqs) parameter_registers
    modify $ add_function_pointers i_a (IS.unions $ map (get_symbolic_function_pointers ctxt . snd) params)


    let postconditions = map postcondition_of_jump_target $ resolve_jump_target (f_ctxt ctxt) i
    if postconditions == [] || any ((==) Nothing) postconditions then do
      -- an external function, or a function that produced a verification error, or with unresolved indirections

      -- 1.) for each parameter, smudge the current state
      forM_ params write_param
      (q@(Predicate q_eqs _),_) <- get

      -- 2.) transfer stateparts that must be kept intact, and generation verification conditions if necessary
      sps <- S.unions <$> (mapM (transfer_current_statepart f' i p q True) $ M.toList p_eqs)
      when (not $ S.null sps) $ modify $ add_function_constraint f' i_a params sps

      -- 3.) For all return registers (RAX,XMM0), write some unknown return value
      forM_ return_registers (\r -> write_reg ctxt i_a r $ Bottom $ FromCall $ f') -- TODO flags
    else do
      -- an internal function, already verified
      (p@(Predicate p_eqs _),vcs) <- get

      -- 1.) obtain the postcondition of the function, and do backwards transposition
      -- TODO: first transpose then supremum
      let (q@(Predicate q_eqs _))   = supremum ctxt $ map fromJust postconditions
      let (vcs',q_eqs_transposed)   = partitionEithers $ map (transpose_bw ctxt f' i_a p) $ filter (uncurry do_transfer) $ M.toList q_eqs
      put ((Predicate M.empty None),S.union vcs $ S.fromList vcs')
      mapM_ (write_sp ctxt i_a mk_mid) q_eqs_transposed
      (q_transposed@(Predicate q_eqs _),vcs) <- get

      -- 2.) transfer stateparts that must be kept intact, and generation verification conditions if necessary
      sps <- S.unions <$> (mapM (transfer_current_statepart f' i p q_transposed False) $ M.toList p_eqs)
      when (not $ S.null sps) $ modify $ add_function_constraint f' i_a params sps
 where
  -- in case of an external function f, which is passed a parameter (r,a) with r a register and a some value (likely a pointer),
  -- do a write to region [a+bot,1] to muddle the state. The value written to that region is an abstraction of what is already there.
  write_param (r,a) = do
    let a'  = SE_Op (Plus 64) [a,rock_bottom]
    let si' = 1 
    v'     <- evalState_discard $ read_sp ctxt (SP_Mem a 1)
    let bot = join_single ctxt v'
    write_sp ctxt (instr_addr i)  mk_mid (SP_Mem a' si',bot)  

  when_is_relevant_param p_eqs r = do
    v <- read_reg ctxt r
    if expr_highly_likely_pointer ctxt v || (is_currently_pointer p_eqs v && not (is_immediate v)) then 
      return $ Just (r,v)
    else
      return $ Nothing



  -- let the current predicate p, before the call, contain an equation sp == v
  -- q provides the predicate after the function call.
  -- We transfer the statepart sp and sometimes forcibly keep its original value v, even if we could not prove that it was preserved.
  -- In those cases, we add annotations in the form of "Function Constraints".
  transfer_current_statepart f' i p@(Predicate p_eqs _) q@(Predicate q_eqs _) is_external (sp,v) = do
    if sp == SP_Reg RIP then do
      -- forcibly transfer and set the value of the instruction pointer
      forced_insert_sp sp (SE_Immediate $ instr_addr i + (fromIntegral $ instr_size i))
      return S.empty
    else if all (necessarily_separate_stateparts ctxt sp) $ M.keys q_eqs then do
      -- if the function did not write to the statepart, transfer it without annotations
      forced_insert_sp sp v
      return S.empty
    else let v' = evalState (read_sp ctxt sp) (q,S.empty) in
      if sp == SP_Reg RSP then do
        -- register RSP must be set to its original value + 8, force this and add an annotation
        let add_vc = not is_external && v' /= simp (SE_Op (Plus 64) [SE_Var $ SP_StackPointer f',SE_Immediate 8])
        forced_insert_sp sp (SE_Op (Plus 64) [v,SE_Immediate 8])
        return $ if add_vc then S.singleton sp else S.empty
      else if or [
           is_external && statepart_preserved_after_external_function_call ctxt is_external sp && (must_be_preserved p_eqs sp v || is_reg_sp sp),
           not is_external && is_mem_sp sp && statepart_preserved_after_external_function_call ctxt is_external sp && must_be_preserved p_eqs sp v
          ] then do
        -- the statepart should have been preserved by the function, but that cannot be proven
        -- forcibly preserve the statepart and annotate
        forced_insert_sp sp v
        let add_vc = v /= v' && not (uninitialized v') 
        --trace ("Preserving (" ++ show i ++ "): " ++ show (sp,v,v')) $
        return $ if add_vc then S.singleton sp else S.empty
      else case find (\(sp',v) -> necessarily_equal_stateparts sp sp') $ M.assocs q_eqs of
        Just (_,v') -> do
          -- the statepart was overwritten by the function, so use its new value
          write_sp ctxt (instr_addr i) mk_mid (sp,v')
          return S.empty
        Nothing     -> do
          -- the statepart was possibly overwritten by the function, so use its old and new value joined
          --(if v /=v' then trace ("Transferring (" ++ show i ++ ") " ++ show (sp,v) ++ " --> " ++ show (join_expr ctxt v v')) else id) $
          write_sp ctxt (instr_addr i) mk_mid (sp,join_expr ctxt v v')
          return S.empty
        

  -- make a memory-write-identifier: the memroy write happened during this function call
  mk_mid = MemWriteFunction (function_name_of_instruction (f_ctxt ctxt) i)  

  -- forcibly insert the statepart into ther current predicate
  -- should never be done without caution, one should always use the mem_write function
  forced_insert_sp sp v = do
    (p@(Predicate p_eqs flg),vcs) <- get
    put (Predicate (M.insert sp v p_eqs) flg,vcs)

 
  must_be_preserved p_eqs (SP_Reg _)                              _ = True
  must_be_preserved p_eqs (SP_Mem (SE_Var (SP_StackPointer _)) _) _ = True
  must_be_preserved p_eqs (SP_Mem a si)                           v = if expr_is_highly_likely_local_pointer ctxt a then expr_highly_likely_pointer ctxt v || is_reg_var v || is_currently_pointer p_eqs v else True 

  is_reg_var (SE_Var (SP_Reg r)) = True
  is_reg_var _                   = False

  is_currently_pointer p_eqs a  = find (is_region_for a . fst) (M.toList p_eqs) /= Nothing || any source_in_finit (srcs_of_expr ctxt a)
  is_region_for a (SP_Reg _)    = False
  is_region_for a (SP_Mem a' _) = a == a'
  source_in_finit (Src_Var sp)  = M.lookup sp (f_init ctxt) /= Nothing
  source_in_finit _             = False

  uninitialized (Bottom (FromUninitializedMemory _)) = True
  uninitialized _                                    = False 

  do_transfer sp@(SP_Reg _)    v = True 
  do_transfer sp@(SP_Mem a si) v = not (any is_local_to_not_f $ srcs_of_expr ctxt a) && not (is_initial sp v)
 
  is_local_to_not_f (Src_StackPointer f') = f_name ctxt /= f' -- TODO keep when f' is from current SCC callgraph
  is_local_to_not_f _                     = False

  postcondition_of_jump_target (ImmediateAddress a) =
    case IM.lookup (fromIntegral a) (ctxt_calls $ f_ctxt ctxt) of
      Just (ReturningWith q) -> Just q
      _                      -> Nothing
  postcondition_of_jump_target _                    = Nothing



statepart_preserved_after_external_function_call ::
  FContext       -- ^ The context
  -> Bool       -- ^ Is the function external?
  -> StatePart  -- ^ A state part 
  -> Bool 
statepart_preserved_after_external_function_call ctxt is_external (SP_Reg r)   = r `elem` callee_saved_registers
statepart_preserved_after_external_function_call ctxt is_external (SP_Mem a _) = not (contains_bot a) && or [
      expr_is_highly_likely_local_pointer ctxt a,
      address_is_unwritable (f_ctxt ctxt) a,
      (is_external && expr_is_global_immediate (f_ctxt ctxt) a)
     ]




is_initial :: StatePart -> SimpleExpr -> Bool
is_initial sp v = v == SE_Var sp










-- | Instruction semantics
push :: FContext -> Word64 -> X86_Operand -> State (Pred,VCS) ()
push ctxt i_a (Immediate imm) = do
  let si = 8
  let address = AddressMinus (AddressStorage RSP) (AddressImm $ fromIntegral si)
  e1 <- resolve_address ctxt address
  write_reg ctxt i_a RSP e1
  write_mem ctxt (MemWriteInstruction i_a (Memory address si) e1) e1 si (SE_Immediate imm)
push ctxt i_a op1 = do
  e0 <- read_operand ctxt op1
  let si = operand_size op1
  let address = AddressMinus (AddressStorage RSP) (AddressImm $ fromIntegral si)
  e1 <- resolve_address ctxt address
  write_reg ctxt i_a RSP e1
  write_mem ctxt (MemWriteInstruction i_a (Memory address si) e1) e1 si e0

pop :: FContext -> Word64 -> X86_Operand -> State (Pred,VCS) ()
pop ctxt i_a op1 = do
  let si = operand_size op1
  e0 <- read_mem ctxt (Memory (AddressStorage RSP) si)
  let address = AddressPlus (AddressStorage RSP) (AddressImm $ fromIntegral si)
  e1 <- resolve_address ctxt address
  write_reg ctxt i_a RSP e1
  write_operand ctxt i_a op1 e0

lea :: FContext -> Word64 -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
lea ctxt i_a op1 (EffectiveAddress a) = do
  e <- resolve_address ctxt a
  write_operand ctxt i_a op1 e


leave :: FContext -> Word64 -> State (Pred,VCS) ()
leave ctxt i_a = mov ctxt i_a (Storage RSP) (Storage RBP) >> pop ctxt i_a (Storage RBP)


ret ctxt i_a = pop ctxt i_a (Storage RIP)

sysret ctxt i_a = do
  e0 <- read_operand ctxt (Storage RCX)
  write_reg ctxt i_a RIP e0
  write_flags (\_ _ -> None) (Storage RCX) (Storage RCX)

jmp ctxt i =
  if instruction_jumps_to_external (f_ctxt ctxt) i then
    -- A jump to an external symbol is treated as a function call and implicit RET
    call ctxt i >> ret ctxt (fromIntegral $ instr_addr i)
  else
    return ()



write_flags :: (X86_Operand -> X86_Operand -> FlagStatus) -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
write_flags g op1 op2 = do
  (Predicate eqs flg,vcs) <- get
  put (Predicate eqs (g op1 op2),vcs)


mov_with_func_op2_to_op1 :: FContext -> Word64 -> (SimpleExpr -> SimpleExpr) -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
mov_with_func_op2_to_op1 ctxt i_a f op1 op2 = do
  e1 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (f e1)

mk_bottom ctxt es = Bottom $ FromSemantics $ S.unions (map (srcs_of_expr ctxt) es)

mov_with_func1 :: FContext -> Word64 -> (FContext -> [SimpleExpr] -> SimpleExpr) -> Bool -> X86_Operand -> State (Pred,VCS) ()
mov_with_func1 ctxt i_a f do_write_flags op1 = do
  e0 <- read_operand ctxt op1
  write_operand ctxt i_a op1 (f ctxt [e0])
  when do_write_flags (write_flags (\_ _ -> None) op1 op1)

mov_with_func :: FContext -> Word64 -> (FContext -> [SimpleExpr] -> SimpleExpr) -> Bool -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
mov_with_func ctxt i_a f do_write_flags op1 op2 = do
  e0 <- read_operand ctxt op1
  e1 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (f ctxt [e0,e1])
  when do_write_flags (write_flags (\_ _ -> None) op1 op2)

mov_with_func3 :: FContext -> Word64 -> (FContext -> [SimpleExpr] -> SimpleExpr) -> Bool -> X86_Operand -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
mov_with_func3 ctxt i_a f do_write_flags op1 op2 op3 = do
  e0 <- read_operand ctxt op1
  e1 <- read_operand ctxt op2
  e2 <- read_operand ctxt op3
  write_operand ctxt i_a op1 (f ctxt [e0,e1,e2])
  when do_write_flags (write_flags (\_ _ -> None) op2 op3)

nop ctxt i_a = return ()

endbr64 ctxt i_a = return ()

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

vmovd = mov

vmovapd = mov

vmovaps = mov


cmov ctxt i_a op1 op2 = do
  e0 <- read_operand ctxt op1
  e1 <- read_operand ctxt op2
  write_operand ctxt i_a op1 $ join_exprs ("cmov") ctxt [e0,e1]

xchg :: FContext -> Word64 -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
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

comiss ctxt i_a = write_flags (\_ _ -> None)

inc :: FContext -> Word64 -> X86_Operand -> State (Pred,VCS) ()
inc ctxt i_a op1 = do
  e1 <- read_operand ctxt op1
  write_operand ctxt i_a op1 (SE_Op (Plus (8 * operand_size op1)) [e1,SE_Immediate 1])
  write_flags (\_ _ -> None) op1 op1

dec :: FContext -> Word64 -> X86_Operand -> State (Pred,VCS) ()
dec ctxt i_a op1 = do
  e1 <- read_operand ctxt op1
  write_operand ctxt i_a op1 (SE_Op (Minus (8 * operand_size op1)) [e1,SE_Immediate 1])
  write_flags (\_ _ -> None) op1 op1

or' :: FContext -> Word64 -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
or' ctxt i_a op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (SE_Op (Or (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2

and' :: FContext -> Word64 -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
and' ctxt i_a op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (SE_Op (And (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2


not' :: FContext -> Word64 -> X86_Operand -> State (Pred,VCS) ()
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

setxx ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False -- TODO ND 0/1

pxor = xor --TODO flags?

pand ctxt i_a = mov_with_func ctxt i_a mk_bottom False

pandn ctxt i_a = mov_with_func ctxt i_a mk_bottom False

por ctxt i_a = mov_with_func ctxt i_a mk_bottom False

ptest ctxt i_a = mov_with_func ctxt i_a mk_bottom True

vpxor = xor --TODO flags?

vpand ctxt i_a = mov_with_func ctxt i_a mk_bottom False

vpandn ctxt i_a = mov_with_func ctxt i_a mk_bottom False

vpor ctxt i_a = mov_with_func ctxt i_a mk_bottom False

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

bt :: FContext -> Word64 -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
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

pmaxsd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

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

is_st_reg_operand (Storage r) = take 2 (show r) == "ST"
is_st_reg_operand _           = False

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

finit' ctxt i_a = return ()

fninit ctxt i_a = return ()

fnstcw ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False

fstcw ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False

emms ctxt i_a = return ()

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

div1 :: FContext -> Word64 -> X86_Operand -> State (Pred,VCS) ()
div1 ctxt i_a op1 = do
  let srcs = case operand_size op1 of
               8 -> [RDX,RAX]
               4 -> [EDX,EAX]
               2 -> [DX,AX]
               1 -> [AH,AL]
  e1 <- read_operand ctxt op1
  src0 <- read_operand ctxt (Storage $ srcs !! 0)
  src1 <- read_operand ctxt (Storage $ srcs !! 1)
  
  write_operand ctxt i_a (Storage $ srcs !! 0) $ SE_Op (Div_Rem (8 * operand_size op1)) [src0,src1,e1]
  write_operand ctxt i_a (Storage $ srcs !! 1) $ SE_Op (Div (8 * operand_size op1)) [src0,src1,e1]
  write_flags (\_ _ -> None) op1 op1


cdq :: FContext -> Word64 -> State (Pred,VCS) ()
cdq ctxt i_a = do
  e1 <- read_operand ctxt (Storage EAX)
  write_operand ctxt i_a (Storage EDX) (mk_bottom ctxt [e1])

cqo :: FContext -> Word64 -> State (Pred,VCS) ()
cqo ctxt i_a = do
  e1 <- read_operand ctxt (Storage RAX)
  write_operand ctxt i_a (Storage RDX) (mk_bottom ctxt [e1])

cdqe :: FContext -> Word64 -> State (Pred,VCS) ()
cdqe ctxt i_a = do
  e1 <- read_operand ctxt (Storage EAX)
  write_operand ctxt i_a (Storage RAX) (SE_SExtend 32 64 e1)

cbw ctxt i_a = mov_with_func1 ctxt i_a mk_bottom True (Storage AX)

cwde ctxt i_a = mov_with_func1 ctxt i_a mk_bottom True (Storage EAX)





shl :: FContext -> Word64 -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
shl ctxt i_a op1 op2@(Immediate i) = do
  e1 <- read_operand ctxt op1
  write_operand ctxt i_a op1 (SE_Op (Times (8 * operand_size op1)) [e1,SE_Immediate $ 2^i])
  write_flags (\_ _ -> None) op1 op2
shl ctxt i_a op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (SE_Op (Shl (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2

shr :: FContext -> Word64 -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
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


rol :: FContext -> Word64 -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
rol ctxt i_a op1 op2@(Immediate i) = do
  e1 <- read_operand ctxt op1
  write_operand ctxt i_a op1 $ SE_Op (Rol (8 * operand_size op1)) [e1,SE_Immediate $ 2^i]
  write_flags (\_ _ -> None) op1 op2
rol ctxt i_a op1 op2 = mov_with_func ctxt i_a mk_bottom True op1 op2

ror :: FContext -> Word64 -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
ror ctxt i_a op1 op2@(Immediate i) = do
  e1 <- read_operand ctxt op1
  write_operand ctxt i_a op1 $ SE_Op (Ror (8 * operand_size op1)) [e1,SE_Immediate $ 2^i]
  write_flags (\_ _ -> None) op1 op2
ror ctxt i_a op1 op2 = mov_with_func ctxt i_a mk_bottom True op1 op2

{-
adc :: FContext -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
adc ctxt op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 $ SE_Binop "adc" [e1,e2]
  write_flags (\_ _ -> None) op1 op2

sbb :: FContext -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
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

imul1 :: FContext -> Word64 -> X86_Operand -> State (Pred,VCS) ()
imul1 ctxt i_a op1 = do
  let srcs = case operand_size op1 of
               8 -> [RDX,RAX]
               4 -> [EDX,EAX]
               2 -> [DX,AX]
               1 -> [AH,AL]
  e1 <- read_operand ctxt op1
  src0 <- read_operand ctxt (Storage $ srcs !! 0)
  src1 <- read_operand ctxt (Storage $ srcs !! 1)
  
  write_operand ctxt i_a (Storage $ srcs !! 0) $ mk_bottom ctxt [src1,e1] -- high part of multiplication
  write_operand ctxt i_a (Storage $ srcs !! 1) $ SE_Op (Times (8 * operand_size op1)) [src1,e1]
  write_flags (\_ _ -> None) op1 op1

imul2 :: FContext -> Word64 -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
imul2 ctxt i_a op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (SE_Op (Times (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2

imul3 :: FContext -> Word64 -> X86_Operand -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
imul3 ctxt i_a op0 op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op0 (SE_Op (Times (8 * operand_size op0)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2



bswap :: FContext -> Word64 -> X86_Operand -> State (Pred,VCS) ()
bswap ctxt i_a op1 = do
  e1 <- read_operand ctxt op1
  write_operand ctxt i_a op1 $ SE_Op (Bswap (8 * operand_size op1)) [e1]
  write_flags (\_ _ -> None) op1 op1

pextrb :: FContext -> Word64 -> X86_Operand -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
pextrb ctxt i_a op0 op1 op2 = do
  e0 <- read_operand ctxt op0
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op0 (SE_Op (Pextr 8) [e0,e1,e2])

pextrd :: FContext -> Word64 -> X86_Operand -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
pextrd ctxt i_a op0 op1 op2 = do
  e0 <- read_operand ctxt op0
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op0 (SE_Op (Pextr 32) [e0,e1,e2])

pextrq :: FContext -> Word64 -> X86_Operand -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
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

vpshufb ctxt i_a = mov_with_func ctxt i_a mk_bottom False

vpshufd ctxt i_a = mov_with_func ctxt i_a mk_bottom False

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

blendvpd ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

blendvps ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

extractps ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

shufps ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

movsd_string ctxt prefix op1 op2 = return () -- TODO 

movsq ctxt prefix op1 op2 = return () -- TODO 

x86_in ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False

x86_out ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False

cli ctxt i_a = return ()

clts ctxt i_a = return ()

cpuid ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False (Storage RAX)

invpcid ctxt i_a = return ()

lgdt ctxt i_a = return ()

lidt ctxt i_a = return ()

lldt ctxt i_a = return ()

ltr ctxt i_a = return ()

rdmsr ctxt i_a = do
  mov_with_func1 ctxt i_a mk_bottom False (Storage RAX)
  mov_with_func1 ctxt i_a mk_bottom False (Storage RDX)

wrmsr ctxt i_a = do
  mov_with_func1 ctxt i_a mk_bottom False (Storage RAX)
  mov_with_func1 ctxt i_a mk_bottom False (Storage RDX)

rdtsc ctxt i_a = do
  mov_with_func1 ctxt i_a mk_bottom False (Storage RAX)
  mov_with_func1 ctxt i_a mk_bottom False (Storage RDX)

swapgs ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False (Storage GS)

xsetbv ctxt i_a = return ()

xsaveopt ctxt i_a = return ()

xrstor ctxt i_a = return ()

wrfsbase ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False (Storage FS)

wrgsbase ctxt i_a = mov_with_func1 ctxt i_a mk_bottom False (Storage GS)

vinsertf128 ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vextractf128 ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vextracti128 ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vperm2f128 ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vperm2i128 ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vshufpd ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vshufps ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vaddpd ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vaddps ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vsubpd ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vsubps ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vmulpd ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vmulps ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vxorpd ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vxorps ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vandpd ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vandps ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vpalignr ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

palignr ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vmovdqa ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vmovdqu  ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vmovlhps ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vzeroupper ctxt i_a = return ()

vpunpckhwd ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vpunpcklwd ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vpcmpeqb ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vpcmpeqw ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

vpsllw ctxt i_a = mov_with_func3 ctxt i_a mk_bottom False

movhps ctxt i_a = mov_with_func ctxt i_a mk_bottom False

movlhps ctxt i_a = mov_with_func ctxt i_a mk_bottom False

movhlps ctxt i_a = mov_with_func ctxt i_a mk_bottom False

vmovhps ctxt i_a = mov_with_func ctxt i_a mk_bottom False


xadd :: FContext -> Word64 -> X86_Operand -> X86_Operand -> State (Pred,VCS) ()
xadd ctxt i_a op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt i_a op1 (SE_Op (Plus (operand_size op1)) [e1,e2])
  write_operand ctxt i_a op2 e1
  write_flags (\_ _ -> None) op1 op2

cmpxchg ctxt i_a = mov_with_func ctxt i_a mk_bottom True

cmps  ctxt i_a pre = mov_with_func ctxt i_a mk_bottom True

cmpsb ctxt i_a pre = mov_with_func ctxt i_a mk_bottom True

cmpsw ctxt i_a pre = mov_with_func ctxt i_a mk_bottom True

cmpsd ctxt i_a pre = mov_with_func ctxt i_a mk_bottom True




tau_i :: FContext -> X86_Instruction -> State (Pred,VCS) ()
tau_i ctxt (Instruction (AddressWord64 i_a) _ PUSH     _ [op1]     _)   = push   ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ POP      _ [op1]     _)   = pop    ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ LEA      _ [op1,op2] _)   = lea    ctxt i_a op1 op2

tau_i ctxt i@(Instruction (AddressWord64 i_a) _ CALL     _ _         _) = call   ctxt i
tau_i ctxt   (Instruction (AddressWord64 i_a) _ RET      _ _         _) = ret    ctxt i_a 
tau_i ctxt   (Instruction (AddressWord64 i_a) _ IRETQ    _ _         _) = ret    ctxt i_a 
tau_i ctxt   (Instruction (AddressWord64 i_a) _ SYSRET   _ _         _) = sysret ctxt i_a
tau_i ctxt i@(Instruction (AddressWord64 i_a) _ JMP      _ [op1]     _) = jmp    ctxt i
tau_i ctxt   (Instruction (AddressWord64 i_a) _ LEAVE    _ _         _) = leave  ctxt i_a

tau_i ctxt (Instruction (AddressWord64 i_a) _       MOV      _ [op1,op2] _) = mov    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       MOVZX    _ [op1,op2] _) = movzx  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       MOVSX    _ [op1,op2] _) = movsx  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       MOVSXD   _ [op1,op2] _) = movsxd ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       MOVAPS   _ [op1,op2] _) = movaps ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       MOVAPD   _ [op1,op2] _) = movapd ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       MOVABS   _ [op1,op2] _) = movabs ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       MOVUPD   _ [op1,op2] _) = movupd ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       MOVUPS   _ [op1,op2] _) = movups ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       MOVDQU   _ [op1,op2] _) = movdqu ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       MOVDQA   _ [op1,op2] _) = movdqa ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       MOVD     _ [op1,op2] _) = movd   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       MOVQ     _ [op1,op2] _) = movq   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       MOVLPD   _ [op1,op2] _) = movlpd ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       MOVLPS   _ [op1,op2] _) = movlps ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) Nothing MOVSD    _ [op1,op2] _) = movsd  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) Nothing MOVSS    _ [op1,op2] _) = movss  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       VMOVD    _ [op1,op2] _) = vmovd  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       VMOVAPD  _ [op1,op2] _) = vmovapd ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _       VMOVAPS  _ [op1,op2] _) = vmovaps ctxt i_a op1 op2

tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVO    _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVNO   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVS    _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVNS   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVE    _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVZ    _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVNE   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVNZ   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVB    _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVNAE  _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVC    _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVNB   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVAE   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVNC   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVBE   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVNA   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVA    _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVNBE  _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVL    _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVNGE  _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVG    _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVGE   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVNL   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVLE   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVNG   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVNLE  _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVP    _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVPE   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVNP   _ [op1,op2] _) = cmov   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMOVPO   _ [op1,op2] _) = cmov   ctxt i_a op1 op2

tau_i ctxt (Instruction (AddressWord64 i_a) _ CDQ      _ []        _) = cdq    ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ CDQE     _ []        _) = cdqe   ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ CQO      _ []        _) = cqo    ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ CBW      _ []        _) = cbw    ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ CWDE     _ []        _) = cwde   ctxt i_a

tau_i ctxt (Instruction (AddressWord64 i_a) _ ADD      _ [op1,op2] _) = add    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ SUB      _ [op1,op2] _) = sub    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ NEG      _ [op1]     _) = neg    ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ INC      _ [op1]     _) = inc    ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ DEC      _ [op1]     _) = dec    ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SHL      _ [op1,op2] _) = shl    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ SHL      _ [op1]     _) = shl    ctxt i_a op1 (Immediate 1)

tau_i ctxt (Instruction (AddressWord64 i_a) _ NOP      _ _         _) = nop     ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ ENDBR64  _ _         _) = endbr64 ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ UD2      _ _         _) = ud2     ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ HLT      _ _         _) = hlt     ctxt i_a

tau_i ctxt (Instruction (AddressWord64 i_a) _ WAIT     _ _         _) = wait    ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ MFENCE   _ _         _) = mfence  ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ CLFLUSH  _ _         _) = clflush ctxt i_a

tau_i ctxt (Instruction (AddressWord64 i_a) _ ADC      _ [op1,op2] _)          = adc    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ SBB      _ [op1,op2] _)          = sbb    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ ROL      _ [op1,op2] _)          = rol    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ ROR      _ [op1,op2] _)          = ror    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ SHR      _ [op1,op2] _)          = shr    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ SHR      _ [op1]     _)          = shr    ctxt i_a op1 (Immediate 1)
tau_i ctxt (Instruction (AddressWord64 i_a) _ SAR      _ [op1,op2] _)          = sar    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ SAR      _ [op1]     _)          = sar    ctxt i_a op1 (Immediate 1)
tau_i ctxt (Instruction (AddressWord64 i_a) _ MUL      _ [op1]     _)          = mul1  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ MUL      _ [op1,op2] _)          = mul2  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ IMUL     _ [op1]     _)          = imul1  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ IMUL     _ [op1,op2] _)          = imul2  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ IMUL     _ [op1,op2,op3] _)      = imul3  ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ IDIV     _ [op1]     _)          = idiv   ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ DIV      _ [op1]     _)          = div1   ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SHLD     _ [op1,op2,op3] _)      = shld ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ SHRD     _ [op1,op2,op3] _)      = shrd ctxt i_a op1 op2 op3

tau_i ctxt (Instruction (AddressWord64 i_a) _ CMP      _ [op1,op2] _) = cmp    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ TEST     _ [op1,op2] _) = test   ctxt i_a op1 op2

tau_i ctxt (Instruction (AddressWord64 i_a) _ XOR      _ [op1,op2] _) = xor    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ OR       _ [op1,op2] _) = or'    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ AND      _ [op1,op2] _) = and'   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ NOT      _ [op1]     _) = not'   ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ BT       _ [op1,op2] _) = bt     ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ BTC      _ [op1,op2] _) = btc    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ BTR      _ [op1,op2] _) = btr    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ BSR      _ [op1,op2] _) = bsr    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ BSF      _ [op1,op2] _) = bsf    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ BTS      _ [op1,op2] _) = bts    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ BSWAP    _ [op1]     _) = bswap  ctxt i_a op1



tau_i ctxt (Instruction (AddressWord64 i_a) _ SETO     _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETNO    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETS     _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETNS    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETE     _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETZ     _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETNE    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETNZ    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETB     _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETNAE   _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETC     _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETNB    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETAE    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETNC    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETBE    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETNA    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETA     _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETNBE   _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETL     _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETNGE   _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETGE    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETNL    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETLE    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETNG    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETG     _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETNLE   _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETP     _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETPE    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETNP    _ [op1] _) = setxx  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ SETPO    _ [op1] _) = setxx  ctxt i_a op1


tau_i ctxt (Instruction (AddressWord64 i_a) _ XORPS     _ [op1,op2] _)            = xorps      ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ MOVMSKPS  _ [op1,op2] _)            = movmskps   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ UNPCKLPS  _ [op1,op2] _)            = unpcklps   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ BLENDVPD  _ [op1,op2]    _)   = blendvpd   ctxt i_a op1 op2 (Storage XMM0)
tau_i ctxt (Instruction (AddressWord64 i_a) _ BLENDVPD  _ [op1,op2,op3] _)   = blendvpd   ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ BLENDVPS  _ [op1,op2]    _)   = blendvps   ctxt i_a op1 op2 (Storage XMM0)
tau_i ctxt (Instruction (AddressWord64 i_a) _ BLENDVPS  _ [op1,op2,op3] _)   = blendvps   ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ EXTRACTPS _ [op1,op2,op3] _)   = extractps  ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ SHUFPS    _ [op1,op2,op3] _)   = shufps     ctxt i_a op1 op2 op3


tau_i ctxt (Instruction (AddressWord64 i_a) _ XORPD    _ [op1,op2] _) = xorpd    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ ANDPD    _ [op1,op2] _) = andpd    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ ANDNPD   _ [op1,op2] _) = andnpd   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ ORPD     _ [op1,op2] _) = orpd     ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ SUBPD    _ [op1,op2] _) = subpd    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ ADDPD    _ [op1,op2] _) = addpd    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ HADDPD   _ [op1,op2] _) = haddpd   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ MOVMSKPD _ [op1,op2] _) = movmskpd   ctxt i_a op1 op2

tau_i ctxt (Instruction (AddressWord64 i_a) _ POR        _ [op1,op2] _)          = por          ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PAND       _ [op1,op2] _)          = pand         ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PANDN      _ [op1,op2] _)          = pandn        ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PXOR       _ [op1,op2] _)          = pxor         ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ VPOR       _ [op1,op2] _)          = vpor         ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ VPAND      _ [op1,op2] _)          = vpand        ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ VPANDN     _ [op1,op2] _)          = vpandn       ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ VPXOR      _ [op1,op2] _)          = vpxor        ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PTEST      _ [op1,op2] _)          = ptest        ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PUNPCKLQDQ _ [op1,op2] _)          = punpcklqdq   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PUNPCKLBW  _ [op1,op2] _)          = punpcklbw    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PUNPCKLDQ  _ [op1,op2] _)          = punpckldq    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PMOVSXDQ   _ [op1,op2] _)          = pmovsxdq     ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PMOVZXDQ   _ [op1,op2] _)          = pmovzxdq     ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PMOVSXBD   _ [op1,op2] _)          = pmovsxbd     ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PMOVZXBD   _ [op1,op2] _)          = pmovzxbd     ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PSHUFB     _ [op1,op2] _)          = pshufb       ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PSHUFD     _ [op1,op2] _)          = pshufd       ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ VPSHUFB    _ [op1,op2] _)          = vpshufb      ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ VPSHUFD    _ [op1,op2] _)          = vpshufd      ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PCMPEQB    _ [op1,op2] _)          = pcmpeqb      ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PCMPEQD    _ [op1,op2] _)          = pcmpeqd      ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PCMPGTB    _ [op1,op2] _)          = pcmpgtb      ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PCMPGTD    _ [op1,op2] _)          = pcmpgtd      ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PADDD      _ [op1,op2] _)          = paddd        ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PADDB      _ [op1,op2] _)          = paddb        ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PADDQ      _ [op1,op2] _)          = paddq        ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PSUBD      _ [op1,op2] _)          = psubd        ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PSUBB      _ [op1,op2] _)          = psubb        ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PSUBQ      _ [op1,op2] _)          = psubq        ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PMULLD     _ [op1,op2] _)          = pmulld       ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PMINSD     _ [op1,op2] _)          = pminsd       ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PMAXSD     _ [op1,op2] _)          = pmaxsd       ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PMINUD     _ [op1,op2] _)          = pminud       ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PMAXUD     _ [op1,op2] _)          = pmaxud       ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PMAXUQ     _ [op1,op2] _)          = pmaxuq       ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PSRLD      _ [op1,op2] _)          = psrld        ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PSRLW      _ [op1,op2] _)          = psrlw        ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PSRLDQ     _ [op1,op2] _)          = psrldq       ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PSLLDQ     _ [op1,op2] _)          = pslldq       ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PSLLQ      _ [op1,op2] _)          = psllq        ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PSRLQ      _ [op1,op2] _)          = psrlq        ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PSUBUSB    _ [op1,op2] _)          = psubusb      ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PSUBUSW    _ [op1,op2] _)          = psubusw      ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PINSRB     _ [op1,op2,op3] _) = pinsrq       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ PINSRQ     _ [op1,op2,op3] _) = pinsrq       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ PINSRD     _ [op1,op2,op3] _) = pinsrd       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ PEXTRB     _ [op1,op2,op3] _) = pextrb       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ PEXTRD     _ [op1,op2,op3] _) = pextrd       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ PEXTRQ     _ [op1,op2,op3] _) = pextrq       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ PSHUFLW    _ [op1,op2,op3] _) = pshuflw      ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ PCLMULQDQ  _ [op1,op2,op3] _) = pclmulqdq    ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ PACKSSDW   _ [op1,op2] _)          = packssdw     ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ PACKSSWB   _ [op1,op2] _)          = packsswb     ctxt i_a op1 op2


tau_i ctxt (Instruction (AddressWord64 i_a) _ SUBSS    _ [op1,op2] _) = subss    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ ADDSS    _ [op1,op2] _) = addss    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ DIVSS    _ [op1,op2] _) = divss    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ MULSS    _ [op1,op2] _) = mulss    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ ROUNDSS  _ [op1,op2] _) = roundss  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ UCOMISS  _ [op1,op2] _) = ucomiss  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ COMISS   _ [op1,op2] _) = comiss   ctxt i_a op1 op2

tau_i ctxt (Instruction (AddressWord64 i_a) _ SUBSD    _ [op1,op2] _) = subsd    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ ADDSD    _ [op1,op2] _) = addsd    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ DIVSD    _ [op1,op2] _) = divsd    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ MULSD    _ [op1,op2] _) = mulsd    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ ROUNDSD  _ [op1,op2] _) = roundsd  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ UCOMISD  _ [op1,op2] _) = ucomisd  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMPLTSD  _ [op1,op2] _) = cmpltsd  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMPEQSD  _ [op1,op2] _) = cmpeqsd  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CMPNEQSD _ [op1,op2] _) = cmpneqsd ctxt i_a op1 op2



tau_i ctxt (Instruction (AddressWord64 i_a) _ CVTSS2SD  _ [op1,op2] _) = cvtss2sd  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CVTSI2SS  _ [op1,op2] _) = cvtsi2ss  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CVTSI2SD  _ [op1,op2] _) = cvtsi2sd  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CVTSD2SS  _ [op1,op2] _) = cvtsd2ss  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CVTTSS2SI _ [op1,op2] _) = cvttss2si ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CVTTSD2SI _ [op1,op2] _) = cvttsd2si ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CVTTPD2DQ _ [op1,op2] _) = cvttpd2dq ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ CVTDQ2PD  _ [op1,op2] _) = cvtdq2pd  ctxt i_a op1 op2

tau_i ctxt (Instruction (AddressWord64 i_a) _ FST      _ [op1]      _)  = fst'     ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ FSTP     _ [op1]      _)  = fstp     ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ FINIT    _ _          _)  = finit'   ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FNINIT   _ _          _)  = fninit   ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FNSTCW   _ [op1]      _)  = fnstcw   ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ FSTCW    _ [op1]      _)  = fstcw    ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ FLD      _ [op1]      _)  = fld      ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ FLD1     _ []         _)  = fld1     ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FLDZ     _ []         _)  = fldz     ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FILD     _ [op1]      _)  = fild     ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ FUCOM    _ _          _)  = fucom    ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FUCOMI   _ _          _)  = fucomi   ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FUCOMIP  _ _          _)  = fucomip  ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FUCOMP   _ _          _)  = fucomp   ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FUCOMPI  _ _          _)  = fucompi  ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FUCOMPP  _ _          _)  = fucompp  ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FCMOVB   _ _          _)  = fcmovxx  ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FCMOVBE  _ _          _)  = fcmovxx  ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FCMOVE   _ _          _)  = fcmovxx  ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FCMOVNE  _ _          _)  = fcmovxx  ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FCMOVU   _ _          _)  = fcmovxx  ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FCMOVNU  _ _          _)  = fcmovxx  ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FCMOVNBE _ _          _)  = fcmovxx  ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FADD     _ [op1]      _)  = fadd1    ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ FADD     _ [op1,op2]  _)  = fadd2    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ FMUL     _ [op1]      _)  = fmul1    ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ FMUL     _ [op1,op2]  _)  = fmul2    ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ FMULP    _ [op1]      _)  = fmulp1   ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ FMULP    _ [op1,op2]  _)  = fmulp2   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ FDIVR    _ [op1]      _)  = fdivr1   ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ FDIVR    _ [op1,op2]  _)  = fdivr2   ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ FDIVRP   _ [op1]      _)  = fdivrp1  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ FDIVRP   _ [op1,op2]  _)  = fdivrp2  ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ FISUB    _ [op1]      _)  = fisub    ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ FISTTP   _ [op1]      _)  = fisttp   ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ FXCH     _ [op1]      _)  = fxch     ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ FCHS     _ []         _)  = fchs     ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ EMMS     _ []         _)  = emms     ctxt i_a


tau_i ctxt (Instruction (AddressWord64 i_a) _ VINSERTF128  _ [op1,op2,op3] _)  = vinsertf128  ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VEXTRACTI128 _ [op1,op2,op3] _)  = vextractf128 ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VEXTRACTF128 _ [op1,op2,op3] _)  = vextracti128 ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VPERM2F128   _ [op1,op2,op3] _)  = vperm2f128   ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VPERM2I128   _ [op1,op2,op3] _)  = vperm2i128   ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VPALIGNR     _ [op1,op2,op3] _)  = vpalignr     ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ PALIGNR      _ [op1,op2,op3] _)  = palignr      ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VANDPS       _ [op1,op2,op3] _)  = vandps       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VSHUFPD      _ [op1,op2,op3] _)  = vshufpd      ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VSHUFPS      _ [op1,op2,op3] _)  = vshufps      ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VADDPD       _ [op1,op2,op3] _)  = vaddpd       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VADDPS       _ [op1,op2,op3] _)  = vaddps       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VSUBPD       _ [op1,op2,op3] _)  = vsubpd       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VSUBPS       _ [op1,op2,op3] _)  = vsubps       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VMULPD       _ [op1,op2,op3] _)  = vmulpd       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VMULPS       _ [op1,op2,op3] _)  = vmulps       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VXORPD       _ [op1,op2,op3] _)  = vxorpd       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VXORPS       _ [op1,op2,op3] _)  = vxorps       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VANDPD       _ [op1,op2,op3] _)  = vandpd       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VANDPS       _ [op1,op2,op3] _)  = vandps       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VMOVDQA      _ [op1,op2,op3] _)  = vmovdqa      ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VMOVDQU      _ [op1,op2,op3] _)  = vmovdqu      ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VMOVLHPS     _ [op1,op2,op3] _)  = vmovlhps     ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VPUNPCKHWD   _ [op1,op2,op3] _)  = vpunpckhwd   ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VPUNPCKLWD   _ [op1,op2,op3] _)  = vpunpcklwd   ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VZEROUPPER   _ []            _)  = vzeroupper   ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ VPCMPEQB     _ [op1,op2,op3] _)  = vpcmpeqb     ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VPCMPEQW     _ [op1,op2,op3] _)  = vpcmpeqw     ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ VPSLLW       _ [op1,op2,op3] _)  = vpsllw       ctxt i_a op1 op2 op3
tau_i ctxt (Instruction (AddressWord64 i_a) _ MOVHPS       _ [op1,op2] _)      = movhps       ctxt i_a op1 op2 
tau_i ctxt (Instruction (AddressWord64 i_a) _ MOVHLPS      _ [op1,op2] _)      = movhlps      ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) _ MOVLHPS      _ [op1,op2] _)      = movlhps      ctxt i_a op1 op2 
tau_i ctxt (Instruction (AddressWord64 i_a) _ VMOVHPS      _ [op1,op2] _)      = vmovhps      ctxt i_a op1 op2 


tau_i ctxt (Instruction (AddressWord64 i_a) _ CPUID    _ []       _)  = cpuid    ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ RDMSR    _ []       _)  = rdmsr    ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ WRMSR    _ []       _)  = wrmsr    ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ RDTSC    _ []       _)  = rdtsc    ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ IN       _ [op1]    _)  = x86_in   ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ OUT      _ [op1]    _)  = x86_out  ctxt i_a op1
tau_i ctxt (Instruction (AddressWord64 i_a) _ CLI      _ []       _)  = cli      ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ CLTS     _ []       _)  = clts     ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ INVPCID  _ _        _)  = invpcid  ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ LGDT     _ _        _)  = lgdt     ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ LIDT     _ _        _)  = lidt     ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ LLDT     _ _        _)  = lldt     ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ LTR      _ _        _)  = ltr      ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ SWAPGS   _ _        _)  = swapgs   ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ XSETBV   _ _        _)  = xsetbv   ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ XSAVEOPT _ _        _)  = xsaveopt ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ XRSTOR   _ _        _)  = xrstor   ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ WRFSBASE _ _        _)  = wrfsbase ctxt i_a
tau_i ctxt (Instruction (AddressWord64 i_a) _ WRGSBASE _ _        _)  = wrgsbase ctxt i_a

tau_i ctxt (Instruction (AddressWord64 i_a) pre CMPS   _ [op1,op2] _) = cmps      ctxt i_a pre op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) pre CMPSB  _ [op1,op2] _) = cmpsb     ctxt i_a pre op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) pre CMPSW  _ [op1,op2] _) = cmpsw     ctxt i_a pre op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) pre CMPSD  _ [op1,op2] _) = cmpsw     ctxt i_a pre op1 op2



tau_i ctxt (Instruction (AddressWord64 i_a) Nothing     XCHG    _ [op1,op2] _) = xchg         ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) (Just LOCK) XADD    _ [op1,op2] _) = xadd         ctxt i_a op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) (Just LOCK) CMPXCHG _ [op1,op2] _) = cmpxchg      ctxt i_a op1 op2

tau_i ctxt (Instruction (AddressWord64 i_a) (Just pre)  MOVSD   _ [op1,op2] _) = movsd_string ctxt pre op1 op2
tau_i ctxt (Instruction (AddressWord64 i_a) (Just pre)  MOVSQ   _ [op1,op2] _) = movsq        ctxt pre op1 op2

tau_i ctxt i =
  if is_jump (instr_opcode i) || is_cond_jump (instr_opcode i) then
    return ()
  else if ctxt_continue_on_unknown_instruction $ f_ctxt ctxt then
    trace ("Unsupported instruction: " ++ show i) $ return () 
  else
    error ("Unsupported instruction: " ++ show i)

-- | Do predicate transformation over a block of instructions.
-- Does not take into account flags, commonly function @`tau_block`@ should be used.
tau_b :: FContext -> [X86_Instruction] -> State (Pred,VCS) ()
tau_b ctxt []  = return ()
tau_b ctxt (i:is) = do
  write_reg ctxt (instr_addr i) RIP (SE_Immediate $ instr_addr i + (fromIntegral $ instr_size i))
  tau_i ctxt i
  tau_b ctxt is



-- TODO JE, other JMP aboves and JUMP lesses
add_jump_to_pred :: X86_Instruction -> X86_Instruction -> FlagStatus -> FlagStatus
add_jump_to_pred i0@(Instruction _ _ JA _ [Immediate trgt] _) i1 flg =
  case flg of
    FS_CMP b o1 o2 -> if instr_addr i1 == fromIntegral trgt then FS_CMP (Just False) o1 o2 else FS_CMP (Just True) o1 o2
    _ -> flg
add_jump_to_pred i0 i1 flg = flg


-- | Do predicate transformation over a basic block in a CFG.
-- Given an edge in the CFG from none block to another, perform predicate transformation.
-- Parameter insts' is needed to set the flags properly. If @Nothing@ is supplied, the flags are overapproximatively set to @`None`@.
tau_block ::
  FContext                    -- ^ The context
  -> [X86_Instruction]        -- ^ The instructions of the basic block
  -> Maybe [X86_Instruction]  -- ^ Optionally, the instructions of the next block if symbolically executing an edge in a CFG.
  -> Pred                     -- ^ The predicate to be transformed
  -> (Pred, VCS)
tau_block ctxt insts insts' p@(Predicate eqs flg) = 
  if insts == [] then
    (p, S.empty)
  else let
      addr                   = instr_addr $ head insts
      eqs'                   = write_rip addr eqs
      (p'',vcs'')            = execState (tau_b ctxt insts) (Predicate eqs' flg, S.empty)
      Predicate eqs'' flgs'' = p'' in
    case insts' of
      Nothing -> (p'', vcs'')
      Just (i':_) ->
        let addr'  = instr_addr i' in
          (Predicate (write_rip addr' eqs'') (add_jump_to_pred (last insts) i' flgs''), vcs'')
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


temp_trace v v' sp = id -- if S.size (srcs_of_expr v'') == 0 && S.size (srcs_of_expr v) > 0 && S.size (srcs_of_expr v') > 0 then traceShow ("temp_trace",sp,v,v',v'') v'' else v''


join_preds ctxt p@(Predicate eqs0 flg0) p'@(Predicate eqs1 flg1) =
  let m    = M.mapWithKey mk_entry eqs0
      flg' = if flg0 == flg1 then flg0 else None
      q    = Predicate m flg' in
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
          fst $ execState (write_sp ctxt 0 mk_mid (sp', v')) (q,S.empty)
        else
          let v = evalState (read_sp ctxt sp') (p,S.empty) in
            fst $ execState (write_sp ctxt 0 mk_mid (sp', temp_trace v v' sp' $ join_expr' ctxt p p' v v')) (q,S.empty)
      Just _  -> q

  mk_mid = MemWriteFunction "joining" -- never used


-- TODO in case of bot, check whether sources of P1 are larger than P0
implies_preds ctxt (Predicate eqs0 flg0) (Predicate eqs1 flg1) = 
 flg1 `elem` [None,flg0] && (all implied_by_eqs0 $ M.toList eqs1) -- && (all_sps_in_eqs1 $ M.keys eqs0)
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
  FContext                 -- ^ The current context
  -> Invariants            -- ^ The currently available invariants
  -> S.Set (NodeInfo,Pred) -- ^ The currently known postconditions
  -> Pred
init_pred ctxt curr_invs curr_posts = 
  let f                    = f_name ctxt
      finit                = f_init ctxt
      finit'               = M.filter (not . contains_bot) finit


      rsp0                 = SE_Var $ SP_StackPointer f
      write_stack_pointer  = M.insert (SP_Reg RSP)    $ rsp0
      write_return_address = M.insert (SP_Mem rsp0 8) $ SE_Var (SP_Mem rsp0 8)
      sps  = S.delete (SP_Reg RIP) $ gather_stateparts curr_invs curr_posts
      eqs  = write_stack_pointer $ write_return_address $ M.union finit' $ M.fromList (map (\sp -> (sp,SE_Var sp)) $ S.toList sps) in
    Predicate eqs None



get_stateparts_of_preds ps = S.unions $ map get_stateparts_of_pred $ S.toList $ ps

get_stateparts_of_pred (Predicate eqs _) = S.filter (not . contains_bot_sp) $ M.keysSet eqs



-- | Given the currently known invariants and postconditions, gather all stateparts occurring in the current function.
gather_stateparts ::
     Invariants            -- ^ The currently available invariants
  -> S.Set (NodeInfo,Pred) -- ^ The currently known postconditions
  -> S.Set StatePart
gather_stateparts invs posts = S.unions [IM.foldrWithKey accumulate_stateparts S.empty invs, get_stateparts_of_preds (S.map snd posts)]
 where
  accumulate_stateparts a p sps = S.union sps (get_stateparts_of_pred p)








-- | Get the invariant for a given instruction address for a given function entry
get_invariant :: FContext -> Int -> Maybe Pred
get_invariant fctxt a = do
  let ctxt   = f_ctxt fctxt
  let entry  = f_entry fctxt
  g         <- IM.lookup entry $ ctxt_cfgs   ctxt
  invs      <- IM.lookup entry $ ctxt_invs   ctxt
  finit     <- IM.lookup entry $ ctxt_finits ctxt
  blockId   <- IM.lookup a $ cfg_addr_to_blockID g
  p         <- IM.lookup blockId invs
  instrs    <- IM.lookup blockId $ cfg_instrs g

  return $ fst $ tau_block fctxt (takeWhile (\i -> fromIntegral (instr_addr i) /= a) instrs) Nothing p






  

instance Propagator FContext Pred where
  tau     = tau_block
  join    = join_preds 
  implies = implies_preds
