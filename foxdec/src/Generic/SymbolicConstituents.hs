{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables #-}

module Generic.SymbolicConstituents where

import qualified X86.Register as Reg
import qualified X86.Operand as X86
import qualified X86.Instruction as X86
import qualified X86.Address as X86
import qualified X86.Opcode as X86
import X86.Instruction (addressof)

import Data.JumpTarget
import Data.SymbolicExpression

import Generic.HasSize
import Generic.Address
import Generic.Operand (GenericOperand(..))
import Generic.Instruction (GenericInstruction(Instruction))


import qualified Data.Serialize as Cereal
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Data.List (intercalate,partition,intersectBy,find)
import Data.Word 

import Control.Monad.State.Strict hiding (join)
import Control.DeepSeq

import GHC.Generics (Generic)
import Debug.Trace


-- |  A verification condition is either:
-- * A precondition of the form:
-- >    Precondition (a0,si0) (a1,si1)
-- This formulates that at the initial state the two regions must be separate.
-- * An assertion of the form:
-- >    Assertion a (a0,si0) (a1,si1)
-- This formulates that dynamically, whenever address a is executed, the two regions are asserted to be separate.
-- * A function constraint of the form:
-- >    FunctionConstraint foo [(RDI, v0), (RSI, v1), ...]   { sp0,sp1,... }
-- This formulates that a function call to function foo with values v0, v1, ... stored in the registers should not overwrite certain state parts.
data VerificationCondition =
    Precondition          SimpleExpr Int SimpleExpr Int                               -- ^ Precondition:           lhs SEP rhs
  | Assertion             SimpleExpr SimpleExpr Int SimpleExpr Int                    -- ^ Assertion:    @address, lhs SEP rhs
  | FunctionConstraint    String Word64 [(Reg.Register,SimpleExpr)] (S.Set StatePart) -- ^ Function name, address, of call, with param registers
  | FunctionPointers      Word64 IS.IntSet                                             -- ^ A set of function pointers passed to a function
  deriving (Generic,Eq,Ord)

-- | An acornym for a set of verification conditions
type VCS = S.Set VerificationCondition



-- | Add a function_pointer_intro to the given symbolic predicate
add_function_pointer a ptr (s,vcs) =
  let (match,remainder) = S.partition belongs_to_a vcs in
    case S.toList match of
      []                         -> (s,S.insert (FunctionPointers a $ IS.singleton ptr) remainder)
      [FunctionPointers _ ptrs'] -> (s,S.insert (FunctionPointers a $ IS.insert ptr ptrs') remainder)
 where
  belongs_to_a (FunctionPointers a' _) = a == a'
  belongs_to_a _                       = False










data SymbolicOperation a = SO_Op X86.Opcode Int (Maybe Int) [a] | SO_Bit Int a | SO_SExtend Int Int a | SO_Overwrite Int a a | SO_Plus a a | SO_Minus a a | SO_Times a a

class (Ord a, Eq a,Show a) => SymbolicExecutable ctxt a where 
  sseparate :: ctxt -> String -> a -> a -> a -> a -> Bool
  senclosed :: ctxt -> a -> a -> a -> a -> Bool
  salias :: ctxt -> a -> a -> a -> a -> Bool
  ssensitive :: ctxt -> a -> a -> a -> Bool
  sread_from_ro_data :: ctxt -> a -> a -> Maybe a
  smk_mem_addresses :: ctxt -> String -> a -> S.Set a

  sjoin :: Foldable t => ctxt -> String -> t a -> a
  swiden :: ctxt -> String -> a -> a
  top :: ctxt -> String -> a

  ssemantics :: ctxt -> String -> SymbolicOperation a -> a 
  sflg_semantics :: ctxt -> a -> X86.Instruction -> FlagStatus -> FlagStatus
  simmediate :: Integral i => ctxt -> i -> a

  mk_svalue :: ctxt -> SimpleExpr -> a
  mk_smem_value :: ctxt -> String -> a -> a -> a

  
  scall :: ctxt -> Bool -> X86.Instruction -> State (Sstate a,VCS) ()
  sjump :: ctxt -> X86.Instruction -> State (Sstate a,VCS) ()

  stry_jump_targets :: ctxt -> a -> Maybe (S.Set ResolvedJumpTarget)


  stry_immediate :: ctxt -> a -> Maybe Word64
  stry_deterministic :: ctxt -> a -> Maybe SimpleExpr
  stry_relocation :: ctxt -> a -> a -> Maybe a
  svalue_to_exprs :: ctxt -> a -> S.Set SimpleExpr
  saddress_has_instruction :: ctxt -> a -> Word64 -> Bool

data Sstate a = Sstate {
    sregs  :: M.Map Reg.Register a,
    smem   :: S.Set ((a,a),a), -- TODO make map
    sflags :: FlagStatus
  }
  deriving (Eq,Ord,Generic)




instance (Show a) => Show (Sstate a) where
  show (Sstate regs mem flags) = intercalate "\n" $ (map show_reg $ M.assocs regs) ++ (map show_mem $ S.toList mem) ++ [show_flags flags]
   where
    show_reg (r,v)      = show r ++ " := " ++ show v
    show_mem ((a,si),v) = "[" ++ show a ++ "," ++ show si ++ "] := " ++ show v 
    show_flags          = show

instance (Ord a, Cereal.Serialize a) => Cereal.Serialize (Sstate a)
instance (NFData a) => NFData (Sstate a)



execSstate :: State (Sstate a, VCS) b -> Sstate a -> Sstate a
execSstate m s = fst $ execState m (s,S.empty)

evalSstate :: State (Sstate a, VCS) b -> Sstate a -> b
evalSstate m s = evalState m (s,S.empty)



-- | Read from a register
sread_rreg ctxt r (s,_) = 
  case M.lookup r (sregs s) of
    Nothing -> mk_svalue ctxt (SE_Var (SP_Reg r))
    Just v  -> v

sread_reg :: SymbolicExecutable ctxt a => ctxt -> Reg.Register -> State (Sstate a,VCS) a
sread_reg ctxt r = do
  let rr = Reg.real r
  v <- gets $ sread_rreg ctxt rr
  if sizeof r == 32 then -- 256 bits
    return v
  else if sizeof r == 16 then -- 128 bit
    return $ ssemantics ctxt "read_reg 128" $ SO_Bit 128 v
  else if sizeof r == 8 then -- 64 bit
    return v
  else if sizeof r == 4 then do -- 32 bit
    rip <- gets $ sread_rreg ctxt $ Reg.RIP
    return $ ssemantics ctxt ("@" ++ show rip ++ ": read_reg 32") $ SO_Bit 32 v
  else if r `elem` Reg.reg16 then -- 16 bit
    return $ ssemantics ctxt "read_reg 16" $ SO_Bit 16 v
  else if r `elem` Reg.reg8 then -- 8 bit low registers
    return $ ssemantics ctxt "read_reg 8" $ SO_Bit 8 v
  else -- 8 bit hi
    return $ swiden ctxt "read_reg" v


-- | Write to a register
swrite_rreg :: SymbolicExecutable ctxt a => ctxt -> Reg.Register -> a -> State (Sstate a,VCS) () 
swrite_rreg ctxt r v = 
  if take 2 (show r) == "ST" then
    return ()
  else do
    (s,vcs) <- get
    put $ (s {sregs = M.insert r v (sregs s), sflags = clean_flg (SP_Reg r) (sflags s) }, vcs)

soverwrite_reg :: SymbolicExecutable ctxt a => ctxt -> Bool -> Reg.Register -> a -> State (Sstate a,VCS) ()
soverwrite_reg ctxt use_existing_value r v = do
 let sz  = sizeof r in
  if sz == 32 then -- 256 bit
    swrite_rreg ctxt r v
  else if sz == 16 then do -- 128 bit
    let rr = Reg.real r
    swrite_rreg ctxt rr (ssemantics ctxt "overreg 128" $ SO_Bit 128 v)
  else if sz == 8 then -- 64 bit
    swrite_rreg ctxt r v
  else if sz == 4 then do -- 32 bit
    let rr = Reg.real r
    swrite_rreg ctxt rr (ssemantics ctxt "overreg 32" $ SO_Bit 32 v)
  else if sz == 2 then do -- 16 bit 
    let rr = Reg.real r
    curr_v <- gets $ sread_rreg ctxt rr
    let v' = if use_existing_value then SO_Overwrite 16 curr_v (ssemantics ctxt "overreg 16" $ SO_Bit 16 v) else SO_Bit 16 v
    swrite_rreg ctxt rr $ ssemantics ctxt "overreg' 16" v'
  else if r `elem` Reg.reg8 then do -- 8 bit low registers
    let rr = Reg.real r
    curr_v <- gets $ sread_rreg ctxt rr
    let v' = if use_existing_value then SO_Overwrite 8 curr_v (ssemantics ctxt "overreg 8" $ SO_Bit 8 v) else SO_Bit 8 v
    swrite_rreg ctxt rr $ ssemantics ctxt "overreg' 8" v'
  else do
    let rr = Reg.real r
    curr_v <- gets $ sread_rreg ctxt rr
    let v' = swiden ctxt "write_reg (bitmode)" $ sjoin ctxt "join write_reg (bitmode)" [v,curr_v]
    swrite_rreg ctxt rr $ v
 where

swrite_reg :: SymbolicExecutable ctxt a => ctxt -> Reg.Register -> a -> State (Sstate a,VCS) ()
swrite_reg ctxt = soverwrite_reg ctxt True


-- | Read from memory
sread_mem :: SymbolicExecutable ctxt a => ctxt -> String -> a -> a -> State (Sstate a,VCS) a
sread_mem ctxt msg a si
  | a == top ctxt "" = return $ top ctxt "read_mem from top"
  | otherwise =
      case stry_relocation ctxt a si of
        Just v -> return v
        _      -> 
          case sread_from_ro_data ctxt a si of
            Just v -> return v
            _      -> sread_mem'
 where
  sread_mem' = do
    (s,vcs) <- get
    case find (\((a0,si0),v0) -> (a0,si0) == (a,si)) $ smem s of
      Just (_,v) -> return v
      _ -> sread_mem''
  sread_mem'' = do
    (s,vcs) <- get
    let (_,overlap) = S.partition ssep $ smem s
    if S.size overlap == 1 then
      let ((a0,si0),v0) = S.findMin overlap in
        if salias ctxt a si a0 si0 then
          return v0
        else if senclosed ctxt a si a0 si0 then
          return $ swiden ctxt "read_mem (enclosure)" v0
        else
          return $ read_from_overlap s overlap
    else
      return $ read_from_overlap s overlap
 
  ssep ((a0,si0),v0) = ((a,si) /= (a0,si0) && ssensitive ctxt a0 si0 v0) || sseparate ctxt "read" a0 si0 a si

  read_from_overlap s overlap
    | S.size overlap == 0 = mk_smem_value ctxt (msg ++ "\nmaking mem value:\n" ++ show s ++ "\n" ++ show (a,si)) a si
    | otherwise = -- error ("Overlapping read:\nReading from: " ++ show (a,si,S.map fst overlap) ++ "\nIn state:\n" ++ show s) 
                  swiden ctxt "read_mem (overlap)" $ sjoin ctxt (msg ++ "\nRead joining: " ++ show (a,si) ++ "\n" ++ show s ++ "\n" ++ show overlap) (S.map snd overlap)



-- | Write to memory
swrite_mem :: SymbolicExecutable ctxt a => ctxt -> a -> a -> a -> State (Sstate a,VCS) ()
swrite_mem ctxt a_v si v = do
  let as = smk_mem_addresses ctxt "swrite_mem" a_v
  mapM_ swrite_mem' as
 where
  swrite_mem' a = do
    modify $ \(s,vcs) -> (s { sflags = clean_flg (SP_Mem (SE_Immediate 0) 0) (sflags s) }, vcs)
    (s,vcs) <- get

    let (equal,enclosed_by,encloses,separate,overlap) = do_partitioning (smem s) a
    if equal /= [] then
      put $ (s { smem = S.fromList (((a,si),v) : (enclosed_by ++ encloses ++ separate ++ overlap)) }, vcs)
    else if enclosed_by /= [] then do
      (p,_) <- get
      let v' = swiden ctxt "write_mem (enclosure)" $ sjoin ctxt ("MemWrite (enclosure)" ++ show (a_v,a,si,enclosed_by) ++ "\n" ++ show p) (v : map snd enclosed_by)
      put $ (s { smem = S.fromList $ (fst $ head enclosed_by,v') : encloses ++ separate ++ overlap }, vcs)
    else if encloses /= [] then do
      let v' = swiden ctxt "write_mem (encloses)" $ sjoin ctxt "MemWrite (encloses)" (v : map snd encloses)
      put $ (s { smem = S.fromList $ ((a,si),v') : separate ++ overlap }, vcs)
    else if overlap == [] then
      put $ (s { smem = S.fromList $ ((a,si),v) : separate }, vcs)
    else let ((a',si'),v') = merge $ ((a,si),v):overlap in
      if a' == top ctxt "" then
        --trace ("TOP: writing to " ++ show (a,si) ++ "\nIn state:\n" ++ show s ++ show (map fst overlap)) -- $ -- TODO add VCS
        return ()
          --put $ (s { smem = S.fromList $ (assign_top overlap ++ separate) }, vcs)
      else do
        let as'  = smk_mem_addresses ctxt "swrite_mem" a'
        let mem' = S.map (\a' -> ((a',si'),v')) as'
        --trace ("Overlapping write:\nWriting to: " ++ show (a,si,map fst overlap) ++ "\nIn state:\n" ++ show s) $
        put $ (s { smem = S.union mem' (S.fromList separate) }, vcs)
  
  do_partitioning m a = S.foldr' (do_partition a) ([],[],[],[],[]) m
  do_partition a ((a0,si0),v0) (equal,enclosing,encloses,separate,overlap) 
    | salias     ctxt          a0 si0 a si = (((a0,si0),v0):equal,enclosing,encloses,separate,overlap) 
    | senclosed  ctxt          a si a0 si0 = (equal,((a0,si0),v0):enclosing,encloses,separate,overlap) 
    | senclosed  ctxt          a0 si0 a si = (equal,enclosing,((a0,si0),v0):encloses,separate,overlap)
    | sseparate  ctxt "wwrite" a si a0 si0 = (equal,enclosing,encloses,((a0,si0),v0):separate,overlap) 
    | ssensitive ctxt          a0 si0 v0   = (equal,enclosing,encloses,((a0,si0),v0):separate,overlap) -- TODO: VCS
    | otherwise                            = (equal,enclosing,encloses,separate,((a0,si0),v0):overlap) 

  merge [r] = r
  merge (((a0,si0),v0):remainder) = 
    let ((a1,si1),v1) = merge remainder 
        a'            = swiden ctxt ("Joining regions: " ++ show [a0,si0,a1,si1])  $ sjoin ctxt "MemWrite (address)" [a0,a1] 
        si'           = top ctxt "write_mem (overlap,size)"
        v'            = swiden ctxt "write_mem (overlapV)"  $ sjoin ctxt "MemWrite (values)"  [v0,v1] in
      ((a',si'),v')
  

  --assign_top = map (\(r,v) -> (r,top ctxt))



-- | Given the address of an operand of an instruction, resolve it given the current state.
sresolve_address :: SymbolicExecutable ctxt a => ctxt -> X86.Address -> State (Sstate a,VCS) a 
sresolve_address ctxt (AddressStorage r) = sread_reg ctxt r
sresolve_address ctxt (AddressImm i)     = return $ simmediate ctxt i
sresolve_address ctxt (AddressMinus a0 a1) = do
  ra0 <- sresolve_address ctxt a0 
  ra1 <- sresolve_address ctxt a1
  return $ ssemantics ctxt "min" $ SO_Minus ra0 ra1
sresolve_address ctxt (AddressPlus a0 a1) = do
  ra0 <- sresolve_address ctxt a0 
  ra1 <- sresolve_address ctxt a1
  return $ ssemantics ctxt "plus" $ SO_Plus ra0 ra1
sresolve_address ctxt (AddressTimes a0 a1) = do
  ra0 <- sresolve_address ctxt a0 
  ra1 <- sresolve_address ctxt a1
  return $ ssemantics ctxt "times" $ SO_Times ra0 ra1


sread_operand :: SymbolicExecutable ctxt a => ctxt -> String -> X86.Operand -> State (Sstate a,VCS) a
sread_operand ctxt msg (Storage r)          = sread_reg ctxt r
sread_operand ctxt msg (EffectiveAddress a) = sresolve_address ctxt a
sread_operand ctxt msg (Immediate a)        = return $ simmediate ctxt a
sread_operand ctxt msg (Memory a si)        = do
  resolved_address <- sresolve_address ctxt a 
  sread_mem ctxt msg resolved_address (simmediate ctxt si)

swrite_operand :: SymbolicExecutable ctxt a => ctxt -> Bool -> X86.Operand -> a -> State (Sstate a,VCS) ()
swrite_operand ctxt use_existing_value (Storage r)   v = soverwrite_reg ctxt use_existing_value r v
swrite_operand ctxt use_existing_value (Memory a si) v = do
  resolved_address <- sresolve_address ctxt a 
  swrite_mem ctxt resolved_address (simmediate ctxt si) v


-- v is bogus, but needed for getting the type checker to accept this. Don't know why. 
swrite_flags :: SymbolicExecutable ctxt a => ctxt -> a -> X86.Instruction -> State (Sstate a,VCS) ()
swrite_flags ctxt v i = do
  (s,vcs) <- get
  put $ (s {sflags = sflg_semantics ctxt v i (sflags s)}, vcs)


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


-- TODO JE, other JMP aboves and JUMP lesses
add_jump_to_pred :: X86.Instruction -> X86.Instruction -> FlagStatus -> FlagStatus
add_jump_to_pred i0@(Instruction _ _ X86.JA _ [Immediate trgt] _) i1 flg =
  case flg of
    FS_CMP b o1 o2 -> if addressof i1 == fromIntegral trgt then FS_CMP (Just False) o1 o2 else FS_CMP (Just True) o1 o2
    _ -> flg
add_jump_to_pred i0@(Instruction _ _ X86.JBE _ [Immediate trgt] _) i1 flg =
  case flg of
    FS_CMP b o1 o2 -> if addressof i1 == fromIntegral trgt then FS_CMP (Just True) o1 o2 else FS_CMP (Just False) o1 o2
    _ -> flg
add_jump_to_pred i0 i1 flg = flg



sreturn :: SymbolicExecutable ctxt a => ctxt -> State (Sstate a,VCS) ()
sreturn ctxt = do
  e0 <- sread_operand ctxt "return" (Memory (AddressStorage Reg.RSP) 8)
  let address = AddressPlus (AddressStorage Reg.RSP) (AddressImm 8)
  e1 <- sresolve_address ctxt address
  swrite_reg ctxt Reg.RSP e1
  swrite_operand ctxt True (Storage Reg.RIP) e0


-- LEA
slea :: SymbolicExecutable ctxt a => ctxt -> AddressWord64 -> X86.Operand -> X86.Operand -> State (Sstate a,VCS) ()
slea ctxt (AddressWord64 i_a) dst (EffectiveAddress a) = do
  e <- sresolve_address ctxt a
  swrite_operand ctxt True dst e
  case stry_immediate ctxt e of
    Nothing  -> return ()
    Just imm -> if saddress_has_instruction ctxt e imm then modify $ add_function_pointer i_a $ fromIntegral imm else return ()

-- MOV
smov :: SymbolicExecutable ctxt a => ctxt -> a -> X86.Instruction -> State (Sstate a,VCS) ()
smov ctxt a i@(Instruction label prefix X86.MOV (Just dst) [src@(Immediate imm)] annot)
  | saddress_has_instruction ctxt a imm   = slea ctxt label dst (EffectiveAddress (AddressImm imm))
  | otherwise                             = sgeneric_cinstr ctxt i
smov ctxt a i                             = sgeneric_cinstr ctxt i


sgeneric_cinstr :: SymbolicExecutable ctxt a => ctxt -> X86.Instruction -> State (Sstate a,VCS) ()
sgeneric_cinstr ctxt i@(Instruction label prefix mnemonic (Just dst) srcs annot) = do
  ops <- mapM (sread_operand ctxt (show i)) srcs
  swrite_operand ctxt True dst $ ssemantics ctxt (show i) $ SO_Op mnemonic (operand_size dst) (maybe_operand_size srcs) ops

operand_size (Storage r)   = sizeof r
operand_size (Memory a si) = si
operand_size (EffectiveAddress _) = 8

maybe_operand_size []   = Nothing
maybe_operand_size srcs
  | any is_immediate srcs = Nothing
  | otherwise =
    let sizes = map operand_size srcs in
      if all ((==) (head sizes)) (tail sizes) then
        Just $ head sizes
      else
        Nothing
 where
  is_immediate (Immediate _) = True
  is_immediate _             = False

sexec_cinstr :: SymbolicExecutable ctxt a => ctxt -> X86.Instruction -> State (Sstate a,VCS) ()
-- sexec_cinstr ctxt i | trace ("sexec_cinstr: "++ show i) False = error "trace"
sexec_cinstr ctxt i@(Instruction label prefix mnemonic (Just dst) srcs annot)
  | mnemonic == X86.LEA                     = slea ctxt label dst $ head srcs
  | mnemonic == X86.MOV                     = smov ctxt (top ctxt "") i
  | mnemonic == X86.XOR && dst == head srcs = swrite_operand ctxt False dst $ simmediate ctxt 0
  | otherwise                               = sgeneric_cinstr ctxt i
sexec_cinstr ctxt i@(Instruction label prefix mnemonic Nothing _ _)
  | X86.isRet mnemonic        = sreturn ctxt
  | X86.isJump mnemonic       = sjump ctxt i
  | X86.isCall mnemonic       = scall ctxt False i
  | otherwise                 = return ()






sset_rip :: SymbolicExecutable ctxt a => ctxt -> X86.Instruction -> State (Sstate a,VCS) ()
sset_rip ctxt i = swrite_reg ctxt Reg.RIP (simmediate ctxt $ addressof i + (fromIntegral $ sizeof i))

sexec_instr :: SymbolicExecutable ctxt a => ctxt -> X86.Instruction -> State (Sstate a,VCS) ()
-- sexec_instr ctxt i | trace ("sexec_isntr: "++ show i) False = error "trace"
sexec_instr ctxt i = do
  (p,_) <- get
  sset_rip ctxt i
  mapM_ (sexec_cinstr ctxt) $ X86.canonicalize i
  swrite_flags ctxt (top ctxt "") i




sexec_block :: SymbolicExecutable ctxt a => ctxt -> [X86.Instruction] -> Maybe [X86.Instruction] -> Sstate a -> (Sstate a,VCS)
sexec_block ctxt []    _    s = (s,S.empty)
sexec_block ctxt block next s = 
  let  m = swrite_reg ctxt Reg.RIP (simmediate ctxt $ addressof (head block)) >> mapM (sexec_instr ctxt) block >> (set_flagstatus next) in
    execState m (s,S.empty)
 where
  set_flagstatus Nothing       = return ()
  set_flagstatus (Just (i':_)) = do
    swrite_reg ctxt Reg.RIP (simmediate ctxt $ addressof i')
    modify $ \(s,vcs) -> (s { sflags = add_jump_to_pred (last block) i' (sflags s) }, vcs)



sjoin_mem :: SymbolicExecutable ctxt a => ctxt -> String -> Sstate a -> Sstate a -> S.Set ((a, a), a)
sjoin_mem ctxt msg s0 s1 =
  let m0     = S.toList $ smem s0
      m1     = S.toList $ smem s1
      shared = M.intersectionWith join_values (M.fromList m0) (M.fromList m1)
      diff   = filter (\(r,_) -> r `M.notMember` shared) (m0 ++ m1) in
    sjoin_mem' (S.fromList $ M.toList shared) $ diff
 where
  sjoin_mem' m [] = m 
  sjoin_mem' m (((a,si),v):regions)
    | otherwise =
      let v0 = evalSstate (sread_mem ctxt (msg ++ "join0" ++ show (a,si,s0,s1)) a si) s0
          v1 = evalSstate (sread_mem ctxt (msg ++ "join0" ++ show (a,si,s0,s1)) a si) s1
          v' = join_values v0 v1
          m' = smem $ execSstate (swrite_mem ctxt a si v') $ Sstate {sregs = M.empty, smem = m, sflags = None} in
        sjoin_mem' m' regions

  join_values a b = sjoin ctxt (msg ++ "States (mem value) " ++ show a ++ " joined with " ++ show b) [a,b]




-- sjoin_states ctxt msg s0 s1 | trace ("sjoin_states") False = error "trace"
sjoin_states ctxt msg s0@(Sstate regs0 mem0 flg0) s1@(Sstate regs1 mem1 flg1) =
  let flg  = join_flags flg0 flg1
      regs = M.unionWith join_reg regs0 regs1
      mem  = sjoin_mem ctxt msg s0 s1
      s'   = Sstate regs mem flg in
     s'
 where
  join_reg a b 
    | a == b    = a
    | otherwise = sjoin ctxt ("States (regs)" ++ show s0 ++ "\n\n\n" ++ show s1 ++ "\n" ++ show(a,b)) [a,b]
  join_flags (FS_CMP (Just True) o0 v0) (FS_CMP (Just True) o0' v0')
    | flg0 == flg1 = flg0
    | o0 /= o0'    = None
    | otherwise    = 
      case (v0,v0') of
        (Immediate n0,Immediate n1) -> FS_CMP (Just True) o0 (Immediate $ max n0 n1)
        _ -> None
  join_flags _ _ 
    | flg0 == flg1 = flg0
    | otherwise    = None


-- | The supremum of a list of predicates
supremum :: SymbolicExecutable ctxt a => ctxt -> [Sstate a] -> Sstate a
supremum ctxt [] = error $ "Cannot compute supremum of []"
supremum ctxt ss = foldr1 (sjoin_states ctxt "supremum") ss

simplies ctxt s0 s1 = (set_rip $ sjoin_states ctxt "simplies" s0 s1) == (set_rip s0)
 where
  set_rip = execSstate (swrite_rreg ctxt Reg.RIP (simmediate ctxt 0))




