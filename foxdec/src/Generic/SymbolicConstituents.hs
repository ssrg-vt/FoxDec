{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

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










data SymbolicOperation v = SO_Op X86.Opcode Int (Maybe Int) [v] | SO_Bit Int v | SO_SExtend Int Int v | SO_Overwrite Int v v | SO_Plus v v | SO_Minus v v | SO_Times v v

data RegionSize = Nat Word64 | UnknownSize
 deriving (Eq,Ord,Generic)

class (Ord v,Eq v,Show v, Eq p,Ord p,Show p) => SymbolicExecutable ctxt v p | ctxt -> v p where 
  sseparate :: ctxt -> String -> p -> RegionSize -> p -> RegionSize -> Bool
  senclosed :: ctxt -> p -> RegionSize -> p -> RegionSize -> Bool
  salias :: ctxt -> p -> RegionSize -> p -> RegionSize -> Bool
  ssensitive :: ctxt -> p -> RegionSize -> v -> Bool
  sread_from_ro_data :: ctxt -> p -> RegionSize -> Maybe v
  smk_mem_addresses :: ctxt -> String -> v -> S.Set p

  sjoin_values :: Foldable t => ctxt -> String -> t v -> v
  swiden_values :: ctxt -> String -> v -> v
  sjoin_pointers :: ctxt -> [p] -> [p]
  top :: ctxt -> String -> v


  ssemantics :: ctxt -> String -> SymbolicOperation v -> v
  sflg_semantics :: ctxt -> v -> X86.Instruction -> FlagStatus -> FlagStatus
  simmediate :: Integral i => ctxt -> i -> v

  smk_init_reg_value :: ctxt -> Reg.Register -> v
  smk_init_mem_value :: ctxt -> String -> p -> RegionSize -> v

  scall :: ctxt -> Bool -> X86.Instruction -> State (Sstate v p,VCS) ()
  sjump :: ctxt -> X86.Instruction -> State (Sstate v p,VCS) ()

  stry_jump_targets :: ctxt -> v -> Maybe (S.Set ResolvedJumpTarget)

  stry_immediate :: ctxt -> v -> Maybe Word64
  stry_deterministic :: ctxt -> v -> Maybe SimpleExpr
  stry_relocation :: ctxt -> p -> RegionSize -> Maybe v
  saddress_has_instruction :: ctxt -> Word64 -> Bool

data Sstate v p = Sstate {
    sregs  :: M.Map Reg.Register v,
    smem   :: M.Map (p,RegionSize) v,
    sflags :: FlagStatus
  }
  deriving (Eq,Ord,Generic)


instance Show RegionSize where
  show (Nat imm)  = show imm
  show UnknownSize = "u"

instance (Show v,Show p) => Show (Sstate v p) where
  show (Sstate regs mem flags) = intercalate "\n" $ (map show_reg $ M.assocs regs) ++ (map show_mem $ M.assocs mem) ++ [show_flags flags]
   where
    show_reg (r,v)      = show r ++ " := " ++ show v
    show_mem ((a,si),v) = "[" ++ show a ++ "," ++ show si ++ "] := " ++ show v 
    show_flags          = show

instance Cereal.Serialize RegionSize
instance (Ord v, Cereal.Serialize v,Ord p, Cereal.Serialize p) => Cereal.Serialize (Sstate v p)
instance NFData RegionSize
instance (NFData v,NFData p) => NFData (Sstate v p)



execSstate :: State (Sstate v p, VCS) b -> Sstate v p -> Sstate v p
execSstate m s = fst $ execState m (s,S.empty)

evalSstate :: State (Sstate v p, VCS) a -> Sstate v p -> a
evalSstate m s = evalState m (s,S.empty)



-- | Read from a register
sread_rreg ctxt r (s,_) = 
  case M.lookup r (sregs s) of
    Nothing -> smk_init_reg_value ctxt r
    Just v  -> v

sread_reg :: SymbolicExecutable ctxt v p => ctxt -> Reg.Register -> State (Sstate v p,VCS) v
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
    return $ swiden_values ctxt "read_reg" v


-- | Write to a register
swrite_rreg :: SymbolicExecutable ctxt v p => ctxt -> Reg.Register -> v -> State (Sstate v p,VCS) () 
swrite_rreg ctxt r v = 
  if take 2 (show r) == "ST" then
    return ()
  else do
    (s,vcs) <- get
    put $ (s {sregs = M.insert r v (sregs s), sflags = clean_flg (SP_Reg r) (sflags s) }, vcs)

soverwrite_reg :: SymbolicExecutable ctxt v p => ctxt -> Bool -> Reg.Register -> v -> State (Sstate v p,VCS) ()
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
    let v' = swiden_values ctxt "write_reg (bitmode)" $ sjoin_values ctxt "join write_reg (bitmode)" [v,curr_v]
    swrite_rreg ctxt rr $ v

swrite_reg :: SymbolicExecutable ctxt v p => ctxt -> Reg.Register -> v -> State (Sstate v p,VCS) ()
swrite_reg ctxt = soverwrite_reg ctxt True


-- | Read from memory
sread_mem :: SymbolicExecutable ctxt v p => ctxt -> String -> v -> RegionSize -> State (Sstate v p,VCS) v
--sread_mem ctxt msg a si | trace ("sread_mem: "++ show (a,si)) False = error "trace"
sread_mem ctxt msg a si = do
  (p,_) <- get
  let ptrs = smk_mem_addresses ctxt ("sread_mem" ++ show (msg,a,si,p)) a
  if S.null ptrs then
    return $ top ctxt "read_mem from top"
  else do
    vs <- mapM (\ptr -> sread_mem_from_ptr ctxt msg ptr si) $ S.toList ptrs
    return $ sjoin_values ctxt (msg ++ "\nRead join:" ++ show (ptrs,vs)) vs

sread_mem_from_ptr :: SymbolicExecutable ctxt v p => ctxt -> String -> p -> RegionSize -> State (Sstate v p,VCS) v
--sread_mem_from_ptr ctxt msg a si | trace ("sread_mem_from_ptr: "++ show (a,si)) False = error "trace"
sread_mem_from_ptr ctxt msg a si =
  case stry_relocation ctxt a si of
    Just v -> return v
    _      -> 
      case sread_from_ro_data ctxt a si of
        Just v -> return v
        _      -> sread_mem' a
 where
  sread_mem' a = do
    (s,vcs) <- get
    case find (\((a0,si0),v0) -> (a0,si0) == (a,si) && ssensitive ctxt a0 si0 v0) $ M.assocs $ smem s of
      Just ((a0,si0),v0) -> return v0
      _ -> sread_mem'' a
  sread_mem'' a = do
    (s,vcs) <- get
    let (_,overlap) = M.partitionWithKey ssep $ smem s
    if M.size overlap == 1 then
      let [((a0,si0),v0)] = M.assocs overlap in
        if salias ctxt a si a0 si0 then
          return v0
        else if senclosed ctxt a si a0 si0 then
          return $ swiden_values ctxt "read_mem (enclosure)" v0
        else
          return $ read_from_overlap a s overlap
    else
      return $ read_from_overlap a s overlap
 
  ssep (a0,si0) v0 = ((a,si) /= (a0,si0) && ssensitive ctxt a0 si0 v0) || sseparate ctxt "read" a0 si0 a si

  read_from_overlap a s overlap
    | M.size overlap == 0 = smk_init_mem_value ctxt (msg ++ "\nmaking init mem value:\n" ++ show s ++ "\n" ++ show (a,si)) a si
    | otherwise = -- error ("Overlapping read:\nReading from: " ++ show (a,si,S.map fst overlap) ++ "\nIn state:\n" ++ show s) 
                  swiden_values ctxt (msg' "widen" a s overlap) $ sjoin_values ctxt (msg' "joining" a s overlap) (M.elems overlap)

  msg' name a s overlap = msg ++ "\nRead " ++ name ++ ": " ++ show (a,si) ++ "\n" ++ show s ++ "\n" ++ show overlap



-- | Write to memory
swrite_mem :: SymbolicExecutable ctxt v p => ctxt -> Bool -> v -> RegionSize -> v -> State (Sstate v p,VCS) ()
swrite_mem ctxt use_existing_value a si v = do
  (p,_) <- get
  let ptrs = smk_mem_addresses ctxt ("swrite_mem\n"++show p) a
  if S.null ptrs then
    --trace ("TOP: writing to " ++ show (a,si) ++ "\nIn state:\n" ++ show p) $
    return () -- TODO ADD VC
  else
    mapM_ (\ptr -> swrite_mem_to_ptr ctxt use_existing_value ptr si v) ptrs

swrite_mem_to_ptr :: SymbolicExecutable ctxt v p => ctxt -> Bool -> p -> RegionSize -> v -> State (Sstate v p,VCS) ()
swrite_mem_to_ptr ctxt use_existing_value a si v = do
  modify $ \(s,vcs) -> (s { sflags = clean_flg (SP_Mem (SE_Immediate 0) 0) (sflags s) }, vcs)
  (s,vcs) <- get

  let (equal,enclosed_by,encloses,separate,overlap) = do_partitioning (smem s) a
  if equal /= [] then
    put $ (s { smem = combine [ [((a,si),v)], enclosed_by, encloses, separate, overlap] }, vcs)
  else if enclosed_by /= [] then do
    (p,_) <- get
    let v' = swiden_values ctxt "write_mem (enclosure)" $ sjoin_values ctxt ("MemWrite (enclosure)" ++ show (a,si,enclosed_by) ++ "\n" ++ show p) (v : map snd enclosed_by)
    put $ (s { smem = combine [ [(fst $ head enclosed_by,v')], encloses, separate, overlap] }, vcs)
  else if encloses /= [] then do
    let v' = swiden_values ctxt "write_mem (encloses)" $ sjoin_values ctxt "MemWrite (encloses)" (v : map snd encloses)
    put $ (s { smem = combine [ [((a,si),v')], separate, overlap] }, vcs)
  else if overlap == [] then
    put $ (s { smem = combine [ [((a,si),v)],  separate ] }, vcs)
  else let m' = merge ctxt $ ((a,si),v):overlap in
    --trace ("Overlapping write:\nWriting to: " ++ show (a,si,map fst overlap) ++ "\nIn state:\n" ++ show s) $
    put $ (s { smem = combine [ m',  separate ] }, vcs)
 where
  combine = M.fromList . concat

  do_partitioning m a = M.foldrWithKey (do_partition a) ([],[],[],[],[]) m
  do_partition a (a0,si0) v0 (equal,enclosing,encloses,separate,overlap) 
    | salias     ctxt          a0 si0 a si = (((a0,si0),v0):equal,enclosing,encloses,separate,overlap) 
    | senclosed  ctxt          a si a0 si0 = (equal,((a0,si0),v0):enclosing,encloses,separate,overlap) 
    | senclosed  ctxt          a0 si0 a si = (equal,enclosing,((a0,si0),v0):encloses,separate,overlap)
    | sseparate  ctxt "wwrite" a si a0 si0 = (equal,enclosing,encloses,((a0,si0),v0):separate,overlap) 
    | ssensitive ctxt          a0 si0 v0   = (equal,enclosing,encloses,((a0,si0),v0):separate,overlap) -- TODO: VCS
    | not use_existing_value               = (equal,enclosing,encloses,((a0,si0),v0):separate,overlap)
    | otherwise                            = (equal,enclosing,encloses,separate,((a0,si0),v0):overlap) 

  merge ::  SymbolicExecutable ctxt v p => ctxt -> [((p, RegionSize), v)] -> [((p, RegionSize), v)]
  merge ctxt m =
    let ptrs = sjoin_pointers ctxt $ map (fst . fst) m
        v'   = swiden_values ctxt "write_mem (overlap)" $ sjoin_values ctxt "MemWrite (values)" $ map snd m in
      map (\ptr -> ((ptr,UnknownSize),v')) ptrs




-- | Given the address of an operand of an instruction, resolve it given the current state.
sresolve_address :: SymbolicExecutable ctxt v p => ctxt -> X86.Address -> State (Sstate v p,VCS) v
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


sread_operand :: SymbolicExecutable ctxt v p => ctxt -> String -> X86.Operand -> State (Sstate v p,VCS) v
--sread_operand ctxt msg op | trace ("sgeneric_cinstr: "++ show op) False = error "trace"
sread_operand ctxt msg (Storage r)          = sread_reg ctxt r
sread_operand ctxt msg (EffectiveAddress a) = sresolve_address ctxt a
sread_operand ctxt msg (Immediate a)        = return $ simmediate ctxt a
sread_operand ctxt msg (Memory a si)        = do
  resolved_address <- sresolve_address ctxt a 
  sread_mem ctxt msg resolved_address (Nat $ fromIntegral si)

swrite_operand :: SymbolicExecutable ctxt v p => ctxt -> Bool -> X86.Operand -> v -> State (Sstate v p,VCS) ()
swrite_operand ctxt use_existing_value (Storage r)   v = soverwrite_reg ctxt use_existing_value r v
swrite_operand ctxt use_existing_value (Memory a si) v = do
  resolved_address <- sresolve_address ctxt a 
  swrite_mem ctxt use_existing_value resolved_address (Nat $ fromIntegral si) v


-- v is bogus, but needed for getting the type checker to accept this. Don't know why. 
swrite_flags :: SymbolicExecutable ctxt v p => ctxt -> v -> X86.Instruction -> State (Sstate v p,VCS) ()
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



sreturn :: SymbolicExecutable ctxt v p => ctxt -> State (Sstate v p,VCS) ()
sreturn ctxt = do
  e0 <- sread_operand ctxt "return" (Memory (AddressStorage Reg.RSP) 8)
  let address = AddressPlus (AddressStorage Reg.RSP) (AddressImm 8)
  e1 <- sresolve_address ctxt address
  swrite_reg ctxt Reg.RSP e1
  swrite_operand ctxt True (Storage Reg.RIP) e0


-- LEA
slea :: SymbolicExecutable ctxt v p => ctxt -> AddressWord64 -> X86.Operand -> X86.Operand -> State (Sstate v p,VCS) ()
slea ctxt (AddressWord64 i_a) dst (EffectiveAddress a) = do
  e <- sresolve_address ctxt a
  swrite_operand ctxt True dst e
  case stry_immediate ctxt e of
    Nothing  -> return ()
    Just imm -> if saddress_has_instruction ctxt imm then modify $ add_function_pointer i_a $ fromIntegral imm else return ()

-- MOV
smov :: SymbolicExecutable ctxt v p => ctxt -> a -> X86.Instruction -> State (Sstate v p,VCS) ()
smov ctxt a i@(Instruction label prefix X86.MOV (Just dst) [src@(Immediate imm)] annot)
  | saddress_has_instruction ctxt imm   = slea ctxt label dst (EffectiveAddress (AddressImm imm))
  | otherwise                           = sgeneric_cinstr ctxt i
smov ctxt a i                           = sgeneric_cinstr ctxt i


sgeneric_cinstr :: SymbolicExecutable ctxt v p => ctxt -> X86.Instruction -> State (Sstate v p,VCS) ()
--sgeneric_cinstr ctxt i | trace ("sgeneric_cinstr: "++ show i) False = error "trace"
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

sexec_cinstr :: SymbolicExecutable ctxt v p => ctxt -> X86.Instruction -> State (Sstate v p,VCS) ()
--sexec_cinstr ctxt i | trace ("sexec_cinstr: "++ show i) False = error "trace"
sexec_cinstr ctxt i@(Instruction label prefix mnemonic (Just dst) srcs annot)
  | mnemonic == X86.LEA                      = slea ctxt label dst $ head srcs
  | mnemonic == X86.MOV                      = smov ctxt (top ctxt "") i
  | mnemonic `elem` xors && dst == head srcs = swrite_operand ctxt False dst $ simmediate ctxt 0
  | otherwise                                = sgeneric_cinstr ctxt i
 where
  xors = [X86.XOR,X86.PXOR]
sexec_cinstr ctxt i@(Instruction label prefix mnemonic Nothing _ _)
  | X86.isRet mnemonic        = sreturn ctxt
  | X86.isJump mnemonic       = sjump ctxt i
  | X86.isCall mnemonic       = scall ctxt False i
  | otherwise                 = return ()






sset_rip :: SymbolicExecutable ctxt v p => ctxt -> X86.Instruction -> State (Sstate v p,VCS) ()
sset_rip ctxt i = swrite_reg ctxt Reg.RIP (simmediate ctxt $ addressof i + (fromIntegral $ sizeof i))

sexec_instr :: SymbolicExecutable ctxt v p => ctxt -> X86.Instruction -> State (Sstate v p,VCS) ()
--sexec_instr ctxt i | trace ("sexec_isntr: "++ show i) False = error "trace"
sexec_instr ctxt i = do
  (p,_) <- get
  sset_rip ctxt i
  mapM_ (sexec_cinstr ctxt) $ X86.canonicalize i
  (p,_) <- get
  swrite_flags ctxt (top ctxt "") i -- $ trace("sexec_isntr: "++ show i++"\n"++show p) i




--sexec_block :: SymbolicExecutable ctxt v p => ctxt -> [X86.Instruction] -> Maybe [X86.Instruction] -> Sstate v p -> (Sstate v p,VCS)
sexec_block ctxt []    _    s = (s,S.empty)
sexec_block ctxt block next s = 
  let  m = swrite_reg ctxt Reg.RIP (simmediate ctxt $ addressof (head block)) >> mapM (sexec_instr ctxt) block >> (set_flagstatus next) in
    execState m (s,S.empty)
 where
  --set_flagstatus :: Maybe [X86.Instruction] -> State (Sstate v p,VCS) ()
  set_flagstatus Nothing       = return ()
  set_flagstatus (Just (i':_)) = do
    swrite_reg ctxt Reg.RIP (simmediate ctxt $ addressof i')
    modify $ \(s,vcs) -> (s { sflags = add_jump_to_pred (last block) i' (sflags s) }, vcs)



sjoin_mem :: SymbolicExecutable ctxt v p => ctxt -> String -> Sstate v p -> Sstate v p -> M.Map (p, RegionSize) v
sjoin_mem ctxt msg s0 s1 =
  let m0     = M.assocs $ smem s0
      m1     = M.assocs $ smem s1
      shared = M.intersectionWith join_values (M.fromList m0) (M.fromList m1)
      diff   = filter (\(r,_) -> r `M.notMember` shared) (m0 ++ m1) in
    sjoin_mem' shared diff
 where
  sjoin_mem' m [] = m 
  sjoin_mem' m (((a,si),v):regions)
    | otherwise =
      let v0 = evalSstate (sread_mem_from_ptr ctxt (msg ++ "join0" ++ show (a,si,s0,s1)) a si) s0
          v1 = evalSstate (sread_mem_from_ptr ctxt (msg ++ "join0" ++ show (a,si,s0,s1)) a si) s1
          v' = join_values v0 v1
          m' = smem $ execSstate (swrite_mem_to_ptr ctxt True a si v') $ Sstate {sregs = M.empty, smem = m, sflags = None} in
        sjoin_mem' m' regions

  join_values a b = sjoin_values ctxt (msg ++ "States (mem value) " ++ show a ++ " joined with " ++ show b) [a,b]




--sjoin_states ctxt msg s0 s1 | trace ("sjoin_states") False = error "trace"
sjoin_states ctxt msg s0@(Sstate regs0 mem0 flg0) s1@(Sstate regs1 mem1 flg1) =
  let flg  = join_flags flg0 flg1
      regs = M.unionWith join_reg regs0 regs1
      mem  = sjoin_mem ctxt msg s0 s1
      s'   = Sstate regs mem flg in
     s'
 where
  join_reg a b 
    | a == b    = a
    | otherwise = sjoin_values ctxt ("States (regs)" ++ show s0 ++ "\n\n\n" ++ show s1 ++ "\n" ++ show(a,b)) [a,b]
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
supremum :: SymbolicExecutable ctxt v p => ctxt -> [Sstate v p] -> Sstate v p
supremum ctxt [] = error $ "Cannot compute supremum of []"
supremum ctxt ss = foldr1 (sjoin_states ctxt "supremum") ss

simplies ctxt s0 s1 = (set_rip $ sjoin_states ctxt "simplies" s0 s1) == (set_rip s0)
 where
  set_rip = execSstate (swrite_rreg ctxt Reg.RIP (simmediate ctxt 0))




