{-# LANGUAGE DeriveGeneric, FlexibleContexts  #-}

module WithAbstractSymbolicValues.Sstate where

import Base


import WithAbstractSymbolicValues.Class
import WithAbstractSymbolicValues.GMem

import Data.Size
import Data.X86.Register
import Data.X86.Instruction
import Data.VerificationCondition
import Data.SymbolicExpression (FlagStatus(..)) -- TODO


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Set.NonEmpty as NES
import Data.List

import Control.Monad.State.Strict hiding (join)

import GHC.Generics (Generic)
import Control.DeepSeq
import qualified Data.Serialize as Cereal






execSstate :: State (Sstate v p, VCS v) b -> Sstate v p -> Sstate v p
execSstate m s = fst $ execState m (s,S.empty)

evalSstate :: State (Sstate v p, VCS v) a -> Sstate v p -> a
evalSstate m s = evalState m (s,S.empty)



-- | Read from a register
sread_rreg ctxt r (s,_) = 
  case M.lookup r (sregs s) of
    Nothing -> smk_init_reg_value ctxt r
    Just v  -> v



sread_reg :: WithAbstractSymbolicValues ctxt v p => ctxt -> Register -> State (Sstate v p,VCS v) v
sread_reg ctxt   (Reg8  r HalfL) = (ssemantics ctxt "read_reg 8" . SO_Bit 8) <$> (gets $ sread_rreg ctxt $ Reg64 r)
sread_reg ctxt   (Reg8  r HalfH) = return $ top ctxt "read_reg halfR"
sread_reg ctxt   (Reg16 r)       = (ssemantics ctxt "read_reg 16" . SO_Bit 16) <$> (gets $ sread_rreg ctxt $ Reg64 r)
sread_reg ctxt   (Reg32 r)       = (ssemantics ctxt "read_reg 32" . SO_Bit 32) <$> (gets $ sread_rreg ctxt $ Reg64 r)
sread_reg ctxt r@(Reg64 _)       = gets $ sread_rreg ctxt r
sread_reg ctxt r@(Reg128 _)      = gets $ sread_rreg ctxt r
sread_reg ctxt r@(RegSeg _)      = gets $ sread_rreg ctxt r
sread_reg ctxt r@(RegTemp)       = gets $ sread_rreg ctxt r
sread_reg ctxt r@(RegFPU _)      = return $ top ctxt "read_reg ST(_)"
sread_reg ctxt r                 = error $ "READING REG " ++ show r






-- | Write to a register
swrite_rreg :: WithAbstractSymbolicValues ctxt v p => ctxt -> String -> Register -> v -> State (Sstate v p,VCS v) () 
swrite_rreg ctxt i (RegFPU _) v = return ()
swrite_rreg ctxt i r          v = do
  (s,vcs) <- get
  -- let tr = if (r == Reg64 RSP || r == Reg64 RBP) && v == top ctxt "" then traceShow (show r ++ "WRITE", v, i, s) else id
  put $ (s {sregs = M.insert r v (sregs s), sflags = clean_flg (SSP_Reg r) (sflags s) }, vcs)


soverwrite_reg :: WithAbstractSymbolicValues ctxt v p => ctxt -> String -> Bool -> Register -> v -> State (Sstate v p,VCS v) ()
soverwrite_reg ctxt i use_existing_value r@(Reg8 _ HalfL) v = do
  let rr = real_reg r
  curr_v <- gets $ sread_rreg ctxt rr
  let v' = if use_existing_value then SO_Overwrite 8 curr_v (ssemantics ctxt "overreg 8" $ SO_Bit 8 v) else SO_Bit 8 v
  swrite_rreg ctxt i rr $ ssemantics ctxt "overreg' 8" v'
soverwrite_reg ctxt i use_existing_value r@(Reg8 _ HalfH) v = swrite_rreg ctxt i (real_reg r) (top ctxt "RegHalfH")
soverwrite_reg ctxt i use_existing_value r@(Reg16 _)      v = do
  let rr = real_reg r
  curr_v <- gets $ sread_rreg ctxt rr
  let v' = if use_existing_value then SO_Overwrite 16 curr_v (ssemantics ctxt "overreg 16" $ SO_Bit 16 v) else SO_Bit 16 v
  swrite_rreg ctxt i rr $ ssemantics ctxt "overreg' 16" v'
soverwrite_reg ctxt i use_existing_value r@(Reg32 _)      v = swrite_rreg ctxt i (real_reg r) (ssemantics ctxt "overreg 32" $ SO_Bit 32 v)
soverwrite_reg ctxt i use_existing_value r@(Reg64 _)      v = swrite_rreg ctxt i r v
soverwrite_reg ctxt i use_existing_value r@(Reg128 _)     v = swrite_rreg ctxt i r v
soverwrite_reg ctxt i use_existing_value r@(RegSeg _)     v = swrite_rreg ctxt i r v
soverwrite_reg ctxt i use_existing_value r@(RegTemp)      v = swrite_rreg ctxt i r v
soverwrite_reg ctxt i use_existing_value r@(RegFPU _)     v = return ()
soverwrite_reg ctxt i use_existing_value r                v = error $ show r


swrite_reg :: WithAbstractSymbolicValues ctxt v p => ctxt -> String -> Register -> v -> State (Sstate v p,VCS v) ()
swrite_reg ctxt i = soverwrite_reg ctxt i True


-- | Read from memory
sread_mem :: WithAbstractSymbolicValues ctxt v p => ctxt -> String -> v -> Maybe ByteSize -> State (Sstate v p,VCS v) v
--sread_mem ctxt msg a si | trace ("sread_mem: "++ show (a,si)) False = error "trace"
sread_mem ctxt msg a si = do
  (p,_) <- get
  let ptrs = smk_mem_addresses ctxt ("sread_mem" ++ show (msg,a,si,p)) False a
  if S.null ptrs then
    return $ top ctxt $ "read_mem from top"
  else do
    vs <- mapM (\ptr -> sread_mem_from_ptr ctxt msg ptr si) $ S.toList ptrs
    return $ sjoin_values ctxt (msg ++ "\nRead join:" ++ show (ptrs,vs)) vs

sread_mem_from_ptr :: WithAbstractSymbolicValues ctxt v p => ctxt -> String -> p -> Maybe ByteSize -> State (Sstate v p,VCS v) v
--sread_mem_from_ptr ctxt msg a si | trace ("sread_mem_from_ptr: "++ show (a,si)) False = error "trace"
sread_mem_from_ptr ctxt msg p@a si = return (sread_from_ro_data ctxt a si) `orTryM` try_global a  `orElseM` sread_mem' a -- TODO this order does not structure ro data section
 where
  try_global a =
    case stry_global ctxt p of
      Nothing -> return Nothing
      Just (a,isPrecise) -> use_global_mem a isPrecise
  use_global_mem a isPrecise = do
    (s,vcs) <- get
    let (v',gmem') = runState (read_global_mem_access ctxt p a si isPrecise) (gmem s)
    put $ (s { gmem = gmem' },vcs)
    return $ Just v'


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
    | M.size overlap == 0 = smk_init_mem_value ctxt ("\nmaking init mem value:\n" ++ show s ++ "\n" ++ show (a,si) ++ "_" ++ msg) a si
    | otherwise = -- error ("Overlapping read:\nReading from: " ++ show (a,si,S.map fst overlap) ++ "\nIn state:\n" ++ show s) 
                  swiden_values ctxt (msg' "widen" a s overlap) $ sjoin_values ctxt (msg' "joining" a s overlap) (M.elems overlap)

  msg' name a s overlap = "Read " ++ name ++ ": " ++ show (a,si) ++ "\n" ++ show s ++ "\n" ++ show overlap ++ "\n"++msg



-- | Write to memory
swrite_mem :: WithAbstractSymbolicValues ctxt v p => ctxt -> Bool -> v -> Maybe ByteSize -> v -> State (Sstate v p,VCS v) ()
swrite_mem ctxt use_existing_value a si v = do
  (p,_) <- get
  let ptrs = smk_mem_addresses ctxt ("swrite_mem\n"++show p) False a
  if S.null ptrs then
    --error ("TOP: writing to " ++ show (a,si) ++ "\nIn state:\n" ++ show p)
    --trace ("TOP: writing to " ++ show (a,si) ++ "\nIn state:\n" ++ show p) $
    return () -- TODO ADD VC
  else
    mapM_ (\ptr -> swrite_mem_to_ptr ctxt use_existing_value ptr si v) ptrs

swrite_mem_to_ptr :: WithAbstractSymbolicValues ctxt v p => ctxt -> Bool -> p -> Maybe ByteSize -> v -> State (Sstate v p,VCS v) ()
swrite_mem_to_ptr ctxt use_existing_value p@a si v = do
  modify $ \(s,vcs) -> (s { sflags = clean_flg (SSP_Mem (simmediate ctxt 0) 0) (sflags s) }, vcs)

  case stry_global ctxt p of
    Nothing -> write' p
    Just (a,isPrecise) -> use_global_mem a isPrecise
 where
  use_global_mem a isPrecise = do
    (s,vcs) <- get
    let (v',gmem') = runState (write_global_mem_access ctxt a si isPrecise v) (gmem s)
    put $ (s { gmem = gmem' },vcs)


  write' p = do
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
      --trace ("Overlapping write:\nWriting to: " ++ show (a,si,map fst overlap) ++ "\nm = " ++ show m' ++ "\nIn state:\n" ++ show s) $
      put $ (s { smem = combine [ m',  separate ] }, vcs)

  combine = M.fromList . concat

  do_partitioning m a = M.foldrWithKey (do_partition a) ([],[],[],[],[]) m
  do_partition a (a0,si0) v0 (equal,enclosing,encloses,separate,overlap) 
    | ssensitive ctxt          a0 si0 v0   = (equal,enclosing,encloses,((a0,si0),v0):separate,overlap) -- TODO: VCS v, especially when local pointer
    | salias     ctxt          a0 si0 a si = (((a0,si0),v0):equal,enclosing,encloses,separate,overlap) 
    | senclosed  ctxt          a si a0 si0 = (equal,((a0,si0),v0):enclosing,encloses,separate,overlap) 
    | senclosed  ctxt          a0 si0 a si = (equal,enclosing,((a0,si0),v0):encloses,separate,overlap)
    | sseparate  ctxt "wwrite" a si a0 si0 = (equal,enclosing,encloses,((a0,si0),v0):separate,overlap) 
    | not use_existing_value               = (equal,enclosing,encloses,((a0,si0),v0):separate,overlap)
    | otherwise                            = (equal,enclosing,encloses,separate,((a0,si0),v0):overlap) 

  merge ::  WithAbstractSymbolicValues ctxt v p => ctxt -> [((p, Maybe ByteSize), v)] -> [((p, Maybe ByteSize), v)]
  merge ctxt m =
    let ptrs = sjoin_pointers ctxt $ map (fst . fst) m
        v'   = swiden_values ctxt "write_mem (overlap)" $ sjoin_values ctxt "MemWrite (values)" $ map snd m in
      map (\ptr -> ((ptr,unknownSize),v')) ptrs



-- v is bogus, but needed for getting the type checker to accept this. Don't know why. 
swrite_flags :: WithAbstractSymbolicValues ctxt v p => ctxt -> v -> Instruction -> State (Sstate v p,VCS v) ()
swrite_flags ctxt v i = do
  (s,vcs) <- get
  put $ (s {sflags = sflg_semantics ctxt v i (sflags s)}, vcs)


-- If the given StatePart is overwritten, does that taint the current flag status?
-- If so, set flagstatus to None, otherwise keep it.
clean_flg :: SStatePart p -> FlagStatus -> FlagStatus
clean_flg sp None               = None
clean_flg sp (FS_CMP b op1 op2) = do
  if is_tainted op1 || is_tainted op2 then
    None
  else
    FS_CMP b op1 op2
 where
  is_tainted (Op_Reg r)             = 
    case sp of
      SSP_Reg r' -> real_reg r' == real_reg r
      _          -> False
      
  is_tainted (Op_Imm _)             = False
  is_tainted (Op_Mem _ _ _ _ _ _ _) =
    case sp of
      SSP_Mem _ _ -> True
      _          -> False





write_sp :: WithAbstractSymbolicValues ctxt v p => ctxt -> SStatePart p -> v -> State (Sstate v p, VCS v) ()
write_sp ctxt (SSP_Reg r)    v = swrite_reg ctxt "write_sp" r v
write_sp ctxt (SSP_Mem a si) v = swrite_mem_to_ptr ctxt True a (Just $ ByteSize si) v

read_sp :: WithAbstractSymbolicValues ctxt v p => ctxt -> SStatePart p -> State (Sstate v p, VCS v) v
read_sp ctxt (SSP_Reg r)    = sread_reg ctxt r
read_sp ctxt (SSP_Mem a si) = sread_mem_from_ptr ctxt "read_sp" a $ (Just $ ByteSize si)


