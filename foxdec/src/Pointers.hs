{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, DeriveGeneric, DefaultSignatures, FlexibleContexts, Strict #-}

{-|
Module      : Pointers
Description : Functions for dealing with symbolic pointers and abstraction.
-}
module Pointers (
   get_pointer_bases,
   get_known_pointer_bases,
   expr_highly_likely_pointer,
   expr_is_global_pointer,
   expr_is_highly_likely_local_pointer,
   expr_is_possibly_local_pointer,
   expr_is_global_immediate,
   srcs_of_expr,
   srcs_of_exprs,
   pointers_from_different_global_section,
   join_exprs,
   join_single,
   pointer_bases_separate,
   sources_separate,
   necessarily_equal,
   necessarily_equal_stateparts,
   necessarily_separate,
   necessarily_separate_stateparts,
   necessarily_enclosed,
  )
  where

import Base
import SimplePred
import Context
import X86_Datastructures
import ControlFlow
import Config

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import Data.Word 
import Debug.Trace


-- * Pointer Bases

-- | A 'PointerBase' is a positive addend of a symbolic expression that may represent a pointer.
-- Retrieves the pointer bases from a symbolic expression.
-- They are either 1.) all known, or 2.) all unknown.
get_pointer_bases :: Context -> SimpleExpr -> S.Set PointerBase
get_pointer_bases ctxt e =
  let bs    = get_pointer_bases' ctxt e 
      known = S.filter (not . is_unknown_ptr_base) bs in
    if not $ S.null known then
      known
    else
      bs
 where
  get_pointer_bases' :: Context -> SimpleExpr -> S.Set PointerBase
  get_pointer_bases' ctxt (Bottom (FromNonDeterminism es)) = S.unions $ S.map (get_pointer_bases ctxt) es
  get_pointer_bases' ctxt (SE_Op (Plus _) es)              = S.unions $ S.map (get_pointer_bases ctxt) $ S.fromList es
  get_pointer_bases' ctxt (SE_Op (Minus _) (e:es))         = get_pointer_bases ctxt e
  get_pointer_bases' ctxt e                                = get_pointer_base e


  get_pointer_base e@(SE_Immediate a)                      = if expr_is_global_immediate ctxt e then S.singleton $ GlobalAddress a else S.singleton $ Unknown e
  get_pointer_base e@(SE_Var (SP_Mem (SE_Immediate a) 8))  = case IM.lookup (fromIntegral a) (ctxt_syms ctxt) of
                                                               Nothing  -> S.singleton $ Unknown e
                                                               Just sym -> S.singleton $ PointerToSymbol a sym
  get_pointer_base   (SE_Var (SP_Reg RSP))                 = S.singleton $ StackPointer
  get_pointer_base   (SE_Malloc id hash)                   = S.singleton $ Malloc id hash
  get_pointer_base  e@(Bottom (FromPointerBases bs))       = if S.null bs then S.singleton $ Unknown e else bs
  get_pointer_base  e                                      = S.singleton $ Unknown e



-- | Returns the set of known pointerbases, or the empty set if none.
get_known_pointer_bases ctxt e = S.filter (not . is_unknown_ptr_base) $ get_pointer_bases ctxt e

-- | Returns true if the expression has known pointerbases.
expr_highly_likely_pointer ctxt e = not $ S.null $ get_known_pointer_bases ctxt e

-- | Returns the set of global pointerbases, or the empty set if none.
get_global_pointer_bases ctxt e = S.filter is_global_ptr_base $ get_pointer_bases ctxt e

-- | Returns true if the expression has a global pointerbase.
expr_is_global_pointer ctxt e = not $ S.null $ get_global_pointer_bases ctxt e

-- | Returns true iff the expression is an immediate address falling into the range of sections of the binary
expr_is_global_immediate ctxt (SE_Immediate a) = address_has_symbol ctxt a || find_section_for_address ctxt (fromIntegral a) /= Nothing
expr_is_global_immediate ctxt _                = False

-- | Returns true if the expression has a local pointerbase, and no others.
expr_is_highly_likely_local_pointer ctxt e = get_known_pointer_bases ctxt e == S.singleton StackPointer || srcs_of_expr ctxt e == (S.singleton $ Src_Var $ SP_Reg $ RSP)

-- | Returns true if the expression has a local pointerbase, but maybe other pointerbases as well.
expr_is_possibly_local_pointer ctxt e = StackPointer `S.member` get_known_pointer_bases ctxt e || (Src_Var $ SP_Reg $ RSP) `S.member` (srcs_of_expr ctxt e)



is_unknown_ptr_base (Unknown _) = True
is_unknown_ptr_base _           = False

is_global_ptr_base (GlobalAddress _) = True
is_global_ptr_base _                 = False

is_malloc (Malloc _ _) = True
is_malloc _            = False






-- * Sources



-- | Returns the set of sources (inputs used to compute the expression) of an expression.
srcs_of_expr ctxt (Bottom typ)         = srcs_of_bottyp ctxt typ
srcs_of_expr ctxt (SE_Malloc id h)     = S.singleton $ Src_Malloc id h
srcs_of_expr ctxt (SE_Var sp)          = S.singleton $ Src_Var sp
srcs_of_expr ctxt e@(SE_Immediate i)   = if expr_is_global_immediate ctxt e then S.singleton $ Src_Var $ SP_Mem e 8 else S.empty -- TODO S.empty
srcs_of_expr ctxt (SE_StatePart sp)    = S.singleton $ Src_Var sp
srcs_of_expr ctxt (SE_Op _ es)         = srcs_of_exprs ctxt es
srcs_of_expr ctxt (SE_Bit i e)         = srcs_of_expr ctxt e
srcs_of_expr ctxt (SE_SExtend _ _ e)   = srcs_of_expr ctxt e
srcs_of_expr ctxt (SE_Overwrite _ a b) = srcs_of_exprs ctxt [a,b]

-- | Returns the set of sources (state parts used to compute the expression) of two expressions.
srcs_of_exprs ctxt es = S.unions $ map (srcs_of_expr ctxt) es 

-- | Returns the set of sources of the bottom type
srcs_of_bottyp ctxt (FromNonDeterminism es)        = S.unions $ S.map (srcs_of_expr ctxt) es
srcs_of_bottyp ctxt (FromPointerBases bs)          = S.unions $ S.map (srcs_of_base ctxt) bs
srcs_of_bottyp ctxt (FromSources srcs)             = srcs
srcs_of_bottyp ctxt (FromBitMode srcs)             = srcs
srcs_of_bottyp ctxt (FromOverlap srcs)             = srcs
srcs_of_bottyp ctxt (FromSemantics srcs)           = srcs
srcs_of_bottyp ctxt (FromMemWrite srcs)            = srcs
srcs_of_bottyp ctxt (FromUninitializedMemory srcs) = srcs
srcs_of_bottyp ctxt (FromCall f)                   = S.singleton $ Src_Function f

-- | Returns the set of sources of the pointerbase
srcs_of_base ctxt StackPointer            = S.singleton $ Src_Var $ SP_Reg RSP
srcs_of_base ctxt (Unknown e)             = srcs_of_expr ctxt e
srcs_of_base ctxt (Malloc id h)           = S.singleton $ Src_Malloc id h
srcs_of_base ctxt (GlobalAddress a)       = S.singleton $ Src_Var $ SP_Mem (SE_Immediate a) 8
srcs_of_base ctxt (PointerToSymbol a sym) = S.singleton $ Src_Var $ SP_Mem (SE_Immediate a) 8






-- * Joining


-- | Given a set of expressions, produce an expression that resembles the join of the entire set.
-- That is, the produced expression should be coarser than the disjunction of all input-expressions.
--
--   (1) First, just use non-determinism, i.e., @a join b@ becomes @{a,b}@. This is precise, but doesn't guarantee termination.
--   (2) If step 1 produces too many cases, join based on known pointerbases. This requires all given expressions to have known pointerbases.
--   (3) If step 2 produces too many bases, or the given expressions have no known pointerbases, join bsed on sources.
--   (4) If step 3 produces too many sources, just produces 'rock_bottom'.
--
--
-- TODO: joining immediates
join_exprs' :: String -> Context -> [SimpleExpr] -> SimpleExpr
join_exprs' msg ctxt es = 
  let es' = S.unions $ map (S.fromList . unfold_non_determinism) es in
    if S.size es' == 0 then
      rock_bottom
    else if S.size es' == 1 then
      head $ S.toList es'  
    else if S.size es' <= max_num_of_cases && all (not . contains_bot) es' then
      Bottom (FromNonDeterminism es')
    else if rock_bottom `S.member` es' then
      rock_bottom
    else let bss = S.map (get_known_pointer_bases ctxt) es'
             bs  = S.unions bss in
      if S.size bs <= max_num_of_bases && all (not . S.null) bss then
        Bottom (FromPointerBases bs)
      else
        let srcs = S.unions $ S.map (srcs_of_expr ctxt) es' in
          if S.size srcs <= max_num_of_sources then
            Bottom (FromSources srcs)
          else
            trace ("Hitting max num of sources: " ++ show max_num_of_sources) rock_bottom
 where
  rsp = Src_Var $ SP_Reg RSP


join_exprs msg ctxt es = 
  let e = join_exprs' msg ctxt es in
    e
--    if expr_is_possibly_local_pointer ctxt e && any (not . expr_is_possibly_local_pointer ctxt) es then traceShow ("joining: ",msg,es,e) e else e
--    if S.size (srcs_of_expr e) == 0 && all (\e -> S.size (srcs_of_expr e) > 0) es && es /= [] then traceShow ("joining: ",es,e, S.size $ srcs_of_exprs es) e else e 


-- | Abstraction for a single expression, even if the expression is concrete.
join_single :: Context -> SimpleExpr -> SimpleExpr
join_single ctxt e =
  let bs = get_known_pointer_bases ctxt e in
    if not (S.null bs) && S.size bs <= max_num_of_bases then
      Bottom (FromPointerBases bs)
    else
      let srcs = srcs_of_expr ctxt e in 
        if S.size srcs <= max_num_of_sources then
          Bottom (FromSources srcs)
        else
          trace ("Hitting max num of sources: " ++ show max_num_of_sources) rock_bottom



-- * Separation


-- | Returns true iff the two given expressions have global pointerbases in different segments/sections of the binary.
-- We do not assume that such pointers are separate, but do assert it.
pointers_from_different_global_section ctxt a0 a1 =
  case (S.toList $ get_global_pointer_bases ctxt a0, S.toList $ get_global_pointer_bases ctxt a1) of
    ([GlobalAddress g0], [GlobalAddress g1]) -> find_section_for_address ctxt (fromIntegral g0) /= find_section_for_address ctxt (fromIntegral g1)
    _ -> False


-- | Two pointerbases are separate if they refer to completely different parts of the memory.
-- We assume Stackframe, Global address space, and Heap are separate.
-- Two different @malloc@'s point to different regions.
pointer_bases_separate StackPointer             StackPointer              = False
pointer_bases_separate StackPointer             (GlobalAddress _)         = True
pointer_bases_separate StackPointer             (PointerToSymbol _ _)     = True
pointer_bases_separate StackPointer             (Malloc _ _)              = True

pointer_bases_separate (GlobalAddress _)        StackPointer              = True
pointer_bases_separate (GlobalAddress _)        (PointerToSymbol _ _)     = True
pointer_bases_separate (GlobalAddress _)        (GlobalAddress _)         = False
pointer_bases_separate (GlobalAddress _)        (Malloc _ _)              = True

pointer_bases_separate (PointerToSymbol _ _)    StackPointer              = True
pointer_bases_separate (PointerToSymbol _ _)    (GlobalAddress _)         = True
pointer_bases_separate (PointerToSymbol _ sym0) (PointerToSymbol _ sym1)  = sym0 /= sym1
pointer_bases_separate (PointerToSymbol _ sym0) (Malloc _ _)              = True

pointer_bases_separate (Malloc id0 hash0)       (Malloc id1 hash1)        = Nothing `notElem` [id0,id1] && Nothing `notElem` [hash0,hash1] && (id0,hash0) /= (id1,hash1)
pointer_bases_separate (Malloc _ _)             _                         = True



-- | Two sources are inputs for separate pointers if, e.g., one of them is the stackpointer and the other a malloc-return-value.
sources_separate :: Context -> FInit -> BotSrc -> BotSrc -> Bool
sources_separate ctxt finit src0 src1 = sources_separate' True (src0,src1)
 where
  sources_separate' use_finit (src0,src1) = 
   case (src_to_base src0, src_to_base src1) of
     (Just b0,Just b1)      -> pointer_bases_separate b0 b1
     (Just StackPointer, _) -> True
     (_, Just StackPointer) -> True
     (Just (Malloc _ _), _) -> True
     (_, Just (Malloc _ _)) -> True
     _                      -> if use_finit then sources_separate_by_finit finit src0 src1 else False

  src_to_base (Src_Var (SP_Reg RSP))   = Just $ StackPointer
  src_to_base (Src_Malloc i h)         = Just $ Malloc i h
  src_to_base _                        = Nothing

  sources_separate_by_finit finit (Src_Function _) _          = False 
  sources_separate_by_finit finit _ (Src_Function _)          = False
  sources_separate_by_finit finit (Src_Var sp0) (Src_Var sp1) =
    case (M.lookup sp0 finit, M.lookup sp0 finit) of
      (Just e0,Just e1) -> all (sources_separate' False) [(s0,s1) | s0 <- S.toList (srcs_of_expr ctxt e0), s1 <- S.toList (srcs_of_expr ctxt e1)]
      _                 -> False


-- | Returns true iff the two given expressions can be shown to be separate.
-- This means that either:
--   * They both have known pointerbases that are assumed to be separate (see 'pointer_bases_separate').
--   * They both have sources that are all separate (see `sources_separate`)
--   * One of them is an immediate and the other is local.

separate_pointer_domains ctxt finit a0 a1 =
  let bs0 = get_known_pointer_bases ctxt a0
      bs1 = get_known_pointer_bases ctxt a1 in
    if not (S.null bs0) && not (S.null bs1) then
      all (uncurry pointer_bases_separate) [(b0,b1) | b0 <- S.toList bs0, b1 <- S.toList bs1]
    else let srcs0 = srcs_of_expr ctxt a0
             srcs1 = srcs_of_expr ctxt a1 in
      or [
        not (S.null srcs0) && not (S.null srcs1) && all (uncurry $ sources_separate ctxt finit) [(s0,s1) | s0 <- S.toList srcs0, s1 <- S.toList srcs1],
        is_immediate a0 && expr_is_possibly_local_pointer ctxt a1, -- TODO test without
        is_immediate a1 && expr_is_possibly_local_pointer ctxt a0
       ]
      



 

-- | Returns true iff the given symbolic region is necessarily enclosed in the other.
-- For example:
--    @[RSP-16,4]@ is enclosed in @[RSP-18,8]@
--    @[RSP+4,4]@ is enclosed in @[RSP,8]@
--
-- Will return @False@ if the expressions contain bottom.
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


-- | Returns true iff the given symbolic regions are ncessarily separate.
-- For example:
--    @[RSP-16,4]@ is separate from @[RSP-12,8]@
--    @[RSP+8,4]@ is separate from @[RSP,8]@
--
-- If none of the cases apply where it can be shjown arithmetically that the expressions are separate,
-- we check whether the expressions can be proven separate based on their domains (see 'separate_pointer_domains').
necessarily_separate ctxt finit a0 si0 a1 si1 =
  if not (contains_bot a0) && not (contains_bot a1) then
    (a0,si0) /= (a1,si1) && sep a0 a1
  else
    separate_pointer_domains ctxt finit a0 a1
 where
  -- two immediate addresses
  sep (SE_Immediate a0)
      (SE_Immediate a1) =
    fromIntegral a0 + si0 <= fromIntegral a1 || fromIntegral a1 + si1 <= fromIntegral a0
  -- v0 - i0 |x| v0 - i1 <==> v0 - i0 + si0 <= v0 - i1 || v0 - i1 + si1 <= v0 - i0 <==> i0-si0 >= i1 || i1-si1 >= i0
  sep (SE_Op (Minus _) [v0, SE_Immediate i0])
      (SE_Op (Minus _) [v1, SE_Immediate i1]) = 
    if necessarily_equal v0 v1 then
      fromIntegral i0 - si0 >= fromIntegral i1 || fromIntegral i1 - si1 >= fromIntegral i0
    else
      separate_pointer_domains ctxt finit a0 a1


  -- v0 - i0 |x| v0 + i1 <==> True
  sep (SE_Op (Minus _) [v0, SE_Immediate i0])
      (SE_Op (Plus  _) [v1, SE_Immediate i1]) = 
    if necessarily_equal v0 v1 then
      True
    else
      separate_pointer_domains ctxt finit a0 a1
  sep (SE_Op (Plus  _) [v0, SE_Immediate i0])
      (SE_Op (Minus _) [v1, SE_Immediate i1]) = 
    if necessarily_equal v0 v1 then
      True
    else
      separate_pointer_domains ctxt finit a0 a1


  -- v0 - i0 |x| v0 <==> i0 >= si0
  sep (SE_Op (Minus _) [v0, SE_Immediate i0])
       v1 = 
    if necessarily_equal v0 v1 then
      fromIntegral i0 >= si0 
    else
      separate_pointer_domains ctxt finit a0 a1
  sep v0
      (SE_Op (Minus _) [v1, SE_Immediate i1]) = 
    if necessarily_equal v0 v1 then
      fromIntegral i1 >= si1
    else
      separate_pointer_domains ctxt finit a0 a1 

  -- v0 + i0 |x| v0 + i1 <==> v0 + i0 + si0 <= v0 + i1 || v0 + i1 + si1 <= v0 + i0 <==> i0+si0 <= i1 || i1+si1 <= i0
  sep (SE_Op (Plus _) [v0, SE_Immediate i0])
      (SE_Op (Plus _) [v1, SE_Immediate i1]) = 
    if necessarily_equal v0 v1 then
      fromIntegral i0 + si0 <= fromIntegral i1 || fromIntegral i1 + si1 <= fromIntegral i0
    else
      separate_pointer_domains ctxt finit a0 a1 


  -- v0 + i0 |x| v0 <==> i0 >= si1
  sep (SE_Op (Plus _) [v0, SE_Immediate i0])
      v1 =
    if necessarily_equal v0 v1 then
      fromIntegral i0 >= si1
    else
      separate_pointer_domains ctxt finit a0 a1 
  sep v1
      (SE_Op (Plus _) [v0,SE_Immediate i0]) =
    if necessarily_equal v0 v1 then
      fromIntegral i0 >= si1
    else
      separate_pointer_domains ctxt finit a0 a1 

  -- remainder
  sep a0 a1 = separate_pointer_domains ctxt finit a0 a1


-- | Returns true iff the given symbolic stateparts are necessarily separate.
necessarily_separate_stateparts ctxt finit (SP_Reg r0)     (SP_Reg r1)     = r0 /= r1
necessarily_separate_stateparts ctxt finit (SP_Mem a0 si0) (SP_Mem a1 si1) = necessarily_separate ctxt finit a0 si0 a1 si1
necessarily_separate_stateparts _    _     _               _               = True




-- | Returns true iff the given symbolic regions are necessarily equal.
-- For example:
--    @[RSP-16,4]@ is enclosed in @[RSP-16,4]@
--
-- Will return @False@ if the expressions contain bottom.
necessarily_equal a0 a1 = a0 == a1 && not (contains_bot a0) && not (contains_bot a1)

-- | Returns true iff the given symbolic stateparts are necessarily equal.
necessarily_equal_stateparts (SP_Reg r0) (SP_Reg r1) = r0 == r1
necessarily_equal_stateparts (SP_Mem a0 si0) (SP_Mem a1 si1) = si0 == si1 && necessarily_equal a0 a1
necessarily_equal_stateparts _ _ = False






