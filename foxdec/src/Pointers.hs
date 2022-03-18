{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, DeriveGeneric, DefaultSignatures, FlexibleContexts, Strict #-}

{-|
Module      : Pointers
Description : Functions for dealing with symbolic pointers and abstraction.
-}
module Pointers (
   FContext (..),
   mk_fcontext,
   get_pointer_domain,
   expr_highly_likely_pointer,
   expr_is_global_pointer,
   expr_is_highly_likely_local_pointer,
   expr_is_global_immediate,
   srcs_of_expr,
   srcs_of_exprs,
   is_known_source,
   join_exprs,
   join_single,
   separate_pointer_domains,
   pointer_bases_separate_necessarily,
   pointer_bases_separate_possibly,
   sources_separate_necessarily,
   sources_separate_possibly,
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
import Data.Maybe
import Debug.Trace


-- | The context augmented with information on the current function
data FContext = FContext {
  f_ctxt  :: Context, -- ^ The context
  f_entry :: Int,     -- ^ The entry address of the current function
  f_name  :: String,  -- ^ The name of the current function
  f_init  :: FInit    -- ^ The initialization of the current function
 }

mk_fcontext :: Context -> Int -> FContext 
mk_fcontext ctxt entry =
  let f     = function_name_of_entry ctxt entry
      finit = IM.lookup entry $ ctxt_finits ctxt
      fctxt = FContext ctxt entry f (finit `orElse` M.empty) in
    fctxt



statepart_to_finit_expr :: FContext -> StatePart -> Maybe SimpleExpr
statepart_to_finit_expr ctxt sp = M.lookup sp $ f_init ctxt

 
-- * Pointer Domains
--
-- Turn a symbolic expression into a pointer domain: either a pointer base, or a set of sources.
--
-- * A 'PointerBase' is a positive addend of a symbolic expression that likely represents a pointer. It can be a stack pointer, an immediate pointer into a section, or a pointer to a known symbol.
-- * A source is some statepart whose initial value affects the value of the pointer.
--
-- Retrieves the pointer bases from a symbolic expression.
-- They are either 1.) all known, or 2.) all unknown.
get_pointer_domain ::
  FContext             -- ^ The current context
  -> SimpleExpr        -- ^ A sybmolic expression 
  -> PointerDomain     -- ^ A pointer domain
get_pointer_domain ctxt e =
  let bs  = get_pointer_bases' True $ simp e in
    if not (S.null bs) && S.size bs <= max_num_of_bases then
      Domain_Bases bs
    else let srcs = srcs_of_expr ctxt e in
      if not (S.null srcs) && S.size srcs <= max_num_of_sources then
        Domain_Sources srcs
      else
        Domain_Sources $ S.empty

 where
  get_pointer_bases' :: Bool -> SimpleExpr -> S.Set PointerBase
  get_pointer_bases' use_finit (Bottom (FromNonDeterminism es)) = S.unions $ S.map (get_pointer_bases' use_finit) es -- TODO non-empty union?
  get_pointer_bases' use_finit (SE_Op (Plus _) es)              = S.unions $ S.map (get_pointer_bases' use_finit) $ S.fromList es
  get_pointer_bases' use_finit (SE_Op (Minus _) (e:es))         = get_pointer_bases' use_finit e
  get_pointer_bases' use_finit e                                = get_pointer_base use_finit e


  get_pointer_base :: Bool -> SimpleExpr -> S.Set PointerBase
  get_pointer_base use_finit (SE_Immediate a)                     = if expr_is_global_immediate (f_ctxt ctxt) e then S.singleton $ GlobalAddress a else S.empty
  get_pointer_base use_finit (SE_Var (SP_StackPointer f))         = S.singleton $ StackPointer f
  get_pointer_base True      (SE_Var sp)                          = (get_pointer_bases' False <$> statepart_to_finit_expr ctxt sp) `orElse` (statepart_to_pointerbase sp)
  get_pointer_base False     (SE_Var sp)                          = (statepart_to_pointerbase sp)
  get_pointer_base use_finit (SE_Malloc id hash)                  = S.singleton $ Malloc id hash
  get_pointer_base use_finit (Bottom (FromPointerBases bs))       = if S.null bs then S.empty else bs
  get_pointer_base use_finit e                                    = S.empty


  statepart_to_pointerbase :: StatePart -> S.Set PointerBase
  statepart_to_pointerbase (SP_Mem (SE_Immediate a) 8)  = case IM.lookup (fromIntegral a) (ctxt_syms $ f_ctxt ctxt) of
                                                            Nothing  -> S.empty
                                                            Just sym -> S.singleton $ PointerToSymbol a sym
  statepart_to_pointerbase _                            = S.empty


-- | returns the set of bases of a domain, if any
bases_of_domain :: PointerDomain -> Maybe (S.Set PointerBase)
bases_of_domain (Domain_Bases bs) = if not $ S.null bs then Just bs else Nothing
bases_of_domain _                 = Nothing

-- | Returns the set of pointer bases, if any
get_pointer_bases ctxt e = bases_of_domain $ get_pointer_domain ctxt e

-- | Returns true if the expression has known pointerbases.
expr_highly_likely_pointer ctxt e = get_pointer_bases ctxt e /= Nothing

-- | Returns the set of global pointerbases, or the empty set if none.
get_global_pointer_bases ctxt e = (S.filter is_global_ptr_base <$> get_pointer_bases ctxt e) `orElse` S.empty

-- | Returns true if the expression has a global pointerbase.
expr_is_global_pointer ctxt e = not $ S.null $ get_global_pointer_bases ctxt e

-- | Returns true iff the expression is an immediate address falling into the range of sections of the binary
expr_is_global_immediate ctxt (SE_Immediate a) = address_has_symbol ctxt a || find_section_for_address ctxt (fromIntegral a) /= Nothing
expr_is_global_immediate ctxt _                = False

-- | Returns true if the expression has a local pointerbase, and no others.
expr_is_highly_likely_local_pointer ctxt e = is_highly_likely_local_pointer_domain ctxt $ get_pointer_domain ctxt e

-- | Returns true iff the give domain is highly likelty local to the current function
is_highly_likely_local_pointer_domain ctxt (Domain_Bases bs)     = all is_local_base bs
 where
  is_local_base (StackPointer f)     = f == f_name ctxt
  is_local_base _                    = False
is_highly_likely_local_pointer_domain ctxt (Domain_Sources srcs) = all is_local_src srcs
 where
  is_local_src (Src_StackPointer f)  = f == f_name ctxt
  is_local_src _                     = False





is_global_ptr_base (GlobalAddress _) = True
is_global_ptr_base _                 = False

is_malloc (Malloc _ _) = True
is_malloc _            = False




-- * Sources

-- | Returns the set of sources (inputs used to compute the expression) of an expression.
srcs_of_expr ctxt e = 
  let srcs       = srcs_of_expr' ctxt True e
      known_srcs = S.filter is_known_source srcs in
   if S.null known_srcs then srcs else known_srcs

is_known_source (Src_StackPointer _)     = True
is_known_source (Src_Malloc _ _)         = True
is_known_source (Src_ImmediateAddress _) = True
is_known_source _                        = False

-- | Returns the set of sources (state parts used to compute the expression) of two expressions.
srcs_of_exprs ctxt es = S.unions $ map (srcs_of_expr' ctxt True) es 
-- | returns the set of sources of a domain, if any
sources_of_domain :: FContext -> PointerDomain -> S.Set BotSrc 
sources_of_domain ctxt (Domain_Bases bs)     = S.unions $ S.map (srcs_of_base ctxt True) bs
sources_of_domain ctxt (Domain_Sources srcs) = srcs





srcs_of_expr' ctxt use_finit (Bottom typ)                 = srcs_of_bottyp ctxt use_finit typ
srcs_of_expr' ctxt use_finit (SE_Malloc id h)             = S.singleton $ Src_Malloc id h
srcs_of_expr' ctxt use_finit (SE_Var (SP_StackPointer f)) = S.singleton $ Src_StackPointer f
srcs_of_expr' ctxt True      (SE_Var sp)                  = (srcs_of_expr' ctxt False <$> statepart_to_finit_expr ctxt sp) `orElse` (S.singleton $ Src_Var sp)
srcs_of_expr' ctxt False     (SE_Var sp)                  = S.singleton $ Src_Var sp
srcs_of_expr' ctxt use_finit e@(SE_Immediate i)           = if expr_is_global_immediate (f_ctxt ctxt) e then S.singleton $ Src_ImmediateAddress i else S.empty
srcs_of_expr' ctxt use_finit (SE_StatePart sp)            = S.empty 
srcs_of_expr' ctxt use_finit (SE_Op _ es)                 = S.unions $ map (srcs_of_expr' ctxt use_finit) es
srcs_of_expr' ctxt use_finit (SE_Bit i e)                 = srcs_of_expr' ctxt use_finit e
srcs_of_expr' ctxt use_finit (SE_SExtend _ _ e)           = srcs_of_expr' ctxt use_finit e
srcs_of_expr' ctxt use_finit (SE_Overwrite _ a b)         = S.unions $ map (srcs_of_expr' ctxt use_finit) [a,b]


-- | Returns the set of sources of the bottom type
srcs_of_bottyp ctxt use_finit (FromNonDeterminism es)        = S.unions $ S.map (srcs_of_expr' ctxt use_finit) es
srcs_of_bottyp ctxt use_finit (FromPointerBases bs)          = S.unions $ S.map (srcs_of_base ctxt use_finit) bs
srcs_of_bottyp ctxt use_finit (FromSources srcs)             = srcs
srcs_of_bottyp ctxt use_finit (FromBitMode srcs)             = srcs
srcs_of_bottyp ctxt use_finit (FromOverlap srcs)             = srcs
srcs_of_bottyp ctxt use_finit (FromSemantics srcs)           = srcs
srcs_of_bottyp ctxt use_finit (FromMemWrite srcs)            = srcs
srcs_of_bottyp ctxt use_finit (FromUninitializedMemory srcs) = srcs
srcs_of_bottyp ctxt use_finit (FromCall f)                   = S.singleton $ Src_Function f

-- | Returns the set of sources of the pointerbase
srcs_of_base ctxt use_finit (StackPointer f)        = S.singleton $ Src_StackPointer f
srcs_of_base ctxt use_finit (Malloc id h)           = S.singleton $ Src_Malloc id h
srcs_of_base ctxt use_finit (GlobalAddress a)       = S.singleton $ Src_ImmediateAddress a
srcs_of_base ctxt use_finit (PointerToSymbol a sym) = S.singleton $ Src_Var $ SP_Mem (SE_Immediate a) 8





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
join_exprs' :: String -> FContext -> [SimpleExpr] -> SimpleExpr
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
    else let bss = S.map (nothing_to_empty . get_pointer_bases ctxt) es'
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
  nothing_to_empty Nothing   = S.empty
  nothing_to_empty (Just bs) = bs

join_exprs msg ctxt es = 
  let e = join_exprs' msg ctxt es in
    e
--    if expr_is_possibly_local_pointer ctxt e && any (not . expr_is_possibly_local_pointer ctxt) es then traceShow ("joining: ",msg,es,e) e else e
--    if S.size (srcs_of_expr e) == 0 && all (\e -> S.size (srcs_of_expr e) > 0) es && es /= [] then traceShow ("joining: ",es,e, S.size $ srcs_of_exprs es) e else e 
--    if all (\e' -> S.size (srcs_of_expr ctxt e') < S.size (srcs_of_expr ctxt e)) es && es /= [] then traceShow ("joining: ",msg,es,e, S.size $ srcs_of_exprs ctxt es) e else e 


-- | Abstraction for a single expression, even if the expression is concrete.
join_single :: FContext -> SimpleExpr -> SimpleExpr
join_single ctxt e =
  case get_pointer_bases ctxt e of
    Nothing -> join_sources
    Just bs -> if not (S.null bs) && S.size bs <= max_num_of_bases then Bottom (FromPointerBases bs) else join_sources
 where
  join_sources =
    let srcs = srcs_of_expr ctxt e in 
      if S.size srcs <= max_num_of_sources then
        Bottom (FromSources srcs)
      else
        trace ("Hitting max num of sources: " ++ show max_num_of_sources) rock_bottom



-- * Separation


-- | Returns true iff the two given expressions have global pointerbases in different segments/sections of the binary.
-- We do not assume that such pointers are separate, but do assert it.
pointers_from_different_global_section ctxt a0 a1 = find_section_for_address ctxt (fromIntegral a0) /= find_section_for_address ctxt (fromIntegral a1)



domains_separate ctxt necc (Domain_Bases bs0) (Domain_Bases bs1) = 
  let quant = if necc then all else any in
    quant (uncurry $ pointer_bases_separate ctxt necc) [(b0,b1) | b0 <- S.toList bs0, b1 <- S.toList bs1]
domains_separate ctxt necc dom0 dom1 =
  let quant = if necc then all else any
      srcs0 = sources_of_domain ctxt dom0
      srcs1 = sources_of_domain ctxt dom1 in
    if S.null srcs0 || S.null srcs1 then
      False
    else
      quant (uncurry $ sources_separate ctxt necc) [(src0,src1) | src0 <- S.toList srcs0, src1 <- S.toList srcs1]




-- | Two pointerbases are separate if they refer to completely different parts of the memory.
-- We assume Stackframe, Global address space, and Heap are separate.
-- Two different @malloc@'s point to different regions.
pointer_bases_separate ctxt necc (StackPointer f)           (StackPointer f')         = if necc then False else f /= f'
pointer_bases_separate ctxt necc (StackPointer f)           (GlobalAddress _)         = True
pointer_bases_separate ctxt necc (StackPointer f)           (PointerToSymbol _ _)     = True
pointer_bases_separate ctxt necc (StackPointer f)           (Malloc _ _)              = True

pointer_bases_separate ctxt necc (GlobalAddress _)          (StackPointer f)          = True
pointer_bases_separate ctxt necc (GlobalAddress _)          (PointerToSymbol _ _)     = True
pointer_bases_separate ctxt necc (GlobalAddress a0)         (GlobalAddress a1)        = if necc then False else pointers_from_different_global_section (f_ctxt ctxt) a0 a1
pointer_bases_separate ctxt necc (GlobalAddress _)          (Malloc _ _)              = True

pointer_bases_separate ctxt necc (PointerToSymbol _ _)      (StackPointer f)          = True
pointer_bases_separate ctxt necc (PointerToSymbol _ _)      (GlobalAddress _)         = True
pointer_bases_separate ctxt necc (PointerToSymbol _ sym0)   (PointerToSymbol _ sym1)  = sym0 /= sym1
pointer_bases_separate ctxt necc (PointerToSymbol _ sym0)   (Malloc _ _)              = True

pointer_bases_separate ctxt necc (Malloc id0 hash0)         (Malloc id1 hash1)        = Nothing `notElem` [id0,id1] && Nothing `notElem` [hash0,hash1] && (id0,hash0) /= (id1,hash1)
pointer_bases_separate ctxt necc (Malloc _ _)               _                         = True


pointer_bases_separate_necessarily ctxt = pointer_bases_separate ctxt True
pointer_bases_separate_possibly    ctxt = pointer_bases_separate ctxt False

-- | Two sources are inputs for separate pointers if, e.g., one of them is the stackpointer and the other a malloc-return-value.
sources_separate :: FContext -> Bool -> BotSrc -> BotSrc -> Bool
sources_separate ctxt necc src0 src1 =
  case (src_to_base src0, src_to_base src1) of
    (Just b0,Just b1)          -> pointer_bases_separate ctxt necc b0 b1
    (Just (StackPointer _), _) -> not necc
    (_, Just (StackPointer _)) -> not necc
    (Just (Malloc _ _), _)     -> True
    (_, Just (Malloc _ _))     -> True
    _                          -> False
 where
  src_to_base (Src_StackPointer f)     = Just $ StackPointer f
  src_to_base (Src_Malloc i h)         = Just $ Malloc i h
  src_to_base (Src_ImmediateAddress a) = Just $ GlobalAddress a
  src_to_base _                        = Nothing



sources_separate_necessarily ctxt = sources_separate ctxt True
sources_separate_possibly    ctxt = sources_separate ctxt False



-- | Returns true iff the two given expressions can be shown to be separate.
-- This means that either:
--   * They both have spearate pointer domains.
--   * One of them is an immediate and the other is local.

separate_pointer_domains ctxt necc a0 a1 =
  let dom0 = get_pointer_domain ctxt a0
      dom1 = get_pointer_domain ctxt a1 in
  or [
    domains_separate ctxt necc dom0 dom1,
    is_immediate a0 && expr_is_highly_likely_local_pointer ctxt a1,
    is_immediate a1 && expr_is_highly_likely_local_pointer ctxt a0
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
necessarily_separate ctxt a0 si0 a1 si1 =
  if not (contains_bot a0) && not (contains_bot a1) then
    (a0,si0) /= (a1,si1) && sep a0 a1
  else
    separate_pointer_domains ctxt True a0 a1
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
      separate_pointer_domains ctxt True a0 a1


  -- v0 - i0 |x| v0 + i1 <==> True
  sep (SE_Op (Minus _) [v0, SE_Immediate i0])
      (SE_Op (Plus  _) [v1, SE_Immediate i1]) = 
    if necessarily_equal v0 v1 then
      True
    else
      separate_pointer_domains ctxt True a0 a1
  sep (SE_Op (Plus  _) [v0, SE_Immediate i0])
      (SE_Op (Minus _) [v1, SE_Immediate i1]) = 
    if necessarily_equal v0 v1 then
      True
    else
      separate_pointer_domains ctxt True a0 a1


  -- v0 - i0 |x| v0 <==> i0 >= si0
  sep (SE_Op (Minus _) [v0, SE_Immediate i0])
       v1 = 
    if necessarily_equal v0 v1 then
      fromIntegral i0 >= si0 
    else
      separate_pointer_domains ctxt True a0 a1
  sep v0
      (SE_Op (Minus _) [v1, SE_Immediate i1]) = 
    if necessarily_equal v0 v1 then
      fromIntegral i1 >= si1
    else
      separate_pointer_domains ctxt True a0 a1 

  -- v0 + i0 |x| v0 + i1 <==> v0 + i0 + si0 <= v0 + i1 || v0 + i1 + si1 <= v0 + i0 <==> i0+si0 <= i1 || i1+si1 <= i0
  sep (SE_Op (Plus _) [v0, SE_Immediate i0])
      (SE_Op (Plus _) [v1, SE_Immediate i1]) = 
    if necessarily_equal v0 v1 then
      fromIntegral i0 + si0 <= fromIntegral i1 || fromIntegral i1 + si1 <= fromIntegral i0
    else
      separate_pointer_domains ctxt True a0 a1 


  -- v0 + i0 |x| v0 <==> i0 >= si1
  sep (SE_Op (Plus _) [v0, SE_Immediate i0])
      v1 =
    if necessarily_equal v0 v1 then
      fromIntegral i0 >= si1
    else
      separate_pointer_domains ctxt True a0 a1 
  sep v1
      (SE_Op (Plus _) [v0,SE_Immediate i0]) =
    if necessarily_equal v0 v1 then
      fromIntegral i0 >= si1
    else
      separate_pointer_domains ctxt True a0 a1 

  -- remainder
  sep a0 a1 = separate_pointer_domains ctxt True a0 a1


-- | Returns true iff the given symbolic stateparts are necessarily separate.
necessarily_separate_stateparts ctxt (SP_Reg r0)     (SP_Reg r1)     = r0 /= r1
necessarily_separate_stateparts ctxt (SP_Mem a0 si0) (SP_Mem a1 si1) = necessarily_separate ctxt a0 si0 a1 si1
necessarily_separate_stateparts _    _               _               = True




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






