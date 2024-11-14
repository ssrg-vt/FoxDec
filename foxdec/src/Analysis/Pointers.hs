{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, DeriveGeneric, DefaultSignatures, FlexibleContexts, Strict #-}

{-|
Module      : Pointers
Description : Functions for dealing with symbolic pointers and abstraction.
-}
module Analysis.Pointers where

import Base
import Config

import Data.SymbolicExpression
import Data.Symbol

import Analysis.Context
import Analysis.FunctionNames

import X86.Register


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Int (Int64)

import Data.List
import Data.Word 
import Data.Maybe
import Debug.Trace



-- | Returns true iff the expression is an immediate address falling into the range of sections of the binary
expr_is_global_immediate ctxt (SE_Immediate a)
  | is_roughly_an_address ctxt (fromIntegral a) = address_has_external_symbol ctxt a || find_section_for_address ctxt (fromIntegral a) /= Nothing
  | otherwise = False
expr_is_global_immediate ctxt _ = False


-- | Returns true if the expression has a local pointerbase
expr_is_highly_likely_local_pointer ctxt e = get_pointer_domain ctxt e `existsAndSatisfies` is_local_pointer_domain ctxt True
expr_is_maybe_local_pointer         ctxt e = get_pointer_domain ctxt e `existsAndSatisfies` is_local_pointer_domain ctxt False

-- | Returns true iff the give domain is highly likelty local to the current function
is_local_pointer_domain ctxt necc (Domain_Bases bs) = quant is_local_base bs
 where
  is_local_base (StackPointer f)     = f == function_name_of_entry (f_ctxt ctxt) (f_entry ctxt)
  is_local_base _                    = False
  quant                              = if necc then all else any
is_local_pointer_domain ctxt necc (Domain_Sources srcs) = quant is_local_src srcs
 where
  is_local_src (Src_StackPointer f)  = f == function_name_of_entry (f_ctxt ctxt) (f_entry ctxt)
  is_local_src _                     = False
  quant                              = if necc then all else any


-- | Returns true if the expression has known pointerbases.
expr_highly_likely_pointer fctxt e = get_pointer_bases fctxt e /= Nothing


-- | Returns true if the expression has a heap pointerbase
expr_is_highly_likely_heap_pointer ctxt e = get_pointer_domain ctxt e `existsAndSatisfies` is_heap_pointer_domain ctxt

is_heap_pointer_domain ctxt (Domain_Bases bs) = all is_heap_base bs
 where
  is_heap_base (Malloc id hash)     = True
  is_heap_base _                    = False
is_heap_pointer_domain ctxt (Domain_Sources srcs) = all is_heap_source srcs
 where
  is_heap_source (Src_Malloc id hash) = True
  is_heap_source _                    = False






-- | Returns true iff the given symbolic regions are necessarily separate.
-- TODO: add VCS, not necc
necessarily_separate fctxt msg a0 (Just si0) a1 (Just si1) = 
  case (si0,si1) of
    (SE_Immediate si0', SE_Immediate si1') -> necessarily_separate_expressions a0 si0' a1 si1' || necessarily_separate_no_size fctxt msg a0 a1 
    _ -> necessarily_separate_no_size fctxt msg a0 a1
necessarily_separate fctxt msg a0 _ a1 _ = necessarily_separate_no_size fctxt msg a0 a1 

necessarily_separate_no_size fctxt msg a0 a1 = a0 /= a1 && (use_domains $ separate_pointer_domains fctxt a0 a1)
 where
  use_domains (necc,poss)
    | necc                    = True
    | poss                    = True
    | necessarily_equal a0 a1 = False
    | msg == "write" && not (contains_bot a0) && not (contains_bot a1) = trace ("Don't know separation of: " ++ show a0 ++ " and " ++ show a1) False
    | otherwise               = False

-- | Returns true iff the given symbolic regions are necessarily equal.
necessarily_equal a0 a1 = a0 == a1 && not (contains_bot a0) && not (contains_bot a1)











-- | Returns true iff the given symbolic regions are ncessarily separate.
-- For example:
--    @[RSP-16,4]@ is separate from @[RSP-12,8]@
--    @[RSP+8,4]@ is separate from @[RSP,8]@
--
-- If none of the cases apply where it can be shjown arithmetically that the expressions are separate,
-- we check whether the expressions can be proven separate based on their domains (see 'separate_pointer_domains').
necessarily_separate_expressions a0 si0 a1 si1 = not (contains_bot a0) && not (contains_bot a1) && (a0,si0) /= (a1,si1) && sep a0 a1
 where
  -- two immediate addresses
  sep (SE_Immediate a0)
      (SE_Immediate a1) =
    fromIntegral a0 + si0 <= fromIntegral a1 || fromIntegral a1 + si1 <= fromIntegral a0
  -- v0 - i0 |x| v0 - i1 <==> v0 - i0 + si0 <= v0 - i1 || v1 - i1 + si1 <= v0 - i0 <==> i0-si0 >= i1 || i1-si1 >= i0
  sep (SE_Op Minus _ [v0, SE_Immediate i0])
      (SE_Op Minus _ [v1, SE_Immediate i1]) = v0 == v1 && (fromIntegral i0 - si0 >= fromIntegral i1 || fromIntegral i1 - si1 >= fromIntegral i0)
  -- v0 - i0 |x| v0 + i1 <==> True
  sep (SE_Op Minus _ [v0, SE_Immediate i0])
      (SE_Op Plus  _ [v1, SE_Immediate i1]) = v0 == v1
  sep (SE_Op Plus  _ [v0, SE_Immediate i0])
      (SE_Op Minus _ [v1, SE_Immediate i1]) = v0 == v1
  -- v0 - i0 |x| v0 <==> i0 >= si0
  sep (SE_Op Minus _ [v0, SE_Immediate i0])
       v1 = v0 == v1 && fromIntegral i0 >= si0
  sep v0
      (SE_Op Minus _ [v1, SE_Immediate i1]) = v0 == v1 && fromIntegral i1 >= si1
  -- v0 + i0 |x| v0 + i1 <==> v0 + i0 + si0 <= v0 + i1 || v0 + i1 + si1 <= v0 + i0 <==> i0+si0 <= i1 || i1+si1 <= i0
  sep (SE_Op Plus _ [v0, SE_Immediate i0])
      (SE_Op Plus _ [v1, SE_Immediate i1]) =  v0 == v1 && (fromIntegral i0 + si0 <= fromIntegral i1 || fromIntegral i1 + si1 <= fromIntegral i0)
  -- v0 + i0 |x| v0 <==> i0 >= si1
  sep (SE_Op Plus _ [v0, SE_Immediate i0])
      v1 = v0 == v1 && fromIntegral i0 >= si1
  sep v1
      (SE_Op Plus _ [v0,SE_Immediate i0]) = v0 == v1 && fromIntegral i0 >= si1
  -- remainder
  sep a0 a1 = False




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
  enc (SE_Op Minus _ [v0, SE_Immediate i0])
      (SE_Op Minus _ [v1, SE_Immediate i1]) = 
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
  enc (SE_Op Plus _ [v0, SE_Immediate i0])
      (SE_Op Plus _ [v1, SE_Immediate i1]) = 
    v0 == v1 && i0 >= i1 && fromIntegral i0 + si0 <= fromIntegral i1 + si1
  -- v0 + i0 enc v0 <==> i0 + si0 <= si1
  enc (SE_Op Plus _ [v0, SE_Immediate i0])
      v1 =
    v0 == v1 && fromIntegral i0 + si0 <= si1
  -- immediates
  enc (SE_Immediate a0) (SE_Immediate a1) = 
    (fromIntegral a0::Int64) >= fromIntegral a1 && (fromIntegral a0::Int64) + fromIntegral si0 <= fromIntegral a1 + fromIntegral si1
  -- v0 enc v0 <==> si0 <= si1
  enc v0 v1
    | v0 == v1  = si0 <= si1
    | otherwise = False


-- | Returns true iff the given symbolic stateparts are necessarily equal.
necessarily_equal_stateparts (SP_Reg r0) (SP_Reg r1) = r0 == r1
necessarily_equal_stateparts (SP_Mem a0 si0) (SP_Mem a1 si1) = si0 == si1 && necessarily_equal a0 a1
necessarily_equal_stateparts _ _ = False


-- | Returns true iff the given symbolic stateparts are necessarily separate.
necessarily_separate_stateparts ctxt (SP_Reg r0)     (SP_Reg r1)     = r0 /= r1
necessarily_separate_stateparts ctxt (SP_Mem a0 si0) (SP_Mem a1 si1) = necessarily_separate_expressions a0 si0 a1 si1
necessarily_separate_stateparts _    _               _               = True


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
  FContext               -- ^ The current context
  -> SimpleExpr          -- ^ A symbolic expression 
  -> Maybe PointerDomain -- ^ A pointer domain
get_pointer_domain ctxt e =
  let bs  = get_pointer_base_set ctxt e in
    if not (S.null bs) then --  && S.size bs <= get_max_num_of_bases then
      Just $ Domain_Bases bs
    else let srcs = srcs_of_expr ctxt e in
      --if NES.size srcs <= get_max_num_of_sources then
        Just $ Domain_Sources srcs
      --else
      --  Nothing

 where
  --get_max_num_of_bases   = ctxt_max_num_of_bases $ f_ctxt ctxt
  --get_max_num_of_sources = ctxt_max_num_of_sources $ f_ctxt ctxt


get_pointer_base_set :: FContext -> SimpleExpr -> S.Set PointerBase
get_pointer_base_set ctxt (Bottom (FromNonDeterminism es)) = S.unions $ S.map (get_pointer_base_set ctxt) $ es -- TODO non-empty union?
get_pointer_base_set ctxt (SE_Op Plus _ es)                = S.unions $ S.map (get_pointer_base_set ctxt) $ S.fromList es
get_pointer_base_set ctxt (SE_Op Minus _ (e:es))           = get_pointer_base_set ctxt e
get_pointer_base_set ctxt (SE_Op And _ [e,SE_Immediate _]) = get_pointer_base_set ctxt e
get_pointer_base_set ctxt e                                = get_pointer_base e

 where
  get_pointer_base :: SimpleExpr -> S.Set PointerBase
  get_pointer_base (SE_Immediate a)
    | expr_is_global_immediate (f_ctxt ctxt) e =
    case IM.lookup (fromIntegral a) (ctxt_symbol_table $ f_ctxt ctxt) of
      Just sym -> S.singleton $ BaseIsSymbol sym
      Nothing  -> S.singleton $ GlobalAddress a 
    | otherwise = S.empty
  get_pointer_base (SE_Var (SP_StackPointer f))         = S.singleton $ StackPointer f
  get_pointer_base (SE_Var sp)                          = (statepart_to_pointerbase sp)
  get_pointer_base (SE_Malloc id hash)                  = S.singleton $ Malloc id hash
  get_pointer_base (Bottom (FromPointerBases bs))       = bs
  get_pointer_base e                                    = S.empty


  statepart_to_pointerbase :: StatePart -> S.Set PointerBase
  statepart_to_pointerbase (SP_Mem (SE_Immediate a) 8)  = case IM.lookup (fromIntegral a) (ctxt_symbol_table $ f_ctxt ctxt) of
                                                            Just (PointerToLabel  sym ex) -> S.singleton $ BaseIsSymbol $ AddressOfLabel sym  ex
                                                            Just (PointerToObject sym ex) -> S.singleton $ BaseIsSymbol $ AddressOfObject sym ex
                                                            _ -> S.empty
  statepart_to_pointerbase (SP_Reg FS)                  = S.singleton ThreadLocalStorage
  statepart_to_pointerbase (SP_Reg RSP)                 = S.singleton $ StackPointer $ "0x" ++ showHex (f_entry ctxt)
  statepart_to_pointerbase _                            = S.empty




-- | Returns true iff the two given expressions can be shown to be separate based on their domains.
separate_pointer_domains ctxt a0 a1 =
  let dom0 = get_pointer_domain ctxt a0
      dom1 = get_pointer_domain ctxt a1 in
    (separate_domains True dom0 dom1, separate_domains False dom0 dom1)
 where
  separate_domains True (Just (Domain_Bases bs0)) (Just (Domain_Bases bs1)) = 
    all (uncurry $ pointer_bases_separate ctxt True) [(b0,b1) | b0 <- S.toList bs0, b1 <- S.toList bs1]
  separate_domains False (Just (Domain_Bases bs0)) (Just (Domain_Bases bs1)) = 
    S.disjoint bs0 bs1 && any (uncurry $ pointer_bases_separate ctxt False) [(b0,b1) | b0 <- S.toList bs0, b1 <- S.toList bs1]

  separate_domains necc dom0 dom1 =
    let srcs0 = sources_of_domain dom0 a0
        srcs1 = sources_of_domain dom1 a1 in
      if necc then
        all (uncurry $ sources_separate ctxt necc) [(src0,src1) | src0 <- S.toList srcs0, src1 <- S.toList srcs1]
      else
        source_sets_separate ctxt necc srcs0 srcs1

  sources_of_domain (Just (Domain_Sources srcs)) _ = srcs
  sources_of_domain _ a                            = srcs_of_expr ctxt a




-- * Pointer bases
--
-- | Two pointerbases are separate if they refer to completely different parts of the memory.
-- We assume Stackframe, Global address space, and Heap are separate.
-- Two different @malloc@'s point to different regions.
pointer_bases_separate ctxt necc (StackPointer f)           (StackPointer f')         = if necc then False else f /= f'
pointer_bases_separate ctxt necc (StackPointer f)           (GlobalAddress _)         = True
pointer_bases_separate ctxt necc (StackPointer f)           (BaseIsSymbol _)          = True
pointer_bases_separate ctxt necc (StackPointer f)           (Malloc _ _)              = True
pointer_bases_separate ctxt necc (StackPointer f)           ThreadLocalStorage        = True

pointer_bases_separate ctxt necc (GlobalAddress _)          (StackPointer f)          = True
pointer_bases_separate ctxt necc (GlobalAddress _)          (BaseIsSymbol _)          = True
pointer_bases_separate ctxt necc (GlobalAddress a0)         (GlobalAddress a1)        = if necc then False else pointers_from_different_global_section (f_ctxt ctxt) a0 a1
pointer_bases_separate ctxt necc (GlobalAddress _)          (Malloc _ _)              = True
pointer_bases_separate ctxt necc (GlobalAddress _)          ThreadLocalStorage        = True

pointer_bases_separate ctxt necc (BaseIsSymbol _)           (StackPointer f)          = True
pointer_bases_separate ctxt necc (BaseIsSymbol _)           (GlobalAddress _)         = True
pointer_bases_separate ctxt necc (BaseIsSymbol sym0)        (BaseIsSymbol sym1)       = sym0 /= sym1
pointer_bases_separate ctxt necc (BaseIsSymbol sym0)        (Malloc _ _)              = True
pointer_bases_separate ctxt necc (BaseIsSymbol sym0)        ThreadLocalStorage        = True

pointer_bases_separate ctxt necc (Malloc id0 hash0)         (Malloc id1 hash1)        = Nothing `notElem` [id0,id1] && Nothing `notElem` [hash0,hash1] && (id0,hash0) /= (id1,hash1)
pointer_bases_separate ctxt necc (Malloc _ _)               _                         = True

pointer_bases_separate ctxt necc ThreadLocalStorage         (StackPointer f')         = True
pointer_bases_separate ctxt necc ThreadLocalStorage         (GlobalAddress _)         = True
pointer_bases_separate ctxt necc ThreadLocalStorage         (BaseIsSymbol _)          = True
pointer_bases_separate ctxt necc ThreadLocalStorage         (Malloc _ _)              = True
pointer_bases_separate ctxt necc ThreadLocalStorage         ThreadLocalStorage        = False

pointer_bases_separate_necessarily ctxt = pointer_bases_separate ctxt True
pointer_bases_separate_possibly    ctxt = pointer_bases_separate ctxt False


-- | Returns true iff the two given expressions have global pointerbases in different segments/sections of the binary.
-- We do not assume that such pointers are separate, but do assert it.
pointers_from_different_global_section ctxt a0 a1 = find_section_for_address ctxt (fromIntegral a0) /= find_section_for_address ctxt (fromIntegral a1)





-- | Returns the set of pointer bases, if any
get_pointer_bases ctxt e =
  let bs = get_pointer_base_set ctxt e in
    if S.null bs then
      Nothing
    else
      Just bs



-- * Pointer sources
srcs_of_expr ctxt (Bottom typ)                 = srcs_of_bottyp ctxt typ
srcs_of_expr ctxt (SE_Malloc id h)             = S.singleton $ Src_Malloc id h
srcs_of_expr ctxt (SE_Var (SP_StackPointer f)) = S.singleton $ Src_StackPointer f
srcs_of_expr ctxt (SE_Var sp)                  = S.singleton $ Src_Var sp
--srcs_of_expr ctxt (SE_StatePart sp)            = S.empty 
srcs_of_expr ctxt (SE_Op _ _ es)               = S.unions $ S.map (srcs_of_expr ctxt) $ S.fromList es
srcs_of_expr ctxt (SE_Bit i e)                 = srcs_of_expr ctxt e
srcs_of_expr ctxt (SE_SExtend _ _ e)           = srcs_of_expr ctxt e
srcs_of_expr ctxt (SE_Overwrite _ a b)         = S.unions $ S.map (srcs_of_expr ctxt) $ S.fromList [a,b]
srcs_of_expr ctxt e@(SE_Immediate i)           = 
  if address_has_external_symbol (f_ctxt ctxt) $ fromIntegral i then
    S.singleton $ Src_ImmediateAddress i
  else case find_section_for_address (f_ctxt ctxt) $ fromIntegral i of
    Just (_,_,a0,_,_) -> S.singleton $ Src_ImmediateAddress a0
    Nothing  -> S.empty

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
srcs_of_base ctxt (StackPointer f)        = S.singleton $ Src_StackPointer f
srcs_of_base ctxt (Malloc id h)           = S.singleton $ Src_Malloc id h
srcs_of_base ctxt (GlobalAddress a)       = srcs_of_expr ctxt $ SE_Immediate a
srcs_of_base ctxt (BaseIsSymbol sym)      = S.empty-- TODOS.singleton $ Src_Var $ SP_Mem (SE_Immediate a) 8
srcs_of_base ctxt ThreadLocalStorage      = S.singleton $ Src_Var $ SP_Reg FS





source_sets_separate ctxt necc srcs0 srcs1 = 
 let  --(ms0,srcs0'') = S.partition is_src_mem srcs0'
     --(ms1,srcs1'') = S.partition is_src_mem srcs1' in
     in
   not (S.null srcs0)
   &&
   not (S.null srcs1)
   &&
   (
     (S.disjoint srcs0 srcs1 && any (uncurry $ sources_separate ctxt necc) [(src0,src1) | src0 <- S.toList srcs0, src1 <- S.toList srcs1])
 --  ||
 --    (not (S.null ms0) && not (S.null ms1) && all (\src0 -> all (\src1 -> sources_separate ctxt necc src0 src1) ms1) ms0)
   )


-- | Two sources are inputs for separate pointers if, e.g., one of them is the stackpointer and the other a malloc-return-value.
sources_separate :: FContext -> Bool -> BotSrc -> BotSrc -> Bool
sources_separate ctxt necc (Src_Function f0) (Src_Function f1) = not necc && f0 /= f1
sources_separate ctxt necc (Src_Function f0) _                 = not necc 
sources_separate ctxt necc _                 (Src_Function f1) = not necc 
sources_separate ctxt necc src0 src1
  | otherwise =
      case (src_to_base src0, src_to_base src1) of
        (Just b0,Just b1)           -> pointer_bases_separate ctxt necc b0 b1
        (Just (StackPointer _), _)  -> not necc
        (_, Just (StackPointer _))  -> not necc
        (Just (Malloc _ _), _)      -> True
        (_, Just (Malloc _ _))      -> True
        (Just ThreadLocalStorage,_) -> not necc
        (_,Just ThreadLocalStorage) -> not necc
        _                           -> False -- if necc then False else src0 /= src1 -- TODO
 where
  src_to_base (Src_StackPointer f)     = Just $ StackPointer f
  src_to_base (Src_Malloc i h)         = Just $ Malloc i h
  src_to_base (Src_ImmediateAddress a) = Just $ GlobalAddress a
  src_to_base (Src_Var (SP_Reg FS))    = Just $ ThreadLocalStorage
  src_to_base _                        = Nothing

sources_separate_necessarily ctxt = sources_separate ctxt True
sources_separate_possibly    ctxt = sources_separate ctxt False





-- * Joining
rock_bottom = Bottom $ FromSources S.empty

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
  let es' = S.unions $ map (S.fromList . unfold_non_determinism (f_ctxt ctxt)) es in
    if S.size es' == 0 then
      rock_bottom
    else if S.size es' == 1 then
      head $ S.toList es'  
    else if S.size es' <= get_max_num_of_cases && all (not . contains_bot) es' then
      Bottom (FromNonDeterminism es')
    else if rock_bottom `S.member` es' then
      rock_bottom
    else let bss = S.map (nothing_to_empty . get_pointer_bases ctxt) es'
             bs  = S.unions bss in
      if S.size bs <= get_max_num_of_bases && all (not . S.null) bss then
        Bottom (FromPointerBases bs)
      else
        let srcs = S.map (srcs_of_expr ctxt) es' in
          if any S.null srcs then
            rock_bottom
          else if S.null $ S.unions srcs then
            rock_bottom
          else if (S.size $ S.unions srcs) <= get_max_num_of_sources then
            Bottom (FromSources $ S.unions srcs)
          else
            {--trace ("Hitting max num of sources: " ++ show get_max_num_of_sources)--} rock_bottom
 where
  get_max_num_of_cases   = ctxt_max_num_of_cases $ f_ctxt ctxt
  get_max_num_of_bases   = ctxt_max_num_of_bases $ f_ctxt ctxt
  get_max_num_of_sources = ctxt_max_num_of_sources $ f_ctxt ctxt

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
    Just bs -> if not (S.null bs) && S.size bs <= get_max_num_of_bases then Bottom (FromPointerBases bs) else join_sources
 where
  get_max_num_of_bases   = ctxt_max_num_of_bases $ f_ctxt ctxt
  get_max_num_of_sources = ctxt_max_num_of_sources $ f_ctxt ctxt

  join_sources =
    let srcs = srcs_of_expr ctxt e in 
      if S.null srcs then
        rock_bottom
      else if S.size srcs <= get_max_num_of_sources then
        Bottom (FromSources srcs)
      else
        {--trace ("Hitting max num of sources: " ++ show get_max_num_of_sources) --} rock_bottom




-- | Unfold an expression with non-determinisism to a list of expressions.
-- Keep an eye on the produced size, as this may cause blow-up.
unfold_non_determinism :: Context -> SimpleExpr -> [SimpleExpr]
unfold_non_determinism ctxt (Bottom (FromNonDeterminism es)) = S.toList es
unfold_non_determinism ctxt (SE_Op op si es)                 = 
  let es' = map (unfold_non_determinism ctxt) es in
    if crossProduct_size es' > ctxt_max_expr_size ctxt then [rock_bottom] else map (SE_Op op si) $ crossProduct es'
unfold_non_determinism ctxt (SE_Bit b e)                     = map (SE_Bit b) $ (unfold_non_determinism ctxt) e
unfold_non_determinism ctxt (SE_SExtend l h e)               = map (SE_SExtend l h) $ (unfold_non_determinism ctxt) e
unfold_non_determinism ctxt (SE_Overwrite l a b)             = 
  let as = unfold_non_determinism ctxt a
      bs = unfold_non_determinism ctxt b in
    if crossProduct_size [as,bs] > ctxt_max_expr_size ctxt then [rock_bottom] else [ SE_Overwrite l a' b' | a' <- as, b' <- bs ]
unfold_non_determinism ctxt e                                = [e]



-- | If the size of an expression becomes too large, we simply turn it into Bottom.
trim_expr ctxt e =
  if expr_size e > get_max_expr_size then 
    {--trace ("Hitting expr_size limit of " ++ show get_max_expr_size ++ ".")--} rock_bottom -- Bottom (FromSources $ srcs_of_expr e)
  else
    e
 where
  get_max_expr_size = ctxt_max_expr_size $ f_ctxt ctxt


