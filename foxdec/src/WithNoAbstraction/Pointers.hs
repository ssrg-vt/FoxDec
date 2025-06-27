{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, DeriveGeneric, DefaultSignatures, FlexibleContexts, StrictData #-}

{-|
Module      : Pointers
Description : Functions for dealing with symbolic pointers and abstraction.
-}
module WithNoAbstraction.Pointers where

import Base
import Config

import Data.SymbolicExpression
import Data.Symbol
import Data.L0
import Data.SValue
import Data.SPointer
import Data.X86.Register

import WithAbstractSymbolicValues.Class
import WithAbstractSymbolicValues.FInit

import Binary.Generic


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set.NonEmpty as NES
import Data.Int (Int64)

import Data.List
import Data.Word 
import Data.Maybe
import Debug.Trace

import GHC.Generics


-- | An abstract domain for pointers
data PointerDomain =
    Domain_Bases    (NES.NESet PointerBase)  -- a non-empty set of bases
  | Domain_Sources  (NES.NESet BotSrc)       -- a possibly empty set of sources
  deriving (Generic,Eq,Ord,Show)



-- | Returns true iff a symbol is associated with the address.
address_has_external_symbol bin a =
  case IM.lookup (fromIntegral a) $ binary_get_symbol_table bin of
    Just (PointerToLabel _ ex)  -> ex
    Just (PointerToObject _ ex) -> ex
    Just (AddressOfObject _ ex) -> ex
    Just (AddressOfLabel _ ex)  -> ex
    _ -> False





-- | Returns true iff the expression is an immediate address falling into the range of sections of the binary
expr_is_global_immediate bin (SE_Immediate a)
  | is_roughly_an_address bin (fromIntegral a) = address_has_external_symbol bin a || find_section_for_address bin (fromIntegral a) /= Nothing
  | otherwise = False
expr_is_global_immediate ctxt _ = False


-- | Returns true if the expression has a local pointerbase
expr_is_highly_likely_local_pointer bin e = get_pointer_domain bin no_finit e `existsAndSatisfies` is_local_pointer_domain True
expr_is_maybe_local_pointer         bin e = get_pointer_domain bin no_finit e `existsAndSatisfies` is_local_pointer_domain False


expr_is_highly_likely_global_pointer bin e = get_pointer_domain bin no_finit e `existsAndSatisfies` is_global_pointer_domain True
expr_is_maybe_global_pointer         bin e = get_pointer_domain bin no_finit e `existsAndSatisfies` is_global_pointer_domain False


no_finit = FInit S.empty M.empty

-- | Returns true iff the give domain is highly likely local to the current function
is_local_pointer_domain necc (Domain_Bases bs) = quant is_local_base bs
 where
  is_local_base StackPointer     = True
  is_local_base _                = False
  quant                          = if necc then all else any
is_local_pointer_domain necc (Domain_Sources srcs) = quant is_local_src srcs
 where
  is_local_src (Src_StackPointer)  = True
  is_local_src _                   = False
  quant                            = if necc then all else any

-- | Returns true iff the give domain is highly likely global
is_global_pointer_domain necc (Domain_Bases bs) = quant is_global_base bs
 where
  is_global_base (GlobalAddress _) = True
  is_global_base _                 = False
  quant                            = if necc then all else any
is_global_pointer_domain necc (Domain_Sources srcs) = quant is_global_src srcs
 where
  is_global_src (Src_ImmediateAddress _) = True
  is_global_src _                        = False
  quant                                  = if necc then all else any


-- | Returns true if the expression has known pointerbases.
expr_highly_likely_pointer bin finit e = not $ S.null $ get_pointer_base_set bin finit e

{--
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

--}



-- | Returns true iff the given symbolic regions are necessarily separate.
-- TODO: add VCS, not necc
necessarily_separate bin finit msg a0 (Just si0) a1 (Just si1) = 
  case (si0,si1) of
    (SE_Immediate si0', SE_Immediate si1') -> necessarily_separate_expressions a0 si0' a1 si1' || necessarily_separate_no_size bin finit msg a0 a1 
    _ -> necessarily_separate_no_size bin finit msg a0 a1
necessarily_separate bin finit msg a0 _ a1 _ = necessarily_separate_no_size bin finit msg a0 a1 

necessarily_separate_no_size bin finit msg a0 a1 = a0 /= a1 && (use_domains $ separate_pointer_domains bin finit a0 a1)
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




-- * Pointer Domains
--
-- Turn a symbolic expression into a pointer domain: either a pointer base, or a set of sources.
--
-- * A 'PointerBase' is a positive addend of a symbolic expression that likely represents a pointer. It can be a stack pointer, an immediate pointer into a section, or a pointer to a known symbol.
-- * A source is some statepart whose initial value affects the value of the pointer.
--
get_pointer_domain ::
  BinaryClass bin =>
     bin                     -- ^ The binary
  -> FInit SValue SPointer   -- ^ The current context
  -> SimpleExpr              -- ^ A symbolic expression 
  -> Maybe PointerDomain     -- ^ A pointer domain
get_pointer_domain bin finit e =
  let bs = get_pointer_base_set bin finit e in
    if not (S.null bs) then --  && S.size bs <= get_max_num_of_bases then TODO
      Just $ Domain_Bases $ NES.unsafeFromSet bs
    else let srcs = srcs_of_expr bin e in
      if not (S.null srcs) && S.size srcs <= 100 then -- TODO get_max_num_of_sources then
        Just $ Domain_Sources $  NES.unsafeFromSet srcs
      else
        Nothing

 where
  --get_max_num_of_bases   = ctxt_max_num_of_bases $ f_ctxt ctxt
  --get_max_num_of_sources = ctxt_max_num_of_sources $ f_ctxt ctxt


get_pointer_base_set ::
  BinaryClass bin =>
     bin                     -- ^ The binary
  -> FInit SValue SPointer   -- ^ The current context
  -> SimpleExpr
  -> S.Set PointerBase
get_pointer_base_set bin finit (Bottom (FromNonDeterminism es)) = S.unions $ S.map (get_pointer_base_set bin finit) $ NES.toSet es -- TODO non-empty union?
get_pointer_base_set bin finit (SE_Op Plus _ es)                = S.unions $ S.map (get_pointer_base_set bin finit) $ S.fromList es
get_pointer_base_set bin finit (SE_Op Minus _ (e:es))           = get_pointer_base_set bin finit e
get_pointer_base_set bin finit (SE_Op And _ [e,SE_Immediate _]) = get_pointer_base_set bin finit e
get_pointer_base_set bin finit (SE_Op And _ [SE_Immediate _,e]) = get_pointer_base_set bin finit e
get_pointer_base_set bin (FInit sps m) e                        = get_pointer_base e

 where
  get_pointer_base :: SimpleExpr -> S.Set PointerBase
  get_pointer_base (SE_Immediate a)
    | expr_is_global_immediate bin e = S.singleton $ GlobalAddress a 
    | otherwise = S.empty
  get_pointer_base (SE_Var sp)                        = statepart_to_pointerbase sp
  get_pointer_base (SE_Malloc id hash)                = S.singleton $ Malloc id hash
  get_pointer_base (Bottom (FromPointerBases bs))     = NES.toSet bs
  get_pointer_base e                                  = S.empty


  statepart_to_pointerbase :: StatePart -> S.Set PointerBase
  statepart_to_pointerbase (SP_Mem (SE_Immediate a) 8)  = case IM.lookup (fromIntegral a) (binary_get_symbol_table bin) of
                                                            Just sym -> S.singleton $ GlobalAddress a
                                                            Nothing -> S.empty
  statepart_to_pointerbase (SP_Reg (RegSeg FS))         = S.singleton ThreadLocalStorage
  statepart_to_pointerbase (SP_Reg (Reg64 RSP))         = S.singleton $ StackPointer
  statepart_to_pointerbase sp
    | any (has_mem_rel sp) $ M.keys m                   = S.singleton $ BaseIsStatePart sp
  statepart_to_pointerbase _                            = S.empty


  has_mem_rel (SP_Reg r)    (sp0,sp1) = SSP_Reg r `elem` [sp0,sp1]
  has_mem_rel (SP_Mem a si) (sp0,sp1) = SSP_Mem (Ptr_Concrete a) si  `elem` [sp0,sp1]




-- | Returns true iff the two given expressions can be shown to be separate based on their domains.
separate_pointer_domains bin finit a0 a1 =
  let dom0 = get_pointer_domain bin finit a0
      dom1 = get_pointer_domain bin finit a1 in
    (separate_domains True dom0 dom1, separate_domains False dom0 dom1)
 where
  separate_domains True (Just (Domain_Bases bs0)) (Just (Domain_Bases bs1)) = 
    all (uncurry $ pointer_bases_separate bin finit True) [(b0,b1) | b0 <- neSetToList bs0, b1 <- neSetToList bs1]
  separate_domains False (Just (Domain_Bases bs0)) (Just (Domain_Bases bs1)) = 
    NES.disjoint bs0 bs1 && any (uncurry $ pointer_bases_separate bin finit False) [(b0,b1) | b0 <- neSetToList bs0, b1 <- neSetToList bs1]

  separate_domains necc dom0 dom1 =
    let srcs0 = sources_of_domain dom0 a0
        srcs1 = sources_of_domain dom1 a1 in
      if necc then
        all (uncurry $ sources_separate bin finit necc) [(src0,src1) | src0 <- S.toList srcs0, src1 <- S.toList srcs1]
      else
        source_sets_separate bin finit necc srcs0 srcs1

  sources_of_domain (Just (Domain_Sources srcs)) _ = NES.toSet srcs
  sources_of_domain _ a                            = srcs_of_expr bin a




-- * Pointer bases
--
-- | Two pointerbases are separate if they refer to completely different parts of the memory.
-- We assume Stackframe, Global address space, and Heap are separate.
-- Two different @malloc@'s point to different regions.
pointer_bases_separate bin finit necc StackPointer           StackPointer            = False
pointer_bases_separate bin finit necc StackPointer           (GlobalAddress _)       = True
pointer_bases_separate bin finit necc StackPointer           (Malloc _ _)            = True
pointer_bases_separate bin finit necc StackPointer           ThreadLocalStorage      = True
pointer_bases_separate bin finit necc StackPointer           (BaseIsStatePart _)     = True

pointer_bases_separate bin finit necc (GlobalAddress _)      StackPointer        = True
pointer_bases_separate bin finit necc (GlobalAddress a0)     (GlobalAddress a1)  = if a1 > a0 then a1 - a0 >= 8 else a0 - a1 >= 8 -- if necc then False else pointers_from_different_global_section bin a0 a1
pointer_bases_separate bin finit necc (GlobalAddress _)      (Malloc _ _)        = True
pointer_bases_separate bin finit necc (GlobalAddress _)      ThreadLocalStorage  = True
pointer_bases_separate bin finit necc (GlobalAddress _)      (BaseIsStatePart _) = True -- TODO add to finit whether pointer is global

pointer_bases_separate bin finit necc (Malloc id0 hash0)     (Malloc id1 hash1)  = Nothing `notElem` [id0,id1] && Nothing `notElem` [hash0,hash1] && (id0,hash0) /= (id1,hash1)
pointer_bases_separate bin finit necc (Malloc _ _)           _                   = True

pointer_bases_separate bin finit necc ThreadLocalStorage     StackPointer        = True
pointer_bases_separate bin finit necc ThreadLocalStorage     (GlobalAddress _)   = True
pointer_bases_separate bin finit necc ThreadLocalStorage     (Malloc _ _)        = True
pointer_bases_separate bin finit necc ThreadLocalStorage     ThreadLocalStorage  = False
pointer_bases_separate bin finit necc ThreadLocalStorage     (BaseIsStatePart _) = True

pointer_bases_separate bin finit necc (BaseIsStatePart sp0)  StackPointer        = True
pointer_bases_separate bin finit necc (BaseIsStatePart sp0)  (GlobalAddress _)   = True -- TODO add to finit whether pointer is global
pointer_bases_separate bin finit necc (BaseIsStatePart sp0)  (Malloc _ _)        = True
pointer_bases_separate bin finit necc (BaseIsStatePart sp0)  ThreadLocalStorage  = True
pointer_bases_separate bin (FInit sps m) necc (BaseIsStatePart sp0)  (BaseIsStatePart sp1) = lookup_mem_rel sp0 sp1
 where
  lookup_mem_rel sp0 sp1 =
    case find (\((sp0',sp1'),mem_rel) -> (sp_eq_ssp sp0 sp0' && sp_eq_ssp sp1 sp1') || sp_eq_ssp sp0 sp1' && sp_eq_ssp sp1 sp0') $ M.assocs m of
      Just (_, Separate) -> True
      _ -> False

  sp_eq_ssp (SP_Reg r0) (SSP_Reg r1) = r0 == r1
  sp_eq_ssp (SP_Mem a0 si0) (SSP_Mem (Ptr_Concrete a1) si1) = a0==a1 && si0==si1
  sp_eq_ssp _ _ = False


pointer_bases_separate_necessarily bin finit = pointer_bases_separate bin finit True
pointer_bases_separate_possibly    bin finit = pointer_bases_separate bin finit False




-- | Returns true iff the two given expressions have global pointerbases in different segments/sections of the binary.
-- We do not assume that such pointers are separate, but do assert it.
pointers_from_different_global_section ctxt a0 a1 = find_section_for_address ctxt (fromIntegral a0) /= find_section_for_address ctxt (fromIntegral a1)



-- * Pointer sources
srcs_of_expr ctxt (Bottom typ)                 = srcs_of_bottyp ctxt typ
srcs_of_expr ctxt (SE_Malloc id h)             = S.singleton $ Src_Malloc id h
srcs_of_expr ctxt (SE_Var sp)                  = S.singleton $ Src_Var sp
srcs_of_expr ctxt (SE_Op _ _ es)               = S.unions $ S.map (srcs_of_expr ctxt) $ S.fromList es
srcs_of_expr ctxt (SE_Bit i e)                 = srcs_of_expr ctxt e
srcs_of_expr ctxt (SE_SExtend _ _ e)           = srcs_of_expr ctxt e
srcs_of_expr ctxt (SE_Overwrite _ a b)         = S.unions $ S.map (srcs_of_expr ctxt) $ S.fromList [a,b]
srcs_of_expr ctxt e@(SE_Immediate i)           
  | address_has_external_symbol ctxt $ fromIntegral i =
    S.singleton $ Src_ImmediateAddress i
  | otherwise =
    case find_section_for_address ctxt $ fromIntegral i of
      Just (_,_,a0,_,_,_) -> S.singleton $ Src_ImmediateAddress a0
      Nothing  -> S.empty

-- | Returns the set of sources of the bottom type
srcs_of_bottyp ctxt (FromNonDeterminism es)        = S.unions $ S.map (srcs_of_expr ctxt) $ NES.toSet es
srcs_of_bottyp ctxt (FromPointerBases bs)          = S.unions $ S.map (srcs_of_base ctxt) $ NES.toSet bs
srcs_of_bottyp ctxt (FromSources srcs)             = NES.toSet $ srcs
srcs_of_bottyp ctxt (FromBitMode srcs)             = NES.toSet $ srcs
srcs_of_bottyp ctxt (FromOverlap srcs)             = NES.toSet $ srcs
srcs_of_bottyp ctxt (FromSemantics srcs)           = NES.toSet $ srcs
srcs_of_bottyp ctxt (FromMemWrite srcs)            = NES.toSet $ srcs
srcs_of_bottyp ctxt (FromUninitializedMemory srcs) = NES.toSet $ srcs
srcs_of_bottyp ctxt (FromCall f)                   = S.singleton $ Src_Function f


-- | Returns the set of sources of the pointerbase
srcs_of_base ctxt StackPointer            = S.singleton $ Src_StackPointer
srcs_of_base ctxt (Malloc id h)           = S.singleton $ Src_Malloc id h
srcs_of_base ctxt (GlobalAddress a)       = srcs_of_expr ctxt $ SE_Immediate a
srcs_of_base ctxt ThreadLocalStorage      = S.singleton $ Src_Var $ SP_Reg (RegSeg FS)







source_sets_separate bin finit necc srcs0 srcs1 = 
 not (S.null srcs0)
 &&
 not (S.null srcs1)
 &&
   (
     (S.disjoint srcs0 srcs1 && any (uncurry $ sources_separate bin finit necc) [(src0,src1) | src0 <- S.toList srcs0, src1 <- S.toList srcs1])
 --  ||
 --    (not (S.null ms0) && not (S.null ms1) && all (\src0 -> all (\src1 -> sources_separate ctxt necc src0 src1) ms1) ms0)
   )


-- | Two sources are inputs for separate pointers if, e.g., one of them is the stackpointer and the other a malloc-return-value.
sources_separate bin finit necc (Src_Function f0) (Src_Function f1) = not necc && f0 /= f1
sources_separate bin finit necc (Src_Function f0) _                 = not necc 
sources_separate bin finit necc _                 (Src_Function f1) = not necc 
sources_separate bin finit necc src0 src1
  | otherwise =
      case (src_to_base src0, src_to_base src1) of
        (Just b0,Just b1)           -> pointer_bases_separate bin finit necc b0 b1
        (Just StackPointer, _)      -> not necc
        (_, Just StackPointer)      -> not necc
        (Just (Malloc _ _), _)      -> True
        (_, Just (Malloc _ _))      -> True
        (Just ThreadLocalStorage,_) -> not necc
        (_,Just ThreadLocalStorage) -> not necc
        _                           -> if necc then False else src0 /= src1 -- TODO use finit
 where
  src_to_base (Src_StackPointer)       = Just $ StackPointer
  src_to_base (Src_Malloc i h)         = Just $ Malloc i h
  src_to_base (Src_ImmediateAddress a) = Just $ GlobalAddress a
  src_to_base (Src_Var (SP_Reg (RegSeg FS)))    = Just $ ThreadLocalStorage
  src_to_base _                        = Nothing

sources_separate_necessarily bin finit = sources_separate bin finit True
sources_separate_possibly    bin finit = sources_separate bin finit False
