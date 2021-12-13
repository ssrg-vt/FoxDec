{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, DeriveGeneric, DefaultSignatures, FlexibleContexts, StrictData #-}

{-# OPTIONS_HADDOCK hide #-}


module Pointers where

import Base
import SimplePred
import Context
import X86_Datastructures
import ControlFlow


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import Data.Word 






get_pointer_bases :: Context -> SimpleExpr -> [PointerBase]
get_pointer_bases ctxt (Bottom (FromNonDeterminism es) _) = concatMap (get_pointer_bases ctxt) $ S.toList es
get_pointer_bases ctxt (SE_Op (Plus _) es)                = concatMap (get_pointer_bases ctxt) es
get_pointer_bases ctxt (SE_Op (Minus _) (e:es))           = get_pointer_bases ctxt e
get_pointer_bases ctxt e                                  = get_pointer_base e
 where
  get_pointer_base e@(SE_Immediate a)                      = if address_has_symbol ctxt a || find_section_for_address ctxt (fromIntegral a) /= Nothing then [GlobalAddress a] else [Unknown e]
  get_pointer_base e@(SE_Var (SP_Mem (SE_Immediate a) 8))  = case IM.lookup (fromIntegral a) (ctxt_syms ctxt) of
                                                               Nothing  -> [Unknown e]
                                                               Just sym -> [PointerToSymbol sym]
  get_pointer_base   (SE_Var (SP_Reg RSP))                 = [StackPointer]
  get_pointer_base   (SE_Malloc id hash)                   = [Malloc id hash]
  get_pointer_base   (Bottom (FromAbstraction bases) _)    = if S.null bases then [Unknown e] else S.toList bases
  get_pointer_base   e                                     = [Unknown e]

is_unknown_ptr_base (Unknown _) = True
is_unknown_ptr_base _           = False

is_global_ptr_base (GlobalAddress _) = True
is_global_ptr_base _                 = False

get_known_pointer_bases ctxt e = filter (not . is_unknown_ptr_base) $ get_pointer_bases ctxt e

expr_highly_likely_pointer ctxt e = get_known_pointer_bases ctxt e /= []

get_global_pointer_bases ctxt e = filter is_global_ptr_base $ get_pointer_bases ctxt e

expr_is_global_pointer ctxt e = get_global_pointer_bases ctxt e /= []

expr_is_local_pointer ctxt e = get_pointer_bases ctxt e == [StackPointer]


pointer_bases_separate StackPointer           StackPointer            = False
pointer_bases_separate StackPointer           (GlobalAddress _)       = True
pointer_bases_separate StackPointer           (PointerToSymbol _)     = True
pointer_bases_separate (GlobalAddress _)      StackPointer            = True
pointer_bases_separate (GlobalAddress _)      (PointerToSymbol _)     = True
pointer_bases_separate (GlobalAddress _)      (GlobalAddress _)       = False
pointer_bases_separate (PointerToSymbol _)    StackPointer            = True
pointer_bases_separate (PointerToSymbol _)    (GlobalAddress _)       = True
pointer_bases_separate (PointerToSymbol sym0) (PointerToSymbol sym1)  = sym0 /= sym1
pointer_bases_separate (Malloc id0 hash0)     (Malloc id1 hash1)      = Nothing `notElem` [id0,id1] && Nothing `notElem` [hash0,hash1] && (id0,hash0) /= (id1,hash1)
pointer_bases_separate (Malloc _ _)           _                       = True
pointer_bases_separate _                      (Malloc _ _)            = True



pointers_have_separate_bases ctxt a0 a1 =
  let bs0 = get_known_pointer_bases ctxt a0
      bs1 = get_known_pointer_bases ctxt a1 in
    bs0 /= [] && bs1 /= [] && all (uncurry pointer_bases_separate) [(b0,b1) | b0 <- bs0, b1 <- bs1]


invalid_bottom_pointer ctxt e = 
  let bases = get_pointer_bases ctxt e in
    all is_invalid_ptr_base bases
 where
  is_invalid_ptr_base (Unknown e) = contains_bot e && all_bot_satisfy invalid_bottom_base_for_pointer e
  is_invalid_ptr_base _           = False
  invalid_bottom_base_for_pointer (FromAbstraction bs) srcs = S.null bs
  invalid_bottom_base_for_pointer _ _                       = True

is_empty_FromAbstraction (FromAbstraction bs) = S.null bs
is_empty_FromAbstraction _                    = False





-- | Given a set of expressions, produce an expression that resembles the join of the entire set.
-- That is, the produced expression should be coarser than the disjunction of all input-expressions.
--
-- We join based on pointer-bases. If two expressions are "joinable", i.e., if they share the same pointer-bases,
-- they are joined to a bottom abstract expression with these pointer-bases.
join_nondeterministic_exprs ctxt es = 
  let es' = S.unions $ map exprs_of es in
   if S.size es' == 1 then 
     head $ S.toList es'
   else if any contains_bot es' then
     mk_bottom es'
   else let es'' = S.fromList $ join_expr_list $ S.toList es' in
     if S.size es'' == 1 then
       head $ S.toList es''
     else if S.size es'' > 25 then
       mk_bottom es''
     else
       Bottom (FromNonDeterminism es'') srcs
 where

  exprs_of (Bottom (FromNonDeterminism es) _) = S.unions $ map exprs_of $ S.toList es
  exprs_of e                                  = S.singleton e
  
  srcs = S.empty -- S.unions $ map srcs_of_expr es


  join_expr_list []     = []
  join_expr_list [e]    = [e]
  join_expr_list (e:es) =
    let (joins,unjoins) = partition (joinable e) es in
      join_exprs (e:joins) : join_expr_list unjoins

  joinable e0 e1 = 
    let bases0 = get_known_pointer_bases ctxt e0
        bases1 = get_known_pointer_bases ctxt e1 in
      or [
        bases0 /= [] && bases0 == bases1
        --pointers_from_same_global_section ctxt e0 e1
       ]

  join_exprs [e] = e
  join_exprs es  = mk_bottom es




  mk_bottom es = 
    if rock_bottom `elem` es then
      rock_bottom
    else let bs = S.fromList $ concatMap (get_known_pointer_bases ctxt) es in
      if S.null bs || S.size bs > 25 then -- TODO what if list of globals
        rock_bottom
      else
        Bottom (FromAbstraction bs) srcs
      


  get_global (GlobalAddress g) = g


rock_bottom = Bottom (FromAbstraction S.empty) S.empty


pointers_from_different_global_section ctxt a0 a1 =
    case (get_global_pointer_bases ctxt a0, get_global_pointer_bases ctxt a1) of
      ([GlobalAddress g0], [GlobalAddress g1]) -> find_section_for_address ctxt (fromIntegral g0) /= find_section_for_address ctxt (fromIntegral g1)
      _ -> False
