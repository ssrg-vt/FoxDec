{-# LANGUAGE DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

module WithAbstractSymbolicValues.InstanceOfAbstractPredicates where

import Config

import WithAbstractPredicates.Class

import WithAbstractSymbolicValues.SymbolicExecution
import WithAbstractSymbolicValues.ResolveIndirections
import WithAbstractSymbolicValues.Class
import WithAbstractSymbolicValues.FInit

import Binary.Generic

import Data.L0

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Data.Word

instance (Show v,Ord v,BinaryClass bin, WithAbstractSymbolicValues (bin, Config, L0 (Sstate v p) (FInit v p) v, Word64) v p) => WithAbstractPredicates bin (Sstate v p) (FInit v p) v where
  symbolically_execute = sexec_block
  verify_postcondition = sverify_postcondition
  finit_to_init_pred = finit_to_init_sstate
  pred_to_finit = sstate_to_finit
  resolve_indirection = stry_resolve_indirection
  is_weaker_than = simplies
  join_preds static = sjoin_states static ""
  join_finits static = join_finit static
  new_finit = \_ -> FInit S.empty M.empty
  pp_finit = \_ -> pp_finitC


