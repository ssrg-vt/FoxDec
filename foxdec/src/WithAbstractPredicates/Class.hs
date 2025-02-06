{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances, FlexibleContexts#-}

module WithAbstractPredicates.Class where

import Data.L0
import Data.Indirection
import Data.VerificationCondition
import Binary.Generic
import Data.X86.Instruction

import Control.Monad.State.Strict




class (Show v,Ord v,Eq pred, Eq finit,BinaryClass bin,Show pred,Show finit) => WithAbstractPredicates bin pred finit v | pred -> finit, pred -> v where
 symbolically_execute :: LiftingEntry bin pred finit v -> Bool -> [Instruction] -> Maybe [Instruction] -> State (pred,VCS v) ()
 verify_postcondition :: LiftingEntry bin pred finit v -> pred -> Bool
 finit_to_init_pred :: LiftingEntry bin pred finit v -> finit -> pred
 pred_to_finit :: LiftingEntry bin pred finit v -> pred -> finit
 resolve_indirection :: LiftingEntry bin pred finit v -> pred -> [Instruction] -> Indirections
 is_weaker_than :: LiftingEntry bin pred finit v -> pred -> pred -> Bool
 join_preds :: LiftingEntry bin pred finit v -> pred -> pred -> pred
 join_finits :: LiftingEntry bin pred finit v -> finit -> finit -> finit
 new_finit :: Lifting bin pred finit v -> finit
 pp_finit :: Lifting bin pred finit v -> finit -> String


