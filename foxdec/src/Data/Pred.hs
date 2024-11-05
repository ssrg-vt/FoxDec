{-# LANGUAGE DeriveGeneric, DefaultSignatures, StrictData #-}

{-|
Module      : Pred 
Description : A datatype for symbolic expressions and symbolic predicates.

A datatype for symbolic predicates, tailored to storing information
on equalities between the current values stored in state parts (lhs) 
and constant expressions (rhs).
-}

module Data.Pred 
 where

import Base
import Config
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.Word (Word64,Word32)
import Data.Traversable (for)
import Data.List
import Data.Maybe (mapMaybe,fromJust)
import Debug.Trace
import GHC.Generics
import Data.Bits (testBit, (.|.), (.&.))
import qualified Data.Serialize as Cereal hiding (get,put)
import X86.Register (Register)
import qualified X86.Operand as X86

import Data.SymbolicExpression



-- | A symbolic predicate consists of:
--
--   * A mapping from stateparts to symbolic expressions.
--   * The status of the flags.
--   * A set of verification conditions.
data Predicate = Predicate (M.Map StatePart SimpleExpr) FlagStatus
  deriving (Generic,Eq,Ord)


instance Cereal.Serialize Predicate

instance Show Predicate where
  show (Predicate eqs flg) =
       (intercalate "\n" $ (map (\(sp,e) -> show sp ++ " := " ++ show e) $ M.toList eqs))
    ++ (if show flg == "" then "" else "\n" ++ show flg)




-- | Pretty print predicate, showing Bottom expressions only as Bot
pp_pred :: Predicate -> String
pp_pred (Predicate eqs _) = (intercalate "\n" $ mapMaybe pp_pred_entry $ M.toList eqs)
 where
  pp_pred_entry (sp,v) =
    --if sp == SP_Reg RIP || contains_bot_sp sp then
    --  Nothing
    --else
      Just $ show sp ++ " := " ++ pp_expr v





