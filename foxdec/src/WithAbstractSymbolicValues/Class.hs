{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module WithAbstractSymbolicValues.Class where

import Base

import Data.SymbolicExpression (FlagStatus(..)) -- TODO

import Data.Indirection
import Data.JumpTarget
import Data.Size
import Data.GlobalMem
import Data.VerificationCondition
import Data.X86.Opcode
import Data.X86.Instruction
import Data.X86.Register

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.IntMap.Internal as IM (lookupLT, lookupGT)
import qualified Data.Set.NonEmpty as NES
import Data.Word
import Data.List

import GHC.Generics (Generic)
import Control.Monad.State.Strict hiding (join)

import qualified Data.Serialize as Cereal hiding (get,put)
import Control.DeepSeq
import GHC.Generics


data SymbolicOperation v = SO_Op Opcode Int (Maybe Int) [v] | SO_Bit Int v | SO_SExtend Int Int v | SO_Overwrite Int v v | SO_Plus v v | SO_Minus v v | SO_Times v v

-- | A statepart is either a register or a region in memory
data SStatePart p =
    SSP_Reg Register -- ^ A register
  | SSP_Mem p Int    -- ^ A region with a symbolic address and an immediate size.
 deriving (Eq, Ord, Generic)


instance (Cereal.Serialize p) => Cereal.Serialize (SStatePart p)
instance (NFData p) => NFData (SStatePart p)






data Sstate v p = Sstate {
    sregs  :: M.Map Register v,
    smem   :: M.Map (p,Maybe ByteSize) v,
    gmem   :: GlobalMem v,
    sflags :: [FlagStatus]
  }
  deriving (Eq,Ord,Generic)

instance (Ord v, Cereal.Serialize v,Ord p, Cereal.Serialize p) => Cereal.Serialize (Sstate v p)
instance (NFData v,NFData p) => NFData (Sstate v p)

instance (Show v,Show p) => Show (Sstate v p) where
  show (Sstate regs mem gmem flags) = intercalate "\n" $ (map show_reg $ M.assocs regs) ++ (map show_mem $ M.assocs mem) ++ [show gmem] ++ [show_flags flags]
   where
    show_reg (r,v)      = show r ++ " := " ++ show v
    show_mem ((a,si),v) = "[" ++ show a ++ "," ++ show_size si ++ "] := " ++ show v 
    show_flags []       = ""
    show_flags [flg]    = show flg
    show_flags flgs     = show flgs
    show_size Nothing   = "unknown"
    show_size (Just (ByteSize si)) = show si





-- | A function initialisation consists of a mapping of state parts to values, and memory relations
data MemRelation = Separate | Aliassing | Unknown
  deriving (Generic,Eq,Ord,Show)



data FInit v p = FInit (S.Set (SStatePart p,v)) (M.Map (SStatePart p,SStatePart p) MemRelation)
  deriving (Generic,Eq,Ord)


instance Cereal.Serialize MemRelation
instance (Cereal.Serialize v,Cereal.Serialize p, Ord p,Ord v) => Cereal.Serialize (FInit v p)

instance NFData MemRelation
instance (NFData p,NFData v) => NFData (FInit v p)


empty_finit = FInit S.empty M.empty


unknownSize = Nothing


class (Ord v,Eq v,Show v, Eq p,Ord p,Show p) => WithAbstractSymbolicValues ctxt v p | ctxt -> v p where 
  sseparate :: ctxt -> String -> p -> Maybe ByteSize -> p -> Maybe ByteSize -> Bool
  senclosed :: ctxt -> p -> Maybe ByteSize -> p -> Maybe ByteSize -> Bool
  salias :: ctxt -> p -> Maybe ByteSize -> p -> Maybe ByteSize -> Bool
  ssensitive :: ctxt -> p -> Maybe ByteSize -> v -> Bool
  sread_from_ro_data :: ctxt -> p -> Maybe ByteSize -> Maybe v
  smk_mem_addresses :: ctxt -> String -> Bool -> v -> S.Set p

  sjoin_values :: Foldable t => ctxt -> String -> t v -> v
  swiden_values :: ctxt -> String -> v -> v
  sjoin_pointers :: ctxt -> [p] -> [p]
  top :: ctxt -> String -> v


  ssemantics :: ctxt -> String -> SymbolicOperation v -> v
  sflg_semantics :: ctxt -> v -> Instruction -> [FlagStatus] -> [FlagStatus]
  simmediate :: Integral i => ctxt -> i -> v

  smk_init_reg_value :: ctxt -> Register -> v
  smk_init_mem_value :: ctxt -> String -> p -> Maybe ByteSize -> v

  scall :: ctxt -> Instruction -> State (Sstate v p,VCS v) ()
  sjump :: ctxt -> Instruction -> State (Sstate v p,VCS v) ()

  saddress_has_instruction :: ctxt -> Word64 -> Bool

  stry_global :: ctxt -> p -> Maybe (Int, Bool)
  stry_jump_targets :: ctxt -> v -> Maybe (S.Set ResolvedJumpTarget)
  stry_resolve_error_call :: ctxt -> Instruction -> v -> Maybe Indirection

  stry_immediate :: ctxt -> v -> Maybe Word64
  simmediate_to_pointer :: ctxt -> Word64 -> p
  sis_deterministic :: ctxt -> v -> Bool
  scheck_regs_in_postcondition :: ctxt -> v -> v -> Bool

  sget_gmem_structure :: ctxt -> GMemStructure


