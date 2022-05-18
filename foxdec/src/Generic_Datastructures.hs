{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

{-|
Module      : Generic_Datastructures
Description : Datastructures for storing x86-64 instructions.
-}

module Generic_Datastructures where

import Data.List ( intercalate )
import Data.Word (Word64)
import Base ( showHex )
import qualified Data.Map as M
import GHC.Generics ( Generic )
import qualified Data.Serialize as Cereal hiding (get,put)
import Typeclasses.HasSize (HasSize (sizeof))
import Generic.Address (GenericAddress)
import Generic.Operand (GenericOperand)

-- | A generic instruction
data Instruction label storage prefix opcode annotation = Instruction {
  instr_label  :: label,                           -- ^ unique identifier of the instruction: can be an immediate address, or a string label
  instr_prefix :: Maybe prefix,                    -- ^ optional: prefix, e.g., lock or repz
  instr_opcode :: opcode,                          -- ^ opcode/mnemonic
  instr_dest   :: Maybe (GenericOperand storage) , -- ^ destination operand, possibly none
  instr_srcs   :: [GenericOperand storage],        -- ^ source operands, possibly empty
  instr_annot  :: Maybe annotation                 -- ^ optional: an annotation, such as the instruction size
 }
  deriving (Eq,Ord,Generic)


instance (Cereal.Serialize label, Cereal.Serialize storage, Cereal.Serialize prefix, Cereal.Serialize opcode, Cereal.Serialize annotation) =>
         Cereal.Serialize (Instruction label storage prefix opcode annotation)

instance (Eq storage, Show storage,Show label,Show prefix,Show opcode,Show annotation) => Show (Instruction label storage prefix opcode annotation) where
  show (Instruction label prefix opcode dst srcs annot) =
    show label ++ ": " ++
    show_prefix prefix ++
    show opcode ++ " " ++
    show_dest dst ++
    intercalate ", " (map show srcs) ++
    show_annot annot
   where
    show_prefix Nothing    = ""
    show_prefix (Just pre) = show pre

    show_annot Nothing = ""
    show_annot (Just annot) = " (" ++ show annot ++ ")"

    show_dest Nothing = ""
    show_dest (Just op) = show op ++ " <- "
