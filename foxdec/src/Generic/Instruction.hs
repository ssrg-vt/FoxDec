{-# LANGUAGE DeriveGeneric #-}

module Generic.Instruction (GenericInstruction(..)) where

import           Generic.Operand (GenericOperand)
import           GHC.Generics (Generic)
import qualified Data.Serialize as Cereal
import           Data.List (intercalate)

-- | A generic instruction
data GenericInstruction label storage prefix opcode annotation =
  Instruction { label :: label                         -- ^ unique identifier of the instruction: can be an immediate address, or a string label
              , prefix :: Maybe prefix                 -- ^ optional: prefix, e.g., lock or repz
              , opcode :: opcode                       -- ^ opcode/mnemonic
              , dest :: Maybe (GenericOperand storage) -- ^ destination operand, possibly none
              , srcs :: [GenericOperand storage]       -- ^ source operands, possibly empty
              , annot :: Maybe annotation              -- ^ optional: an annotation, such as the instruction size
              }
  deriving (Eq, Ord, Generic)

instance ( Cereal.Serialize label
         , Cereal.Serialize storage
         , Cereal.Serialize prefix
         , Cereal.Serialize opcode
         , Cereal.Serialize annotation)
  => Cereal.Serialize (GenericInstruction
                         label
                         storage
                         prefix
                         opcode
                         annotation)

instance (Show storage, Show label, Show prefix, Show opcode, Show annotation)
  => Show (GenericInstruction label storage prefix opcode annotation) where
  show (Instruction label prefix opcode dst srcs annot) = show label
    ++ ": "
    ++ showPrefix prefix
    ++ show opcode
    ++ " "
    ++ showDest dst
    ++ intercalate ", " (map show srcs)
    ++ showAnnot annot
    where
      showPrefix Nothing = ""
      showPrefix (Just pre) = show pre

      showAnnot Nothing = ""
      showAnnot (Just annot) = " (" ++ show annot ++ ")"

      showDest Nothing = ""
      showDest (Just op) = show op ++ " <- "