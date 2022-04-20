{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

{-|
Module      : Generic_Datastructures
Description : Datastructures for storing x86-64 instructions.
-}

module Generic_Datastructures where

import Data.List
import Data.Word (Word64)
import Base
import qualified Data.Map as M
import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)


-- | An unresolved address, within the operand of an instruction, based on polymorphic type `storage`.
data GenericAddress storage =
    AddressStorage storage                                          -- ^ Reading a pointer from a storage
  | AddressImm Word64                                               -- ^ Immediate value 
  | AddressMinus (GenericAddress storage) (GenericAddress storage)  -- ^ Minus
  | AddressPlus  (GenericAddress storage) (GenericAddress storage)  -- ^ Plus
  | AddressTimes (GenericAddress storage) (GenericAddress storage)  -- ^ Times
  deriving (Eq,Ord,Generic)


-- | A generic statepart, based on polymorphic type `storage`.
data GenericOperand storage =
    Memory            (GenericAddress storage) Int -- ^ A region in memory, whose address is stored in the given state part and whose size in bytes is given in the Int
  | EffectiveAddress  (GenericAddress storage)     -- ^ An address itself, but not the value stored at the address.
  | Storage           storage                      -- ^ A storage location such as a register or a variable
  | Immediate         Word64                       -- ^ An immediate value
  deriving (Eq,Ord,Generic)


-- | A generic instruction
data Instruction label storage prefix opcode annotation = Instruction {
  instr_label  :: label,                    -- ^ unique identifier of the instruction: can be an immediate address, or a string label
  instr_prefix :: Maybe prefix,             -- ^ optional: prefix, e.g., lock or repz
  instr_opcode :: opcode,                   -- ^ opcode/mnemonic
  instr_ops    :: [GenericOperand storage], -- ^ operands, possibly empty
  instr_annot  :: Maybe annotation          -- ^ optional: an annotation, such as the instruciton size
 }
  deriving (Eq,Ord,Generic)


-- | An empty type, but a showable instance can be derived
newtype Void = Void Void

-- | A type for encapsulating an immediate (allows to always show hex)
data AddressWord64 = AddressWord64 Word64
  deriving (Eq,Ord,Generic)



instance Cereal.Serialize AddressWord64
instance (Cereal.Serialize storage) => Cereal.Serialize (GenericAddress storage)
instance (Cereal.Serialize storage) => Cereal.Serialize (GenericOperand storage)
instance (Cereal.Serialize label, Cereal.Serialize storage, Cereal.Serialize prefix, Cereal.Serialize opcode, Cereal.Serialize annotation) => 
         Cereal.Serialize (Instruction label storage prefix opcode annotation)






-- showing the datastructures
instance Show storage => Show (GenericAddress storage) where
  show (AddressStorage st)  = show st
  show (AddressImm imm)     = showHex imm
  show (AddressMinus a0 a1) = show a0 ++ " - " ++ show a1
  show (AddressPlus  a0 a1) = show a0 ++ " + " ++ show a1
  show (AddressTimes a0 a1) = show a0 ++ " * " ++ show a1

instance Show storage => Show (GenericOperand storage) where
  show (Memory addr si)        = show_size_directive si ++ " [" ++ show addr ++ "," ++ show si ++ "]"
  show (EffectiveAddress addr) = "[" ++ show addr ++ "]"
  show (Storage st)            = show st
  show (Immediate imm)         = showHex imm

-- | Showing a size directive
show_size_directive 1  = "BYTE PTR"
show_size_directive 2  = "WORD PTR"
show_size_directive 4  = "DWORD PTR"
show_size_directive 8  = "QWORD PTR"
show_size_directive 16 = "XMMWORD PTR"
show_size_directive 32 = "YMMWORD PTR"
show_size_directive si = show (si*8) ++ " PTR"

instance (Eq storage, Show storage,Show label,Show prefix,Show opcode,Show annotation) => Show (Instruction label storage prefix opcode annotation) where
  show (Instruction label prefix opcode ops annot) = 
    show label ++ ": " ++ 
    show_prefix prefix ++
    show opcode ++ " " ++
    (intercalate ", " $ map show ops) ++
    show_annot annot
   where
    show_prefix Nothing    = ""
    show_prefix (Just pre) = show pre

    show_annot Nothing = ""
    show_annot (Just annot) = " (" ++ show annot ++ ")"

instance Show AddressWord64 where
  show (AddressWord64 a) = showHex a

instance Show Void where
  show (Void _) = "void"

