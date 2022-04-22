module Data.Generic
    ( Statement(StmtInstruction, StmtSpecial)
    , Program(Program, programBasicBlocks, programControlFlow)
    , MutableVariable(MutableVariable, mutableVariableName)
    , ImmutableVariable
        ( ImmutableVariable
        , immutableVariableIndex
        , immutableVariableName
        )
    , Variable(ImmutableVar, MutableVar)
    , Phi(Phi, phiArguments)
    , mapP
    , mapI
    ) where

import           Base                           ( showHex
                                                , showHex_set
                                                )
import qualified Data.Graph.Dom                as G
import qualified Data.IntMap                   as IM
import           Data.List                      ( intercalate )
import           Generic_Datastructures         ( GenericAddress
                                                    ( AddressImm
                                                    , AddressMinus
                                                    , AddressPlus
                                                    , AddressStorage
                                                    , AddressTimes
                                                    )
                                                , GenericOperand
                                                    ( EffectiveAddress
                                                    , Immediate
                                                    , Memory
                                                    , Storage
                                                    )
                                                , Instruction(Instruction)
                                                )

--------------------------------------------------------------------------------
-- DATA
--------------------------------------------------------------------------------

-- | A generic statement
data Statement label storage prefix opcode annotation special =
    StmtInstruction [Instruction label storage prefix opcode annotation] -- ^ A non-empty list of normal instructions
  | StmtSpecial special

-- | A program over generic statements
data Program label storage prefix opcode annotation special = Program
    { programBasicBlocks
          :: IM.IntMap
              [Statement label storage prefix opcode annotation special]-- ^ A mapping from blockIDs to lists of statements
    , programControlFlow :: G.Rooted -- ^ A graph based on integers (blockIDs)
    }

newtype MutableVariable = MutableVariable { mutableVariableName :: String }
data ImmutableVariable = ImmutableVariable
    { immutableVariableName  :: String
    , immutableVariableIndex :: Int
    }

-- | A variable.
-- | Mutable variables can be reassigned,
-- | SSA variables should only be assigned once
data Variable = MutableVar MutableVariable | ImmutableVar ImmutableVariable

-- | A phi node with its arguments.
-- | Usually, these arguments are Variables
newtype Phi arg = Phi
    { phiArguments :: [arg]
    }

--------------------------------------------------------------------------------
-- OPERATIONS
--------------------------------------------------------------------------------

-- map two functions over a program, transforming regular instructions and special instructions
mapP
    :: (  [Instruction label storage prefix opcode annotation]
       -> [Instruction label1 storage1 prefix1 opcode1 annotation1]
       )
    -> (special -> special1)
    -> Program label storage prefix opcode annotation special
    -> Program label1 storage1 prefix1 opcode1 annotation1 special1
mapP transform_instructions transform_special (Program blocks (root, g)) =
    Program (mapP_blocks blocks) (root, g)
  where
    mapP_blocks = IM.map mapP_block
    mapP_block  = map mapP_statement
    mapP_statement (StmtInstruction is) =
        StmtInstruction $ transform_instructions is
    mapP_statement (StmtSpecial i) = StmtSpecial $ transform_special i

-- map a function over an instruction, transforming the storages
mapI
    :: (storage -> storage1)
    -> Instruction label storage prefix opcode annotation
    -> Instruction label storage1 prefix opcode annotation
mapI transform_storage (Instruction label prefix mnemonic ops annot) =
    Instruction label prefix mnemonic (map mapI_op ops) annot
  where
    mapI_op (Memory address si   ) = Memory (mapI_address address) si
    mapI_op (EffectiveAddress a  ) = EffectiveAddress $ mapI_address a
    mapI_op (Storage          r  ) = Storage $ transform_storage r
    mapI_op (Immediate        imm) = Immediate imm

    mapI_address (AddressStorage r  ) = AddressStorage $ transform_storage r
    mapI_address (AddressImm     imm) = AddressImm imm
    mapI_address (AddressMinus a0 a1) =
        AddressMinus (mapI_address a0) (mapI_address a1)
    mapI_address (AddressPlus a0 a1) =
        AddressPlus (mapI_address a0) (mapI_address a1)
    mapI_address (AddressTimes a0 a1) =
        AddressTimes (mapI_address a0) (mapI_address a1)

--------------------------------------------------------------------------------
-- TYPE CLASSES
--------------------------------------------------------------------------------

instance (Eq storage, Show storage,Show label,Show prefix,Show opcode, Show annotation,Show special) => Show (Statement label storage prefix opcode annotation special) where
    show (StmtInstruction i ) = show i
    show (StmtSpecial     sp) = show sp


instance (Eq storage, Show storage,Show label,Show prefix,Show opcode, Show annotation,Show special) => Show (Program label storage prefix opcode annotation special) where
    show (Program blocks (root, g)) = intercalate
        "\n\n"
        [ "BLOCKS:\n" ++ intercalate "\n" (map show_block $ IM.toList blocks)
        , "ENTRY: " ++ showHex root
        , "GRAPH:\n" ++ intercalate "\n" (map show_edge $ IM.toList g)
        ]
      where
        show_block (a, b) =
            "BLOCK " ++ show a ++ ":\n" ++ intercalate "\n" (map show b)
        show_edge (a, as) = showHex a ++ " --> " ++ showHex_set as
