{-# LANGUAGE TupleSections #-}

module Data.X86
    ( Label
    , Storage
    , Special
    , Instruction
    , Statement
    , Program
    , fromContext
    , canonicalize
    ) where

import           Base                           ( showHex )
import           Context                        ( CFG
                                                    ( cfg_blocks
                                                    , cfg_edges
                                                    , cfg_instrs
                                                    )
                                                , Context(ctxt_cfgs)
                                                )
import qualified Data.Generic                  as Generic
import           Data.Generic                   ( mapP )
import qualified Data.IntMap                   as IM
import qualified Data.IntSet                   as IS
import           Data.Maybe                     ( isNothing )
import           Data.Void                      ( Void )
import           Generic_Datastructures         ( AddressWord64
                                                , GenericAddress(AddressStorage)
                                                , GenericOperand
                                                    ( Immediate
                                                    , Memory
                                                    , Storage
                                                    )
                                                )
import qualified Generic_Datastructures        as GD
import           X86_Datastructures             ( Opcode
                                                    ( ADD
                                                    , IMUL
                                                    , MOV
                                                    , POP
                                                    , PUSH
                                                    , SUB
                                                    , XCHG
                                                    )
                                                , Prefix
                                                , Register(RAX, RDX, RSP)
                                                , operand_size
                                                )

--------------------------------------------------------------------------------
-- DATA
--------------------------------------------------------------------------------

type Label = AddressWord64

type Storage = Register

type Special = Void

type Instruction = GD.Instruction Label Storage Prefix Opcode Int

type Statement = Generic.Statement Label Storage Prefix Opcode Int Special

type Program = Generic.Program Label Storage Prefix Opcode Int Special -- labels are words, storage locations are registers, there are no special instructions

--------------------------------------------------------------------------------
-- PARSING
--------------------------------------------------------------------------------

-- | From a context stored in a .report file, retrieve an L0 program for a given function entry.
fromContext
    :: Context -- ^ The context
    -> Int     -- ^ The function entry of interest
    -> Program
fromContext ctxt entry = case IM.lookup entry $ ctxt_cfgs ctxt of
    Just cfg -> cfg_to_L0 cfg
    Nothing  -> error $ "Function entry " ++ showHex entry ++ " does not exist."
  where
  -- convert a CFG to L0
  -- add edges for all terminal blocks to an empty set of successors
    cfg_to_L0 cfg =
        let
            blocks =
                IM.map (map (Generic.StmtInstruction . pure)) $ cfg_instrs cfg
            edges = cfg_edges cfg
            terminals =
                filter (is_terminal_block cfg) $ IM.keys (cfg_blocks cfg)
            edges' = IM.fromList $ map (, IS.empty) terminals
        in
            Generic.Program blocks (0, IM.unionWith IS.union edges edges')
    -- if the block terminal?
    is_terminal_block cfg b = isNothing $ IM.lookup b (cfg_edges cfg)

--------------------------------------------------------------------------------
-- X86 -> X86
--------------------------------------------------------------------------------

canonicalize :: Program -> Program
canonicalize = mapP explicitize id
  where
  -- PUSH
    explicitize [GD.Instruction label prefix PUSH [op1] annot] =
        let si = operand_size op1
        in  [ GD.Instruction label
                             prefix
                             SUB
                             [Storage RSP, Immediate $ fromIntegral si]
                             annot
            , GD.Instruction label
                             prefix
                             MOV
                             [Memory (AddressStorage RSP) si, op1]
                             Nothing
            ]
    -- POP
    explicitize [GD.Instruction label prefix POP [op1] annot] =
        let si = operand_size op1
        in  [ GD.Instruction label
                             prefix
                             MOV
                             [op1, Memory (AddressStorage RSP) si]
                             Nothing
            , GD.Instruction label
                             prefix
                             ADD
                             [GD.Storage RSP, Immediate $ fromIntegral si]
                             annot
            ]
    --IMUL (1)
    explicitize [GD.Instruction label prefix IMUL [op1] annot] =
        [ GD.Instruction label
                         prefix
                         IMUL
                         [GD.Storage RDX, GD.Storage RAX, op1]
                         annot
        , GD.Instruction label
                         prefix
                         IMUL
                         [GD.Storage RAX, GD.Storage RAX, op1]
                         Nothing
        ]
    -- XCHG
    explicitize [GD.Instruction label prefix XCHG [op1, op2] annot] = -- TODO: think about this, as now the order matters
        [ GD.Instruction label prefix MOV [op1, op2] annot
        , GD.Instruction label prefix MOV [op2, op1] Nothing
        ]
    -- REMAINDER
    explicitize i = i
