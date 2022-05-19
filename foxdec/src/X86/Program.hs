{-# LANGUAGE TupleSections #-}

module X86.Program (Program(..)) where

import           Base (showHex)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import           Data.Maybe (isNothing)
import           Data.ControlFlow (isTerminal)
import           Analysis.Context (Context(..), CFG(..))
import           Generic.Program (GenericProgram(..))
import           Generic.BasicBlock (GenericBasicBlock(..))
import qualified Generic.Program as Prog
import qualified X86.Instruction as Instr
import qualified X86.Instruction as X86

type Program = GenericProgram X86.Instruction

-- | From a context stored in a .report file, retrieve an X86 program for a given function entry.
fromContext :: Context -- ^ The context
            -> Int     -- ^ The function entry of interest
            -> Program
fromContext ctxt entry = case IM.lookup entry $ ctxt_cfgs ctxt of
  Just cfg -> cfgToX86 cfg
  Nothing  -> error $ "Function entry " ++ showHex entry ++ " does not exist."
  where
    -- add edges for all terminal blocks to an empty set of successors
    cfgToX86 cfg =
      let blocks = BasicBlock <$> cfg_instrs cfg
          edges = cfg_edges cfg
          terminals = filter (isTerminal cfg) $ IM.keys (cfg_blocks cfg)
          edges' = IM.fromList $ (, IS.empty) <$> terminals
      in Program blocks (0, IM.unionWith IS.union edges edges')

canonicalize :: Program -> Program
canonicalize = Prog.mapBasicBlocks (>>= BasicBlock <$> Instr.canonicalize)