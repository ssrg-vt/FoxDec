{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, DeriveGeneric, DefaultSignatures, FlexibleContexts, Strict #-}


{-# OPTIONS_HADDOCK hide #-}


module OutputGeneration.LRToCallGraph where

import Base

import Binary.Generic

import InputLifting.ControlFlowGraph
import InputLifting.NextRips
import InputLifting.Types

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set.NonEmpty as NES
import Data.List
import Data.List.Split (chunksOf)
import Data.List.Extra (groupSort)
import Data.Maybe (fromJust)
import Debug.Trace

import Control.Monad.State.Strict
import Control.Monad.Extra
import Control.Monad.Reader

get_call_graph_sources :: BinaryClass bin => XLifted bin IS.IntSet
get_call_graph_sources = do
  (bin,config,lr,funcs) <- ask
  srcs <- filterM is_callgraph_source $ IM.keys funcs
  return $ IS.fromList srcs


is_callgraph_source entry = do
  prev <- withLR $ get_prev_collapsed_calls (FunctionEntry entry) (InstructionAddress entry)
  return $ S.null prev

