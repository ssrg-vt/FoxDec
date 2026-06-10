{-# LANGUAGE PartialTypeSignatures, DeriveGeneric, StrictData#-}

module OutputGeneration.CFG where

import Base
import Config

import Algorithm.SCC

import Binary.Generic
import InputLifting.Types
import InputLifting.ControlFlowGraph

import Data.X86.Instruction

import Data.Word
import Data.List
import Data.List.Extra (firstJust)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import GHC.Float.RealFracMethods (floorDoubleInt,int2Double)

-- | Export a CFG to .dot file
--
-- Strongly connected components get the same color.
cfg_to_dot ::
  BinaryClass bin => bin -> ControlFlowGraph -> String
cfg_to_dot bin g =
  "diGraph " ++ name ++ "{\n"
  ++ intercalate "\n" (map (node_to_dot g sccs) $ IM.keys $ cfg_basic_blocks g)
  ++ "\n\n"
  ++ intercalate "\n" (map edge_to_dot' $ IM.toList $ xgraph_all_edges $ cfg_edges g)
  ++ "\n}"
 where
  node_to_dot g sccs blockId =
    let bgcolor = hex_color_of blockId sccs
        fgcolor = hex_color_of_text bgcolor in
       "\t" 
    ++ mk_node blockId
    ++ "  ["
    ++ "style=filled fillcolor=\"" ++ bgcolor ++ "\" fontcolor=\"" ++ fgcolor ++ "\" shape=" ++ node_shape blockId ++ " "
    ++ "label=\""
    ++ show_block g blockId
    ++ "\"]"

  edge_to_dot' (blockId, blockIds) = intercalate "\n" $ map (edge_to_dot'' blockId) $ IS.toList blockIds

  edge_to_dot'' blockId blockId' = "\t" ++ mk_node blockId ++ " -> " ++ mk_node blockId'

  mk_node v = name ++ "_" ++ showHex v

  node_shape _ =  "oval" {-- TODO
  node_shape (ReturnsWith _) blockId = "oval"
  node_shape (Terminates) blockId = "terminator"
  node_shape (TimeOut) blockId = "invtriangle"
  node_shape (HasUnresolvedIndirections blockIDs) blockId 
    | blockId `elem` blockIDs = "box3d"
    | otherwise = "oval"
  node_shape (VerificationError errors) blockId 
    | blockId `elem` map fst errors = "invtriangle"
    | otherwise = "oval"--}
    
  name = "_" ++ (map repl $ binary_file_name bin)
  repl '.' = '_'
  repl c   = c
  sccs = all_sccs g IS.empty

hex_color_of vertex sccs =
  case findIndex (IS.member vertex) sccs of
    Just n -> hex_colors !! (126 - (floorDoubleInt $ 127 * int2Double n / int2Double (length sccs)))
    Nothing -> "#FFFFFF"



-- | Shows the block associated to the givern blockID.
show_block ::
  ControlFlowGraph -- ^ The CFG
  -> Int -- ^ The blockID
  -> String
show_block g b =
  let instrs = cfg_basic_blocks g IM.! b in
       showHex b ++ " ["
    ++ showHex (inAddress $ head instrs)
    ++ ","
    ++ showHex (inAddress $ last instrs)
    ++ "]"

