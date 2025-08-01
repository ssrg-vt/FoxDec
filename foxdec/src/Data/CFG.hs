{-# LANGUAGE DeriveGeneric #-}


module Data.CFG where

import Base
import Data.X86.Instruction

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe


import qualified Data.Serialize as Cereal hiding (get,put)
import Control.DeepSeq
import GHC.Generics


-- | A control flow graph with blocks and edges.
-- A blockID (represented as an @Int@) is a unique identifier of a basic block.
-- We store basic blocks twice: once as addresses, and once as instructions.
data CFG = CFG {
  cfg_blocks :: IM.IntMap [Int],            -- ^ A mapping of blockIDs to instruction addresses
  cfg_edges  :: IM.IntMap (IS.IntSet),      -- ^ A mapping of blockIDs to sets of blocKIDs
  cfg_addr_to_blockID :: IM.IntMap Int,     -- ^ A mapping of instruction addresses to blockIDs
  cfg_fresh :: Int,                         -- ^ A fresh blockID
  cfg_instrs :: IM.IntMap [Instruction]     -- ^ A mapping of blockIDs to instructions
 }
 deriving (Show,Generic,Eq)


num_of_instructions = IM.foldr (+) 0 . IM.map length . cfg_blocks

init_cfg a = CFG { cfg_blocks = IM.singleton 0 [fromIntegral a], cfg_edges = IM.empty, cfg_addr_to_blockID = IM.singleton (fromIntegral a) 0, cfg_fresh = 1, cfg_instrs = IM.empty }



-- | Returns true if the given blockID is a leaf-node in the given CFG.
is_end_node ::
  CFG     -- ^ The CFG
  -> Int  -- ^ The blockID
  -> Bool
is_end_node cfg = IS.null . post cfg

-- | The set of next blocks from the given block in a CFG
post :: CFG -> Int -> IS.IntSet
post g blockId = fromMaybe IS.empty (IM.lookup blockId (cfg_edges g))

-- | Fetching an instruction list given a block ID
fetch_block ::
  CFG    -- ^ The CFG
  -> Int -- ^ The blockID
  -> [Instruction]
fetch_block g blockId =
  case IM.lookup blockId $ cfg_instrs $ g of
    Nothing -> error $ "Block with ID" ++ show blockId ++ " not found in cfg."
    Just b -> b


instance Cereal.Serialize CFG
instance NFData CFG



instance IntGraph CFG where
  intgraph_post    = post
  intgraph_sources = \_ -> IS.singleton 0
  intgraph_V       = IM.keysSet . cfg_blocks -- IS.unions $ IM.keysSet (cfg_edges g) : IM.elems (cfg_edges g)
   where
    post :: CFG -> IS.Key -> IS.IntSet
    post g blockId = fromMaybe IS.empty (IM.lookup blockId (cfg_edges g))
