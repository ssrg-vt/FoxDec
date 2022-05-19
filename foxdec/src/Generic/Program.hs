module Generic.Program
    ( GenericProgram(..)
    , mapBasicBlocks
    , mapInstructions
    , foldBasicBlocks
    , foldInstructions) where

import qualified Data.IntMap.Strict as IM
import           Generic.BasicBlock (GenericBasicBlock)
import qualified Data.Graph.Dom as G
import           Data.Tuple.Extra (second)
import           Data.List (intercalate)
import           Base (showHex, showHex_set)

data GenericProgram instr =
  Program { basicBlocks :: IM.IntMap (GenericBasicBlock instr) -- ^ A mapping from blockIDs to lists of statements
          , controlFlow :: G.Rooted                            -- ^ A graph based on integers (blockIDs)
          }

mapBasicBlocks :: (GenericBasicBlock instr1 -> GenericBasicBlock instr2)
               -> GenericProgram instr1
               -> GenericProgram instr2
mapBasicBlocks f p@(Program bbs _) =
  p { basicBlocks = IM.fromList $ second f <$> IM.toList bbs }

mapInstructions
  :: (instr1 -> instr2) -> GenericProgram instr1 -> GenericProgram instr2
mapInstructions f = mapBasicBlocks (fmap f)

foldBasicBlocks
  :: (a -> GenericBasicBlock instr -> a) -> a -> GenericProgram instr -> a
foldBasicBlocks f a (Program bbs _) = foldl f a $ snd <$> IM.toList bbs

foldInstructions :: (instr -> a -> a) -> a -> GenericProgram instr -> a
foldInstructions f = foldBasicBlocks (foldr f)

instance Functor GenericProgram where
  fmap = mapInstructions

instance Foldable GenericProgram where
  foldr = foldInstructions

instance (Show instr) => Show (GenericProgram instr) where
  show (Program blocks (root, g)) = intercalate
    "\n\n"
    [ "BLOCKS:\n" ++ intercalate "\n" (showBlock <$> IM.toList blocks)
    , "ENTRY: " ++ showHex root
    , "GRAPH:\n" ++ intercalate "\n" (showEdge <$> IM.toList g)]
    where
      showBlock (label, bb) = "BLOCK " ++ show label ++ ":\n" ++ show bb

      showEdge (from, to) = showHex from ++ " --> " ++ showHex_set to
