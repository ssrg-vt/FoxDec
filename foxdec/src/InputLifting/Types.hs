{-# LANGUAGE DeriveGeneric, StrictData #-}

module InputLifting.Types where

import Base
import Config

import Data.X86.Instruction
import Binary.Generic




import Data.Word
import Data.List
import Data.List.Extra (firstJust)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Extra
import GHC.Generics

import qualified Data.Serialize as Cereal
import Control.DeepSeq

-- A relation of pairs of Ints
type IntRelation = IM.IntMap IS.IntSet

int_rel_add_pair i0 i1 = IM.insertWith IS.union i0 (IS.singleton i1)

int_rel_lookup :: Int -> IntRelation -> IS.IntSet
int_rel_lookup = IM.findWithDefault mempty



-- The types FunctionEntry and InstructionAddress
-- These are just Ints, but we use these datatypes to make sure that the type system captures confusion between the two
data FunctionEntry = FunctionEntry Int
  deriving (Eq, Ord, Generic)

instance Show FunctionEntry where
  show (FunctionEntry a) = "0x" ++ showHex a

class Intable a where 
  toInt :: a -> Int

instance Intable FunctionEntry where
  toInt (FunctionEntry a) = a

data InstructionAddress = InstructionAddress Int
  deriving (Eq, Ord, Generic)

instance Show InstructionAddress where
  show (InstructionAddress a) = "0x" ++ showHex a

instance Intable InstructionAddress where
  toInt (InstructionAddress a) = a


cast_entry_to_address (FunctionEntry a) = InstructionAddress a
cast_address_to_entry (InstructionAddress a) = FunctionEntry a


data IndirectionResolving =
    ResolvedJumpTable Word64 Word64
  | ResolvedJump Next
  | ResolvedCall Next
  | ResolvedCallToError Bool
  | UnresolvedJumpTable String
  | UnresolvedCallToError
  | UnresolvedJump
  | UnresolvedCall
  deriving (Eq,Generic)

instance Show IndirectionResolving where
  show (ResolvedJumpTable base bound) = "resolved jump table (base = 0x" ++ showHex base ++ ", bound = 0x" ++ showHex bound ++ ")"
  show (ResolvedJump next) = "resolved jump (target = " ++ show next ++ ")"
  show (ResolvedCall next) = "resolved call (target = " ++ show next ++ ")"
  show (ResolvedCallToError does_return) = "resolved error(); (" ++ (if does_return then "returns" else "terminates") ++ ")"
  show (UnresolvedJumpTable errmsg) = "unresolved jumptable"
  show (UnresolvedCallToError) = "unresolved error();"
  show (UnresolvedJump) = "unresolved jump"
  show (UnresolvedCall) = "unresolved call"

data Next = 
    NxtTerminal (Maybe String) -- optional: calling a function that terminates
  | NxtAddresses (Maybe String) (S.Set InstructionAddress) -- optional: a called external function, then the next address(es)
  | NxtSyscall
  | NxtReturn (Maybe String) -- optional: calling a function before returning
  | NxtInternalCall InstructionAddress -- target
  deriving (Eq, Ord, Generic)

instance Show Next where
  show (NxtTerminal Nothing)      = "(terminates)"
  show (NxtTerminal (Just f))     = "(terminate by calling " ++ f ++ ")"
  show (NxtAddresses Nothing as)  = show (S.toList as)
  show (NxtAddresses (Just f) as) = f ++  " returning to " ++ show (S.toList as)
  show (NxtReturn Nothing)        = "(returns)"
  show (NxtReturn (Just f))       = "(returns after " ++ f ++ ")"
  show (NxtInternalCall trgt)     = show trgt ++ " (call)"


-- The algorithm keeps track of the following information:
data LiftedRepresentationUnstructured = LiftedRepresentationUnstructured {
    current_instrs        :: IM.IntMap Instruction -- ^ Memoization of disassembled instructions: mapping instruction addresses to instructions
  , current_nexts         :: IM.IntMap Next -- ^ Memoization of 'next_rips' function
  , current_cfg           :: XGraph -- ^ All currently known control flow transfers
  , current_inlining      :: XGraph -- ^ An edges (f0,f1) iff f0 is inlined in f1
  , current_bag           :: S.Set (Maybe InstructionAddress,InstructionAddress) -- ^ A bag of edges to be explored (parent,child to be explored)
  , current_fmap          :: IM.IntMap FunctionEntry -- ^ Mapping from instruction addresses to function entries
  , current_callers       :: IntRelation -- ^ Mapping from function entries to their callers (addresses of CALLs)
  , current_returns       :: IntRelation -- ^ Mapping from function entries to RET instructions (addresses)
  , current_indirections  :: IM.IntMap IndirectionResolving
  , current_real_entries  :: S.Set (FunctionEntry)-- ^ Set of entry points of likely real actual functions (not snippets)
  , current_covered       :: IM.IntMap Int
 }


init_lr = LiftedRepresentationUnstructured mempty mempty (XEdges mempty mempty) (XEdges mempty mempty) mempty mempty mempty mempty mempty mempty mempty

-- The algorithm does IO, reads from the given binary and config file, and maintains as state the information in the struct above.
type XLifting bin = StateT LiftedRepresentationUnstructured (ReaderT (bin,Config) IO)

-- After lifting, the algorithm does IO, reads from the lifted representation, the egenrated CFGs, the original binary and config file
type XLifted bin = ReaderT (bin,Config,LiftedRepresentationUnstructured,IM.IntMap ControlFlowGraph) IO

withLR :: XLifting bin a -> XLifted bin a
withLR m = do
  (bin,config,lr,cfgs) <- ask
  lift $ runReaderT (evalStateT m lr) (bin,config)




xtoLog msg = liftIO $ putStrLn msg
xDebug 0 msg = return () -- liftIO $ putStrLn msg
xDebug 1 msg = return () -- liftIO $ putStrLn msg


-- Registering updates to rthe current LiftedRepresentationUnstructured
register_add_to_bag :: Maybe InstructionAddress -> InstructionAddress -> LiftedRepresentationUnstructured -> LiftedRepresentationUnstructured
register_add_to_bag a0 a1 lr = lr { current_bag = S.insert (a0,a1) $ current_bag lr }

register_caller :: FunctionEntry -> InstructionAddress -> XLifting bin ()
register_caller entry caller = modify $ \lr -> lr { current_callers = int_rel_add_pair (toInt entry) (toInt caller) (current_callers lr) } 

register_return :: FunctionEntry -> InstructionAddress -> LiftedRepresentationUnstructured -> LiftedRepresentationUnstructured
register_return entry a lr = lr { current_returns = int_rel_add_pair (toInt entry) (toInt a) (current_returns lr) }

register_current_instrs :: Instruction -> InstructionAddress -> LiftedRepresentationUnstructured -> LiftedRepresentationUnstructured
register_current_instrs i a lr = lr { current_instrs = IM.insert (toInt a) i (current_instrs lr) } 

register_entry_for_instruction :: FunctionEntry -> InstructionAddress -> LiftedRepresentationUnstructured -> LiftedRepresentationUnstructured
register_entry_for_instruction entry a lr = lr { current_fmap = IM.insert (toInt a) entry $ current_fmap lr }

register_edge :: InstructionAddress -> InstructionAddress -> LiftedRepresentationUnstructured -> LiftedRepresentationUnstructured
register_edge a a' lr = lr { current_cfg = xgraph_add_edge (current_cfg lr) (toInt a) (toInt a') }

register_next_rips :: InstructionAddress -> Next -> LiftedRepresentationUnstructured -> LiftedRepresentationUnstructured
register_next_rips a nxt lr = lr { current_nexts = IM.insert (toInt a) nxt (current_nexts lr)  }

register_inlining :: FunctionEntry -> FunctionEntry -> LiftedRepresentationUnstructured -> LiftedRepresentationUnstructured
register_inlining a0 a1 lr = lr { current_inlining = xgraph_add_edge (current_inlining lr)  (toInt a0) (toInt a1)  }

register_real_entries :: S.Set (FunctionEntry) -> LiftedRepresentationUnstructured -> LiftedRepresentationUnstructured
register_real_entries entries lr = lr { current_real_entries = S.union (current_real_entries lr) entries }

register_cover :: Int -> Int -> LiftedRepresentationUnstructured -> LiftedRepresentationUnstructured
register_cover a k lr = lr { current_covered = IM.insert a k (current_covered lr) }

register_resolving :: InstructionAddress -> IndirectionResolving -> LiftedRepresentationUnstructured -> LiftedRepresentationUnstructured
register_resolving a ind lr = lr { current_indirections = IM.insert (toInt a) ind (current_indirections lr) }

-- Reading from the current LiftedRepresentationUnstructured
get_entry_of_instruction :: InstructionAddress -> XLifting bin (Maybe FunctionEntry)
get_entry_of_instruction a = gets (IM.lookup (toInt a) . current_fmap)

get_callers :: FunctionEntry -> XLifting bin (S.Set InstructionAddress)
get_callers entry = gets (S.fromList . map InstructionAddress . IS.toList . int_rel_lookup (toInt entry) . current_callers)

get_returns :: FunctionEntry -> XLifting bin (S.Set InstructionAddress)
get_returns entry = gets (S.fromList . map InstructionAddress . IS.toList . int_rel_lookup (toInt entry) . current_returns)

get_next_rips :: InstructionAddress  -> XLifting bin (Maybe Next)
get_next_rips a = gets (IM.lookup (toInt a) . current_nexts)

get_inlining :: FunctionEntry  -> XLifting bin (S.Set FunctionEntry)
get_inlining a = do
  inlining <- gets current_inlining
  return $ S.map FunctionEntry $ S.fromList $ IS.toList $ xgraph_children inlining $ toInt a

get_is_real_entry :: FunctionEntry -> XLifting bin Bool
get_is_real_entry entry = gets (S.member entry . current_real_entries)




-- Fetch an instruction, memoizing the disassembled instruction in the current_instrs map
fetch :: BinaryClass bin => InstructionAddress -> XLifting bin (Maybe Instruction)
fetch a = do
  (bin,config) <- ask
  instrs       <- gets current_instrs
  case IM.lookup (toInt a) instrs of
    Just i  -> return $ Just i
    Nothing -> do
      mi <- liftIO $ fetch_instruction bin $ fromIntegral $ toInt a
      case mi of
        Nothing -> return Nothing
        Just i  -> modify (register_current_instrs i a) >> return (Just i)
          



type Blocks  = IM.IntMap [Instruction]
type Sources = IS.IntSet       -- ^ Set of instruction addresses that start the CFG (entry point and landing pads)
type Leaks   = IS.IntSet       -- ^ Set of instruction that leak (i.e., normal control flow to other function)

data ControlFlowGraph = ControlFlowGraph {
    cfg_basic_blocks :: Blocks
  , cfg_edges :: XGraph
  , cfg_sources :: Sources
  , cfg_leaks :: Leaks 
  , cfg_components :: S.Set FunctionEntry
 }
  deriving (Generic)


-- The algorithm keeps track of the following information:
data LiftedRepresentationFunctions bin = LiftedRepresentationFunctions {
    lrf_binary       :: bin
  , lrf_config       :: Config
  , lrf_instrs       :: IM.IntMap Instruction -- ^ Memoization of disassembled instructions: mapping instruction addresses to instructions
  , lrf_nexts        :: IM.IntMap Next        -- ^ Memoization of 'next_rips' function
  , lrf_cfgs         :: IM.IntMap ControlFlowGraph -- ^ A mapping from function entries to CFGs
  , lrf_indirections :: IM.IntMap IndirectionResolving
 }


instance NFData InstructionAddress
instance NFData FunctionEntry
instance NFData XGraph
instance NFData Next
instance NFData IndirectionResolving
instance NFData ControlFlowGraph

instance Cereal.Serialize InstructionAddress
instance Cereal.Serialize FunctionEntry
instance Cereal.Serialize XGraph
instance Cereal.Serialize Next
instance Cereal.Serialize IndirectionResolving
instance Cereal.Serialize ControlFlowGraph

instance BinaryClass bin => Cereal.Serialize (LiftedRepresentationFunctions bin) where
  put (LiftedRepresentationFunctions bin config instrs nexts cfgs comments) = do
    Cereal.put config
    Cereal.put instrs
    Cereal.put nexts
    Cereal.put cfgs
    Cereal.put comments

  get = do
    config <- Cereal.get
    instrs <- Cereal.get
    nexts  <- Cereal.get
    cfgs   <- Cereal.get
    inds   <- Cereal.get
    return $ LiftedRepresentationFunctions binary_null config instrs nexts cfgs inds


data XGraph = XEdges (IM.IntMap IS.IntSet) (IM.IntMap IS.IntSet)
  deriving (Generic,Show)

xgraph_empty = XEdges IM.empty IM.empty

xgraph_fw_edges (XEdges fw bw) = fw

xgraph_is_parent (XEdges fw bw) a = IM.member a fw

xgraph_add_edge (XEdges fw bw) a0 a1 = XEdges (IM.insertWith IS.union a0 (IS.singleton a1) fw) (IM.insertWith IS.union a1 (IS.singleton a0) bw)

xgraph_add_edges (XEdges fw bw) a0 a1s = XEdges (IM.insertWith IS.union a0 a1s fw) (IS.foldr (\a1 -> IM.insertWith IS.union a1 (IS.singleton a0)) bw a1s)

xgraph_delete_node a (XEdges fw bw) = XEdges fw' bw'
 where 
  fw' = IM.map (IS.delete a) $ IM.delete a fw
  bw' = IM.map (IS.delete a) $ IM.delete a bw

xgraph_delete_edges g a0 = IS.foldr (\a1 g -> xgraph_delete_edge g a0 a1) g (xgraph_children g a0)

xgraph_delete_edge (XEdges fw bw) a0 a1 = XEdges (IM.adjust (IS.delete a1) a0 fw) (IM.adjust (IS.delete a0) a1 bw)

xgraph_add_vertex (XEdges fw bw) a = XEdges (IM.insertWith IS.union a IS.empty fw) (IM.insertWith IS.union a IS.empty bw)

xgraph_all_vertices (XEdges fw bw) = IS.union (IM.keysSet fw) (IM.keysSet bw) 

xgraph_is_edge  (XEdges fw bw) a0 a1 =
  case IM.lookup a0 fw of
    Nothing -> False
    Just as -> IS.member a1 as


xgraph_all_edges :: XGraph -> IM.IntMap IS.IntSet
xgraph_all_edges (XEdges fw bw) = fw

xgraph_all_parents :: XGraph -> IS.IntSet
xgraph_all_parents (XEdges fw bw) = IM.keysSet fw

xgraph_parents :: XGraph -> Int -> IS.IntSet
xgraph_parents (XEdges fw bw) v = IM.lookup v bw `orElse` IS.empty

xgraph_children :: XGraph -> Int -> IS.IntSet
xgraph_children (XEdges fw bw) v = IM.lookup v fw `orElse` IS.empty

-- TODO 
xgraph_is_reachable :: XGraph -> Int -> IS.IntSet
xgraph_is_reachable g a = dfs IS.empty $ IS.singleton a
 where
  dfs visited frontier
    | IS.null frontier = visited
    | otherwise = 
      let visited'   = visited `IS.union` frontier
          neighbors  = IS.unions $ map (xgraph_children g) $ IS.toList frontier
          frontier'  = neighbors `IS.difference` visited' in
        dfs visited' frontier'

xgraph_is_reachable_bidrectional :: XGraph -> Int -> IS.IntSet
xgraph_is_reachable_bidrectional g a = dfs IS.empty $ IS.singleton a
 where
  dfs visited frontier
    | IS.null frontier = visited
    | otherwise = 
      let visited'   = visited `IS.union` frontier
          neighbors  = IS.unions $ map get_neighbor $ IS.toList frontier
          frontier'  = neighbors `IS.difference` visited' in
        dfs visited' frontier'

  get_neighbor v = IS.union (xgraph_children g v) (xgraph_parents g v)

xgraph_is_source g a = IS.null $ xgraph_parents g a 

xgraph_all_sources g = IS.filter (xgraph_is_source g) $ xgraph_all_vertices g 

xgraph_weak_components :: XGraph -> [IS.IntSet]
xgraph_weak_components g = go (xgraph_all_vertices g) []
  where
    go remaining acc
      | IS.null remaining = acc
      | otherwise =
          let v = IS.findMin remaining
              comp = xgraph_is_reachable_bidrectional g v
              remaining' = remaining `IS.difference` comp
          in go remaining' (comp : acc)





