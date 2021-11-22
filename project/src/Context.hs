{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

-----------------------------------------------------------------------------
-- |
-- A @`Context`@ stores all the information retrieved from the binary, as well as the command-line parameters passed to FoxDec.
-- Moreover, it stores information obtained during verification, such as CFGs, invariants, etc. (see @`Context`@).
-- 
-- Module "VerificationReportInterface" provides functions for obtaining and interfacing with a @Context@.
--
-----------------------------------------------------------------------------


module Context where

import Base
import DisassembleCapstone
import SimplePred
import X86_Datastructures

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.List
import Data.Word (Word8,Word64)

import Data.List (intercalate)
import Data.Maybe (mapMaybe,fromJust)

import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)



-- | A control flow graph with blocks and edges.
-- A blockID (represented as an @Int@) is a unique identifier of a basic block.
-- We store basic blocks twice: once as addresses, and once as instructions.
data CFG = CFG {
  cfg_blocks :: IM.IntMap [Int],        -- ^ A mapping of blockIDs to instruction addresses
  cfg_edges  :: IM.IntMap (IS.IntSet),  -- ^ A mapping of blockIDs to sets of blocKIDs
  cfg_addr_to_blockID :: IM.IntMap Int, -- ^ A mapping of instruction addresses to blockIDs
  cfg_fresh :: Int,                     -- ^ A fresh blockID
  cfg_instrs :: IM.IntMap [Instr]       -- ^ A mapping of blockIDs to lists of disassembled instructions.
 }
 deriving (Show,Generic)

-- | Per instruction address, a set of jump targets.
type Indirections = IM.IntMap IS.IntSet

-- | Sections: segment names, section names, addresses and sizes. 
type SectionsInfo = [(String,String,Int,Int)]

-- | An enumeration indicating the result of verification over a function
data VerificationResult = 
     VerificationSuccess              -- ^ Function was succesfully verified
  | VerificationSuccesWithAssertions  -- ^ Function was succesfully verified, but required assertions
  | VerificationUnresolvedIndirection -- ^ Function contains an unresolved indirection
  | VerificationError                 -- ^ There was some verification error, e.g., return adresss overwrite
  | Unverified                        -- ^ The function has not been verified.
  deriving (Show, Eq, Generic)

-- | Invariants: a mapping of blockIDs to predicates
type Invariants = IM.IntMap Pred




-- | For each leaf-node in a CFG we store the following info.
data NodeInfo = 
    Normal                -- ^ The basic block behaves normally, e.g., a ret
  | UnresolvedIndirection -- ^ The basic block ends in an unresolved indirection
  | Terminal              -- ^ The basic blocks ends with, e.g., a call to exit()
 deriving (Show,Generic,Eq,Ord)


-- | Per function, we report on:
data Report = Report {
   report_cfg     :: CFG,                        -- ^ The control flow graph
   report_invs    :: Invariants,                 -- ^ The invariants
   report_posts   :: S.Set (NodeInfo,Pred),      -- ^ The postcondition(s)
   report_result  :: VerificationResult,         -- ^ The verification result
   report_vcs     :: S.Set VerificationCondition -- ^ The verification conditions
 }
 deriving Generic

-----------------------------------------------------------------------------
-- |
-- The context datastructure.
--
-- __S__: Information __S__tatically obtained by reading from the binary
--
-- __D__: Information __D__ynamically updated during verification
-------------------------------------------------------------------------------
data Context = Context {
   ctxt_dump          :: IM.IntMap Word8,   -- ^ __S__: mapping from addresses to bytes (data and instructions from the binary/executable)
   ctxt_syms          :: IM.IntMap String,  -- ^ __S__: the symbol table: a mapping of addresses to function names for external functions
   ctxt_calls         :: IM.IntMap Bool,    -- ^ __D__: the currently known and verified entry addresses of functions (true iff always terminating, i.e., non-returning)
   ctxt_entries       :: Graph,             -- ^ __D__: a graph with an edge (e0,e1) if entry address e0 calls entry address e1, and e0 and e1 have not been verified yet
   ctxt_sections      :: SectionsInfo,      -- ^ __S__: information on segments/section
   ctxt_dirname       :: String,            -- ^ __S__: the name of the directory where the .dump, .entry, .sections and .symbols files reside
   ctxt_name          :: String,            -- ^ __S__: the name of the binary
   ctxt_generate_pdfs :: Bool,              -- ^ __S__: do we call graphviz to generate PDFs from .dot files?
   ctxt_inds          :: Indirections,      -- ^ __D__: the currently known indirections
   ctxt_report        :: IM.IntMap Report   -- ^ __D__: a mapping from function entries to reports storing verification results
 }
 deriving Generic


instance Cereal.Serialize NodeInfo
instance Cereal.Serialize VerificationResult
instance Cereal.Serialize CFG
instance Cereal.Serialize Report
instance Cereal.Serialize Context







-- | Reading from a data section.
--
-- Reads maximally up to 8 bytes. Returns @Nothing@ if the given address is out-of-range.
read_from_datasection :: 
  Context          -- ^ The context 
  -> Word64        -- ^ An address
  -> Int           -- ^ Size, i.e., the number of bytes to read
  -> Maybe Word64
read_from_datasection ctxt a si =
  let ds    = ctxt_dump ctxt
      bytes = map (\a ->  IM.lookup a ds) [fromIntegral a..(fromIntegral a)+si-1] in
    if si <= 8 && not (Nothing `elem` bytes) then -- TODO 256 bits
      Just $ bytes_to_word $ map fromJust bytes
    else
      Nothing


-- | Find a section for an address (see @`SectionsInfo`@)
find_section_for_address ::
   Context                              -- ^ The context
   -> Int                               -- ^ An address
   -> Maybe (String, String, Int, Int)
find_section_for_address ctxt a = find (address_in_section a) (ctxt_sections ctxt)
 where
  address_in_section a (_,_,a0,si) = a0 <= a && a < a0 + si



-- | Fetching an instruction
--
-- Returns @Nothing@ if the given address is out-of-range.
fetch_instruction :: 
  Context              -- ^ The context
  -> Int               -- ^ An address
  -> IO (Maybe Instr)
fetch_instruction ctxt a = do
  let dump = ctxt_dump ctxt
  disassemble dump a


-- | Pretty printing an instruction
pp_instruction ::
  Context   -- ^ The context
  -> Instr  -- ^ An instruction
  -> String
pp_instruction ctxt i =
  if is_call (i_opcode i) then
    show i ++
      case i_op1 i of
        Just (Immediate imm) -> 
          case IM.lookup (fromIntegral imm) $ ctxt_syms ctxt of
            Nothing  -> " (0x" ++ showHex imm ++ ")"
            Just sym -> " (" ++ (show sym) ++ ")"
        _ -> ""
  else
    show i



