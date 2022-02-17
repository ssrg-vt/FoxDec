{-# LANGUAGE DeriveGeneric, DefaultSignatures, StrictData #-}

{-|
Module      : CFG_Gen
Description : A @"Context"@ stores all the information retrieved from the binary, as well as the command-line parameters passed to FoxDec.

The context stores, among others, information obtained during verification, such as CFGs, invariants, etc. (see @`Context`@).
Module "VerificationReportInterface" provides functions for obtaining and interfacing with a @Context@.
-}


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
 deriving (Show,Generic,Eq)


-- | Per instruction address, a set of jump targets.
type Indirections = IM.IntMap IS.IntSet

-- | Sections: segment names, section names, addresses and sizes. 
type SectionsInfo = [(String,String,Int,Int)]

-- | An enumeration indicating the result of verification over a function
data VerificationResult = 
     VerificationSuccess              -- ^ Function was succesfully verified
  | VerificationSuccesWithAssumptions -- ^ Function was succesfully verified, but required assertions
  | VerificationUnresolvedIndirection -- ^ Function contains an unresolved indirection
  | VerificationError String          -- ^ There was some verification error, e.g., return adresss overwrite
  | Unverified                        -- ^ The function has not been verified.
  deriving (Eq, Generic)


-- | Invariants: a mapping of blockIDs to predicates
type Invariants = IM.IntMap Pred

-- | For each leaf-node in a CFG we store the following info.
data NodeInfo = 
    Normal                -- ^ The basic block behaves normally, e.g., a ret
  | UnresolvedIndirection -- ^ The basic block ends in an unresolved indirection
  | Terminal              -- ^ The basic blocks ends with, e.g., a call to exit()
 deriving (Show,Generic,Eq,Ord)

-- | Postconditions: for each final block the @NodeInfo@ and the final predicate after execution of the block
type Postconditions = S.Set (NodeInfo,Pred)


-- | Identifies where a memwrite occurred
-- TODO should be generalized, is a StatePartWriteIdentifier
data MemWriteIdentifier =
   MemWriteFunction String Int StatePart      -- ^ A function with @name@ at address @i_a@ wrote to a statepart
 | MemWriteInstruction Int Operand SimpleExpr -- ^ An instruction wrote to an operand, resolving to an address
  deriving (Generic,Eq,Ord)

-- |  A verification condition is either:
-- * A precondition of the form:
-- >    Precondition (a0,si0) (a1,si1)
-- This formulates that at the initial state the two regions must be separate.
-- * An assertion of the form:
-- >    Assertion a (a0,si0) (a1,si1)
-- This formulates that dynamically, whenever address a is executed, the two regions are asserted to be separate.
-- * A function constraint of the form:
-- >    FunctionConstraint foo [(RDI, v0), (RSI, v1), ...]   { sp0,sp1,... }
-- This formulates that a function call to function foo with values v0, v1, ... stored in the registers should not overwrite certain state parts.
data VerificationCondition =
    Precondition          SimpleExpr Int SimpleExpr Int                            -- ^ Precondition:           lhs SEP rhs
  | Assertion             SimpleExpr SimpleExpr Int SimpleExpr Int                 -- ^ Assertion:    @address, lhs SEP rhs
  | FunctionConstraint    String     Int [(Register,SimpleExpr)] (S.Set StatePart) -- ^ Function name, address, of call, with param registers
  | SourcelessMemWrite    MemWriteIdentifier                                       -- ^ A write to a statepart for which no information was available
  | IntroFunctionPointer  Int MemWriteIdentifier                                   -- ^ A function pointer was written to a state part 
  deriving (Generic,Eq,Ord)

-- | An acornym for a set of verification conditions
type VCS = S.Set VerificationCondition


-- | A function initialisation consists of a mapping of stateparts to expressions.
type FInit = M.Map StatePart SimpleExpr

-- | A function call 
data FReturnBehavior = 
    Terminating              -- ^ The function does never return
  | ReturningWith Pred       -- ^ The function returns withg the symbolic changes stored in the predicate
  | UnknownRetBehavior       -- ^  It is unknown whether the function returns or not
 deriving (Show,Generic,Eq,Ord)


-----------------------------------------------------------------------------
-- |
-- The context datastructure.
--
-- __S__: Information __S__tatically obtained by reading from the binary
--
-- __D__: Information __D__ynamically updated during verification
-------------------------------------------------------------------------------
data Context = Context {
   ctxt_dump          :: IM.IntMap Word8,                -- ^ __S__: mapping from addresses to bytes (data and instructions from the binary/executable)
   ctxt_syms          :: IM.IntMap String,               -- ^ __S__: the symbol table: a mapping of addresses to function names for external functions
   ctxt_sections      :: SectionsInfo,                   -- ^ __S__: information on segments/section
   ctxt_dirname       :: String,                         -- ^ __S__: the name of the directory where the .dump, .entry, .sections and .symbols files reside
   ctxt_name          :: String,                         -- ^ __S__: the name of the binary
   ctxt_generate_pdfs :: Bool,                           -- ^ __S__: do we call graphviz to generate PDFs from .dot files?

   ctxt_entries       :: Graph,                          -- ^ __D__: a graph with an edge (e0,e1) if entry address e0 calls entry address e1, and e0 and e1 have not been verified yet
   ctxt_cfgs          :: IM.IntMap CFG,                  -- ^ __D__: the currently known control flow graphs per function entry
   ctxt_calls         :: IM.IntMap FReturnBehavior,      -- ^ __D__: the currently known and verified entry addresses of functions mapped to return-information
   ctxt_invs          :: IM.IntMap Invariants,           -- ^ __D__: the currently known invariants
   ctxt_posts         :: IM.IntMap Postconditions,       -- ^ __D__: the currently known postconditions
   ctxt_inds          :: Indirections,                   -- ^ __D__: the currently known indirections
   ctxt_finits        :: IM.IntMap FInit,                -- ^ __D__: the currently known function initialisations
   ctxt_vcs           :: IM.IntMap VCS,                  -- ^ __D__: the verification conditions
   ctxt_results       :: IM.IntMap VerificationResult,   -- ^ __D__: the verification result
   ctxt_recursions    :: IM.IntMap IS.IntSet
 }
 deriving Generic


instance Cereal.Serialize NodeInfo
instance Cereal.Serialize VerificationResult
instance Cereal.Serialize CFG
instance Cereal.Serialize FReturnBehavior
instance Cereal.Serialize MemWriteIdentifier
instance Cereal.Serialize VerificationCondition
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


-- | Show function initialisation
show_finit :: FInit -> String
show_finit finit = intercalate ", " $ (map (\(sp,e) -> show sp ++ " ~= " ++ show e) $ M.toList finit)





instance Show VerificationResult where
 show VerificationSuccess               = "VerificationSuccess"
 show VerificationSuccesWithAssumptions = "VerificationSuccesWithAssumptions"
 show VerificationUnresolvedIndirection = "VerificationUnresolvedIndirection"
 show (VerificationError msg)           = "VerificationError: " ++ msg


instance Show MemWriteIdentifier where
  show (MemWriteFunction f a sp)       = f ++ "@" ++ showHex a ++ " WRITES TO " ++ show sp
  show (MemWriteInstruction a operand a') = "@" ++ showHex a ++ " WRITES TO " ++ show operand ++ " == " ++ show a'



instance Show VerificationCondition where
  show (Precondition lhs _ rhs _)      = show lhs ++ " SEP " ++ show rhs
  show (Assertion a  lhs _ rhs _)      = "@" ++ show a ++ ": " ++ show lhs ++ " SEP " ++ show rhs
  show (SourcelessMemWrite mid)        = "UNKNOWN WRITE: " ++ show mid
  show (IntroFunctionPointer a mid)    = "Function pointer " ++ showHex a ++ "(" ++ show mid ++ ")"
  show (FunctionConstraint f a ps sps) = f ++ "@" ++ showHex a ++ "(" ++ intercalate "," (map show_param ps) ++ ") PRESERVES " ++ (intercalate "," (map show $ S.toList sps))
   where
    show_param (r,e) = show r ++ ":=" ++ strip_parentheses (show e)

-- | Is the given verification condition an assertion?
is_assertion (Assertion _ _ _ _ _) = True
is_assertion _                     = False

-- | Is the given verification condition a precondition?
is_precondition (Precondition _ _ _ _) = True
is_precondition _                      = False

-- | Is the given verification condition a function constraint?
is_func_constraint (FunctionConstraint _ _ _ _) = True
is_func_constraint _                            = False

-- | Is the given verification condition a sourceless memwrite?
is_sourceless_memwrite (SourcelessMemWrite _) = True
is_sourceless_memwrite _                      = False

-- | Is the given verification condition a function pointer introduction?
is_introfunctionpointer (IntroFunctionPointer _ _) = True
is_introfunctionpointer _                          = False

-- | Count the number of assertions in the set of verification conditions.
count_instructions_with_assertions = S.size . S.map (\(Assertion rip _ _ _ _) -> rip) . S.filter is_assertion


-- | Count the number of sourceless memory writes in the set of verification conditions.
count_sourceless_memwrites = S.size . S.map (\(SourcelessMemWrite mid) -> prune mid) . S.filter is_sourceless_memwrite
 where
  prune mid@(MemWriteFunction f i_a sp)               = mid
  prune mid@(MemWriteInstruction i_a addr a_resolved) = MemWriteInstruction i_a addr $ SE_Immediate 0



