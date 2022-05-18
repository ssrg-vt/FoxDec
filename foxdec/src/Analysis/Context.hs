{-# LANGUAGE DeriveGeneric, DefaultSignatures, Strict #-}

{-|
Module      : CFG_Gen
Description : A @"Context"@ stores all the information retrieved from the binary, as well as the command-line parameters passed to FoxDec.

The context stores, among others, information obtained during verification, such as CFGs, invariants, etc. (see @`Context`@).
Module "VerificationReportInterface" provides functions for obtaining and interfacing with a @Context@.
-}


module Analysis.Context where

import Base
import Config
import Pass.DisassembleCapstone
import Data.SimplePred
import Generic_Datastructures
import X86_Datastructures

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List ( find, intercalate )
import Data.Word (Word8,Word64)
import Data.Maybe (mapMaybe,fromJust)


import GHC.Generics
import GHC.Natural (naturalToInteger)
import qualified Data.Serialize as Cereal hiding (get,put)



-- | A control flow graph with blocks and edges.
-- A blockID (represented as an @Int@) is a unique identifier of a basic block.
-- We store basic blocks twice: once as addresses, and once as instructions.
data CFG = CFG {
  cfg_blocks :: IM.IntMap [Int],            -- ^ A mapping of blockIDs to instruction addresses
  cfg_edges  :: IM.IntMap (IS.IntSet),      -- ^ A mapping of blockIDs to sets of blocKIDs
  cfg_addr_to_blockID :: IM.IntMap Int,     -- ^ A mapping of instruction addresses to blockIDs
  cfg_fresh :: Int,                         -- ^ A fresh blockID
  cfg_instrs :: IM.IntMap [X86_Instruction] -- ^ A mapping of blockIDs to lists of disassembled instructions.
 }
 deriving (Show,Generic,Eq)



-- | A jump table
-- Let the actual indirection be @JMP RAX@, implying jt_trgt_operand == RAX.
-- This jump table can be implemented with inserting this instruction before the indirection:
--
--    MOV jt_trgt_operand, QWORD PTR [jt_address + 8*jt_index_operand]
data JumpTable = JumpTable {
  jt_index_operand   :: X86_Operand, -- ^ The operand that is bounded by some immediate, serving as an index into a table
  jt_trgt_operand    :: X86_Operand, -- ^ The operand of the jump
  jt_table_entries   :: [Int]        -- ^ An ordered list of instruction addresses to which is jumped
 }
 deriving (Show,Generic,Eq)

-- | Resolving the operand of a jump/call can produce one of the following.
data ResolvedJumpTarget =
   Unresolved               -- ^ An indirect branch that has not been resolved yet
 | External String          -- ^ A call to external function f
 | ImmediateAddress Word64  -- ^ An internal call to the given address
 deriving (Eq,Show,Generic,Ord)

-- | An indirection
data Indirection =
    IndirectionResolved (S.Set ResolvedJumpTarget) -- ^ An indirection that could be resolved to one or more addresses
  | IndirectionJumpTable JumpTable                 -- ^ An indirection based on a jump table
 deriving (Show,Generic,Eq)

-- | Per instruction address, a a jump table
type Indirections = IM.IntMap Indirection

-- |  Information on the sections in the binary
data SectionsInfo = SectionsInfo {
  si_sections    :: [(String,String,Int,Int)], -- ^ Sections: segment names, section names, addresses and sizes.
  si_min_address :: Int,
  si_max_address :: Int
 }
 deriving (Show,Generic,Eq)


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
   MemWriteFunction String Word64 StatePart          -- ^ A function with @name@ at address @i_a@ wrote to a statepart
 | MemWriteInstruction Word64 X86_Operand SimpleExpr -- ^ An instruction wrote to an operand, resolving to an address
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
    Precondition          SimpleExpr Int SimpleExpr Int                           -- ^ Precondition:           lhs SEP rhs
  | Assertion             SimpleExpr SimpleExpr Int SimpleExpr Int                -- ^ Assertion:    @address, lhs SEP rhs
  | FunctionConstraint    String Word64 [(Register,SimpleExpr)] (S.Set StatePart) -- ^ Function name, address, of call, with param registers
  | SourcelessMemWrite    MemWriteIdentifier                                      -- ^ A write to a statepart for which no information was available
  | FunctionPointers      Word64 IS.IntSet                                        -- ^ A set of function pointers passed to a function
  deriving (Generic,Eq,Ord)

-- | An acornym for a set of verification conditions
type VCS = S.Set VerificationCondition



-- | An abstract domain for pointers
data PointerDomain =
    Domain_Bases    (S.Set PointerBase)  -- a non-empty set of bases
  | Domain_Sources  (S.Set BotSrc)       -- a possibly empty set of sources
  deriving (Generic,Eq,Ord)



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
   ctxt_config        :: Config,                         -- ^ __S__: the configuration file in ./config
   ctxt_dump          :: IM.IntMap Word8,                -- ^ __S__: mapping from addresses to bytes (constant data and instructions from the binary/executable)
   ctxt_data          :: IM.IntMap Word8,                -- ^ __S__: mapping from addresses to bytes (writable data section)
   ctxt_syms          :: IM.IntMap String,               -- ^ __S__: the symbol table: a mapping of addresses to function names for external functions
   ctxt_sections      :: SectionsInfo,                   -- ^ __S__: information on segments/section
   ctxt_dirname       :: String,                         -- ^ __S__: the name of the directory where the .dump, .entry, .sections and .symbols files reside
   ctxt_name          :: String,                         -- ^ __S__: the name of the binary

   ctxt_entries       :: Graph,                          -- ^ __D__: a graph with an edge (e0,e1) if entry address e0 calls entry address e1, and e0 and e1 have not been verified yet
   ctxt_cfgs          :: IM.IntMap CFG,                  -- ^ __D__: the currently known control flow graphs per function entry
   ctxt_calls         :: IM.IntMap FReturnBehavior,      -- ^ __D__: the currently known and verified entry addresses of functions mapped to return-information
   ctxt_invs          :: IM.IntMap Invariants,           -- ^ __D__: the currently known invariants
   ctxt_posts         :: IM.IntMap Postconditions,       -- ^ __D__: the currently known postconditions
   ctxt_inds          :: Indirections,                   -- ^ __D__: the currently known indirections
   ctxt_finits        :: IM.IntMap FInit,                -- ^ __D__: the currently known function initialisations
   ctxt_vcs           :: IM.IntMap VCS,                  -- ^ __D__: the verification conditions
   ctxt_results       :: IM.IntMap VerificationResult,   -- ^ __D__: the verification result
   ctxt_recursions    :: IM.IntMap IS.IntSet             -- ^ __D__: a mapping from function entries to the set of mutually recursive functions entries they occur in
 }
 deriving Generic



instance Cereal.Serialize NodeInfo
instance Cereal.Serialize VerificationResult
instance Cereal.Serialize JumpTable
instance Cereal.Serialize ResolvedJumpTarget
instance Cereal.Serialize Indirection
instance Cereal.Serialize CFG
instance Cereal.Serialize FReturnBehavior
instance Cereal.Serialize MemWriteIdentifier
instance Cereal.Serialize VerificationCondition
instance Cereal.Serialize PointerDomain
instance Cereal.Serialize SectionsInfo
instance Cereal.Serialize Context



-- | intialize an empty context based on the command-line parameters
init_context config dirname name =
  let dirname'       = if last dirname  == '/' then dirname else dirname ++ "/"
      empty_sections = SectionsInfo [] 0 0 in
    Context config IM.empty IM.empty  IM.empty empty_sections dirname' name (Edges IM.empty) IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty


-- | purge the context before exporting it (may save a lot of disk space)
purge_context  :: Context -> Context
purge_context (Context config dump dat syms sections dirname name entries cfgs calls invs posts inds finits vcs results recursions) =
  let keep_preconditions = store_preconditions_in_report config
      keep_assertions    = store_assertions_in_report config
      vcs'               = IM.filter (not . S.null) $ IM.map (purge keep_preconditions keep_assertions) vcs in
    Context config dump dat syms sections dirname name (Edges IM.empty) cfgs calls invs posts inds finits vcs' results IM.empty
 where
  purge keep_preconditions keep_assertions vcs = S.filter (keep_vcs keep_preconditions keep_assertions) vcs

  keep_vcs keep_preconditions keep_assertions (Precondition _ _ _ _)   = keep_preconditions
  keep_vcs keep_preconditions keep_assertions (Assertion    _ _ _ _ _) = keep_assertions
  keep_vcs keep_preconditions keep_assertions _                        = True



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


-- | Is the immediate roughly in range to be an address?
is_roughly_an_address ::
   Context                              -- ^ The context
   -> Int                               -- ^ An address
   -> Bool
is_roughly_an_address ctxt a =
  let si = ctxt_sections ctxt in
    a >= si_min_address si && a <= si_max_address si

-- | Find a section for an address (see @`SectionsInfo`@)
find_section_for_address ::
   Context                              -- ^ The context
   -> Int                               -- ^ An address
   -> Maybe (String, String, Int, Int)
find_section_for_address ctxt a =
  if is_roughly_an_address ctxt a then
    find (address_in_section a) (si_sections $ ctxt_sections ctxt)
  else
    Nothing
 where
  address_in_section a (_,_,a0,si) = a0 <= a && a < a0 + si






-- | Fetching an instruction
--
-- Returns @Nothing@ if the given address is out-of-range.
fetch_instruction ::
  Context              -- ^ The context
  -> Int               -- ^ An address
  -> IO (Maybe X86_Instruction)
fetch_instruction ctxt a = do
  let dump = ctxt_dump ctxt
  disassemble dump a


-- | Pretty printing an instruction
pp_instruction ::
  Context             -- ^ The context
  -> X86_Instruction  -- ^ An instruction
  -> String
pp_instruction ctxt i =
  if is_call (instr_opcode i) then
    show i ++
      case instr_srcs i of
        [Immediate imm] ->
          case IM.lookup (fromIntegral imm) $ ctxt_syms ctxt of
            Nothing  -> " (0x" ++ showHex imm ++ ")"
            Just sym -> " (" ++ (show sym) ++ ")"
        _ -> ""
  else
    show i



instance Show PointerDomain where
  show (Domain_Bases bs)     = show bs
  show (Domain_Sources srcs) = show srcs


-- | Show function initialisaExpr
show_finit :: FInit -> String
show_finit finit = intercalate ", " $ (map (\(sp,e) -> show sp ++ " ~= " ++ show e) $ M.toList finit)



instance Show VerificationResult where
 show VerificationSuccess               = "VerificationSuccess"
 show VerificationSuccesWithAssumptions = "VerificationSuccesWithAssumptions"
 show VerificationUnresolvedIndirection = "VerificationUnresolvedIndirection"
 show (VerificationError msg)           = "VerificationError: " ++ msg


instance Show MemWriteIdentifier where
  show (MemWriteFunction f a sp)       = f ++ "@" ++ showHex a ++ " WRITES TO " ++ show sp
  show (MemWriteInstruction a operand a') = "@" ++ showHex a ++ " WRITES TO " ++ show operand ++ (if add_resolved operand a' then " == " ++ show a' else "")
   where
    add_resolved (Storage r) (SE_StatePart (SP_Reg r')) = r /= r'
    add_resolved _           _                          = True



instance Show VerificationCondition where
  show (Precondition lhs _ rhs _)      = show lhs ++ " SEP " ++ show rhs
  show (Assertion a  lhs _ rhs _)      = "@" ++ show a ++ ": " ++ show lhs ++ " SEP " ++ show rhs
  show (SourcelessMemWrite mid)        = "UNKNOWN WRITE: " ++ show mid
  show (FunctionPointers a ptrs)       = "@" ++ showHex a ++ ": function pointers " ++ showHex_set ptrs
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
is_functionpointers (FunctionPointers _ _) = True
is_functionpointers _                      = False

-- | Count the number of assertions in the set of verification conditions.
count_instructions_with_assertions = S.size . S.map (\(Assertion rip _ _ _ _) -> rip) . S.filter is_assertion


-- | Count the number of sourceless memory writes in the set of verification conditions.
count_sourceless_memwrites = S.size . S.map (\(SourcelessMemWrite mid) -> prune mid) . S.filter is_sourceless_memwrite
 where
  prune mid@(MemWriteFunction f i_a sp)               = mid
  prune mid@(MemWriteInstruction i_a addr a_resolved) = MemWriteInstruction i_a addr $ SE_Immediate 0



ctxt_continue_on_unknown_instruction = continue_on_unknown_instruction . ctxt_config
ctxt_generate_pdfs = generate_pdfs . ctxt_config
ctxt_verbose_logs = verbose_logs . ctxt_config
ctxt_store_preconditions_in_report = store_preconditions_in_report . ctxt_config
ctxt_store_assertions_in_report = store_assertions_in_report . ctxt_config

ctxt_max_time :: Context -> Int
ctxt_max_time = fromIntegral . max_time . ctxt_config

ctxt_max_num_of_cases :: Context -> Int
ctxt_max_num_of_cases = fromIntegral . max_num_of_cases . ctxt_config

ctxt_max_num_of_bases :: Context -> Int
ctxt_max_num_of_bases = fromIntegral . max_num_of_bases . ctxt_config

ctxt_max_num_of_sources :: Context -> Int
ctxt_max_num_of_sources = fromIntegral . max_num_of_sources . ctxt_config

ctxt_max_jump_table_size :: Context -> Int
ctxt_max_jump_table_size = fromIntegral . max_jump_table_size . ctxt_config

ctxt_max_expr_size :: Context -> Int
ctxt_max_expr_size = fromIntegral . max_expr_size . ctxt_config


