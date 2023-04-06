{-# LANGUAGE DeriveGeneric, DefaultSignatures, Strict, StandaloneDeriving, BangPatterns #-}

{-|
Module      : Context
Description : A @"Context"@ stores all the information retrieved from the binary, as well as the command-line parameters passed to FoxDec.

The context stores, among others, information obtained during verification, such as CFGs, invariants, etc. (see @`Context`@).
Module "VerificationReportInterface" provides functions for obtaining and interfacing with a @Context@.
-}


module Analysis.Context where

import Base
import Config

import Analysis.Capstone

import Data.JumpTarget
import Data.SymbolicExpression
import Data.SValue

import Generic.Binary
import Generic.SymbolicConstituents

import Instantiation.BinaryElf
import Instantiation.BinaryMacho

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set.NonEmpty as NES


import Data.List ( find, intercalate )
import Data.Word (Word8,Word64)
import Data.Maybe (mapMaybe,fromJust)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Int (Int64)


import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)
import X86.Register (Register)
import X86.Opcode (isCall)
import X86.Conventions
import qualified X86.Instruction as X86
import qualified X86.Operand as X86
import X86.Operand (GenericOperand(..))
import Generic.Operand (GenericOperand(..)) -- TODO: why is this needed?
import qualified Generic.Instruction as Instr
import Generic.Instruction (GenericInstruction(Instruction))
import X86.Opcode (Opcode(JMP), isCall, isJump)

import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)

import Control.DeepSeq


-- | The context augmented with information on the current function
data FContext = FContext {
  f_ctxt  :: Context,   -- ^ The context
  f_entry :: Int,       -- ^ The entry address of the current function
  f_init  :: FInit      -- ^ The initialization of the current function
 }

mk_fcontext :: Context -> Int -> FContext
mk_fcontext ctxt entry =
  let -- f     = function_name_of_entry ctxt entry
      finit = IM.lookup entry $ ctxt_finits ctxt
      fctxt = FContext ctxt entry (finit `orElse` init_finit) in
    fctxt

-- | Reading a binary given a filename (ELF or MachO)
read_binary :: String -> String -> IO (Maybe Binary)
read_binary dirname name = do
  let filename = dirname ++ name
  exists <- doesFileExist filename
  if exists then do
    -- if the original binary is given, we now assume it its an ELF
    content <-  BS.readFile filename
    if (BS.unpack $ BS.take 4 content) == [0x7f, 0x45, 0x4C, 0x46] then
      return $ Just $ Binary $ elf_read_file content
    else
      return Nothing
  else do
    -- otherwise, see if the binary is contained in the following files (MachO)
    exists1 <- doesFileExist (filename ++ ".dump")
    exists2 <- doesFileExist (filename ++ ".data")
    exists3 <- doesFileExist (filename ++ ".sections")
    exists4 <- doesFileExist (filename ++ ".symbols")
    exists5 <- doesFileExist (filename ++ ".entry")
    if and [exists1,exists2,exists3,exists4,exists5] then do
      macho <- macho_read_file dirname name
      return $ Just $ Binary $ macho
    else
      return Nothing



-- | A control flow graph with blocks and edges.
-- A blockID (represented as an @Int@) is a unique identifier of a basic block.
-- We store basic blocks twice: once as addresses, and once as instructions.
data CFG = CFG {
  cfg_blocks :: IM.IntMap [Int],            -- ^ A mapping of blockIDs to instruction addresses
  cfg_edges  :: IM.IntMap (IS.IntSet),      -- ^ A mapping of blockIDs to sets of blocKIDs
  cfg_addr_to_blockID :: IM.IntMap Int,     -- ^ A mapping of instruction addresses to blockIDs
  cfg_fresh :: Int,                         -- ^ A fresh blockID
  cfg_instrs :: IM.IntMap [X86.Instruction] -- ^ A mapping of blockIDs to lists of disassembled instructions.
 }
 deriving (Show,Generic,Eq)


-- | A jump table with :
--   index: an operand containing a bounded index at the beginning of execution of the block
--   bound: the bound on idx
--   trgt: the operand containg the jump target at the end of execution of the block
--   tbl: a table from values of idx to resulting jump targets
data JumpTable = JumpTable {
  jtbl_index  :: X86.Operand,
  jtbl_bound  :: Int,
  jtbl_target :: X86.Operand,
  jtbl_table  :: IM.IntMap Word64 
 }
 deriving (Generic, Eq)

-- | An indirection is either a jump table or a set of resolved jump targets
data Indirection = Indirection_JumpTable JumpTable | Indirection_Resolved (S.Set ResolvedJumpTarget) | Indirection_Unresolved
 deriving (Generic, Eq)

-- | Per instruction address, a set of possibly resolved jump targets
type Indirections = IM.IntMap Indirection




-- | An enumeration indicating the result of verification over a function
data VerificationResult =
     VerificationSuccess              -- ^ Function was succesfully verified
  | VerificationSuccesWithAssumptions -- ^ Function was succesfully verified, but required assertions
  | VerificationUnresolvedIndirection -- ^ Function contains an unresolved indirection
  | VerificationError String          -- ^ There was some verification error, e.g., return adresss overwrite
  | Unverified                        -- ^ The function has not been verified.
  deriving (Eq, Generic)


-- | Predicates: symbolic states
type Predicate = Sstate SValue


-- | Invariants: a mapping of blockIDs to predicates
type Invariants = IM.IntMap Predicate


-- | For each leaf-node in a CFG we store the following info. (TODO MOVE?)
data NodeInfo =
    Normal                -- ^ The basic block behaves normally, e.g., a ret
  | UnresolvedIndirection -- ^ The basic block ends in an unresolved indirection
  | Terminal              -- ^ The basic blocks ends with, e.g., a call to exit()
 deriving (Show,Generic,Eq,Ord)

-- | Postconditions: for each final block the @NodeInfo@ and the final predicate after execution of the block
type Postconditions = S.Set (NodeInfo,Predicate)





-- | An abstract domain for pointers
data PointerDomain =
    Domain_Bases    (NES.NESet PointerBase)  -- a non-empty set of bases
  | Domain_Sources  (NES.NESet BotSrc)       -- a possibly empty set of sources
  deriving (Generic,Eq,Ord)



-- | A function initialisation consists of a mapping of registers to symbolic pointers TODO
data MemRelation = Separate | Aliassing | Unknown
  deriving (Generic,Eq,Ord,Show)
data FInit = FInit (S.Set (SStatePart,Maybe SValue)) (M.Map (SStatePart,SStatePart) MemRelation)
  deriving (Generic,Eq,Ord)

-- | A function call 
data FReturnBehavior =
    Terminating              -- ^ The function does never return
  | ReturningWith Predicate  -- ^ The function returns withg the symbolic changes stored in the predicate
  | UnknownRetBehavior       -- ^  It is unknown whether the function returns or not
 deriving (Show,Generic,Eq,Ord)


-- | A symbolic state part
data SStatePart =
    SSP_Reg Register -- ^ A register
  | SSP_Mem SValue Int
 deriving (Show,Generic,Eq,Ord)



-----------------------------------------------------------------------------
-- |
-- The context datastructure.
--
-- __S__: Information __S__tatically obtained by reading from the binary
--
-- __D__: Information __D__ynamically updated during verification
-------------------------------------------------------------------------------
data Context_ = Context_ {
   ctxt__config        :: !Config,                         -- ^ __S__: the configuration file in ./config
   ctxt__verbose       :: !Bool,                           -- ^ __S__: is verbose output requested?
   ctxt__syms          :: !SymbolTable,                    -- ^ __S__: the symbol table: a mapping of addresses to function names for external functions
   ctxt__sections      :: !SectionsInfo,                   -- ^ __S__: information on segments/section
   ctxt__relocs        :: !(S.Set Relocation),             -- ^ __S__: relocation entries
   ctxt__dirname       :: !String,                         -- ^ __S__: the name of the directory where the .dump, .entry, .sections and .symbols files reside
   ctxt__name          :: !String,                         -- ^ __S__: the name of the binary
   ctxt__start         :: !Int,                            -- ^ __S__: the address of the _start symbol

   ctxt__entries       :: !Graph,                          -- ^ __D__: a graph with an edge (e0,e1) if entry address e0 calls entry address e1, and e0 and e1 have not been verified yet
   ctxt__cfgs          :: !(IM.IntMap CFG),                -- ^ __D__: the currently known control flow graphs per function entry
   ctxt__calls         :: !(IM.IntMap FReturnBehavior),    -- ^ __D__: the currently known and verified entry addresses of functions mapped to return-information
   ctxt__invs          :: !(IM.IntMap Invariants),         -- ^ __D__: the currently known invariants
   ctxt__posts         :: !(IM.IntMap Postconditions),     -- ^ __D__: the currently known postconditions
   ctxt__stateparts    :: !(IM.IntMap (S.Set SStatePart)), -- ^ __D__: the state parts read from/written to be the function
   ctxt__inds          :: !Indirections,                   -- ^ __D__: the currently known indirections
   ctxt__finits        :: !(IM.IntMap FInit),              -- ^ __D__: the currently known function initialisations
   ctxt__vcs           :: !(IM.IntMap VCS),                -- ^ __D__: the verification conditions
   ctxt__results       :: !(IM.IntMap VerificationResult), -- ^ __D__: the verification result
   ctxt__recursions    :: !(IM.IntMap IS.IntSet),          -- ^ __D__: a mapping from function entries to the set of mutually recursive functions entries they occur in
   ctxt__runningtime   :: !Int64                           -- ^ __D__: running time of lifting effort
 }
 deriving Generic

data Context = Context {
  ctxt_binary :: !Binary,
  ctxt_ioref  :: IORef (IM.IntMap X86.Instruction),
  ctxt_ctxt_  :: !Context_
 }


ctxt_config      = ctxt__config      . ctxt_ctxt_
ctxt_verbose     = ctxt__verbose     . ctxt_ctxt_
ctxt_syms        = ctxt__syms        . ctxt_ctxt_
ctxt_sections    = ctxt__sections    . ctxt_ctxt_
ctxt_relocs      = ctxt__relocs      . ctxt_ctxt_
ctxt_dirname     = ctxt__dirname     . ctxt_ctxt_
ctxt_name        = ctxt__name        . ctxt_ctxt_
ctxt_start       = ctxt__start       . ctxt_ctxt_
ctxt_entries     = ctxt__entries     . ctxt_ctxt_
ctxt_cfgs        = ctxt__cfgs        . ctxt_ctxt_
ctxt_calls       = ctxt__calls       . ctxt_ctxt_
ctxt_invs        = ctxt__invs        . ctxt_ctxt_
ctxt_posts       = ctxt__posts       . ctxt_ctxt_
ctxt_stateparts  = ctxt__stateparts  . ctxt_ctxt_
ctxt_inds        = ctxt__inds        . ctxt_ctxt_
ctxt_finits      = ctxt__finits      . ctxt_ctxt_
ctxt_vcs         = ctxt__vcs         . ctxt_ctxt_
ctxt_results     = ctxt__results     . ctxt_ctxt_
ctxt_recursions  = ctxt__recursions  . ctxt_ctxt_
ctxt_runningtime = ctxt__runningtime . ctxt_ctxt_

set_ctxt_syms       syms     (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__syms = syms})
set_ctxt_sections   sections (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__sections = sections})
set_ctxt_relocs     relocs   (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__relocs = relocs})
set_ctxt_start      start    (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__start = start})
set_ctxt_entries    entries  (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__entries = entries})
set_ctxt_calls      calls    (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__calls = calls})
set_ctxt_inds       inds     (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__inds = inds})
set_ctxt_posts      posts    (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__posts = posts})
set_ctxt_stateparts sps      (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__stateparts = sps})
set_ctxt_vcs        vcs      (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__vcs = vcs})
set_ctxt_finits     finits   (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__finits = finits})
set_ctxt_invs       invs     (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__invs = invs})
set_ctxt_cfgs       cfgs     (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__cfgs = cfgs})
set_ctxt_results    results  (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__results = results})
set_ctxt_recursions recs     (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__recursions = recs})
set_ctxt_runningtime time    (Context bin ioref ctxt_) = Context bin ioref (ctxt_ {ctxt__runningtime = time})

instance Cereal.Serialize VerificationResult
instance Cereal.Serialize ResolvedJumpTarget
instance Cereal.Serialize JumpTable
instance Cereal.Serialize Symbol
instance Cereal.Serialize SymbolTable
instance Cereal.Serialize Indirection
instance Cereal.Serialize Relocation
instance Cereal.Serialize MemRelation 
instance Cereal.Serialize FInit
instance Cereal.Serialize CFG
instance Cereal.Serialize FReturnBehavior
instance Cereal.Serialize VerificationCondition
instance Cereal.Serialize PointerDomain
instance Cereal.Serialize SectionsInfo
instance Cereal.Serialize SStatePart
instance Cereal.Serialize NodeInfo
instance Cereal.Serialize Context_


instance NFData VerificationResult
instance NFData ResolvedJumpTarget
instance NFData JumpTable
instance NFData Symbol
instance NFData SymbolTable
instance NFData Indirection
instance NFData Relocation
instance NFData MemRelation 
instance NFData FInit
instance NFData CFG
instance NFData FReturnBehavior
instance NFData VerificationCondition
instance NFData PointerDomain
instance NFData SectionsInfo
instance NFData SStatePart
instance NFData NodeInfo
instance NFData Context_

-- | intialize an empty context based on the command-line parameters
init_finit = FInit S.empty M.empty
init_context binary ioref config verbose dirname name =
  let !sections = binary_get_sections_info binary
      !symbols  = binary_get_symbols binary
      !relocs   = binary_get_relocations binary in
  Context binary ioref $ Context_ config verbose symbols sections relocs dirname name 0 (Edges IM.empty) IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty 0


-- | purge the context before exporting it (may save a lot of disk space)
purge_context  :: Context -> Context_
purge_context (Context binary _ (Context_ config verbose syms sections relocs dirname name start entries cfgs calls invs posts stateparts inds finits vcs results recursions runningTime)) =
  let keep_preconditions = store_preconditions_in_L0 config
      keep_assertions    = store_assertions_in_L0 config
      vcs'               = IM.filter (not . S.null) $ IM.map (purge keep_preconditions keep_assertions) vcs in
    Context_ config verbose syms sections relocs dirname name start (Edges IM.empty) cfgs calls invs IM.empty IM.empty inds finits vcs' results IM.empty runningTime
 where
  purge keep_preconditions keep_assertions vcs = S.filter (keep_vcs keep_preconditions keep_assertions) vcs

  keep_vcs keep_preconditions keep_assertions (Precondition _ _ _ _)   = keep_preconditions
  keep_vcs keep_preconditions keep_assertions (Assertion    _ _ _ _ _) = keep_assertions
  keep_vcs keep_preconditions keep_assertions _                        = True



-- | Getting the symbol table
ctxt_symbol_table ctxt =
  case ctxt_syms ctxt of
    SymbolTable tbl -> tbl



-- | Reading from a read-only data section.
--
-- Reads maximally up to 8 bytes. Returns @Nothing@ if the given address is out-of-range.
read_from_ro_datasection ::
     Context       -- ^ The context 
  -> Word64        -- ^ An address
  -> Int           -- ^ Size, i.e., the number of bytes to read
  -> Maybe Word64
read_from_ro_datasection ctxt a si = bytes_to_word <$> binary_read_ro_data (ctxt_binary ctxt) a si

-- | Reading from a writable data section.
--
-- Reads maximally up to 8 bytes. Returns @Nothing@ if the given address is out-of-range.
read_from_datasection ::
     Context       -- ^ The context 
  -> Word64        -- ^ An address
  -> Int           -- ^ Size, i.e., the number of bytes to read
  -> Maybe Word64
read_from_datasection ctxt a si = bytes_to_word <$> binary_read_data (ctxt_binary ctxt) a si



-- | Is the immediate roughly in range to be an address?
is_roughly_an_address ::
  Context                              -- ^ The context
  -> Word64                            -- ^ An address
  -> Bool
is_roughly_an_address ctxt a = 
  let si = ctxt_sections ctxt in
    a >= si_min_address si && a <= si_max_address si

-- | Find a section for an address (see @`SectionsInfo`@)
find_section_for_address ::
   Context                              -- ^ The context
   -> Word64                            -- ^ An address
   -> Maybe (String, String, Word64, Word64)
find_section_for_address ctxt a =
  if is_roughly_an_address ctxt a then
    find (address_in_section a) (si_sections $ ctxt_sections ctxt)
  else
    Nothing
 where
  address_in_section a (_,_,a0,si) = a0 <= a && a < a0 + si


-- | Find a section ending at address (see @`SectionsInfo`@)
find_section_ending_at ::
   Context                              -- ^ The context
   -> Word64                            -- ^ An address
   -> Maybe (String, String, Word64, Word64)
find_section_ending_at ctxt a = find (address_ends_at_section a) (si_sections $ ctxt_sections ctxt)
 where
  address_ends_at_section a (_,_,a0,si) = a == a0 + si



-- | Fetching an instruction
--
-- Returns @Nothing@ if the given address is out-of-range.
fetch_instruction ::
  Context              -- ^ The context
  -> Word64            -- ^ An address
  -> IO (Maybe X86.Instruction)
fetch_instruction ctxt a =
  case binary_read_ro_data (ctxt_binary ctxt) a 20 of -- maximum instruction length == 15
    Nothing -> return Nothing
    Just bytes -> disassemble (ctxt_ioref ctxt) bytes a


-- | Pretty printing an instruction
pp_instruction ::
  Context             -- ^ The context
  -> X86.Instruction  -- ^ An instruction
  -> String
pp_instruction ctxt i =
  if isCall (Instr.opcode i) then
    show i ++
      case Instr.srcs i of
        [Immediate imm] ->
          case IM.lookup (fromIntegral imm) $ ctxt_symbol_table ctxt of
            Just (Relocated_Function sym) -> " (" ++ show sym ++ ")"
            Just (Internal_Label sym)     -> " (" ++ show sym ++ ")"
            Nothing  -> " (0x" ++ showHex imm ++ ")"
        _ -> ""
  else
    show i



instance Show PointerDomain where
  show (Domain_Bases bs)     = "[" ++ intercalate "," (map show $ neSetToList bs) ++ "]_B"
  show (Domain_Sources srcs) = "[" ++ intercalate "," (map show $ neSetToList srcs) ++ "]_S"


-- | Show function initialisation
instance Show FInit where
 show (FInit sps m) = intercalate "\n" $ filter ((/=) []) $ 
  [ "Stateparts:" 
  , intercalate ", " $ map (show . fst) $ S.toList $ S.filter (((==) Nothing) . snd) sps
  , intercalate "\n" $ map show_sp $ S.toList $ S.filter (((/=) Nothing) . snd) sps
  , intercalate "\n" $ map show_entry $ M.toList m ]
  where
    show_sp (sp,Just v)    = show sp ++ " == " ++ show v
    show_entry ((sp0,sp1),r) = show (sp0,sp1) ++ ": " ++ show r




instance Show VerificationResult where
 show VerificationSuccess               = "VerificationSuccess"
 show VerificationSuccesWithAssumptions = "VerificationSuccesWithAssumptions"
 show VerificationUnresolvedIndirection = "VerificationUnresolvedIndirection"
 show (VerificationError msg)           = "VerificationError: " ++ msg


instance Show JumpTable where
  show (JumpTable idx bnd trgt tbl) = "JumpTable: " ++ show idx ++ " < " ++ show bnd ++ " --> " ++ show trgt ++ " in " ++ showHex_set (IS.fromList $ map fromIntegral $ IM.elems tbl)

instance Show Indirection where
  show (Indirection_JumpTable tbl)   = show tbl
  show (Indirection_Resolved  trgts) = show trgts





instance Show VerificationCondition where
  show (Precondition lhs _ rhs _)      = show lhs ++ " SEP " ++ show rhs
  show (Assertion a  lhs _ rhs _)      = "@" ++ show a ++ ": " ++ show lhs ++ " SEP " ++ show rhs
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


-- | Is the given verification condition a function pointer introduction?
is_functionpointers (FunctionPointers _ _) = True
is_functionpointers _                      = False

-- | Count the number of assertions in the set of verification conditions.
count_instructions_with_assertions = S.size . S.map (\(Assertion rip _ _ _ _) -> rip) . S.filter is_assertion





ctxt_continue_on_unknown_instruction = continue_on_unknown_instruction . ctxt_config
ctxt_generate_pdfs = generate_pdfs . ctxt_config
ctxt_verbose_logs = verbose_logs . ctxt_config
ctxt_store_preconditions_in_L0 = store_preconditions_in_L0 . ctxt_config
ctxt_store_assertions_in_L0 = store_assertions_in_L0 . ctxt_config

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



-- | Read in the .L0 file from a file with the given file name.
--   May produce an error if no L0 can be read from the file.
--   Returns the L0 stored in the .L0 file.
ctxt_read_L0 :: 
   String        -- ^ The directory name
   -> String     -- ^ The file name of the binary
   -> IO Context
ctxt_read_L0 dirname name = do
  rcontents <- BS.readFile (dirname ++ name ++ ".L0")
  ioref     <- newIORef IM.empty
  bcontents <- read_binary dirname name
  case (Cereal.decode rcontents, bcontents) of
    (Left err,_)           -> error $ "Could not read verification L0 in file " ++ (dirname ++ name ++ ".L0") ++  "\n" ++ show err
    (_,Nothing)            -> error $ "Cannot read binary file: " ++ dirname ++ name
    (Right ctxt_,Just bin) -> return $ Context bin ioref ctxt_





-- | Returns true iff an instruction can be fetched from the address.
address_has_instruction ctxt a =
  case find_section_for_address ctxt $ fromIntegral a of
    Nothing                    -> False
    Just (segment,section,_,_) -> (segment,section) `elem` sections_with_instructions && unsafePerformIO (fetch_instruction ctxt $ fromIntegral a) /= Nothing


