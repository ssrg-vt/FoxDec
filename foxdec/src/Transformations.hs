{-# LANGUAGE TupleSections #-}

{-|
Module      : Transformations
Description : Various transformations for assembly dialects.
-}

module Transformations where

import           Base                       (showHex, showHex_set)
import           Context                    (CFG (cfg_blocks, cfg_edges, cfg_instrs),
                                             Context (ctxt_cfgs))
import           Generic_Datastructures     (AddressWord64,
                                             GenericAddress (AddressImm, AddressMinus, AddressPlus, AddressStorage, AddressTimes),
                                             GenericOperand (..),
                                             Instruction (Instruction))
import           X86_Datastructures         (Opcode (ADD, IMUL, MOV, POP, PUSH, SUB, XCHG),
                                             Prefix, Register (RAX, RDX, RSP),
                                             operand_size)

import           Control.Monad.State.Strict ()
import qualified Data.Graph.Dom             as G
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import           Data.List                  (intercalate)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, isNothing)
import qualified Data.Set                   as S
import           Data.Void                  (Void)
import           Data.Word                  (Word64)
import           Debug.Trace                ()





-- | A generic statement
data Statement label storage prefix opcode annotation special =
    Stmt_Instruction [Instruction label storage prefix opcode annotation] -- ^ A non-empty list of normal instructions
  | Stmt_Special special

data Program label storage prefix opcode annotation special = Program {
  program_basic_blocks :: IM.IntMap [Statement label storage prefix opcode annotation special],  -- ^ A mapping from blockIDs to lists of statements
  program_controlfow   :: G.Rooted                                                               -- ^ A graph based on integers (blockIDs)
 }


type L0 = Program AddressWord64 Register Prefix Opcode Int Void -- labels are words, storage locations are registers, there are no special instructions


-- For L1, storages are registers combined with index sets
data L1_Storage     = L1_Storage Register IS.IntSet deriving (Eq)
type L1             = Program AddressWord64 L1_Storage Prefix Opcode Int Void
type L1_Operand     = GenericOperand L1_Storage
type L1_Instruction = Instruction AddressWord64 L1_Storage Prefix Opcode Int
type L1_Statement   = Statement AddressWord64 L1_Storage Prefix Opcode Int Void
type L1_Blocks      = IM.IntMap [L1_Statement]




-- | From a context stored in a .report file, retrieve an L0 program for a given function entry.
obtain_L0_program ::
     Context -- ^ The context
  -> Int     -- ^ The function entry of interest
  -> L0
obtain_L0_program ctxt entry =
  case IM.lookup entry $ ctxt_cfgs ctxt of
    Just cfg -> cfg_to_L0 cfg
    Nothing  -> error $ "Function entry " ++ showHex entry ++ " does not exist."
 where
  -- convert a CFG to L0
  -- add edges for all terminal blocks to an empty set of successors
  cfg_to_L0 cfg =
    let blocks    = IM.map (map (Stmt_Instruction . singleton)) $ cfg_instrs cfg
        edges     = cfg_edges cfg
        terminals = filter (is_terminal_block cfg) $ IM.keys (cfg_blocks cfg)
        edges'    = IM.fromList $ map (,IS.empty) terminals in
      Program blocks (0,IM.unionWith IS.union edges edges')
  -- if the block terminal?
  is_terminal_block cfg b = isNothing $ IM.lookup b (cfg_edges cfg)

  singleton a = [a]


-- map two functions over a program, transforming regular instructions and special instructions
mapP ::
     ([Instruction label storage prefix opcode annotation] -> [Instruction label1 storage1 prefix1 opcode1 annotation1])
  -> (special -> special1)
  -> Program label  storage  prefix  opcode  annotation  special
  -> Program label1 storage1 prefix1 opcode1 annotation1 special1
mapP transform_instructions transform_special (Program blocks (root,g)) =
  Program (mapP_blocks blocks) (root,g)
 where
  mapP_blocks                          = IM.map mapP_block
  mapP_block                           = map mapP_statement
  mapP_statement (Stmt_Instruction is) = Stmt_Instruction $ transform_instructions is
  mapP_statement (Stmt_Special i)      = Stmt_Special $ transform_special i

-- map a function over an instruction, transforming the storages
mapI ::
     (storage -> storage1)
  -> Instruction label storage  prefix opcode annotation
  -> Instruction label storage1 prefix opcode annotation
mapI transform_storage (Instruction label prefix mnemonic ops annot) =
  Instruction label prefix mnemonic (map mapI_op ops) annot
 where
  mapI_op (Memory address si)  = Memory (mapI_address address) si
  mapI_op (EffectiveAddress a) = EffectiveAddress $ mapI_address a
  mapI_op (Storage r)          = Storage $ transform_storage r
  mapI_op (Immediate imm)      = Immediate imm

  mapI_address (AddressStorage r)   = AddressStorage $ transform_storage r
  mapI_address (AddressImm imm)     = AddressImm imm
  mapI_address (AddressMinus a0 a1) = AddressMinus (mapI_address a0) (mapI_address a1)
  mapI_address (AddressPlus a0 a1)  = AddressPlus  (mapI_address a0) (mapI_address a1)
  mapI_address (AddressTimes a0 a1) = AddressTimes (mapI_address a0) (mapI_address a1)




-- | Transformation: make the dataflows explicit
l0_to_l0_explicitize_dataflow :: L0 -> L0
l0_to_l0_explicitize_dataflow = mapP explicitize id
 where
  -- PUSH
  explicitize [Instruction label prefix PUSH [op1] annot] =
    let si = operand_size op1 in
     [
      Instruction label prefix SUB [Storage RSP,Immediate $ fromIntegral si] annot,
      Instruction label prefix MOV [Memory (AddressStorage RSP) si,op1] Nothing
     ]
  -- POP
  explicitize [Instruction label prefix POP [op1] annot] =
    let si = operand_size op1 in
     [
      Instruction label prefix MOV [op1,Memory (AddressStorage RSP) si] Nothing,
      Instruction label prefix ADD [Storage RSP,Immediate $ fromIntegral si] annot
     ]
  --IMUL (1)
  explicitize [Instruction label prefix IMUL [op1] annot] =
   [
    Instruction label prefix IMUL [Storage RDX, Storage RAX, op1] annot,
    Instruction label prefix IMUL [Storage RAX, Storage RAX, op1] Nothing
   ]
  -- XCHG
  explicitize [Instruction label prefix XCHG [op1,op2] annot] = -- TODO: think about this, as now the order matters
   [
    Instruction label prefix MOV [op1,op2] annot,
    Instruction label prefix MOV [op2,op1] Nothing
   ]
  -- REMAINDER
  explicitize i = i




-- transform an L0 instruction into an L1 instruction, trivially, by adding an empty set of indices
l0_instruction_to_l1_instruction = mapI l0_storage_to_l1_storage
 where
  l0_storage_to_l1_storage r = L1_Storage r IS.empty


-- | Trivial L0 to L1 translation by adding empty sets of indices
l0_to_l1 :: L0 -> L1
l0_to_l1 = mapP (map l0_instruction_to_l1_instruction) id






{--
-- | Not really a "to_ssa" function!
to_ssa :: Register -> L1 -> L1
to_ssa reg (Program blocks (root,g)) =
  let i_curr        = IS.singleton 0
      i_max         = 1
      (_,blocks')   = execState (traverse i_curr root) (i_max,blocks) in
    Program blocks' (root,g)
 where
  traverse :: IS.IntSet -> Int -> State (Int,L1_Blocks) ()
  traverse i_curr v = do
    (_,blocks) <- get
    let block = blocks IM.! v
    (encountered,modified,i_curr',block') <- traverse_block block i_curr False False
    when (modified || not encountered) $ do
      (i_max,blocks) <- get
      put (i_max,IM.insert v block' blocks)
      let post = IM.lookup v g `orElse` IS.empty
      mapM_ (traverse i_curr') $ IS.toList post


  traverse_block :: [L1_Statement] -> IS.IntSet -> Bool -> Bool -> State (Int,L1_Blocks) (Bool,Bool,IS.IntSet,[L1_Statement])
  traverse_block []                         i_curr encountered modified = return (encountered,modified,i_curr,[])
  traverse_block (Stmt_Instruction i:stmts) i_curr encountered modified = do
    (i_max,blocks) <- get
    let (i0,encountered0,modified0) = add_indices_to_read i i_curr reg
    let (i1,encountered1,modified1) = add_index_to_write i0 i_max reg
    if modified1 then do
      put (i_max+1,blocks)
      (_,_,i_curr,block) <- traverse_block stmts (IS.singleton i_max) True True
      return (True,True,i_curr,Stmt_Instruction i1:block)
    else do
      (encountered2,modified2,i_curr,block) <- traverse_block stmts i_curr (encountered0 || encountered1 || encountered) (modified0 || modified1 || modified)
      return (encountered2,modified2,i_curr,Stmt_Instruction i1:block)




-- add an index to a read
-- First operand may read from a storage as well!
add_indices_to_read :: L1_Instruction -> IS.IntSet -> Register -> (L1_Instruction, Bool, Bool)
add_indices_to_read (Instruction label prefix mnemonic op1 op2 op3 annot) i_curr reg =
  let is_storage   = operand_is_storage op1
      op1'         = if not $ is_storage then add_indices_to_operand reg i_curr <$> op1 else op1
      op2'         = add_indices_to_operand reg i_curr <$> op2
      op3'         = add_indices_to_operand reg i_curr <$> op3
      encountered1 = op1 /= Nothing && reg `register_elem_of` fromJust op1
      modified1    = op1 /= op1'
      encountered2 = op2 /= Nothing && reg `register_elem_of` fromJust op2
      modified2    = op2 /= op2'
      encountered3 = op3 /= Nothing && reg `register_elem_of` fromJust op3
      modified3    = op3 /= op3'
      encountered  = encountered1 || encountered2 || encountered3
      modified     = modified1 || modified2 || modified3 in
  (Instruction label prefix mnemonic op1' op2' op3' annot,encountered,modified)


-- add an index to a write
-- This happens only when the first operand is a register with currently no indices
add_index_to_write :: L1_Instruction -> Int -> Register -> (L1_Instruction,Bool,Bool)
add_index_to_write (Instruction label prefix mnemonic op1 op2 op3 annot) i_max reg =
  let is_storage  = operand_is_storage_with_no_indices op1
      op1'        = if is_storage then add_indices_to_operand reg (IS.singleton i_max) <$> op1 else op1
      encountered = op1 /= Nothing && reg `register_elem_of` fromJust op1
      modified    = op1 /= op1' in
  (Instruction label prefix mnemonic op1' op2 op3 annot,encountered,modified)


operand_is_storage (Just (Storage _)) = True
operand_is_storage _                  = False

operand_is_storage_with_no_indices (Just (Storage (L1_Storage _ is))) = IS.null is
operand_is_storage_with_no_indices _                                  = False


add_indices_to_operand :: Register -> IS.IntSet -> L1_Operand -> L1_Operand
add_indices_to_operand r is (Memory a si)        = Memory (add_indices_to_address r is a) si
add_indices_to_operand r is (EffectiveAddress a) = EffectiveAddress $ add_indices_to_address r is a
add_indices_to_operand r is (Immediate imm)      = Immediate imm
add_indices_to_operand r is (Storage r')         = Storage $ add_indices_to_l1_storage r is r'

add_indices_to_address r is (AddressImm imm)     = AddressImm imm
add_indices_to_address r is (AddressMinus a0 a1) = AddressMinus (add_indices_to_address r is a0) (add_indices_to_address r is a1)
add_indices_to_address r is (AddressPlus  a0 a1) = AddressPlus  (add_indices_to_address r is a0) (add_indices_to_address r is a1)
add_indices_to_address r is (AddressTimes a0 a1) = AddressTimes (add_indices_to_address r is a0) (add_indices_to_address r is a1)
add_indices_to_address r is (AddressStorage r')  = AddressStorage $ add_indices_to_l1_storage r is r'

add_indices_to_l1_storage r is (L1_Storage r' is') =
  let encountered = real_reg r == real_reg r' in
    if encountered then
      L1_Storage r' $ IS.union is' is
    else
      L1_Storage r' is'



register_elem_of r (Memory a si)               = register_elem_of_address r a
register_elem_of r (EffectiveAddress a)        = register_elem_of_address r a
register_elem_of r (Immediate imm)             = False
register_elem_of r (Storage (L1_Storage r' _)) = r == r'

register_elem_of_address r (AddressImm imm)                   = False
register_elem_of_address r (AddressMinus a0 a1)               = register_elem_of_address r a0 || register_elem_of_address r a1
register_elem_of_address r (AddressPlus  a0 a1)               = register_elem_of_address r a0 || register_elem_of_address r a1
register_elem_of_address r (AddressTimes a0 a1)               = register_elem_of_address r a0 || register_elem_of_address r a1
register_elem_of_address r (AddressStorage (L1_Storage r' _)) = r == r'
--}







instance (Eq storage, Show storage,Show label,Show prefix,Show opcode, Show annotation,Show special) => Show (Statement label storage prefix opcode annotation special) where
  show (Stmt_Instruction i) = show i
  show (Stmt_Special sp)    = show sp

instance (Eq storage, Show storage,Show label,Show prefix,Show opcode, Show annotation,Show special) => Show (Program label storage prefix opcode annotation special) where
  show (Program blocks (root,g)) = intercalate "\n\n" [
    "BLOCKS:\n" ++ intercalate "\n" (map show_block $ IM.toList blocks),
    "ENTRY: " ++ showHex root,
    "GRAPH:\n" ++ intercalate "\n" (map show_edge $ IM.toList g)
   ]
   where
    show_block (a,b) = "BLOCK " ++ show a ++ ":\n" ++ intercalate "\n" (map show b)
    show_edge (a,as) = showHex a ++ " --> " ++ showHex_set as


instance Show L1_Storage where
  show (L1_Storage r is) = show r ++ (if IS.null is then "" else if IS.size is == 1 then "_" ++ show (IS.findMin is) else show (IS.toList is))
