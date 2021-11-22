{-# LANGUAGE PartialTypeSignatures, Strict #-}

module CFG_Gen where

import Base
import Context
import MachineState
import SimplePred
import Propagation
import Conventions
import SCC


import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Either (fromRight,fromLeft,partitionEithers)
import Data.Maybe (fromJust)
import Data.List
import Data.List.Split (chunksOf)
import Data.Word (Word64)
import X86_Datastructures
import Control.Monad ((>=>))
import Debug.Trace
import Numeric (readHex)
import GHC.Float.RealFracMethods (floorDoubleInt,int2Double)
import System.IO.Unsafe (unsafePerformIO)

-- the algorithm below has been formally proven correct in Isabelle/HOL
split_graph' a g = 
  case IM.lookup a (cfg_addr_to_blockID g) of
    Nothing -> Just g
    Just blockID ->
      case IM.lookup blockID (cfg_blocks g) of
        Nothing -> Just g
        Just block ->
          if last block /= a then do
            let (begin,end) = break ((==) a) block
            let f           = cfg_fresh g
            let blocks'     = IM.insert blockID (begin++ [a]) $ IM.insert f (tail end) (cfg_blocks g)
            let edges'      = IM.insert blockID (IS.singleton f) $ IM.mapKeys (\k -> if k == blockID then f else k) (cfg_edges g)
            let a_to_b'     = IM.mapWithKey (\addr blockID -> if addr `elem` tail end then f else blockID) (cfg_addr_to_blockID g)
            let fresh'      = f + 1
            return $ CFG blocks' edges' a_to_b' fresh' IM.empty
          else
            return g

split_graph a g = do
  case IM.lookup a (cfg_addr_to_blockID g) of
    Nothing -> Just g
    Just blockID ->
      case IM.lookup blockID (cfg_blocks g) of
        Nothing -> Just g
        Just block ->
          if head block /= a then do
            let (begin,end) = break ((==) a) block
            let f           = cfg_fresh g
            let blocks'     = IM.insert blockID begin $ IM.insert f end  (cfg_blocks g)
            let edges'      = IM.insert blockID (IS.singleton f) $ IM.mapKeys (\k -> if k == blockID then f else k) (cfg_edges g)
            let a_to_b'     = IM.mapWithKey (\addr blockID -> if addr `elem` end then f else blockID) (cfg_addr_to_blockID g)
            let fresh'      = f + 1
            return $ CFG blocks' edges' a_to_b' fresh' IM.empty
          else
            return g

add_edge_to_graph a0 a1 g = do
  case IM.lookup a0 (cfg_addr_to_blockID g) of
    Nothing -> Just g
    Just blockID ->
      case IM.lookup blockID (cfg_blocks g) of
        Nothing -> Just g
        Just block ->
          case IM.lookup a1 (cfg_addr_to_blockID g) of
            Just blockID' -> do
              case IM.lookup blockID' (cfg_blocks g) of
                Nothing -> Just g
                Just block' -> do
                  let edges' = IM.alter (add_to_intset blockID') blockID (cfg_edges g)
                  return $ g { cfg_edges = edges' }
            Nothing -> do
              case IM.lookup blockID (cfg_edges g) of
                Nothing -> do
                  let blocks' = IM.alter (append_to_list a1) blockID (cfg_blocks g)
                  let a_to_b' = IM.insert a1 blockID (cfg_addr_to_blockID g)
                  return $ g { cfg_blocks = blocks', cfg_addr_to_blockID = a_to_b' }
                _ -> do
                  let f       = cfg_fresh g
                  let blocks' = IM.insert f [a1] (cfg_blocks g)
                  let edges'  = IM.alter (add_to_intset f) blockID (cfg_edges g)
                  let a_to_b' = IM.insert a1 f (cfg_addr_to_blockID g)
                  let fresh'  = f + 1
                  return $ CFG blocks' edges' a_to_b' fresh' IM.empty



add_to_intset a Nothing  = Just $ IS.singleton a
add_to_intset a (Just x) = Just $ IS.insert a x 

append_to_list a Nothing = Just [a]
append_to_list a (Just x) = Just (x++[a]) 

add_edge a0 a1 g =
  case add_to g of 
    Nothing -> error "Could not add edge"
    Just g -> g
 where add_to = split_graph' a0 >=> split_graph a1 >=> add_edge_to_graph a0 a1


add_edges [] g = g 
add_edges ((a0,a1):edges) g = add_edges edges (add_edge a0 a1 g)

init_cfg a = CFG { cfg_blocks = IM.singleton 0 [a], cfg_edges = IM.empty, cfg_addr_to_blockID = IM.singleton a 0, cfg_fresh = 1, cfg_instrs = IM.empty }


is_consecutive a b []      = False
is_consecutive a b [_]     = False
is_consecutive a b (c:d:x) = (a,b) == (c,d) || is_consecutive a b (d:x)

is_edge g a0 a1 =
 case lookup of
  Nothing -> False
  Just b -> b
 where
  lookup = do
    blockId  <- IM.lookup a0 (cfg_addr_to_blockID g)
    blockId' <- IM.lookup a1 (cfg_addr_to_blockID g)
    b  <- IM.lookup blockId  (cfg_blocks g)
    b' <- IM.lookup blockId' (cfg_blocks g)
    if last b == a0 then do
      edges <- IM.lookup blockId (cfg_edges g)
      return $ head b' == a1 && blockId' `IS.member` edges
    else
      return $ blockId == blockId' && is_consecutive a0 a1 b










-- returns true iff an instruction can be fetched from the address 
address_has_instruction ctxt a =
  case unsafePerformIO $ fetch_instruction ctxt $ fromIntegral a of -- TODO. However, should be safe as result is immutable.
    Nothing -> False
    Just i  -> True

-- returns true iff a symolb is associated with the address
address_has_symbol ctxt a =
  case IM.lookup (fromIntegral a) $ ctxt_syms ctxt of
    Nothing  -> False
    Just sym -> True

-- returns truee if the adress is external, i.e., has no instruction or has a symbol
address_is_external ctxt a = address_has_symbol ctxt a || not (address_has_instruction ctxt a)


-- resolving the operand of a jump call can either produce 
--   Unresolved:  an indirect branch that has not been resolved yet
--   External f:  a call to external function f
--   Immediate a: an internal call to the given address
data ResolvedJumpTarget = Unresolved | External String | ImmediateAddress Word64
 deriving (Eq,Show)

-- many operands can statically be resolved, even though technically they are indirect (relative to RIP)
operand_static_resolve :: Context -> Instr -> Maybe Operand -> ResolvedJumpTarget
operand_static_resolve ctxt i (Just (Immediate a'))                                                 = ImmediateAddress a'
operand_static_resolve ctxt i (Just (Address (AddrPlus (FromReg RIP) (AddrImm imm))))               = ImmediateAddress $ fromIntegral (i_addr i) + fromIntegral (i_size i) + fromIntegral imm
operand_static_resolve ctxt i (Just (Address (AddrPlus (AddrImm imm) (FromReg RIP))))               = ImmediateAddress $ fromIntegral (i_addr i) + fromIntegral (i_size i) + fromIntegral imm
operand_static_resolve ctxt i (Just (Address (SizeDir si (AddrPlus  (FromReg RIP) (AddrImm imm))))) = static_resolve_rip_expr ctxt i (\rip -> rip + imm) si
operand_static_resolve ctxt i (Just (Address (SizeDir si (AddrPlus  (AddrImm imm) (FromReg RIP))))) = static_resolve_rip_expr ctxt i (\rip -> rip + imm) si
operand_static_resolve ctxt i (Just (Address (SizeDir si (AddrMinus (FromReg RIP) (AddrImm imm))))) = static_resolve_rip_expr ctxt i (\rip -> rip - imm) si
operand_static_resolve ctxt i _                                                                     = Unresolved

static_resolve_rip_expr ctxt i f si =
  let rip     = i_addr i + i_size i
      a'      = fromIntegral $ f rip
      syms    = ctxt_syms    ctxt in
    case (IM.lookup (fromIntegral a') syms,read_from_datasection ctxt a' si) of
      (Just s,  a'')      -> 
        -- Example:
        --   Instruction 10005464e: CALL 64 ptr [RIP + 1751660] 6 read from address 1002000c0 which has symbol _objc_msgSend producing address 0
        --   Address 1002000c0 is returned and treated as an external function call       
        -- trace ("Instruction " ++ show i ++ " read from address " ++ showHex a' ++ " which has symbol " ++ s ++ " producing address " ++ showHex_option a'') $ 
        External s
      (Nothing, Just a'') ->
        -- Example:
        --   Instruction 10011e093: CALL 64 ptr [RIP + 1098831] 6 read from address 10022a4e8 producing address 100131d63
        --   Address 100131d63 is returned as that is the function pointer to be called
        -- trace ("Instruction " ++ show i ++ " read from address " ++ showHex a' ++ " producing address " ++ showHex a'') $
        ImmediateAddress a''
      (Nothing,Nothing)   ->
        Unresolved

-- Resolve the first operand of a call or jump instruction.
-- returns a list of  ResolvedJumpTargets
-- TODO: we don't store indirections to external functions
resolve_jump_target ctxt i =
  case IM.lookup (i_addr i) $ ctxt_inds ctxt of
    Just as -> map (ImmediateAddress . fromIntegral) $ filter (not . address_is_external ctxt) $ IS.toList as -- already resolved indirection
    Nothing -> 
      case operand_static_resolve ctxt i (i_op1 i) of
        Unresolved -> [Unresolved] -- unresolved indirection
        External sym ->
          if "libc_start_main" `isInfixOf` sym then
            [Unresolved] -- An indirection: register RDI holds the pointer of the main function
          else
            [External sym]
        ImmediateAddress a  ->
          case IM.lookup (fromIntegral a) $ ctxt_syms ctxt of
            Just sym -> [External sym]
            Nothing  -> if not (address_has_instruction ctxt a) then [External $ showHex a] else [ImmediateAddress a]


instruction_jumps_to_external ctxt i = 
  all resolve_is_external $ resolve_jump_target ctxt i
 where
  resolve_is_external (External _) = True
  resolve_is_external _            = False

-- Tries to retrieve a function name with an entry address
-- If the entry matches a known symbol, return that.
-- Otherwise, simply return the entry address itself in hexadecimal notation.
-- However, there is one exception: 
-- 	if the first instruction at the entry address immediately jumps to an external function,
-- 	return the name of that external function instead. This happens in a .got section.
function_name_of_entry :: Context -> Int -> String
function_name_of_entry ctxt a =
  case IM.lookup a $ ctxt_syms ctxt of
    Just sym -> sym
    Nothing  ->
      case unsafePerformIO $ fetch_instruction ctxt a of -- TODO. However, should be safe as result is immutable.
        Just i@(Instr _ _ JMP op1 _ _ _ _)  ->
          case operand_static_resolve ctxt i op1 of
            External sym -> sym
            _ -> "0x" ++ showHex a
        _ -> "0x" ++ showHex a

function_name_of_instruction :: Context -> Instr -> String
function_name_of_instruction ctxt i =
  if is_call (i_opcode i) || is_jump (i_opcode i) then
    case operand_static_resolve ctxt i (i_op1 i) of
      External sym       -> sym
      ImmediateAddress a -> function_name_of_entry ctxt $ fromIntegral a
      Unresolved         -> "indirection@" ++ showHex (i_addr i)
  else
    ""





resolve_call ctxt entry i =
  let resolved_addresses = resolve_jump_target ctxt i in
    if any ((==) Unresolved) resolved_addresses then
      Right []
    else 
      let nexts          = map next resolved_addresses
          (lefts,rights) = partitionEithers nexts in
        if lefts == [] then
          Right $ concat rights
        else
          Left $ IS.fromList $ concat lefts
 where
  next (External sym) =
    -- external function call to known symbol
    if sym `elem` exiting_functon_calls then 
      Right []
    else
      Right [i_addr i + i_size i]
  next (ImmediateAddress a') =
    -- call to an immediate address
    case IM.lookup (fromIntegral a') $ ctxt_calls ctxt of
      Just terminating -> 
        -- internal function call already analyzed
        Right $ if terminating then [] else [i_addr i + i_size i]
      Nothing -> 
        -- internal function call not analyzed yet
        if not $ graph_is_parent (ctxt_entries ctxt) (fromIntegral a') then --TODO remove
          -- a new entry address, first time visit
          Left $ [fromIntegral a'] -- trace  ("New function call found with entry: " ++ showHex a') $ 
        else if not $ graph_is_edge (ctxt_entries ctxt) entry (fromIntegral a') then
          Left $ [fromIntegral a'] -- trace ("Adding new edge to entry graph (" ++ showHex entry ++ ","  ++ showHex a' ++ ")") $  
        else
          -- a recursive call
          Right $ [i_addr i + i_size i] -- trace ("Recursive call to entry: " ++ showHex a') $ 

stepA :: Context -> Int -> Int -> IO (Either IS.IntSet [Int])
stepA ctxt entry a = do
  instr <- fetch_instruction ctxt a
  case instr of
    Nothing -> return $ Right [] -- error $ "Cannot find instruction at addres: " ++ showHex a
    Just i -> 
      if is_halt (i_opcode i) then
        return $ Right []
      else if is_jump (i_opcode i) then
        return $ Right $ concatMap get_internal_addresses $ resolve_jump_target ctxt i 
      else if is_cond_jump $ i_opcode i then
        return $ Right $ (concatMap get_internal_addresses $ resolve_jump_target ctxt i) ++ [a + i_size i]
      else if is_call (i_opcode i) then
        return $ resolve_call ctxt entry i
      else if is_ret (i_opcode i) then
        return $ Right []
      else
        return $ Right [a + i_size i]

get_internal_addresses (External _)         = []
get_internal_addresses Unresolved           = []
get_internal_addresses (ImmediateAddress a) = [fromIntegral a]

{-- END OF X86 SPECIFIC ---}

mk_graph :: Context -> Int -> S.Set (Int, Int) -> CFG -> IS.IntSet -> IO (IS.IntSet,CFG) 
mk_graph ctxt entry bag g new_calls =
  case S.minView bag of
    Nothing -> return $ (new_calls,g)
    Just ((a0,a1),bag) -> do
      if is_edge g a0 a1 then 
        mk_graph ctxt entry bag g new_calls
      else do
        let g' = add_edge a0 a1 g
        nxt <- stepA ctxt entry a1
        case nxt of
          Left as -> do
            mk_graph ctxt entry bag g' (IS.union as new_calls)
          Right as -> do
            let bag' = S.union (S.fromList $ map (\a2 -> (a1,a2)) as) bag
            mk_graph ctxt entry bag' g' new_calls
    

fromJust' as Nothing = error $ showHex_list as
fromJust' _ (Just a) = a

cfg_add_instrs ctxt g = do
  instrs <- mapM block_to_instrs $ IM.toList $ cfg_blocks g
  return $ g { cfg_instrs = IM.fromList instrs }
 where
    block_to_instrs (a,as) = do 
      instrs <- mapM (fetch_instruction ctxt) as
      return $ (a, map (fromJust' as) instrs)

cfg_gen :: Context -> Int -> IO (Either IS.IntSet CFG)
cfg_gen ctxt entry = do
 let g           = init_cfg entry
 nxt            <- stepA ctxt entry entry
 let bag         = S.fromList $ map (\a -> (entry,a)) (fromRight [] nxt)
 (new_calls,g') <- mk_graph ctxt entry bag g IS.empty
 if IS.null new_calls then do
   g'' <- cfg_add_instrs ctxt g'
   return $ Right g''
 else 
   return $ Left new_calls
     


        

is_end_node g b = IS.null $ post g b

is_unresolved_indirection ctxt i = (is_call (i_opcode i) || is_jump (i_opcode i) || is_cond_jump (i_opcode i))
                   && (any ((==) Unresolved) $ resolve_jump_target ctxt i)



node_info_of ctxt g blockId =
  let a    = last (im_lookup ("C.) Block " ++ show blockId ++ " in cfg.") (cfg_blocks g) blockId)
      i    = last (im_lookup ("D.) Block " ++ show blockId ++ " in instrs.") (cfg_instrs g) blockId) in
    if IS.null (post g blockId) then
      if is_unresolved_indirection ctxt i then
        UnresolvedIndirection
      else if is_call (i_opcode i) || is_halt (i_opcode i) then
        Terminal
      else
        Normal
    else
      Normal






-- Fetching an instruction list given a block ID
fetch_block :: CFG -> Int -> [Instr]
fetch_block g blockId =
  case IM.lookup blockId $ cfg_instrs $ g of
    Nothing -> error $ "Block with ID" ++ show blockId ++ " not found in cfg."
    Just b -> b


show_block :: CFG -> Int -> String
show_block g b = 
  let instrs = im_lookup ("show_block: Block " ++ show b ++ "in cfg.") (cfg_blocks g) b in
       show b ++ " ["
    ++ showHex (head instrs)
    ++ ","
    ++ showHex (last instrs)
    ++ "]" 


show_invariants :: CFG -> Invariants -> String
show_invariants g invs = intercalate "\n\n" $ map show_entry $ IM.toList $ invs
 where
  show_entry (blockId, p) =  "Block " ++ show_block g blockId ++ ":\n" ++ show p


cfg_to_dot :: Context -> CFG -> String
cfg_to_dot ctxt g =
 let name  = ctxt_name ctxt
     frontier = IS.empty
     sccs     = scc_of g 0 frontier in
  "diGraph " ++ name ++ "{\n"
  ++ intercalate "\n" (map (node_to_dot sccs) $ IM.keys $ cfg_blocks g)
  ++ "\n\n"
  ++ intercalate "\n" (map edge_to_dot' $ IM.toList $ cfg_edges g)
  ++ "\n}"
 where
  node_to_dot sccs blockId =
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

  mk_node v = ctxt_name ctxt ++ "_" ++ showHex v

  node_shape blockId =
    case node_info_of ctxt g blockId of
      Normal -> "oval"
      Terminal -> "invhouse"
      UnresolvedIndirection -> "box3d"

hex_color_of vertex sccs =
  case findIndex (IS.member vertex) sccs of
    Just n -> hex_colors !! (126 - (floorDoubleInt $ 127 * int2Double n / int2Double (length sccs)))
    Nothing -> "#FFFFFF"
