{-# LANGUAGE FlexibleContexts, DeriveGeneric, StandaloneDeriving, StrictData #-}


module OutputGeneration.Retrieval where

import Base

import Data.SValue
import Data.SymbolicExpression

import Analysis.Context
import Analysis.Pointers 
import Analysis.ControlFlow

import Instantiation.SymbolicPropagation (get_invariant)

import Generic.SymbolicConstituents
import Generic.HasSize (HasSize(sizeof))
import Generic.Instruction (GenericInstruction(..))
import Generic.Operand
import X86.Instruction


import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List 
import Data.Foldable
import Data.Word
import Control.Monad.State.Strict
import System.Exit (die)
import Debug.Trace

-- | Retrieves a set of instructions.
ctxt_get_instructions :: Context -> S.Set Instruction
ctxt_get_instructions ctxt =
  let entries = ctxt_get_function_entries ctxt
      cfgs    = map (\entry -> IM.lookup entry $ ctxt_cfgs ctxt) $ S.toList entries
      instrs  = map (\(Just cfg) -> concat $ IM.elems $ cfg_instrs cfg) cfgs in
    if length entries /= IM.size (ctxt_cfgs ctxt) then
      error $ show (length entries, length cfgs)
    else
      S.unions $ map S.fromList instrs



-- | Retrieves a set of funtion entries.
ctxt_get_function_entries :: Context -> S.Set Int
ctxt_get_function_entries = S.fromList . IM.keys . ctxt_calls


-- | Retrieves function summaries (preconditions, postconditions)
ctxt_get_function_summary ctxt entry =
  let finit = ctxt_finits ctxt IM.! entry
      post  = IM.lookup entry $ ctxt_calls ctxt in
    (fromIntegral entry,(finit,post))



{--
ctxt_disassemble_address :: Context -> Word64 -> IO (Instruction,String)
ctxt_disassemble_address ctxt a = do
  let entries = ctxt_get_function_entries ctxt
      cfgs    = map (\entry -> (entry,IM.lookup entry (C.ctxt_cfgs ctxt))) $ S.toList entries
      instrs  = map (\(entry,Just cfg) -> (entry,concat $ IM.elems $ C.cfg_instrs cfg)) cfgs


  i <- ctxt_get_instruction ctxt $ fromIntegral a
  return $ (mk_json_instruction i, C.pp_instruction ctxt i)
--}



-- | Retrieves a textual representation of the function boundaries
ctxt_mk_function_boundary ctxt entry =
  let cfg = ctxt_cfgs ctxt IM.! entry
      addresses = concat $ IM.elems $ cfg_blocks cfg in
    (fromIntegral entry, intercalate "\n" $ map show_chunk $ mk_consecutive_chunks addresses)
 where
  show_chunk [i]   = showHex i ++ " (single instruction)"
  show_chunk chunk = showHex (head chunk) ++ "-->" ++ showHex (last chunk)

  mk_consecutive_chunks :: [Int] -> [[Int]]
  mk_consecutive_chunks = split_consecutives . sort

  split_consecutives :: [Int] -> [[Int]]
  split_consecutives []         = []
  split_consecutives [i]        = [[i]]
  split_consecutives (i0:i1:is) = if i1 < i0 + 16 then add_to_hd i0 $ split_consecutives (i1:is) else [i0] : split_consecutives (i1:is)


  add_to_hd :: Int -> [[Int]] -> [[Int]]
  add_to_hd i []       = [[i]]
  add_to_hd i (is:iss) = (i:is) : iss



-- | Given an address @a@, retrieve all invariants (the address may occur in multiple functions).
ctxt_get_inv :: Context -> Word64 -> (Word64,Maybe [(Word64,Predicate)])
ctxt_get_inv ctxt a = do
  let entries = ctxt_get_function_entries ctxt
      invs    = map get_invariant_per_entry $ S.toList entries in
    if invs == [] || all ((==) Nothing) invs then do
      (a,Nothing)
    else
      (a,Just $ catMaybes invs)
 where
  get_invariant_per_entry entry =
    let fctxt = mk_fcontext ctxt entry in
     case get_invariant fctxt $ fromIntegral a of
       Nothing -> Nothing
       Just inv -> Just (fromIntegral entry,inv)






-- | Retrieve an overview of memory operands
-- Returns per instruction a tuple (entry, addr, ptrs).
-- Here:
--   entry is the entry address of the function of the instruction,
--   addr is the address of the instruction
--   ptrs is a list of symbolic pointers for each operand (or Nothing if not a memory-operand)
ctxt_resolve_mem_operands :: Context -> [(Word64,Word64, [Maybe SValue])]
ctxt_resolve_mem_operands ctxt = 
  let entries = ctxt_get_function_entries ctxt
      cfgs    = map (\entry -> (entry,IM.lookup entry $ ctxt_cfgs ctxt)) $ S.toList entries
      instrs  = map (\(entry,Just cfg) -> (entry,concat $ IM.elems $ cfg_instrs cfg)) cfgs in
    concatMap resolve_mem_operands instrs
 where
  resolve_mem_operands (entry,instrs) = map (resolve_mem_operand entry) instrs

  resolve_mem_operand entry i = (fromIntegral entry, addressof i, map (resolve entry i) $ get_operands i)

  resolve entry i (Memory a si) = 
    let fctxt   = mk_fcontext ctxt entry
        Just p  = get_invariant fctxt (fromIntegral $ addressof i)
        ptr     = evalState (sset_rip fctxt i >> sresolve_address fctxt a) (p,S.empty) in
      --(if ptr == Top then trace ("TOP(U): " ++ show i ++ "\n" ++ show p) else id) $ Just ptr
      Just ptr
  resolve entry i _ = Nothing

  get_operands i = 
    case dest $ head $ canonicalize i of
      Nothing  -> []--srcs i
      Just dst -> [dst]-- : srcs i




-- | Retrieve instruction for a given instruction address, both as datastructure and pretty-printed
ctxt_get_instruction :: Context -> Int -> IO Instruction
ctxt_get_instruction ctxt a = do
  i <- fetch_instruction ctxt $ fromIntegral a
  case i of
    Nothing -> die $ "Could not disassemble instruction at address: " ++ showHex a
    Just i  -> return i


-- | Retrieve all instruction addresses.
ctxt_get_instruction_addresses :: Context -> S.Set Word64
ctxt_get_instruction_addresses ctxt =
  S.map fromIntegral $ S.unions $ map cfg_to_addresses $ IM.elems $ ctxt_cfgs ctxt
 where
  cfg_to_addresses g = S.fromList $ concat $ IM.elems $ cfg_blocks g

-- | Given an address @a@, retrieve the set of next addresses.
ctxt_get_controlflow :: Context -> Word64 -> IO (Word64,[Word64])
ctxt_get_controlflow ctxt a = do
  let entries = ctxt_get_function_entries ctxt
  posts   <- mapM get_post_per_entry $ S.toList entries
  return $ (fromIntegral a,map fromIntegral $ IS.toList $ IS.unions posts)
 where
  get_post_per_entry entry = do
    post <- stepA ctxt get_invariant entry $ fromIntegral a
    case post of
      Left _     -> return $ IS.empty
      Right nxts -> return $ IS.fromList $ map fst nxts


-- | Return a set of all CFGs
-- Returns a mapping from function entries to CFGs.
ctxt_get_cfgs :: Context -> IM.IntMap String
ctxt_get_cfgs ctxt = do
  let entries = ctxt_get_function_entries ctxt 
      cfgs    = ctxt_cfgs ctxt in
    IM.mapWithKey cfg_with_instrs cfgs
 where
  cfg_with_instrs entry cfg = "ENTRY:" ++ showHex entry ++ "\n\n" ++ intercalate "\n" (map show_block $ IM.elems $ cfg_instrs cfg)

  show_block instrs = intercalate "\n" $ map (pp_instruction ctxt) instrs
