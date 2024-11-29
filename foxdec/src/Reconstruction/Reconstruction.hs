{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StrictData #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : Reconstruction
Description : Reconstruct source-level constructs from the binary
-}



module Reconstruction.Reconstruction (reconstruct) where



-- TODO: introduce LEA with globalimmediate as imm + Top if not used yet
-- TODO: CMPSB and the likes with REP prefix
--
-- TODO in merged global overview, R*_0 makes no sense
-- TODO: (RSI_0 + 0x1a140), RSI_0 should not be considered a domain
--
--
-- gzip 0xb7e0
--  {0x50014,0x50028,0x5003c,0x50050} is accessed by same instruction, so group

{--
{0x1cb42,0x1cb46,0x1cb4a,0x1cb4e,0x1cb52}
<<0x1cb42>>
|
+-  [(TOP + 0x1cb42), 1] := Top
|
`- ¶[0x1cb42, 2] := 8
¶[0x1cb46, 2] := 8
--}


import Base
import Config

import OutputGeneration.Retrieval

import Analysis.Context hiding (PointerDomain,NodeInfo)
import Analysis.ControlFlow
import Analysis.FunctionNames
import Analysis.Pointers hiding (PointerDomain,get_pointer_domain)


import Generic.Instruction (GenericInstruction(Instruction))
import Generic.Operand (GenericOperand(..))
import Generic.Address
import Generic.HasSize (HasSize(..))

import Data.SymbolicExpression
--import Instantiation.SymbolicPropagation (ExternalFunctionOutput(..),ExternalFunctionBehavior(..), external_function_behavior)

import qualified X86.Operand as X86
import qualified X86.Instruction as X86
import qualified X86.Address as X86
import X86.Register
import X86.Opcode

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Tree.View as TV

import Data.List
import Data.Maybe
import Data.Word
import Data.Char (chr)
import Data.Functor ((<&>))
import Data.Bits (testBit)
import Data.Int (Int64)
import Data.Foldable (foldr')

import Control.Monad.State
import Control.Monad (forM_)
import Control.Monad.Extra (concatMapM)

import GHC.Base (compareInt)

import Debug.Trace

-- Function 0x8860, 0x4670, 0xb2b0 of gzip
--
-- Function 0x1309 of clientserver

-- 0x8290 of sha512sum has domains
--  ├╴<<RDI_0>>
--  └╴<<(RDI_0 + RSI_0)>>

reconstruct ctxt = do
   return () {--
  let entries    = ctxt_get_function_entries ctxt
  mems <- mapM (reconstruct_entry ctxt) $ S.toList entries

  return ()
  let fctxt      = mk_fcontext ctxt 0
  putStrLn $ "Overall joined result:"
  putStrLn $ show_smemory_html fctxt $ join_mems mems



reconstruct_entry ctxt entry = do
  let fctxt      = mk_fcontext ctxt entry
  let Just cfg   = IM.lookup entry (ctxt_cfgs ctxt)

  let tree       = evalState (dfs_spanning_tree ctxt cfg 0) IS.empty
  putStrLn $ "Entry: 0x" ++ showHex entry
  TV.drawTree $ fmap show tree

  let paths      = spanning_tree_to_cycles tree
  let paths'     = map (finish_path cfg) paths

  results       <- mapM (reconstruct_path fctxt cfg) paths'

  let mems = map (\(_,_,_,Invariant mem _) -> mem) results
  let mem' = join_mems mems
  putStrLn $ "Joined result:"
  putStrLn $ show_smemory_html fctxt mem'
  return $ prune_mem_to_only_globals mem'
 where
  reconstruct_path fctxt cfg path = do
    let asemantics = path_to_asemantics ctxt cfg path             -- [ASemantics]
    let (ras,inv)  = tau_path fctxt asemantics                    -- ([ResolvedAddresses], Invariant)
    let inv'       = widen_repeated_accesses fctxt asemantics ras inv   -- Invariant

    putStrLn $ show_result fctxt path asemantics ras inv'
    putStrLn $ "\n\n"
    return (path,asemantics,ras,inv')

  show_result fctxt path sems ras inv = intercalate "\n"
    [ show path
    , show_results [] $ zip sems ras
    , show_invariant fctxt inv ]




prune_mem_to_only_globals :: SMemory -> SMemory
prune_mem_to_only_globals (SMemory mem) = SMemory $ M.map dom_vars_to_top $ M.filterWithKey is_global mem
 where
  is_global (Bases bs) _ = any is_global_base bs
  is_global _          _ = False

  is_global_base (GlobalAddress _) = True
  is_global_base (BaseIsSymbol _)  = True
  is_global_base _                 = False

  dom_vars_to_top (SDomain accs) = SDomain $ nub $ map access_vars_to_top accs

  access_vars_to_top (SStorage a si val latest) = SStorage (vars_to_top a) si Top False

  vars_to_top (SE_Var _ )             = Bottom RockBottom
  vars_to_top (SE_StatePart _ _)      = Bottom RockBottom
  vars_to_top (SE_Op op si es)        = SE_Op op si $ map vars_to_top es
  vars_to_top (SE_Bit si e)           = SE_Bit si $ vars_to_top e
  vars_to_top (SE_SExtend l h e)      = SE_SExtend l h $ vars_to_top e
  vars_to_top (SE_Overwrite si e0 e1) = SE_Overwrite si (vars_to_top e0) (vars_to_top e1)
  vars_to_top e                       = e





show_results :: [ASemantics] -> [(ASemantics,ResolvedAddresses)] -> String
show_results _      [] = ""
show_results visited p@((sem,ras):path)
  | sem `elem` visited = "...\n" ++ show_results visited (dropWhile (\(sem,_) -> sem `elem` visited) path)
  | otherwise =
    let visited' = sem : visited
        ras'     = M.map nub $ M.unionsWith (++) $ map snd $ filter ((==) sem . fst) p in
      show_sem_ras (sem,ras') ++ "\n" ++ show_results visited' path





show_sem_ras (sem,ras) = pad_to 85 (show sem) ++ show_ras ras

show_ras :: ResolvedAddresses -> String
show_ras ras
  | M.null ras = ""
  | otherwise  = "// " ++ (intercalate " ;; " $ map show_entry $ M.toList ras)
 where
  show_entry (a,[a']) = show a ++ "==" ++ show a'
  show_entry (a,as')  = show a ++ "== {" ++ intercalate "," (map show as') ++ "}"


show_set l r s
  | S.null s       = l++r
  | S.size s  == 1 = S.findMin s
  | otherwise      = l ++ intercalate "," (S.toList s) ++ r





data NodeData = Unfinished Int | BlockIDs [Int]
  deriving Eq

instance Show NodeData where
  show (Unfinished b)    = show b ++ " ..."
  show (BlockIDs bs)     = show bs





----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- Some graph related functions
----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- Generate a spanning tree for the given CFG
dfs_spanning_tree :: Context -> CFG -> Int -> State IS.IntSet (T.Tree NodeData)
dfs_spanning_tree ctxt cfg blockID = do
  let nexts        = IM.lookup blockID (cfg_edges cfg)
  visited         <- get
  let is_visited   = blockID `IS.member` visited
  put $ IS.insert blockID visited
  
  if is_visited then
    return $ T.Node (Unfinished blockID) []
  else case IS.toList <$> nexts of
    Nothing  -> return $ T.Node (BlockIDs [blockID]) []
    Just []  -> return $ T.Node (BlockIDs [blockID]) []
    Just [b] -> add_to_root blockID <$> dfs_spanning_tree ctxt cfg b
    Just bs  -> T.Node (BlockIDs [blockID]) <$> (mapM (dfs_spanning_tree ctxt cfg) bs)

 where
  add_to_root blockID (T.Node (BlockIDs bs) t)        = T.Node (BlockIDs $ blockID:bs) t
  add_to_root blockID n@(T.Node (Unfinished _) [])    = T.Node (BlockIDs [blockID]) [n]

-- Given a spanning tree, generate paths that repeat cycles a certain number of times
spanning_tree_to_cycles :: T.Tree NodeData -> [[Int]]
spanning_tree_to_cycles = mk_paths []
 where
  mk_paths path (T.Node (Unfinished b) [])
    | b `elem` path = [path ++ (concat $ replicate 4 $ skipUntil b path)]
    | otherwise     = [path]

  mk_paths path (T.Node (BlockIDs bs) [])   = [path ++ bs]
  mk_paths path (T.Node (BlockIDs bs) nxts) = concatMap (mk_paths (path ++ bs)) nxts

-- Given a path, extend the path so that it reaches an end node
finish_path :: CFG -> [Int] -> [Int]
finish_path cfg p = 
  let finish = evalState (path_from_node_to_finish cfg $ last p) IS.empty in
    case finish of
      Nothing -> error $ "Cannot find path to finish."
      Just p' -> p ++ tail p'
 where
  path_from_node_to_finish :: CFG -> Int -> State IS.IntSet (Maybe [Int])
  path_from_node_to_finish cfg blockID = do
    let nexts        = IM.lookup blockID (cfg_edges cfg)
    visited         <- get
    let is_visited   = blockID `IS.member` visited
    put $ IS.insert blockID visited

    if is_visited then
      return Nothing
    else case IS.toList <$> nexts of
      Nothing -> return $ Just [blockID]
      Just [] -> return $ Just [blockID]
      Just bs -> do
        path <- find_path_from_blocks bs 
        return $ ((:) blockID) <$> path

  find_path_from_blocks []     = return Nothing
  find_path_from_blocks (b:bs) = do
    path <- path_from_node_to_finish cfg b
    case path of
      Nothing -> find_path_from_blocks bs
      Just p  -> return $ Just p
      
                  



-- TODO: move to base
skipUntil a [] = []
skipUntil a l@(b:bs)
  | a == b    = l
  | otherwise = skipUntil a bs





----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- Abstract Semantics
----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- For operations with a destination and one or more sources, the destination is the first SimpleExpr, the source(s) follow second.
-- The last to words for each are the instruction address and the size of the instruction.
data ASemantics =
    Call String Word64 Word64  -- ^ A call to a function
  | Ret Word64 Word64 -- ^ Return
  | Jump Word64 Word64 -- ^ A jump
  | Lea SimpleExpr SimpleExpr Word64 Word64 -- ^ Load Effective Addresss
  | Mov SimpleExpr SimpleExpr Word64 Word64 -- ^ MOV
  | MovZX SimpleExpr SimpleExpr Int Word64 Word64 -- ^ MOV
  | SExtend SimpleExpr Int SimpleExpr Int Word64 Word64 -- ^ Sign extension
  | SetXX SimpleExpr Word64 Word64 -- ^ SetXX functions (e.g., SETE, SETNE)
  | Apply Operator Int SimpleExpr [SimpleExpr] Word64 Word64 -- ^ A generic operator (e.g., ADD, XOR)
  | ApplyWhenImm Operator Int SimpleExpr [SimpleExpr] Word64 Word64 -- ^ A generic operator applied only when one argument is an immediate (e.g., AND, OR)
  | NoSemantics Opcode (Maybe SimpleExpr) [SimpleExpr] Word64 Word64 -- ^ No relevant semantics (e.g., floating points)
 deriving Eq


instance Show ASemantics where
  show (Call f rip _)                        = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "CALL") ++ f
  show (Ret rip _)                           = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "RET")
  show (Jump rip _)                          = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "JUMP")
  show (Lea  dst src rip _)                  = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "LEA") ++ show_dst_srcs dst [src]
  show (Mov  dst src rip _)                  = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "MOV") ++ show_dst_srcs dst [src]
  show (MovZX dst src _ rip _)               = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "MOVZX") ++ show_dst_srcs dst [src]
  show (SExtend dst h src l rip _)           = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "SEXT") ++ show_dst_srcs dst [src]
  show (SetXX dst rip _)                     = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "SETXX") ++ show_maybe_dst (Just dst) ++ "_"
  show (Apply op op_si dst src rip _)        = pad_to 10 ("0x" ++ showHex rip) ++ pad_to 11 (delim (show op)) ++ show_dst_srcs dst [src]
  show (ApplyWhenImm op op_si dst src rip _) = pad_to 10 ("0x" ++ showHex rip) ++ pad_to 11 (delim (show op)) ++ show_dst_srcs dst [src]
  show (NoSemantics op dst srcs rip _)       = pad_to 10 ("0x" ++ showHex rip) ++ pad_to 11 ("#" ++ delim (show op)) ++ show_maybe_dst dst ++ show_srcs srcs


pad_to n str
  | length str < n = str ++ replicate (n - length str) ' '
  | otherwise      = str

delim str = "<" ++ str ++ ">"

show_dst_srcs dst srcs    = show_maybe_dst (Just dst) ++ show_srcs srcs 
show_maybe_dst Nothing    = pad_to 23 "_" ++ "<- "
show_maybe_dst (Just dst) = pad_to 23 (show dst) ++ "<- "
show_srcs srcs            = intercalate "," (map show srcs)


-- here we map X86 mnemonics to abstract semantics
moves = 
  [ MOV
  , MOVSD
  , MOVSS 
  , MOVAPS
  , MOVAPD
  , MOVUPS
  , MOVUPD
  , MOVABS
  , MOVDQU
  , MOVDQA
  , MOVLPD
  , MOVD
  , MOVQ -- TODO if prefix = Nothing?
  , VMOVD
  , VMOVAPD
  , VMOVAPS
  ]

xors = 
  [ XOR
  , PXOR
  , VPXOR
  , XORPS
  , XORPD ]

sextends =
  [ MOVSX
  , MOVSXD
  , CDQE
  , CWDE
  , CBW ]

setxxs = 
  [ SETO  
  , SETNO
  , SETS 
  , SETNS
  , SETE 
  , SETZ 
  , SETNE
  , SETNZ
  , SETB 
  , SETNAE
  , SETC 
  , SETNB
  , SETAE
  , SETNC
  , SETBE
  , SETNA
  , SETA 
  , SETNBE
  , SETL 
  , SETNGE
  , SETG 
  , SETGE
  , SETNL
  , SETLE
  , SETNG
  , SETNLE
  , SETP 
  , SETPE
  , SETNP
  , SETPO ]

cmovs = 
  [ CMOVO   
  , CMOVNO 
  , CMOVS  
  , CMOVNS 
  , CMOVE  
  , CMOVZ  
  , CMOVNE 
  , CMOVNZ 
  , CMOVB  
  , CMOVNAE
  , CMOVC  
  , CMOVNB 
  , CMOVAE 
  , CMOVNC 
  , CMOVBE 
  , CMOVNA 
  , CMOVA  
  , CMOVNBE
  , CMOVL  
  , CMOVNGE
  , CMOVG  
  , CMOVGE 
  , CMOVNL 
  , CMOVLE 
  , CMOVNG 
  , CMOVNLE
  , CMOVP  
  , CMOVPE 
  , CMOVNP 
  , CMOVPO ]


-- | Given the address of an operand of an instruction, turn it to a symbolic expression.
address_to_expr :: X86.Address -> SimpleExpr
address_to_expr (AddressStorage r)   = SE_StatePart (SP_Reg r) Nothing
address_to_expr (AddressImm i)       = SE_Immediate i
address_to_expr (AddressMinus a0 a1) = SE_Op Minus 64 [address_to_expr a0, address_to_expr a1] 
address_to_expr (AddressTimes a0 a1) = SE_Op Times 64 [address_to_expr a0, address_to_expr a1] 
address_to_expr (AddressPlus  a0 a1) = SE_Op Plus  64 [address_to_expr a0, address_to_expr a1] 

-- | Given an operand of an instruction, turn it to a symbolic expression
operand_to_expr :: X86.Operand -> SimpleExpr
operand_to_expr (Storage r)          = SE_StatePart (SP_Reg r) Nothing
operand_to_expr (EffectiveAddress a) = simp $ address_to_expr a
operand_to_expr (Immediate i)        = SE_Immediate i
operand_to_expr (Memory a si)        = SE_StatePart (SP_Mem (simp $ address_to_expr a) si) Nothing


-- | Turn an X86 instruction into abstract semantics
instr_to_semantics :: Context -> X86.Instruction -> ASemantics
instr_to_semantics ctxt i@(Instruction _ _ LEA      (Just dst) [src] _)            = Lea (operand_to_expr dst) (operand_to_expr src) (X86.addressof i) (fromIntegral $ sizeof i)
instr_to_semantics ctxt i@(Instruction _ _ ADD      (Just dst) [src0,src1] _)      = mk_apply Plus   dst [src0,src1] i
instr_to_semantics ctxt i@(Instruction _ _ SUB      (Just dst) [src0,src1] _)      = mk_apply Minus  dst [src0,src1] i
instr_to_semantics ctxt i@(Instruction _ _ NEG      (Just dst) [src] _)            = mk_apply Minus  dst [Immediate 0,src] i
instr_to_semantics ctxt i@(Instruction _ _ INC      (Just dst) [src] _)            = mk_apply Plus   dst [src,Immediate 1] i
instr_to_semantics ctxt i@(Instruction _ _ DEC      (Just dst) [src] _)            = mk_apply Minus  dst [src,Immediate 1] i
instr_to_semantics ctxt i@(Instruction _ _ IMUL     (Just dst) [src0,src1] _)      = mk_apply Times  dst [src0,src1] i
instr_to_semantics ctxt i@(Instruction _ _ IMUL_LO  (Just dst) [src0,src1] _)      = mk_apply IMulLo dst [src0,src1] i
instr_to_semantics ctxt i@(Instruction _ _ IMUL_HI  (Just dst) [src0,src1] _)      = mk_apply IMulHi dst [src0,src1] i
instr_to_semantics ctxt i@(Instruction _ _ IDIV_LO  (Just dst) [src0,src1,src2] _) = mk_apply SdivLo dst [src0,src1,src2] i
instr_to_semantics ctxt i@(Instruction _ _ DIV_LO   (Just dst) [src0,src1,src2] _) = mk_apply UdivLo dst [src0,src1,src2] i
instr_to_semantics ctxt i@(Instruction _ _ IDIV_HI  (Just dst) [src0,src1,src2] _) = mk_apply SdivHi dst [src0,src1,src2] i
instr_to_semantics ctxt i@(Instruction _ _ DIV_HI   (Just dst) [src0,src1,src2] _) = mk_apply UdivHi dst [src0,src1,src2] i
instr_to_semantics ctxt i@(Instruction _ _ SHL      (Just dst) [src0,src1] _)      = mk_apply Shl    dst [src0,src1] i
instr_to_semantics ctxt i@(Instruction _ _ SHR      (Just dst) [src0,src1] _)      = mk_apply Shr    dst [src0,src1] i
instr_to_semantics ctxt i@(Instruction _ _ SAR      (Just dst) [src0,src1] _)      = mk_apply Sar    dst [src0,src1] i
instr_to_semantics ctxt i@(Instruction _ _ ADC      (Just dst) [src0,src1] _)      = mk_apply Adc    dst [src0,src1] i
instr_to_semantics ctxt i@(Instruction _ _ SBB      (Just dst) [src0,src1] _)      = mk_apply Sbb    dst [src0,src1] i
instr_to_semantics ctxt i@(Instruction _ _ CWD      (Just dst) [src] _)            = mk_apply (SExtHi (8 * (operand_size dst))) dst [src] i
instr_to_semantics ctxt i@(Instruction _ _ AND      (Just dst) [src0,src1] _)      = mk_apply_imm And dst [src0,src1] i
instr_to_semantics ctxt i@(Instruction _ _ OR       (Just dst) [src0,src1] _)      = mk_apply_imm Or  dst [src0,src1] i
instr_to_semantics ctxt i@(Instruction _ _ CDQ      (Just dst) [src] _)            = mk_apply (SExtHi (8 * (operand_size dst))) dst [src] i
instr_to_semantics ctxt i@(Instruction _ _ CQO      (Just dst) [src] _)            = mk_apply (SExtHi (8 * (operand_size dst))) dst [src] i

instr_to_semantics ctxt i@(Instruction _ _ MOVZX    (Just dst) [src] _)            = MovZX (operand_to_expr dst) (operand_to_expr src) (8*operand_size src) (X86.addressof i) (fromIntegral $ sizeof i)


instr_to_semantics ctxt i@(Instruction _ _ mnemonic dst srcs _)
  | isRet mnemonic            = Ret (X86.addressof i) (fromIntegral $ sizeof i)
  | isCall mnemonic           = Call (function_name_of_instruction ctxt i) (X86.addressof i) (fromIntegral $ sizeof i)
  | isJump mnemonic           = Jump (X86.addressof i) (fromIntegral $ sizeof i)
  | mnemonic `elem` moves     = Mov (operand_to_expr $ fromJust dst) (operand_to_expr (srcs!!0)) (X86.addressof i) (fromIntegral $ sizeof i)
  | mnemonic `elem` sextends  = SExtend (operand_to_expr $ fromJust dst) (8 * (operand_size $ fromJust dst)) (operand_to_expr (srcs!!0)) (8 * operand_size (srcs!!0))  (X86.addressof i) (fromIntegral $ sizeof i)
  | mnemonic `elem` setxxs    = SetXX (operand_to_expr $ fromJust dst) (X86.addressof i) (fromIntegral $ sizeof i)
  | mnemonic `elem` cmovs     = mk_apply Cmov (fromJust dst) [ srcs!!0 ,srcs!!1] i
  | mnemonic `elem` xors      =
    if srcs!!0 == srcs!!1 then
      Mov  (operand_to_expr $ fromJust dst) (SE_Immediate 0) (X86.addressof i) (fromIntegral $ sizeof i)
    else
      NoSemantics mnemonic (operand_to_expr <$> dst) (map operand_to_expr srcs) (X86.addressof i) (fromIntegral $ sizeof i)
  | otherwise                 = NoSemantics mnemonic (operand_to_expr <$> dst) (map operand_to_expr srcs) (X86.addressof i) (fromIntegral $ sizeof i)



mk_apply op dst srcs i = Apply op (8*operand_size dst) (operand_to_expr dst) (map operand_to_expr srcs) (X86.addressof i) (fromIntegral $ sizeof i)
mk_apply_imm op dst srcs i = ApplyWhenImm op (8*operand_size dst) (operand_to_expr dst) (map operand_to_expr srcs) (X86.addressof i) (fromIntegral $ sizeof i)


--TODO: BSR, ROl, ROR,BSWAP, PEXTRB/D/Q
-- TODO: OR, NOT
--TODO TEST
-- TODO: XADD
--TODO other sign extension thingies
--





operand_size (Storage r)          = sizeof r
operand_size (Memory a si)        = si
operand_size (EffectiveAddress _) = 8



-- | Turn a path in the CFG to a list of abstract instructions
path_to_asemantics :: Context -> CFG -> [Int] -> [ASemantics]
path_to_asemantics ctxt cfg = map (instr_to_semantics ctxt) . concatMap X86.canonicalize . concatMap toInstrs
 where
  toInstrs :: Int -> [X86.Instruction]
  toInstrs blockID = fromJust $ IM.lookup blockID (cfg_instrs cfg)








----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- Symbolic Execution
----------------------------------------------------------------------------
----------------------------------------------------------------------------

type Regs = M.Map Register (Maybe SimpleExpr)

data SStoredVal = Written SimpleExpr | Initial | Top 
  deriving (Eq,Ord)

data SAccess = SStorage SimpleExpr Int SStoredVal Bool | SRef SimpleExpr
  deriving (Eq)

data SDomain = SDomain [SAccess]

data SMemory = SMemory (M.Map PointerDomain SDomain)



join_mems :: [SMemory] -> SMemory
join_mems mems = SMemory $ M.unionsWith join_domains $ map (\(SMemory mem) -> mem) mems

join_domains :: SDomain -> SDomain -> SDomain
join_domains (SDomain accs0) (SDomain accs1) = SDomain $ foldr' insert_access accs1 accs0
 where
  insert_access acc0@(SStorage a0 si0 val0 latest0) accs1 =
    case partition (aliasses_with_access a0 si0) accs1 of
      ([SStorage a1 si1 val1 latest1],rest) -> SStorage a1 si1 (join_val val0 val1) False : rest
      ([],_) -> acc0:accs1

  join_val v0 v1
    | v0 == v1  = v0
    | otherwise = Top

instance Show SAccess where
  show acc@(SRef ptr)                   = "<" ++ show ptr ++ ">"
  show acc@(SStorage ptr si val latest) = show_latest ++ "[" ++ show ptr ++ ", " ++ show si ++ "] := " ++ show_val val latest
   where
    show_val Top         _     = "Top"
    show_val Initial     True  = "_"
    show_val Initial     False = "Top"
    show_val (Written v) True
      | is_initial acc         = "_"
      | otherwise              = "" ++ show v
    show_val (Written v) False = "Top"

    show_latest
     | latest    = [chr 182]
     | otherwise = " "

is_initial (SStorage ptr si Initial latest)     = latest
is_initial (SStorage ptr si Top     _)          = False
is_initial (SStorage ptr si (Written v) latest) = latest && v == SE_Var (SP_Mem ptr si)

is_latest (SStorage _ _ _ latest) = latest


compare_accesses fctxt (SStorage a0 si0 _ _) (SStorage a1 si1 _ _) = is_below fctxt (a0,si0) (a1,si1)
compare_accesses fctxt (SStorage a0 si0 _ _) (SRef a1)             = is_below fctxt (a0,si0) (a1,0)
compare_accesses fctxt (SRef a0)             (SStorage a1 si1 _ _) = is_below fctxt (a0,0)   (a1,si1)
compare_accesses fctxt (SRef a0)             (SRef a1)             = is_below fctxt (a0,0)   (a1,0)

is_below fctxt (a0,si0) (a1,si1) =
  let a0' = prune fctxt a0
      a1' = prune fctxt a1
      dirty_a0 = a0' /= a0
      dirty_a1 = a1' /= a1 in
    case distance a0' 0 a1' of
      Just d  -> if d==0 then
                   if dirty_a0 && not dirty_a1 then
                     LT
                   else if dirty_a1 && not dirty_a0 then
                     GT
                   else compare si1 si0
                 else if testBit d 63 then
                   GT
                 else
                   LT
      Nothing ->if a0'==a1' then compare si0 si1 else  compare a0' a1'


show_sdomain fctxt (SDomain accs) = remove_newlines $ T.drawForest $ groups_to_forest $ map (sortBy (compare_accesses fctxt)) $ group_domain fctxt $ sortBy (compare_accesses fctxt) accs
 where
  groups_to_forest = map group_to_child
  group_to_child group
    | length group > 1 = T.Node (mk_group_header (head group)) $ map mk_node group
    | otherwise        = mk_node $ head group

  mk_node acc = T.Node (show acc) []

  mk_group_header (SStorage a si _ _) = "<<" ++ show (prune fctxt a) ++">>"



show_smemory fctxt (SMemory mem) = remove_newlines $ intercalate "\n" $ map (show_sdomain fctxt) $ M.elems mem

show_smemory_html fctxt (SMemory mem) = TV.htmlTree Nothing $ T.Node header $ concatMap sdomain_to_forest $ M.elems mem
 where
  sdomain_to_forest (SDomain accs) = groups_to_forest $ map (sortBy (compare_accesses fctxt)) $ group_domain fctxt $ sortBy (compare_accesses fctxt) accs

  groups_to_forest = map group_to_child
  group_to_child group
    | length group > 1 = T.Node (mk_group_header (head group)) $ map mk_node group
    | otherwise        = mk_node $ head group

  mk_node acc = T.Node (TV.NodeInfo TV.InitiallyExpanded (show_saccess acc) "") []

  show_saccess (SStorage a si _ _) = "[" ++ show a ++ "," ++ show si ++ "]"

  header = TV.NodeInfo TV.InitiallyExpanded "" ""
  mk_group_header (SStorage a si _ _) = TV.NodeInfo TV.InitiallyExpanded ("<<" ++ show (prune fctxt a) ++ ">>") ""

  

remove_newlines []              = []
remove_newlines ('\n':'\n':str) = remove_newlines ('\n':str)
remove_newlines (c:str)         = c : remove_newlines str


insert_storage_into_domain :: FContext -> SimpleExpr -> Int -> State [SAccess] SAccess
insert_storage_into_domain fctxt a' si = do
  accs <- get
  case find (aliasses_with_access a' si) accs of
    Just n  -> return n 
    Nothing -> do
      let accs'       = SStorage a' si Initial True : accs
      let (touched,_) = runState (partition_domain_touched_by fctxt a' si) accs'
      -- TODO too simple if enclosure?
      let latest     = all is_latest touched
      let initial    = latest && all is_initial touched
      let val        = if initial then Initial else Top
      let storage    = SStorage a' si val (val /= Top && latest)
      put $ storage:accs
      return storage


insert_storage_into_mem fctxt msg a' si (SMemory mem) =
  let dom = get_pointer_domain fctxt $ prune fctxt a' in
    if dom == NoDomain then
      error $ show (a',si) ++ "\n" ++ show_smemory fctxt (SMemory mem)
    else
      let SDomain accs = M.findWithDefault (SDomain []) dom mem
          (acc,accs')  = runState (insert_storage_into_domain fctxt a' si) accs in
        (acc,SMemory $ M.insert dom (SDomain accs') mem)

group_domain fctxt [] = []
group_domain fctxt mem@((SStorage a0 si0 _ _):accs) =
  let (touched,not_touched) = runState (partition_domain_touched_by fctxt a0 si0) mem in
    touched : group_domain fctxt not_touched


partition_domain_touched_by :: FContext -> SimpleExpr -> Int -> State [SAccess] [SAccess]
partition_domain_touched_by fctxt a' si = do
  touched0     <- extract (overlapping_access a' si)
  mem <- get
  if touched0 == [] then error $ show (a',si,mem) else return ()
  not_touched0 <- get
  dirty_below  <- extract (is_dirty_below touched0 not_touched0)
  above        <- if is_dirty fctxt a' then go_contiguous_upwards_all touched0 else return []
  let ret       = concat [touched0,dirty_below,above]
  return ret
 where

  is_dirty_below touched0 not_touched0 acc@(SRef _)              = False
  is_dirty_below touched0 not_touched0 acc@(SStorage a0 si0 _ _)
    | not (is_dirty fctxt a0) = False
    | otherwise =  
      let (touched,_) = runState (go_contiguous_upwards acc) not_touched0 in
        intersect touched touched0 /= []

  overlapping_access a' si (SRef _)              = False
  overlapping_access a' si (SStorage a0 si0 _ _) = (prune fctxt a',si) `overlaps` (prune fctxt a0,si0)


  overlapping_accesses (SStorage a0 si0 _ _) acc1 = overlapping_access a0 si0 acc1

  go_contiguous_upwards_all = concatMapM go_contiguous_upwards

  go_contiguous_upwards (SStorage a si _ _) = do
    above0 <- extract (is_contiguous_above a si)
    above1 <- extract (\acc1 -> any (overlapping_accesses acc1) above0)
    above2 <- concatMapM go_contiguous_upwards (above0 ++ above1)
    return $ concat [above0, above1, above2]
 
  is_contiguous_above a si (SRef _)              = False
  is_contiguous_above a si (SStorage a0 si0 _ _) =
    case distance (prune fctxt a) si (prune fctxt a0) of
      Just d  -> not (testBit d 63) && (fromIntegral d::Int64) < fromIntegral si
      Nothing -> False

is_dirty fctxt a = prune fctxt a /= a


extract f = do
  (yes,no) <- gets $ partition f 
  put no
  return yes



(a0,si0) `aliasses_with` (a1,si1) = si0==si1 && (a0==a1 || necessarily_equal a0 a1)

(a0,si0) `enclosed_in` (a1,si1)   = necessarily_enclosed a0 si0 a1 si1

(a0,si0) `encompasses` (a1,si1)   = necessarily_enclosed a1 si1 a0 si0

(a0,si0) `overlaps` (a1,si1)      = (a0,1) `enclosed_in` (a1,si1) || (a1,1) `enclosed_in` (a0,si0)


aliasses_with_access a' si (SStorage a0 si0 _ _) = (a',si) `aliasses_with` (a0,si0)

enclosed_in_access a' si (SStorage a0 si0 _ _) = (a',si) `enclosed_in` (a0,si0)

encompasses_access a' si (SStorage a0 si0 _ _) = (a',si) `encompasses` (a0,si0)



distance a si a' = 
  case simp $ SE_Op Minus 64 [a',SE_Op Plus 64 [a,SE_Immediate $ fromIntegral si]] of
    SE_Immediate imm -> Just imm
    _                -> Nothing



data Invariant = Invariant SMemory Regs

show_invariant fctxt (Invariant mem regs) = show_invariant_regs show "\n" regs ++ "\n" ++ show_smemory fctxt mem

get_mem :: State Invariant SMemory
get_mem = get <&> (\(Invariant mem _) -> mem)

get_regs :: State Invariant Regs
get_regs = get <&> (\(Invariant _ regs) -> regs)

modify_regs :: (Regs -> Regs) -> State Invariant ()
modify_regs f = modify (\(Invariant mem regs) -> Invariant mem (f regs))



read_top_from_statepart :: StatePart -> Regs -> SimpleExpr
read_top_from_statepart sp regs = do
  let Just (Just rip) = M.lookup RIP regs in
    SE_StatePart sp $ Just $ show rip


sread_mem :: FContext -> SimpleExpr -> SimpleExpr -> Int -> State Invariant SimpleExpr
sread_mem fctxt a a' si = do
  Invariant mem regs <- get

  let (st,mem') = insert_storage_into_mem fctxt "hallo2" a' si mem
  put $ Invariant mem' regs
  case st of
    (SStorage _ _ val latest) -> mk_val val latest

 where
  mk_val _           False = get_regs <&> (read_top_from_statepart $ SP_Mem a si)
  mk_val Top         _     = get_regs <&> (read_top_from_statepart $ SP_Mem a si)
  mk_val Initial     True  = return $ SE_Var $ SP_Mem a' si
  mk_val (Written v) True  = return $ v

  -- TODO?
  take_bytes si Top         = Top
  take_bytes si Initial     = Initial
  take_bytes si (Written v) = Written $ simp $ SE_Bit (8*si) v


swrite_mem :: FContext -> SimpleExpr -> Int -> Maybe SimpleExpr -> State Invariant ()
swrite_mem fctxt a' si v' = do
  -- 1.) insert region into memory model
  Invariant mem regs <- get
  let (_,SMemory mem') = insert_storage_into_mem fctxt ("hallo3"++show (a',si) ++ "\n" ++ show_invariant fctxt (Invariant mem regs) ) a' si mem

  let dom = get_pointer_domain fctxt $ prune fctxt a'
  let mem'' = M.adjust dom_write dom mem'
  put $ Invariant (SMemory mem'') regs
  
 where 
  dom_write (SDomain accs) = 
    let (touched,not_touched) = runState (partition_domain_touched_by fctxt a' si) accs
        tr = if length touched > 1 then trace ("\nTouched: " ++ show (a',si) ++ "\n" ++ show touched ++"\n") else id in
      SDomain $ map overwrite_access touched ++ not_touched

  overwrite_access acc@(SRef _) = acc
  overwrite_access acc@(SStorage a0 si0 val0 latest0) 
    | (a',si) `aliasses_with` (a0,si0)  = SStorage a0 si0 (mk_val v') True
    | (a',si) `enclosed_in` (a0,si0)    = SStorage a0 si0 Top False
    | (a',si) `encompasses` (a0,si0)    = SStorage a0 si0 Top False -- TODO use take_bytes
    | otherwise                         = SStorage a0 si0 Top False -- trace ("Overwriting: " ++ show (a',si,a0,si0)) $ 

  mk_val Nothing  = Top
  mk_val (Just e) = Written e




-- TODO separation should protect sensitive stack regions
-- ¶[(RSP_0 - 8), 8] := RBP_0
-- ¶[(RSP_0 - 16), 8] := R15_0
-- ¶[(RSP_0 - 24), 8] := R14_0
-- ¶[(RSP_0 - 32), 8] := R13_0
-- ¶[(RSP_0 - 40), 8] := R12_0
-- ¶[(RSP_0 - 48), 8] := RBX_0
-- ¶[(RSP_0 - 64), 8] := [(FS_0 + 40), 8]_0






data PointerDomain = Bases (S.Set PointerBase) | Sources (S.Set StatePart) | NoDomain
  deriving (Eq,Ord)

instance Show PointerDomain where
  show (Bases bs)     = show_set "{" "}" $ S.map show bs
  show (Sources srcs) = show_set "{" "}" $ S.map show srcs
  show NoDomain       = "UnknownDomain"
 
get_pointer_domain fctxt a' =
  let bases = get_pointer_base_set fctxt a' in
    if not $ S.null bases then
      Bases $ S.map globals_to_section_starts bases
    else
      let srcs = get_pointer_sources a' in
        if not $ S.null srcs then
          Sources srcs
        else if is_immediate a' then -- TODO remove this
          Bases $ S.singleton $ GlobalAddress $ from_immediate a'
        else
          NoDomain
 where
  from_immediate (SE_Immediate i) = i

  get_pointer_sources :: SimpleExpr -> S.Set StatePart
  get_pointer_sources (SE_Op Plus _ es)                = S.unions $ map get_pointer_sources es
  get_pointer_sources (SE_Op Minus _ (e:es))           = get_pointer_sources e
  get_pointer_sources (SE_Op And _ [e,SE_Immediate _]) = get_pointer_sources e
  get_pointer_sources e                                = get_pointer_src e

  get_pointer_src (SE_Var sp@(SP_Reg r))
    | sizeof r == 8 = S.singleton sp
    | otherwise     = S.empty
  get_pointer_src (SE_Var sp@(SP_Mem a si))
    | si == 8       = S.singleton sp
    | otherwise     = S.empty
  get_pointer_src _ = S.empty

  globals_to_section_starts (GlobalAddress a) =
    case find_section_for_address (f_ctxt fctxt) a of
      Just (_,_,a0,_,_) -> GlobalAddress a0
      Nothing -> error $ "No section for: " ++ show a'
  globals_to_section_starts b = b


has_pointer_domain fctxt a' = get_pointer_domain fctxt a' /= NoDomain


prune :: FContext -> SimpleExpr -> SimpleExpr
prune fctxt = prune'' fctxt $ SE_Immediate 0

prune_to_bot fctxt = prune'' fctxt $ Bottom RockBottom

prune'' :: FContext -> SimpleExpr -> SimpleExpr -> SimpleExpr
prune'' fctxt subst e =
  let e0 = prune' True e
      e1 = prune' False e in
    if e0 /= e1 && (if is_immediate e1 then expr_is_global_immediate (f_ctxt fctxt) e1 else has_pointer_domain fctxt e1) then
      e1
    else
      e0
 where
  prune' keepAnd (SE_Op Plus  si es)                  = simp $ SE_Op Plus  si $ map (prune' keepAnd) es
  prune' keepAnd (SE_Op Minus si (e:es))              = simp $ SE_Op Minus si $ (prune' keepAnd e:map prune_keep_only_imms es)
  prune' keepAnd (SE_Op And   si [e,SE_Immediate i])
    | keepAnd   = simp $ SE_Op And   si $ [prune' keepAnd e,SE_Immediate i]
    | otherwise = SE_Immediate 0
  prune' keepAnd (SE_Op _     _  _)                   = subst
  prune' keepAnd (SE_Immediate imm)                   = SE_Immediate imm
  prune' keepAnd e                                   
    | get_pointer_domain fctxt e == NoDomain = SE_Immediate 0
    | otherwise                              = e

  prune_keep_only_imms e@(SE_Immediate _) = e
  prune_keep_only_imms (SE_Op op si es)   = simp $ SE_Op op si $ map prune_keep_only_imms es
  prune_keep_only_imms _                  = subst





 
show_invariant_regs :: (a -> String) -> String -> M.Map Register (Maybe a) -> String
show_invariant_regs show_a delim = intercalate delim . map show_entry . M.assocs
 where
  show_entry (r,v)    = show r ++ " == " ++ show_maybe_expr v
  show_maybe_expr Nothing  = "UNKNOWN"
  show_maybe_expr (Just e) = show_a e






sread_reg :: Register -> State Invariant SimpleExpr
sread_reg r = do
  regs <- get_regs
  return $ do_read (real r) (sizeof r) regs
 where
  do_read rr 32 = get_value rr
  do_read rr 16 = simp . SE_Bit 128 . get_value rr
  do_read rr 8  = get_value rr
  do_read rr 4  = simp . SE_Bit 32 . get_value rr
  do_read rr 2  = simp . SE_Bit 16 . get_value rr
  do_read rr 1  = simp . SE_Bit 8  . get_value rr

  get_value rr regs = 
    case M.lookup rr regs of
      Nothing       -> SE_Var $ SP_Reg rr
      Just Nothing  -> read_top_from_statepart (SP_Reg rr) regs
      Just (Just v) -> v




contains :: SimpleExpr -> SimpleExpr -> Bool
contains e e0@(SE_Immediate _)               = e0==e
contains e e0@(SE_Var _)                     = e0==e
contains e e0@(SE_StatePart (SP_Reg r) _)    = e0==e
contains e e0@(SE_StatePart (SP_Mem a si) _) = (e0==e || contains e a)
contains e e0@(SE_Malloc a id)               = e0==e
contains e e0@(SE_Op op si es)               = (e0==e || any (contains e) es)
contains e e0@(SE_Bit n e')                  = (e0==e || contains e e')
contains e e0@(SE_SExtend l h e')            = (e0==e || contains e e')
contains e e0@(SE_Overwrite n e0' e1')       = (e0==e || contains e e0' || contains e e1')
contains e e0@(Bottom _)                     = e0==e



swrite_reg :: Register -> Maybe SimpleExpr -> State Invariant ()
swrite_reg r v' = do
  curr_v <- sread_reg r
  modify_regs $ do_write (real r) (sizeof r) curr_v
 where
  do_write rr 32 curr_v = M.insert rr v'
  do_write rr 16 curr_v = M.insert rr (simp <$> SE_Bit 128 <$> v')
  do_write rr 8  curr_v = M.insert rr v'
  do_write rr 4  curr_v = M.insert rr (simp <$> SE_Bit 32 <$> v')
  do_write rr 2  curr_v = M.insert rr (simp <$> SE_Overwrite 16 curr_v <$> SE_Bit 16 <$> v')
  do_write rr 1  curr_v = M.insert rr (simp <$> SE_Overwrite 8 curr_v <$> SE_Bit 16 <$> v')
  -- TODO writes to high bytes of lower 2 bytes




sread_statepart :: FContext -> StatePart -> State Invariant SimpleExpr
sread_statepart fctxt (SP_Reg r)    = sread_reg r
sread_statepart fctxt (SP_Mem a si) = do
  as' <- operand_address_to_resolved_exprs fctxt a
  case as' of
    Nothing  -> do
      s <- get
      error $ "Read from domainless pointer: " ++ show (SP_Mem a si) ++ "\n" ++ show_invariant fctxt s --  get_regs <&> (read_top_from_statepart $ SP_Mem a si)
    Just as' -> do
      rets <- nub <$> mapM do_read as'
      return $ foldr1 (\v0 v1 -> SE_Op Cmov (si*8) [v0,v1]) rets 
 where
  do_read a'
    | has_pointer_domain fctxt a' = sread_mem fctxt a a' si
    -- TODO check not needed
      



sresolve_expr :: FContext -> SimpleExpr -> State Invariant SimpleExpr
sresolve_expr fctxt e@(SE_Immediate _)        = return e
sresolve_expr fctxt e@(SE_Var _)              = return e
sresolve_expr fctxt   (SE_StatePart sp _)     = sread_statepart fctxt sp
sresolve_expr fctxt   (SE_Op op si es)        = (simp . SE_Op op si) <$> mapM (sresolve_expr fctxt) es
sresolve_expr fctxt   (SE_Bit n e)            = (simp . SE_Bit n) <$> sresolve_expr fctxt e
sresolve_expr fctxt   (SE_SExtend l h e)      = (simp . SE_SExtend l h) <$> sresolve_expr fctxt e
sresolve_expr fctxt   (SE_Overwrite n e0 e1)  = do
  e0' <- sresolve_expr fctxt e0
  e1' <- sresolve_expr fctxt e1
  return $ simp $ SE_Overwrite n e0 e1
sresolve_expr fctxt e@(Bottom _)              = return e


-- take @a@: the address as it occurs in the operand of an instruction.
-- For example: RAX + RBX*4 in the memory operand QWORD PTR [RAX + RBX*4]
-- Try to resolve this address to a symbolic value by reading its inputs.
operand_address_to_resolved_exprs :: FContext -> SimpleExpr -> State Invariant (Maybe [SimpleExpr])
operand_address_to_resolved_exprs fctxt a = do
  a'      <- sresolve_expr fctxt a
  let as'  = nub $ map simp $ unfold_cmovs a'

  if all (has_pointer_domain fctxt) as' then
    return $ Just as'
  else do
    rets <- try_operand_address_to_base a
    case rets of
      [] -> return Nothing
      _  -> return $ Just $ nub rets
 where
  try_operand_address_to_base :: SimpleExpr -> State Invariant [SimpleExpr]
  try_operand_address_to_base op = concat <$> (mapM (get_base op) $ M.assocs $ addends op)
  get_base op (SE_StatePart (SP_Reg r) _,1) = do
    a' <- sread_reg r
    -- TODO unfold cmovs here as well
    let bases = get_pointer_base_set fctxt a'
    if S.size bases == 1 then
      singleton . simp <$> mk_expr r a' op
    else
      return []
  get_base _ _ = return []

  mk_expr r a' e@(SE_StatePart sp@(SP_Reg r') id)
    | r' == r                      = return a'
    | otherwise                    = get_regs <&> (read_top_from_statepart sp)
  mk_expr r a' e@(SE_Immediate _)  = return e
  mk_expr r a' e@(SE_Op op si es)  = SE_Op op si <$> mapM (mk_expr r a') es



  unfold_cmovs :: SimpleExpr -> [SimpleExpr]
  unfold_cmovs (SE_Op Cmov si es)     = concatMap unfold_cmovs es
  unfold_cmovs (SE_Op op si es)       = map (SE_Op op si)    $ crossProduct (map unfold_cmovs es)
  unfold_cmovs (SE_Bit n e)           = map (SE_Bit n)       $ unfold_cmovs e
  unfold_cmovs (SE_SExtend l h e)     = map (SE_SExtend l h) $ unfold_cmovs e
  unfold_cmovs (SE_Overwrite n e0 e1) = map (\[e0',e1'] -> SE_Overwrite n e0' e1') $ crossProduct (map unfold_cmovs [e0,e1])
  unfold_cmovs e                      = [e]





swrite_dst :: FContext -> SimpleExpr -> Maybe SimpleExpr -> State Invariant ()
swrite_dst fctxt (SE_StatePart (SP_Reg r) _)    v' = swrite_reg r v'
swrite_dst fctxt (SE_StatePart (SP_Mem a si) _) v' = do
  as' <- operand_address_to_resolved_exprs fctxt a
  case as' of
    Just as' -> forM_ as' do_write
    Nothing  -> do
      inv <- get
      error $ "Writing to baseless address: " ++ show (a,si) ++ "\n" ++ show_invariant fctxt inv
 where
  do_write a'
    | has_pointer_domain fctxt a' = swrite_mem fctxt a' si (simp . SE_Bit (si*8) <$> v')
    -- TODO check not needed

sread_src :: FContext -> SimpleExpr -> State Invariant SimpleExpr
sread_src fctxt   (SE_StatePart sp _)          = sread_statepart fctxt sp
sread_src fctxt e@(SE_Immediate imm)           = return $ e
sread_src fctxt e@(SE_Overwrite n src0 src1)   = do
  src0' <- sread_src fctxt src0
  src1' <- sread_src fctxt src1
  return $ simp $ SE_Overwrite n src0' src1'
sread_src fctxt e                              = error $ "Reading from " ++ show e


widen_repeated_accesses :: FContext -> [ASemantics] -> [ResolvedAddresses] -> Invariant -> Invariant
widen_repeated_accesses fctxt sems ras = update_inv $ zip sems ras
 where
  update_inv []                inv = inv
  update_inv all@((sem,ras):_) inv =
    let (same,others) = partition ((==) sem . fst) all in
      if length same == 1 then
        update_inv others inv
      else
        let merged_ras    = M.map nub $ M.unionsWith (++) $ map snd same
            inv'          = foldr (update_inv_per_operand sem) inv $ M.toList merged_ras in
          update_inv others inv'

  update_inv_per_operand sem ((a,si),as') inv
    | length as' <= 1 = inv
    | otherwise       =
      let groups = non_trivial_distanced_groups as' in
        foldr (update_inv_for_group sem si) inv groups

  non_trivial_distanced_groups as' =
    let pruned_as' = map (prune fctxt) as'
        groups     = quotientByL hasDistance pruned_as' in
      filter (\group -> length group > 1) $ map nub groups

  update_inv_for_group sem si group inv =
    let group'             = sortBy smallerDistance group
        a'                 = simp $ SE_Op Plus 64 [head group', Bottom RockBottom]
        Invariant mem regs = inv
        (_,mem')           = insert_storage_into_mem fctxt "hallo1" a' si mem in
      trace ("\nWIDENING: " ++ show a')
        Invariant mem' regs


pruned_equal fctxt a0 a1 = prune fctxt a0 == prune fctxt a1

hasDistance a0 a1 = distance a0 0 a1 /= Nothing

smallerDistance a0 a1 = 
  case distance a0 0 a1 of 
    Just d -> if testBit d 63 then GT else if d == 0 then EQ else LT

type ResolvedAddresses = M.Map (SimpleExpr,Int) [SimpleExpr]

tau_path :: FContext -> [ASemantics] -> ([ResolvedAddresses], Invariant)
tau_path fctxt p =
  let init_invariant   = Invariant (SMemory M.empty) M.empty in
    runState (traverse 0 p) init_invariant
 where
  traverse :: Int -> [ASemantics] -> State Invariant [ResolvedAddresses]
  traverse n []      = return []
  traverse n (sem:p) = do
    set_rip (size_of sem + rip_of sem)

    resolved_ops <- gets $ resolved_operands sem

    -- regs <- get_regs
    tau fctxt n sem
    resolved_ops' <- traverse (n+1) p
    return $ resolved_ops : resolved_ops'
    -- return (prune_invariant_for_instruction sem regs:regs')
    -- return (M.empty:regs')


  resolved_operands sem inv = M.map nub $ M.unionsWith (++) $ map (resolve_operand inv) $ operands_of sem

  resolve_operand inv (SE_StatePart (SP_Mem a si) Nothing) = 
    let Just as' = evalState (operand_address_to_resolved_exprs fctxt a) inv in
      M.singleton (a,si) as'
  resolve_operand inv (SE_StatePart (SP_Reg r) Nothing) = M.empty
  resolve_operand inv (SE_Immediate _) = M.empty


prune_invariant_for_instruction :: ASemantics -> Regs -> Regs
prune_invariant_for_instruction sem = M.filterWithKey is_relevant
 where
  is_relevant r _ = real r `elem` map real (regs_of_sem sem)

  regs_of_sem (Apply op op_si dst srcs rip si)        = concatMap regs_of_op (dst:srcs)
  regs_of_sem (ApplyWhenImm op op_si dst srcs rip si) = concatMap regs_of_op (dst:srcs)
  regs_of_sem (Mov dst src rip si)                    = concatMap regs_of_op [dst,src]
  regs_of_sem (MovZX dst src _ rip si)                = concatMap regs_of_op [dst,src]
  regs_of_sem (SExtend dst _ src _ rip si)            = concatMap regs_of_op [dst,src]
  regs_of_sem (SetXX dst rip si)                      = concatMap regs_of_op [dst]
  regs_of_sem (NoSemantics op Nothing srcs rip si)    = concatMap regs_of_op srcs
  regs_of_sem (NoSemantics op (Just dst) srcs rip si) = concatMap regs_of_op (dst:srcs)
  regs_of_sem (Lea dst src rip si)                    = [] -- regs_of_expr src
  regs_of_sem (Call f rip si)                         = []
  regs_of_sem (Jump rip si)                           = []
  regs_of_sem (Ret rip si)                            = [RSP]

  regs_of_op (SE_StatePart (SP_Mem a si) _) = regs_of_expr a
  regs_of_op (SE_StatePart (SP_Reg r) _)    = []
  regs_of_op _                              = []

  

regs_of_expr :: SimpleExpr -> [Register]
regs_of_expr (SE_Immediate _)               = []
regs_of_expr (SE_Var _)                     = []
regs_of_expr (SE_StatePart (SP_Reg r) _)    = [r]
regs_of_expr (SE_StatePart (SP_Mem a si) _) = regs_of_expr a
regs_of_expr (SE_Op op si es)               = concatMap regs_of_expr es
regs_of_expr (SE_Bit n e)                   = regs_of_expr e
regs_of_expr (SE_SExtend l h e)             = regs_of_expr e
regs_of_expr (SE_Overwrite n e0 e1)         = concat [regs_of_expr e0,regs_of_expr e1]
regs_of_expr (Bottom _)                     = []




set_rip :: Word64 -> State Invariant ()
set_rip rip = swrite_reg RIP (Just $ SE_Immediate rip)


size_of (Call f rip si)                        = si
size_of (Ret rip si)                           = si
size_of (Jump rip si)                          = si
size_of (Lea  dst src rip si)                  = si
size_of (Mov  dst src rip si)                  = si
size_of (MovZX dst src _ rip si)               = si
size_of (SExtend dst h src l rip si)           = si
size_of (SetXX dst rip si)                     = si
size_of (Apply op op_si dst src rip si)        = si
size_of (ApplyWhenImm op op_si dst src rip si) = si
size_of (NoSemantics op dst srcs rip si)       = si

rip_of (Call f rip si)                        = rip
rip_of (Ret rip si)                           = rip
rip_of (Jump rip si)                          = rip
rip_of (Lea  dst src rip si)                  = rip
rip_of (Mov  dst src rip si)                  = rip
rip_of (MovZX dst _ src rip si)               = rip
rip_of (SExtend dst h src l rip si)           = rip
rip_of (SetXX dst rip si)                     = rip
rip_of (Apply op op_si dst src rip si)        = rip
rip_of (ApplyWhenImm op op_si dst src rip si) = rip
rip_of (NoSemantics op dst srcs rip si)       = rip


scall n f rip = external_behavior $ external_function_behavior f
 where
  external_behavior (ExternalFunctionBehavior _ (Input reg)) = do
    ret_val <- sread_reg reg
    swrite_reg RAX (Just ret_val)
  external_behavior _ = do
    let retval = SE_Malloc (Just rip) (Just $ f ++ "_" ++ show n)
    swrite_reg RAX (Just retval) -- TODO and XMM0?

--scall n f rip 
--  | "0x" `isPrefixOf` f || "indirection@" `isPrefixOf` f = swrite_reg RAX Nothing -- TODO
--  | otherwise                                            = external_call n rip f $ external_function_behavior f


external_call n rip f (ExternalFunctionBehavior _ FreshPointer) = do
  let fresh = SE_Malloc (Just rip) (Just $ f ++ "_" ++ show n)
  swrite_reg RAX (Just fresh)
external_call n rip f (ExternalFunctionBehavior _ UnknownReturnValue) = do
  swrite_reg RAX Nothing




tau :: FContext -> Int -> ASemantics -> State Invariant ()
tau fctxt n (Apply op op_si dst srcs rip si)          = do
  srcs' <- mapM (sread_src fctxt) srcs
  swrite_dst fctxt dst (Just $ simp $ SE_Op op op_si srcs')
tau fctxt n (ApplyWhenImm op op_si dst srcs rip si)   = do
  srcs' <- mapM (sread_src fctxt) srcs
  if any is_immediate srcs' then
    swrite_dst fctxt dst (Just $ simp $ SE_Op op op_si srcs')
  else
    swrite_dst fctxt dst Nothing  
tau fctxt n (Mov dst src rip si)                = do
  src' <- sread_src fctxt src
  swrite_dst fctxt dst (Just src') 
tau fctxt n (MovZX dst src op_si rip si)        = do
  src' <- sread_src fctxt src
  swrite_dst fctxt dst (Just $ simp $ SE_Bit op_si src') 
tau fctxt n (SExtend dst h src l rip si)        = do
  src' <- sread_src fctxt src
  swrite_dst fctxt dst (Just $ simp $ SE_SExtend l h src') 
tau fctxt n (Lea dst src rip si)                = do
  src' <- sresolve_expr fctxt src
  swrite_dst fctxt dst (Just src') 
tau fctxt n (NoSemantics op dst srcs rip si)       = do
  -- Note sources must be read, as reading can influence the memory model
  srcs' <- mapM (sread_src fctxt) srcs
  case dst of
    Nothing  -> return ()
    Just dst -> swrite_dst fctxt dst Nothing  
tau fctxt n (SetXX dst rip si)                  = swrite_dst fctxt dst $ Just $ SE_Op ZeroOne 8 []
tau fctxt n (Call f rip si)                     = scall n f rip 
tau fctxt n (Jump rip si)                       = return () -- TODO what if call to external function
tau fctxt n (Ret rip si)                        = do
  v' <- sread_statepart fctxt $ SP_Mem (SE_StatePart (SP_Reg RSP) Nothing) 8
  swrite_reg RIP (Just v') 




operands_of :: ASemantics -> [SimpleExpr]
operands_of (Apply op op_si dst srcs rip si)          = dst : srcs
operands_of (ApplyWhenImm op op_si dst srcs rip si)   = dst : srcs
operands_of (Mov dst src rip si)                      = [dst,src]
operands_of (MovZX dst src op_si rip si)              = [dst,src]
operands_of (SExtend dst h src l rip si)              = [dst,src]
operands_of (Lea dst src rip si)                      = []
operands_of (NoSemantics op Nothing srcs rip si)      = srcs
operands_of (NoSemantics op (Just dst) srcs rip si)   = dst:srcs
operands_of (SetXX dst rip si)                        = [dst]
operands_of (Call f rip si)                           = []
operands_of (Jump rip si)                             = []
operands_of (Ret rip si)                              = [ SE_StatePart (SP_Mem (SE_StatePart (SP_Reg RSP) Nothing) 8) Nothing ]

{--
 - {--
full_cfg_post ctxt (entry,blockID)
  | isCall (opcode i) || isJump (opcode i) = S.unions $ map jump_target_to_next $ resolve_jump_target ctxt i
  | otherwise                              =
    case post of
      Nothing   -> S.empty
      Just post -> within_current_cfg post
 where
  Just cfg = IM.lookup entry (ctxt_cfgs ctxt)
  post     = IM.lookup blockID (cfg_edges cfg)
  i        = 
    case IM.lookup blockID $ cfg_instrs cfg of
      Just is -> last is
      _       -> error $ show (entry,blockID)

  jump_target_to_next Unresolved           = S.empty
  jump_target_to_next (External sym)       =
    case post of
      Just post -> within_current_cfg post
      _         -> error $ show ("0x" ++ showHex entry,blockID,sym,i)
  jump_target_to_next (ImmediateAddress a) =
    case find_block_starting_at $ fromIntegral a of
      Just nxt -> S.singleton nxt

  find_block_starting_at a
    | isCall (opcode i) = find_outside_cfg a `orTry` find_inside_cfg a
    | otherwise         = find_inside_cfg a `orTry` find_outside_cfg a

  within_current_cfg = S.fromList . map (\nxt -> (entry,nxt)) . IS.toList


  -- search for a block outside of the current cfg
  find_outside_cfg a = (\a -> (a,0)) <$> (find ((==) a) (map fromIntegral $ IM.keys $ ctxt_calls ctxt))
  -- search for a block in the current cfg that starts at @a@, and if found, make a label for it
  find_inside_cfg a = (\(blockId,_) -> (entry,blockID)) <$> find (block_starts_at a) (IM.toList $ cfg_instrs cfg)

  block_starts_at a (blockId, instrs) = instrs /= [] && X86.addressof (head instrs) == fromIntegral a




full_cfg_vertices ctxt entry =
  let Just cfg = IM.lookup entry (ctxt_cfgs ctxt) in
    S.fromList $ map (\nxt -> (entry,nxt)) $ IM.keys $ cfg_blocks cfg
--}



dfs_spanning_forest :: IntGraph g => g -> [Int] -> State IS.IntSet [T.Tree (Maybe Int)]
dfs_spanning_forest g []     = return []
dfs_spanning_forest g (v:vs) = do
  visited         <- get
  let is_visited = v `IS.member` visited
  if is_visited then
    dfs_spanning_forest g vs
  else do
    tree  <- dfs_spanning_tree g v
    trees <- dfs_spanning_forest g vs
    return $ tree:trees

--}
--}
