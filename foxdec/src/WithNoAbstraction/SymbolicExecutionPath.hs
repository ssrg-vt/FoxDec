{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StrictData #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : SymbolicExecutionPath
Description : Symbolic execution of paths ina control flow graph
-}



module WithNoAbstraction.SymbolicExecutionPath where


{--
This module exposes function symbolically_execute_path that takes as input a @path@
and symbolically executes it. A path is given as a list of blockIDs (ints) of basic blocks.

As an example in wc_small, for function entry 0x1504, a path is [0,1,3,4,9,5,6,11,18,12,13,14,16,20,21].
This produces:

[0,1,3,4,9,5,6,11,18,12,13,14,16,20,21]
0x1504    <NOP>
0x1508    <PUSH>     RBP                                        // RBP==RBP_0
0x1509    <MOV>      RBP                    <- RSP              // RSP==(RSP_0 - 8)
0x150c    <->        RSP                    <- RSP,32           // RSP==(RSP_0 - 8)
0x1510    <MOV>      [(RBP - 24), 8]        <- RDI              // RDI==RDI_0 ;; (RBP - 24)==(RSP_0 - 32)
0x1514    <MOV>      [(RBP - 4), 4]         <- 0                // (RBP - 4)==(RSP_0 - 12)
0x151b    <MOV>      RAX                    <- [(RBP - 24), 8]  // (RBP - 24)==(RSP_0 - 32) ;; [(RBP - 24), 8]==RDI_0
0x151f    <MOV>      RDI                    <- RAX              // RAX==RDI_0
etc.
etc.

This shows per instruction the semantics that were executed with destination (left op <-) and source operands (right of ->).
As comment, it shows for each source operand to what value it was resolved.
For all memory operands (including destination) it shows to what values the addresses were resolved.

Consider the MOV at 0x151f. Source operand RAX holds the initial value of RDI (denoted with RDI_0).
At 0x151b, address RBP-24 resolves to RSP_0-32 and the memory operand resolved to RDI_0.
--}

import Base hiding (show_set)
import Config
import Conventions

import Algorithm.Graph
import OutputGeneration.PathGeneration 

import WithAbstractPredicates.ControlFlow
import Binary.FunctionNames
import WithNoAbstraction.Pointers hiding (PointerDomain,get_pointer_domain)
import WithNoAbstraction.SymbolicExecution hiding (CSemantics(..),scall,sjump)
import WithAbstractSymbolicValues.Class hiding (scall,sjump)
import WithNoAbstraction.Lifted

import Data.SymbolicExpression hiding (show_srcs,swrite_mem)
import Binary.Generic

import Data.L0
import Data.X86.Instruction
import Data.X86.Opcode
import Data.X86.Register
import Data.Symbol
import Data.CFG
import Data.Size
import Data.SValue hiding (Top)
import Data.SPointer
import Data.JumpTarget


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
import Data.Foldable (foldr',foldlM,foldrM)

import Control.Monad.State.Strict
import Control.Monad (forM_)
import Control.Monad.Extra (concatMapM)

import GHC.Base (compareInt)

import Debug.Trace






-- For each entry: produce a set of paths and symbolically execute
symb_exec_all_entries bin config l0 = do
  let l       = (bin,config,l0)
  let entries = S.fromList $ IM.keys $ l0_functions l0 
  let funcs   = exported_functions l
  mapM_ (symb_exec_entry l) $ entries -- S.intersection entries funcs
  -- mems <- mapM (symb_exec_entry (bin,config,l0)) entries
  --putStrLn $ "Overall joined result:"
  --putStrLn $ show_smemory_html (bin,config,l0) $ join_mems mems



exported_functions l@(bin,config,l0) = S.difference (S.fromList $ map fst $ binary_get_exported_functions bin) $ externals bin l0
 where
  externals bin l0 = S.fromList $ IM.keys $ IM.filter is_relocation $ binary_get_symbol_table bin
  is_relocation (PointerToExternalFunction str)  = str /= ""
  is_relocation (PointerToObject str ex _ _) = ex && str /= ""
  is_relocation (AddressOfObject str ex) = ex && str /= ""
  is_relocation (AddressOfLabel str ex) = ex && str /= ""
  is_relocation _ = False



-- For the given entry: produce a set of paths and symbolically execute
symb_exec_entry (bin,config,l0) entry = do
  let ctxt = (bin,config,l0,fromIntegral entry)
  paths   <- produce_set_of_paths' ctxt 4
  results <- mapM (\path -> symbolically_execute_path ctxt path Nothing init_symstate) $ catMaybes paths

  let inputs = S.unions $ map (\(_,_,_,ins) -> ins) results
  putStrLn $ "0x" ++ showHex entry ++ ": " ++ (function_name_of_entry bin $ fromIntegral entry)
  putStrLn $ show $ S.toList $ S.unions $ S.map toInputRegs $ inputs

  --let mems = map (\(_,_,SymState mem _) -> mem) results
  --let mem' = join_mems mems
  --putStrLn $ "Joined result:"
  --putStrLn $ show_smemory_html l0' mem'
  --return $ prune_mem_to_only_globals mem'
  return ()

 where
  toInputRegs (SP_Reg r)
    | real_reg r `notElem` [Reg64 RSP,RegSeg FS] = S.singleton r
    | otherwise = S.empty
  toInputRegs (SP_Mem a si) = S.unions $ map (toInputRegs . SP_Reg) $ regs_of_expr a

-- For the given entry: produce a set of paths
-- We compute the spanning tree thorugh a depth-first-search.
-- Given that spanning tree, we repeat some cycles a couple of times.
-- Then we finish the path to some exit-point.
-- TODO use produce_set_of_paths from Algorithm.Graph
produce_set_of_paths' ctxt@(bin,config,l0,entry) repetition = do
  let Just (_,Just result) = IM.lookup (fromIntegral entry) (l0_functions l0)
  let cfg = result_cfg result
  let tree = evalState (dfs_spanning_tree cfg 0) IS.empty
  putStrLn $ "Entry: 0x" ++ showHex entry
  TV.drawTree $ fmap show tree
  let paths  = spanning_tree_to_cycles repetition tree
  let paths' = map (finish_path cfg) paths
  return paths'




init_symstate = SymState (SMemory M.empty) M.empty M.empty

get_symstate (_,_,symstate,_) = symstate

symb_exec_nested_path :: Lifted -> NestedPath -> SymState -> IO ([ASemantics], [ResolvedOperands], SymState, S.Set StatePart)
symb_exec_nested_path l (NestedPath p) symstate = foldlM go ([],[],symstate,S.empty) p
 where
  go (sems0,ras0,symstate0,inputs0) npe = do
    (sems1,ras1,symstate1,inputs1) <- symb_exec_nested_path_element l symstate0 npe
    return $ (sems0 ++ sems1,ras0 ++ ras1,symstate1,S.union inputs0 inputs1)
    


symb_exec_nested_path_element :: Lifted -> SymState -> NestedPathElement -> IO ([ASemantics], [ResolvedOperands], SymState, S.Set StatePart)
symb_exec_nested_path_element l@(bin,config,l0) symstate (CallReturn entry0 entry1 np) = do
  let l' = (bin,config,l0,fromIntegral entry1)
  (sems,ras,symstate1,inputs) <- symb_exec_nested_path l np symstate
  let sysmstate2 = clean_below_current_stackframe l' symstate1
  return (sems,ras,sysmstate2,inputs)
symb_exec_nested_path_element l@(bin,config,l0) symstate (PathWithinFunction entry [] Nothing False) = return $ ([],[],symstate,S.empty)
symb_exec_nested_path_element l@(bin,config,l0) symstate (PathWithinFunction entry p n True) = symbolically_execute_path (bin,config,l0,fromIntegral entry) p n symstate
symb_exec_nested_path_element l symstate p = error $ show $ NestedPath [p]







symb_exec_transiting_path :: Lifted -> TransitingPath -> StateT SymState IO ([ASemantics], [ResolvedOperands], S.Set StatePart)
symb_exec_transiting_path l p@(TransitingPath [] Nothing)      = return ([],[],S.empty)
symb_exec_transiting_path l p@(TransitingPath [] (Just (b,n))) = symb_exec_internal_path l [b] (Just n)
symb_exec_transiting_path l p@(TransitingPath _ end)           = do
  let (same,others) = split_transiting_path p
  (sems0,ras0,inputs0) <- symb_exec_internal_path l same Nothing
  symb_exec_function_return l (last same)
  (sems1,ras1,inputs1) <- symb_exec_transiting_path l $ TransitingPath others end
  return $ (sems0 ++ sems1,ras0 ++ ras1,S.union inputs0 inputs1)
    

symb_exec_internal_path (bin,config,l0) p@(FunctionBlockID entry _:_) maybeN = do
  let l' = (bin,config,l0,fromIntegral entry)
  symstate <- get
  let p' = map (\(FunctionBlockID _ blockID) -> blockID) p
  (sems,ras,symstate',inputs) <- liftIO $ symbolically_execute_path l' p' maybeN symstate  
  put symstate'
  return (sems,ras,inputs)


symb_exec_function_return l@(bin,config,l0) (FunctionBlockID entry blockID)
  | block_ends_in_ret l entry blockID = do
    let l' = (bin,config,l0,fromIntegral entry)
    modify $ clean_below_current_stackframe l'
  | otherwise = return ()




init_sym_state_with :: Register -> Word64 -> SymState
init_sym_state_with reg value = execState (swrite_reg reg (Just $ SE_Immediate value)) init_symstate


read_RIP symstate =
  let v = evalState (sread_reg (Reg64 RIP)) symstate in
    case v of
      SE_Immediate a -> a

-- Tries to follow a concrete path, where each jump and conditional jump is evlauted deterministically.
-- Errors out if it cnanot decide where some jump leads to.
symbolically_execute_until :: LiftedWithEntry -> Int -> Int -> IS.IntSet -> SymState -> IO (S.Set SymState)
symbolically_execute_until l@(bin,config,l0,entry) count a as symstate
  | a `IS.member` as = return $ S.singleton symstate
  | count >= 100     = error $ show_symstate l symstate
  | otherwise        = do
    Just i          <- fetch_instruction bin $ fromIntegral a
    let sem          = instr_to_semantics l i
    let symstates'   = execState (run i sem count) (S.singleton symstate)
    symstates''     <- mapM (\symstate' -> symbolically_execute_until l (count+1) (fromIntegral $ read_RIP symstate') as symstate') $ S.toList symstates'
    return $ S.unions symstates''
 where
  run i sem count
    | isCondJump (inOperation i) = gets (S.unions . S.map (evalState (do_cond_jump i))) >>= put
    | isJump (inOperation i) = determinize $ do_jump sem
    | inOperation i `elem` [CMP,SUB] = determinize $ (set_flags sem >> do_normal_instr sem)
    | otherwise = determinize $ do_normal_instr sem

  determinize :: State SymState () -> State (S.Set SymState) ()
  determinize m = (gets $ S.map (execState m)) >>= put

  set_flags sem@(NoSemantics CMP _ [src0,src1] _ _) = set_CMP_flags sem src0 src1
  set_flags sem@(Apply Minus _ _ [src0,src1] _ _)   = set_CMP_flags sem src0 src1


  set_CMP_flags sem src0 src1 = do
    set_rip (size_of sem + rip_of sem)
    v0 <- sread_src l src0
    v1 <- sread_src l src1
    symstate <- get
    case (v0,v1) of
      (SE_Immediate imm0,SE_Immediate imm1) -> do
        set_flag "ZF" (imm0 == imm1)
        set_flag "CF" (imm0  < imm1)
      -- _ -> trace ("UNSETTING\n" ++  show_symstate l symstate) unset_all_flags
      _ -> unset_all_flags 


  sread_flag flg = do
    SymState regs mem flgs <- get
    return $ M.lookup flg flgs

  set_flag flg b = do
    SymState regs mem flgs <- get
    put $ SymState regs mem $ M.insert flg b flgs

  unset_all_flags = do
    SymState regs mem _ <- get
    put $ SymState regs mem M.empty

  do_normal_instr sem = do
      set_rip (size_of sem + rip_of sem)
      tau l count sem

  do_jump sem@(Jump rip src _ si) = do
    set_rip (size_of sem + rip_of sem)
    trgt <- sread_src l src
    case (src,trgt) of
      (SE_Immediate _, SE_Immediate a') -> set_rip (rip_of sem + a') 
      (_             , SE_Immediate a') -> set_rip a'
    

  do_cond_jump :: Instruction -> State SymState (S.Set SymState)
  do_cond_jump i = 
    case inOperation i of
      JNZ -> do
        zf <- sread_flag "ZF"
        case zf of
          (Just b0) -> do_jump_if (not b0) i 
          Nothing   -> do
            symstate <- get
            return $ S.fromList [execState (do_jump_if True i) symstate, execState (do_jump_if False i) symstate]
      JNBE -> do
        cf <- sread_flag "CF"
        zf <- sread_flag "ZF"
        case (cf,zf) of
          (Just b0,Just b1) -> do_jump_if (not b0 && not b1) i
      _ -> do
        symstate <- get
        error $ "unsupported conditional jump: " ++ show i ++ "\n" ++ show_symstate l symstate

  do_jump_if True  i@(Instruction a _ _ ops _ si) = do
    do_jump $ Jump a (operand_to_expr $ ops!!0) i (fromIntegral si)
    gets S.singleton
  do_jump_if False i@(Instruction a _ _ ops _ si) = do
    set_rip (a + fromIntegral si)
    gets S.singleton


-- SYMBOLIC EXECUTION MAIN FUNCTION
-- Step 1: assign Abstract Semantics to the path
-- Step 2: symbolically execute the abstract semantics
-- Step 3: print out the results
--
-- Returns:
-- 1.) the abstract semantics of the path
-- 2.) per instruction, the resolved operands
-- 3.) the final symbolic state, after symbolically executing the entire path
symbolically_execute_path :: LiftedWithEntry -> [Int] -> Maybe Int -> SymState -> IO ([ASemantics], [ResolvedOperands], SymState, S.Set StatePart)
symbolically_execute_path ctxt@(bin,config,l0,entry) path n symstate = do
  let Just (_,Just result) = IM.lookup (fromIntegral entry) (l0_functions l0)
  let cfg = result_cfg result

  let asemantics = path_to_asemantics ctxt cfg n path
  let (ras,inv)  = tau_path ctxt asemantics symstate
  let inputs     = S.unions $ map resolved_operands_to_inputs $ zip asemantics ras

  --let inv'       = widen_repeated_accesses l0 asemantics ras inv

  --putStrLn $ show_result path asemantics ras inv inputs
  --putStrLn $ "\n\n"
  return (asemantics,ras,inv,inputs)
 where
  show_result path sems ras inv inputs = intercalate "\n"
    [ show path
    , show_results [] $ zip sems ras
    , show_symstate ctxt inv
    , "INPUTS:"
    , show $ S.toList inputs ]


show_results :: [ASemantics] -> [(ASemantics,ResolvedOperands)] -> String
show_results _      [] = ""
show_results visited p@((sem,ras):path)
  | sem `elem` visited = "...\n" ++ show_results visited (dropWhile (\(sem,_) -> sem `elem` visited) path)
  | otherwise =
    let visited' = sem : visited
        ras'     = M.map nub $ M.unionsWith (++) $ map snd $ filter ((==) sem . fst) p in
      show_sem_ras (sem,ras') ++ "\n" ++ show_results visited' path

show_sem_ras (sem,ras) = pad_to 85 (show sem) ++ show_ras ras

show_ras :: ResolvedOperands -> String
show_ras ras
  | M.null ras = ""
  | otherwise  = "// " ++ (intercalate " ;; " $ map show_entry $ M.toList ras)
 where
  show_entry (sp,[a']) = show_sp sp ++ "==" ++ show a'
  show_entry (sp,as')  = show_sp sp ++ "== {" ++ intercalate "," (map show as') ++ "}"

  show_sp sp@(SP_Reg r)    = show r
  show_sp sp@(SP_Mem a 0)  = show a
  show_sp sp@(SP_Mem a si) = show sp

show_set l r s
  | S.null s       = l++r
  | S.size s  == 1 = S.findMin s
  | otherwise      = l ++ intercalate "," (S.toList s) ++ r




resolved_operands_to_inputs :: (ASemantics, ResolvedOperands) -> S.Set StatePart
resolved_operands_to_inputs (Push _ _ _ _,_) = S.empty
resolved_operands_to_inputs (sem,ras) = S.unions $ concatMap get_stateparts $ M.elems ras
 where
  get_stateparts = map state_parts_of_expr

state_parts_of_expr (Bottom typ)         = S.empty
state_parts_of_expr (SE_Malloc _ _)      = S.empty
state_parts_of_expr (SE_Var sp)          = S.singleton sp
state_parts_of_expr (SE_Immediate _)     = S.empty
state_parts_of_expr (SE_StatePart sp _ ) = S.empty
state_parts_of_expr (SE_Op _ _ es)       = S.unions $ map state_parts_of_expr es
state_parts_of_expr (SE_Bit i e)         = state_parts_of_expr e
state_parts_of_expr (SE_SExtend _ _ e)   = state_parts_of_expr e
state_parts_of_expr (SE_Overwrite _ a b) = S.union (state_parts_of_expr a) (state_parts_of_expr b)




----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- Abstract Semantics
----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- For operations with a destination and one or more sources, the destination is the first SimpleExpr, the source(s) follow second.
-- The last two words for each are the instruction address and the size of the instruction.
data ASemantics =
    Call SimpleExpr Instruction Word64 Word64  -- ^ A call to a function
  | Ret Word64 Word64 -- ^ Return
  | Jump Word64 SimpleExpr Instruction Word64 -- ^ A jump
  | SysCall Word64 Word64
  | Nop Word64 Word64 -- ^ A NOP
  | Lea SimpleExpr SimpleExpr Word64 Word64 -- ^ Load Effective Addresss
  | Push SimpleExpr Int Word64 Word64 -- ^ Push
  | Pop SimpleExpr Int Word64 Word64 -- ^ Pop
  | Leave Word64 Word64 -- ^ Leave
  | SimpleExpr Int Word64 Word64 -- ^ Pop
  | Mov SimpleExpr SimpleExpr Word64 Word64 -- ^ MOV
  | MovZX SimpleExpr SimpleExpr Int Word64 Word64 -- ^ MOV with zero extension
  | SExtend SimpleExpr Int SimpleExpr Int Word64 Word64 -- ^ Sign extension
  | SetXX SimpleExpr Word64 Word64 -- ^ SetXX functions (e.g., SETE, SETNE)
  | Apply Operator Int SimpleExpr [SimpleExpr] Word64 Word64 -- ^ A generic operator (e.g., ADD, XOR)
  | ApplyWhenImm Operator Int SimpleExpr [SimpleExpr] Word64 Word64 -- ^ A generic operator applied only when one argument is an immediate (e.g., AND, OR)
  | NoSemantics Opcode (Maybe SimpleExpr) [SimpleExpr] Word64 Word64 -- ^ No relevant semantics (e.g., floating points)
 deriving Eq


instance Show ASemantics where
  show (Call src _ rip _)                     = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "CALL") ++ show_srcs [src]
  show (Jump rip src i _)                     = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "JUMP") ++ show_srcs [src]
  show (SysCall rip si)                       = pad_to 10 ("0x" ++ showHex rip) ++ "Syscall"
  show (Ret rip _)                            = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "RET")
  show (Nop rip _)                            = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "NOP")
  show (Push src _ rip _)                     = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "PUSH") ++ show_srcs [src]
  show (Pop dst _ rip _)                      = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "POP") ++ show_dst_srcs dst []
  show (Leave rip _)                          = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "LEAVE")
  show (Lea  dst src rip _)                   = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "LEA") ++ show_dst_srcs dst [src]
  show (Mov  dst src rip _)                   = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "MOV") ++ show_dst_srcs dst [src]
  show (MovZX dst src _ rip _)                = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "MOVZX") ++ show_dst_srcs dst [src]
  show (SExtend dst h src l rip _)            = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "SEXT") ++ show_dst_srcs dst [src]
  show (SetXX dst rip _)                      = pad_to 10 ("0x" ++ showHex rip) ++ (pad_to 11 $ delim "SETXX") ++ show_maybe_dst (Just dst) ++ "_"
  show (Apply op op_si dst srcs rip _)        = pad_to 10 ("0x" ++ showHex rip) ++ pad_to 11 (delim (show op)) ++ show_dst_srcs dst srcs
  show (ApplyWhenImm op op_si dst srcs rip _) = pad_to 10 ("0x" ++ showHex rip) ++ pad_to 11 (delim (show op)) ++ show_dst_srcs dst srcs
  show (NoSemantics op dst srcs rip _)        = pad_to 10 ("0x" ++ showHex rip) ++ pad_to 11 ("#" ++ delim (show op)) ++ show_maybe_dst dst ++ show_srcs srcs


pad_to n str
  | length str < n = str ++ replicate (n - length str) ' '
  | otherwise      = str

delim str = "<" ++ str ++ ">"

show_dst_srcs :: SimpleExpr -> [SimpleExpr] -> String
show_dst_srcs dst srcs    = show_maybe_dst (Just dst) ++ show_srcs srcs 
show_maybe_dst Nothing    = pad_to 23 "_" ++ "<- "
show_maybe_dst (Just dst) = pad_to 23 (show dst) ++ "<- "
show_srcs srcs            = intercalate "," (map show srcs)


-- Here we map X86 mnemonics to abstract semantics
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

nops = [NOP,ENDBR64]

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


-- | Concrete instructions are turned into abstract semantics
address_to_expr base indx scale displ (Just seg) = simp $ SE_Op Plus 64 [mk_seg, address_to_expr base indx scale displ Nothing ]
 where
  mk_seg = SE_StatePart (SP_Reg $ RegSeg seg) Nothing
address_to_expr base indx scale displ Nothing    = simp $ SE_Op Plus 64  [SE_Op Plus 64  [mk_base base, mk_mult indx], mk_displ]
 where
  mk_base RegNone = SE_Immediate 0
  mk_base _       = SE_StatePart (SP_Reg base) Nothing

  mk_mult RegNone = SE_Immediate 0
  mk_mult _       = SE_Op Times 64 [SE_StatePart (SP_Reg indx) Nothing, SE_Immediate $ fromIntegral scale ]

  mk_displ = SE_Immediate $ fromIntegral displ

-- | Given an operand of an instruction, turn it to a symbolic expression
operand_to_expr :: Operand -> SimpleExpr
operand_to_expr (Op_Reg r _) = SE_StatePart (SP_Reg r) Nothing
operand_to_expr (Op_Imm (Immediate _ i)) = SE_Immediate i -- TODO sextend?
operand_to_expr (Op_Mem si base indx scale displ seg info) = SE_StatePart (SP_Mem (address_to_expr base indx scale displ seg) (mk_si si)) Nothing
 where
  mk_si (BitSize si) = si `div` 8


-- | Turn a concrete instruction into abstract semantics
instr_to_semantics :: LiftedWithEntry -> Instruction -> ASemantics
instr_to_semantics l0 i@(Instruction _ _ LEA      [dst,src] _ _)       = Lea (operand_to_expr dst) (operand_to_expr src) (inAddress i) (fromIntegral $ inSize i)
instr_to_semantics l0 i@(Instruction _ _ ADD      [dst,src0] _ _)      = mk_apply Plus   dst [dst,src0] i
instr_to_semantics l0 i@(Instruction _ _ SUB      [dst,src0] _ _)      = mk_apply Minus  dst [dst,src0] i
instr_to_semantics l0 i@(Instruction _ _ NEG      [dst] _ _)           = mk_apply Minus  dst [Op_Imm $ Immediate (BitSize 64) 0,dst] i
instr_to_semantics l0 i@(Instruction _ _ INC      [dst] _ _)           = mk_apply Plus   dst [dst,Op_Imm $ Immediate (BitSize 64) 1,dst] i
instr_to_semantics l0 i@(Instruction _ _ DEC      [dst] _ _)           = mk_apply Minus  dst [dst,Op_Imm $ Immediate (BitSize 64) 1 ] i
instr_to_semantics l0 i@(Instruction _ _ IMUL     [dst,src0] _ _)      = mk_apply Times  dst [dst,src0] i
instr_to_semantics l0 i@(Instruction _ _ IMUL_LO  [dst,src0] _ _)      = mk_apply IMulLo dst [dst,src0] i
instr_to_semantics l0 i@(Instruction _ _ IMUL_HI  [dst,src0] _ _)      = mk_apply IMulHi dst [dst,src0] i
instr_to_semantics l0 i@(Instruction _ _ IDIV_LO  [dst,src0,src1] _ _) = mk_apply SdivLo dst [dst,src0,src1] i
instr_to_semantics l0 i@(Instruction _ _ DIV_LO   [dst,src0,src1] _ _) = mk_apply UdivLo dst [dst,src0,src1] i
instr_to_semantics l0 i@(Instruction _ _ IDIV_HI  [dst,src0,src1] _ _) = mk_apply SdivHi dst [dst,src0,src1] i
instr_to_semantics l0 i@(Instruction _ _ DIV_HI   [dst,src0,src1] _ _) = mk_apply UdivHi dst [dst,src0,src1] i
instr_to_semantics l0 i@(Instruction _ _ SHL      [dst,src0] _ _)      = mk_apply Shl    dst [dst,src0] i
instr_to_semantics l0 i@(Instruction _ _ SHR      [dst,src0] _ _)      = mk_apply Shr    dst [dst,src0] i
instr_to_semantics l0 i@(Instruction _ _ SAR      [dst,src0] _ _)      = mk_apply Sar    dst [dst,src0] i
instr_to_semantics l0 i@(Instruction _ _ ADC      [dst,src0] _ _)      = mk_apply Adc    dst [dst,src0] i
instr_to_semantics l0 i@(Instruction _ _ SBB      [dst,src0] _ _)      = mk_apply Sbb    dst [dst,src0] i
instr_to_semantics l0 i@(Instruction _ _ AND      [dst,src0] _ _)      = mk_apply_imm And dst [dst,src0] i
instr_to_semantics l0 i@(Instruction _ _ OR       [dst,src0] _ _)      = mk_apply_imm Or  dst [dst,src0] i
instr_to_semantics l0 i@(Instruction _ _ CDQ      [dst,src] _ _)       = mk_apply (SExtHi (operand_size_bits dst)) dst [src] i
instr_to_semantics l0 i@(Instruction _ _ CQO      [dst,src] _ _)       = mk_apply (SExtHi (operand_size_bits dst)) dst [src] i
instr_to_semantics l0 i@(Instruction _ _ CWD      [dst,src] _ _)       = mk_apply (SExtHi (operand_size_bits dst)) dst [dst] i

instr_to_semantics l0 i@(Instruction _ _ MOVZX    [dst,src] _ _)       = MovZX (operand_to_expr dst) (operand_to_expr src) (operand_size_bits src) (inAddress i) (fromIntegral $ inSize i)

instr_to_semantics l0 i@(Instruction _ _ mnemonic [] _ _)
  | isRet mnemonic            = Ret (inAddress i) (fromIntegral $ inSize i)
  | isSyscall mnemonic        = SysCall (inAddress i) (fromIntegral $ inSize i)
  | mnemonic == LEAVE         = Leave (inAddress i) (fromIntegral $ inSize i)
  | mnemonic `elem` nops      = Nop (inAddress i) (fromIntegral $ inSize i)
  | otherwise                 = NoSemantics mnemonic Nothing [] (inAddress i) (fromIntegral $ inSize i)


instr_to_semantics (bin,_,_,_) i@(Instruction _ _ mnemonic ops _ _)
  | isCall mnemonic           = Call (operand_to_expr $ ops!!0) i (inAddress i) (fromIntegral $ inSize i)
  | isJump mnemonic           = Jump (inAddress i) (operand_to_expr $ ops!!0) i (fromIntegral $ inSize i)
  | mnemonic == PUSH          = Push (operand_to_expr $ ops!!0) (operand_size_bits $ ops!!0) (inAddress i) (fromIntegral $ inSize i)
  | mnemonic == POP           = Pop (operand_to_expr $ ops!!0) (operand_size_bits $ ops!!0) (inAddress i) (fromIntegral $ inSize i)
  | mnemonic `elem` nops      = Nop (inAddress i) (fromIntegral $ inSize i)
  | mnemonic `elem` moves     = Mov (operand_to_expr $ ops!!0) (operand_to_expr $ ops!!1) (inAddress i) (fromIntegral $ inSize i)
  | mnemonic `elem` sextends  = SExtend (operand_to_expr $ ops!!0) (operand_size_bits $ ops!!0) (operand_to_expr (ops!!1)) (operand_size_bits (ops!!1))  (inAddress i) (fromIntegral $ inSize i)
  | mnemonic `elem` setxxs    = SetXX (operand_to_expr $ ops!!0) (inAddress i) (fromIntegral $ inSize i)
  | mnemonic `elem` cmovs     =
    if operand_size_bits (ops!!0) == 64 then
      mk_apply Cmov (ops!!0) [ ops!!0 ,ops!!1 ] i
    else
      NoSemantics mnemonic (mk_dst $ ops!!0) (map operand_to_expr $ inSrcs i) (inAddress i) (fromIntegral $ inSize i)
  | mnemonic `elem` xors      =
    if show (ops!!0) == show (ops!!1) then
      Mov  (operand_to_expr $ ops!!0) (SE_Immediate 0) (inAddress i) (fromIntegral $ inSize i)
    else
      NoSemantics mnemonic (mk_dst $ ops!!0) (map operand_to_expr $ inSrcs i) (inAddress i) (fromIntegral $ inSize i)
  | otherwise                 = NoSemantics mnemonic (mk_dst $ ops!!0) (map operand_to_expr $ inSrcs i) (inAddress i) (fromIntegral $ inSize i)
 where
  mk_dst dst
    | operandIsWritten dst = Just $ operand_to_expr dst
    | otherwise = Nothing





mk_apply op dst srcs i = Apply op (operand_size_bits dst) (operand_to_expr dst) (map operand_to_expr srcs) (inAddress i) (fromIntegral $ inSize i)
mk_apply_imm op dst srcs i = ApplyWhenImm op (operand_size_bits dst) (operand_to_expr dst) (map operand_to_expr srcs) (inAddress i) (fromIntegral $ inSize i)

operand_size_bits op =
  case operand_size op of
    ByteSize si ->  8*si


--TODO: BSR, ROl, ROR,BSWAP, PEXTRB/D/Q
-- TODO: NOT
--TODO TEST
--

-- | Turn a path in the CFG to a list of abstract semantics
path_to_asemantics :: LiftedWithEntry -> CFG -> Maybe Int -> [Int] -> [ASemantics]
path_to_asemantics l0 cfg n = map (instr_to_semantics l0) . concatMap canonicalize . concat . cut_last_block n . map toInstrs
 where
  toInstrs :: Int -> [Instruction]
  toInstrs blockID = fromJust $ IM.lookup blockID (cfg_instrs cfg)

  cut_last_block Nothing  p = p
  cut_last_block (Just n) p = init p ++ [ take n $ last p ]








----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- Symbolic Execution
----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- A symbolic state stores a mapping from registers to expressions.
-- If the register is not in the mapping, it has not been read or written yet.
-- If the register is assigned Nothing, then its value is unknown.
type Regs = M.Map Register (Maybe SimpleExpr)
type Flags = M.Map String Bool

-- Symbolic memory structures the memory into PointerDomains.
-- Each PointerDomain is separate from all other PointerDomains.
data PointerDomain = Bases (S.Set PointerBase) | Sources (S.Set StatePart) | NoDomain
  deriving (Eq,Ord)

-- Per PointerDomain, we keep track of the memory accesses.
-- An access of the form "SStorage a si v latest" indicates that
-- at address $a$ a region of $si$ bytes stores value $v$, and $latest$ is true iff
-- that value is the last written value in the current domain.
--
-- An access of the form "SRef a" says that pointer $a$ was computed (e.g., through an LEA).
data SAccess = SStorage SimpleExpr Int SStoredVal Bool | SRef SimpleExpr
  deriving (Eq, Ord)


-- A value stored in memory is either some value, an indication that the region has not been written to yet, or unknown.
data SStoredVal = Written SimpleExpr | Initial | Top 
  deriving (Eq,Ord)

-- Per domain, we keep track of a list of accesses.
data SDomain = SDomain [SAccess]
  deriving (Eq, Ord)

data SMemory = SMemory (M.Map PointerDomain SDomain)
  deriving (Eq, Ord)

-- The symbolic state: memory and registers
data SymState = SymState SMemory Regs Flags
  deriving (Eq, Ord)





----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- Symbolic Execution: memory
----------------------------------------------------------------------------
----------------------------------------------------------------------------
(a0,si0) `aliasses_with` (a1,si1) = si0==si1 && (a0==a1 || necessarily_equal a0 a1)

(a0,si0) `enclosed_in` (a1,si1)   = necessarily_enclosed a0 si0 a1 si1

(a0,si0) `encompasses` (a1,si1)   = necessarily_enclosed a1 si1 a0 si0

(a0,si0) `overlaps` (a1,si1)      = (a0,1) `enclosed_in` (a1,si1) || (a1,1) `enclosed_in` (a0,si0)


aliasses_with_access a' si (SStorage a0 si0 _ _) = (a',si) `aliasses_with` (a0,si0)

enclosed_in_access a' si (SStorage a0 si0 _ _) = (a',si) `enclosed_in` (a0,si0)

encompasses_access a' si (SStorage a0 si0 _ _) = (a',si) `encompasses` (a0,si0)


-- Insert a new memory access into the current list of accesses
-- Returns the access as it is is inserted
insert_storage_into_domain :: LiftedWithEntry -> SimpleExpr -> Int -> State [SAccess] SAccess
insert_storage_into_domain l0 a' si = do
  accs <- get
  case find (aliasses_with_access a' si) accs of
    Just n  -> return n 
    Nothing -> do
      let accs'       = SStorage a' si Initial True : accs
      let (touched,_) = runState (partition_domain_touched_by l0 a' si) accs'
      -- TODO too simple if enclosure?
      let latest     = all is_latest touched
      let initial    = latest && all is_initial touched
      let val        = if initial then Initial else Top
      let storage    = SStorage a' si val (val /= Top && latest)
      put $ storage:accs
      return storage

-- Insert a new memory access into the current memory
-- Returns the access as it is is inserted, as well as the new memory
insert_storage_into_mem l0 a' si (SMemory mem) =
  let dom = get_pointer_domain l0 $ prune l0 a' in
    if dom == NoDomain then
      (SStorage a' si Top False,SMemory mem)
      --error $ show (a',si) ++ "\n" ++ show_smemory l0 (SMemory mem)
    else
      let SDomain accs = M.findWithDefault (SDomain []) dom mem
          (acc,accs')  = runState (insert_storage_into_domain l0 a' si) accs in
        (acc,SMemory $ M.insert dom (SDomain accs') mem)

-- Assume that a new access [a',si] has already been inserted into the current list of accesses.
-- Retrieve all the accesses from the current list of accesses that are possibly "touched" when 
-- writing to the new access [a',si]. 
partition_domain_touched_by :: LiftedWithEntry -> SimpleExpr -> Int -> State [SAccess] [SAccess]
partition_domain_touched_by l0 a' si = do
  touched0     <- extract (overlapping_access a' si)
  mem <- get
  if touched0 == [] then error $ show (a',si,mem) else return ()
  not_touched0 <- get
  dirty_below  <- extract (is_dirty_below touched0 not_touched0)
  above        <- if is_dirty l0 a' then go_contiguous_upwards_all touched0 else return []
  let ret       = concat [touched0,dirty_below,above]
  return ret
 where
  is_dirty_below touched0 not_touched0 acc@(SRef _)              = False
  is_dirty_below touched0 not_touched0 acc@(SStorage a0 si0 _ _)
    | not (is_dirty l0 a0) = False
    | otherwise =  
      let (touched,_) = runState (go_contiguous_upwards acc) not_touched0 in
        intersect touched touched0 /= []

  overlapping_access a' si (SRef _)              = False
  overlapping_access a' si (SStorage a0 si0 _ _) = (prune l0 a',si) `overlaps` (prune l0 a0,si0)


  overlapping_accesses (SStorage a0 si0 _ _) acc1 = overlapping_access a0 si0 acc1

  go_contiguous_upwards_all = concatMapM go_contiguous_upwards

  go_contiguous_upwards (SStorage a si _ _) = do
    above0 <- extract (is_contiguous_above a si)
    above1 <- extract (\acc1 -> any (overlapping_accesses acc1) above0)
    above2 <- concatMapM go_contiguous_upwards (above0 ++ above1)
    return $ concat [above0, above1, above2]
 
  is_contiguous_above a si (SRef _)              = False
  is_contiguous_above a si (SStorage a0 si0 _ _) =
    case distance (prune l0 a) si (prune l0 a0) of
      Just d  -> not (testBit d 63) && (fromIntegral d::Int64) < fromIntegral si
      Nothing -> False

  extract f = do
    (yes,no) <- gets $ partition f 
    put no
    return yes

  is_dirty l0 a = prune l0 a /= a


distance :: SimpleExpr -> Int -> SimpleExpr -> Maybe Word64
distance a si a' = 
  case simp $ SE_Op Minus 64 [a',SE_Op Plus 64 [a,SE_Immediate $ fromIntegral si]] of
    SE_Immediate imm -> Just imm
    _                -> Nothing



-- A join operator over two or more symbolic memories
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

-- Clean up the memory so that only global accesses are kept.
prune_mem_to_only_globals :: SMemory -> SMemory
prune_mem_to_only_globals (SMemory mem) = SMemory $ M.map dom_vars_to_top $ M.filterWithKey is_global mem
 where
  is_global (Bases bs) _ = any is_global_base bs
  is_global _          _ = False

  is_global_base (GlobalAddress _) = True
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



-- Printing functions
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

instance Show PointerDomain where
  show (Bases bs)     = show_set "{" "}" $ S.map show bs
  show (Sources srcs) = show_set "{" "}" $ S.map show srcs
  show NoDomain       = "UnknownDomain"

show_sdomain l0 (SDomain accs) = remove_newlines $ T.drawForest $ groups_to_forest $ map (sortBy (compare_accesses l0)) $ group_domain l0 $ sortBy (compare_accesses l0) accs
 where
  groups_to_forest = map group_to_child
  group_to_child group
    | length group > 1 = T.Node (mk_group_header (head group)) $ map mk_node group
    | otherwise        = mk_node $ head group

  mk_node acc = T.Node (show acc) []

  mk_group_header (SStorage a si _ _) = "<<" ++ show (prune l0 a) ++">>"

show_smemory l0 (SMemory mem) = remove_newlines $ intercalate "\n" $ map (show_sdomain l0) $ M.elems mem

show_smemory_html :: LiftedWithEntry -> SMemory -> String
show_smemory_html l0 (SMemory mem) = TV.htmlTree Nothing $ T.Node header $ concatMap sdomain_to_forest $ M.elems mem
 where
  sdomain_to_forest (SDomain accs) = groups_to_forest $ map (sortBy (compare_accesses l0)) $ group_domain l0 $ sortBy (compare_accesses l0) accs

  groups_to_forest = map group_to_child
  group_to_child group
    | length group > 1 = T.Node (mk_group_header (head group)) $ map mk_node group
    | otherwise        = mk_node $ head group

  mk_node acc = T.Node (TV.NodeInfo TV.InitiallyExpanded (show_saccess acc) "") []

  show_saccess (SStorage a si _ _) = "[" ++ show a ++ "," ++ show si ++ "]"

  header = TV.NodeInfo TV.InitiallyExpanded "" ""
  mk_group_header (SStorage a si _ _) = TV.NodeInfo TV.InitiallyExpanded ("<<" ++ show (prune l0 a) ++ ">>") ""

  
group_domain l0 [] = []
group_domain l0 mem@((SStorage a0 si0 _ _):accs) =
  let (touched,not_touched) = runState (partition_domain_touched_by l0 a0 si0) mem in
    touched : group_domain l0 not_touched

remove_newlines []              = []
remove_newlines ('\n':'\n':str) = remove_newlines ('\n':str)
remove_newlines (c:str)         = c : remove_newlines str


is_initial (SStorage ptr si Initial latest)     = latest
is_initial (SStorage ptr si Top     _)          = False
is_initial (SStorage ptr si (Written v) latest) = latest && v == SE_Var (SP_Mem ptr si)

is_latest (SStorage _ _ _ latest) = latest


compare_accesses l0 (SStorage a0 si0 _ _) (SStorage a1 si1 _ _) = is_below l0 (a0,si0) (a1,si1)
compare_accesses l0 (SStorage a0 si0 _ _) (SRef a1)             = is_below l0 (a0,si0) (a1,0)
compare_accesses l0 (SRef a0)             (SStorage a1 si1 _ _) = is_below l0 (a0,0)   (a1,si1)
compare_accesses l0 (SRef a0)             (SRef a1)             = is_below l0 (a0,0)   (a1,0)

is_below l0 (a0,si0) (a1,si1) =
  let a0' = prune l0 a0
      a1' = prune l0 a1
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




----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- Symbolic Execution: symbolic state
----------------------------------------------------------------------------
----------------------------------------------------------------------------
show_symstate l0 (SymState mem regs flgs) = show_symstate_regs show "\n" regs ++ "\n" ++ show_smemory l0 mem ++ show_sflags  flgs


show_sflags flgs
  | M.null flgs = ""
  | otherwise = "\n" ++ (intercalate "\n" $ map (\(flg,v) -> flg ++ " == " ++ show v) $ M.assocs flgs)

get_mem :: State SymState SMemory
get_mem = get <&> (\(SymState mem _ _) -> mem)

get_regs :: State SymState Regs
get_regs = get <&> (\(SymState _ regs _) -> regs)

get_flags :: State SymState Flags
get_flags = get <&> (\(SymState _ _ flgs) -> flgs)

modify_regs :: (Regs -> Regs) -> State SymState ()
modify_regs f = modify (\(SymState mem regs flgs) -> SymState mem (f regs) flgs)



read_top_from_statepart :: StatePart -> Regs -> SimpleExpr
read_top_from_statepart sp regs = do
  let Just (Just rip) = M.lookup (Reg64 RIP) regs in
    SE_StatePart sp $ Just $ show rip


sread_mem :: LiftedWithEntry -> SimpleExpr -> SimpleExpr -> Int -> State SymState SimpleExpr
sread_mem l0@(bin,_,_,_) a a' si = do
  -- 1.) insert region into memory model
  SymState mem regs flgs <- get
  let (st,mem') = insert_storage_into_mem l0 a' si mem
  put $ SymState mem' regs flgs
  -- 2.) use the state of the access to retrieve a value
  case st of
    (SStorage _ _ val latest) -> mk_val val latest
 where
  mk_val _           False = get_regs <&> (read_top_from_statepart $ SP_Mem a si)
  mk_val Top         _     = get_regs <&> (read_top_from_statepart $ SP_Mem a si)
  mk_val (Written v) True  = return $ v
  mk_val Initial     True  = 
    case a' of
      SE_Immediate imm -> 
        case read_from_ro_datasection bin imm si of
          Nothing -> return $ SE_Var $ SP_Mem a' si
          Just v  -> return $ SE_Immediate v
      _           -> return $ SE_Var $ SP_Mem a' si


swrite_mem :: LiftedWithEntry -> SimpleExpr -> Int -> Maybe SimpleExpr -> State SymState ()
swrite_mem l0 a' si v' = do
  -- 1.) insert region into memory model
  SymState mem regs flgs <- get
  let (_,SMemory mem') = insert_storage_into_mem l0 a' si mem
   -- 2.) overwrite all regions that are touched by doing the current write
  let dom = get_pointer_domain l0 $ prune l0 a'
  let mem'' = M.adjust dom_write dom mem'
  put $ SymState (SMemory mem'') regs flgs
 where 
  dom_write (SDomain accs) = 
    let (touched,not_touched) = runState (partition_domain_touched_by l0 a' si) accs
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

  -- TODO?
  take_bytes si Top         = Top
  take_bytes si Initial     = Initial
  take_bytes si (Written v) = Written $ simp $ SE_Bit (8*si) v

get_pointer_bases :: LiftedWithEntry -> SimpleExpr -> S.Set PointerBase
get_pointer_bases (bin,config,l0,entry) a =
  let Just (finit,_) = IM.lookup (fromIntegral entry) $ l0_functions l0 in
    get_pointer_base_set bin finit a

 
get_pointer_domain :: LiftedWithEntry -> SimpleExpr -> PointerDomain
get_pointer_domain l0@(bin,_,_,_) a' =
  let bases = get_pointer_bases l0 a' in
    if not $ S.null bases then
      Bases $ S.map globals_to_section_starts bases
    else
      let srcs = get_pointer_sources a' in
        if not $ S.null srcs then
          Sources srcs
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
    | regSize r == ByteSize 8 = S.singleton sp
    | otherwise               = S.empty
  get_pointer_src (SE_Var sp@(SP_Mem a si))
    | si == 8        = S.singleton sp
    | otherwise      = S.empty
  get_pointer_src _  = S.empty

  globals_to_section_starts (GlobalAddress a) =
    case find_section_for_address bin a of
      Just (_,_,a0,_,_,_) -> GlobalAddress a0
      Nothing -> GlobalAddress a -- error $ "No section for: " ++ show a'
  globals_to_section_starts b = b


has_pointer_domain :: LiftedWithEntry -> SimpleExpr -> Bool
has_pointer_domain l0 a' = get_pointer_domain l0 a' /= NoDomain


prune :: LiftedWithEntry -> SimpleExpr -> SimpleExpr
prune l0 = prune'' l0 $ SE_Immediate 0

prune_to_bot l0 = prune'' l0 $ Bottom RockBottom

prune'' :: LiftedWithEntry -> SimpleExpr -> SimpleExpr -> SimpleExpr
prune'' ctxt@(bin,config,l0,entry) subst e =
  let e0 = prune' True e
      e1 = prune' False e in
    if e0 /= e1 && (if is_immediate e1 then expr_is_global_immediate bin e1 else has_pointer_domain ctxt e1) then
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
    | get_pointer_domain ctxt e == NoDomain = SE_Immediate 0
    | otherwise                             = e

  prune_keep_only_imms e@(SE_Immediate _) = e
  prune_keep_only_imms (SE_Op op si es)   = simp $ SE_Op op si $ map prune_keep_only_imms es
  prune_keep_only_imms _                  = subst



clean_below_current_stackframe :: LiftedWithEntry -> SymState -> SymState
clean_below_current_stackframe l ss@(SymState (SMemory m) regs flgs) = 
  case read_rsp regs of
    Just rsp_value -> 
      let  new_mem = M.mapWithKey (clean_local_and_below_stackframe rsp_value) m in
        SymState (SMemory new_mem) regs flgs
    Nothing -> ss
 where
  read_rsp regs = 
    case M.lookup (Reg64 RSP) regs of
      Just (Just v) -> Just v
      Nothing -> Nothing
      x -> error $ "Unexpected RSP value: " ++ show x ++ "\n" ++ show_symstate l ss

  clean_local_and_below_stackframe :: SimpleExpr -> PointerDomain -> SDomain -> SDomain
  clean_local_and_below_stackframe rsp_value dom v@(SDomain accs)
    | is_local_domain dom = SDomain $ filter (access_is_local_and_above_stackframe rsp_value) accs
    | otherwise = v

  is_local_domain (Bases bs) = StackPointer `S.member` bs
  is_local_domain (Sources srcs) = (SP_Reg $ Reg64 RSP) `S.member` srcs 
  is_local_domain NoDomain = False

  access_is_local_and_above_stackframe :: SimpleExpr -> SAccess -> Bool
  access_is_local_and_above_stackframe rsp_value (SStorage a si v touched) =
    case distance (prune l a) si rsp_value of
      -- compute rsp - (a+si)
      Just d -> (fromIntegral d :: Int64) < -8
      _ -> False --TODO!!! error $ show (a,prune l a, si,rsp_value) We should implement more rigid pruning here

{--
--
-- An access of the form "SRef a" says that pointer $a$ was computed (e.g., through an LEA).
data SAccess = SStorage SimpleExpr Int SStoredVal Bool | SRef SimpleExpr
  deriving (Eq)

-- A value stored in memory is either some value, an indication that the region has not been written to yet, or unknown.
data SStoredVal = Written SimpleExpr | Initial | Top 
  deriving (Eq,Ord)

-- Per domain, we keep track of a list of accesses.
data SDomain = SDomain [SAccess]

data SMemory = SMemory (M.Map PointerDomain SDomain)
--}



 
show_symstate_regs :: (a -> String) -> String -> M.Map Register (Maybe a) -> String
show_symstate_regs show_a delim = intercalate delim . map show_entry . M.assocs
 where
  show_entry (r,v)    = show r ++ " == " ++ show_maybe_expr v
  show_maybe_expr Nothing  = "UNKNOWN"
  show_maybe_expr (Just e) = show_a e






sread_reg :: Register -> State SymState SimpleExpr
sread_reg r = do
  regs <- get_regs
  let ByteSize si = regSize r
  return $ do_read (real_reg r) si regs
 where
  do_read rr 64 = get_value rr
  do_read rr 32 = get_value rr
  do_read rr 16 = simp . SE_Bit 128 . get_value rr
  do_read rr 10 = read_top_from_statepart (SP_Reg rr) -- ST registers not supported
  do_read rr 8  = get_value rr
  do_read rr 4  = simp . SE_Bit 32 . get_value rr
  do_read rr 2  = simp . SE_Bit 16 . get_value rr
  do_read rr 1  = simp . SE_Bit 8  . get_value rr
  do_read rr si = error $ show (rr,si)

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



swrite_reg :: Register -> Maybe SimpleExpr -> State SymState ()
swrite_reg r v' = do
  curr_v <- sread_reg r
  let ByteSize si = regSize r
  modify_regs $ do_write (real_reg r) si curr_v
 where
  do_write rr 64 curr_v = M.insert rr v'
  do_write rr 32 curr_v = M.insert rr v'
  do_write rr 16 curr_v = M.insert rr (simp <$> SE_Bit 128 <$> v')
  do_write rr 10 curr_v = id -- ST registers not supported
  do_write rr 8  curr_v = M.insert rr v'
  do_write rr 4  curr_v = M.insert rr (simp <$> SE_Bit 32 <$> v')
  do_write rr 2  curr_v = M.insert rr (simp <$> SE_Overwrite 16 curr_v <$> SE_Bit 16 <$> v')
  do_write rr 1  curr_v = M.insert rr (simp <$> SE_Overwrite 8 curr_v <$> SE_Bit 16 <$> v')
  do_write rr si curr_v = error $ show (rr,si,curr_v)
  -- TODO writes to high bytes of lower 2 bytes




sread_statepart :: LiftedWithEntry -> StatePart -> State SymState SimpleExpr
sread_statepart l0 (SP_Reg r)    = sread_reg r
sread_statepart l0 (SP_Mem a si) = do
  as' <- operand_address_to_resolved_exprs l0 a
  case as' of
    Nothing  -> do
      get_regs <&> (read_top_from_statepart $ SP_Mem a si)
      --s <- get
      --error $ "Read from domainless pointer: " ++ show (SP_Mem a si) ++ "\n" ++ show_symstate l0 s
    Just as' -> do
      rets <- nub <$> mapM do_read as'
      return $ foldr1 (\v0 v1 -> SE_Op Cmov (si*8) [v0,v1]) rets 
 where
  do_read a'
    | has_pointer_domain l0 a' = sread_mem l0 a a' si
    | otherwise = error $ show (SP_Mem a si,a')
    -- TODO check not needed
      



sresolve_expr :: LiftedWithEntry -> SimpleExpr -> State SymState SimpleExpr
sresolve_expr l0 e@(SE_Immediate _)        = return e
sresolve_expr l0 e@(SE_Var _)              = return e
sresolve_expr l0   (SE_StatePart sp _)     = sread_statepart l0 sp
sresolve_expr l0   (SE_Op op si es)        = (simp . SE_Op op si) <$> mapM (sresolve_expr l0) es
sresolve_expr l0   (SE_Bit n e)            = (simp . SE_Bit n) <$> sresolve_expr l0 e
sresolve_expr l0   (SE_SExtend l h e)      = (simp . SE_SExtend l h) <$> sresolve_expr l0 e
sresolve_expr l0   (SE_Overwrite n e0 e1)  = do
  e0' <- sresolve_expr l0 e0
  e1' <- sresolve_expr l0 e1
  return $ simp $ SE_Overwrite n e0 e1
sresolve_expr l0 e@(Bottom _)              = return e


-- take @a@: the address as it occurs in the operand of an instruction.
-- For example: RAX + RBX*4 in the memory operand QWORD PTR [RAX + RBX*4]
-- Try to resolve this address to a symbolic value by reading its inputs.
operand_address_to_resolved_exprs :: LiftedWithEntry -> SimpleExpr -> State SymState (Maybe [SimpleExpr])
operand_address_to_resolved_exprs l0 a = do
  a'      <- sresolve_expr l0 a
  let as'  = nub $ map simp $ unfold_cmovs a'

  if all (has_pointer_domain l0) as' then
    return $ Just as'
  else do
    rets <- try_operand_address_to_base a
    case rets of
      [] -> return Nothing
      _  -> return $ Just $ nub rets
 where
  try_operand_address_to_base :: SimpleExpr -> State SymState [SimpleExpr]
  try_operand_address_to_base op = concat <$> (mapM (get_base op) $ M.assocs $ addends op)
  get_base op (SE_StatePart (SP_Reg r) _,1) = do
    a' <- sread_reg r
    -- TODO unfold cmovs here as well
    let bases = get_pointer_bases l0 a'
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



soverwrite_dst :: LiftedWithEntry -> SimpleExpr -> Maybe SimpleExpr -> State SymState ()
soverwrite_dst l0 (SE_StatePart (SP_Reg r) _) = swrite_reg (real_reg r)
soverwrite_dst l0 sp = swrite_dst l0 sp



swrite_dst :: LiftedWithEntry -> SimpleExpr -> Maybe SimpleExpr -> State SymState ()
swrite_dst l0 (SE_StatePart (SP_Reg r) _)    v' = swrite_reg r v'
swrite_dst l0 (SE_StatePart (SP_Mem a si) _) v' = do
  as' <- operand_address_to_resolved_exprs l0 a
  case as' of
    Just as' -> forM_ as' do_write
    Nothing  -> return ()
    --Nothing  -> do
    --  inv <- get
    --  error $ "Writing to baseless address: " ++ show (a,si) ++ "\n" ++ show_symstate l0 inv
 where
  do_write :: SimpleExpr -> State SymState ()
  do_write a'
    | has_pointer_domain l0 a' = swrite_mem l0 a' si (simp . SE_Bit (si*8) <$> v')
    -- TODO check not needed
swrite_dst _ e _ = error $ show e

sread_src :: LiftedWithEntry -> SimpleExpr -> State SymState SimpleExpr
sread_src l0   (SE_StatePart sp _)          = sread_statepart l0 sp
sread_src l0 e@(SE_Immediate imm)           = return $ e
sread_src l0 e@(SE_Overwrite n src0 src1)   = do
  src0' <- sread_src l0 src0
  src1' <- sread_src l0 src1
  return $ simp $ SE_Overwrite n src0' src1'
sread_src l0 e                              = error $ "Reading from " ++ show e


widen_repeated_accesses :: LiftedWithEntry -> [ASemantics] -> [ResolvedOperands] -> SymState -> SymState
widen_repeated_accesses l0 sems ras = update_inv $ zip sems ras
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

  update_inv_per_operand sem (SP_Reg r,as') inv = inv
  update_inv_per_operand sem (SP_Mem a si,as') inv
    | length as' <= 1 = inv
    | si == 0         = inv
    | otherwise       =
      let groups = non_trivial_distanced_groups as' in
        foldr (update_inv_for_group sem si) inv groups

  non_trivial_distanced_groups as' =
    let pruned_as' = map (prune l0) as'
        groups     = quotientByL hasDistance pruned_as' in
      filter (\group -> length group > 1) $ map nub groups

  update_inv_for_group sem si group inv =
    let group'                 = sortBy smallerDistance group
        a'                     = simp $ SE_Op Plus 64 [head group', Bottom RockBottom]
        SymState mem regs flgs = inv
        (_,mem')               = insert_storage_into_mem l0 a' si mem in
      trace ("\nWIDENING: " ++ show a')
        SymState mem' regs flgs


pruned_equal l0 a0 a1 = prune l0 a0 == prune l0 a1

hasDistance a0 a1 = distance a0 0 a1 /= Nothing

smallerDistance a0 a1 = 
  case distance a0 0 a1 of 
    Just d -> if testBit d 63 then GT else if d == 0 then EQ else LT

type ResolvedOperands = M.Map StatePart [SimpleExpr]

tau_path :: LiftedWithEntry -> [ASemantics] -> SymState -> ([ResolvedOperands], SymState)
tau_path l@(_,_,_,entry) p symstate = runState (traverse 0 p) symstate
 where
  traverse :: Int -> [ASemantics] -> State SymState [ResolvedOperands]
  traverse n []      = return []
  traverse n (sem:p) = do
    set_rip (size_of sem + rip_of sem)

    resolved_ops <- gets $ resolve_operands l sem

    -- regs <- get_regs
    tau l n sem
    resolved_ops' <- traverse (n+1) p
    return $ resolved_ops : resolved_ops'
    -- return (prune_symstate_for_instruction sem regs:regs')
    -- return (M.empty:regs')




resolve_operands :: LiftedWithEntry -> ASemantics -> SymState -> ResolvedOperands
resolve_operands l@(bin,config,l0,_) sem inv = 
  let inv' = execState (set_rip (size_of sem + rip_of sem)) inv 
      ops  = operands_of sem ++  map (\r -> SE_StatePart (SP_Reg r) Nothing) (syscall_input_registers sem inv' ++ function_call_input_registers (bin,config,l0) sem) in
    M.map nub $ M.unionsWith (++) $ map (resolve_operand inv' sem) ops
 where
  resolve_operand inv sem (SE_StatePart (SP_Mem a 0) Nothing) = 
    case evalState (operand_address_to_resolved_exprs l a) inv of
      Just as' -> M.singleton (SP_Mem a 0) as'
      Nothing  -> M.empty
  resolve_operand inv sem (SE_StatePart (SP_Mem a si) Nothing) = 
    case evalState (operand_address_to_resolved_exprs l a) inv of
      Just as' -> let v = evalState (sread_statepart l (SP_Mem a si)) inv in
                    M.fromList [(SP_Mem a 0, as'), (SP_Mem a si, [v])]
      Nothing -> let a' = evalState (sresolve_expr l a) inv
                     v = evalState (sread_statepart l (SP_Mem a si)) inv in
                    M.fromList [(SP_Mem a 0, [a']), (SP_Mem a si, [v])]
  resolve_operand inv sem (SE_StatePart (SP_Reg r) Nothing) =
    let v = evalState (sread_reg r) inv in
      M.singleton (SP_Reg r) [v]
  resolve_operand inv sem (SE_Immediate _) = M.empty
  resolve_operand inv sem e = error $ show e


syscall_input_registers (SysCall _ _) inv = 
  case evalState (sread_reg $ Reg64 RAX) inv of
    SE_Immediate imm -> do
      let argcount = snd $ num_of_input_registers_of_sys_call $ fromIntegral imm in
        take argcount all_input_regs_of_syscalls
    _ -> all_input_regs_of_syscalls
syscall_input_registers _ _ = []

function_call_input_registers l@(bin,config,l0) sem = 
  case instruction_of sem of
    Nothing -> []
    Just i ->
      let trgts = get_known_jump_targets l i in
        if any is_internal trgts then
          []
        else let f = function_name_of_instruction bin i in
          take (get_argcount bin f) all_input_regs_of_functions
 where
  instruction_of (Call src i rip si) = Just i
  instruction_of (Jump rip src i si) = Just i
  instruction_of _ = Nothing

  is_internal (ImmediateAddress _) = True
  is_internal _ = False



operands_of :: ASemantics -> [SimpleExpr]
operands_of (Apply op op_si dst srcs rip si)          = operands_of_dst dst ++ srcs
operands_of (ApplyWhenImm op op_si dst srcs rip si)   = operands_of_dst dst ++ srcs
operands_of (Mov dst src rip si)                      = operands_of_dst dst ++ [src]
operands_of (MovZX dst src op_si rip si)              = operands_of_dst dst ++ [src]
operands_of (SExtend dst h src l rip si)              = operands_of_dst dst ++ [src]
operands_of (Lea dst src rip si)                      = operands_of_dst dst ++ [src]
operands_of (Nop  _ _)                                = []
operands_of (Push src _ _ _)                          = [] -- [src]
operands_of (Pop dst _ _ _)                           = []
operands_of (Leave _ _)                               = []
operands_of (NoSemantics op Nothing srcs rip si)      = srcs
operands_of (NoSemantics op (Just dst) srcs rip si)   = operands_of_dst dst ++ srcs
operands_of (SetXX dst rip si)                        = operands_of_dst dst
operands_of (Call src _ rip si)                       = [src]
operands_of (SysCall rip si)                          = [ SE_StatePart (SP_Reg $ Reg64 RAX) Nothing ]
operands_of (Jump rip src _ si)                       = [src]
operands_of (Ret rip si)                              = [ SE_StatePart (SP_Mem (SE_StatePart (SP_Reg $ Reg64 RSP) Nothing) 8) Nothing ]

operands_of_dst dst@(SE_StatePart (SP_Mem a si) Nothing) = [SE_StatePart (SP_Mem a 0) Nothing]
operands_of_dst _ = []



prune_symstate_for_instruction :: ASemantics -> Regs -> Regs
prune_symstate_for_instruction sem = M.filterWithKey is_relevant
 where
  is_relevant r _ = real_reg r `elem` map real_reg (regs_of_sem sem)

  regs_of_sem (Apply op op_si dst srcs rip si)        = concatMap regs_of_op (dst:srcs)
  regs_of_sem (ApplyWhenImm op op_si dst srcs rip si) = concatMap regs_of_op (dst:srcs)
  regs_of_sem (Mov dst src rip si)                    = concatMap regs_of_op [dst,src]
  regs_of_sem (MovZX dst src _ rip si)                = concatMap regs_of_op [dst,src]
  regs_of_sem (SExtend dst _ src _ rip si)            = concatMap regs_of_op [dst,src]
  regs_of_sem (SetXX dst rip si)                      = concatMap regs_of_op [dst]
  regs_of_sem (NoSemantics op Nothing srcs rip si)    = concatMap regs_of_op srcs
  regs_of_sem (NoSemantics op (Just dst) srcs rip si) = concatMap regs_of_op (dst:srcs)
  regs_of_sem (Lea dst src rip si)                    = [] -- regs_of_expr src
  regs_of_sem (Nop _ _)                               = []
  regs_of_sem (Push src _ _ _)                        = regs_of_op src ++ [Reg64 RSP]
  regs_of_sem (Pop dst _ _ _)                         = regs_of_op dst ++ [Reg64 RSP]
  regs_of_sem (Leave _ _)                             = [Reg64 RSP,Reg64 RBP]
  regs_of_sem (Call _ _ rip si)                       = []
  regs_of_sem (Jump rip _ _ si)                       = []
  regs_of_sem (Ret rip si)                            = [Reg64 RSP]
  regs_of_sem (SysCall rip si)                        = [Reg64 RAX]

  regs_of_op (SE_StatePart (SP_Mem a si) _) = regs_of_expr a
  regs_of_op (SE_StatePart (SP_Reg r) _)    = [r]
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
regs_of_expr (SE_Malloc _ _)                = []




set_rip :: Word64 -> State SymState ()
set_rip rip = swrite_reg (Reg64 RIP) (Just $ SE_Immediate rip)


size_of (Call _ i rip si)                      = si
size_of (Ret rip si)                           = si
size_of (SysCall rip si)                       = si
size_of (Jump rip _ _ si)                      = si
size_of (Lea  dst src rip si)                  = si
size_of (Nop rip si)                           = si
size_of (Push _ _ rip si)                      = si
size_of (Pop _ _ rip si)                       = si
size_of (Leave _ si)                           = si
size_of (Mov  dst src rip si)                  = si
size_of (MovZX dst src _ rip si)               = si
size_of (SExtend dst h src l rip si)           = si
size_of (SetXX dst rip si)                     = si
size_of (Apply op op_si dst src rip si)        = si
size_of (ApplyWhenImm op op_si dst src rip si) = si
size_of (NoSemantics op dst srcs rip si)       = si

rip_of (Call _ i rip si)                      = rip
rip_of (Ret rip si)                           = rip
rip_of (SysCall rip si)                       = rip
rip_of (Jump rip _ _ si)                      = rip
rip_of (Lea  dst src rip si)                  = rip
rip_of (Nop rip si)                           = rip
rip_of (Push _ _ rip si)                      = rip
rip_of (Pop _ _ rip si)                       = rip
rip_of (Leave rip si)                         = rip
rip_of (Mov  dst src rip si)                  = rip
rip_of (MovZX dst _ src rip si)               = rip
rip_of (SExtend dst h src l rip si)           = rip
rip_of (SetXX dst rip si)                     = rip
rip_of (Apply op op_si dst src rip si)        = rip
rip_of (ApplyWhenImm op op_si dst src rip si) = rip
rip_of (NoSemantics op dst srcs rip si)       = rip






spush l0 src op_si = do
  -- RSP -= operand_size
  let si = case src of
             SE_Immediate _ -> 8
             _ -> op_si `div` 8
  let rsp = Reg64 RSP
  rsp_value <- sread_reg rsp
  let new_rsp_value = simp $ SE_Op Minus 64 [rsp_value, SE_Immediate $ fromIntegral si]
  swrite_reg rsp $ Just new_rsp_value
  -- *[RSP,si] := src
  src_value <- sread_src l0 src
  let sp = SE_StatePart (SP_Mem (SE_StatePart (SP_Reg $ Reg64 RSP) Nothing) si) Nothing
  swrite_dst l0 sp $ Just src_value


spop l0 dst op_si = do
  -- dst := *[RSP,si]
  let si = op_si `div` 8
  let sp = SP_Mem (SE_StatePart (SP_Reg $ Reg64 RSP) Nothing) si
  src_value <- sread_statepart l0 sp
  swrite_dst l0 dst $ Just src_value
  -- RSP += operand_size
  let rsp = Reg64 RSP
  rsp_value <- sread_reg rsp
  let new_rsp_value = simp $ SE_Op Plus 64 [rsp_value, SE_Immediate $ fromIntegral si]
  swrite_reg rsp $ Just new_rsp_value

sleave l0 = do
  -- RSP := RBP
  let rbp = Reg64 RBP
  let rsp = Reg64 RSP
  rbp_value <- sread_reg rbp
  swrite_reg rsp $ Just rbp_value
  -- POP RBP
  spop l0 (SE_StatePart (SP_Reg rbp) Nothing) 64


sret l0@(_,_,_,entry) = do
  -- RIP := *[RSP,8]
  v' <- sread_statepart l0 $ SP_Mem (SE_StatePart (SP_Reg $ Reg64 RSP) Nothing) 8
  swrite_reg (Reg64 RIP) (Just v') 
  -- RSP += 8
  let rsp = Reg64 RSP
  rsp_value <- sread_reg rsp
  let new_rsp_value = simp $ SE_Op Plus 64 [rsp_value, SE_Immediate 8]
  --traceShow ("RET:", showHex entry, new_rsp_value) $
  swrite_reg rsp $ Just new_rsp_value



-- TODO refactor code of sjump and scall, also when partially unresolved
sjump l@(bin,config,l0,_) n rip i
  | jump_is_actually_a_call (bin,config,l0) i = call_then_return $ get_known_jump_targets (bin,config,l0) i
  | otherwise = return ()
 where
  call_then_return trgts
    | any is_internal trgts = return ()
    | all isExternal trgts = do
      let f = function_name_of_instruction bin i
      if isPrefixOf "0x" f then
        error $ show i
      else do
        scall l n i rip
        sret l
    | otherwise = error $ show (i,trgts)

  isExternal (External _) = True
  isExternal (ExternalDeref _) = True
  isExternal (Returns True) = True
  isExternal (Unresolved) = True
  isExternal _ = False

  is_internal (ImmediateAddress _) = True
  is_internal _ = False

scall_internal l@(_,_,_,entry) = do
  {--let rsp = Reg64 RSP
  rsp_value <- sread_reg rsp
  traceShow ("CALL:", showHex entry, f, rsp_value) $--}
  spush l (SE_StatePart (SP_Reg $ Reg64 RIP) Nothing) 64


scall l@(bin,config,l0,entry) n i rip = call $ get_known_jump_targets (bin,config,l0) i
 where
  call trgts
    | any is_internal trgts = scall_internal l
    | otherwise             = let f = function_name_of_instruction bin i in external_behavior f $ external_function_behavior f

  is_internal (ImmediateAddress _) = True
  is_internal _ = False

  --external_behavior f (ExternalFunctionBehavior _ (Input reg)) = do
  --  let regs = take (argcount f) all_input_regs_of_functions
  --  mapM_ sread_reg regs 
  --  ret_val <- sread_reg reg
  --  swrite_reg (Reg64 RAX) (Just ret_val)

  external_behavior f _ = do
    let argc = get_argcount bin f
    let regs = take argc all_input_regs_of_functions
    mapM_ sread_reg regs 
    let retval = SE_Malloc (Just rip) (Just $ f ++ "_" ++ show n)
    swrite_reg (Reg64 RAX) (Just retval) -- TODO and XMM0?

get_argcount bin f =
  case binary_get_function_signature bin f of
    Nothing -> argcount bin f
    Just Variadic -> 6
    Just (Argcount n) -> n
 where
  argcount bin f 
    | any (\p -> isPrefixOf p f) ["0x", "*", "syscall@", "indirection@"] = 0
    | otherwise = trace ("Do not know external function: " ++ f) 0

cap_expr e (SE_StatePart sp _)
  | expr_size e > 50 = get_regs <&> (read_top_from_statepart sp)
  | otherwise        = return e

tau :: LiftedWithEntry -> Int -> ASemantics -> State SymState ()
tau l0 n (Apply op op_si dst srcs rip si)          = do
  srcs' <- mapM (sread_src l0) srcs
  let v = simp $ SE_Op op op_si srcs'
  v' <- cap_expr v dst 
  swrite_dst l0 dst $ Just v'
tau l0 n (ApplyWhenImm op op_si dst srcs rip si)   = do
  srcs' <- mapM (sread_src l0) srcs
  let v = simp $ SE_Op op op_si srcs'
  v' <- cap_expr v dst 
  if any is_immediate srcs' then
    swrite_dst l0 dst $ Just v'
  else
    swrite_dst l0 dst Nothing  
tau l0 n (Mov dst src rip si)                = do
  src' <- sread_src l0 src
  swrite_dst l0 dst (Just src') 
tau l0 n (MovZX dst src op_si rip si)        = do
  src' <- sread_src l0 src
  swrite_dst l0 dst (Just $ simp $ SE_Bit op_si src') 
tau l0 n (SExtend dst h src l rip si)        = do
  src' <- sread_src l0 src
  swrite_dst l0 dst (Just $ simp $ SE_SExtend l h src') 
tau l0 n (Lea dst src rip si)                = do
  let SE_StatePart (SP_Mem a si) _ = src
  src' <- sresolve_expr l0 a
  swrite_dst l0 dst (Just src') 
tau l0 n (NoSemantics op dst srcs rip si)    = do
  -- Note sources must be read, as reading can influence the memory model
  srcs' <- mapM (sread_src l0) srcs
  case dst of
    Nothing  -> return ()
    Just dst -> swrite_dst l0 dst Nothing  
tau l0 n sem@(SysCall rip si)                    = do
  -- Note sources must be read, as reading can influence the memory model
  regs <- gets $ syscall_input_registers sem
  mapM_ sread_reg regs
  mapM_ (\r -> swrite_reg r Nothing) $ regs_clobbered_by_syscall
tau l0 n (SetXX dst rip si)                  = soverwrite_dst l0 dst $ Just $ SE_Op ZeroOne 8 []
tau l0 n (Call op i rip si)                  = scall l0 n i rip 
tau l0 n (Jump rip op i si)                  = sjump l0 n rip i
tau l0 n (Nop rip si)                        = return ()

tau l0 n (Push src op_si rip si)             = spush l0 src op_si
tau l0 n (Pop dst op_si rip si)              = spop l0 dst op_si
tau l0 n (Leave rip si)                      = sleave l0
tau l0 n (Ret rip si)                        = sret l0







{--
 - {--
full_cfg_post l0 (entry,blockID)
  | isCall (opcode i) || isJump (opcode i) = S.unions $ map jump_target_to_next $ resolve_jump_target l0 i
  | otherwise                              =
    case post of
      Nothing   -> S.empty
      Just post -> within_current_cfg post
 where
  Just cfg = IM.lookup entry (l0_cfgs l0)
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
  find_outside_cfg a = (\a -> (a,0)) <$> (find ((==) a) (map fromIntegral $ IM.keys $ l0_calls l0))
  -- search for a block in the current cfg that starts at @a@, and if found, make a label for it
  find_inside_cfg a = (\(blockId,_) -> (entry,blockID)) <$> find (block_starts_at a) (IM.toList $ cfg_instrs cfg)

  block_starts_at a (blockId, instrs) = instrs /= [] && inAddress (head instrs) == fromIntegral a




full_cfg_vertices l0 entry =
  let Just cfg = IM.lookup entry (l0_cfgs l0) in
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



      
                  








-- TODO separation should protect sensitive stack regions
-- ¶[(RSP_0 - 8), 8] := RBP_0
-- ¶[(RSP_0 - 16), 8] := R15_0
-- ¶[(RSP_0 - 24), 8] := R14_0
-- ¶[(RSP_0 - 32), 8] := R13_0
-- ¶[(RSP_0 - 40), 8] := R12_0
-- ¶[(RSP_0 - 48), 8] := RBX_0
-- ¶[(RSP_0 - 64), 8] := [(FS_0 + 40), 8]_0


--
--
--
--
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


-- Function 0x8860, 0x4670, 0xb2b0 of gzip
--
-- Function 0x1309 of clientserver

-- 0x8290 of sha512sum has domains
--  ├╴<<RDI_0>>
--  └╴<<(RDI_0 + RSI_0)>>
--
--}
--
--
