{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StrictData #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : SymbolicExecution
Description : Symbolic execution of paths in a control flow graph
-}



module InputLifting.SymbolicExecution where


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

import Binary.FunctionNames
import Binary.Generic

--TODO
import WithNoAbstraction.SymbolicExecution (external_function_behavior)
import WithNoAbstraction.Pointers (expr_is_global_immediate,get_pointer_base_set,necessarily_enclosed,necessarily_equal)
import WithAbstractSymbolicValues.Class (empty_finit)

import Data.SymbolicExpression hiding (show_srcs,swrite_mem)

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
import Control.Monad (forM_, when)
import Control.Monad.Extra (concatMapM)

import GHC.Base (compareInt)

import Debug.Trace


type Context bin = (bin,Config)



init_symstate = SymState (SMemory M.empty) M.empty Nothing []

get_symstate (_,_,symstate,_) = symstate

init_sym_state_with :: Register -> Word64 -> SymState
init_sym_state_with reg value = execState (swrite_reg reg (Just $ SE_Immediate value)) init_symstate

read_RIP symstate =
  let v = evalState (sread_reg (Reg64 RIP)) symstate in
    case v of
      SE_Immediate a -> a
      _ -> error $ "RIP = " ++ show v


-- SYMBOLIC EXECUTION MAIN FUNCTION
-- Step 1: assign Abstract Semantics to the path
-- Step 2: symbolically execute the abstract semantics
-- Step 3: print out the results
--
-- Returns:
-- 1.) the abstract semantics of the path
-- 2.) per instruction, the resolved operands
-- 3.) the final symbolic state, after symbolically executing the entire path
symbolically_execute_path :: BinaryClass bin => Context bin -> [Instruction] -> SymState -> IO ([ASemantics], [ResolvedOperands], SymState, S.Set StatePart,String)
symbolically_execute_path ctxt path symstate = do
  let asemantics = path_to_asemantics ctxt path
  let (ras,inv)  = tau_path ctxt asemantics symstate
  let inputs     = S.unions $ map resolved_operands_to_inputs $ zip asemantics ras
  let pp_result  = show_result path asemantics ras inv inputs
  --putStrLn $ show_result path asemantics ras inv inputs
  --putStrLn $ "\n\n"
  return (asemantics,ras,inv,inputs,pp_result)
 where
  show_result path sems ras inv inputs = intercalate "\n"
    [ -- show path
      show_results [] $ zip sems ras
    , show_symstate ctxt inv
    , "INPUTS:"
    , show $ S.toList inputs ]




-- Tries to follow a concrete path, where each jump and conditional jump is evaluated deterministically.
-- Errors out if it cannot decide where some jump leads to.
symbolically_execute_until :: BinaryClass bin => Context bin -> Int -> IS.IntSet -> SymState -> StateT Int IO (S.Set SymState)
symbolically_execute_until ctxt@(bin,config) a as symstate
  | a `IS.member` as = return $ S.singleton symstate
  | otherwise        = do
    count <- get
    put $ count + 1
    Just i          <- liftIO $ fetch_instruction bin $ fromIntegral a
    if count >= 1000 || isRet (inOperation i) then
      return $ S.singleton symstate
    else do
      let sem          = instr_to_semantics ctxt i
      let symstates'   = execState (run i sem count) (S.singleton symstate)
      symstates''     <- mapM (\symstate' -> symbolically_execute_until ctxt (fromIntegral $ read_RIP symstate') as symstate') $ S.toList symstates'
      return $ S.unions symstates''
 where
  run :: Instruction -> ASemantics -> Int -> State (S.Set SymState) ()
  run i sem count
    | isCondJump (inOperation i) = gets (S.unions . S.map (evalState (do_cond_jump i (inOperation i)))) >>= put
    | isJump (inOperation i) = determinize $ do_jump sem
    | otherwise = determinize $ do_normal_instr sem count

  determinize :: State SymState () -> State (S.Set SymState) ()
  determinize m = (gets $ S.map (execState m)) >>= put


  do_normal_instr (ASemantics sem rip si _) count = do
      set_rip (rip + si)
      tau ctxt count True rip si sem

  do_jump sem@(ASemantics (Jump src _) rip si _) = do
    set_rip (rip + si)
    trgt <- sread_src ctxt src
    case (src,trgt) of
      (SE_Immediate _, SE_Immediate a') -> set_rip (rip + a') 
      (_             , SE_Immediate a') -> set_rip a'
      _                                 -> return () -- TODO when this happens
      
    

  do_cond_jump :: Instruction -> Opcode -> State SymState (S.Set SymState)
  do_cond_jump i JZ   = do_cond_jump_on_flags i ["ZF"]      (\[zf] -> zf)
  do_cond_jump i JNZ  = do_cond_jump_on_flags i ["ZF"]      (\[zf] -> not zf)
  do_cond_jump i JBE  = do_cond_jump_on_flags i ["CF","ZF"] (\[cf,zf] -> cf || zf)
  do_cond_jump i JNBE = do_cond_jump_on_flags i ["CF","ZF"] (\[cf,zf] -> not cf && not zf)
  do_cond_jump i JB   = do_cond_jump_on_flags i ["CF"]      (\[cf] -> cf)
  do_cond_jump i JNB  = do_cond_jump_on_flags i ["CF"]      (\[cf] -> not cf)
  do_cond_jump i JNLE = do_cond_jump_on_flags i ["SG"]      (\[sg] -> sg)
  do_cond_jump i op   = do
    symstate <- get
    error $ "unsupported conditional jump: " ++ show i ++ "\n" ++ show_symstate ctxt symstate

  do_cond_jump_on_flags i flgs cond = do
    flg_values <- mapM sread_flag flgs
    if Nothing `elem` flg_values then
      do_jump_both i
    else
      do_jump_if (cond $ map fromJust flg_values) i

  do_jump_both i = do
    symstate <- get
    return $ S.fromList [execState (do_jump_if True i) symstate, execState (do_jump_if False i) symstate]
  do_jump_if True  i@(Instruction a _ _ ops _ si) = do
    do_jump $ (ASemantics (Jump (operand_to_expr $ ops!!0) i) a (fromIntegral si) False)
    gets S.singleton
  do_jump_if False i@(Instruction a _ _ ops _ si) = do
    set_rip (a + fromIntegral si)
    gets S.singleton


  sread_flag flg = do
    SymState regs mem flgs invs <- get
    case flgs of
      Just (CMP,SE_Immediate i0,SE_Immediate i1) -> return $ Just $ flag_after_CMP flg i0 i1
      _ -> return Nothing

  flag_after_CMP "ZF" i0 i1 = i0 == i1
  flag_after_CMP "CF" i0 i1 = i0  < i1
  flag_after_CMP "SG" i0 i1 = (fromIntegral i0::Int64) > (fromIntegral i1::Int64)







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
resolved_operands_to_inputs (ASemantics (Push _ _) _ _ _,_) = S.empty
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
data ASemantics = ASemantics {
    asem_execution :: AExecution
  , asem_rip :: Word64
  , asem_size :: Word64
  , asem_writesToFlags :: Bool
  }
 deriving Eq

data AExecution =
    Call SimpleExpr Instruction -- ^ A call to a function
  | Ret -- ^ Return
  | Jump SimpleExpr Instruction -- ^ A jump
  | CondJump Opcode -- ^ A jump
  | SysCall 
  | Nop -- ^ A NOP
  | Lea SimpleExpr SimpleExpr -- ^ Load Effective Addresss
  | Push SimpleExpr Int -- ^ Push
  | Pop SimpleExpr Int -- ^ Pop
  | Leave -- ^ Leave
  | SimpleExpr Int -- ^ Pop
  | Mov SimpleExpr SimpleExpr -- ^ MOV
  | MovZX SimpleExpr SimpleExpr Int -- ^ MOV with zero extension
  | SExtend SimpleExpr Int SimpleExpr Int -- ^ Sign extension
  | SetXX SimpleExpr -- ^ SetXX functions (e.g., SETE, SETNE)
  | Apply Operator Int SimpleExpr [SimpleExpr] -- ^ A generic operator (e.g., ADD, XOR)
  | ApplyWhenImm Operator Int SimpleExpr [SimpleExpr] -- ^ A generic operator applied only when one argument is an immediate (e.g., AND, OR)
  | SetFlag Opcode SimpleExpr SimpleExpr -- ^ Set the flag according to a mnemonic (e.g., CMP) applied to two operands
  | NoSemantics Opcode (Maybe SimpleExpr) [SimpleExpr] -- ^ No relevant semantics (e.g., floating points)
 deriving Eq


instance Show AExecution where
  show (Call src _ )                    = (pad_to 11 $ delim "CALL") ++ show_srcs [src]
  show (Jump src i)                     = (pad_to 11 $ delim "JUMP") ++ show_srcs [src]
  show (CondJump op)                    = (pad_to 11 $ delim $ show op) 
  show (SysCall)                        = "Syscall"
  show (Ret)                            = (pad_to 11 $ delim "RET")
  show (Nop)                            = (pad_to 11 $ delim "NOP")
  show (Push src _)                     = (pad_to 11 $ delim "PUSH") ++ show_srcs [src]
  show (Pop dst _)                      = (pad_to 11 $ delim "POP") ++ show_dst_srcs dst []
  show (Leave)                          = (pad_to 11 $ delim "LEAVE")
  show (Lea  dst src)                   = (pad_to 11 $ delim "LEA") ++ show_dst_srcs dst [src]
  show (Mov  dst src)                   = (pad_to 11 $ delim "MOV") ++ show_dst_srcs dst [src]
  show (MovZX dst src _)                = (pad_to 11 $ delim "MOVZX") ++ show_dst_srcs dst [src]
  show (SExtend dst h src l)            = (pad_to 11 $ delim "SEXT") ++ show_dst_srcs dst [src]
  show (SetXX dst)                      = (pad_to 11 $ delim "SETXX") ++ show_maybe_dst (Just dst) ++ "_"
  show (Apply op op_si dst srcs)        = pad_to 11 (delim (show op)) ++ show_dst_srcs dst srcs
  show (ApplyWhenImm op op_si dst srcs) = pad_to 11 (delim (show op)) ++ show_dst_srcs dst srcs
  show (SetFlag op src0 src1)           = pad_to 11 (delim (show op)) ++ show_srcs [src0,src1]
  show (NoSemantics op dst srcs)        = pad_to 11 ("#" ++ delim (show op)) ++ show_maybe_dst dst ++ show_srcs srcs

instance Show ASemantics where
  show (ASemantics aexec rip si _ ) = pad_to 10 ("0x" ++ showHex rip) ++ show aexec


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
instr_to_semantics :: BinaryClass bin => Context bin -> Instruction -> ASemantics
instr_to_semantics ctxt i@(Instruction a prefix mnemonic ops info si) = ASemantics (instr_to_aexecution ctxt i) a (fromIntegral si) (WritesToFlags `elem` info)

instr_to_aexecution :: BinaryClass bin => Context bin -> Instruction -> AExecution
instr_to_aexecution ctxt i@(Instruction _ _ LEA      [dst,src] _ _)       = Lea (operand_to_expr dst) (operand_to_expr src) 
instr_to_aexecution ctxt i@(Instruction _ _ ADD      [dst,src0] _ _)      = mk_apply Plus   dst [dst,src0] i
instr_to_aexecution ctxt i@(Instruction _ _ SUB      [dst,src0] _ _)      = mk_apply Minus  dst [dst,src0] i
instr_to_aexecution ctxt i@(Instruction _ _ NEG      [dst] _ _)           = mk_apply Minus  dst [Op_Imm $ Immediate (BitSize 64) 0,dst] i
instr_to_aexecution ctxt i@(Instruction _ _ INC      [dst] _ _)           = mk_apply Plus   dst [dst,Op_Imm $ Immediate (BitSize 64) 1] i
instr_to_aexecution ctxt i@(Instruction _ _ DEC      [dst] _ _)           = mk_apply Minus  dst [dst,Op_Imm $ Immediate (BitSize 64) 1] i
instr_to_aexecution ctxt i@(Instruction _ _ IMUL     [dst,src0] _ _)      = mk_apply Times  dst [dst,src0] i
instr_to_aexecution ctxt i@(Instruction _ _ IMUL_LO  [dst,src0] _ _)      = mk_apply IMulLo dst [dst,src0] i
instr_to_aexecution ctxt i@(Instruction _ _ IMUL_HI  [dst,src0] _ _)      = mk_apply IMulHi dst [dst,src0] i
instr_to_aexecution ctxt i@(Instruction _ _ IDIV_LO  [dst,src0,src1] _ _) = mk_apply SdivLo dst [dst,src0,src1] i
instr_to_aexecution ctxt i@(Instruction _ _ DIV_LO   [dst,src0,src1] _ _) = mk_apply UdivLo dst [dst,src0,src1] i
instr_to_aexecution ctxt i@(Instruction _ _ IDIV_HI  [dst,src0,src1] _ _) = mk_apply SdivHi dst [dst,src0,src1] i
instr_to_aexecution ctxt i@(Instruction _ _ DIV_HI   [dst,src0,src1] _ _) = mk_apply UdivHi dst [dst,src0,src1] i
instr_to_aexecution ctxt i@(Instruction _ _ SHL      [dst,src0] _ _)      = mk_apply Shl    dst [dst,src0] i
instr_to_aexecution ctxt i@(Instruction _ _ SHR      [dst,src0] _ _)      = mk_apply Shr    dst [dst,src0] i
instr_to_aexecution ctxt i@(Instruction _ _ SAR      [dst,src0] _ _)      = mk_apply Sar    dst [dst,src0] i
instr_to_aexecution ctxt i@(Instruction _ _ ADC      [dst,src0] _ _)      = mk_apply Adc    dst [dst,src0] i
instr_to_aexecution ctxt i@(Instruction _ _ SBB      [dst,src0] _ _)      = mk_apply Sbb    dst [dst,src0] i
instr_to_aexecution ctxt i@(Instruction _ _ AND      [dst,src0] _ _)      = mk_apply_imm And dst [dst,src0] i
instr_to_aexecution ctxt i@(Instruction _ _ OR       [dst,src0] _ _)      = mk_apply_imm Or  dst [dst,src0] i
instr_to_aexecution ctxt i@(Instruction _ _ CDQ      [dst,src] _ _)       = mk_apply (SExtHi (operand_size_bits dst)) dst [src] i
instr_to_aexecution ctxt i@(Instruction _ _ CQO      [dst,src] _ _)       = mk_apply (SExtHi (operand_size_bits dst)) dst [src] i
instr_to_aexecution ctxt i@(Instruction _ _ CWD      [dst,src] _ _)       = mk_apply (SExtHi (operand_size_bits dst)) dst [dst] i

instr_to_aexecution ctxt i@(Instruction _ _ MOVZX    [dst,src] _ _)       = MovZX (operand_to_expr dst) (operand_to_expr src) (operand_size_bits src)

instr_to_aexecution ctxt i@(Instruction _ _ mnemonic [] _ _)
  | isRet mnemonic            = Ret
  | isSyscall mnemonic        = SysCall 
  | mnemonic == LEAVE         = Leave 
  | mnemonic `elem` nops      = Nop
  | otherwise                 = NoSemantics mnemonic Nothing []


instr_to_aexecution (bin,_) i@(Instruction _ _ mnemonic ops _ _)
  | isCall mnemonic           = Call (operand_to_expr $ ops!!0) i 
  | isJump mnemonic           = Jump (operand_to_expr $ ops!!0) i 
  | isCondJump mnemonic       = CondJump mnemonic
  | mnemonic == PUSH          = Push (operand_to_expr $ ops!!0) (operand_size_bits $ ops!!0) 
  | mnemonic == POP           = Pop (operand_to_expr $ ops!!0) (operand_size_bits $ ops!!0)
  | mnemonic `elem` [CMP]     = SetFlag mnemonic (operand_to_expr $ ops!!0) (operand_to_expr $ ops!!1)
  | mnemonic `elem` nops      = Nop
  | mnemonic `elem` moves     = Mov (operand_to_expr $ ops!!0) (operand_to_expr $ ops!!1)
  | mnemonic `elem` sextends  = SExtend (operand_to_expr $ ops!!0) (operand_size_bits $ ops!!0) (operand_to_expr (ops!!1)) (operand_size_bits (ops!!1)) 
  | mnemonic `elem` setxxs    = SetXX (operand_to_expr $ ops!!0) 
  | mnemonic `elem` cmovs     =
    --if operand_size_bits (ops!!0) == 64 then
      mk_apply Cmov (ops!!0) [ ops!!0 ,ops!!1 ] i
    --else
    --  NoSemantics mnemonic (mk_dst $ ops!!0) (map operand_to_expr $ inSrcs i)
  | mnemonic `elem` xors      =
    if show (ops!!0) == show (ops!!1) then
      Mov  (operand_to_expr $ ops!!0) (SE_Immediate 0)
    else
      mk_apply_imm Xor (ops!!0) [ops!!0,ops!!1] i -- NoSemantics mnemonic (mk_dst $ ops!!0) (map operand_to_expr $ inSrcs i)
  | otherwise                 = NoSemantics mnemonic (mk_dst $ ops!!0) (map operand_to_expr $ inSrcs i)
 where
  mk_dst dst
    | operandIsWritten dst = Just $ operand_to_expr dst
    | otherwise = Nothing



mk_apply op dst srcs i = Apply op (operand_size_bits dst) (operand_to_expr dst) (map operand_to_expr srcs)
mk_apply_imm op dst srcs i = ApplyWhenImm op (operand_size_bits dst) (operand_to_expr dst) (map operand_to_expr srcs)

operand_size_bits op =
  case operand_size op of
    ByteSize si ->  8*si


--TODO: BSR, ROl, ROR,BSWAP, PEXTRB/D/Q
-- TODO: NOT
--TODO TEST
--

-- | Turn a path in the CFG to a list of abstract semantics
path_to_asemantics :: BinaryClass bin => Context bin -> [Instruction] -> [ASemantics]
path_to_asemantics ctxt = map (instr_to_semantics ctxt) . concatMap canonicalize 








----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- Symbolic Execution
----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- A symbolic state stores a mapping from registers to expressions.
-- If the register is not in the mapping, it has not been read or written yet.
-- If the register is assigned Nothing, then its value is unknown.
type Regs = M.Map Register (Maybe SimpleExpr)
type Flags = Maybe (Opcode,SimpleExpr,SimpleExpr)

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
data SymState = SymState {
    symstate_mem   :: SMemory
  , symstate_regs  :: Regs
  , symstate_flags :: Flags
  , symstate_invs  :: [(String,SimpleExpr,SimpleExpr)]
  }
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
insert_storage_into_domain ctxt a' si regs = do
  accs <- get
  case find (aliasses_with_access a' si) accs of
    Just n  -> return n 
    Nothing -> do
      let accs'       = SStorage a' si Initial True : accs
      let (touched,untouched) = runState (partition_domain_touched_by ctxt a' si) accs'
      if False && (any (strictly_encompasses a' si) touched && all (non_strictly_encompasses a' si) touched) then do
        let latest     = True
        let initial    = latest && all is_initial touched
        let val        = if initial then Initial else Written $ read_top_from_statepart (SP_Mem a' si) regs
        let storage    = SStorage a' si val latest
        put $ storage:untouched
        return storage
      else do
        let latest     = all is_latest touched
        let initial    = latest && all is_initial touched
        let val        = if initial then Initial else Written $ read_top_from_statepart (SP_Mem a' si) regs
        let storage    = SStorage a' si val latest
        put $ storage:accs
        return storage
 where
  strictly_encompasses a' si (SRef _) = True
  strictly_encompasses a' si (SStorage a0 si0 v latest0) = si0 < si && (a',si) `encompasses` (a0,si0)

  non_strictly_encompasses a' si (SRef _) = True
  non_strictly_encompasses a' si (SStorage a0 si0 v latest0) = (a',si) `encompasses` (a0,si0)

-- Insert a new memory access into the current memory
-- Returns the access as it is is inserted, as well as the new memory
insert_storage_into_mem ctxt a' si regs (SMemory mem) =
  let dom = get_pointer_domain ctxt $ prune ctxt a' in
    if dom == NoDomain then
      --(SStorage a' si Top False,SMemory mem)
      error $ show (a',si) ++ "\n" ++ show_smemory ctxt (SMemory mem)
    else
      let SDomain accs = M.findWithDefault (SDomain []) dom mem
          (acc,accs')  = runState (insert_storage_into_domain ctxt a' si regs) accs in
        (acc,SMemory $ M.insert dom (SDomain accs') mem)

-- Assume that a new access [a',si] has already been inserted into the current list of accesses.
-- Retrieve all the accesses from the current list of accesses that are possibly "touched" when 
-- writing to the new access [a',si]. 
partition_domain_touched_by :: BinaryClass bin => Context bin -> SimpleExpr -> Int -> State [SAccess] [SAccess]
partition_domain_touched_by ctxt a' si = do
  touched0     <- extract (overlapping_access a' si)
  mem <- get
  if touched0 == [] then error $ show (a',si,mem) else return ()
  not_touched0 <- get
  dirty_below  <- extract (is_dirty_below touched0 not_touched0)
  above        <- if is_dirty ctxt a' then go_contiguous_upwards_all touched0 else return []
  let ret       = concat [touched0,dirty_below,above]
  return ret
 where
  is_dirty_below touched0 not_touched0 acc@(SRef _)              = False
  is_dirty_below touched0 not_touched0 acc@(SStorage a0 si0 _ _)
    | not (is_dirty ctxt a0) = False
    | otherwise =  
      let (touched,_) = runState (go_contiguous_upwards acc) not_touched0 in
        intersect touched touched0 /= []

  overlapping_access a' si (SRef _)              = False
  overlapping_access a' si (SStorage a0 si0 _ _) = (prune ctxt a',si) `overlaps` (prune ctxt a0,si0)


  overlapping_accesses (SStorage a0 si0 _ _) acc1 = overlapping_access a0 si0 acc1

  go_contiguous_upwards_all = concatMapM go_contiguous_upwards

  go_contiguous_upwards (SStorage a si _ _) = do
    above0 <- extract (is_contiguous_above a si)
    above1 <- extract (\acc1 -> any (overlapping_accesses acc1) above0)
    above2 <- concatMapM go_contiguous_upwards (above0 ++ above1)
    return $ concat [above0, above1, above2]
 
  is_contiguous_above a si (SRef _)              = False
  is_contiguous_above a si (SStorage a0 si0 _ _) =
    case distance (prune ctxt a) si (prune ctxt a0) of
      Just d  -> not (testBit d 63) && (fromIntegral d::Int64) < fromIntegral si
      Nothing -> False

  extract f = do
    (yes,no) <- gets $ partition f 
    put no
    return yes

  is_dirty ctxt a = prune ctxt a /= a


distance :: SimpleExpr -> Int -> SimpleExpr -> Maybe Word64
distance a si a' = 
  case simp $ SE_Op Minus 64 [a',SE_Op Plus 64 [a,SE_Immediate $ fromIntegral si]] of
    SE_Immediate imm -> Just imm
    _                -> Nothing





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

show_sdomain ctxt (SDomain accs) = remove_newlines $ T.drawForest $ groups_to_forest $ map (sortBy (compare_accesses ctxt)) $ group_domain ctxt $ sortBy (compare_accesses ctxt) accs
 where
  groups_to_forest = map group_to_child
  group_to_child group
    | length group > 1 = T.Node (mk_group_header (head group)) $ map mk_node group
    | otherwise        = mk_node $ head group

  mk_node acc = T.Node (show acc) []

  mk_group_header (SStorage a si _ _) = "<<" ++ show (prune ctxt a) ++">>"

show_smemory ctxt (SMemory mem) = remove_newlines $ intercalate "\n" $ map (show_sdomain ctxt) $ M.elems mem

show_smemory_html :: BinaryClass bin => Context bin -> SMemory -> String
show_smemory_html ctxt (SMemory mem) = TV.htmlTree Nothing $ T.Node header $ concatMap sdomain_to_forest $ M.elems mem
 where
  sdomain_to_forest (SDomain accs) = groups_to_forest $ map (sortBy (compare_accesses ctxt)) $ group_domain ctxt $ sortBy (compare_accesses ctxt) accs

  groups_to_forest = map group_to_child
  group_to_child group
    | length group > 1 = T.Node (mk_group_header (head group)) $ map mk_node group
    | otherwise        = mk_node $ head group

  mk_node acc = T.Node (TV.NodeInfo TV.InitiallyExpanded (show_saccess acc) "") []

  show_saccess (SStorage a si _ _) = "[" ++ show a ++ "," ++ show si ++ "]"

  header = TV.NodeInfo TV.InitiallyExpanded "" ""
  mk_group_header (SStorage a si _ _) = TV.NodeInfo TV.InitiallyExpanded ("<<" ++ show (prune ctxt a) ++ ">>") ""

  
group_domain ctxt [] = []
group_domain ctxt mem@((SStorage a0 si0 _ _):accs) =
  let (touched,not_touched) = runState (partition_domain_touched_by ctxt a0 si0) mem in
    touched : group_domain ctxt not_touched

remove_newlines []              = []
remove_newlines ('\n':'\n':str) = remove_newlines ('\n':str)
remove_newlines (c:str)         = c : remove_newlines str


is_initial (SStorage ptr si Initial latest)     = latest
is_initial (SStorage ptr si Top     _)          = False
is_initial (SStorage ptr si (Written v) latest) = latest && v == SE_Var (SP_Mem ptr si) && not (contains_statepart ptr)

is_latest (SStorage _ _ _ latest) = latest


compare_accesses ctxt (SStorage a0 si0 _ _) (SStorage a1 si1 _ _) = is_below ctxt (a0,si0) (a1,si1)
compare_accesses ctxt (SStorage a0 si0 _ _) (SRef a1)             = is_below ctxt (a0,si0) (a1,0)
compare_accesses ctxt (SRef a0)             (SStorage a1 si1 _ _) = is_below ctxt (a0,0)   (a1,si1)
compare_accesses ctxt (SRef a0)             (SRef a1)             = is_below ctxt (a0,0)   (a1,0)

is_below ctxt (a0,si0) (a1,si1) =
  let a0' = prune ctxt a0
      a1' = prune ctxt a1
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
show_symstate ctxt (SymState mem regs flgs invs) = show_symstate_regs show "\n" regs ++ "\n" ++ show_smemory ctxt mem ++ show_sflags flgs ++ show_symstate_invs invs

show_symstate_invs [] = ""
show_symstate_invs invs = "\nInvariants: " ++ (intercalate " && " $ map (\(cmp,e0,e1) -> show e0 ++ " " ++ cmp ++ " " ++ show e1) invs)

show_sflags Nothing = ""
show_sflags (Just (op,e0,e1)) = "\nFlags: " ++ show op ++ show (e0,e1)

get_mem :: State SymState SMemory
get_mem = get <&> (\(SymState mem _ _ _) -> mem)

get_regs :: State SymState Regs
get_regs = get <&> (\(SymState _ regs _ _) -> regs)

get_flags :: State SymState Flags
get_flags = get <&> (\(SymState _ _ flgs _) -> flgs)

modify_regs :: (Regs -> Regs) -> State SymState ()
modify_regs f = modify (\(SymState mem regs flgs invs) -> SymState mem (f regs) flgs invs)



read_top_from_statepart :: StatePart -> Regs -> SimpleExpr
read_top_from_statepart sp regs = do
  let Just (Just rip) = M.lookup (Reg64 RIP) regs in
    SE_StatePart (replace_RIP rip sp) $ Just $ show rip
 where
  replace_RIP rip (SP_Mem a si) = SP_Mem (simp (substE id (SE_StatePart (SP_Reg (Reg64 RIP)) Nothing) rip a)) si
  replace_RIP rip sp            = sp


sread_mem :: BinaryClass bin => Context bin -> SimpleExpr -> SimpleExpr -> Int -> State SymState SimpleExpr
sread_mem ctxt a a' si = do
  -- 1.) insert region into memory model
  SymState mem regs flgs invs <- get
  let (st,mem') = insert_storage_into_mem ctxt a' si regs mem
  put $ SymState mem' regs flgs invs
  -- 2.) use the state of the access to retrieve a value
  case st of
    (SStorage _ _ val latest) -> mk_val val latest
 where
  mk_val _           False = get_regs <&> (read_top_from_statepart $ SP_Mem a' si)
  mk_val Top         _     = get_regs <&> (read_top_from_statepart $ SP_Mem a' si)
  mk_val (Written v) True  = return $ v
  mk_val Initial     True  = return $ read_unwritten_mem ctxt a' si


read_unwritten_mem ctxt@(bin,_) a'@(SE_Immediate imm) si = try_read_reloc imm si `orTry` try_read_symbol imm si `orTry` (SE_Immediate <$> read_from_ro_datasection bin imm si) `orElse` (SE_Var $ SP_Mem a' si)
 where
  -- TODO only if si==8?
  try_read_reloc a si =
    case IM.lookup (fromIntegral a) $ binary_get_relocations bin of
      Just (Relocation a1) -> Just $ SE_Immediate a1
      Nothing -> Nothing

  try_read_symbol a si =
    case IM.lookup (fromIntegral a) $ binary_get_symbol_table bin of
      Just (PointerToInternalFunction f a1)       -> Just $ SE_Immediate a1
      Just (Relocated_ResolvedObject o a1 addend) -> Just $ SE_Immediate $ fromIntegral $ fromIntegral a1 + addend
      Just (PointerToExternalFunction f)          -> Just $ SE_Var $ SP_Mem (SE_Immediate a) si
      Just (PointerToObject f True _ _)           -> Just $ SE_Var $ SP_Mem (SE_Immediate a) si
      _                                           -> Nothing
read_unwritten_mem ctxt a' si = SE_Var $ SP_Mem a' si


swrite_mem :: BinaryClass bin => Context bin -> SimpleExpr -> Int -> Maybe SimpleExpr -> State SymState ()
swrite_mem ctxt a' si v' = do
  -- 1.) insert region into memory model
  SymState mem regs flgs invs <- get
  let (_,SMemory mem') = insert_storage_into_mem ctxt a' si regs mem
   -- 2.) overwrite all regions that are touched by doing the current write
  let dom = get_pointer_domain ctxt $ prune ctxt a'
  let mem'' = M.adjust (dom_write regs) dom mem'
  put $ SymState (SMemory mem'') regs flgs invs
 where 
  dom_write regs (SDomain accs) = 
    let (touched,not_touched) = runState (partition_domain_touched_by ctxt a' si) accs
        tr = if length touched > 1 then trace ("\nTouched: " ++ show (a',si) ++ "\n" ++ show touched ++"\n") else id in
      SDomain $ map (overwrite_access regs) touched ++ not_touched

  overwrite_access regs acc@(SRef _) = acc
  overwrite_access regs acc@(SStorage a0 si0 v latest0) 
    | (a',si) `aliasses_with` (a0,si0)  = SStorage a0 si0 (mk_val regs v') True
    | (a',si) `enclosed_in`   (a0,si0)  = SStorage a0 si0 Top False
    | (a',si) `encompasses`   (a0,si0)  = SStorage a0 si0 Top False -- TODO use take_bytes
    | otherwise                         = SStorage a0 si0 Top False -- trace ("Overwriting: " ++ show (a',si,a0,si0)) $ 

  mk_val regs Nothing  = Written $ read_top_from_statepart (SP_Mem a' si) regs
  mk_val regs (Just e) = Written $ e

  -- TODO?
  take_bytes si Top         = Top
  take_bytes si Initial     = Initial
  take_bytes si (Written v) = Written $ simp $ SE_Bit (8*si) v

get_pointer_bases :: BinaryClass bin => Context bin -> SimpleExpr -> S.Set PointerBase
get_pointer_bases ctxt@(bin,_) a = get_pointer_base_set bin empty_finit a

 
get_pointer_domain :: BinaryClass bin => Context bin -> SimpleExpr -> PointerDomain
get_pointer_domain ctxt@(bin,_) a' =
  let bases = get_pointer_bases ctxt a' in
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


has_pointer_domain :: BinaryClass bin => Context bin -> SimpleExpr -> Bool
has_pointer_domain ctxt a' = get_pointer_domain ctxt a' /= NoDomain


prune :: BinaryClass bin => Context bin -> SimpleExpr -> SimpleExpr
prune ctxt = prune'' ctxt $ SE_Immediate 0

prune_to_bot ctxt = prune'' ctxt $ Bottom RockBottom

prune'' :: BinaryClass bin => Context bin -> SimpleExpr -> SimpleExpr -> SimpleExpr
prune'' ctxt@(bin,config) subst e =
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

sclear_flg = modify $ \(SymState mem regs flgs invs) -> SymState mem regs Nothing invs

swrite_flg op e0 e1 = modify $ \(SymState mem regs flgs invs) -> SymState mem regs (Just (op,e0,e1)) invs

swrite_reg :: Register -> Maybe SimpleExpr -> State SymState ()
swrite_reg r v = do
  curr_v <- sread_reg r
  let ByteSize si = regSize r
  let rr = real_reg r
  regs <- get_regs
  let v' = case v of
             Nothing -> read_top_from_statepart (SP_Reg rr) regs
             Just v  -> v
            
  modify_regs $ do_write rr si curr_v $ Just v'
 where
  do_write rr 64 curr_v v' = M.insert rr v'
  do_write rr 32 curr_v v' = M.insert rr v'
  do_write rr 16 curr_v v' = M.insert rr (simp <$> SE_Bit 128 <$> v')
  do_write rr 10 curr_v v' = id -- ST registers not supported
  do_write rr 8  curr_v v' = M.insert rr v'
  do_write rr 4  curr_v v' = M.insert rr (simp <$> SE_Bit 32 <$> v')
  do_write rr 2  curr_v v' = M.insert rr (simp <$> SE_Overwrite 16 curr_v <$> SE_Bit 16 <$> v')
  do_write rr 1  curr_v v' = M.insert rr (simp <$> SE_Overwrite 8  curr_v <$> SE_Bit 16 <$> v')
  do_write rr si curr_v v' = error $ show (rr,si,curr_v)
  -- TODO writes to high bytes of lower 2 bytes




sread_statepart :: BinaryClass bin => Context bin -> StatePart -> State SymState SimpleExpr
sread_statepart ctxt (SP_Reg r)    = sread_reg r
sread_statepart ctxt (SP_Mem a si) = do
  as' <- operand_address_to_resolved_exprs ctxt a
  case as' of
    Nothing  -> do
      a' <- sresolve_expr ctxt a
      get_regs <&> (read_top_from_statepart $ SP_Mem a' si)
      -- s <- get
      --error $ "Read from domainless pointer: " ++ show (SP_Mem a si) ++ "\n" ++ show_symstate ctxt s
    Just as' -> do
      rets <- nub <$> mapM do_read as'
      return $ foldr1 (\v0 v1 -> SE_Op Cmov (si*8) [v0,v1]) rets 
 where
  do_read a'
    | has_pointer_domain ctxt a' = sread_mem ctxt a a' si
    | otherwise = get_regs <&> (read_top_from_statepart $ SP_Mem a' si)



sresolve_expr :: BinaryClass bin => Context bin -> SimpleExpr -> State SymState SimpleExpr
sresolve_expr ctxt e@(SE_Immediate _)        = return e
sresolve_expr ctxt e@(SE_Var (SP_Mem a si))  = do
  a_v <- sresolve_expr ctxt a
  return $ read_unwritten_mem ctxt a_v si
sresolve_expr ctxt e@(SE_Var _)              = return e
sresolve_expr ctxt   (SE_StatePart sp _)     = sread_statepart ctxt sp
sresolve_expr ctxt   (SE_Op op si es)        = (simp . SE_Op op si) <$> mapM (sresolve_expr ctxt) es
sresolve_expr ctxt   (SE_Bit n e)            = (simp . SE_Bit n) <$> sresolve_expr ctxt e
sresolve_expr ctxt   (SE_SExtend l h e)      = (simp . SE_SExtend l h) <$> sresolve_expr ctxt e
sresolve_expr ctxt   (SE_Overwrite n e0 e1)  = do
  e0' <- sresolve_expr ctxt e0
  e1' <- sresolve_expr ctxt e1
  return $ simp $ SE_Overwrite n e0 e1
sresolve_expr ctxt e@(Bottom _)              = return e
sresolve_expr ctxt e@(SE_Malloc _ _)         = return e


-- take @a@: the address as it occurs in the operand of an instruction.
-- For example: RAX + RBX*4 in the memory operand QWORD PTR [RAX + RBX*4]
-- Try to resolve this address to a symbolic value by reading its inputs.
operand_address_to_resolved_exprs :: BinaryClass bin => Context bin -> SimpleExpr -> State SymState (Maybe [SimpleExpr])
operand_address_to_resolved_exprs ctxt a = do
  a'      <- sresolve_expr ctxt a
  let as'  = [a'] -- nub $ map simp $ unfold_cmovs a'

  if all (has_pointer_domain ctxt) as' then
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
    let bases = get_pointer_bases ctxt a'
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



soverwrite_dst :: BinaryClass bin => Context bin -> SimpleExpr -> Maybe SimpleExpr -> State SymState ()
soverwrite_dst ctxt (SE_StatePart (SP_Reg r) _) = swrite_reg (real_reg r)
soverwrite_dst ctxt sp = swrite_dst ctxt sp



swrite_dst :: BinaryClass bin => Context bin -> SimpleExpr -> Maybe SimpleExpr -> State SymState ()
swrite_dst ctxt (SE_StatePart (SP_Reg r) _)    v' = swrite_reg r v'
swrite_dst ctxt (SE_StatePart (SP_Mem a si) _) v' = do
  as' <- operand_address_to_resolved_exprs ctxt a
  case as' of
    Just as' -> forM_ as' do_write
    Nothing  -> return ()
    --Nothing  -> do
    --  inv <- get
    --  error $ "Writing to baseless address: " ++ show (a,si) ++ "\n" ++ show_symstate ctxt inv
 where
  do_write :: SimpleExpr -> State SymState ()
  do_write a'
    | has_pointer_domain ctxt a' = swrite_mem ctxt a' si (simp . SE_Bit (si*8) <$> v')
    -- TODO check not needed
swrite_dst _ e _ = error $ show e

sread_src :: BinaryClass bin => Context bin -> SimpleExpr -> State SymState SimpleExpr
sread_src ctxt   (SE_StatePart sp _)          = sread_statepart ctxt sp
sread_src ctxt e@(SE_Immediate imm)           = return $ e
sread_src ctxt e@(SE_Overwrite n src0 src1)   = do
  src0' <- sread_src ctxt src0
  src1' <- sread_src ctxt src1
  return $ simp $ SE_Overwrite n src0' src1'
sread_src ctxt e                              = error $ "Reading from " ++ show e



pruned_equal ctxt a0 a1 = prune ctxt a0 == prune ctxt a1

hasDistance a0 a1 = distance a0 0 a1 /= Nothing

smallerDistance a0 a1 = 
  case distance a0 0 a1 of 
    Just d -> if testBit d 63 then GT else if d == 0 then EQ else LT

type ResolvedOperands = M.Map StatePart [SimpleExpr]

tau_path :: BinaryClass bin => Context bin -> [ASemantics] -> SymState -> ([ResolvedOperands], SymState)
tau_path ctxt p symstate = runState (traverse 0 p) symstate
 where
  traverse :: Int -> [ASemantics] -> State SymState [ResolvedOperands]
  traverse n []         = return []
  traverse n p0@(sem@(ASemantics exec rip si writesToFlags):p) = do
    set_rip (asem_size sem + asem_rip sem)
    when writesToFlags $ sclear_flg 

    resolved_ops <- gets $ resolve_operands ctxt sem

    -- regs <- get_regs
    tau ctxt n (does_fall_through p0) rip si exec
    resolved_ops' <- traverse (n+1) p
    return $ resolved_ops : resolved_ops'
    -- return (prune_symstate_for_instruction sem regs:regs')
    -- return (M.empty:regs')

  does_fall_through [sem] = True
  does_fall_through (sem0:sem1:_) = asem_size sem0 + asem_rip sem0 == asem_rip sem1
      


resolve_operands :: BinaryClass bin => Context bin -> ASemantics -> SymState -> ResolvedOperands
resolve_operands ctxt sem inv = 
  let inv' = execState (set_rip (asem_size sem + asem_rip sem)) inv 
      ops  = operands_of sem ++  map (\r -> SE_StatePart (SP_Reg r) Nothing) ((if asem_execution sem == SysCall then syscall_input_registers inv' else []) ++ function_call_input_registers ctxt sem) in
    M.map nub $ M.unionsWith (++) $ map (resolve_operand inv' sem) ops
 where
  resolve_operand inv sem (SE_StatePart (SP_Mem a 0) Nothing) = 
    case evalState (operand_address_to_resolved_exprs ctxt a) inv of
      Just as' -> M.singleton (SP_Mem a 0) as'
      Nothing  -> M.empty
  resolve_operand inv sem (SE_StatePart (SP_Mem a si) Nothing) = 
    case evalState (operand_address_to_resolved_exprs ctxt a) inv of
      Just as' -> let v = evalState (sread_statepart ctxt (SP_Mem a si)) inv in
                    M.fromList [(SP_Mem a 0, as'), (SP_Mem a si, [v])]
      Nothing -> let a' = evalState (sresolve_expr ctxt a) inv
                     v = evalState (sread_statepart ctxt (SP_Mem a si)) inv in
                    M.fromList [(SP_Mem a 0, [a']), (SP_Mem a si, [v])]
  resolve_operand inv sem (SE_StatePart (SP_Reg r) Nothing) =
    let v = evalState (sread_reg r) inv in
      M.singleton (SP_Reg r) [v]
  resolve_operand inv sem (SE_Immediate _) = M.empty
  resolve_operand inv sem e = error $ show e


syscall_input_registers inv = 
  case evalState (sread_reg $ Reg64 RAX) inv of
    SE_Immediate imm -> do
      let argcount = snd $ num_of_input_registers_of_sys_call $ fromIntegral imm in
        take argcount all_input_regs_of_syscalls
    _ -> all_input_regs_of_syscalls

function_call_input_registers ctxt@(bin,config) sem = 
  case instruction_of sem of
    Nothing -> []
    Just i ->
      let trgts = [jump_target_for_instruction bin i] in -- TODO replace with actual symb exec
        if any is_internal trgts then
          []
        else let f = function_name_of_instruction bin i in
          take (get_argcount bin f) all_input_regs_of_functions
 where
  instruction_of (ASemantics (Call _ i) _ _ _) = Just i
  instruction_of (ASemantics (Jump _ i) _ _ _) = Just i
  instruction_of _ = Nothing

  is_internal (ImmediateAddress _) = True
  is_internal _ = False



operands_of :: ASemantics -> [SimpleExpr]
operands_of (ASemantics exec _ _ _) = get exec
 where
  get (Apply op op_si dst srcs)          =  get_dst dst ++ srcs
  get (ApplyWhenImm op op_si dst srcs)   =  get_dst dst ++ srcs
  get (Mov dst src)                      =  get_dst dst ++ [src]
  get (MovZX dst src op_si)              =  get_dst dst ++ [src]
  get (SExtend dst h src l)              =  get_dst dst ++ [src]
  get (Lea dst src)                      =  get_dst dst ++ [src]
  get (Nop)                              = []
  get (Push src _)                       = [] -- [src]
  get (Pop dst _)                        = []
  get (Leave)                            = []
  get (NoSemantics op Nothing srcs)      = srcs
  get (NoSemantics op (Just dst) srcs)   =  get_dst dst ++ srcs
  get (SetXX dst)                        =  get_dst dst
  get (Call src _)                       = [src]
  get (SysCall)                          = [ SE_StatePart (SP_Reg $ Reg64 RAX) Nothing ]
  get (Jump src _)                       = [src]
  get (CondJump _)                       = []
  get (Ret)                              = [ SE_StatePart (SP_Mem (SE_StatePart (SP_Reg $ Reg64 RSP) Nothing) 8) Nothing ]
  get (SetFlag _ src0 src1)              = [src0,src1]

  get_dst dst@(SE_StatePart (SP_Mem a si) Nothing) = [SE_StatePart (SP_Mem a 0) Nothing]
  get_dst _ = []



prune_symstate_for_instruction :: ASemantics -> Regs -> Regs
prune_symstate_for_instruction sem = M.filterWithKey is_relevant
 where
  is_relevant r _ = real_reg r `elem` map real_reg (regs_of_sem sem)

  regs_of_sem (ASemantics exec _ _ _) = regs_of_aexec exec

  regs_of_aexec (Apply op op_si dst srcs)        = concatMap regs_of_op (dst:srcs)
  regs_of_aexec (ApplyWhenImm op op_si dst srcs) = concatMap regs_of_op (dst:srcs)
  regs_of_aexec (Mov dst src)                    = concatMap regs_of_op [dst,src]
  regs_of_aexec (MovZX dst src _)                = concatMap regs_of_op [dst,src]
  regs_of_aexec (SExtend dst _ src _)            = concatMap regs_of_op [dst,src]
  regs_of_aexec (SetXX dst)                      = concatMap regs_of_op [dst]
  regs_of_aexec (NoSemantics op Nothing srcs)    = concatMap regs_of_op srcs
  regs_of_aexec (NoSemantics op (Just dst) srcs) = concatMap regs_of_op (dst:srcs)
  regs_of_aexec (Lea dst src)                    = [] -- regs_of_expr src
  regs_of_aexec (Nop)                            = []
  regs_of_aexec (Push src _ )                    = regs_of_op src ++ [Reg64 RSP]
  regs_of_aexec (Pop dst _ )                     = regs_of_op dst ++ [Reg64 RSP]
  regs_of_aexec (Leave)                          = [Reg64 RSP,Reg64 RBP]
  regs_of_aexec (Call _ _)                       = []
  regs_of_aexec (Jump _ _)                       = []
  regs_of_aexec (CondJump _)                     = []
  regs_of_aexec (Ret)                            = [Reg64 RSP]
  regs_of_aexec (SysCall)                        = [Reg64 RAX]
  regs_of_aexec (SetFlag _ src0 src1)            = concatMap regs_of_op [src0,src1]

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







spush ctxt src op_si = do
  -- RSP -= operand_size
  let si = case src of
             SE_Immediate _ -> 8
             _ -> op_si `div` 8
  let rsp = Reg64 RSP
  rsp_value <- sread_reg rsp
  let new_rsp_value = simp $ SE_Op Minus 64 [rsp_value, SE_Immediate $ fromIntegral si]
  swrite_reg rsp $ Just new_rsp_value
  -- *[RSP,si] := src
  src_value <- sread_src ctxt src
  let sp = SE_StatePart (SP_Mem (SE_StatePart (SP_Reg $ Reg64 RSP) Nothing) si) Nothing
  swrite_dst ctxt sp $ Just src_value


spop ctxt dst op_si = do
  -- dst := *[RSP,si]
  let si = op_si `div` 8
  let sp = SP_Mem (SE_StatePart (SP_Reg $ Reg64 RSP) Nothing) si
  src_value <- sread_statepart ctxt sp
  swrite_dst ctxt dst $ Just src_value
  -- RSP += operand_size
  let rsp = Reg64 RSP
  rsp_value <- sread_reg rsp
  let new_rsp_value = simp $ SE_Op Plus 64 [rsp_value, SE_Immediate $ fromIntegral si]
  swrite_reg rsp $ Just new_rsp_value

sleave ctxt = do
  -- RSP := RBP
  let rbp = Reg64 RBP
  let rsp = Reg64 RSP
  rbp_value <- sread_reg rbp
  swrite_reg rsp $ Just rbp_value
  -- POP RBP
  spop ctxt (SE_StatePart (SP_Reg rbp) Nothing) 64


sret ctxt = do
  -- RIP := *[RSP,8]
  v' <- sread_statepart ctxt $ SP_Mem (SE_StatePart (SP_Reg $ Reg64 RSP) Nothing) 8
  swrite_reg (Reg64 RIP) (Just v') 
  -- RSP += 8
  let rsp = Reg64 RSP
  rsp_value <- sread_reg rsp
  let new_rsp_value = simp $ SE_Op Plus 64 [rsp_value, SE_Immediate 8]
  --traceShow ("RET:", showHex entry, new_rsp_value) $
  swrite_reg rsp $ Just new_rsp_value



sset_flag ctxt op src0 src1 = do
  src0_value <- sread_src ctxt src0
  src1_value <- sread_src ctxt src1
  case (src0_value,src1_value) of
    (SE_Op Minus si [a,SE_Immediate i],v1) -> swrite_flg op a (simp' $ SE_Op Plus si [v1,SE_Immediate i])
    _ -> swrite_flg op src0_value src1_value


scondjump ctxt rip fall_through JA   si = modify $ set_symstate_inv fall_through "<=" ">"
scondjump ctxt rip fall_through JNBE si = modify $ set_symstate_inv fall_through "<=" ">"
scondjump ctxt rip fall_through JBE  si = modify $ set_symstate_inv fall_through ">"  "<="
scondjump ctxt rip fall_through _    si = return ()

set_symstate_inv fall_through cmp0 cmp1 s@(SymState mem regs Nothing invs) = s
set_symstate_inv fall_through cmp0 cmp1 s@(SymState mem regs flgs@(Just (op,e0,e1)) invs) 
  | fall_through = SymState mem regs flgs ((cmp0,e0,e1):invs)
  | otherwise    = SymState mem regs flgs ((cmp1,e0,e1):invs)

  


-- TODO refactor code of sjump and scall, also when partially unresolved
sjump ctxt@(bin,config) n rip i = jump_to_trgts [jump_target_for_instruction bin i]
 where
  jump_to_trgts trgts
    | any is_internal trgts = return ()
    | all isExternal trgts  = do
      let f = function_name_of_instruction bin i
      if isPrefixOf "0x" f then
        error $ show i
      else do
        -- scall ctxt n i rip
        -- sret ctxt
        return () -- TODO!
    | otherwise = error $ show (i,trgts)

  isExternal (External _) = True
  isExternal (ExternalDeref _) = True
  isExternal (Returns True) = True
  isExternal (Unresolved) = True
  isExternal _ = False

  is_internal (ImmediateAddress _) = True
  is_internal _ = False

scall_internal ctxt n f rip = do
  {--let rsp = Reg64 RSP
  rsp_value <- sread_reg rsp
  traceShow ("CALL:", showHex entry, f, rsp_value) $--}
  -- spush ctxt (SE_StatePart (SP_Reg $ Reg64 RIP) Nothing) 64
  let retval = SE_Malloc (Just rip) (Just $ f ++ "_" ++ show n)
  swrite_reg (Reg64 RAX) (Just retval) -- TODO and XMM0?  

scall ctxt@(bin,_) n i rip = call [jump_target_for_instruction bin i] -- TODO
 where
  call trgts
    | any is_internal trgts = scall_internal ctxt n (function_name_of_instruction bin i) rip
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
    | otherwise = 0 -- trace ("Do not know external function: " ++ f) 0

cap_expr e (SE_StatePart sp _)
  | expr_size e > 50 = get_regs <&> (read_top_from_statepart sp)
  | otherwise        = return e

tau :: BinaryClass bin => Context bin -> Int -> Bool -> Word64 -> Word64 -> AExecution -> State SymState ()
tau ctxt n fall_through rip si (Apply op op_si dst srcs)          = do
  when (op == Minus) $ do
    tau ctxt n fall_through rip si $ SetFlag CMP dst (srcs!!1)

  srcs' <- mapM (sread_src ctxt) srcs
  let v = simp $ SE_Op op op_si srcs'
  v' <- cap_expr v dst 
  swrite_dst ctxt dst $ Just v'

tau ctxt n fall_through rip si (ApplyWhenImm op op_si dst srcs)   = do
  srcs' <- mapM (sread_src ctxt) srcs
  let v = simp $ SE_Op op op_si srcs'
  v' <- cap_expr v dst 
  if any is_immediate srcs' then
    swrite_dst ctxt dst $ Just v'
  else
    swrite_dst ctxt dst Nothing  
tau ctxt n fall_through rip si (Mov dst src)                = do
  src' <- sread_src ctxt src
  swrite_dst ctxt dst (Just src') 
tau ctxt n fall_through rip si (MovZX dst src op_si)        = do
  src' <- sread_src ctxt src
  swrite_dst ctxt dst (Just $ simp $ SE_Bit op_si src') 
tau ctxt n fall_through rip si (SExtend dst h src l)        = do
  src' <- sread_src ctxt src
  swrite_dst ctxt dst (Just $ simp $ SE_SExtend l h src') 
tau ctxt n fall_through rip si (Lea dst src)                = do
  let SE_StatePart (SP_Mem a si) _ = src
  src' <- sresolve_expr ctxt a
  swrite_dst ctxt dst (Just src') 
tau ctxt n fall_through rip si (NoSemantics op dst srcs)    = do
  -- Note sources must be read, as reading can influence the memory model
  srcs' <- mapM (sread_src ctxt) srcs
  case dst of
    Nothing  -> return ()
    Just dst -> swrite_dst ctxt dst Nothing  
tau ctxt n fall_through rip si sem@(SysCall)                    = do
  -- Note sources must be read, as reading can influence the memory model
  regs <- gets $ syscall_input_registers 
  mapM_ sread_reg regs
  mapM_ (\r -> swrite_reg r Nothing) $ regs_clobbered_by_syscall
tau ctxt n fall_through rip si sem@(SetFlag op src0 src1) = do
  v <- sread_src ctxt src0
  --case v of
  -- SE_Immediate _ -> swrite_dst ctxt src0 Nothing
  --  _              -> return ()
  sset_flag ctxt op src0 src1

tau ctxt n fall_through rip si (SetXX dst)                  = soverwrite_dst ctxt dst $ Just $ SE_Op ZeroOne 8 []
tau ctxt n fall_through rip si (Call op i)                  = scall ctxt n i rip 
tau ctxt n fall_through rip si (Jump op i)                  = sjump ctxt n rip i
tau ctxt n fall_through rip si (CondJump op)                = scondjump ctxt rip fall_through op si
tau ctxt n fall_through rip si (Nop)                        = return ()

tau ctxt n fall_through rip si (Push src op_si)             = spush ctxt src op_si
tau ctxt n fall_through rip si (Pop dst op_si)              = spop ctxt dst op_si
tau ctxt n fall_through rip si (Leave)                      = sleave ctxt
tau ctxt n fall_through rip si (Ret)                        = sret ctxt








      
                  








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
