{-# LANGUAGE PartialTypeSignatures, MultiParamTypeClasses, FlexibleContexts, Strict #-}

-----------------------------------------------------------------------------
-- |
-- Symbolic execution of sequential lists of instructions of type @`Instr'@ on predicates of type @`Pred`@.
-----------------------------------------------------------------------------

module SymbolicExecution (
  tau_blockID,
  tau_b,
  init_pred,
  statepart_is_preserved_after_function_call,
  gather_stateparts,
  expr_highly_likely_pointer
  ) where

import Base
import SimplePred
import Context
import MachineState
import Propagation
import X86_Datastructures
import CFG_Gen
import Conventions

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Data.Maybe (fromJust)
import Data.Word 
import Control.Monad.State.Strict hiding (join)
import Control.Monad.Extra (anyM,whenM,mapMaybeM)
import Data.List
import Data.Maybe (mapMaybe)
import Debug.Trace





push :: Context -> Operand -> State Pred ()
push ctxt (Immediate imm) = do
  let si = 8
  let address = AddrMinus (FromReg RSP) (AddrImm si)
  e1 <- resolve_address address
  write_reg RSP e1
  write_mem ctxt address e1 si (SE_Immediate imm)
push ctxt op1 = do
  e0 <- read_operand ctxt op1
  let si = operand_size op1
  let address = AddrMinus (FromReg RSP) (AddrImm $ fromIntegral si)
  e1 <- resolve_address address
  write_reg RSP e1
  write_mem ctxt address e1 si e0

pop :: Context -> Operand -> State Pred ()
pop ctxt op1 = do
  let si = operand_size op1
  e0 <- read_mem ctxt (SizeDir si (FromReg RSP))
  e1 <- resolve_address (AddrPlus (FromReg RSP) (AddrImm $ fromIntegral si))
  write_reg RSP e1
  write_operand ctxt op1 e0

lea :: Context -> Operand -> Operand -> State Pred ()
lea ctxt op1 (Address a) = do
  e <- resolve_address a
  write_operand ctxt op1 e


leave :: Context -> State Pred ()
leave ctxt = mov ctxt (Reg RSP) (Reg RBP) >> pop ctxt (Reg RBP)





add_call_verification_conditions :: Context -> Instr -> State Pred ()
add_call_verification_conditions ctxt i = do
  local_params <- mapMaybeM when_local parameter_registers
  when (local_params /= []) (do
    let f = function_name_of_instruction ctxt i
    modify $ add_function_constraint f local_params
   )
 where
  when_local r = do
    v <- read_reg r
    if S.singleton Local == expr_to_addr_type ctxt v then
      return $ Just (r,v)
    else
      return Nothing



call :: Context -> Instr -> State Pred ()
call ctxt i = do
  add_call_verification_conditions ctxt i
  Predicate eqs flg vcs muddle_status <- get
  let eqs' = M.mapWithKey (statepart_after_function_call ctxt i) eqs
  put $ Predicate eqs' None vcs (new_muddle_status muddle_status)
  forM_ return_registers (\r -> write_reg r $ Bottom FromCall $ S.singleton (Src_Function $ function_name_of_instruction ctxt i))
 where
  new_muddle_status Muddled = Muddled
  new_muddle_status _       = if instruction_jumps_to_external ctxt i then ExternalOnly else Muddled


statepart_after_function_call ctxt i sp v  = if statepart_is_preserved_after_function_call ctxt i sp then v else mk_bottom_after_function_call ctxt i $ srcs_of_expr v

-- | Returns true iff the given statepart is preserved (unmodified) after a function call.
-- This happens if the statepart is local, if the address belongs to an unwritable data section, or if the function call is external and the statepart belongs to a section that cannot be modified by external functions according to the "Conventions".
statepart_is_preserved_after_function_call ::
  Context      -- ^ The context
  -> Instr     -- ^ The instruction currently symbolically executed (a @call@)
  -> StatePart -- ^ A state part 
  -> Bool
statepart_is_preserved_after_function_call ctxt i (SP_Reg r)   = r `elem` callee_saved_registers 
statepart_is_preserved_after_function_call ctxt i (SP_Mem a _) =
      S.toList (expr_to_addr_type ctxt a) == [Local]
   || address_is_unwritable ctxt a 
   || (instruction_jumps_to_external ctxt i && address_is_unmodifiable_by_external_functions ctxt a)

mk_bottom_after_function_call ctxt i srcs = Bottom FromSemantics $ S.insert (Src_Function $ function_name_of_instruction ctxt i) srcs


ret ctxt = pop ctxt (Reg RIP)

sysret ctxt = do
  e0 <- read_operand ctxt (Reg RCX)
  write_rreg RIP e0
  write_flags (\_ _ -> None) (Reg RCX) (Reg RCX)

jmp ctxt i =
  if instruction_jumps_to_external ctxt i then
    -- A jump to an external symbol is treated as a function call and implicit RET
    call ctxt i >> ret ctxt
  else
    return ()



write_flags :: (Operand -> Operand -> FlagStatus) -> Operand -> Operand -> State Pred ()
write_flags g op1 op2 = do
  Predicate eqs flg vcs muddle_status <- get
  put $ Predicate eqs (g op1 op2) vcs muddle_status


mov_with_func_op2_to_op1 :: Context -> (SimpleExpr -> SimpleExpr) -> Operand -> Operand -> State Pred ()
mov_with_func_op2_to_op1 ctxt f op1 op2 = do
  e1 <- read_operand ctxt op2
  write_operand ctxt op1 (f e1)

mk_bottom es = Bottom FromSemantics $ S.unions (map srcs_of_expr es)

mov_with_func1 :: Context -> ([SimpleExpr] -> SimpleExpr) -> Bool -> Operand -> State Pred ()
mov_with_func1 ctxt f do_write_flags op1 = do
  e0 <- read_operand ctxt op1
  write_operand ctxt op1 (f [e0])
  when do_write_flags (write_flags (\_ _ -> None) op1 op1)

mov_with_func :: Context -> ([SimpleExpr] -> SimpleExpr) -> Bool -> Operand -> Operand -> State Pred ()
mov_with_func ctxt f do_write_flags op1 op2 = do
  e0 <- read_operand ctxt op1
  e1 <- read_operand ctxt op2
  write_operand ctxt op1 (f [e0,e1])
  when do_write_flags (write_flags (\_ _ -> None) op1 op2)

mov_with_func3 :: Context -> ([SimpleExpr] -> SimpleExpr) -> Bool -> Operand -> Operand -> Operand -> State Pred ()
mov_with_func3 ctxt f do_write_flags op1 op2 op3 = do
  e0 <- read_operand ctxt op1
  e1 <- read_operand ctxt op2
  e2 <- read_operand ctxt op3
  write_operand ctxt op1 (f [e0,e1,e2])
  when do_write_flags (write_flags (\_ _ -> None) op2 op3)

nop ctxt = return ()

ud2 ctxt = return ()

hlt ctxt = return ()

wait ctxt = return ()

mfence ctxt = return ()

clflush ctxt = return ()

mov ctxt = mov_with_func_op2_to_op1 ctxt id 

movzx ctxt op1 op2 = do
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 e2

movsx ctxt op1 op2 = do
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 (SE_SExtend (8 * operand_size op2) (8 * operand_size op1) e2)

movsxd ctxt op1 op2 = do
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 (SE_SExtend (8 * operand_size op2) (8 * operand_size op1) e2)

movsd = mov

movss = mov

movaps = mov

movapd = mov

movups = mov

movupd = mov

movabs = mov

movdqu = mov

movdqa = mov

movlpd = mov

movlps = mov

movd   = mov

movq   = mov

cmov ctxt op1 op2 = do
  e0 <- read_operand ctxt op1
  e1 <- read_operand ctxt op2
  -- TODO check whetehr already contains ND
  write_operand ctxt op1 $ Bottom (FromNonDeterminism $ S.fromList [e0,e1]) $ srcs_of_exprs e0 e1

xchg :: Context -> Operand -> Operand -> State Pred ()
xchg ctxt op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 e2
  write_operand ctxt op2 e1


cmp ctxt = write_flags $ FS_CMP Nothing

add ctxt op1 = mov_with_func ctxt (SE_Op (Plus (8 * operand_size op1))) True op1

sub ctxt op1 = mov_with_func ctxt (SE_Op (Minus (8 * operand_size op1))) True op1

neg ctxt op1 = mov_with_func1 ctxt (\e -> SE_Op (Minus (8 * operand_size op1)) (SE_Immediate 0 : e) ) True op1

test ctxt = write_flags (\_ _ -> None) --TODO needed?

ucomisd ctxt = write_flags (\_ _ -> None)

ucomiss ctxt = write_flags (\_ _ -> None)

inc :: Context -> Operand -> State Pred ()
inc ctxt op1 = do
  e1 <- read_operand ctxt op1
  write_operand ctxt op1 (SE_Op (Plus (8 * operand_size op1)) [e1,SE_Immediate 1])
  write_flags (\_ _ -> None) op1 op1

dec :: Context -> Operand -> State Pred ()
dec ctxt op1 = do
  e1 <- read_operand ctxt op1
  write_operand ctxt op1 (SE_Op (Minus (8 * operand_size op1)) [e1,SE_Immediate 1])
  write_flags (\_ _ -> None) op1 op1

or' :: Context -> Operand -> Operand -> State Pred ()
or' ctxt op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 (SE_Op (Or (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2

and' :: Context -> Operand -> Operand -> State Pred ()
and' ctxt op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 (SE_Op (And (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2


not' :: Context -> Operand -> State Pred ()
not' ctxt op1 = do
  e1 <- read_operand ctxt op1
  write_operand ctxt op1 (SE_Op (Not (8 * operand_size op1)) [e1])
  write_flags (\_ _ -> None) op1 op1


xor ctxt op1 op2 = do
  if op1 == op2 then
    mov_with_func ctxt (\x -> SE_Immediate 0) True op1 op2
  else do
    e1 <- read_operand ctxt op1
    e2 <- read_operand ctxt op2
    write_operand ctxt op1 (SE_Op (Xor (8 * operand_size op1)) [e1,e2])
    write_flags (\_ _ -> None) op1 op2

{--
setxx :: Context -> Operand -> State Pred ()
setxx ctxt op1 = do
  e1 <- read_operand ctxt op1
  write_operand ctxt op1 (SE_Op SetXX [e1])
  write_flags (\_ _ -> None) op1 op1
--}
setxx ctxt = mov_with_func1 ctxt mk_bottom False

pxor = xor --TODO flags?

pand ctxt = mov_with_func ctxt mk_bottom False

pandn ctxt = mov_with_func ctxt mk_bottom False

por ctxt = mov_with_func ctxt mk_bottom False

ptest ctxt = mov_with_func ctxt mk_bottom True


xorpd = xor --TODO flags?


xorps = xor -- TODO flags?

andpd ctxt = mov_with_func ctxt mk_bottom False

andnpd ctxt = mov_with_func ctxt mk_bottom False

orpd ctxt = mov_with_func ctxt mk_bottom False

subpd ctxt = mov_with_func ctxt mk_bottom False

addpd ctxt = mov_with_func ctxt mk_bottom False

subss ctxt = mov_with_func ctxt mk_bottom False

addss ctxt = mov_with_func ctxt mk_bottom False

mulss ctxt = mov_with_func ctxt mk_bottom False

divss ctxt = mov_with_func ctxt mk_bottom False

roundss ctxt = mov_with_func ctxt mk_bottom False

subsd ctxt = mov_with_func ctxt mk_bottom False

addsd ctxt = mov_with_func ctxt mk_bottom False

mulsd ctxt = mov_with_func ctxt mk_bottom False

divsd ctxt = mov_with_func ctxt mk_bottom False

roundsd ctxt = mov_with_func ctxt mk_bottom False

bt :: Context -> Operand -> Operand -> State Pred ()
bt ctxt op1 op2 = do
  write_flags (\_ _ -> None) op1 op2

btc ctxt = mov_with_func ctxt mk_bottom True

btr ctxt = mov_with_func ctxt mk_bottom True

bsr ctxt op1 op2 = do
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 $ SE_Op (Bsr (8 * operand_size op1)) [e2]
  write_flags (\_ _ -> None) op1 op2

bsf ctxt = mov_with_func ctxt mk_bottom True

bts ctxt = mov_with_func ctxt mk_bottom True

paddd ctxt = mov_with_func ctxt mk_bottom False

paddb ctxt = mov_with_func ctxt mk_bottom False

paddq ctxt = mov_with_func ctxt mk_bottom False

psubd ctxt = mov_with_func ctxt mk_bottom False

psubb ctxt = mov_with_func ctxt mk_bottom False

psubq ctxt = mov_with_func ctxt mk_bottom False

psrld ctxt = mov_with_func ctxt mk_bottom False

psrlw ctxt = mov_with_func ctxt mk_bottom False

psrldq ctxt = mov_with_func ctxt mk_bottom False

pslldq ctxt = mov_with_func ctxt mk_bottom False

psllq ctxt = mov_with_func ctxt mk_bottom False

psrlq ctxt = mov_with_func ctxt mk_bottom False

pmulld ctxt = mov_with_func ctxt mk_bottom False

pminud ctxt = mov_with_func ctxt mk_bottom False

pminsd ctxt = mov_with_func ctxt mk_bottom False

pmaxud ctxt = mov_with_func ctxt mk_bottom False

pmaxuq ctxt = mov_with_func ctxt mk_bottom False

psubusb ctxt = mov_with_func ctxt mk_bottom False

psubusw ctxt = mov_with_func ctxt mk_bottom False

packssdw ctxt = mov_with_func ctxt mk_bottom False

packsswb ctxt = mov_with_func ctxt mk_bottom False

cvtss2sd ctxt = mov_with_func ctxt mk_bottom False

cvtsd2ss ctxt = mov_with_func ctxt mk_bottom False

cvtsi2sd ctxt = mov_with_func ctxt mk_bottom False

cvtsi2ss ctxt = mov_with_func ctxt mk_bottom False

cvttss2si ctxt = mov_with_func ctxt mk_bottom False

cvttsd2si ctxt = mov_with_func ctxt mk_bottom False

cvttpd2dq ctxt = mov_with_func ctxt mk_bottom False

cvtdq2pd ctxt = mov_with_func ctxt mk_bottom False

is_st_reg_operand (Reg r) = take 2 (show r) == "ST"
is_st_reg_operand _       = False

fst' ctxt op1 =
  if is_st_reg_operand op1 then
    return ()
  else
    write_operand ctxt op1 $ Bottom FromSemantics $ S.empty

fstp ctxt op1 =
  if is_st_reg_operand op1 then
    return ()
  else
    write_operand ctxt op1 $ Bottom FromSemantics $ S.empty

fld ctxt op1 = return ()

fld1 ctxt = return ()

fldz ctxt = return ()

fild ctxt = mov_with_func1 ctxt mk_bottom False

fxch ctxt = return ()

fchs ctxt = return ()
  
fucom ctxt = return ()

fucomi ctxt = return ()

fucomip ctxt = return ()

fucomp ctxt = return ()

fucompi ctxt = return ()

fucompp ctxt = return ()

finit ctxt = return ()

fninit ctxt = return ()

fnstcw ctxt = mov_with_func1 ctxt mk_bottom False

fstcw ctxt = mov_with_func1 ctxt mk_bottom False


-- We do not model state changes to ST(_) registers, hence the following semantics
-- instead of:
--    mov_with_func ctxt mk_bottom False (Reg ST0) op1
fadd1 ctxt op1 = return ()

fadd2 ctxt op1 op2 = return ()

fmul1 ctxt op1 = return ()

fmul2 ctxt op1 op2 = return ()

fmulp1 ctxt op1 = return ()

fmulp2 ctxt op1 op2 = return ()

fdivr1 ctxt op1 = return ()

fdivr2 ctxt op1 op2 = return ()

fdivrp1 ctxt op1 = return ()

fdivrp2 ctxt op1 op2 = return ()

fisub ctxt op1 = return ()

fcmovxx ctxt = return ()

fisttp ctxt = mov_with_func1 ctxt mk_bottom False

idiv ctxt = mov_with_func1 ctxt mk_bottom True

div1 :: Context -> Operand -> State Pred ()
div1 ctxt op1 = do
  let srcs = case operand_size op1 of
               8 -> [RDX,RAX]
               4 -> [EDX,EAX]
               2 -> [DX,AX]
               1 -> [AH,AL]
  e1 <- read_operand ctxt op1
  src0 <- read_operand ctxt (Reg $ srcs !! 0)
  src1 <- read_operand ctxt (Reg $ srcs !! 1)
  
  write_operand ctxt (Reg $ srcs !! 0) $ SE_Op (Div_Rem (8 * operand_size op1)) [src0,src1,e1]
  write_operand ctxt (Reg $ srcs !! 1) $ SE_Op (Div (8 * operand_size op1)) [src0,src1,e1]
  write_flags (\_ _ -> None) op1 op1


cdq :: Context -> State Pred ()
cdq ctxt = do
  e1 <- read_operand ctxt (Reg EAX)
  write_operand ctxt (Reg EDX) (mk_bottom [e1])

cqo :: Context -> State Pred ()
cqo ctxt = do
  e1 <- read_operand ctxt (Reg RAX)
  write_operand ctxt (Reg RDX) (mk_bottom [e1])

cdqe :: Context -> State Pred ()
cdqe ctxt = do
  e1 <- read_operand ctxt (Reg EAX)
  write_operand ctxt (Reg RAX) (SE_SExtend 32 64 e1)

cbw ctxt = mov_with_func1 ctxt mk_bottom True (Reg AX)

cwde ctxt = mov_with_func1 ctxt mk_bottom True (Reg EAX)





shl :: Context -> Operand -> Operand -> State Pred ()
shl ctxt op1 op2@(Immediate i) = do
  e1 <- read_operand ctxt op1
  write_operand ctxt op1 (SE_Op (Times (8 * operand_size op1)) [e1,SE_Immediate $ 2^i])
  write_flags (\_ _ -> None) op1 op2
shl ctxt op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 (SE_Op (Shl (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2

shr :: Context -> Operand -> Operand -> State Pred ()
shr ctxt op1 op2@(Immediate i) = do
  e1 <- read_operand ctxt op1
  write_operand ctxt op1 (SE_Op (Udiv (8 * operand_size op1)) [e1,SE_Immediate $ 2^i])
  write_flags (\_ _ -> None) op1 op2
shr ctxt op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 (SE_Op (Shr (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2

sar ctxt op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 (SE_Op (Sar (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2


shld ctxt = mov_with_func3 ctxt mk_bottom False -- TODO

shrd ctxt = mov_with_func3 ctxt mk_bottom False -- TODO


rol :: Context -> Operand -> Operand -> State Pred ()
rol ctxt op1 op2@(Immediate i) = do
  e1 <- read_operand ctxt op1
  write_operand ctxt op1 $ SE_Op (Rol (8 * operand_size op1)) [e1,SE_Immediate $ 2^i]
  write_flags (\_ _ -> None) op1 op2
rol ctxt op1 op2 = mov_with_func ctxt mk_bottom True op1 op2

ror :: Context -> Operand -> Operand -> State Pred ()
ror ctxt op1 op2@(Immediate i) = do
  e1 <- read_operand ctxt op1
  write_operand ctxt op1 $ SE_Op (Ror (8 * operand_size op1)) [e1,SE_Immediate $ 2^i]
  write_flags (\_ _ -> None) op1 op2
ror ctxt op1 op2 = mov_with_func ctxt mk_bottom True op1 op2

{-
adc :: Context -> Operand -> Operand -> State Pred ()
adc ctxt op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 $ SE_Binop "adc" [e1,e2]
  write_flags (\_ _ -> None) op1 op2

sbb :: Context -> Operand -> Operand -> State Pred ()
sbb ctxt op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 $ SE_Binop "sbb" [e1,e2]
  write_flags (\_ _ -> None) op1 op2
-}
adc ctxt = mov_with_func ctxt mk_bottom True

sbb ctxt = mov_with_func ctxt mk_bottom True

mul1 ctxt = mov_with_func1 ctxt mk_bottom True --TODO 

mul2 ctxt = mov_with_func ctxt mk_bottom True --TODO 

imul1 :: Context -> Operand -> State Pred ()
imul1 ctxt op1 = do
  let srcs = case operand_size op1 of
               8 -> [RDX,RAX]
               4 -> [EDX,EAX]
               2 -> [DX,AX]
               1 -> [AH,AL]
  e1 <- read_operand ctxt op1
  src0 <- read_operand ctxt (Reg $ srcs !! 0)
  src1 <- read_operand ctxt (Reg $ srcs !! 1)
  
  write_operand ctxt (Reg $ srcs !! 0) $ mk_bottom [src1,e1] -- high part of multiplication
  write_operand ctxt (Reg $ srcs !! 1) $ SE_Op (Times (8 * operand_size op1)) [src1,e1]
  write_flags (\_ _ -> None) op1 op1

imul2 :: Context -> Operand -> Operand -> State Pred ()
imul2 ctxt op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 (SE_Op (Times (8 * operand_size op1)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2

imul3 :: Context -> Operand -> Operand -> Operand -> State Pred ()
imul3 ctxt op0 op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op0 (SE_Op (Times (8 * operand_size op0)) [e1,e2])
  write_flags (\_ _ -> None) op1 op2



bswap :: Context -> Operand -> State Pred ()
bswap ctxt op1 = do
  e1 <- read_operand ctxt op1
  write_operand ctxt op1 $ SE_Op (Bswap (8 * operand_size op1)) [e1]
  write_flags (\_ _ -> None) op1 op1

pextrb :: Context -> Operand -> Operand -> Operand -> State Pred ()
pextrb ctxt op0 op1 op2 = do
  e0 <- read_operand ctxt op0
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op0 (SE_Op (Pextr 8) [e0,e1,e2])

pextrd :: Context -> Operand -> Operand -> Operand -> State Pred ()
pextrd ctxt op0 op1 op2 = do
  e0 <- read_operand ctxt op0
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op0 (SE_Op (Pextr 32) [e0,e1,e2])

pextrq :: Context -> Operand -> Operand -> Operand -> State Pred ()
pextrq ctxt op0 op1 op2 = do
  e0 <- read_operand ctxt op0
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op0 (SE_Op (Pextr 64) [e0,e1,e2])

haddpd ctxt = mov_with_func ctxt mk_bottom False

pinsrq ctxt = mov_with_func3 ctxt mk_bottom False

pinsrd ctxt = mov_with_func3 ctxt mk_bottom False

pshufb ctxt = mov_with_func ctxt mk_bottom False

pshufd ctxt = mov_with_func ctxt mk_bottom False

pshuflw ctxt = mov_with_func3 ctxt mk_bottom False

pclmulqdq ctxt = mov_with_func3 ctxt mk_bottom False

pcmpeqb ctxt = mov_with_func ctxt mk_bottom False

pcmpeqd ctxt = mov_with_func ctxt mk_bottom False

pcmpgtb ctxt = mov_with_func ctxt mk_bottom False

pcmpgtd ctxt = mov_with_func ctxt mk_bottom False

movmskps ctxt = mov_with_func ctxt mk_bottom False

pmovsxdq ctxt = mov_with_func ctxt mk_bottom False

pmovzxdq ctxt = mov_with_func ctxt mk_bottom False

pmovsxbd ctxt = mov_with_func ctxt mk_bottom False

pmovzxbd ctxt = mov_with_func ctxt mk_bottom False

movmskpd ctxt = mov_with_func ctxt mk_bottom False

unpcklps ctxt = mov_with_func ctxt mk_bottom False

cmpltsd ctxt = mov_with_func ctxt mk_bottom False

cmpeqsd ctxt = mov_with_func ctxt mk_bottom False

cmpneqsd ctxt = mov_with_func ctxt mk_bottom False

punpcklqdq ctxt = mov_with_func ctxt mk_bottom False

punpckldq ctxt = mov_with_func ctxt mk_bottom False

punpcklbw ctxt = mov_with_func ctxt mk_bottom False

blendvps ctxt = mov_with_func3 ctxt mk_bottom False

extractps ctxt = mov_with_func3 ctxt mk_bottom False

movsd_string ctxt prefix op1 op2 = return () -- TODO 

movsq ctxt prefix op1 op2 = return () -- TODO 

x86_in ctxt = mov_with_func1 ctxt mk_bottom False

x86_out ctxt = mov_with_func1 ctxt mk_bottom False

cli ctxt = return ()

clts ctxt = return ()

cpuid ctxt = mov_with_func1 ctxt mk_bottom False (Reg RAX)

invpcid ctxt = return ()

lgdt ctxt = return ()

lidt ctxt = return ()

lldt ctxt = return ()

ltr ctxt = return ()

rdmsr ctxt = do
  mov_with_func1 ctxt mk_bottom False (Reg RAX)
  mov_with_func1 ctxt mk_bottom False (Reg RDX)

wrmsr ctxt = do
  mov_with_func1 ctxt mk_bottom False (Reg RAX)
  mov_with_func1 ctxt mk_bottom False (Reg RDX)

rdtsc ctxt = do
  mov_with_func1 ctxt mk_bottom False (Reg RAX)
  mov_with_func1 ctxt mk_bottom False (Reg RDX)

swapgs ctxt = mov_with_func1 ctxt mk_bottom False (Reg GS)

xsetbv ctxt = return ()

xsaveopt ctxt = return ()

xrstor ctxt = return ()

wrfsbase ctxt = mov_with_func1 ctxt mk_bottom False (Reg FS)

wrgsbase ctxt = mov_with_func1 ctxt mk_bottom False (Reg GS)



xadd :: Context -> Operand -> Operand -> State Pred ()
xadd ctxt op1 op2 = do
  e1 <- read_operand ctxt op1
  e2 <- read_operand ctxt op2
  write_operand ctxt op1 (SE_Op (Plus (operand_size op1)) [e1,e2])
  write_operand ctxt op2 e1
  write_flags (\_ _ -> None) op1 op2

cmpxchg ctxt = mov_with_func ctxt mk_bottom True


tau_i :: Context -> Instr -> State Pred ()
tau_i ctxt (Instr _ _ PUSH     (Just op1) _          _ _ _)   = push   ctxt op1
tau_i ctxt (Instr _ _ POP      (Just op1) _          _ _ _)   = pop    ctxt op1
tau_i ctxt (Instr _ _ LEA      (Just op1) (Just op2) _ _ _)   = lea    ctxt op1 op2

tau_i ctxt i@(Instr _ _ CALL     _          _          _ _ _) = call   ctxt i
tau_i ctxt   (Instr _ _ RET      _          _          _ _ _) = ret    ctxt 
tau_i ctxt   (Instr _ _ IRETQ    _          _          _ _ _) = ret    ctxt 
tau_i ctxt   (Instr _ _ SYSRET   _          _          _ _ _) = sysret ctxt 
tau_i ctxt i@(Instr _ _ JMP      (Just op1) _          _ _ _) = jmp    ctxt i
tau_i ctxt   (Instr _ _ LEAVE    _          _          _ _ _) = leave  ctxt

tau_i ctxt (Instr _ _       MOV      (Just op1) (Just op2) _ _ _) = mov    ctxt op1 op2
tau_i ctxt (Instr _ _       MOVZX    (Just op1) (Just op2) _ _ _) = movzx  ctxt op1 op2
tau_i ctxt (Instr _ _       MOVSX    (Just op1) (Just op2) _ _ _) = movsx  ctxt op1 op2
tau_i ctxt (Instr _ _       MOVSXD   (Just op1) (Just op2) _ _ _) = movsxd ctxt op1 op2
tau_i ctxt (Instr _ _       MOVAPS   (Just op1) (Just op2) _ _ _) = movaps ctxt op1 op2
tau_i ctxt (Instr _ _       MOVAPD   (Just op1) (Just op2) _ _ _) = movapd ctxt op1 op2
tau_i ctxt (Instr _ _       MOVABS   (Just op1) (Just op2) _ _ _) = movabs ctxt op1 op2
tau_i ctxt (Instr _ _       MOVUPD   (Just op1) (Just op2) _ _ _) = movupd ctxt op1 op2
tau_i ctxt (Instr _ _       MOVUPS   (Just op1) (Just op2) _ _ _) = movups ctxt op1 op2
tau_i ctxt (Instr _ _       MOVDQU   (Just op1) (Just op2) _ _ _) = movdqu ctxt op1 op2
tau_i ctxt (Instr _ _       MOVDQA   (Just op1) (Just op2) _ _ _) = movdqa ctxt op1 op2
tau_i ctxt (Instr _ _       MOVD     (Just op1) (Just op2) _ _ _) = movd   ctxt op1 op2
tau_i ctxt (Instr _ _       MOVQ     (Just op1) (Just op2) _ _ _) = movq   ctxt op1 op2
tau_i ctxt (Instr _ _       MOVLPD   (Just op1) (Just op2) _ _ _) = movlpd ctxt op1 op2
tau_i ctxt (Instr _ _       MOVLPS   (Just op1) (Just op2) _ _ _) = movlps ctxt op1 op2
tau_i ctxt (Instr _ Nothing MOVSD    (Just op1) (Just op2) _ _ _) = movsd  ctxt op1 op2
tau_i ctxt (Instr _ Nothing MOVSS    (Just op1) (Just op2) _ _ _) = movss  ctxt op1 op2

tau_i ctxt (Instr _ _ CMOVO    (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVNO   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVS    (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVNS   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVE    (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVZ    (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVNE   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVNZ   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVB    (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVNAE  (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVC    (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVNB   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVAE   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVNC   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVBE   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVNA   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVA    (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVNBE  (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVL    (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVNGE  (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVG    (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVGE   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVNL   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVLE   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVNG   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVNLE  (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVP    (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVPE   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVNP   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2
tau_i ctxt (Instr _ _ CMOVPO   (Just op1) (Just op2) _ _ _) = cmov   ctxt op1 op2

tau_i ctxt (Instr _ _ CDQ      Nothing    _          _ _ _) = cdq    ctxt
tau_i ctxt (Instr _ _ CDQE     Nothing    _          _ _ _) = cdqe   ctxt
tau_i ctxt (Instr _ _ CQO      Nothing    _          _ _ _) = cqo    ctxt
tau_i ctxt (Instr _ _ CBW      Nothing    _          _ _ _) = cbw    ctxt
tau_i ctxt (Instr _ _ CWDE     Nothing    _          _ _ _) = cwde   ctxt

tau_i ctxt (Instr _ _ ADD      (Just op1) (Just op2) _ _ _) = add    ctxt op1 op2
tau_i ctxt (Instr _ _ SUB      (Just op1) (Just op2) _ _ _) = sub    ctxt op1 op2
tau_i ctxt (Instr _ _ NEG      (Just op1) Nothing    _ _ _) = neg    ctxt op1
tau_i ctxt (Instr _ _ INC      (Just op1) _          _ _ _) = inc    ctxt op1
tau_i ctxt (Instr _ _ DEC      (Just op1) _          _ _ _) = dec    ctxt op1
tau_i ctxt (Instr _ _ SHL      (Just op1) (Just op2) _ _ _) = shl    ctxt op1 op2
tau_i ctxt (Instr _ _ SHL      (Just op1) Nothing    _ _ _) = shl    ctxt op1 (Immediate 1)

tau_i ctxt (Instr _ _ NOP      _          _          _ _ _) = nop    ctxt
tau_i ctxt (Instr _ _ UD2      _          _          _ _ _) = ud2    ctxt
tau_i ctxt (Instr _ _ HLT      _          _          _ _ _) = hlt     ctxt

tau_i ctxt (Instr _ _ WAIT     _          _          _ _ _) = wait    ctxt
tau_i ctxt (Instr _ _ MFENCE   _          _          _ _ _) = mfence  ctxt
tau_i ctxt (Instr _ _ CLFLUSH  _          _          _ _ _) = clflush ctxt

tau_i ctxt (Instr _ _ ADC      (Just op1) (Just op2) _ _ _) = adc    ctxt op1 op2
tau_i ctxt (Instr _ _ SBB      (Just op1) (Just op2) _ _ _) = sbb    ctxt op1 op2
tau_i ctxt (Instr _ _ ROL      (Just op1) (Just op2) _ _ _) = rol    ctxt op1 op2
tau_i ctxt (Instr _ _ ROR      (Just op1) (Just op2) _ _ _) = ror    ctxt op1 op2
tau_i ctxt (Instr _ _ SHR      (Just op1) (Just op2) _ _ _) = shr    ctxt op1 op2
tau_i ctxt (Instr _ _ SHR      (Just op1) Nothing    _ _ _) = shr    ctxt op1 (Immediate 1)
tau_i ctxt (Instr _ _ SAR      (Just op1) (Just op2) _ _ _) = sar    ctxt op1 op2
tau_i ctxt (Instr _ _ SAR      (Just op1) Nothing    _ _ _) = sar    ctxt op1 (Immediate 1)
tau_i ctxt (Instr _ _ MUL      (Just op1) Nothing    _ _ _) = mul1  ctxt op1
tau_i ctxt (Instr _ _ MUL      (Just op1) (Just op2) Nothing _ _) = mul2  ctxt op1 op2
tau_i ctxt (Instr _ _ IMUL     (Just op1) Nothing    _ _ _) = imul1  ctxt op1
tau_i ctxt (Instr _ _ IMUL     (Just op1) (Just op2) Nothing _ _)    = imul2  ctxt op1 op2
tau_i ctxt (Instr _ _ IMUL     (Just op1) (Just op2) (Just op3) _ _) = imul3  ctxt op1 op2 op3
tau_i ctxt (Instr _ _ IDIV     (Just op1) Nothing    _ _ _) = idiv   ctxt op1
tau_i ctxt (Instr _ _ DIV      (Just op1) Nothing    _ _ _) = div1   ctxt op1
tau_i ctxt (Instr _ _ SHLD     (Just op1) (Just op2) (Just op3) _ _) = shld ctxt op1 op2 op3
tau_i ctxt (Instr _ _ SHRD     (Just op1) (Just op2) (Just op3) _ _) = shrd ctxt op1 op2 op3

tau_i ctxt (Instr _ _ CMP      (Just op1) (Just op2) _ _ _) = cmp    ctxt op1 op2
tau_i ctxt (Instr _ _ TEST     (Just op1) (Just op2) _ _ _) = test   ctxt op1 op2

tau_i ctxt (Instr _ _ XOR      (Just op1) (Just op2) _ _ _) = xor    ctxt op1 op2
tau_i ctxt (Instr _ _ OR       (Just op1) (Just op2) _ _ _) = or'    ctxt op1 op2
tau_i ctxt (Instr _ _ AND      (Just op1) (Just op2) _ _ _) = and'   ctxt op1 op2
tau_i ctxt (Instr _ _ NOT      (Just op1) _          _ _ _) = not'   ctxt op1
tau_i ctxt (Instr _ _ BT       (Just op1) (Just op2) _ _ _) = bt     ctxt op1 op2
tau_i ctxt (Instr _ _ BTC      (Just op1) (Just op2) _ _ _) = btc    ctxt op1 op2
tau_i ctxt (Instr _ _ BTR      (Just op1) (Just op2) _ _ _) = btr    ctxt op1 op2
tau_i ctxt (Instr _ _ BSR      (Just op1) (Just op2) _ _ _) = bsr    ctxt op1 op2
tau_i ctxt (Instr _ _ BSF      (Just op1) (Just op2) _ _ _) = bsf    ctxt op1 op2
tau_i ctxt (Instr _ _ BTS      (Just op1) (Just op2) _ _ _) = bts    ctxt op1 op2
tau_i ctxt (Instr _ _ BSWAP    (Just op1) _          _ _ _) = bswap  ctxt op1



tau_i ctxt (Instr _ _ SETO     (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETNO    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETS     (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETNS    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETE     (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETZ     (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETNE    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETNZ    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETB     (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETNAE   (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETC     (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETNB    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETAE    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETNC    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETBE    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETNA    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETA     (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETNBE   (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETL     (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETNGE   (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETGE    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETNL    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETLE    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETNG    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETG     (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETNLE   (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETP     (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETPE    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETNP    (Just op1) Nothing    _ _ _) = setxx  ctxt op1
tau_i ctxt (Instr _ _ SETPO    (Just op1) Nothing    _ _ _) = setxx  ctxt op1


tau_i ctxt (Instr _ _ XORPS     (Just op1) (Just op2) _ _ _)            = xorps      ctxt op1 op2
tau_i ctxt (Instr _ _ MOVMSKPS  (Just op1) (Just op2) _ _ _)            = movmskps   ctxt op1 op2
tau_i ctxt (Instr _ _ UNPCKLPS  (Just op1) (Just op2) _ _ _)            = unpcklps   ctxt op1 op2
tau_i ctxt (Instr _ _ BLENDVPS  (Just op1) (Just op2) Nothing    _ _)   = blendvps   ctxt op1 op2 (Reg XMM0)
tau_i ctxt (Instr _ _ BLENDVPS  (Just op1) (Just op2) (Just op3) _ _)   = blendvps   ctxt op1 op2 op3
tau_i ctxt (Instr _ _ EXTRACTPS (Just op1) (Just op2) (Just op3) _ _)   = extractps  ctxt op1 op2 op3

tau_i ctxt (Instr _ _ XORPD    (Just op1) (Just op2) _ _ _) = xorpd    ctxt op1 op2
tau_i ctxt (Instr _ _ ANDPD    (Just op1) (Just op2) _ _ _) = andpd    ctxt op1 op2
tau_i ctxt (Instr _ _ ANDNPD   (Just op1) (Just op2) _ _ _) = andnpd   ctxt op1 op2
tau_i ctxt (Instr _ _ ORPD     (Just op1) (Just op2) _ _ _) = orpd     ctxt op1 op2
tau_i ctxt (Instr _ _ SUBPD    (Just op1) (Just op2) _ _ _) = subpd    ctxt op1 op2
tau_i ctxt (Instr _ _ ADDPD    (Just op1) (Just op2) _ _ _) = addpd    ctxt op1 op2
tau_i ctxt (Instr _ _ HADDPD   (Just op1) (Just op2) _ _ _) = haddpd   ctxt op1 op2
tau_i ctxt (Instr _ _ MOVMSKPD (Just op1) (Just op2) _ _ _) = movmskpd   ctxt op1 op2

tau_i ctxt (Instr _ _ POR      (Just op1) (Just op2) _ _ _)            = por    ctxt op1 op2
tau_i ctxt (Instr _ _ PAND     (Just op1) (Just op2) _ _ _)            = pand   ctxt op1 op2
tau_i ctxt (Instr _ _ PANDN    (Just op1) (Just op2) _ _ _)            = pandn  ctxt op1 op2
tau_i ctxt (Instr _ _ PXOR     (Just op1) (Just op2) _ _ _)            = pxor   ctxt op1 op2
tau_i ctxt (Instr _ _ PTEST    (Just op1) (Just op2) _ _ _)            = ptest  ctxt op1 op2
tau_i ctxt (Instr _ _ PUNPCKLQDQ (Just op1) (Just op2) _ _ _)          = punpcklqdq   ctxt op1 op2
tau_i ctxt (Instr _ _ PUNPCKLBW  (Just op1) (Just op2) _ _ _)          = punpcklbw    ctxt op1 op2
tau_i ctxt (Instr _ _ PUNPCKLDQ  (Just op1) (Just op2) _ _ _)          = punpckldq    ctxt op1 op2
tau_i ctxt (Instr _ _ PMOVSXDQ   (Just op1) (Just op2) _ _ _)          = pmovsxdq     ctxt op1 op2
tau_i ctxt (Instr _ _ PMOVZXDQ   (Just op1) (Just op2) _ _ _)          = pmovzxdq     ctxt op1 op2
tau_i ctxt (Instr _ _ PMOVSXBD   (Just op1) (Just op2) _ _ _)          = pmovsxbd     ctxt op1 op2
tau_i ctxt (Instr _ _ PMOVZXBD   (Just op1) (Just op2) _ _ _)          = pmovzxbd     ctxt op1 op2
tau_i ctxt (Instr _ _ PSHUFB     (Just op1) (Just op2) _ _ _)          = pshufb       ctxt op1 op2
tau_i ctxt (Instr _ _ PSHUFD     (Just op1) (Just op2) _ _ _)          = pshufd       ctxt op1 op2
tau_i ctxt (Instr _ _ PCMPEQB    (Just op1) (Just op2) _ _ _)          = pcmpeqb      ctxt op1 op2
tau_i ctxt (Instr _ _ PCMPEQD    (Just op1) (Just op2) _ _ _)          = pcmpeqd      ctxt op1 op2
tau_i ctxt (Instr _ _ PCMPGTB    (Just op1) (Just op2) _ _ _)          = pcmpgtb      ctxt op1 op2
tau_i ctxt (Instr _ _ PCMPGTD    (Just op1) (Just op2) _ _ _)          = pcmpgtd      ctxt op1 op2
tau_i ctxt (Instr _ _ PADDD      (Just op1) (Just op2) _ _ _)          = paddd        ctxt op1 op2
tau_i ctxt (Instr _ _ PADDB      (Just op1) (Just op2) _ _ _)          = paddb        ctxt op1 op2
tau_i ctxt (Instr _ _ PADDQ      (Just op1) (Just op2) _ _ _)          = paddq        ctxt op1 op2
tau_i ctxt (Instr _ _ PSUBD      (Just op1) (Just op2) _ _ _)          = psubd        ctxt op1 op2
tau_i ctxt (Instr _ _ PSUBB      (Just op1) (Just op2) _ _ _)          = psubb        ctxt op1 op2
tau_i ctxt (Instr _ _ PSUBQ      (Just op1) (Just op2) _ _ _)          = psubq        ctxt op1 op2
tau_i ctxt (Instr _ _ PMULLD     (Just op1) (Just op2) _ _ _)          = pmulld       ctxt op1 op2
tau_i ctxt (Instr _ _ PMINSD     (Just op1) (Just op2) _ _ _)          = pminsd       ctxt op1 op2
tau_i ctxt (Instr _ _ PMINUD     (Just op1) (Just op2) _ _ _)          = pminud       ctxt op1 op2
tau_i ctxt (Instr _ _ PMAXUD     (Just op1) (Just op2) _ _ _)          = pmaxud       ctxt op1 op2
tau_i ctxt (Instr _ _ PMAXUQ     (Just op1) (Just op2) _ _ _)          = pmaxuq       ctxt op1 op2
tau_i ctxt (Instr _ _ PSRLD      (Just op1) (Just op2) _ _ _)          = psrld        ctxt op1 op2
tau_i ctxt (Instr _ _ PSRLW      (Just op1) (Just op2) _ _ _)          = psrlw        ctxt op1 op2
tau_i ctxt (Instr _ _ PSRLDQ     (Just op1) (Just op2) _ _ _)          = psrldq       ctxt op1 op2
tau_i ctxt (Instr _ _ PSLLDQ     (Just op1) (Just op2) _ _ _)          = pslldq       ctxt op1 op2
tau_i ctxt (Instr _ _ PSLLQ      (Just op1) (Just op2) _ _ _)          = psllq        ctxt op1 op2
tau_i ctxt (Instr _ _ PSRLQ      (Just op1) (Just op2) _ _ _)          = psrlq        ctxt op1 op2
tau_i ctxt (Instr _ _ PSUBUSB    (Just op1) (Just op2) _ _ _)          = psubusb      ctxt op1 op2
tau_i ctxt (Instr _ _ PSUBUSW    (Just op1) (Just op2) _ _ _)          = psubusw      ctxt op1 op2
tau_i ctxt (Instr _ _ PINSRQ     (Just op1) (Just op2) (Just op3) _ _) = pinsrq       ctxt op1 op2 op3
tau_i ctxt (Instr _ _ PINSRD     (Just op1) (Just op2) (Just op3) _ _) = pinsrd       ctxt op1 op2 op3
tau_i ctxt (Instr _ _ PEXTRB     (Just op1) (Just op2) (Just op3) _ _) = pextrb       ctxt op1 op2 op3
tau_i ctxt (Instr _ _ PEXTRD     (Just op1) (Just op2) (Just op3) _ _) = pextrd       ctxt op1 op2 op3
tau_i ctxt (Instr _ _ PEXTRQ     (Just op1) (Just op2) (Just op3) _ _) = pextrq       ctxt op1 op2 op3
tau_i ctxt (Instr _ _ PSHUFLW    (Just op1) (Just op2) (Just op3) _ _) = pshuflw      ctxt op1 op2 op3
tau_i ctxt (Instr _ _ PCLMULQDQ  (Just op1) (Just op2) (Just op3) _ _) = pclmulqdq    ctxt op1 op2 op3
tau_i ctxt (Instr _ _ PACKSSDW   (Just op1) (Just op2) _ _ _)          = packssdw     ctxt op1 op2
tau_i ctxt (Instr _ _ PACKSSWB   (Just op1) (Just op2) _ _ _)          = packsswb     ctxt op1 op2


tau_i ctxt (Instr _ _ SUBSS    (Just op1) (Just op2) _ _ _) = subss    ctxt op1 op2
tau_i ctxt (Instr _ _ ADDSS    (Just op1) (Just op2) _ _ _) = addss    ctxt op1 op2
tau_i ctxt (Instr _ _ DIVSS    (Just op1) (Just op2) _ _ _) = divss    ctxt op1 op2
tau_i ctxt (Instr _ _ MULSS    (Just op1) (Just op2) _ _ _) = mulss    ctxt op1 op2
tau_i ctxt (Instr _ _ ROUNDSS  (Just op1) (Just op2) _ _ _) = roundss  ctxt op1 op2
tau_i ctxt (Instr _ _ UCOMISS  (Just op1) (Just op2) _ _ _) = ucomiss  ctxt op1 op2

tau_i ctxt (Instr _ _ SUBSD    (Just op1) (Just op2) _ _ _) = subsd    ctxt op1 op2
tau_i ctxt (Instr _ _ ADDSD    (Just op1) (Just op2) _ _ _) = addsd    ctxt op1 op2
tau_i ctxt (Instr _ _ DIVSD    (Just op1) (Just op2) _ _ _) = divsd    ctxt op1 op2
tau_i ctxt (Instr _ _ MULSD    (Just op1) (Just op2) _ _ _) = mulsd    ctxt op1 op2
tau_i ctxt (Instr _ _ ROUNDSD  (Just op1) (Just op2) _ _ _) = roundsd  ctxt op1 op2
tau_i ctxt (Instr _ _ UCOMISD  (Just op1) (Just op2) _ _ _) = ucomisd  ctxt op1 op2
tau_i ctxt (Instr _ _ CMPLTSD  (Just op1) (Just op2) _ _ _) = cmpltsd  ctxt op1 op2
tau_i ctxt (Instr _ _ CMPEQSD  (Just op1) (Just op2) _ _ _) = cmpeqsd  ctxt op1 op2
tau_i ctxt (Instr _ _ CMPNEQSD (Just op1) (Just op2) _ _ _) = cmpneqsd ctxt op1 op2



tau_i ctxt (Instr _ _ CVTSS2SD  (Just op1) (Just op2) _ _ _) = cvtss2sd  ctxt op1 op2
tau_i ctxt (Instr _ _ CVTSI2SS  (Just op1) (Just op2) _ _ _) = cvtsi2ss  ctxt op1 op2
tau_i ctxt (Instr _ _ CVTSI2SD  (Just op1) (Just op2) _ _ _) = cvtsi2sd  ctxt op1 op2
tau_i ctxt (Instr _ _ CVTSD2SS  (Just op1) (Just op2) _ _ _) = cvtsd2ss  ctxt op1 op2
tau_i ctxt (Instr _ _ CVTTSS2SI (Just op1) (Just op2) _ _ _) = cvttss2si ctxt op1 op2
tau_i ctxt (Instr _ _ CVTTSD2SI (Just op1) (Just op2) _ _ _) = cvttsd2si  ctxt op1 op2
tau_i ctxt (Instr _ _ CVTTPD2DQ (Just op1) (Just op2) _ _ _) = cvttpd2dq ctxt op1 op2
tau_i ctxt (Instr _ _ CVTDQ2PD  (Just op1) (Just op2) _ _ _) = cvtdq2pd  ctxt op1 op2

tau_i ctxt (Instr _ _ FST      (Just op1) _          _ _ _)  = fst'     ctxt op1
tau_i ctxt (Instr _ _ FSTP     (Just op1) _          _ _ _)  = fstp     ctxt op1
tau_i ctxt (Instr _ _ FINIT    _          _          _ _ _)  = finit    ctxt
tau_i ctxt (Instr _ _ FNINIT   _          _          _ _ _)  = fninit   ctxt
tau_i ctxt (Instr _ _ FNSTCW   (Just op1) _          _ _ _)  = fnstcw   ctxt op1
tau_i ctxt (Instr _ _ FSTCW    (Just op1) _          _ _ _)  = fstcw    ctxt op1
tau_i ctxt (Instr _ _ FLD      (Just op1) _          _ _ _)  = fld      ctxt op1
tau_i ctxt (Instr _ _ FLD1     Nothing    _          _ _ _)  = fld1     ctxt
tau_i ctxt (Instr _ _ FLDZ     Nothing    _          _ _ _)  = fldz     ctxt
tau_i ctxt (Instr _ _ FILD     (Just op1) Nothing    _ _ _)  = fild     ctxt op1
tau_i ctxt (Instr _ _ FUCOM    _          _          _ _ _)  = fucom    ctxt
tau_i ctxt (Instr _ _ FUCOMI   _          _          _ _ _)  = fucomi   ctxt
tau_i ctxt (Instr _ _ FUCOMIP  _          _          _ _ _)  = fucomip  ctxt
tau_i ctxt (Instr _ _ FUCOMP   _          _          _ _ _)  = fucomp   ctxt
tau_i ctxt (Instr _ _ FUCOMPI  _          _          _ _ _)  = fucompi  ctxt
tau_i ctxt (Instr _ _ FUCOMPP  _          _          _ _ _)  = fucompp  ctxt
tau_i ctxt (Instr _ _ FCMOVB   _          _          _ _ _)  = fcmovxx  ctxt
tau_i ctxt (Instr _ _ FCMOVBE  _          _          _ _ _)  = fcmovxx  ctxt
tau_i ctxt (Instr _ _ FCMOVE   _          _          _ _ _)  = fcmovxx  ctxt
tau_i ctxt (Instr _ _ FCMOVNE  _          _          _ _ _)  = fcmovxx  ctxt
tau_i ctxt (Instr _ _ FCMOVU   _          _          _ _ _)  = fcmovxx  ctxt
tau_i ctxt (Instr _ _ FCMOVNU  _          _          _ _ _)  = fcmovxx  ctxt
tau_i ctxt (Instr _ _ FCMOVNBE _          _          _ _ _)  = fcmovxx  ctxt
tau_i ctxt (Instr _ _ FADD     (Just op1) Nothing    _ _ _)  = fadd1    ctxt op1
tau_i ctxt (Instr _ _ FADD     (Just op1) (Just op2) _ _ _)  = fadd2    ctxt op1 op2
tau_i ctxt (Instr _ _ FMUL     (Just op1) Nothing    _ _ _)  = fmul1    ctxt op1
tau_i ctxt (Instr _ _ FMUL     (Just op1) (Just op2) _ _ _)  = fmul2    ctxt op1 op2
tau_i ctxt (Instr _ _ FMULP    (Just op1) Nothing    _ _ _)  = fmulp1   ctxt op1
tau_i ctxt (Instr _ _ FMULP    (Just op1) (Just op2) _ _ _)  = fmulp2   ctxt op1 op2
tau_i ctxt (Instr _ _ FDIVR    (Just op1) Nothing    _ _ _)  = fdivr1   ctxt op1
tau_i ctxt (Instr _ _ FDIVR    (Just op1) (Just op2) _ _ _)  = fdivr2   ctxt op1 op2
tau_i ctxt (Instr _ _ FDIVRP   (Just op1) Nothing    _ _ _)  = fdivrp1  ctxt op1
tau_i ctxt (Instr _ _ FDIVRP   (Just op1) (Just op2) _ _ _)  = fdivrp2  ctxt op1 op2
tau_i ctxt (Instr _ _ FISUB    (Just op1) Nothing    _ _ _)  = fisub    ctxt op1
tau_i ctxt (Instr _ _ FISTTP   (Just op1) Nothing    _ _ _)  = fisttp   ctxt op1
tau_i ctxt (Instr _ _ FXCH     (Just op1) _          _ _ _)  = fxch     ctxt
tau_i ctxt (Instr _ _ FCHS     Nothing    _          _ _ _)  = fchs     ctxt


tau_i ctxt (Instr _ _ CPUID    Nothing    _          _ _ _)  = cpuid    ctxt
tau_i ctxt (Instr _ _ RDMSR    Nothing    _          _ _ _)  = rdmsr    ctxt
tau_i ctxt (Instr _ _ WRMSR    Nothing    _          _ _ _)  = wrmsr    ctxt
tau_i ctxt (Instr _ _ RDTSC    Nothing    _          _ _ _)  = rdtsc    ctxt
tau_i ctxt (Instr _ _ IN       (Just op1) _          _ _ _)  = x86_in   ctxt op1
tau_i ctxt (Instr _ _ OUT      (Just op1) _          _ _ _)  = x86_out  ctxt op1
tau_i ctxt (Instr _ _ CLI      Nothing    _          _ _ _)  = cli      ctxt
tau_i ctxt (Instr _ _ CLTS     Nothing    _          _ _ _)  = clts     ctxt
tau_i ctxt (Instr _ _ INVPCID  _          _          _ _ _)  = invpcid  ctxt
tau_i ctxt (Instr _ _ LGDT     _          _          _ _ _)  = lgdt     ctxt
tau_i ctxt (Instr _ _ LIDT     _          _          _ _ _)  = lidt     ctxt
tau_i ctxt (Instr _ _ LLDT     _          _          _ _ _)  = lldt     ctxt
tau_i ctxt (Instr _ _ LTR      _          _          _ _ _)  = ltr      ctxt
tau_i ctxt (Instr _ _ SWAPGS   _          _          _ _ _)  = swapgs   ctxt
tau_i ctxt (Instr _ _ XSETBV   _          _          _ _ _)  = xsetbv   ctxt
tau_i ctxt (Instr _ _ XSAVEOPT _          _          _ _ _)  = xsaveopt ctxt
tau_i ctxt (Instr _ _ XRSTOR   _          _          _ _ _)  = xrstor   ctxt
tau_i ctxt (Instr _ _ WRFSBASE _          _          _ _ _)  = wrfsbase ctxt
tau_i ctxt (Instr _ _ WRGSBASE _          _          _ _ _)  = wrgsbase ctxt



tau_i ctxt (Instr _ Nothing XCHG (Just op1) (Just op2) _ _ _)        = xchg         ctxt op1 op2

tau_i ctxt (Instr _ (Just LOCK) XADD    (Just op1) (Just op2) _ _ _) = xadd         ctxt op1 op2
tau_i ctxt (Instr _ (Just LOCK) CMPXCHG (Just op1) (Just op2) _ _ _) = cmpxchg      ctxt op1 op2

tau_i ctxt (Instr _ (Just pre)  MOVSD   (Just op1) (Just op2) _ _ _) = movsd_string ctxt pre op1 op2
tau_i ctxt (Instr _ (Just pre)  MOVSQ   (Just op1) (Just op2) _ _ _) = movsq        ctxt pre op1 op2

tau_i ctxt i =
  if is_jump (i_opcode i) || is_cond_jump (i_opcode i) then
    return ()
  else
    error $ "Unsupported instruction: " ++ show i

-- | Do predicate transformation over a block of instructions.
-- Does not take into account flags, commonly function @`tau_blockID`@ should be used.
tau_b :: Context -> [Instr] -> State Pred ()
tau_b ctxt []  = return ()
tau_b ctxt (i:is) = do
  write_rreg RIP (SE_Immediate $ fromIntegral $ i_addr i + i_size i)
  tau_i ctxt i
  tau_b ctxt is



-- TODO JE, other JMP aboves and JUMP lesses
add_jump_to_pred :: Instr -> Instr -> FlagStatus -> FlagStatus
add_jump_to_pred i0@(Instr _ _ JA (Just (Immediate trgt)) _ _ _ _) i1 flg =
  case flg of
    FS_CMP b o1 o2 -> if i_addr i1 == fromIntegral trgt then FS_CMP (Just False) o1 o2 else FS_CMP (Just True) o1 o2
    _ -> flg
add_jump_to_pred i0 i1 flg = flg


-- | Do predicate transformation over a basic block in a CFG.
-- Given an edge in the CFG from blockId to blockId', perform predicate transformation.
-- Parameter blockId' is needed to set the flags properly. If @Nothing@ is supplied, the flags are overapproximatively set to @`None`@.
-- We provide the option to skip the last instruction, as sometimes we need to symbolically execute a block up to but excluding the last instruction.
tau_blockID ::
  Context       -- ^ The context
  -> CFG        -- ^ The CFG
  -> Int        -- ^ The block ID of the basic block
  -> Maybe Int  -- ^ Optionally, the block ID of the next block if symbolically executing an edge in a CFG.
  -> Bool       -- ^ Must the last instruction of the block be ignored?
  -> Pred       -- ^ The predicate to be transformed
  -> Pred
tau_blockID ctxt g blockId blockId' remove_last_instruction p@(Predicate eqs flg vcs muddle_status) = 
  let insts0                            = fetch_block g blockId
      insts                             = if remove_last_instruction then init insts0 else insts0 in
    if insts == [] then
      p
    else let
      addr                              = i_addr $ head insts
      eqs'                              = write_rip addr eqs
      p''                               = execState (tau_b ctxt insts) $ Predicate eqs' flg vcs muddle_status
      Predicate eqs'' flgs'' vcs'' im'' = p'' in
    case blockId' of
      Nothing -> p''
      Just b' ->
        let insts' = fetch_block g b'
            addr'  = i_addr $ head insts' in
          Predicate (write_rip addr' eqs'')
                    (add_jump_to_pred (last insts) (head insts') flgs'')
                    vcs''
                    im''
 where
  write_rip addr eqs = M.insert (SP_Reg RIP) (SE_Immediate $ fromIntegral addr) eqs

-- | Tries to guess if the symbolic expression is a pointer.
-- Any immediate that falls in the address range of the binary is highly likely a pointer.
-- A value stored in a memory location belonging to an external function symbol is also highly likely a pointer.
expr_highly_likely_pointer ctxt (SE_Immediate a)                     = read_from_datasection ctxt a 1 /= Nothing
expr_highly_likely_pointer ctxt (SE_Var (SP_Mem (SE_Immediate a) 8)) = IM.lookup (fromIntegral a) (ctxt_syms ctxt) /= Nothing
expr_highly_likely_pointer ctxt _                                    = False


join_expr ctxt e0 e1 = 
  if expr_highly_likely_pointer ctxt e0 && expr_highly_likely_pointer ctxt e1 then
    Bottom (FromNonDeterminism $ S.fromList [e0,e1]) $ srcs_of_exprs e0 e1
  else case (e0,e1) of
    (SE_Op (Plus b) [e, SE_Immediate i0], SE_Op (Plus _) [e', SE_Immediate i1]) -> 
      if e == e' then SE_Op (Plus b) [e, Bottom FromAbstraction S.empty] else bot
    (e, SE_Op (Plus b) [e', SE_Immediate i1]) -> 
      if e == e' then SE_Op (Plus b) [e, Bottom FromAbstraction S.empty] else bot
    (SE_Op (Plus b) [e, SE_Immediate i0], e') -> 
      if e == e' then SE_Op (Plus b) [e, Bottom FromAbstraction S.empty] else bot
    (SE_Immediate i0, SE_Immediate i1) -> 
      if abs (i0 - i1) < 32 then SE_Op (Plus 64) [SE_Immediate $ min i0 i1, Bottom FromAbstraction S.empty] else bot
    _ -> bot
 where
  bot = Bottom FromAbstraction $ srcs_of_exprs e0 e1




-- the join of two predicates
-- 1.) keep any key-value pair (sp,v) that is in both predicates
-- 2.) for any remaining key-value pair (sp,v) in either of the predicates, add (sp,Bottom)
-- 3.) the flag expression is kept based on strict equality
--
--
-- Assumes any statepart not currently in the state is unwritten to.
join_muddle_status Muddled _      = Muddled
join_muddle_status _ Muddled      = Muddled
join_muddle_status ExternalOnly _ = ExternalOnly
join_muddle_status _ ExternalOnly = ExternalOnly
join_muddle_status _ _            = Clean

join_preds ctxt (Predicate eqs0 flg0 vcs0 muddle_status0) (Predicate eqs1 flg1 vcs1 muddle_status1) =
  let m    = map mk_entry $ M.toList eqs0
      m'   = mapMaybe mk_entry' $ M.toList eqs1
      eqs' = M.fromList $ m ++ m'
      flg' = if flg0 == flg1 then flg0 else None
      vcs' = S.union vcs0 vcs1 
      im'  = join_muddle_status muddle_status0 muddle_status1 in
    Predicate eqs' flg' vcs' im'
 where
  mk_entry (sp,v) =
    case find (\(sp',v') -> necessarily_equal_stateparts sp sp') $ M.toList eqs1 of
      Nothing -> if is_initial sp v then (sp, v) else (sp, Bottom FromAbstraction $ srcs_of_expr v)
      Just (sp',v') -> 
        if necessarily_equal v v' || v == v' then
          (sp,v)
        else
          (sp,join_expr ctxt v v')
  mk_entry' (sp',v' ) =
    case find (\(sp,v) -> necessarily_equal_stateparts sp sp') $ M.toList eqs0 of
      Nothing -> if is_initial sp' v' then Just (sp', v') else Just (sp', Bottom FromAbstraction $ srcs_of_expr v')
      Just _  -> Nothing


is_initial :: StatePart -> SimpleExpr -> Bool
is_initial sp v = v == SE_Var sp

-- TODO in case of bot, check whether sources of P1 are larger than P0
implies_preds ctxt (Predicate eqs0 flg0 vcs0 muddle_status0) (Predicate eqs1 flg1 vcs1 muddle_status1) = 
 muddle_status0 == muddle_status1 && flg1 `elem` [None,flg0] && (all implied_by_eqs0 $ M.toList eqs1) && (all_sps_in_eqs1 $ M.keys eqs0)
 where
  implied_by_eqs0 (SP_Reg RIP, _)  = True -- technicality 
  implied_by_eqs0 (sp1,Bottom _ _) = True
  implied_by_eqs0 (sp1,v1) =
    if contains_bot v1 then
      True
    else case find (\(sp0,v0) -> necessarily_equal_stateparts sp0 sp1) $ M.toList eqs0 of
      Nothing -> False
      Just (sp0,v0) -> necessarily_equal v0 v1  || join_expr ctxt v0 v1 == v1


  all_sps_in_eqs1 sps0 = all (\sp0 -> contains_bot_sp sp0 || (find (\sp1 -> necessarily_equal_stateparts sp0 sp1) $ M.keys eqs1) /= Nothing) sps0


-- | The initial predicate.
-- Given the currently known invariants and postconditions, gather all stateparts occurring in the current function.
-- The initial predicate assign a variable to each statepart, e.g.: @RSP == RSP0, RDI == RDI, ...@.
init_pred :: 
  Invariants               -- The currently available invariants
  -> S.Set (NodeInfo,Pred) -- The currently known postconditions
  -> Pred
init_pred curr_invs curr_posts = 
  let sps = S.delete (SP_Reg RIP) $ S.insert (SP_Mem (SE_Var (SP_Reg RSP)) 8) $ gather_stateparts curr_invs curr_posts
      eqs = M.fromList (map (\sp -> (sp,SE_Var sp)) $ S.toList sps) in
    Predicate eqs None S.empty Clean



get_stateparts_of_preds ps = S.unions $ map get_stateparts_of_pred $ S.toList $ ps

get_stateparts_of_pred (Predicate eqs _ _ _) = S.filter (not . contains_bot_sp) $ M.keysSet eqs



-- | Given the currently known invariants and postconditions, gather all stateparts occurring in the current function.
gather_stateparts ::
  Invariants               -- ^ The currently available invariants
  -> S.Set (NodeInfo,Pred) -- ^ The currently known postconditions
  -> S.Set StatePart
gather_stateparts invs posts = S.union (IM.foldrWithKey accumulate_stateparts S.empty invs) (get_stateparts_of_preds (S.map snd posts))
 where
  accumulate_stateparts a p sps = S.union sps (get_stateparts_of_pred p)



instance Propagator Context Pred where
  tau     = \ctxt g blockId blockId' -> tau_blockID ctxt g blockId blockId' False
  join    = join_preds 
  implies = implies_preds
