{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}


-------------------------------------------------------------------------
-- | A datatype for symbolic predicates, tailored to storing information
-- on equalities between the current values stored in state parts (lhs) 
-- and constant expressions (rhs).
-------------------------------------------------------------------------


module SimplePred ( 
  Pred (..),
  StatePart (..),
  SimpleExpr (..),
  FlagStatus (..),
  VerificationCondition(..),
  StateMuddleStatus (..),
  BotTyp (..),
  BotSrc (..),
  Operator (..),
  AddrType (..),
  regs_of,
  srcs_of_expr,
  srcs_of_exprs,
  contains_bot,
  contains_bot_sp,
  add_assertion,
  add_precondition,
  add_function_constraint,
  is_precondition,
  is_assertion,
  is_func_constraint,
  all_bot_satisfy,
  simp,
  trim_expr,
  count_instructions_with_assertions
 )
 where

import Base
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import X86_Datastructures
import Data.Word (Word64,Word32)
import Data.Traversable (for)
import Data.List
import Data.Maybe (mapMaybe,fromJust)
import Debug.Trace
import GHC.Generics
import Data.Bits (testBit, (.|.), (.&.))
import qualified Data.Serialize as Cereal hiding (get,put)

-- | Bot represents an unknown (bottom) value.
-- We annotate each occurence of Bot with a BotTyp.
-- This type indicates where the bottom value originates from.
-- We don't use this, i.e., all bottoms are created equal.
-- The only bottom that has semantics meaning is the last one, non-determinstically picking a value from a set.
data BotTyp = 
    FromAbstraction                         -- ^ Abstraction joined two terms to Bottom
  | FromOverlap                             -- ^ A read from two possibly overlapping regions
  | FromMemWrite                            -- ^ A write to two possibly overlapping regions 
  | FromCall                                -- ^ A function call was executed, setting stateparts to bottom
  | FromSemantics                           -- ^ An instruction with unknown semantics
  | FromUninitializedMemory                 -- ^ Reading from memory not written to yet
  | FromBitMode                             -- ^ Should not happen, but if a register writes to a registeralias with unknown bit size
  | FromNonDeterminism (S.Set SimpleExpr)   -- ^ The expression evaluates to one of the expressions in the set
 deriving (Eq, Ord, Generic)


-- | Bot represents an unknown (bottom) value.
-- We annotate each occurence of Bot with a BotSrc.
-- This indicates the sources that are used to compute the bottom value.
-- For example, the symbolic expressions @RDI0@ and @RSI0@ are joined to a Bottom value with sources @{RDI0,RSI0}@.
-- We don't use this, i.e., all bottoms are created equal.
-- It is, however, very convenient for debugging.
data BotSrc = Src_SP StatePart | Src_Function String
 deriving (Eq, Ord, Generic)


-- | A symbolic expression that is used as a pointer, can be matched to one (or more) of the following types.
-- This happens in over-approximative fashion, i.e., if we don't know we just guess all of these.
-- However, an expression such as @RSP0 - 16@ is guessed to be @Local@, an immediate address within the data sections is guessed to be @Global@.
-- The @Local@ and @Global@ address spaces are assumed to be separate.
data AddrType = Heap | Global | Local
 deriving (Eq, Ord, Generic)

-- | An operator is a pure operation over bit-vectors, annotated with the bit-size of its operands.
-- For example, @Plus 64@ denotes 64-bit addition.
-- @Udiv@ and @Times@ are operators op type @w -> w -> w@ with all words same length.
-- @Div@ and @Div_Rem@ are operators of type @w -> w -> w -> w@ performing concatenation of the first two words and then doing division/remainder.
data Operator = 
    Plus Int | Minus Int | Times Int | And Int | Or Int | Xor Int | Not Int | SetXX | Bsr Int 
  | Div_Rem Int | Div Int | Shl Int | Shr Int | Sar Int | Udiv Int | Ror Int | Rol Int
  | Bswap Int | Pextr Int
 deriving (Eq, Ord, Generic)


instance Show Operator where
  show (Plus    b) = "+"       ++ show b
  show (Minus   b) = "-"       ++ show b
  show (Times   b) = "*"       ++ show b
  show (And     b) = "&"       ++ show b
  show (Or      b) = "|"       ++ show b
  show (Xor     b) = "xor"     ++ show b
  show (Not     b) = "not"     ++ show b
  show (Bsr     b) = "bsr"     ++ show b
  show (Div_Rem b) = "div_rem" ++ show b
  show (Div     b) = "div"     ++ show b
  show (Shl     b) = "shl"     ++ show b
  show (Shr     b) = "shr"     ++ show b
  show (Sar     b) = "sar"     ++ show b
  show (Udiv    b) = "udiv"    ++ show b
  show (Ror     b) = "ror"     ++ show b
  show (Rol     b) = "rol"     ++ show b
  show (Bswap   b) = "bswap"   ++ show b
  show (Pextr   b) = "pextr"   ++ show b

-- | A symbolic expression with as leafs either immediates, variables, or stateparts.
-- A variable is a constant represneting some initial value, e.g., RDI0.
-- A statepart evaluates to its current value, e.g., RDI.
data SimpleExpr =
    Bottom       BotTyp (S.Set BotSrc)      -- ^ Bottom (unknown value)
  | SE_Var       StatePart                  -- ^ A variable representing the initial value stored in the statepart (e.g., RSP0)
  | SE_Immediate Word64                     -- ^ An immediate word
  | SE_StatePart StatePart                  -- ^ The value stored currently in the statepart
  | SE_Op        Operator [SimpleExpr]      -- ^ Application of an @`Operator`@ to the list of arguments
  | SE_Bit       Int SimpleExpr             -- ^ Taking the lower bits of a value
  | SE_SExtend   Int Int SimpleExpr         -- ^ Sign extension
  | SE_Overwrite Int SimpleExpr SimpleExpr  -- ^ Overwriting certain bits of a value with bits from another value
 deriving (Eq, Ord, Generic)

-- | A statepart is either a register or a region in memory
data StatePart =
    SP_Reg Register          -- ^ A register
  | SP_Mem SimpleExpr Int    -- ^ A region with a symbolic address and an immediate size.
 deriving (Eq, Ord, Generic)

instance Show BotTyp where
 show FromAbstraction         = "a"
 show FromBitMode             = "b"
 show FromOverlap             = "o"
 show FromCall                = "c"
 show FromSemantics           = "s"
 show FromUninitializedMemory = "m"
 show FromMemWrite            = "w"
 show (FromNonDeterminism es) = "nd|" ++ intercalate "," (map show $ S.toList es) ++ "|"

instance Show AddrType where
 show Heap   = "h"
 show Global = "g"
 show Local  = "l"

instance Show BotSrc where
  show (Src_SP sp) = show sp
  show (Src_Function f) = f

instance Show SimpleExpr where
  show (Bottom typ srcs)         = "Bot[" ++ show typ ++ ";" ++ intercalate "," (map show $ S.toList srcs) ++ "]"
  show (SE_Var sp)               = show sp ++ "_0"
  show (SE_Immediate i)          = if i > 2000 then "0x" ++ showHex i else show i
  show (SE_StatePart sp)         = show sp
  show (SE_Op (Plus  _) [e0,e1]) = "(" ++ show e0 ++ " + "   ++ show e1 ++ ")"
  show (SE_Op (Minus _) [e0,e1]) = "(" ++ show e0 ++ " - "   ++ show e1 ++ ")"
  show (SE_Op (Times _) [e0,e1]) = "(" ++ show e0 ++ " * "   ++ show e1 ++ ")"
  show (SE_Op (And   _) [e0,e1]) = "(" ++ show e0 ++ " & "   ++ show e1 ++ ")"
  show (SE_Op (Or    _) [e0,e1]) = "(" ++ show e0 ++ " | "   ++ show e1 ++ ")"
  show (SE_Op (Xor   _) [e0,e1]) = "(" ++ show e0 ++ " xor " ++ show e1 ++ ")"
  show (SE_Op op es)             = show op ++ "(" ++ intercalate "," (map show es) ++ ")"
  show (SE_Bit i a)              = "b" ++ show i ++ "(" ++ show a ++ ")"
  show (SE_SExtend l h a)        = "signextend(" ++ show l ++ "," ++ show h ++ ", " ++ show a ++ ")"
  show (SE_Overwrite i a b)      = "overwrite(" ++ show i ++ "," ++ show a ++ "," ++ show b ++ ")"






expr_size (Bottom typ srcs)    = 1 + (sum $ S.map expr_size_src srcs)
expr_size (SE_Var sp)          = expr_size_sp sp
expr_size (SE_Immediate _)     = 1
expr_size (SE_StatePart sp)    = expr_size_sp sp
expr_size (SE_Op _ es)         = 1 + (sum $ map expr_size es)
expr_size (SE_Bit i e)         = 1 + expr_size e
expr_size (SE_SExtend l h e)   = 1 + expr_size e
expr_size (SE_Overwrite _ _ e) = 1 + expr_size e

expr_size_sp (SP_Reg r)     = 1
expr_size_sp (SP_Mem a si)  = 1 + expr_size a 

expr_size_src (Src_SP sp)      = expr_size_sp sp
expr_size_src (Src_Function f) = 1





-- | Returns true iff the expression contains Bot
contains_bot (Bottom typ _)       = True
contains_bot (SE_Var sp)          = contains_bot_sp sp
contains_bot (SE_Immediate _)     = False
contains_bot (SE_StatePart sp)    = contains_bot_sp sp
contains_bot (SE_Op _ es)         = any contains_bot es
contains_bot (SE_Bit i e)         = contains_bot e
contains_bot (SE_SExtend _ _ e)   = contains_bot e
contains_bot (SE_Overwrite _ a b) = contains_bot a || contains_bot b

-- | Returns true iff the statepart contains Bot
contains_bot_sp (SP_Reg r)     = False
contains_bot_sp (SP_Mem a si)  = contains_bot a



-- | Returns the set of registers occuring in the symbolic expression.
regs_of (Bottom typ _)       = S.empty
regs_of (SE_Var sp)          = regs_of_sp sp
regs_of (SE_Immediate _)     = S.empty
regs_of (SE_StatePart sp)    = regs_of_sp sp
regs_of (SE_Op _ es)         = S.unions $ map regs_of es
regs_of (SE_Bit i e)         = regs_of e
regs_of (SE_SExtend _ _ e)   = regs_of e
regs_of (SE_Overwrite _ a b) = S.union (regs_of a) (regs_of b)

regs_of_sp (SP_Reg r)     = S.singleton r
regs_of_sp (SP_Mem a si)  = regs_of a 




is_imm_expr (Bottom _ _)             = False
is_imm_expr (SE_Var sp)              = False
is_imm_expr (SE_Immediate _)         = True
is_imm_expr (SE_StatePart sp)        = False
is_imm_expr (SE_Op _ es)             = all is_imm_expr es
is_imm_expr (SE_Bit i e)             = is_imm_expr e
is_imm_expr (SE_SExtend _ _ e)       = is_imm_expr e
is_imm_expr (SE_Overwrite _ e0 e1)   = is_imm_expr e0 && is_imm_expr e1

-- | Do all occurences of Bottom satisfy the given predicate?
all_bot_satisfy p (Bottom typ srcs)    = p typ srcs
all_bot_satisfy p (SE_Var sp)          = all_bot_satisfy_sp p sp
all_bot_satisfy p (SE_Immediate _)     = True
all_bot_satisfy p (SE_StatePart sp)    = all_bot_satisfy_sp p sp
all_bot_satisfy p (SE_Op _ es)         = all (all_bot_satisfy p) es
all_bot_satisfy p (SE_Bit i e)         = all_bot_satisfy p e
all_bot_satisfy p (SE_SExtend _ _ e)   = all_bot_satisfy p e
all_bot_satisfy p (SE_Overwrite _ a b) = all_bot_satisfy p a && all_bot_satisfy p b

all_bot_satisfy_sp p (SP_Reg r)      = True
all_bot_satisfy_sp p (SP_Mem a si)   = all_bot_satisfy p a


-- | Returns the set of sources (state parts used to compute the expression) of an expression.
srcs_of_expr (Bottom _ srcs)      = srcs
srcs_of_expr (SE_Var sp)          = srcs_of_sp sp
srcs_of_expr (SE_Immediate i)     = S.empty
srcs_of_expr (SE_StatePart sp)    = srcs_of_sp sp
srcs_of_expr (SE_Op _ es)         = S.unions $ map srcs_of_expr es
srcs_of_expr (SE_Bit i e)         = srcs_of_expr e
srcs_of_expr (SE_SExtend _ _ e)   = srcs_of_expr e
srcs_of_expr (SE_Overwrite _ a b) = srcs_of_exprs a b

-- | Returns the set of sources (state parts used to compute the expression) of a statepart.
srcs_of_sp sp@(SP_Reg r) = S.singleton $ Src_SP sp
srcs_of_sp sp@(SP_Mem a si) = S.singleton  $ Src_SP sp -- S.insert sp $ srcs_of_expr a

-- | Returns the set of sources (state parts used to compute the expression) of two expressions.
srcs_of_exprs e0 e1 =
  let srcs = S.union (srcs_of_expr e0) (srcs_of_expr e1) in
    if sum (S.map expr_size_src srcs) > 20 then srcs_of_expr e0 else srcs --TODO

srcs_of_expr_bounded e =
  let srcs = srcs_of_expr e in
    if sum (S.map expr_size_src srcs) > 20 then S.fromList (take 10 $ S.toList srcs)  else srcs --TODO

-- | If the size of an expression becomes too large, we simply turn it into Bottom.
trim_expr e =
  if expr_size e > 1000 then 
    Bottom FromAbstraction $ srcs_of_expr_bounded e
  else
    e




sextend_32_64 w = if testBit w 31 then w .|. 0xFFFFFFFF00000000 else w
sextend_16_64 w = if testBit w 15 then w .|. 0xFFFFFFFFFFFF0000 else w
sextend_8_64  w = if testBit w 7  then w .|. 0xFFFFFFFFFFFFFF00 else w


-- | Simplification of symbolic expressions. 
--
-- Must always produce an expression logically equivalent to the original.
simp :: SimpleExpr -> SimpleExpr
simp (SE_Bit i (SE_Bit i' e))   = SE_Bit (min i i') $ simp e
simp (SE_Bit i (SE_Overwrite i' e0 e1)) = if i <= i' then SE_Bit i (simp e1) else SE_Bit i $ SE_Overwrite i' (simp e0) (simp e1)

simp (SE_Overwrite i (SE_Overwrite i' e0 e1) e2) = if i >= i' then SE_Overwrite i (simp e0) (simp e2) else SE_Overwrite i (SE_Overwrite i' (simp e0) (simp e1)) (simp e2)
-- simp (SE_Overwrite i (SE_Bit i' e0) e1) = if i' <= i then SE_Bit i (simp e1) else SE_Overwrite i (SE_Bit i' $ simp e0) (simp e1) TODO
simp (SE_SExtend l h (SE_Bit i e))  = if i == l then SE_SExtend l h (simp e) else SE_SExtend l h (SE_Bit i $ simp e)

simp (SE_Op (Minus b0) [SE_Op (Minus b1) [a0,a1], a2]) = simp $ SE_Op (Minus b0) [a0, SE_Op (Plus b0)  [a1, a2]] -- (a0-a1)-a2 ==> a0 - (a1 + a2)
simp (SE_Op (Plus b0)  [SE_Op (Minus b1) [a0,a1], a2]) = simp $ SE_Op (Minus b0) [a0, SE_Op (Minus b0) [a1, a2]] -- (a0-a1)+a2 ==> a0 - (a1 - a2)
simp (SE_Op (Minus b0) [SE_Op (Plus b1)  [a0,a1], a2]) = simp $ SE_Op (Plus b0)  [a0, SE_Op (Minus b0) [a1, a2]] -- (a0+a1)-a2 ==> a0 + (a1 - a2)
simp (SE_Op (Plus b0)  [SE_Op (Plus b1)  [a0,a1], a2]) = simp $ SE_Op (Plus b0)  [a0, SE_Op (Plus b0)  [a1, a2]] -- (a0+a1)+a2 ==> a0 + (a1 + a2)

simp (SE_Op (Plus b0)  [SE_Immediate 0, e1]) = simp e1 -- 0 + e1 = e1
simp (SE_Op (Plus b0)  [e0, SE_Immediate 0]) = simp e0 -- e0 + 0 = e0
simp (SE_Op (Minus b0) [e0, SE_Immediate 0]) = simp e0 -- e0 - 0 = e0

simp (SE_Overwrite i (SE_Immediate 0) e) = simp e

simp (SE_Op (Minus b0) [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 - i1) -- Immediate: i0 - i1
simp (SE_Op (Plus  b0) [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 + i1) -- Immediate: i0 + i1
simp (SE_Op (Times b0) [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 * i1) -- Immediate: i0 * i1


simp (SE_Bit 32 (SE_Immediate i)) = SE_Immediate (i .&. 0x00000000FFFFFFFF)
simp (SE_Bit 16 (SE_Immediate i)) = SE_Immediate (i .&. 0x000000000000FFFF)
simp (SE_Bit 8  (SE_Immediate i)) = SE_Immediate (i .&. 0x00000000000000FF)

simp (SE_SExtend 32 64 (SE_Immediate i)) = SE_Immediate (sextend_32_64 i)
simp (SE_SExtend 16 64 (SE_Immediate i)) = SE_Immediate (sextend_16_64 i)
simp (SE_SExtend 8  64 (SE_Immediate i)) = SE_Immediate (sextend_8_64 i)

simp (SE_Op (Minus b0) [SE_Immediate i0, SE_Op (Minus b1) [e1, SE_Immediate i1]]) = simp $ SE_Op (Minus b0) [SE_Immediate (i0+i1), e1] -- i0-(e1-i1) ==> (i0+i1)-e1
simp (SE_Op (Minus b0) [SE_Immediate i0, SE_Op (Plus b1)  [e1, SE_Immediate i1]]) = simp $ SE_Op (Minus b0) [SE_Immediate (i0-i1), e1] -- i0-(e1+i1) ==> (i0-i1)-e1
simp (SE_Op (Plus b0)  [SE_Immediate i0, SE_Op (Minus b1) [e1, SE_Immediate i1]]) = simp $ SE_Op (Plus b0)  [e1, SE_Immediate (i0-i1)] -- i0+(e1-i1) ==> e1+(i0-i1)

simp (SE_Bit i (SE_Op (Plus  b0) [e0, e1])) = simp $ SE_Op (Plus i)  [SE_Bit i e0, SE_Bit i e1] -- b(e0+e1) = b(e0) + b(e1)
simp (SE_Bit i (SE_Op (Minus b0) [e0, e1])) = simp $ SE_Op (Minus i) [SE_Bit i e0, SE_Bit i e1] -- b(e0-e1) = b(e0) - b(e1)
simp (SE_Bit i (SE_Op (Times b0) [e0, e1])) = simp $ SE_Op (Times i) [SE_Bit i e0, SE_Bit i e1] -- b(e0*e1) = b(e0) * b(e1)
simp (SE_Bit i (SE_Op (And   b0) [e0, e1])) = simp $ SE_Op (And i)   [SE_Bit i e0, SE_Bit i e1] -- b(e0&e1) = b(e0) & b(e1)
simp (SE_Bit i (SE_Op (Or    b0) [e0, e1])) = simp $ SE_Op (Or i)    [SE_Bit i e0, SE_Bit i e1] -- b(e0|e1) = b(e0) | b(e1)
simp (SE_Bit i (SE_Op (Xor   b0) [e0, e1])) = simp $ SE_Op (Xor i)   [SE_Bit i e0, SE_Bit i e1] -- b(e0 xor e1) = b(e0) xor b(e1)
simp (SE_Bit i (SE_Op (Not   b0) [e0]))     = simp $ SE_Op (Not i)   [SE_Bit i e0]              -- b(not e0) = b(not e0)


simp (SE_Op op es)          = SE_Op op $ map simp es
simp (SE_StatePart sp)      = SE_StatePart $ simp_sp sp
simp (SE_Bit i e)           = SE_Bit i $ simp e
simp (SE_SExtend l h e)     = SE_SExtend l h $ simp e
simp (SE_Overwrite i e0 e1) = SE_Overwrite i (simp e0) (simp e1)
simp e                      = e

simp_sp (SP_Mem e si)    = SP_Mem (simp e) si
simp_sp sp               = sp




instance Show StatePart where
  show (SP_Reg r) = show r
  show (SP_Mem a si) = "[" ++ show a ++ ", " ++ show si ++ "]"

-- | Symbolically represent the status of all flags in the current state
data FlagStatus = 
    None                                 -- ^ No information known, flags could have any value
  | FS_CMP (Maybe Bool) Operand Operand  -- ^ The flags are set by the x86 CMP instruction applied to the given operands.
  deriving (Generic,Eq,Ord)


-- |  A verification condition is either:
-- * A precondition of the form:
-- >    Precondition (a0,si0) (a1,si1)
-- This formulates that at the initial state the two regions must be separate.
-- * An assertion of the form:
-- >    Assertion a (a0,si0) (a1,si1)
-- This formulates that dynamically, whenever address a is executed, the two regions are asserted to be separate.
-- * A function constraint of the form:
-- >    FunctionConstraint foo [(RDI, v0), (RSI, v1), ...]
-- This formulates that a function call to function foo with values v0, v1, ... stored in the registers should not overwrite any part of the local stack frame of the caller.
data VerificationCondition =
    Precondition       SimpleExpr Int SimpleExpr Int            -- ^ Precondition:           lhs SEP rhs
  | Assertion          SimpleExpr SimpleExpr Int SimpleExpr Int -- ^ Assertion:    @address, lhs SEP rhs
  | FunctionConstraint String     [(Register,SimpleExpr)]       -- ^ Function name, with param registers
  deriving (Generic,Eq,Ord)

instance Show VerificationCondition where
  show (Precondition lhs _ rhs _) = show lhs ++ " SEP " ++ show rhs
  show (Assertion a  lhs _ rhs _) = "@" ++ show a ++ ": " ++ show lhs ++ " SEP " ++ show rhs
  show (FunctionConstraint f ps)  = f ++ "(" ++ intercalate "," (map show_param ps) ++ ")"
   where
    show_param (r,e) = show r ++ ":=" ++ strip_parentheses (show e)

-- | Is the given verification condition an assertion?
is_assertion (Assertion _ _ _ _ _) = True
is_assertion _                 = False

-- | Is the given verification condition a precondition?
is_precondition (Precondition _ _ _ _) = True
is_precondition _                      = False

-- | Is the given verification condition a function constraint?
is_func_constraint (FunctionConstraint _ _) = True
is_func_constraint _                        = False

-- | Count the number of assertions in the set of verification conditions.
count_instructions_with_assertions = S.size . S.map (\(Assertion rip _ _ _ _) -> rip) . S.filter is_assertion


-- | Have functions been called by the current function?
data StateMuddleStatus = 
   Clean        -- ^ No function calls have been executed
 | ExternalOnly -- ^ All function calls were to external functions
 | Muddled      -- ^ At least one internal function has been called
  deriving (Generic,Eq,Show,Ord)

-- | A symbolic predicate consists of:
--
--   * A mapping from stateparts to symbolic expressions.
--
--   * The status of the flags.
--   
--   * A set of verification conditions.
--
--   * The @`StateMuddleStatus`@
data Pred = Predicate (M.Map StatePart SimpleExpr) FlagStatus (S.Set VerificationCondition) StateMuddleStatus
  deriving (Generic,Eq,Ord)

-- | Add a precondition to the given symbolic predicate
add_precondition       a0 si0 a1 si1 (Predicate eqs fs vcs muddle_status) = Predicate eqs fs (S.insert (Precondition a0 si0 a1 si1) vcs)  muddle_status
-- | Add an assertion to the given symbolic predicate
add_assertion      rip a0 si0 a1 si1 (Predicate eqs fs vcs muddle_status) = Predicate eqs fs (S.insert (Assertion rip a0 si0 a1 si1) vcs) muddle_status
-- | Add a function constraint to the given symbolic predicate
add_function_constraint f ps         (Predicate eqs fs vcs muddle_status) = Predicate eqs fs (S.insert (FunctionConstraint f ps) vcs) muddle_status


instance Cereal.Serialize BotTyp
instance Cereal.Serialize BotSrc
instance Cereal.Serialize AddrType
instance Cereal.Serialize StatePart
instance Cereal.Serialize Operator
instance Cereal.Serialize SimpleExpr
instance Cereal.Serialize FlagStatus
instance Cereal.Serialize VerificationCondition
instance Cereal.Serialize StateMuddleStatus
instance Cereal.Serialize Pred

instance Show FlagStatus where
 show None = ""
 show (FS_CMP Nothing      op1 op2) = "flags set by CMP(" ++ show_operand' op1 ++ "," ++ show_operand' op2 ++ ")"
 show (FS_CMP (Just True)  op1 op2) = show_operand' op1 ++ " < " ++ show_operand' op2
 show (FS_CMP (Just False) op1 op2) = show_operand' op1 ++ " >= " ++ show_operand' op2


instance Show Pred where
  show (Predicate eqs flg vcs muddle_status) =
       (intercalate "\n" $ (map (\(sp,e) -> show sp ++ " := " ++ show e) $ M.toList eqs))
    ++ (if show flg == "" then "" else "\n" ++ show flg)
    ++ (if S.size vcs  == 0 then "" else "\n" ++ show (S.toList vcs))
    ++ (if muddle_status == Clean then "" else "\n" ++ "(" ++ show muddle_status ++ ")")




