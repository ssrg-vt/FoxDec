{-# LANGUAGE DeriveGeneric, DefaultSignatures, StrictData #-}

{-|
Module      : SimplePred
Description : A datatype for symbolic expressions and symbolic predicates.

A datatype for symbolic predicates, tailored to storing information
on equalities between the current values stored in state parts (lhs) 
and constant expressions (rhs).
-}

module SimplePred ( 
  Pred (..),
  StatePart (..),
  SimpleExpr (..),
  FlagStatus (..),
  StateMuddleStatus (..),
  BotTyp (..),
  BotSrc (..),
  Operator (..),
  PointerBase(..),
  is_immediate,
  is_mem_sp,
  is_reg_sp,
  contains_bot,
  contains_bot_sp,
  all_bot_satisfy,
  simp,
  rock_bottom,
  trim_expr,
  pp_expr,
  pp_pred,
  unfold_non_determinism,
  expr_size
 )
 where

import Base
import Config
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


-- | A pointerbase is a positive addend of a symbolic expression that may represent a pointer.
data PointerBase = 
    StackPointer                      -- ^ The stackpointer, for /local/ variables
  | Malloc (Maybe Int) (Maybe String) -- ^ A malloc (at the /heap/) at a given address (hash is unused for now)
  | GlobalAddress Word64              -- ^ A /global/ address in the range of the sections of the binary.
  | PointerToSymbol Word64 String     -- ^ An address with an associated symbol.
  | Unknown SimpleExpr                -- ^ n expresion without identifiable pointerbase,
  deriving (Generic,Eq,Ord)



-- | Bot represents an unknown (bottom) value.
-- We annotate each occurence of Bot with a BotTyp.
-- This type indicates where the bottom value originates from.
-- The latter  six are all equal, we just use them for debugging and information.
-- They indicate that the  value is unknown, but was computed using the set of sources.
data BotTyp = 
    FromNonDeterminism (S.Set SimpleExpr)   -- ^ The expression evaluates to one of the expressions in the set
  | FromPointerBases (S.Set PointerBase)    -- ^ The expression is a pointer-computation with known base(s)
  | FromCall String                         -- ^ Return value of a function call

  | FromSources (S.Set BotSrc)              -- ^ The expression is some computation based on sources.
  | FromOverlap (S.Set BotSrc)              -- ^ A read from two possibly overlapping regions
  | FromMemWrite (S.Set BotSrc)             -- ^ A write to two possibly overlapping regions 
  | FromSemantics (S.Set BotSrc)            -- ^ An instruction with unknown semantics
  | FromBitMode (S.Set BotSrc)              -- ^ Should not happen, but if a register writes to a registeralias with unknown bit size
  | FromUninitializedMemory (S.Set BotSrc)  -- ^ Reading from memory not written to yet
 deriving (Eq, Ord, Generic)


-- | Sources that may be used to compute an expression. That is, the inputs to an expression.
data BotSrc = 
    Src_Var StatePart                       -- An initial variable, i.e., a constant
  | Src_Malloc (Maybe Int) (Maybe String)   -- A malloced address
  | Src_Function String                     -- A function return value
 deriving (Eq, Ord, Generic)




-- | An operator is a pure operation over bit-vectors, annotated with the bit-size of its operands.
-- For example, @Plus 64@ denotes 64-bit addition.
-- @Udiv@ and @Times@ are operators op type @w -> w -> w@ with all words same length.
-- @Div@ and @Div_Rem@ are operators of type @w -> w -> w -> w@ performing concatenation of the first two words and then doing division/remainder.
data Operator = 
    Minus Int | Plus Int | Times Int | And Int | Or Int | Xor Int | Not Int | SetXX | Bsr Int 
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

-- | A symbolic expression with as leafs either immediates, variables, live values of stateparts, or malloced addresses.
-- A variable is a constant representing some initial value, e.g., RDI_0, or [RSP_0,8]_0.
-- A statepart evaluates to its current value, e.g., RDI or [RSP,8].
data SimpleExpr =
    SE_Immediate Word64                     -- ^ An immediate word
  | SE_Var       StatePart                  -- ^ A variable representing the initial value stored in the statepart (e.g., RSP0)
  | SE_StatePart StatePart                  -- ^ The value stored currently in the statepart
  | SE_Malloc    (Maybe Int) (Maybe String) -- ^ A malloc return value with possibly an ID
  | SE_Op        Operator [SimpleExpr]      -- ^ Application of an @`Operator`@ to the list of arguments
  | SE_Bit       Int SimpleExpr             -- ^ Taking the lower bits of a value
  | SE_SExtend   Int Int SimpleExpr         -- ^ Sign extension
  | SE_Overwrite Int SimpleExpr SimpleExpr  -- ^ Overwriting certain bits of a value with bits from another value
  | Bottom       BotTyp                     -- ^ Bottom (unknown value)
 deriving (Eq, Ord, Generic)

-- | A statepart is either a register or a region in memory
data StatePart =
    SP_Reg Register          -- ^ A register
  | SP_Mem SimpleExpr Int    -- ^ A region with a symbolic address and an immediate size.
 deriving (Eq, Ord, Generic)


instance Show BotSrc where
  show (Src_Var sp)      = show $ SE_Var sp
  show (Src_Malloc id h) = show $ SE_Malloc id h
  show (Src_Function f)  = f

show_srcs srcs = "|" ++ intercalate "," (map show $ S.toList srcs) ++ "|"

show_bottyp show_srcs (FromNonDeterminism es)        = "nd|" ++ intercalate "," (map show $ S.toList es) ++ "|"
show_bottyp show_srcs (FromPointerBases bs)          = "pbs|" ++ intercalate "," (map show $ S.toList bs) ++ "|"
show_bottyp show_srcs (FromSources srcs)             = "src" ++ show_srcs srcs
show_bottyp show_srcs (FromBitMode srcs)             = "b" ++ show_srcs srcs
show_bottyp show_srcs (FromOverlap srcs)             = "o" ++ show_srcs srcs
show_bottyp show_srcs (FromSemantics srcs)           = "s" ++ show_srcs srcs
show_bottyp show_srcs (FromMemWrite srcs)            = "w" ++ show_srcs srcs
show_bottyp show_srcs (FromUninitializedMemory srcs) = "m" ++ show_srcs srcs
show_bottyp show_srcs (FromCall f)                   = "c|" ++ f ++ "|"

instance Show BotTyp where
  show = show_bottyp show_srcs

show_expr show_srcs (Bottom typ)              = "Bot[" ++ show_bottyp show_srcs typ ++ "]"
show_expr show_srcs (SE_Malloc Nothing _)     = "malloc()" 
show_expr show_srcs (SE_Malloc (Just id) _)   = "malloc@" ++ showHex id ++ "()" 
show_expr show_srcs (SE_Var sp)               = show sp ++ "_0"
show_expr show_srcs (SE_Immediate i)          = if i > 2000 then "0x" ++ showHex i else show i
show_expr show_srcs (SE_StatePart sp)         = show sp
show_expr show_srcs (SE_Op (Plus  _) [e0,e1]) = "(" ++ show_expr show_srcs e0 ++ " + "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (Minus _) [e0,e1]) = "(" ++ show_expr show_srcs e0 ++ " - "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (Times _) [e0,e1]) = "(" ++ show_expr show_srcs e0 ++ " * "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (And   _) [e0,e1]) = "(" ++ show_expr show_srcs e0 ++ " & "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (Or    _) [e0,e1]) = "(" ++ show_expr show_srcs e0 ++ " | "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (Xor   _) [e0,e1]) = "(" ++ show_expr show_srcs e0 ++ " xor " ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op op es)             = show op ++ "(" ++ intercalate "," (map (show_expr show_srcs) es) ++ ")"
show_expr show_srcs (SE_Bit i a)              = "b" ++ show i ++ "(" ++ show_expr show_srcs a ++ ")"
show_expr show_srcs (SE_SExtend l h a)        = "signextend(" ++ show l ++ "," ++ show h ++ ", " ++ show_expr show_srcs a ++ ")"
show_expr show_srcs (SE_Overwrite i a b)      = "overwrite(" ++ show i ++ "," ++ show_expr show_srcs a ++ "," ++ show_expr show_srcs b ++ ")"


instance Show SimpleExpr where
  show = show_expr show_srcs





-- | Returns true iff the expression is an immediate value
is_immediate (SE_Immediate _) = True
is_immediate _                = False


-- | Is the statepart memory?
is_mem_sp (SP_Reg _)    = False
is_mem_sp (SP_Mem a si) = True

-- | Is the statepart a register?
is_reg_sp (SP_Reg _)    = True
is_reg_sp (SP_Mem a si) = False


-- | Returns true iff the expression contains Bot
contains_bot (Bottom typ)         = True
contains_bot (SE_Malloc _ _)      = False
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






-- | concatMap function p over all bottom values in the expression  
map_all_bot :: Ord a => (BotTyp -> S.Set a) -> SimpleExpr -> S.Set a
map_all_bot p (Bottom typ)         = p typ
map_all_bot p (SE_Malloc _ _)      = S.empty
map_all_bot p (SE_Var sp)          = map_all_bot_sp p sp
map_all_bot p (SE_Immediate _)     = S.empty
map_all_bot p (SE_StatePart sp)    = map_all_bot_sp p sp
map_all_bot p (SE_Op _ es)         = S.unions $ map (map_all_bot p) es
map_all_bot p (SE_Bit i e)         = map_all_bot p e
map_all_bot p (SE_SExtend _ _ e)   = map_all_bot p e
map_all_bot p (SE_Overwrite _ a b) = S.unions [map_all_bot p a, map_all_bot p b]

map_all_bot_sp p (SP_Reg r)      = S.empty
map_all_bot_sp p (SP_Mem a si)   = map_all_bot p a


-- | Do all occurences of Bottom satisfy the given predicate?
all_bot_satisfy p = and . map_all_bot (S.singleton . p)




-- | 
expr_size (Bottom typ)         = 1 + expr_size_bottyp typ
expr_size (SE_Malloc id _)     = 1
expr_size (SE_Var sp)          = expr_size_sp sp
expr_size (SE_Immediate _)     = 1
expr_size (SE_StatePart sp)    = expr_size_sp sp
expr_size (SE_Op _ es)         = 1 + (sum $ map expr_size es)
expr_size (SE_Bit i e)         = 1 + expr_size e
expr_size (SE_SExtend l h e)   = 1 + expr_size e
expr_size (SE_Overwrite _ _ e) = 1 + expr_size e

expr_size_sp (SP_Reg r)     = 1
expr_size_sp (SP_Mem a si)  = 1 + expr_size a 

expr_size_bottyp (FromNonDeterminism es)        = sum $ S.map expr_size es
expr_size_bottyp (FromPointerBases bs)          = S.size bs
expr_size_bottyp (FromSources srcs)             = S.size srcs
expr_size_bottyp (FromBitMode srcs)             = S.size srcs
expr_size_bottyp (FromOverlap srcs)             = S.size srcs
expr_size_bottyp (FromSemantics srcs)           = S.size srcs
expr_size_bottyp (FromMemWrite srcs)            = S.size srcs
expr_size_bottyp (FromUninitializedMemory srcs) = S.size srcs
expr_size_bottyp (FromCall _)                   = 1


-- | If the size of an expression becomes too large, we simply turn it into Bottom.
trim_expr e =
  if expr_size e > max_expr_size then 
    traceShow ("Hitting expr_size limit of " ++ show max_expr_size ++ ".") rock_bottom -- Bottom (FromSources $ srcs_of_expr e)
  else
    e




-- | The lowest botom element
rock_bottom = Bottom (FromSources S.empty)



sextend_32_64 w = if testBit w 31 then w .|. 0xFFFFFFFF00000000 else w
sextend_16_64 w = if testBit w 15 then w .|. 0xFFFFFFFFFFFF0000 else w
sextend_8_64  w = if testBit w 7  then w .|. 0xFFFFFFFFFFFFFF00 else w


-- | Simplification of symbolic expressions. 
--
-- Must always produce an expression logically equivalent to the original.
simp :: SimpleExpr -> SimpleExpr
simp e = 
  let e' = simp' e in
    if e == e' then e' else simp e'

simp' (SE_Bit i (SE_Bit i' e))   = SE_Bit (min i i') $ simp' e
simp' (SE_Bit i (SE_Overwrite i' e0 e1)) = if i <= i' then SE_Bit i (simp' e1) else SE_Bit i $ SE_Overwrite i' (simp' e0) (simp' e1)

simp' (SE_Overwrite i (SE_Overwrite i' e0 e1) e2) = if i >= i' then SE_Overwrite i (simp' e0) (simp' e2) else SE_Overwrite i (SE_Overwrite i' (simp' e0) (simp' e1)) (simp' e2)
simp' (SE_SExtend l h (SE_Bit i e))  = if i == l then SE_SExtend l h (simp' e) else SE_SExtend l h (SE_Bit i $ simp' e)

simp' (SE_Op (Minus b0) [SE_Op (Minus b1) [a0,a1], a2]) = simp' $ SE_Op (Minus b0) [a0, SE_Op (Plus b0)  [a1, a2]] -- (a0-a1)-a2 ==> a0 - (a1 + a2)
simp' (SE_Op (Plus b0)  [SE_Op (Minus b1) [a0,a1], a2]) = simp' $ SE_Op (Minus b0) [a0, SE_Op (Minus b0) [a1, a2]] -- (a0-a1)+a2 ==> a0 - (a1 - a2)
simp' (SE_Op (Minus b0) [SE_Op (Plus b1)  [a0,a1], a2]) = simp' $ SE_Op (Plus b0)  [a0, SE_Op (Minus b0) [a1, a2]] -- (a0+a1)-a2 ==> a0 + (a1 - a2)
simp' (SE_Op (Plus b0)  [SE_Op (Plus b1)  [a0,a1], a2]) = simp' $ SE_Op (Plus b0)  [a0, SE_Op (Plus b0)  [a1, a2]] -- (a0+a1)+a2 ==> a0 + (a1 + a2)

simp' (SE_Op (Plus b0)  [SE_Immediate 0, e1]) = simp' e1 -- 0 + e1 = e1
simp' (SE_Op (Plus b0)  [e0, SE_Immediate 0]) = simp' e0 -- e0 + 0 = e0
simp' (SE_Op (Minus b0) [e0, SE_Immediate 0]) = simp' e0 -- e0 - 0 = e0
simp' (SE_Op (Times b0) [e0, SE_Immediate 0]) = SE_Immediate 0
simp' (SE_Op (Times b0) [SE_Immediate 0, e1]) = SE_Immediate 0

simp' (SE_Overwrite i (SE_Immediate 0) e) = simp' e

simp' (SE_Op (Minus b0) [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 - i1)   -- Immediate: i0 - i1
simp' (SE_Op (Plus  b0) [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 + i1)   -- Immediate: i0 + i1
simp' (SE_Op (Times b0) [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 * i1)   -- Immediate: i0 * i1
simp' (SE_Op (Or    b0) [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 .|. i1) -- Immediate: i0 | i1
simp' (SE_Op (And   b0) [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 .&. i1) -- Immediate: i0 & i1



simp' (SE_Bit 32 (SE_Immediate i)) = SE_Immediate (i .&. 0x00000000FFFFFFFF)
simp' (SE_Bit 16 (SE_Immediate i)) = SE_Immediate (i .&. 0x000000000000FFFF)
simp' (SE_Bit 8  (SE_Immediate i)) = SE_Immediate (i .&. 0x00000000000000FF)

simp' (SE_SExtend 32 64 (SE_Immediate i)) = SE_Immediate (sextend_32_64 i)
simp' (SE_SExtend 16 64 (SE_Immediate i)) = SE_Immediate (sextend_16_64 i)
simp' (SE_SExtend 8  64 (SE_Immediate i)) = SE_Immediate (sextend_8_64 i)

simp' (SE_Op (Minus b0) [SE_Immediate i0, SE_Op (Minus b1) [e1, SE_Immediate i1]]) = simp' $ SE_Op (Minus b0) [SE_Immediate (i0+i1), e1] -- i0-(e1-i1) ==> (i0+i1)-e1
simp' (SE_Op (Minus b0) [SE_Immediate i0, SE_Op (Plus b1)  [e1, SE_Immediate i1]]) = simp' $ SE_Op (Minus b0) [SE_Immediate (i0-i1), e1] -- i0-(e1+i1) ==> (i0-i1)-e1
simp' (SE_Op (Minus b0) [SE_Immediate i0, SE_Op (Plus b1)  [SE_Immediate i1, e1]]) = simp' $ SE_Op (Minus b0) [SE_Immediate (i0-i1), e1] -- i0-(i1+e1) ==> (i0-i1)-e1
simp' (SE_Op (Plus b0)  [SE_Immediate i0, SE_Op (Minus b1) [e1, SE_Immediate i1]]) = simp' $ SE_Op (Plus b0)  [e1, SE_Immediate (i0-i1)] -- i0+(e1-i1) ==> e1+(i0-i1)
simp' (SE_Op (Plus b0)  [SE_Immediate i0, SE_Op (Plus  b1) [e1, SE_Immediate i1]]) = simp' $ SE_Op (Plus b0)  [e1, SE_Immediate (i0+i1)] -- i0+(e1+i1) ==> e1+(i0+i1)



simp' (SE_Op (Minus b) [e,SE_Immediate i]) = if testBit i 63 then SE_Op (Plus b)  [simp' e,SE_Immediate (-i)] else SE_Op (Minus b) [simp' e,SE_Immediate i]
simp' (SE_Op (Plus  b) [e,SE_Immediate i]) = if testBit i 63 then SE_Op (Minus b) [simp' e,SE_Immediate (-i)] else SE_Op (Plus  b) [simp' e,SE_Immediate i]


simp' (SE_Bit i (SE_Op (Plus  b0) [e0, e1])) = simp' $ SE_Op (Plus i)  [SE_Bit i e0, SE_Bit i e1] -- b(e0+e1) = b(e0) + b(e1)
simp' (SE_Bit i (SE_Op (Minus b0) [e0, e1])) = simp' $ SE_Op (Minus i) [SE_Bit i e0, SE_Bit i e1] -- b(e0-e1) = b(e0) - b(e1)
simp' (SE_Bit i (SE_Op (Times b0) [e0, e1])) = simp' $ SE_Op (Times i) [SE_Bit i e0, SE_Bit i e1] -- b(e0*e1) = b(e0) * b(e1)
simp' (SE_Bit i (SE_Op (And   b0) [e0, e1])) = simp' $ SE_Op (And i)   [SE_Bit i e0, SE_Bit i e1] -- b(e0&e1) = b(e0) & b(e1)
simp' (SE_Bit i (SE_Op (Or    b0) [e0, e1])) = simp' $ SE_Op (Or i)    [SE_Bit i e0, SE_Bit i e1] -- b(e0|e1) = b(e0) | b(e1)
simp' (SE_Bit i (SE_Op (Xor   b0) [e0, e1])) = simp' $ SE_Op (Xor i)   [SE_Bit i e0, SE_Bit i e1] -- b(e0 xor e1) = b(e0) xor b(e1)
simp' (SE_Bit i (SE_Op (Not   b0) [e0]))     = simp' $ SE_Op (Not i)   [SE_Bit i e0]              -- b(not e0) = b(not e0)


simp' (Bottom (FromNonDeterminism es)) = Bottom $ FromNonDeterminism $ S.map simp' es
simp' (SE_Op op es)          = SE_Op op $ map simp' es
simp' (SE_StatePart sp)      = SE_StatePart $ simp'_sp sp
simp' (SE_Bit i e)           = SE_Bit i $ simp' e
simp' (SE_SExtend l h e)     = SE_SExtend l h $ simp' e
simp' (SE_Overwrite i e0 e1) = SE_Overwrite i (simp' e0) (simp' e1)
simp' (SE_Var sp)            = SE_Var $ simp'_sp sp
simp' e                      = e

simp'_sp (SP_Mem e si)    = SP_Mem (simp' e) si
simp'_sp sp               = sp




instance Show StatePart where
  show (SP_Reg r) = show r
  show (SP_Mem a si) = "[" ++ show a ++ ", " ++ show si ++ "]"

-- | Symbolically represent the status of all flags in the current state
data FlagStatus = 
    None                                 -- ^ No information known, flags could have any value
  | FS_CMP (Maybe Bool) Operand Operand  -- ^ The flags are set by the x86 CMP instruction applied to the given operands.
  deriving (Generic,Eq,Ord)


instance Show PointerBase where
  show StackPointer            = "StackPointer"
  show (Malloc Nothing  _)     = "malloc()"
  show (Malloc (Just a) _)     = "malloc@" ++ showHex a ++ "()"
  show (GlobalAddress a)       = "GlobalAddress@" ++ showHex a
  show (PointerToSymbol a sym) = "PointerToSymbol_" ++ sym
  show (Unknown e)             = "Unknown_" ++ show e


-- | Have functions been called by the current function?
data StateMuddleStatus = 
   Clean        -- ^ No function calls have been executed
 | ExternalOnly -- ^ All function calls were to external functions
 | Muddled      -- ^ At least one internal function has been called
  deriving (Generic,Eq,Show,Ord)



-- | A symbolic predicate consists of:
--
--   * A mapping from stateparts to symbolic expressions.
--   * The status of the flags.
--   * A set of verification conditions.
--   * The @"StateMuddleStatus"@.
data Pred = Predicate (M.Map StatePart SimpleExpr) FlagStatus StateMuddleStatus
  deriving (Generic,Eq,Ord)

instance Cereal.Serialize PointerBase
instance Cereal.Serialize BotTyp
instance Cereal.Serialize BotSrc
instance Cereal.Serialize StatePart
instance Cereal.Serialize Operator
instance Cereal.Serialize SimpleExpr
instance Cereal.Serialize FlagStatus
instance Cereal.Serialize StateMuddleStatus
instance Cereal.Serialize Pred

instance Show FlagStatus where
 show None = ""
 show (FS_CMP Nothing      op1 op2) = "flags set by CMP(" ++ show_operand' op1 ++ "," ++ show_operand' op2 ++ ")"
 show (FS_CMP (Just True)  op1 op2) = show_operand' op1 ++ " < " ++ show_operand' op2
 show (FS_CMP (Just False) op1 op2) = show_operand' op1 ++ " >= " ++ show_operand' op2


instance Show Pred where
  show (Predicate eqs flg muddle_status) =
       (intercalate "\n" $ (map (\(sp,e) -> show sp ++ " := " ++ show e) $ M.toList eqs))
    ++ (if show flg == "" then "" else "\n" ++ show flg)
    ++ (if muddle_status == Clean then "" else "\n" ++ "(" ++ show muddle_status ++ ")")



-- | Pretty print expression, showing Bottom expressions only as Bot
pp_expr = show_expr (\_ -> "")

-- | Pretty print predicate, showing Bottom expressions only as Bot
pp_pred :: Pred -> String
pp_pred (Predicate eqs _ muddlestatus) = (intercalate "\n" $ mapMaybe pp_pred_entry $ M.toList eqs) ++ "\n" ++ show muddlestatus
 where
  pp_pred_entry (sp,v) =
    --if sp == SP_Reg RIP || contains_bot_sp sp then
    --  Nothing
    --else
      Just $ show sp ++ " := " ++ pp_expr v



-- crossProduct [[1], [2,3,4], [5]] == [[1,2,5],[1,3,5],[1,4,5]]
-- The size of a crossProduct [x_0,x_1,x_i] is the number of produced lists |x_0|*|x_1|*...*|x_i| times the size of each list i.
crossProduct :: [[a]] -> [[a]]
crossProduct []       = [[]]
crossProduct (as:ass) = [ b:bs | b <- as, bs <- crossProduct ass ]

crossProduct_size x = product (length x : map length x) 

-- | Unfold an expression with non-determinisism to a list of expressions.
-- Keep an eye on the produced size, as this may cause blow-up.
unfold_non_determinism :: SimpleExpr -> [SimpleExpr]
unfold_non_determinism (Bottom (FromNonDeterminism es)) = S.toList es
unfold_non_determinism (SE_Op op es)                    = 
  let es' = map unfold_non_determinism es in
    if crossProduct_size es' > max_expr_size then [rock_bottom] else map (SE_Op op) $ crossProduct es'
unfold_non_determinism (SE_Bit b e)                     = map (SE_Bit b) $ unfold_non_determinism e
unfold_non_determinism (SE_SExtend l h e)               = map (SE_SExtend l h) $ unfold_non_determinism e
unfold_non_determinism (SE_Overwrite l a b)             = [ SE_Overwrite l a' b' | a' <-  unfold_non_determinism a, b' <- unfold_non_determinism b ]
unfold_non_determinism e                                = [e]
