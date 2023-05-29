{-# LANGUAGE DeriveGeneric, DefaultSignatures, StrictData #-}

{-|
Module      : SimplePred
Description : A datatype for symbolic expressions and symbolic predicates.

A datatype for symbolic predicates, tailored to storing information
on equalities between the current values stored in state parts (lhs) 
and constant expressions (rhs).
-}

module Data.SymbolicExpression ( 
  StatePart (..),
  SimpleExpr (..),
  FlagStatus (..),
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
  pp_expr,
  expr_size,
  sextend_32_64,
  sextend_16_64,
  sextend_8_64
 )
 where

import Base
import Config
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.Set.NonEmpty as NES

import Data.Int (Int32,Int64)
import Data.Word (Word64,Word32)
import Data.Traversable (for)
import Data.List
import Data.Maybe (mapMaybe,fromJust)
import Debug.Trace
import GHC.Generics
import Data.Bits (testBit, (.|.), (.&.), xor)
import qualified Data.Serialize as Cereal hiding (get,put)
import X86.Register (Register)
import qualified X86.Operand as X86

import Control.DeepSeq


-- | A pointerbase is a positive addend of a symbolic expression that may represent a pointer.
data PointerBase = 
    StackPointer String                  -- ^ The stack frame of the given function
  | Malloc (Maybe Word64) (Maybe String) -- ^ A malloc (at the /heap/) at a given address (hash is unused for now)
  | GlobalAddress Word64                 -- ^ A /global/ address in the range of the sections of the binary.
  | PointerToSymbol Word64 String        -- ^ An address with an associated symbol.
  | ThreadLocalStorage                   -- ^ A pointer to the thread-local-storage (e.g., FS register)
  deriving (Generic,Eq,Ord)



-- | Bot represents an unknown (bottom) value.
-- We annotate each occurence of Bot with a BotTyp.
-- This type indicates where the bottom value originates from.
-- The latter  six are all equal, we just use them for debugging and information.
-- They indicate that the  value is unknown, but was computed using the set of sources.
data BotTyp = 
    FromNonDeterminism (NES.NESet SimpleExpr)   -- ^ The expression evaluates to one of the expressions in the set
  | FromPointerBases (NES.NESet PointerBase)    -- ^ The expression is a pointer-computation with known base(s)
  | FromCall String                             -- ^ Return value of a function call
  | FromSources (NES.NESet BotSrc)              -- ^ The expression is some computation based on sources.
  | FromOverlap (NES.NESet BotSrc)              -- ^ A read from two possibly overlapping regions
  | FromMemWrite (NES.NESet BotSrc)             -- ^ A write to two possibly overlapping regions 
  | FromSemantics (NES.NESet BotSrc)            -- ^ An instruction with unknown semantics
  | FromBitMode (NES.NESet BotSrc)              -- ^ Should not happen, but if a register writes to a registeralias with unknown bit size
  | FromUninitializedMemory (NES.NESet BotSrc)  -- ^ Reading from memory not written to yet
 deriving (Eq, Ord, Generic)


-- | Sources that may be used to compute an expression. That is, the inputs to an expression.
data BotSrc = 
    Src_Var StatePart                        -- ^ An initial variable, i.e., a constant
  | Src_StackPointer String                  -- ^ The stack pointer of the given function
  | Src_Malloc (Maybe Word64) (Maybe String) -- ^ A malloced address
  | Src_ImmediateAddress Word64              -- ^ An immediate used in the computation of the pointer
  | Src_Function String                      -- ^ A return value from a function
  | Src_Mem (NES.NESet BotSrc)               -- ^ reading from memory
  | Src_ImmediateConstants                   -- ^ Some immediate value
 deriving (Eq, Ord, Generic)




-- | An operator is a pure operation over bit-vectors, annotated with the bit-size of its operands.
-- For example, @Plus 64@ denotes 64-bit addition.
-- @Udiv@ and @Times@ are operators op type @w -> w -> w@ with all words same length.
-- @Div@ and @Div_Rem@ are operators of type @w -> w -> w -> w@ performing concatenation of the first two words and then doing division/remainder.
data Operator = 
    Minus | Plus | Times | And | Or | Xor | Not | Bsr 
  | Div_Rem | Div | Shl | Shr | Sar | Udiv | Ror | Rol 
  | Bswap | Pextr
 deriving (Eq, Ord, Generic)


instance Show Operator where
  show (Plus    ) = "+"       
  show (Minus   ) = "-"       
  show (Times   ) = "*"       
  show (And     ) = "&"       
  show (Or      ) = "|"       
  show (Xor     ) = "xor"     
  show (Not     ) = "not"     
  show (Bsr     ) = "bsr"     
  show (Div_Rem ) = "div_rem" 
  show (Div     ) = "div"     
  show (Shl     ) = "shl"     
  show (Shr     ) = "shr"     
  show (Sar     ) = "sar"     
  show (Udiv    ) = "udiv"    
  show (Ror     ) = "ror"     
  show (Rol     ) = "rol"
  show (Bswap   ) = "bswap"
  show (Pextr   ) = "pextr"

-- | A symbolic expression with as leafs either immediates, variables, live values of stateparts, or malloced addresses.
-- A variable is a constant representing some initial value, e.g., RDI_0, or [RSP_0,8]_0.
-- A statepart evaluates to its current value, e.g., RDI or [RSP,8].
data SimpleExpr =
    SE_Immediate Word64                        -- ^ An immediate word
  | SE_Var       StatePart                     -- ^ A variable representing the initial value stored in the statepart (e.g., RSP0)
  | SE_StatePart StatePart                     -- ^ The value stored currently in the statepart
  | SE_Malloc    (Maybe Word64) (Maybe String) -- ^ A malloc return value with possibly an ID
  | SE_Op        Operator Int [SimpleExpr]     -- ^ Application of an @`Operator`@ to the list of arguments
  | SE_Bit       Int SimpleExpr                -- ^ Taking the lower bits of a value
  | SE_SExtend   Int Int SimpleExpr            -- ^ Sign extension
  | SE_Overwrite Int SimpleExpr SimpleExpr     -- ^ Overwriting certain bits of a value with bits from another value
  | Bottom       BotTyp                        -- ^ Bottom (unknown value)
 deriving (Eq, Ord, Generic)

-- | A statepart is either a register or a region in memory
data StatePart =
    SP_StackPointer String   -- ^ The stack pointer of the given function
  | SP_Reg Register          -- ^ A register
  | SP_Mem SimpleExpr Int    -- ^ A region with a symbolic address and an immediate size.
 deriving (Eq, Ord, Generic)


instance Show BotSrc where
  show (Src_Var sp)               = show $ SE_Var sp
  show (Src_StackPointer f)       = "RSP_" ++ f
  show (Src_Malloc id h)          = show $ SE_Malloc id h
  show (Src_ImmediateAddress a)   = showHex a
  show (Src_Function f)           = f
  show (Src_Mem a)                = "*[" ++ intercalate "," (map show $ neSetToList a) ++ "]"
  show (Src_ImmediateConstants)   = "constant"

show_srcs srcs = "|" ++ intercalate "," (map show $ S.toList $ NES.toSet srcs) ++ "|"

show_bottyp show_srcs (FromNonDeterminism es)        = "nd|" ++ intercalate "," (map show $ S.toList $ NES.toSet es) ++ "|"
show_bottyp show_srcs (FromPointerBases bs)          = "pbs|" ++ intercalate "," (map show $ S.toList $ NES.toSet bs) ++ "|"
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
show_expr show_srcs (SE_Op (Plus ) _ [e0,e1]) = "(" ++ show_expr show_srcs e0 ++ " + "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (Minus) _ [e0,e1]) = "(" ++ show_expr show_srcs e0 ++ " - "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (Times) _ [e0,e1]) = "(" ++ show_expr show_srcs e0 ++ " * "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (And  ) _ [e0,e1]) = "(" ++ show_expr show_srcs e0 ++ " & "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (Or   ) _ [e0,e1]) = "(" ++ show_expr show_srcs e0 ++ " | "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (Xor  ) _ [e0,e1]) = "(" ++ show_expr show_srcs e0 ++ " xor " ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op op si es)          = show op ++ show si ++ "(" ++ intercalate "," (map (show_expr show_srcs) es) ++ ")"
show_expr show_srcs (SE_Bit i a)              = "b" ++ show i ++ "(" ++ show_expr show_srcs a ++ ")"
show_expr show_srcs (SE_SExtend l h a)        = "signextend(" ++ show l ++ "," ++ show h ++ ", " ++ show_expr show_srcs a ++ ")"
show_expr show_srcs (SE_Overwrite i a b)      = "overwrite(" ++ show i ++ "," ++ show_expr show_srcs a ++ "," ++ show_expr show_srcs b ++ ")"


instance Show SimpleExpr where
  show = show_expr show_srcs





-- | Returns true iff the expression is an immediate value
is_immediate (SE_Immediate _) = True
is_immediate _                = False


-- | Is the statepart memory?
is_mem_sp (SP_StackPointer _) = False
is_mem_sp (SP_Reg _)          = False
is_mem_sp (SP_Mem a si)       = True

-- | Is the statepart a register?
is_reg_sp (SP_StackPointer _) = True
is_reg_sp (SP_Reg _)          = True
is_reg_sp (SP_Mem a si)       = False


-- | Returns true iff the expression contains Bot
contains_bot (Bottom typ)         = True
contains_bot (SE_Malloc _ _)      = False
contains_bot (SE_Var sp)          = contains_bot_sp sp
contains_bot (SE_Immediate _)     = False
contains_bot (SE_StatePart sp)    = contains_bot_sp sp
contains_bot (SE_Op _ _ es)       = any contains_bot es
contains_bot (SE_Bit i e)         = contains_bot e
contains_bot (SE_SExtend _ _ e)   = contains_bot e
contains_bot (SE_Overwrite _ a b) = contains_bot a || contains_bot b

-- | Returns true iff the statepart contains Bot
contains_bot_sp (SP_StackPointer _) = False
contains_bot_sp (SP_Reg r)          = False
contains_bot_sp (SP_Mem a si)       = contains_bot a





-- | concatMap function p over all bottom values in the expression  
all_bot_satisfy :: (BotTyp -> Bool) -> SimpleExpr -> Bool
all_bot_satisfy p (Bottom typ)         = p typ
all_bot_satisfy p (SE_Malloc _ _)      = True
all_bot_satisfy p (SE_Var sp)          = all_bot_satisfy_sp p sp
all_bot_satisfy p (SE_Immediate _)     = True
all_bot_satisfy p (SE_StatePart sp)    = all_bot_satisfy_sp p sp
all_bot_satisfy p (SE_Op _ _ es)       = all (all_bot_satisfy p) es
all_bot_satisfy p (SE_Bit i e)         = all_bot_satisfy p e
all_bot_satisfy p (SE_SExtend _ _ e)   = all_bot_satisfy p e
all_bot_satisfy p (SE_Overwrite _ a b) = and [all_bot_satisfy p a, all_bot_satisfy p b]

all_bot_satisfy_sp p (SP_StackPointer r) = True
all_bot_satisfy_sp p (SP_Reg r)          = True
all_bot_satisfy_sp p (SP_Mem a si)       = all_bot_satisfy p a





-- | 
expr_size (Bottom typ)         = 1 + expr_size_bottyp typ
expr_size (SE_Malloc id _)     = 1
expr_size (SE_Var sp)          = expr_size_sp sp
expr_size (SE_Immediate _)     = 1
expr_size (SE_StatePart sp)    = expr_size_sp sp
expr_size (SE_Op _ _ es)       = 1 + (sum $ map expr_size es)
expr_size (SE_Bit i e)         = 1 + expr_size e
expr_size (SE_SExtend l h e)   = 1 + expr_size e
expr_size (SE_Overwrite _ _ e) = 1 + expr_size e

expr_size_sp (SP_StackPointer _) = 1
expr_size_sp (SP_Reg r)          = 1
expr_size_sp (SP_Mem a si)       = 1 + expr_size a 

expr_size_bottyp (FromNonDeterminism es)        = sum $ NES.map expr_size es
expr_size_bottyp (FromPointerBases bs)          = NES.size bs
expr_size_bottyp (FromSources srcs)             = NES.size srcs
expr_size_bottyp (FromBitMode srcs)             = NES.size srcs
expr_size_bottyp (FromOverlap srcs)             = NES.size srcs
expr_size_bottyp (FromSemantics srcs)           = NES.size srcs
expr_size_bottyp (FromMemWrite srcs)            = NES.size srcs
expr_size_bottyp (FromUninitializedMemory srcs) = NES.size srcs
expr_size_bottyp (FromCall _)                   = 1




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

simp' (SE_Bit i (SE_Bit i' e))           = SE_Bit (min i i') $ simp' e
simp' (SE_Bit i (SE_Overwrite i' e0 e1)) = if i <= i' then SE_Bit i (simp' e1) else SE_Bit i $ SE_Overwrite i' (simp' e0) (simp' e1)
simp' (SE_Bit i (SE_SExtend l h e))      = if i == l then simp' e else SE_Bit i $ SE_SExtend l h $ simp' e

simp' (SE_Overwrite i (SE_Overwrite i' e0 e1) e2) = if i >= i' then SE_Overwrite i (simp' e0) (simp' e2) else SE_Overwrite i (SE_Overwrite i' (simp' e0) (simp' e1)) (simp' e2)

simp' (SE_SExtend l h (SE_Bit i e))  = if i == l then SE_SExtend l h (simp' e) else SE_SExtend l h (SE_Bit i $ simp' e)

simp' (SE_Op Minus si0 [SE_Op Minus _ [a0,a1], a2]) = simp' $ SE_Op Minus si0 [a0, SE_Op Plus  si0 [a1, a2]] -- (a0-a1)-a2 ==> a0 - (a1 + a2)
simp' (SE_Op Plus  si0 [SE_Op Minus _ [a0,a1], a2]) = simp' $ SE_Op Minus si0 [a0, SE_Op Minus si0 [a1, a2]] -- (a0-a1)+a2 ==> a0 - (a1 - a2)
simp' (SE_Op Minus si0 [SE_Op Plus  _ [a0,a1], a2]) = simp' $ SE_Op Plus  si0 [a0, SE_Op Minus si0 [a1, a2]] -- (a0+a1)-a2 ==> a0 + (a1 - a2)
simp' (SE_Op Plus  si0 [SE_Op Plus  _ [a0,a1], a2]) = simp' $ SE_Op Plus  si0 [a0, SE_Op Plus  si0 [a1, a2]] -- (a0+a1)+a2 ==> a0 + (a1 + a2)

simp' (SE_Op Plus  si0 [SE_Immediate 0, e1]) = simp' e1 -- 0 + e1 = e1
simp' (SE_Op Plus  si0 [e0, SE_Immediate 0]) = simp' e0 -- e0 + 0 = e0
simp' (SE_Op Minus si0 [e0, SE_Immediate 0]) = simp' e0 -- e0 - 0 = e0
simp' (SE_Op Times si0 [e0, SE_Immediate 0]) = SE_Immediate 0
simp' (SE_Op Times si0 [SE_Immediate 0, e1]) = SE_Immediate 0
simp' (SE_Op Udiv  si0 [SE_Immediate 0, e1]) = SE_Immediate 0
simp' (SE_Op Div   si0 [SE_Immediate 0, e1]) = SE_Immediate 0

simp' (SE_Overwrite i (SE_Immediate 0) e) = simp' e

simp' (SE_Op Minus si0 [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 - i1)     -- Immediate: i0 - i1
simp' (SE_Op Plus  si0 [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 + i1)     -- Immediate: i0 + i1
simp' (SE_Op Times si0 [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 * i1)     -- Immediate: i0 * i1
simp' (SE_Op Or    si0 [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 .|. i1)   -- Immediate: i0 | i1
simp' (SE_Op And   si0 [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 .&. i1)   -- Immediate: i0 & i1
simp' (SE_Op Xor   si0 [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 `xor` i1) -- Immediate: i0 xor i1
simp' (SE_Op Udiv  64  [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0  `div` i1) 
simp' (SE_Op Udiv  32  [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (fromIntegral ((fromIntegral i0::Word32) `div` (fromIntegral i1::Word32)))
simp' (SE_Op Div   64  [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (fromIntegral ((fromIntegral i0::Int64)  `div` (fromIntegral i1::Int64))) 
simp' (SE_Op Div   32  [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (fromIntegral ((fromIntegral i0::Int32)  `div` (fromIntegral i1::Int32)))



simp' (SE_Bit 128 (SE_Immediate i)) = SE_Immediate i
simp' (SE_Bit 32  (SE_Immediate i)) = SE_Immediate (i .&. 0x00000000FFFFFFFF)
simp' (SE_Bit 16  (SE_Immediate i)) = SE_Immediate (i .&. 0x000000000000FFFF)
simp' (SE_Bit 8   (SE_Immediate i)) = SE_Immediate (i .&. 0x00000000000000FF)

simp' (SE_SExtend 32 64 (SE_Immediate i)) = SE_Immediate (sextend_32_64 i)
simp' (SE_SExtend 16 64 (SE_Immediate i)) = SE_Immediate (sextend_16_64 i)
simp' (SE_SExtend 8  64 (SE_Immediate i)) = SE_Immediate (sextend_8_64 i)

simp' (SE_Op Minus si0 [SE_Immediate i0, SE_Op Minus _ [e1, SE_Immediate i1]]) = simp' $ SE_Op Minus si0 [SE_Immediate (i0+i1), e1] -- i0-(e1-i1) ==> (i0+i1)-e1
simp' (SE_Op Minus si0 [SE_Immediate i0, SE_Op Plus  _ [e1, SE_Immediate i1]]) = simp' $ SE_Op Minus si0 [SE_Immediate (i0-i1), e1] -- i0-(e1+i1) ==> (i0-i1)-e1
simp' (SE_Op Minus si0 [SE_Immediate i0, SE_Op Plus  _ [SE_Immediate i1, e1]]) = simp' $ SE_Op Minus si0 [SE_Immediate (i0-i1), e1] -- i0-(i1+e1) ==> (i0-i1)-e1
simp' (SE_Op Plus  si0 [SE_Immediate i0, SE_Op Minus _ [e1, SE_Immediate i1]]) = simp' $ SE_Op Plus  si0 [e1, SE_Immediate (i0-i1)] -- i0+(e1-i1) ==> e1+(i0-i1)
simp' (SE_Op Plus  si0 [SE_Immediate i0, SE_Op Plus  _ [e1, SE_Immediate i1]]) = simp' $ SE_Op Plus  si0 [e1, SE_Immediate (i0+i1)] -- i0+(e1+i1) ==> e1+(i0+i1)

simp' (SE_Op Minus si0 [e,SE_Immediate i]) = if testBit i 63 then SE_Op Plus si0 [simp' e,SE_Immediate (-i)] else SE_Op Minus si0 [simp' e,SE_Immediate i]
simp' (SE_Op Plus  si0 [e,SE_Immediate i]) = if testBit i 63 && i /= 0x8000000000000000 then SE_Op Minus si0 [simp' e,SE_Immediate (-i)] else SE_Op Plus  si0 [simp' e,SE_Immediate i]
simp' (SE_Op Plus  si0 [SE_Immediate i,e]) = if testBit i 63  && i /= 0x8000000000000000 then SE_Op Minus si0 [simp' e,SE_Immediate (-i)] else SE_Op Plus  si0 [simp' e,SE_Immediate i]


simp' (SE_Bit i (SE_Op Plus  si0 [e0, e1])) = simp' $ SE_Op Plus  si0 [SE_Bit i e0, SE_Bit i e1] -- b(e0+e1) = b(e0) + b(e1)
simp' (SE_Bit i (SE_Op Minus si0 [e0, e1])) = simp' $ SE_Op Minus si0 [SE_Bit i e0, SE_Bit i e1] -- b(e0-e1) = b(e0) - b(e1)
simp' (SE_Bit i (SE_Op Times si0 [e0, e1])) = simp' $ SE_Op Times si0 [SE_Bit i e0, SE_Bit i e1] -- b(e0*e1) = b(e0) * b(e1)
simp' (SE_Bit i (SE_Op And   si0 [e0, e1])) = simp' $ SE_Op And   si0 [SE_Bit i e0, SE_Bit i e1] -- b(e0&e1) = b(e0) & b(e1)
simp' (SE_Bit i (SE_Op Or    si0 [e0, e1])) = simp' $ SE_Op Or    si0 [SE_Bit i e0, SE_Bit i e1] -- b(e0|e1) = b(e0) | b(e1)
simp' (SE_Bit i (SE_Op Xor   si0 [e0, e1])) = simp' $ SE_Op Xor   si0 [SE_Bit i e0, SE_Bit i e1] -- b(e0 xor e1) = b(e0) xor b(e1)
simp' (SE_Bit i (SE_Op Not   si0 [e0]))     = simp' $ SE_Op Not   si0 [SE_Bit i e0]              -- b(not e0) = b(not e0)

simp' (SE_Op Xor si0 [e0,e1]) = if e0 == e1 then SE_Immediate 0 else SE_Op Xor si0 $ map simp' [e0,e1]

simp' (Bottom (FromNonDeterminism es)) = Bottom $ FromNonDeterminism $ NES.map simp' es
simp' (SE_Op op si0 es)                = SE_Op op si0 $ map simp' es
simp' (SE_StatePart sp)                = SE_StatePart $ simp'_sp sp
simp' (SE_Bit i e)                     = SE_Bit i $ simp' e
simp' (SE_SExtend l h e)               = SE_SExtend l h $ simp' e
simp' (SE_Overwrite i e0 e1)           = SE_Overwrite i (simp' e0) (simp' e1)
simp' (SE_Var sp)                      = SE_Var $ simp'_sp sp
simp' e                                = e

simp'_sp (SP_Mem e si) = SP_Mem (simp' e) si
simp'_sp sp            = sp




instance Show StatePart where
  show (SP_StackPointer f) = "RSP_" ++ f
  show (SP_Reg r)          = show r
  show (SP_Mem a si)       = "[" ++ show a ++ ", " ++ show si ++ "]"

-- | Symbolically represent the status of all flags in the current state
data FlagStatus = 
    None                                         -- ^ No information known, flags could have any value
  | FS_CMP (Maybe Bool) X86.Operand X86.Operand  -- ^ The flags are set by the x86 CMP instruction applied to the given operands.
  deriving (Generic,Eq,Ord)


instance Show FlagStatus where
 show None = ""
 show (FS_CMP Nothing      op1 op2) = "flags set by CMP(" ++ show op1 ++ "," ++ show op2 ++ ")"
 show (FS_CMP (Just True)  op1 op2) = show op1 ++ " < " ++ show op2
 show (FS_CMP (Just False) op1 op2) = show op1 ++ " >= " ++ show op2


instance Show PointerBase where
  show (StackPointer f)        = "StackPointer_" ++ f
  show (Malloc Nothing  _)     = "malloc()"
  show (Malloc (Just a) _)     = "malloc@" ++ showHex a ++ "()"
  show (GlobalAddress a)       = "GlobalAddress@" ++ showHex a
  show (PointerToSymbol a sym) = "PointerToSymbol_" ++ sym
  show ThreadLocalStorage      = "ThreadLocalStorage"




-- | Pretty print expression, showing Bottom expressions only as Bot
pp_expr = show_expr (\_ -> "")



instance Cereal.Serialize PointerBase
instance Cereal.Serialize BotTyp
instance Cereal.Serialize BotSrc
instance Cereal.Serialize StatePart
instance Cereal.Serialize Operator
instance Cereal.Serialize SimpleExpr
instance Cereal.Serialize FlagStatus


instance NFData PointerBase
instance NFData BotTyp
instance NFData BotSrc
instance NFData StatePart
instance NFData Operator
instance NFData SimpleExpr
instance NFData FlagStatus


