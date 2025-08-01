{-# LANGUAGE DeriveGeneric #-}


{-|
Module      : SimplePred
Description : A datatype for symbolic expressions and symbolic predicates.

A datatype for symbolic predicates, tailored to storing information
on equalities between the current values stored in state parts (lhs) 
and constant expressions (rhs).
-}

module Data.SymbolicExpression where

import Base
import Config

import Binary.Generic
import Data.Symbol
import Data.Size
import Data.X86.Register
import Data.X86.Instruction

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
import Data.Bits (testBit, (.|.), (.&.), xor, shiftL,shiftR)
import qualified Data.Serialize as Cereal hiding (get,put)


import Control.DeepSeq


-- | A pointerbase is a positive addend of a symbolic expression that may represent a pointer.
data PointerBase = 
    StackPointer                         -- ^ The stack frame of the given function
  | Malloc (Maybe Word64) (Maybe String) -- ^ A malloc (at the /heap/) at a given address (hash is unused for now)
  | GlobalAddress Word64                 -- ^ A /global/ address in the range of the sections of the binary.
  | ThreadLocalStorage                   -- ^ A pointer to the thread-local-storage (e.g., FS register)
  | BaseIsStatePart StatePart            -- ^ A statepart that is known to contain a pointer
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
  | RockBottom
 deriving (Eq, Ord, Generic)


-- | Sources that may be used to compute an expression. That is, the inputs to an expression.
data BotSrc = 
    Src_Var StatePart                        -- ^ An initial variable, i.e., a constant
  | Src_StackPointer                         -- ^ The stack pointer of the given function
  | Src_Malloc (Maybe Word64) (Maybe String) -- ^ A malloced address
  | Src_ImmediateAddress Word64              -- ^ An immediate used in the computation of the pointer
  | Src_Function String                      -- ^ A return value from a function
  | Src_Mem (NES.NESet BotSrc)               -- ^ reading from memory
  | Src_ImmediateConstants                   -- ^ Some immediate value
 deriving (Eq, Ord, Generic)




-- | An operator is a pure operation over bit-vectors, annotated with the bit-size of its operands.
-- For example, @Plus 64@ denotes 64-bit addition.
data Operator = 
    Minus | Plus | Times | And | Or | Xor | Not | Bsr 
  | SExtHi Int -- ^ The high part after sign-extension from $n$ bits, thus producing $n$-bit value
  | IMulLo | IMulHi
  | UdivHi | UdivLo | Udiv
  | SdivHi | SdivLo | Sdiv
  | Shl | Shr | Sar | Ror | Rol 
  | Bswap | Pextr | Sbb | Adc | Cmov | ZeroOne
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
  show (Shl     ) = "shl"     
  show (Shr     ) = "shr"     
  show (Sar     ) = "sar"     
  show (Ror     ) = "ror"     
  show (Rol     ) = "rol"
  show (Sbb     ) = "sbb"
  show (Adc     ) = "adc"
  show (Bswap   ) = "bswap"
  show (Pextr   ) = "pextr"
  show (Cmov    ) = "cmov"

  show (ZeroOne ) = "setxx"
  show (SExtHi b) = "sext_h"
  show (IMulLo  ) = "imul_l"    
  show (IMulHi  ) = "imul_h" 
  show (Udiv    ) = "udiv"    
  show (UdivLo  ) = "udiv_l"    
  show (UdivHi  ) = "udiv_h"    
  show (Sdiv    ) = "sdiv"
  show (SdivLo  ) = "sdiv_l" 
  show (SdivHi  ) = "sdiv_h"    

-- | A symbolic expression with as leafs either immediates, variables, live values of stateparts, or malloced addresses.
-- A variable is a constant representing some initial value, e.g., RDI_0, or [RSP_0,8]_0.
-- A statepart evaluates to its current value, e.g., RDI or [RSP,8].
data SimpleExpr =
    SE_Immediate Word64                        -- ^ An immediate word
  | SE_Var       StatePart                     -- ^ A variable representing the initial value stored in the statepart (e.g., RSP0)
  | SE_StatePart StatePart (Maybe String)      -- ^ The value stored currently in the statepart
  | SE_Malloc    (Maybe Word64) (Maybe String) -- ^ A malloc return value with possibly an ID
  | SE_Op        Operator Int [SimpleExpr]     -- ^ Application of an @`Operator`@ to the list of arguments
  | SE_Bit       Int SimpleExpr                -- ^ Taking the lower bits of a value
  | SE_SExtend   Int Int SimpleExpr            -- ^ Sign extension
  | SE_Overwrite Int SimpleExpr SimpleExpr     -- ^ Overwriting certain bits of a value with bits from another value
  | Bottom       BotTyp                        -- ^ Bottom (unknown value)
 deriving (Eq, Ord, Generic)

-- | A statepart is either a register or a region in memory
data StatePart =
    SP_Reg Register          -- ^ A register
  | SP_Mem SimpleExpr Int    -- ^ A region with a symbolic address and an immediate size.
 deriving (Eq, Ord, Generic)


instance Show BotSrc where
  show (Src_Var sp)               = show $ SE_Var sp
  show (Src_StackPointer)         = "RSP"
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

show_expr show_srcs (Bottom RockBottom)             = "TOP"
show_expr show_srcs (Bottom typ)                    = "Bot[" ++ show_bottyp show_srcs typ ++ "]"
show_expr show_srcs (SE_Malloc Nothing _)           = "malloc()" 
show_expr show_srcs (SE_Malloc (Just a) Nothing)    = "malloc@" ++ showHex a ++ "()" 
show_expr show_srcs (SE_Malloc (Just a) (Just id))  = id ++ "@" ++ showHex a
show_expr show_srcs (SE_Var sp)                     = show sp ++ "_0"
show_expr show_srcs (SE_Immediate i)                = if i > 2000 then "0x" ++ showHex i else show i
show_expr show_srcs (SE_StatePart sp Nothing)       = show sp
show_expr show_srcs (SE_StatePart sp (Just id))     = (show sp ++ "@@") ++ id
show_expr show_srcs (SE_Op (Plus ) _ [e0,e1])       = "(" ++ show_expr show_srcs e0 ++ " + "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (Minus) _ [e0,e1])       = "(" ++ show_expr show_srcs e0 ++ " - "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (Times) _ [e0,e1])       = "(" ++ show_expr show_srcs e0 ++ " * "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (And  ) _ [e0,e1])       = "(" ++ show_expr show_srcs e0 ++ " & "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (Or   ) _ [e0,e1])       = "(" ++ show_expr show_srcs e0 ++ " | "   ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op (Xor  ) _ [e0,e1])       = "(" ++ show_expr show_srcs e0 ++ " xor " ++ show_expr show_srcs e1 ++ ")"
show_expr show_srcs (SE_Op op si es)                = show op ++ show si ++ "(" ++ intercalate "," (map (show_expr show_srcs) es) ++ ")"
show_expr show_srcs (SE_Bit i a)                    = "b" ++ show i ++ "(" ++ show_expr show_srcs a ++ ")"
show_expr show_srcs (SE_SExtend l h a)              = "signextend(" ++ show l ++ "," ++ show h ++ ", " ++ show_expr show_srcs a ++ ")"
show_expr show_srcs (SE_Overwrite i a b)            = "overwrite(" ++ show i ++ "," ++ show_expr show_srcs a ++ "," ++ show_expr show_srcs b ++ ")"


instance Show SimpleExpr where
  show = show_expr show_srcs





-- | Returns true iff the expression is an immediate value
is_immediate (SE_Immediate _) = True
is_immediate _                = False


-- | Is the statepart memory?
is_mem_sp (SP_Reg _)        = False
is_mem_sp (SP_Mem a si)     = True

-- | Is the statepart a register?
is_reg_sp (SP_Reg _)        = True
is_reg_sp (SP_Mem a si)     = False


-- | Returns true iff the expression contains Bot
contains_bot (Bottom typ)         = True
contains_bot (SE_Malloc _ _)      = False
contains_bot (SE_Var sp)          = contains_bot_sp sp
contains_bot (SE_Immediate _)     = False
contains_bot (SE_StatePart sp _ ) = contains_bot_sp sp
contains_bot (SE_Op _ _ es)       = any contains_bot es
contains_bot (SE_Bit i e)         = contains_bot e
contains_bot (SE_SExtend _ _ e)   = contains_bot e
contains_bot (SE_Overwrite _ a b) = contains_bot a || contains_bot b

-- | Returns true iff the statepart contains Bot
contains_bot_sp (SP_Reg r)        = False
contains_bot_sp (SP_Mem a si)     = contains_bot a





-- | concatMap function p over all bottom values in the expression  
all_bot_satisfy :: (BotTyp -> Bool) -> SimpleExpr -> Bool
all_bot_satisfy p (Bottom typ)         = p typ
all_bot_satisfy p (SE_Malloc _ _)      = True
all_bot_satisfy p (SE_Var sp)          = all_bot_satisfy_sp p sp
all_bot_satisfy p (SE_Immediate _)     = True
all_bot_satisfy p (SE_StatePart sp _)  = all_bot_satisfy_sp p sp
all_bot_satisfy p (SE_Op _ _ es)       = all (all_bot_satisfy p) es
all_bot_satisfy p (SE_Bit i e)         = all_bot_satisfy p e
all_bot_satisfy p (SE_SExtend _ _ e)   = all_bot_satisfy p e
all_bot_satisfy p (SE_Overwrite _ a b) = and [all_bot_satisfy p a, all_bot_satisfy p b]

all_bot_satisfy_sp p (SP_Reg r)        = True
all_bot_satisfy_sp p (SP_Mem a si)     = all_bot_satisfy p a





-- | 
expr_size (Bottom typ)         = 1 + expr_size_bottyp typ
expr_size (SE_Malloc id _)     = 1
expr_size (SE_Var sp)          = expr_size_sp sp
expr_size (SE_Immediate _)     = 1
expr_size (SE_StatePart sp _)  = expr_size_sp sp
expr_size (SE_Op _ _ es)       = 1 + (sum $ map expr_size es)
expr_size (SE_Bit i e)         = 1 + expr_size e
expr_size (SE_SExtend l h e)   = 1 + expr_size e
expr_size (SE_Overwrite _ _ e) = 1 + expr_size e

expr_size_sp (SP_Reg r)        = 1
expr_size_sp (SP_Mem a si)     = 1 + expr_size a 

expr_size_bottyp (FromNonDeterminism es)        = sum $ NES.map expr_size es
expr_size_bottyp (FromPointerBases bs)          = NES.size bs
expr_size_bottyp (FromSources srcs)             = NES.size srcs
expr_size_bottyp (FromBitMode srcs)             = NES.size srcs
expr_size_bottyp (FromOverlap srcs)             = NES.size srcs
expr_size_bottyp (FromSemantics srcs)           = NES.size srcs
expr_size_bottyp (FromMemWrite srcs)            = NES.size srcs
expr_size_bottyp (FromUninitializedMemory srcs) = NES.size srcs
expr_size_bottyp (FromCall _)                   = 1


unfold_cmovs :: SimpleExpr -> [SimpleExpr]
unfold_cmovs (SE_Op Cmov si es)     = map simp $ concatMap unfold_cmovs es
unfold_cmovs (SE_Op op si es)       = map (SE_Op op si)    $ crossProduct (map unfold_cmovs es)
unfold_cmovs (SE_Bit n e)           = map (SE_Bit n)       $ unfold_cmovs e
unfold_cmovs (SE_SExtend l h e)     = map (SE_SExtend l h) $ unfold_cmovs e
unfold_cmovs (SE_Overwrite n e0 e1) = map (\[e0',e1'] -> SE_Overwrite n e0' e1') $ crossProduct (map unfold_cmovs [e0,e1])
unfold_cmovs e                      = [e]



-- | Simplification of symbolic expressions. 
--
-- Must always produce an expression logically equivalent to the original.
simp :: SimpleExpr -> SimpleExpr
simp e = 
  let e' = simp' e in
    if e == e' then reorder_addends e' else simp e'

simp' (SE_Bit i (SE_Bit i' e))           = SE_Bit (min i i') $ simp' e
simp' (SE_Bit i (SE_Overwrite i' e0 e1)) = if i <= i' then SE_Bit i (simp' e1) else SE_Bit i $ SE_Overwrite i' (simp' e0) (simp' e1)
simp' (SE_Bit i (SE_SExtend l h e))      
  | i == l    = simp' e
  | i == h    = SE_SExtend l h $ simp' e
  | otherwise = SE_Bit i $ SE_SExtend l h $ simp' e

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
simp' (SE_Op Times si0 [SE_Immediate 1, e1]) = e1
simp' (SE_Op Times si0 [e0, SE_Immediate 1]) = e0
simp' (SE_Op Udiv  si0 [SE_Immediate 0, e1]) = SE_Immediate 0
simp' (SE_Op Sdiv  si0 [SE_Immediate 0, e1]) = SE_Immediate 0
simp' (SE_Op Shl   si0 [SE_Immediate 0, e1]) = SE_Immediate 0
simp' (SE_Op Shr   si0 [SE_Immediate 0, e1]) = SE_Immediate 0
simp' (SE_Op Sar   si0 [SE_Immediate 0, e1]) = SE_Immediate 0
simp' (SE_Op And   si0 [SE_Immediate 0, e1]) = SE_Immediate 0 -- 0 & e1 = 0
simp' (SE_Op And   si0 [e0, SE_Immediate 0]) = SE_Immediate 0 -- e0 & 0 = 0
simp' (SE_Op Or    si0 [SE_Immediate 0, e1]) = simp' e1 -- 0 | e1 = e1
simp' (SE_Op Or    si0 [e0, SE_Immediate 0]) = simp' e0 -- e0 | 0 = e0
simp' (SE_Overwrite i (SE_Immediate 0) e)    = simp' $ SE_Bit i e


simp' (SE_Op Shl si0   [e,SE_Immediate i]) = simp' $ SE_Op Times si0 [e,SE_Immediate $ 2^i]

simp' (SE_Op Minus si0 [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 - i1)     -- Immediate: i0 - i1
simp' (SE_Op Plus  si0 [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 + i1)     -- Immediate: i0 + i1
simp' (SE_Op Times si0 [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 * i1)     -- Immediate: i0 * i1
simp' (SE_Op Or    si0 [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 .|. i1)   -- Immediate: i0 | i1
simp' (SE_Op And   si0 [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 .&. i1)   -- Immediate: i0 & i1
simp' (SE_Op Xor   si0 [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 `xor` i1) -- Immediate: i0 xor i1
simp' (SE_Op Udiv  64  [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0  `div` i1) 
simp' (SE_Op Udiv  32  [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (fromIntegral ((fromIntegral i0::Word32) `div` (fromIntegral i1::Word32)))
simp' (SE_Op Sdiv  64  [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (fromIntegral ((fromIntegral i0::Int64)  `div` (fromIntegral i1::Int64))) 
simp' (SE_Op Sdiv  32  [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (fromIntegral ((fromIntegral i0::Int32)  `div` (fromIntegral i1::Int32)))
simp' (SE_Op Shl   si0 [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 `shiftL` fromIntegral i1)
simp' (SE_Op Shr   si0 [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (i0 `shiftR` fromIntegral i1)
simp' (SE_Op Sar   64  [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (fromIntegral $ (fromIntegral i0::Int64) `shiftR` (fromIntegral i1))
simp' (SE_Op Sar   32  [SE_Immediate i0, SE_Immediate i1]) = SE_Immediate (fromIntegral $ (fromIntegral i0::Int32) `shiftR` (fromIntegral i1))



simp' e@(SE_Bit 64  e1@(SE_Malloc _ _))                = e1
simp' e@(SE_Bit si0 e1@(SE_StatePart (SP_Reg r) _))    = if byteSize (regSize r) * 8 == si0 then e1 else e
simp' e@(SE_Bit si0 e1@(SE_StatePart (SP_Mem _ si) _)) = if si*8 == si0 then simp' e1 else SE_Bit si0 $ simp' e1
simp' e@(SE_Bit si0 e1@(SE_Var (SP_Reg r)))            = if byteSize (regSize r) * 8 == si0 then e1 else e
simp' e@(SE_Bit si0 e1@(SE_Var (SP_Mem _ si)))         = if si*8 == si0 then simp' e1 else SE_Bit si0 $ simp' e1
simp' e@(SE_Bit si0 e1@(SE_Op (SExtHi b) _ [_]))       = if b==si0 then simp' e1 else SE_Bit si0 $ simp' e1
simp' e@(SE_Bit si0 e1@(SE_Op Cmov si1 es))            = SE_Op Cmov si1 $ map (simp' . SE_Bit si0) es


simp' (SE_Op SdivLo si0 es@[SE_Op (SExtHi b) _ [e0], e1,e2]) = if e0==e1 then simp' $ SE_Op Sdiv si0 [e0,e2] else SE_Op SdivLo si0 $ map simp' es


simp' (SE_Bit 128 (SE_Immediate i)) = SE_Immediate i
simp' (SE_Bit 64  (SE_Immediate i)) = SE_Immediate (i .&. 0xFFFFFFFFFFFFFFFF)
simp' (SE_Bit 32  (SE_Immediate i)) = SE_Immediate (i .&. 0x00000000FFFFFFFF)
simp' (SE_Bit 16  (SE_Immediate i)) = SE_Immediate (i .&. 0x000000000000FFFF)
simp' (SE_Bit 8   (SE_Immediate i)) = SE_Immediate (i .&. 0x00000000000000FF)

simp' (SE_SExtend 32 64 (SE_Immediate i)) = SE_Immediate (sextend_32_64 i)
simp' (SE_SExtend 16 64 (SE_Immediate i)) = SE_Immediate (sextend_16_64 i)
simp' (SE_SExtend 8  64 (SE_Immediate i)) = SE_Immediate (sextend_8_64 i)

--simp' (SE_Op Minus si0 [SE_Immediate i0, SE_Op Minus _ [e1, SE_Immediate i1]]) = simp' $ SE_Op Minus si0 [SE_Immediate (i0+i1), e1] -- i0-(e1-i1) ==> (i0+i1)-e1
--simp' (SE_Op Minus si0 [SE_Immediate i0, SE_Op Plus  _ [e1, SE_Immediate i1]]) = simp' $ SE_Op Minus si0 [SE_Immediate (i0-i1), e1] -- i0-(e1+i1) ==> (i0-i1)-e1
--simp' (SE_Op Minus si0 [SE_Immediate i0, SE_Op Plus  _ [SE_Immediate i1, e1]]) = simp' $ SE_Op Minus si0 [SE_Immediate (i0-i1), e1] -- i0-(i1+e1) ==> (i0-i1)-e1
--simp' (SE_Op Plus  si0 [SE_Immediate i0, SE_Op Minus _ [e1, SE_Immediate i1]]) = simp' $ SE_Op Plus  si0 [e1, SE_Immediate (i0-i1)] -- i0+(e1-i1) ==> e1+(i0-i1)
--simp' (SE_Op Plus  si0 [SE_Immediate i0, SE_Op Plus  _ [e1, SE_Immediate i1]]) = simp' $ SE_Op Plus  si0 [e1, SE_Immediate (i0+i1)] -- i0+(e1+i1) ==> e1+(i0+i1)

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

simp' (SE_Op Times si0 [SE_Op Times si1 [e1, SE_Immediate i1],SE_Immediate i0]) = SE_Op Times si0 [e1,SE_Immediate (i0*i1)]    -- (e1*i1)*i0 = e1*(i0*i1)



--simp' (SE_Op Plus  si0 es@[SE_Op Times _ [e0, SE_Immediate i0], SE_Op Times _ [e1, SE_Immediate i1]])
--  | e0 == e1  = SE_Op Times si0 [simp' e0, SE_Immediate (i0+i1)]                                                                        -- a*i0+a*i1 = a*(i0+i1)
--  | otherwise = SE_Op Plus si0 $ map simp' es
--simp' (SE_Op Minus si0 es@[e0,SE_Op Minus _ [e1, e2]])
--  | e0 == e1  = simp' e2                                                                                                                -- a-(a-b) = b
--  | otherwise = SE_Op Minus si0 $ map simp' es
--simp' (SE_Op Minus si0 es@[e0,SE_Op Plus _ [e1, e2]])
--  | e0 == e1  = simp' $ SE_Op Minus si0 [SE_Immediate 0,e2]                                                                             -- a-(a+b) = -b
--  | otherwise = SE_Op Minus si0 $ map simp' es


--simp' (SE_Op Plus  si0 [e0,e1]) = if e0 == e1 then simp' $ SE_Op Times si0 [e0,SE_Immediate 2] else SE_Op Plus si0 $ map simp' [e0,e1]  -- a + a   = a*2
simp' (SE_Op Xor   si0 [e0,e1]) = if e0 == e1 && not (contains_bot e0) then SE_Immediate 0 else SE_Op Xor si0 $ map simp' [e0,e1]                                -- a xor a = 0
--simp' (SE_Op Minus si0 [e0,e1]) = if e0 == e1 then SE_Immediate 0 else SE_Op Minus si0 $ map simp' [e0,e1]                              -- a - a   = 0




simp' (Bottom (FromNonDeterminism es)) = Bottom $ FromNonDeterminism $ NES.map simp' es
simp' (SE_Op op si0 es)                = SE_Op op si0 $ map simp' es
simp' (SE_StatePart sp id)             = SE_StatePart (simp'_sp sp) id
simp' (SE_Bit i e)                     = SE_Bit i $ simp' e
simp' (SE_SExtend l h e)               = SE_SExtend l h $ simp' e
simp' (SE_Overwrite i e0 e1)           = SE_Overwrite i (simp' e0) (simp' e1)
simp' (SE_Var sp)                      = SE_Var $ simp'_sp sp
simp' e                                = e

simp'_sp (SP_Mem e si) = SP_Mem (simp' e) si
simp'_sp sp            = sp




addends :: SimpleExpr -> M.Map SimpleExpr Word64
addends = gather_immediates . addends'
 where
  addends' (SE_Op Plus  _ es)                   = M.unionsWith (+) $ map addends' es
  addends' (SE_Op Minus _ (e0:es))              = M.unionsWith (+) [addends' e0,  M.map (\i -> 0-i) $ M.unionsWith (+) $ map addends' es]
  addends' (SE_Op Times _ [e,SE_Immediate imm]) = M.singleton e imm
  addends' (SE_Op Times _ [SE_Immediate imm,e]) = M.singleton e imm
  addends' e                                    = M.singleton e 1

  gather_immediates :: M.Map SimpleExpr Word64 -> M.Map SimpleExpr Word64
  gather_immediates m =
    let (imms,rem) = M.partitionWithKey entry_is_immediate m in
      if M.null imms then rem else M.insert (SE_Immediate $ gather_imms imms) 1 rem

  gather_imms :: M.Map SimpleExpr Word64 -> Word64
  gather_imms = M.foldrWithKey (\(SE_Immediate i) q -> ((+) (i*q))) 0

  entry_is_immediate (SE_Immediate _) _ = True
  entry_is_immediate _ _ = False


reorder_addends :: SimpleExpr -> SimpleExpr
reorder_addends e
  | contains_bot e = e
  | otherwise      = 
  let si        = get_size e
      m         = M.filter    ((/=) 0) $ addends e
      (pos,neg) = M.partition ((<) 0)  $ m in
    if M.null pos && M.null neg then
      SE_Immediate 0
    else if not (M.null pos) && M.null neg then
      simp' $ mk_expr si $ M.toList pos
    else if M.null pos && not (M.null neg) then
      simp' $ SE_Op Minus (fromJust si) [SE_Immediate 0, mk_expr si $ M.toList $ M.map ((\i -> 0-i)) neg]
    else
      simp' $ SE_Op Minus (fromJust si) [mk_expr si $ M.toList pos, mk_expr si $ M.toList $ M.map ((\i -> 0-i)) neg]
 where
  mk_expr si [(e,occ)]     = mk_addend si e occ
  mk_expr si ((e,occ):rem) = SE_Op Plus (fromJust si) [mk_addend si e occ, mk_expr si rem] 

  mk_addend si e occ
    | occ == 1 = e
    | occ  > 1 = SE_Op Times (fromJust si) [e,SE_Immediate occ]

  get_size (SE_Op Plus si es)  = Just si
  get_size (SE_Op Minus si es) = Just si
  get_size (SE_Op Times si es) = Just si
  get_size _                   = Nothing



instance Show StatePart where
  show (SP_Reg r)        = show r
  show (SP_Mem a si)     = "[" ++ show a ++ ", " ++ show si ++ "]"

-- | Symbolically represent the status of all flags in the current state
data FlagStatus = 
    FS_EQ Operand Operand -- ^ The two operands evlauet to equal values
  | FS_CMP (Maybe Bool) Operand Operand  -- ^ The flags are set by the x86 CMP instruction applied to the given operands.
  deriving (Generic,Eq,Ord)

is_FS_CMP (FS_CMP _ _ _) = True
is_FS_CMP _ = False

instance Show FlagStatus where
 show (FS_EQ op1 op2)  = show op1 ++ " == " ++ show op2
 show (FS_CMP Nothing      op1 op2) = "flags set by CMP(" ++ show op1 ++ "," ++ show op2 ++ ")"
 show (FS_CMP (Just True)  op1 op2) = show op1 ++ " < " ++ show op2
 show (FS_CMP (Just False) op1 op2) = show op1 ++ " >= " ++ show op2


instance Show PointerBase where
  show (StackPointer)        = "StackPointer"
  show (Malloc Nothing  _)   = "malloc()"
  show (Malloc (Just a) _)   = "malloc@" ++ showHex a ++ "()"
  show (GlobalAddress a)     = "GlobalAddress@" ++ showHex a
  show ThreadLocalStorage    = "ThreadLocalStorage"
  show (BaseIsStatePart sp)  = show sp




-- | Pretty print expression, showing Bottom expressions only as Bot
pp_expr = show_expr (\_ -> "")



instance Cereal.Serialize PointerBase
instance Cereal.Serialize BotTyp
instance Cereal.Serialize BotSrc
instance Cereal.Serialize StatePart
instance Cereal.Serialize Operator
instance Cereal.Serialize SimpleExpr
instance Cereal.Serialize FlagStatus


instance NFData Symbol
instance NFData PointerBase
instance NFData BotTyp
instance NFData BotSrc
instance NFData StatePart
instance NFData Operator
instance NFData SimpleExpr
instance NFData FlagStatus


