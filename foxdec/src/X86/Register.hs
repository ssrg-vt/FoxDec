{-# LANGUAGE DeriveGeneric #-}

module X86.Register (Register(..), reg8, reg16, reg32, reg64, reg80, reg128, reg256, size, real) where

import GHC.Generics (Generic)
import qualified Data.Serialize as Cereal

data Register = InvalidRegister
  | RIP | EIP
  | RAX | EAX | AX | AH | AL
  | RBX | EBX | BX | BH | BL
  | RCX | ECX | CX | CH | CL
  | RDX | EDX | DX | DH | DL
  | RDI | EDI | DI | DIL
  | RSI | ESI | SI | SIL
  | RSP | ESP | SP | SPL
  | RBP | EBP | BP | BPL
  | R15 | R15D | R15W | R15B
  | R14 | R14D | R14W | R14B
  | R13 | R13D | R13W | R13B
  | R12 | R12D | R12W | R12B
  | R11 | R11D | R11W | R11B
  | R10 | R10D | R10W | R10B
  | R9 | R9D | R9W | R9B
  | R8 | R8D | R8W | R8B
  | CS | DS | ES | FS | GS | SS
  | EIZ | RIZ
  | ST0 | ST1 | ST2 | ST3 | ST4 | ST5 | ST6 | ST7
  | YMM0 | YMM1 | YMM2 | YMM3 | YMM4 | YMM5 | YMM6 | YMM7 | YMM8 | YMM9 | YMM10 | YMM11 | YMM12 | YMM13 | YMM14 | YMM15
  | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 | XMM8 | XMM9 | XMM10 | XMM11 | XMM12 | XMM13 | XMM14 | XMM15
  deriving (Show,Eq,Read,Ord,Generic,Enum)

instance Cereal.Serialize Register


-- | List of 256 bit registers
reg256 :: [Register]
reg256 = [YMM0,YMM1,YMM2,YMM3,YMM4,YMM5,YMM6,YMM7,YMM8,YMM9,YMM10,YMM11,YMM12,YMM13,YMM14,YMM15]
-- | List of 128 bit registers
reg128 :: [Register]
reg128 = [XMM0,XMM1,XMM2,XMM3,XMM4,XMM5,XMM6,XMM7,XMM8,XMM9,XMM10,XMM11,XMM12,XMM13,XMM14,XMM15]
-- | List of 80 bit registers
reg80 :: [Register]
reg80  = [ST0,ST1,ST2,ST3,ST4,ST5,ST6,ST7]
reg64 :: [Register]
-- | List of 64 bit registers
reg64  = [RAX, RBX, RCX, RDX, RSI, RDI, RSP, RBP, R8, R9, R10, R11, R12, R13, R14, R15, RIP,CS,DS,ES,FS,GS,SS]
-- | List of 32 bit registers
reg32 :: [Register]
reg32  = [EAX, EBX, ECX, EDX, ESI, EDI, ESP, EBP, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D]
-- | List of 16 bit registers
reg16 :: [Register]
reg16  = [AX,BX,CX,DX,SI,DI,SP,BP,R8W,R9W,R10W,R11W,R12W,R13W,R14W,R15W]
-- | List of 8 bit registers
reg8 :: [Register]
reg8   = [AL,BL,CL,DL,SIL,DIL,SPL,BPL,R8B,R9B,R10B,R11B,R12B,R13B,R14B,R15B]

-- | The size of the given register, in bytes.
size :: Num p => Register -> p
size r
  | r `elem` reg256 = 32
  | r `elem` reg128 = 16
  | r `elem` reg80 = 10
  | r `elem` reg64 = 8
  | r `elem` reg32 = 4
  | r `elem` reg16 = 2
  | r `elem` reg8 ++ [AH,BH,CH,DH] = 1
  | otherwise = error $ "Size of " ++ show r ++ " unknown"


-- | Matches register names to the real registers
-- E.g.: EAX is actually a part of RAX
real :: Register -> Register
real EAX = RAX
real EBX = RBX
real ECX = RCX
real EDX = RDX
real ESI = RSI
real EDI = RDI
real ESP = RSP
real EBP = RBP
real R8D = R8
real R9D = R9
real R10D = R10
real R11D = R11
real R12D = R12
real R13D = R13
real R14D = R14
real R15D = R15

real AX = RAX
real BX = RBX
real CX = RCX
real DX = RDX
real SI = RSI
real DI = RDI
real SP = RSP
real BP = RBP
real R8W = R8
real R9W = R9
real R10W = R10
real R11W = R11
real R12W = R12
real R13W = R13
real R14W = R14
real R15W = R15

real AL = RAX
real BL = RBX
real CL = RCX
real DL = RDX
real SIL = RSI
real DIL = RDI
real SPL = RSP
real BPL = RBP
real R8B = R8
real R9B = R9
real R10B = R10
real R11B = R11
real R12B = R12
real R13B = R13
real R14B = R14
real R15B = R15

real XMM0 = YMM0
real XMM1 = YMM1
real XMM2 = YMM2
real XMM3 = YMM3
real XMM4 = YMM4
real XMM5 = YMM5
real XMM6 = YMM6
real XMM7 = YMM7
real XMM8 = YMM8
real XMM9 = YMM9
real XMM10 = YMM10
real XMM11 = YMM11
real XMM12 = YMM12
real XMM13 = YMM13
real XMM14 = YMM14
real XMM15 = YMM15

real ST0 = ST0
real ST1 = ST1
real ST2 = ST2
real ST3 = ST3
real ST4 = ST4
real ST5 = ST5
real ST6 = ST6
real ST7 = ST7

real AH = RAX
real BH = RBX
real CH = RCX
real DH = RDX
real r = if r `elem` (reg64 ++ reg256) then r else error $ "Cannot match register " ++ show r ++ " to real register"


