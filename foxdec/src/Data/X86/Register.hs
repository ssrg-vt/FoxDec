{-# LANGUAGE DeriveGeneric #-}

module Data.X86.Register where

import Data.Size

import qualified Data.Serialize as Cereal hiding (get,put)
import Control.DeepSeq
import GHC.Generics

data Register =
        RegNone
      | Reg8 GPR RegHalf
      | Reg16 GPR
      | Reg32 GPR
      | Reg64 GPR
      | Reg128 SSEReg
      | RegSeg SReg
      | RegFPU FPUReg
      | RegTemp
    deriving (Eq, Ord, Generic)

instance Show Register where
  show RegNone          = "NONE"

  show (Reg8 RIP HalfL) = "IPL"
  show (Reg16 RIP)      = "IP"
  show (Reg32 RIP)      = "EIP"
  show (Reg64 RIP)      = "RIP"

  show (Reg8 RAX HalfL) = "AL"
  show (Reg8 RAX HalfH) = "AH"
  show (Reg16 RAX)      = "AX"
  show (Reg32 RAX)      = "EAX"
  show (Reg64 RAX)      = "RAX"

  show (Reg8 RBX HalfL) = "BL"
  show (Reg8 RBX HalfH) = "BH"
  show (Reg16 RBX)      = "BX"
  show (Reg32 RBX)      = "EBX"
  show (Reg64 RBX)      = "RBX"

  show (Reg8 RCX HalfL) = "CL"
  show (Reg8 RCX HalfH) = "CH"
  show (Reg16 RCX)      = "CX"
  show (Reg32 RCX)      = "ECX"
  show (Reg64 RCX)      = "RCX"

  show (Reg8 RDX HalfL) = "DL"
  show (Reg8 RDX HalfH) = "DH"
  show (Reg16 RDX)      = "DX"
  show (Reg32 RDX)      = "EDX"
  show (Reg64 RDX)      = "RDX"

  show (Reg8 RSP HalfL) = "SPL"
  show (Reg16 RSP)      = "SP"
  show (Reg32 RSP)      = "ESP"
  show (Reg64 RSP)      = "RSP"

  show (Reg8 RBP HalfL) = "BPL"
  show (Reg16 RBP)      = "BP"
  show (Reg32 RBP)      = "EBP"
  show (Reg64 RBP)      = "RBP"

  show (Reg8 RDI HalfL) = "DIL"
  show (Reg16 RDI)      = "DI"
  show (Reg32 RDI)      = "EDI"
  show (Reg64 RDI)      = "RDI"

  show (Reg8 RSI HalfL) = "SIL"
  show (Reg16 RSI)      = "SI"
  show (Reg32 RSI)      = "ESI"
  show (Reg64 RSI)      = "RSI"

  show (Reg8 R8 HalfL)  = "R8B"
  show (Reg16 R8)       = "R8W"
  show (Reg32 R8)       = "R8D"
  show (Reg64 R8)       = "R8"

  show (Reg8 R9 HalfL)  = "R9B"
  show (Reg16 R9)       = "R9W"
  show (Reg32 R9)       = "R9D"
  show (Reg64 R9)       = "R9"

  show (Reg8 R10 HalfL) = "R10B"
  show (Reg16 R10)      = "R10W"
  show (Reg32 R10)      = "R10D"
  show (Reg64 R10)      = "R10"

  show (Reg8 R11 HalfL) = "R11B"
  show (Reg16 R11)      = "R11W"
  show (Reg32 R11)      = "R11D"
  show (Reg64 R11)      = "R11"

  show (Reg8 R12 HalfL) = "R12B"
  show (Reg16 R12)      = "R12W"
  show (Reg32 R12)      = "R12D"
  show (Reg64 R12)      = "R12"

  show (Reg8 R13 HalfL) = "R13B"
  show (Reg16 R13)      = "R13W"
  show (Reg32 R13)      = "R13D"
  show (Reg64 R13)      = "R13"

  show (Reg8 R14 HalfL) = "R14B"
  show (Reg16 R14)      = "R14W"
  show (Reg32 R14)      = "R14D"
  show (Reg64 R14)      = "R14"

  show (Reg8 R15 HalfL) = "R15B"
  show (Reg16 R15)      = "R15W"
  show (Reg32 R15)      = "R15D"
  show (Reg64 R15)      = "R15"

  show (RegSeg r)       = show r
  show (RegFPU r)       = show r
  show (Reg128 r)       = show r
  show (RegTemp)        = "TEMP"



data GPR = RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 | RIP
    deriving (Show, Eq, Ord, Generic)

data SReg = ES | CS | SS | DS | FS | GS | SR6 | SR7
    deriving (Show, Eq, Ord, Generic)

data FPUReg = ST0 | ST1 | ST2 | ST3 | ST4 | ST5 | ST6 | ST7
    deriving (Eq, Ord, Generic)

instance Show FPUReg where
    show ST0 = "ST(0)"
    show ST1 = "ST(1)"
    show ST2 = "ST(2)"
    show ST3 = "ST(3)"
    show ST4 = "ST(4)"
    show ST5 = "ST(5)"
    show ST6 = "ST(6)"
    show ST7 = "ST(7)"

data SSEReg = XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 | XMM8 | XMM9 | XMM10 | XMM11 | XMM12 | XMM13 | XMM14 | XMM15
    deriving (Show, Eq, Ord, Generic)

data RegHalf = HalfL | HalfH
    deriving (Show, Eq, Ord, Generic)



real_reg (Reg8 r _) = Reg64 r
real_reg (Reg16 r)  = Reg64 r
real_reg (Reg32 r)  = Reg64 r
real_reg r          = r

regSize (Reg8 _ _) = ByteSize 1
regSize (Reg16 _) = ByteSize 2
regSize (Reg32 _) = ByteSize 4
regSize (Reg64 _) = ByteSize 8
regSize (Reg128 _) = ByteSize 16
regSize (RegSeg _) = ByteSize 8
regSize (RegTemp) = ByteSize 8
regSize (RegFPU _) = ByteSize 10
regSize r = error $ show r



instance Cereal.Serialize SReg
instance Cereal.Serialize FPUReg
instance Cereal.Serialize SSEReg
instance Cereal.Serialize GPR
instance Cereal.Serialize RegHalf
instance Cereal.Serialize Register

instance NFData SReg
instance NFData FPUReg
instance NFData SSEReg
instance NFData GPR
instance NFData RegHalf
instance NFData Register
