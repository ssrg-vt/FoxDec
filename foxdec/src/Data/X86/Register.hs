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
      | Reg128 Int
      | Reg256 Int
      | Reg512 Int
      | RegSeg SReg
      | RegFPU FPUReg
      | RegDebug Int
      | RegControl Int
      | RegMask Int
      | RegMM Int
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

  show (Reg8 R16 HalfL) = "R16B"
  show (Reg16 R16)      = "R16W"
  show (Reg32 R16)      = "R16D"
  show (Reg64 R16)      = "R16"

  show (Reg8 R17 HalfL) = "R17B"
  show (Reg16 R17)      = "R17W"
  show (Reg32 R17)      = "R17D"
  show (Reg64 R17)      = "R17"

  show (Reg8 R18 HalfL)  = "R18B"
  show (Reg16 R18)       = "R18W"
  show (Reg32 R18)       = "R18D"
  show (Reg64 R18)       = "R18"

  show (Reg8 R19 HalfL)  = "R19B"
  show (Reg16 R19)       = "R19W"
  show (Reg32 R19)       = "R19D"
  show (Reg64 R19)       = "R19"

  show (Reg8 R20 HalfL) = "R20B"
  show (Reg16 R20)      = "R20W"
  show (Reg32 R20)      = "R20D"
  show (Reg64 R20)      = "R20"

  show (Reg8 R21 HalfL) = "R21B"
  show (Reg16 R21)      = "R21W"
  show (Reg32 R21)      = "R21D"
  show (Reg64 R21)      = "R21"

  show (Reg8 R22 HalfL) = "R22B"
  show (Reg16 R22)      = "R22W"
  show (Reg32 R22)      = "R22D"
  show (Reg64 R22)      = "R22"

  show (Reg8 R23 HalfL) = "R23B"
  show (Reg16 R23)      = "R23W"
  show (Reg32 R23)      = "R23D"
  show (Reg64 R23)      = "R23"

  show (Reg8 R24 HalfL) = "R24B"
  show (Reg16 R24)      = "R24W"
  show (Reg32 R24)      = "R24D"
  show (Reg64 R24)      = "R24"

  show (Reg8 R25 HalfL) = "R25B"
  show (Reg16 R25)      = "R25W"
  show (Reg32 R25)      = "R25D"
  show (Reg64 R25)      = "R25"

  show (Reg8 R26 HalfL) = "R26B"
  show (Reg16 R26)      = "R26W"
  show (Reg32 R26)      = "R26D"
  show (Reg64 R26)      = "R26"

  show (Reg8 R27 HalfL) = "R27B"
  show (Reg16 R27)      = "R27W"
  show (Reg32 R27)      = "R27D"
  show (Reg64 R27)      = "R27"


  show (Reg8 R28 HalfL)  = "R28B"
  show (Reg16 R28)       = "R28W"
  show (Reg32 R28)       = "R28D"
  show (Reg64 R28)       = "R28"

  show (Reg8 R29 HalfL)  = "R29B"
  show (Reg16 R29)       = "R29W"
  show (Reg32 R29)       = "R29D"
  show (Reg64 R29)       = "R29"

  show (Reg8 R30 HalfL) = "R30B"
  show (Reg16 R30)      = "R30W"
  show (Reg32 R30)      = "R30D"
  show (Reg64 R30)      = "R30"

  show (Reg8 R31 HalfL) = "R31B"
  show (Reg16 R31)      = "R31W"
  show (Reg32 R31)      = "R31D"
  show (Reg64 R31)      = "R31"


  show (RegSeg r)       = show r
  show (RegFPU r)       = show r
  show (Reg128 idx)     = "XMM" ++ show idx
  show (Reg256 idx)     = "YMM" ++ show idx
  show (Reg512 idx)     = "ZMM" ++ show idx
  show (RegMM idx)      = "MM" ++ show idx
  show (RegControl idx) = "CR" ++ show idx
  show (RegDebug idx)   = "DR" ++ show idx
  show (RegMask idx)    = "K" ++ show idx
  show (RegTemp)        = "TEMP"


data GPR = RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 | R16 | R17 | R18 | R19 |
           R20 | R21 | R22 | R23 | R24 | R25 | R26 | R27 | R28 | R29 | R30 | R31 |
           RIP
    deriving (Show, Eq, Ord, Generic)

data SReg = ES | CS | SS | DS | FS | GS | SR6 | SR7
    deriving (Show, Eq, Ord, Generic)

data FPUReg = ST0 | ST1 | ST2 | ST3 | ST4 | ST5 | ST6 | ST7
    deriving (Show, Eq, Ord, Generic)

data RegHalf = HalfL | HalfH
    deriving (Show, Eq, Ord, Generic)



mk_mmx 0 = ST0
mk_mmx 1 = ST1
mk_mmx 2 = ST2
mk_mmx 3 = ST3
mk_mmx 4 = ST4
mk_mmx 5 = ST5
mk_mmx 6 = ST6
mk_mmx 7 = ST7

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
instance Cereal.Serialize GPR
instance Cereal.Serialize RegHalf
instance Cereal.Serialize Register

instance NFData SReg
instance NFData FPUReg
instance NFData GPR
instance NFData RegHalf
instance NFData Register






read_register "IPL" = (Reg8 RIP HalfL)
read_register "IP" = (Reg16 RIP)
read_register "EIP" = (Reg32 RIP)
read_register "RIP" = (Reg64 RIP)

read_register "SPL" = (Reg8 RSP HalfL)
read_register "SP" = (Reg16 RSP)
read_register "ESP" = (Reg32 RSP)
read_register "RSP" = (Reg64 RSP)

read_register "BPL" = (Reg8 RBP HalfL)
read_register "BP" = (Reg16 RBP)
read_register "EBP" = (Reg32 RBP)
read_register "RBP" = (Reg64 RBP)

read_register "AL" = (Reg8 RAX HalfL)
read_register "AH" = (Reg8 RAX HalfH)
read_register "AX" = (Reg16 RAX)
read_register "EAX" = (Reg32 RAX)
read_register "RAX" = (Reg64 RAX)

read_register "BL" = (Reg8 RBX HalfL)
read_register "BH" = (Reg8 RBX HalfH)
read_register "BX" = (Reg16 RBX)
read_register "EBX" = (Reg32 RBX)
read_register "RBX" = (Reg64 RBX)

read_register "CL" = (Reg8 RCX HalfL)
read_register "CH" = (Reg8 RCX HalfH)
read_register "CX" = (Reg16 RCX)
read_register "ECX" = (Reg32 RCX)
read_register "RCX" = (Reg64 RCX)

read_register "DL" = (Reg8 RDX HalfL)
read_register "DH" = (Reg8 RDX HalfH)
read_register "DX" = (Reg16 RDX)
read_register "EDX" = (Reg32 RDX)
read_register "RDX" = (Reg64 RDX)

read_register "DIL" = (Reg8 RDI HalfL)
read_register "DI" = (Reg16 RDI)
read_register "EDI" = (Reg32 RDI)
read_register "RDI" = (Reg64 RDI)

read_register "SIL" = (Reg8 RSI HalfL)
read_register "SI" = (Reg16 RSI)
read_register "ESI" = (Reg32 RSI)
read_register "RSI" = (Reg64 RSI)

read_register "R8B" = (Reg8 R8 HalfL)
read_register "R8W" = (Reg16 R8)
read_register "R8D" = (Reg32 R8)
read_register "R8" = (Reg64 R8)

read_register "R9B" = (Reg8 R9 HalfL)
read_register "R9W" = (Reg16 R9)
read_register "R9D" = (Reg32 R9)
read_register "R9" = (Reg64 R9)

read_register "R10B" = (Reg8 R10 HalfL)
read_register "R10W" = (Reg16 R10)
read_register "R10D" = (Reg32 R10)
read_register "R10" = (Reg64 R10)

read_register "R11B" = (Reg8 R11 HalfL)
read_register "R11W" = (Reg16 R11)
read_register "R11D" = (Reg32 R11)
read_register "R11" = (Reg64 R11)

read_register "R12B" = (Reg8 R12 HalfL)
read_register "R12W" = (Reg16 R12)
read_register "R12D" = (Reg32 R12)
read_register "R12" = (Reg64 R12)

read_register "R13B" = (Reg8 R13 HalfL)
read_register "R13W" = (Reg16 R13)
read_register "R13D" = (Reg32 R13)
read_register "R13" = (Reg64 R13)

read_register "R14B" = (Reg8 R14 HalfL)
read_register "R14W" = (Reg16 R14)
read_register "R14D" = (Reg32 R14)
read_register "R14" = (Reg64 R14)

read_register "R15B" = (Reg8 R15 HalfL)
read_register "R15W" = (Reg16 R15)
read_register "R15D" = (Reg32 R15)
read_register "R15" = (Reg64 R15)

read_register "R16B" = (Reg8 R16 HalfL)
read_register "R16W" = (Reg16 R16)
read_register "R16D" = (Reg32 R16)
read_register "R16" = (Reg64 R16)

read_register "R17B" = (Reg8 R17 HalfL)
read_register "R17W" = (Reg16 R17)
read_register "R17D" = (Reg32 R17)
read_register "R17" = (Reg64 R17)

read_register "R18B" = (Reg8 R18 HalfL)
read_register "R18W" = (Reg16 R18)
read_register "R18D" = (Reg32 R18)
read_register "R18" = (Reg64 R18)

read_register "R19B" = (Reg8 R19 HalfL)
read_register "R19W" = (Reg16 R19)
read_register "R19D" = (Reg32 R19)
read_register "R19" = (Reg64 R19)

read_register "R20B" = (Reg8 R20 HalfL)
read_register "R20W" = (Reg16 R20)
read_register "R20D" = (Reg32 R20)
read_register "R20" = (Reg64 R20)

read_register "R21B" = (Reg8 R21 HalfL)
read_register "R21W" = (Reg16 R21)
read_register "R21D" = (Reg32 R21)
read_register "R21" = (Reg64 R21)

read_register "R22B" = (Reg8 R22 HalfL)
read_register "R22W" = (Reg16 R22)
read_register "R22D" = (Reg32 R22)
read_register "R22" = (Reg64 R22)

read_register "R23B" = (Reg8 R23 HalfL)
read_register "R23W" = (Reg16 R23)
read_register "R23D" = (Reg32 R23)
read_register "R23" = (Reg64 R23)

read_register "R24B" = (Reg8 R24 HalfL)
read_register "R24W" = (Reg16 R24)
read_register "R24D" = (Reg32 R24)
read_register "R24" = (Reg64 R24)

read_register "R25B" = (Reg8 R25 HalfL)
read_register "R25W" = (Reg16 R25)
read_register "R25D" = (Reg32 R25)
read_register "R25" = (Reg64 R25)

read_register "R26B" = (Reg8 R26 HalfL)
read_register "R26W" = (Reg16 R26)
read_register "R26D" = (Reg32 R26)
read_register "R26" = (Reg64 R26)

read_register "R27B" = (Reg8 R27 HalfL)
read_register "R27W" = (Reg16 R27)
read_register "R27D" = (Reg32 R27)
read_register "R27" = (Reg64 R27)

read_register "R28B" = (Reg8 R28 HalfL)
read_register "R28W" = (Reg16 R28)
read_register "R28D" = (Reg32 R28)
read_register "R28" = (Reg64 R28)

read_register "R29B" = (Reg8 R29 HalfL)
read_register "R29W" = (Reg16 R29)
read_register "R29D" = (Reg32 R29)
read_register "R29" = (Reg64 R29)

read_register "R30B" = (Reg8 R30 HalfL)
read_register "R30W" = (Reg16 R30)
read_register "R30D" = (Reg32 R30)
read_register "R30" = (Reg64 R30)

read_register "R31B" = (Reg8 R31 HalfL)
read_register "R31W" = (Reg16 R31)
read_register "R31D" = (Reg32 R31)
read_register "R31" = (Reg64 R31)

read_register "ES" = RegSeg ES
read_register "CS" = RegSeg CS
read_register "SS" = RegSeg SS
read_register "DS" = RegSeg DS
read_register "FS" = RegSeg FS
read_register "GS" = RegSeg GS
read_register "SR6" = RegSeg SR6
read_register "SR7" = RegSeg SR7

read_register "XMM0" = Reg128 0
read_register "XMM1" = Reg128 1
read_register "XMM2" = Reg128 2 
read_register "XMM3" = Reg128 3
read_register "XMM4" = Reg128 4
read_register "XMM5" = Reg128 5
read_register "XMM6" = Reg128 6
read_register "XMM7" = Reg128 7
read_register "XMM8" = Reg128 8
read_register "XMM9" = Reg128 9
read_register "XMM10" = Reg128 10
read_register "XMM11" = Reg128 11
read_register "XMM12" = Reg128 12
read_register "XMM13" = Reg128 13
read_register "XMM14" = Reg128 14
read_register "XMM15" = Reg128 15
read_register "XMM16" = Reg128 16
read_register "XMM17" = Reg128 17
read_register "XMM18" = Reg128 18
read_register "XMM19" = Reg128 19
read_register "XMM20" = Reg128 20
read_register "XMM21" = Reg128 21
read_register "XMM22" = Reg128 22
read_register "XMM23" = Reg128 23
read_register "XMM24" = Reg128 24
read_register "XMM25" = Reg128 25
read_register "XMM26" = Reg128 26
read_register "XMM27" = Reg128 27
read_register "XMM28" = Reg128 28
read_register "XMM29" = Reg128 29
read_register "XMM30" = Reg128 30
read_register "XMM31" = Reg128 31

read_register "YMM0" = Reg256 0
read_register "YMM1" = Reg256 1
read_register "YMM2" = Reg256 2 
read_register "YMM3" = Reg256 3
read_register "YMM4" = Reg256 4
read_register "YMM5" = Reg256 5
read_register "YMM6" = Reg256 6
read_register "YMM7" = Reg256 7
read_register "YMM8" = Reg256 8
read_register "YMM9" = Reg256 9
read_register "YMM10" = Reg256 10
read_register "YMM11" = Reg256 11
read_register "YMM12" = Reg256 12
read_register "YMM13" = Reg256 13
read_register "YMM14" = Reg256 14
read_register "YMM15" = Reg256 15
read_register "YMM16" = Reg256 16
read_register "YMM17" = Reg256 17
read_register "YMM18" = Reg256 18
read_register "YMM19" = Reg256 19
read_register "YMM20" = Reg256 20
read_register "YMM21" = Reg256 21
read_register "YMM22" = Reg256 22
read_register "YMM23" = Reg256 23
read_register "YMM24" = Reg256 24
read_register "YMM25" = Reg256 25
read_register "YMM26" = Reg256 26
read_register "YMM27" = Reg256 27
read_register "YMM28" = Reg256 28
read_register "YMM29" = Reg256 29
read_register "YMM30" = Reg256 30
read_register "YMM31" = Reg256 31

read_register "ZMM0" = Reg512 0
read_register "ZMM1" = Reg512 1
read_register "ZMM2" = Reg512 2 
read_register "ZMM3" = Reg512 3
read_register "ZMM4" = Reg512 4
read_register "ZMM5" = Reg512 5
read_register "ZMM6" = Reg512 6
read_register "ZMM7" = Reg512 7
read_register "ZMM8" = Reg512 8
read_register "ZMM9" = Reg512 9
read_register "ZMM10" = Reg512 10
read_register "ZMM11" = Reg512 11
read_register "ZMM12" = Reg512 12
read_register "ZMM13" = Reg512 13
read_register "ZMM14" = Reg512 14
read_register "ZMM15" = Reg512 15
read_register "ZMM16" = Reg512 16
read_register "ZMM17" = Reg512 17
read_register "ZMM18" = Reg512 18
read_register "ZMM19" = Reg512 19
read_register "ZMM20" = Reg512 20
read_register "ZMM21" = Reg512 21
read_register "ZMM22" = Reg512 22
read_register "ZMM23" = Reg512 23
read_register "ZMM24" = Reg512 24
read_register "ZMM25" = Reg512 25
read_register "ZMM26" = Reg512 26
read_register "ZMM27" = Reg512 27
read_register "ZMM28" = Reg512 28
read_register "ZMM29" = Reg512 29
read_register "ZMM30" = Reg512 30
read_register "ZMM31" = Reg512 31

read_register "K0" = RegMask 0
read_register "K1" = RegMask 1
read_register "K2" = RegMask 2 
read_register "K3" = RegMask 3
read_register "K4" = RegMask 4
read_register "K5" = RegMask 5
read_register "K6" = RegMask 6
read_register "K7" = RegMask 7

read_register "DR0" = RegDebug 0
read_register "DR1" = RegDebug 1
read_register "DR2" = RegDebug 2
read_register "DR3" = RegDebug 3
read_register "DR4" = RegDebug 4
read_register "DR5" = RegDebug 5
read_register "DR6" = RegDebug 6
read_register "DR7" = RegDebug 7

read_register "CR0" = RegControl 0
read_register "CR1" = RegControl 1
read_register "CR2" = RegControl 2
read_register "CR3" = RegControl 3
read_register "CR4" = RegControl 4
read_register "CR5" = RegControl 5
read_register "CR6" = RegControl 6
read_register "CR7" = RegControl 7
read_register "CR8" = RegControl 8
read_register "CR9" = RegControl 9
read_register "CR10" = RegControl 10
read_register "CR11" = RegControl 11
read_register "CR12" = RegControl 12
read_register "CR13" = RegControl 13
read_register "CR14" = RegControl 14
read_register "CR15" = RegControl 15


read_register _ = RegNone
