-- This file is a modification of the code found at:
-- https://github.com/ab9rf/disasm86
--
-- The license is the BSD-3-Clause license
-- The modification includes support for SSE/SSE2 instructions.


{--
Copyright Kelly Kinkade (c) 2016

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Kelly Kinkade nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--}


module Disassembler.Disassembler
    (
          disassemble0
    ) where

import Base

import Data.Size
import Data.X86.Opcode
import Data.X86.Instruction
import Data.X86.Register

import Control.Monad (join)
import Control.Monad.Extra (whenM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word64, Word32, Word16, Word8)
import Data.Int (Int64)
import Data.Bits
import Data.List (find)
import Data.Maybe (isJust, isNothing, fromJust, listToMaybe, mapMaybe, catMaybes, fromMaybe, maybeToList)
import Control.Applicative ( (<|>) )
import Data.Attoparsec.ByteString.Lazy hiding (take)

import qualified Data.Map.Lazy as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.Binary.Get as G
import Data.Attoparsec.ByteString.Lazy as A

import Data.Char (ord, toUpper)

import Debug.Trace

show_bytestring = showHex_list . B.unpack


disassemble0 :: Word64 -> ByteString -> Maybe (Instruction)
disassemble0 a s = 
  case parse (disassemble1 a) s of 
    Fail _ _ _   -> Nothing  -- trace ("Fail@ " ++ showHex a ++ ": " ++ show_bytestring s) $ Nothing
    Done r (b,i) -> -- trace ("Disassembled: " ++ show (i {inAddress = a, inSize = BS.length b})) $ 
                    Just $ i {inAddress = a, inSize = BS.length b}


disassemble :: Word64 -> ByteString -> ([Instruction], ByteString)
disassemble ofs s = case (parse (disassemble1 ofs) s) of
                    Fail _ _ _   -> ([], s)
                    Done r (b,i) -> let l = fromIntegral (BS.length b)
                                        (i',r') = disassemble (ofs+l) r
                                      in (i:i', r')

disassemble1 :: Word64 -> Parser (BS.ByteString, Instruction)
disassemble1 ofs = let st = dsInitial ofs
                       res = evalStateT disassemble1' st
                     in match res

type Disassembler a = StateT DisassemblerState Parser a

data DisassemblerState = DisassemblerState {
      dsOffset :: Word64
    , dsLock :: Bool
    , dsRepNE :: Bool
    , dsRep :: Bool
    , dsSegOverride :: Maybe SReg
    , dsRex :: Maybe Word8
    , dsOpWidthOverride :: Bool
    , dsAdWidthOverride :: Bool
    , dsOpWidth :: Int
    , dsAdWidth :: Int
    , dsModRM :: Maybe ModRM
    , dsImmed :: Maybe Operand
    , dsMoffset :: Maybe Operand
}

dsInitial ofs = DisassemblerState { dsOffset = ofs,
                                    dsLock = False, dsRepNE = False, dsRep = False,
                                    dsSegOverride = Nothing,
                                    dsRex = Nothing,
                                    dsOpWidthOverride = False,
                                    dsAdWidthOverride = False,
                                    dsOpWidth = 32, dsAdWidth = 64,
                                    dsModRM = Nothing, dsImmed = Nothing, dsMoffset = Nothing
                                  }

adv :: Int -> Disassembler ()
adv n = modify (\x -> x { dsOffset = (dsOffset x) + (fromIntegral n) })

mask :: Word8 -> Word8 -> Disassembler Word8
mask m v = (lift $ satisfy (\x -> (x .&. m) == v)) <* adv 1

opcode :: Word8 -> Disassembler Word8
opcode v = (lift $ word8 v) <* adv 1

disassemble1' :: Disassembler Instruction
disassemble1' = choice [
--            option () (many' segover >> pure ()) >> option () (mask 0xf1 0x40 >> pure()) >> opcode 0x90 >> instr "nop" []
            endbr64,
            general
          ]

endbr64 = do
  bs <- readBytes 4
  if B.unpack bs == [0xf3,0x0f,0x1e,0xfa] then -- TODO MOVE
    instr "endbr64" [] 
  else
    fail "endbr64"

general = do many' (choice [lockrep, segover, opsize, adsize])
             option () rex
             baseOpcode

lockrep :: Disassembler ()
lockrep = choice [ opcode 0xf0 >> modify (\x -> x { dsLock = True }),
                   opcode 0xf2 >> modify (\x -> x { dsRepNE = True }),
                   opcode 0xf3 >> modify (\x -> x { dsRep = True }) ]

segover :: Disassembler ()
segover = choice [
                    opcode 0x2e >> modify (\x -> x { dsSegOverride = Just CS }),
                    opcode 0x36 >> modify (\x -> x { dsSegOverride = Just SS }),
                    opcode 0x3e >> modify (\x -> x { dsSegOverride = Just DS }),
                    opcode 0x26 >> modify (\x -> x { dsSegOverride = Just ES }),
                    opcode 0x64 >> modify (\x -> x { dsSegOverride = Just FS }),
                    opcode 0x65 >> modify (\x -> x { dsSegOverride = Just GS })
                  ]

opsize :: Disassembler ()
opsize = opcode 0x66 >> modify ( \x -> x { dsOpWidthOverride = True } )

adsize :: Disassembler ()
adsize = opcode 0x67 >> modify ( \x -> x { dsAdWidthOverride = True } )

rex :: Disassembler ()
rex = do o <- mask 0xf0 0x40
         modify (\st -> st { dsRex = Just o })
         pure ()

baseOpcode :: Disassembler Instruction
baseOpcode = choice [
                fail "dunsel"
-- 0x00
                , do i <- mask 0xc7 0x00; opWidthB; modrm; instr (ext1A' i) [modrm_rm, modrm_reg]
                , do i <- mask 0xc7 0x01; opWidthW; modrm; instr (ext1A' i) [modrm_rm, modrm_reg]
                , do i <- mask 0xc7 0x02; opWidthB; modrm; instr (ext1A' i) [modrm_reg, modrm_rm]
                , do i <- mask 0xc7 0x03; opWidthW; modrm; instr (ext1A' i) [modrm_reg, modrm_rm]
                , do i <- mask 0xc7 0x04; opWidthB;   imm; instr (ext1A' i) [accum, immed]
                , do i <- mask 0xc7 0x05; opWidthW;   imm; instr (ext1A' i) [accum, immed]
-- 0x40 (REX prefixes handled above)
-- 0x50
                , do r <- mask 0xf8 0x50; opWidthX' 64 16; instr "push" [reg (r .&. 0x07) id]
                , do r <- mask 0xf8 0x58; opWidthX' 64 16; instr "pop" [reg (r .&. 0x07) id]
-- 0x60
                , opcode 0x63 >> opWidthW >> modrm >> instr "movsxd" [modrm_reg, opWidthF 32 >> modrm_rm]
                , opcode 0x68 >> opWidthX' 32 16 >> imm >> instr "push" [immed]
                , opcode 0x69 >> modrm >> opWidthW >> imm >> instr "imul" [modrm_reg, modrm_rm, immed]
                , opcode 0x6a >> opWidthB >> imm >> instr "push" [immed]
                , opcode 0x6b >> opWidthW >> modrm >> immB >> instr "imul" [modrm_reg, modrm_rm, immed]
                , opcode 0x6c >> instr "insb" []
                , opcode 0x6d >> forkX (fail "invalid") (instr "insd" []) (instr "insw" [])
                , opcode 0x6e >> instr "outsb" []
                , opcode 0x6f >> forkX (instr "outsq" []) (instr "outsd" []) (instr "outsw" [])
-- 0x70
                , do i <- mask 0xf0 0x70; d <- displ; instr (shortjmp i) [pure d]
-- 0x80
                , do opcode 0x80; opWidthB; modrm; i <- modopcode; imm; instr (ext1A i) [modrm_rm, immed]
                , do opcode 0x81; opWidthW; modrm; i <- modopcode; imm; instr (ext1A i) [modrm_rm, immed]
                , do opcode 0x83; opWidthW; modrm; i <- modopcode; immB; instr (ext1A i) [modrm_rm, immed]
                , opcode 0x84 >> opWidthB >> modrm >> instr "test" [modrm_rm, modrm_reg]
                , opcode 0x85 >> opWidthW >> modrm >> instr "test" [modrm_rm, modrm_reg]
                , opcode 0x86 >> opWidthB >> modrm >> instr "xchg" [modrm_rm, modrm_reg]
                , opcode 0x87 >> opWidthW >> modrm >> instr "xchg" [modrm_rm, modrm_reg]
                , opcode 0x88 >> opWidthB >> modrm >> instr "mov" [modrm_rm, modrm_reg]
                , opcode 0x89 >> opWidthW >> modrm >> instr "mov" [modrm_rm, modrm_reg]
                , opcode 0x8a >> opWidthB >> modrm >> instr "mov" [modrm_reg, modrm_rm]
                , opcode 0x8b >> opWidthW >> modrm >> instr "mov" [modrm_reg, modrm_rm]
                , opcode 0x8c >> opWidthX 32 32 16 >> modrm >> instr "mov" [modrm_rm, modrm_sreg] -- intel says otherwise
                , opcode 0x8e >> opWidthX 32 32 16 >> modrm >> instr "mov" [modrm_sreg, modrm_rm] -- intel says otherwise
                , opcode 0x8d >> opWidthW >> modrm >> opmodnot3 >> instr "lea" [modrm_reg, modrm_rm]
                , opcode 0x8f >> modrm >> opcodeMatch 0 >> opWidthF 64 >> instr "pop" [modrm_rm]
-- 0x90
                , opcode 0x90 >> nop
                , do r <- mask 0xf8 0x90; opWidthW; instr "xchg" [reg (r .&. 0x07) id, accum]
                , opcode 0x98 >> forkX (instr "cdqe" []) (instr "cwde" []) (instr "cbw" [])
                , opcode 0x99 >> forkX (instr "cqo" [])  (instr "cdq" [])  (instr "cwd" [])
                , opcode 0x9b >> instr "wait" []
                , opcode 0x9c >> forkX (instr "pushfq" []) (instr "pushfq" []) (instr "pushfq" []) -- intel says otherwise
                , opcode 0x9d >> forkX (instr "popfq" []) (instr "popfq" []) (instr "popf" [])
                , opcode 0x9e >> instr "sahf" []
                , opcode 0x9f >> instr "lahf" []
-- 0xa0
                , opcode 0xa0 >> opWidthB >> moffs >> instr "mov" [accum, moffset]
                , opcode 0xa1 >> opWidthW >> moffs >> instr "mov" [accum, moffset]
                , opcode 0xa2 >> opWidthB >> moffs >> instr "mov" [moffset, accum]
                , opcode 0xa3 >> opWidthW >> moffs >> instr "mov" [moffset, accum]
                , opcode 0xa4 >> instr "movsb" []
                , opcode 0xa5 >> forkX (instr "movsq" []) (instr "movsd" []) (instr "movsw" [])
                , opcode 0xa6 >> instr "cmpsb" []
                , opcode 0xa7 >> forkX (instr "cmpsq" []) (instr "cmpsd" []) (instr "cmpsw" [])
                , opcode 0xa8 >> opWidthB >> imm >> instr "test" [accum, immed]
                , opcode 0xa9 >> opWidthW >> imm >> instr "test" [accum, immed]
                , opcode 0xaa >> instr "stosb" []
                , opcode 0xab >> forkX (instr "stosq" []) (instr "stosd" []) (instr "stosw" [])
                , opcode 0xac >> instr "lodsb" []
                , opcode 0xad >> forkX (instr "lodsq" []) (instr "lodsd" []) (instr "lodsw" [])
                , opcode 0xae >> instr "scasb" []
                , opcode 0xaf >> forkX (instr "scasq" []) (instr "scasd" []) (instr "scasw" [])
-- 0xb0
                , do r <- mask 0xf8 0xb0; opWidthB; immL; instr "mov" [reg (r .&. 0x07) id, immed]
                , do r <- mask 0xf8 0xb8; opWidthW; immL; instr "mov" [reg (r .&. 0x07) id, immed]
-- 0xc0
                , do opcode 0xc0; opWidthB; modrm; i <- ext2A; immB; instr i [modrm_rm, immed]
                , do opcode 0xc1; opWidthW; modrm; i <- ext2A; immB; instr i [modrm_rm, immed]
                , opcode 0xc2 >> immW >> instr "ret" [immed]
                , opcode 0xc3 >> instr "ret" []
                , opcode 0xc6 >> opcode 0xf8 >> immB >> instr "xabort" [immed]
                , opcode 0xc6 >> modrm >> opcodeMatch 0 >> opWidthB >> imm >> instr "mov" [modrm_rm, immed]
                , opcode 0xc7 >> opcode 0xf8 >> immB >> instr "xflush" [immed]
                , opcode 0xc7 >> modrm >> opcodeMatch 0 >> opWidthW >> imm >> instr "mov" [modrm_rm, immed]
                , do opcode 0xc8; opWidthF 16; imm; i <- imm''; instr "enter" [immed, pure $ Op_Imm $ Immediate (BitSize 8) i]
                , opcode 0xc9 >> instr "leave" []
                , opcode 0xca >> opWidthF 16 >> imm >> instr "retf" [immed]
                , opcode 0xcb >> instr "retf" []
                , opcode 0xcc >> instr "int3" []
                , opcode 0xcd >> opWidthB >> imm >> instr "int" [immed]
                , opcode 0xcf >> forkX (instr "iretq" []) (instr "iretd" []) (instr "iretw" [])
-- 0xd0
                , do opcode 0xd0; opWidthB; modrm; i <- ext2A; instr i [modrm_rm, pure (Op_Const 1)]
                , do opcode 0xd1; opWidthW; modrm; i <- ext2A; instr i [modrm_rm, pure (Op_Const 1)]
                , do opcode 0xd2; opWidthB; modrm; i <- ext2A; instr i [modrm_rm, pure (Op_Reg (Reg8 RCX HalfL))]
                , do opcode 0xd3; opWidthW; modrm; i <- ext2A; instr i [modrm_rm, pure (Op_Reg (Reg8 RCX HalfL))]
                , opcode 0xd7 >> instr "xlatb" []
-- fpu: 0xd8
                , fpu
-- 0xe0
                , do opcode 0xe0; d <- displ; instr "loopnz" [pure d]
                , do opcode 0xe1; d <- displ; instr "loope" [pure d]
                , do opcode 0xe2; d <- displ; instr "loop" [pure d]
                , do opcode 0xe3; d <- displ; forkA (instr "jrcxz" [pure d]) (instr "jecxz" [pure d])
                , opcode 0xe4 >> opWidthB >> immB >> instr "in" [accum, immed]
                , opcode 0xe5 >> opWidthX' 32 16 >> immB >> instr "in" [accum, immed]
                , opcode 0xe6 >> opWidthB >> immB >> instr "out" [immed, accum]
                , opcode 0xe7 >> opWidthX' 32 16 >> immB >> instr "out" [immed, accum]
                , do opcode 0xe8; d <- forkX' displD displW; instr "call" [pure d]
                , do opcode 0xe9; d <- forkX' displD displW; instr "jmp" [pure d]
                , do opcode 0xeb; d <- displ; instr "jmp" [pure d]
                , opcode 0xec >> opWidthB >> instr "in" [accum, pure (Op_Reg (Reg16 RDX))]
                , opcode 0xed >> opWidthX' 32 16 >> instr "in" [accum, pure (Op_Reg (Reg16 RDX))]
                , opcode 0xee >> opWidthB >> instr "out" [pure (Op_Reg (Reg16 RDX)), accum]
                , opcode 0xef >> opWidthX' 32 16 >> instr "out" [pure (Op_Reg (Reg16 RDX)), accum]
-- 0x0f, 0x40
                , opcode 0x0f >> (do i <- mask 0xf0 0x40; opWidthW ; modrm; instr (cmov i) [modrm_reg, modrm_rm])
-- 0x0f, 0x80
                , opcode 0x0f >> (do i <- mask 0xf0 0x80; d <- displD; instr (shortjmp i) [pure d])



-- 0xf0
                , opcode 0xf1 >> instr "int1" [] -- not in intel spec
                , opcode 0xf4 >> instr "hlt" []
                , opcode 0xf5 >> instr "cmc" []
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 0 >> imm >> instr "test" [modrm_rm, immed]
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 1 >> imm >> instr "test" [modrm_rm, immed] -- not intel spec
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 2 >> instr "not" [modrm_rm]
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 3 >> instr "neg" [modrm_rm]
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 4 >> instr "mul" [modrm_rm]
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 5 >> instr "imul" [modrm_rm]
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 6 >> instr "div" [modrm_rm]
                , opcode 0xf6 >> modrm >> opWidthB >> opcodeMatch 7 >> instr "idiv" [modrm_rm]
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 0 >> imm >> instr "test" [modrm_rm, immed]
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 1 >> imm >> instr "test" [modrm_rm, immed] -- not intel spec
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 2 >> instr "not" [modrm_rm]
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 3 >> instr "neg" [modrm_rm]
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 4 >> instr "mul" [modrm_rm]
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 5 >> instr "imul" [modrm_rm]
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 6 >> instr "div" [modrm_rm]
                , opcode 0xf7 >> modrm >> opWidthW >> opcodeMatch 7 >> instr "idiv" [modrm_rm]
                , opcode 0xf8 >> instr "clc" []
                , opcode 0xf9 >> instr "stc" []
                , opcode 0xfa >> instr "cli" []
                , opcode 0xfb >> instr "sti" []
                , opcode 0xfc >> instr "cld" []
                , opcode 0xfd >> instr "std" []
                , opcode 0xfe >> modrm >> opWidthB >> opcodeMatch 0 >> instr "inc" [modrm_rm]
                , opcode 0xfe >> modrm >> opWidthB >> opcodeMatch 1 >> instr "dec" [modrm_rm]
                , opcode 0xff >> modrm >> opWidthW >> opcodeMatch 0 >> instr "inc" [modrm_rm]
                , opcode 0xff >> modrm >> opWidthW >> opcodeMatch 1 >> instr "dec" [modrm_rm]
                , opcode 0xff >> modrm >> opWidthF 64 >> opcodeMatch 2 >> instr "call" [Op_Near <$> modrm_rm]
                , opcode 0xff >> modrm >> opWidthX' 32 16 >> opcodeMatch 3 >> instr "call" [Op_Far <$> modrm_rm]
                , opcode 0xff >> modrm >> opWidthF 64 >> opcodeMatch 4 >> instr "jmp" [Op_Near <$> modrm_rm]
                , opcode 0xff >> modrm >> opWidthF 32 >> opcodeMatch 5 >> instr "jmp" [Op_Far <$> modrm_rm]
                , opcode 0xff >> modrm >> opWidthF 64 >> opcodeMatch 6 >> instr "push" [modrm_rm]

                , opcode 0x0f >> opcode 0x00 >> modrm >> opcodeMatch 0 >> instr "sldt" [modrm_rm]
                , opcode 0x0f >> opcode 0x00 >> modrm >> opcodeMatch 1 >> instr "str" [modrm_rm]
                , opcode 0x0f >> opcode 0x00 >> modrm >> opcodeMatch 2 >> instr "lldt" [modrm_rm]
                , opcode 0x0f >> opcode 0x00 >> modrm >> opcodeMatch 3 >> instr "ltr" [modrm_rm]
                , opcode 0x0f >> opcode 0x00 >> modrm >> opcodeMatch 4 >> instr "verr" [modrm_rm]
                , opcode 0x0f >> opcode 0x00 >> modrm >> opcodeMatch 5 >> instr "verw" [modrm_rm]

                , opcode 0x0f >> opcode 0x01 >> opcode 0xc1 >> instr "vmcall" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xc2 >> instr "vmlaunch" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xc3 >> instr "vmresume" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xc4 >> instr "vmxoff" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xc8 >> instr "monitor" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xc9 >> instr "mwait" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xca >> instr "clac" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xcb >> instr "stac" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xcf >> instr "encls" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xd0 >> instr "xgetbv" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xd1 >> instr "xsetbv" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xd4 >> instr "vmfunc" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xd5 >> instr "xend" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xd6 >> instr "xtest" []
                , opcode 0x0f >> opcode 0x01 >> opcode 0xd7 >> instr "enclu" []

                , opcode 0x0f >> opcode 0x0b >> instr "ud2" []

                , opcode 0x0f >> opcode 0x90 >> modrm >> instr "seto" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x91 >> modrm >> instr "setno" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x92 >> modrm >> instr "setb" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x93 >> modrm >> instr "setnb" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x94 >> modrm >> instr "sete" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x95 >> modrm >> instr "setne" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x96 >> modrm >> instr "setbe" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x97 >> modrm >> instr "setnbe" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x98 >> modrm >> instr "sets" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x99 >> modrm >> instr "setns" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x9a >> modrm >> instr "setp" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x9b >> modrm >> instr "setnp" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x9c >> modrm >> instr "setl" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x9d >> modrm >> instr "setnl" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x9e >> modrm >> instr "setle" [opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0x9f >> modrm >> instr "setnle" [opWidthB >> modrm_rm]

                , opcode 0x0f >> opcode 0xa3 >> opWidthW >> modrm >> instr "bt" [modrm_rm, modrm_reg]
                , opcode 0x0f >> opcode 0xab >> modrm >> opWidthW >> instr "bts" [modrm_rm, modrm_reg]
                , opcode 0x0f >> opcode 0xaf >> opWidthW >> modrm >> instr "imul" [modrm_reg, modrm_rm]


                , opcode 0x0f >> opcode 0xba >> modrm >> opWidthW >> opcodeMatch 4 >> immB >> instr "bt" [modrm_rm, immed]
                , opcode 0x0f >> opcode 0xba >> modrm >> opWidthW >> opcodeMatch 5 >> immB >> instr "bts" [modrm_rm, immed]
                , opcode 0x0f >> opcode 0xba >> modrm >> opWidthW >> opcodeMatch 6 >> immB >> instr "btr" [modrm_rm, immed]
                , opcode 0x0f >> opcode 0xba >> modrm >> opWidthW >> opcodeMatch 7 >> immB >> instr "btc" [modrm_rm, immed]
                , opcode 0x0f >> opcode 0xbb >> modrm >> opWidthW >> instr "btc" [modrm_rm, modrm_reg]
                , opcode 0x0f >> opcode 0xbc >> modrm >> opWidthW >> instr "bsf" [modrm_reg, modrm_rm]
                , opcode 0x0f >> opcode 0xbd >> modrm >> opWidthW >> instr "bsr" [modrm_reg, modrm_rm]
                , opcode 0x0f >> opcode 0xbe >> modrm >> instr "movsx" [modrm_reg, opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0xbf >> modrm >> instr "movsx" [modrm_reg, opWidthF 16 >> modrm_rm]
                , opcode 0x0f >> opcode 0xb6 >> modrm >> instr "movzx" [modrm_reg, opWidthB >> modrm_rm]
                , opcode 0x0f >> opcode 0xb7 >> modrm >> instr "movzx" [modrm_reg, opWidthF 16 >> modrm_rm]

                , opcode 0x0f >> opcode 0xc1 >> opWidthW >> modrm >> instr "xadd" [modrm_rm, modrm_reg]

                , opcode 0x0f >>  (do r <- mask 0xf8 0xc8; opWidthW ; instr "bswap" [reg (r .&. 0x07) id])

                , mmx_sse
                , nops


     ]
fpu = choice [ fail "dunsel"
            , do opcode 0xd8; r <- (lift $ satisfy (>=0xc0)); adv 1; instr (fpuD8 (bits 3 3 r)) [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xd8; modrm; opmodnot3; i <- modopcode; instr (fpuD8 i) [modrm_rm]

            , do opcode 0xd9; r <- mask 0xf8 0xc0; instr "fld" [freg (r .&. 7)]
            , do opcode 0xd9; r <- mask 0xf8 0xc8; instr "fxch" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , opcode 0xd9 >> opcode 0xd0 >> instr "fnop" []
            , opcode 0xd9 >> opcode 0xe0 >> instr "fchs" []
            , opcode 0xd9 >> opcode 0xe1 >> instr "fabs" []
            , opcode 0xd9 >> opcode 0xe4 >> instr "ftst" []
            , opcode 0xd9 >> opcode 0xe5 >> instr "fxam" []
            , opcode 0xd9 >> opcode 0xf0 >> instr "f2xm1" []
            , opcode 0xd9 >> opcode 0xf1 >> instr "fyl2x" []
            , opcode 0xd9 >> opcode 0xf2 >> instr "fptan" []
            , opcode 0xd9 >> opcode 0xf3 >> instr "fpatan" []
            , opcode 0xd9 >> opcode 0xf4 >> instr "fxtract" []
            , opcode 0xd9 >> opcode 0xf5 >> instr "fprem1" []
            , opcode 0xd9 >> opcode 0xf6 >> instr "fdecstp" []
            , opcode 0xd9 >> opcode 0xf7 >> instr "fincstp" []
            , opcode 0xd9 >> opcode 0xe8 >> instr "fld1" []
            , opcode 0xd9 >> opcode 0xe9 >> instr "fldl2t" []
            , opcode 0xd9 >> opcode 0xea >> instr "fldl2e" []
            , opcode 0xd9 >> opcode 0xeb >> instr "fldpi" []
            , opcode 0xd9 >> opcode 0xec >> instr "fldlg2" []
            , opcode 0xd9 >> opcode 0xed >> instr "fldln2" []
            , opcode 0xd9 >> opcode 0xee >> instr "fldz" []
            , opcode 0xd9 >> opcode 0xf8 >> instr "fprem" []
            , opcode 0xd9 >> opcode 0xf9 >> instr "fyl2xp1" []
            , opcode 0xd9 >> opcode 0xfa >> instr "fsqrt" []
            , opcode 0xd9 >> opcode 0xfb >> instr "fsincos" []
            , opcode 0xd9 >> opcode 0xfc >> instr "frndint" []
            , opcode 0xd9 >> opcode 0xfd >> instr "fscale" []
            , opcode 0xd9 >> opcode 0xfe >> instr "fsin" []
            , opcode 0xd9 >> opcode 0xff >> instr "fcos" []
            , opcode 0xd9 >> modrm >> opmodnot3 >> opcodeMatch 0 >> opWidthF 32 >> instr "fld" [modrm_rm]
            , opcode 0xd9 >> modrm >> opmodnot3 >> opcodeMatch 2 >> opWidthF 32 >> instr "fst" [modrm_rm]
            , opcode 0xd9 >> modrm >> opmodnot3 >> opcodeMatch 3 >> opWidthF 32 >> instr "fstp" [modrm_rm]
            , opcode 0xd9 >> modrm >> opmodnot3 >> opcodeMatch 4 >> opWidthF 32 >> instr "fldenv" [modrm_rm]
            , opcode 0xd9 >> modrm >> opmodnot3 >> opcodeMatch 5 >> opWidthF 16 >> instr "fldcw" [modrm_rm]
            , opcode 0xd9 >> modrm >> opmodnot3 >> opcodeMatch 6 >> opWidthF 32 >> instr "fnstenv" [modrm_rm]
            , opcode 0xd9 >> modrm >> opmodnot3 >> opcodeMatch 7 >> opWidthF 16 >> instr "fnstcw" [modrm_rm]

            , do opcode 0xda; r <- mask 0xf8 0xc0; instr "fcmovb" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xda; r <- mask 0xf8 0xc8; instr "fcmove" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xda; r <- mask 0xf8 0xd0; instr "fcmovbe" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xda; r <- mask 0xf8 0xd8; instr "fcmovu" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , opcode 0xda >> opcode 0xe9 >> instr "fucompp" []
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 0 >> opWidthF 32 >> instr "fiadd" [modrm_rm]
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 1 >> opWidthF 32 >> instr "fimul" [modrm_rm]
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 2 >> opWidthF 32 >> instr "ficom" [modrm_rm]
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 3 >> opWidthF 32 >> instr "ficomp" [modrm_rm]
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 4 >> opWidthF 32 >> instr "fisub" [modrm_rm]
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 5 >> opWidthF 32 >> instr "fisubr" [modrm_rm]
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 6 >> opWidthF 32 >> instr "fidiv" [modrm_rm]
            , opcode 0xda >> modrm >> opmodnot3 >> opcodeMatch 7 >> opWidthF 32 >> instr "fidivr" [modrm_rm]

            , do opcode 0xdb; r <- mask 0xf8 0xc0; instr "fcmovnb" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xdb; r <- mask 0xf8 0xc8; instr "fcmovne" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xdb; r <- mask 0xf8 0xd0; instr "fcmovnbe" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xdb; r <- mask 0xf8 0xd8; instr "fcmovnu" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xdb; r <- mask 0xf8 0xe8; instr "fucomi" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xdb; r <- mask 0xf8 0xf0; instr "fcomi" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , opcode 0xdb >> opcode 0xe2 >> instr "fclex" []
            , opcode 0xdb >> opcode 0xe3 >> instr "finit" []
            , opcode 0xdb >> modrm >> opmodnot3 >> opcodeMatch 0 >> opWidthF 32 >> instr "fild" [modrm_rm]
            , opcode 0xdb >> modrm >> opmodnot3 >> opcodeMatch 1 >> opWidthF 32 >> instr "fisttp" [modrm_rm]
            , opcode 0xdb >> modrm >> opmodnot3 >> opcodeMatch 2 >> opWidthF 32 >> instr "fist" [modrm_rm]
            , opcode 0xdb >> modrm >> opmodnot3 >> opcodeMatch 3 >> opWidthF 32 >> instr "fistp" [modrm_rm]
            , opcode 0xdb >> modrm >> opmodnot3 >> opcodeMatch 5 >> opWidthF 80 >> instr "fld" [modrm_rm]
            , opcode 0xdb >> modrm >> opmodnot3 >> opcodeMatch 7 >> opWidthF 80 >> instr "fstp" [modrm_rm]

            , do opcode 0xdc; r <- mask 0xf8 0xc0; instr "fadd" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xdc; r <- mask 0xf8 0xc8; instr "fmul" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xdc; r <- mask 0xf8 0xd0; instr "fcom2" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))] -- not in intel spec
            , do opcode 0xdc; r <- mask 0xf8 0xe0; instr "fsubr" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xdc; r <- mask 0xf8 0xe8; instr "fsub" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xdc; r <- mask 0xf8 0xf0; instr "fdivr" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xdc; r <- mask 0xf8 0xf8; instr "fdiv" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xdc; modrm; opmodnot3; i <- modopcode; opWidthF 64 >> instr (fpuD8 i) [modrm_rm]

            , do opcode 0xdd; r <- mask 0xf8 0xc0; instr "ffree" [freg (r .&. 7)]
            , do opcode 0xdd; r <- mask 0xf8 0xd0; instr "fst" [freg (r .&. 7)]
            , do opcode 0xdd; r <- mask 0xf8 0xd8; instr "fstp" [freg (r .&. 7)]
            , do opcode 0xdd; r <- mask 0xf8 0xe0; instr "fucom" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xdd; r <- mask 0xf8 0xe8; instr "fucomp" [freg (r .&. 7)]
            , opcode 0xdd >> modrm >> opmodnot3 >> opcodeMatch 0 >> opWidthF 64 >> instr "fld" [modrm_rm]
            , opcode 0xdd >> modrm >> opmodnot3 >> opcodeMatch 1 >> opWidthF 64 >> instr "fisttp" [modrm_rm]
            , opcode 0xdd >> modrm >> opmodnot3 >> opcodeMatch 2 >> opWidthF 64 >> instr "fst" [modrm_rm]
            , opcode 0xdd >> modrm >> opmodnot3 >> opcodeMatch 3 >> opWidthF 64 >> instr "fstp" [modrm_rm]
            , opcode 0xdd >> modrm >> opmodnot3 >> opcodeMatch 4 >> opWidthF 64 >> instr "frstor" [modrm_rm]
            , opcode 0xdd >> modrm >> opmodnot3 >> opcodeMatch 6 >> opWidthF 64 >> instr "fnsave" [modrm_rm]
            , opcode 0xdd >> modrm >> opmodnot3 >> opcodeMatch 7 >> opWidthF 16 >> instr "fnstsw" [modrm_rm]

            , do opcode 0xde; r <- mask 0xf8 0xc0; instr "faddp" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xde; r <- mask 0xf8 0xc8; instr "fmulp" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xde; r <- mask 0xf8 0xe0; instr "fsubrp" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xde; r <- mask 0xf8 0xe8; instr "fsubp" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xde; r <- mask 0xf8 0xf0; instr "fdivrp" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , do opcode 0xde; r <- mask 0xf8 0xf8; instr "fdivp" [freg (r .&. 7), pure (Op_Reg (RegFPU ST0))]
            , opcode 0xde >> opcode 0xd9 >> instr "fcompp" []
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 0 >> opWidthF 16 >> instr "fiadd" [modrm_rm]
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 1 >> opWidthF 16 >> instr "fimul" [modrm_rm]
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 2 >> opWidthF 16 >> instr "ficom" [modrm_rm]
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 3 >> opWidthF 16 >> instr "ficomp" [modrm_rm]
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 4 >> opWidthF 16 >> instr "fisub" [modrm_rm]
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 5 >> opWidthF 16 >> instr "fisubr" [modrm_rm]
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 6 >> opWidthF 16 >> instr "fidiv" [modrm_rm]
            , opcode 0xde >> modrm >> opmodnot3 >> opcodeMatch 7 >> opWidthF 16 >> instr "fidivr" [modrm_rm]

            , opcode 0xdf >> opcode 0xe0 >> instr "fnstsw" [pure (Op_Reg (Reg16 RAX))]
            , do opcode 0xdf; r <- mask 0xf8 0xe8; instr "fucomip" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , do opcode 0xdf; r <- mask 0xf8 0xf0; instr "fcomip" [pure (Op_Reg (RegFPU ST0)), freg (r .&. 7)]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 0 >> opWidthF 16 >> instr "fild" [modrm_rm]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 1 >> opWidthF 16 >> instr "fisttp" [modrm_rm]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 2 >> opWidthF 16 >> instr "fist" [modrm_rm]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 3 >> opWidthF 16 >> instr "fistp" [modrm_rm]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 4 >> opWidthF 16 >> instr "fbld" [modrm_rm]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 5 >> opWidthF 16 >> instr "fild" [modrm_rm]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 6 >> opWidthF 16 >> instr "fbstp" [modrm_rm]
            , opcode 0xdf >> modrm >> opmodnot3 >> opcodeMatch 7 >> opWidthF 16 >> instr "fistp" [modrm_rm]
            ]



prefixSet :: Int -> Disassembler ()
prefixSet 0xf0 = whenM (not <$> gets dsLock) $ fail "prefixNotSet"
prefixSet 0xf2 = whenM (not <$> gets dsRepNE) $ fail "prefixNotSet"
prefixSet 0xf3 = whenM (not <$> gets dsRep) $ fail "prefixNotSet"
prefixSet 0x66 = whenM (not <$> gets dsOpWidthOverride) $ fail "prefixNotSet"
prefixSet 0x67 = whenM (not <$> gets dsAdWidthOverride) $ fail "prefixNotSet"
prefixSet _    = fail "prefixSet"
  
noPrefix (Instruction a _ op dst srcs si) = Instruction a [] op dst srcs si

-- http://ref.x86asm.net/coder64.html#modrm_byte_32_64
--
-- Column o = opcodeMatch (see IDIV). If column o == r then use modrm
-- Operand = r/m16/32/64 means opWidthW


--TODO
mmx_sse = choice
  [ fail "sse"
  , opcode 0x0f >> opcode 0x10 >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "movss" [modrm_xmm, opWidthF 32 >> modrm_xmm_m])
                                                                     , (0x66, instr "movupd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m])
                                                                     , (0xf2, instr "movsd" [modrm_xmm, opWidthF 64 >> modrm_xmm_m]) ]
                                                                     (        instr "movups" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x11 >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "movss" [opWidthF 32 >> modrm_xmm_m, modrm_xmm])
                                                                     , (0x66, instr "movupd" [opWidthF 128 >> modrm_xmm_m, modrm_xmm])
                                                                     , (0xf2, instr "movsd" [opWidthF 64 >> modrm_xmm_m, modrm_xmm]) ]
                                                                     (        instr "movups" [opWidthF 128 >> modrm_xmm_m, modrm_xmm] )

  , opcode 0x0f >> opcode 0x16 >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, fail  "MOVSHDUP")
                                                                     , (0x66, fail  "MOVHPD") ]
                                                                     (        instr "MOVHPS" [modrm_xmm, opWidthF 64 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x17 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, fail  "MOVHPD") ]
                                                                     (        instr "MOVHPS" [opWidthF 64 >> modrm_xmm_m, modrm_xmm] )

  , opcode 0x0f >> opcode 0x28 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "movapd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "movaps" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x29 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "movapd" [opWidthF 128 >> modrm_xmm_m, modrm_xmm]) ]
                                                                     (        instr "movaps" [opWidthF 128 >> modrm_xmm_m, modrm_xmm] )
  , opcode 0x0f >> opcode 0x2a >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "CVTSI2SS" [modrm_xmm, ifRexW 64 >> modrm_rm])
                                                                     , (0x66, instr "CVTPI2PD" [modrm_xmm, modrm_rm])
                                                                     , (0xf2, instr "CVTSI2SD" [modrm_xmm, ifRexW 64 >> modrm_rm]) ]
                                                                     (        instr "CVTPI2PS" [modrm_xmm, modrm_rm] )
  , opcode 0x0f >> opcode 0x2c >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "CVTTSS2SI" [ifRexW 64 >> modrm_reg,opWidthF 32 >> modrm_xmm_m])
                                                                     , (0x66, instr "CVTTPD2PI" [pure $ Op_Reg RegNone,opWidthF 128 >> modrm_xmm_m]) -- TODO ST registers
                                                                     , (0xf2, instr "CVTTSD2SI" [ifRexW 64 >> modrm_reg,opWidthF 64 >> modrm_xmm_m]) ]
                                                                     (        instr "CVTTPS2PI" [pure $ Op_Reg RegNone,opWidthF 64 >> modrm_xmm_m] ) -- TODO ST registers
  , opcode 0x0f >> opcode 0x2e >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "ucomisd" [modrm_xmm, opWidthF 64 >> modrm_xmm_m]) ]
                                                                     (        instr "ucomiss" [modrm_xmm, opWidthF 32 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x2f >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "comisd" [modrm_xmm, opWidthF 64 >> modrm_xmm_m]) ]
                                                                     (        instr "comiss" [modrm_xmm, opWidthF 32 >> modrm_xmm_m] )


  , opcode 0x0f >> opcode 0x51 >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "sqrtss" [modrm_xmm, opWidthF 32 >> modrm_xmm_m])
                                                                     , (0x66, instr "sqrtpd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m])
                                                                     , (0xf2, instr "sqrtsd" [modrm_xmm, opWidthF 64 >> modrm_xmm_m]) ]
                                                                     (        instr "sqrtps" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x52 >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "rsqrtss" [modrm_xmm, opWidthF 32 >> modrm_xmm_m]) ]
                                                                     (        instr "rsqrtps" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x53 >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "rcpps" [modrm_xmm, opWidthF 32 >> modrm_xmm_m]) ]
                                                                     (        instr "rcpss" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x54 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "andpd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "andps" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x55 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "andnpd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "andnps" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x56 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "orpd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "orps" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x57 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "xorpd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "xorps" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x58 >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "addss" [modrm_xmm, opWidthF 32 >> modrm_xmm_m])
                                                                     , (0x66, instr "addpd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m])
                                                                     , (0xf2, instr "addsd" [modrm_xmm, opWidthF 64 >> modrm_xmm_m]) ]
                                                                     (        instr "addps" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x59 >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "mulss" [modrm_xmm, opWidthF 32 >> modrm_xmm_m])
                                                                     , (0x66, instr "mulpd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m])
                                                                     , (0xf2, instr "mulsd" [modrm_xmm, opWidthF 64 >> modrm_xmm_m]) ]
                                                                     (        instr "mulps" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x5a >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "CVTSS2SD" [modrm_xmm, opWidthF 32 >> modrm_xmm_m])
                                                                     , (0x66, instr "CVTPD2PS" [modrm_xmm, opWidthF 128 >> modrm_xmm_m])
                                                                     , (0xf2, instr "CVTSD2SS" [modrm_xmm, opWidthF 64 >> modrm_xmm_m]) ]
                                                                     (        instr "CVTPS2PD" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x5b >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "CVTTPS2DQ" [modrm_xmm, opWidthF 128 >> modrm_xmm_m])
                                                                     , (0x66, instr "CVTPS2DQ" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "CVTDQ2PS" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x5c >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "subss" [modrm_xmm, opWidthF 32 >> modrm_xmm_m])
                                                                     , (0x66, instr "subpd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m])
                                                                     , (0xf2, instr "subsd" [modrm_xmm, opWidthF 64 >> modrm_xmm_m]) ]
                                                                     (        instr "subps" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x5d >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "minss" [modrm_xmm, opWidthF 32 >> modrm_xmm_m])
                                                                     , (0x66, instr "minpd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m])
                                                                     , (0xf2, instr "minsd" [modrm_xmm, opWidthF 64 >> modrm_xmm_m]) ]
                                                                     (        instr "minps" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x5e >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "divss" [modrm_xmm, opWidthF 32 >> modrm_xmm_m])
                                                                     , (0x66, instr "divpd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m])
                                                                     , (0xf2, instr "divsd" [modrm_xmm, opWidthF 64 >> modrm_xmm_m]) ]
                                                                     (        instr "divps" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )
  , opcode 0x0f >> opcode 0x5f >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "maxss" [modrm_xmm, opWidthF 32 >> modrm_xmm_m])
                                                                     , (0x66, instr "maxpd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m])
                                                                     , (0xf2, instr "maxsd" [modrm_xmm, opWidthF 64 >> modrm_xmm_m]) ]
                                                                     (        instr "maxps" [modrm_xmm, opWidthF 128 >> modrm_xmm_m] )

  , opcode 0x0f >> opcode 0x60 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PUNPCKLBW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "PUNPCKLBW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register
  , opcode 0x0f >> opcode 0x61 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PUNPCKLWD" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "PUNPCKLWD" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register
  , opcode 0x0f >> opcode 0x62 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PUNPCKLDQ" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "PUNPCKLDQ" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register
  , opcode 0x0f >> opcode 0x63 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PACKSSWB" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "PACKSSWB" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register
  , opcode 0x0f >> opcode 0x64 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PCMPGTB" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "PCMPGTB" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register
  , opcode 0x0f >> opcode 0x65 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PCMPGTW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "PCMPGTW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register
  , opcode 0x0f >> opcode 0x66 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PCMPGTD" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "PCMPGTD" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register
  , opcode 0x0f >> opcode 0x67 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PACKUSWB" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "PACKUSWB" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register
  , opcode 0x0f >> opcode 0x68 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PUNPCKHBW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "PUNPCKHBW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register
  , opcode 0x0f >> opcode 0x69 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PUNPCKHWD" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "PUNPCKHWD" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register
  , opcode 0x0f >> opcode 0x6a >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PUNPCKHDQ" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "PUNPCKHDQ" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register
  , opcode 0x0f >> opcode 0x6b >> modrm >> noPrefix <$>                       instr "PACKSSDW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  , opcode 0x0f >> opcode 0x6c >> modrm >> noPrefix <$>                       instr "PUNPCKLQDQ" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  , opcode 0x0f >> opcode 0x6d >> modrm >> noPrefix <$>                       instr "PUNPCKHQDQ" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  , opcode 0x0f >> opcode 0x6e >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "movq" [modrm_xmm, opWidthF 64 >> modrm_rm]) ]
                                                                     (        instr "movq" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register
  , opcode 0x0f >> opcode 0x6f >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "movdqu" [modrm_xmm, opWidthF 128 >> modrm_xmm_m])
                                                                     , (0x66, instr "movdqa" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "movq" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register
  , opcode 0x0f >> opcode 0x70 >> modrm >> immB >> noPrefix <$> forkPrefixes [ (0xf3, instr "PSHUFHW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m, immed])
                                                                             , (0x66, instr "PSHUFD" [modrm_xmm, opWidthF 128 >> modrm_xmm_m, immed])
                                                                             , (0xf2, instr "PSHUFLW" [modrm_xmm, opWidthF 64 >> modrm_xmm_m, immed]) ]
                                                                             (        instr "PSHUFW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone,immed] )


  , prefixSet 0x66 >> opcode 0x0f >> opcode 0x72 >> modrm >> opcodeMatch 4 >> immB >> noPrefix <$> instr "PSRAD" [modrm_xmm_m, immed]
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0x73 >> modrm >> opcodeMatch 2 >> immB >> noPrefix <$> instr "PSRLQ" [modrm_xmm_m, immed]
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0x73 >> modrm >> opcodeMatch 3 >> immB >> noPrefix <$> instr "PSRLDQ" [modrm_xmm_m, immed]
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0x73 >> modrm >> opcodeMatch 6 >> immB >> noPrefix <$> instr "PSLLQ" [modrm_xmm_m, immed]
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0x73 >> modrm >> opcodeMatch 7 >> immB >> noPrefix <$> instr "PSLLDQ" [modrm_xmm_m, immed]
  ,                   opcode 0x0f >> opcode 0x73 >> modrm >> opcodeMatch 2 >> immB >> noPrefix <$> instr "PSRLQ" [pure $ Op_Reg RegNone, immed]
  ,                   opcode 0x0f >> opcode 0x73 >> modrm >> opcodeMatch 2 >> immB >> noPrefix <$> instr "PSLLQ" [pure $ Op_Reg RegNone, immed]

  , opcode 0x0f >> opcode 0x74 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PCMPEQB" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "PCMPEQB" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] )
  , opcode 0x0f >> opcode 0x75 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PCMPEQW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "PCMPEQW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] )
  , opcode 0x0f >> opcode 0x76 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PCMPEQD" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]) ]
                                                                     (        instr "PCMPEQD" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] )

  , opcode 0x0f >> opcode 0x7e >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "movq" [modrm_xmm, opWidthF 64 >> modrm_xmm_m])
                                                                     , (0x66, instr "movq" [opWidthF 64 >> modrm_rm, modrm_xmm]) ]
                                                                     (        instr "movq" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register
  , opcode 0x0f >> opcode 0x7f >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, instr "movdqu" [opWidthF 128 >> modrm_xmm_m, modrm_xmm])
                                                                     , (0x66, instr "movdqa" [opWidthF 128 >> modrm_xmm_m, modrm_xmm]) ]
                                                                     (        instr "movq" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] ) -- TODO ST register


  , opcode 0x0f >> opcode 0xc2 >> modrm >> immB >> noPrefix <$> forkPrefixes [ (0xf3, instr "cmpss" [modrm_xmm, opWidthF 32 >> modrm_xmm_m, immed])
                                                                             , (0x66, instr "cmppd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m, immed])
                                                                             , (0xf2, instr "cmpsd" [modrm_xmm, opWidthF 64 >> modrm_xmm_m, immed]) ]
                                                                             (        instr "cmpps" [modrm_xmm, opWidthF 128 >> modrm_xmm_m, immed] )

  , opcode 0x0f >> opcode 0xc5 >> modrm >> immB >> noPrefix <$> forkPrefixes [(0x66,  instr "PEXTRW" [modrm_reg, modrm_xmm_m, immed]) ]
                                                                             (        instr "PEXTRW" [modrm_reg, pure $ Op_Reg RegNone, immed] )
  , opcode 0x0f >> opcode 0xc6 >> modrm >> immB >> noPrefix <$> forkPrefixes [(0x66,  instr "SHUFPD" [modrm_xmm, modrm_xmm_m, immed]) ]
                                                                             (        instr "SHUFPS" [modrm_xmm, modrm_xmm_m, immed] )




  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xd1 >> modrm >> noPrefix <$> instr "PSRLW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xd1 >> modrm >>              instr "PSRLW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xd2 >> modrm >> noPrefix <$> instr "PSRLD" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xd2 >> modrm >>              instr "PSRLD" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xd3 >> modrm >> noPrefix <$> instr "PSRLQ" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xd3 >> modrm >>              instr "PSRLQ" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xd4 >> modrm >> noPrefix <$> instr "PADDQ" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xd4 >> modrm >>              instr "PADDQ" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xd5 >> modrm >> noPrefix <$> instr "PMULLW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xd5 >> modrm >>              instr "PMULLW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , opcode 0x0f >> opcode 0xd6 >> modrm >> noPrefix <$> forkPrefixes [ (0xf3, fail "MOVQ2DQ")
                                                                             , (0x66, instr "movq"  [opWidthF 64 >> modrm_xmm_m, modrm_xmm])
                                                                             , (0xf2, fail "MOVDQ2Q") ]
                                                                             (        fail "" )
  , opcode 0x0f >> opcode 0xd7 >> modrm >> noPrefix <$> forkPrefixes [ (0x66, instr "PMOVMSKB"  [modrm_rm, modrm_xmm]) ]
                                                                       (      instr "PMOVMSKB"  [modrm_rm, pure $ Op_Reg RegNone] ) -- TODO ST register
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xd8 >> modrm >> noPrefix <$> instr "PSUBUSB" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xd8 >> modrm >>              instr "PSUBUSB" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xd9 >> modrm >> noPrefix <$> instr "PSUBUSW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xd9 >> modrm >>              instr "PSUBUSW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xda >> modrm >> noPrefix <$> instr "PMINUB" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xda >> modrm >>              instr "PMINUB" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xdb >> modrm >> noPrefix <$> instr "PAND" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xdb >> modrm >>              instr "PAND" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xdc >> modrm >> noPrefix <$> instr "PADDUSB" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xdc >> modrm >>              instr "PADDUSB" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xdd >> modrm >> noPrefix <$> instr "PADDUSW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xdd >> modrm >>              instr "PADDUSW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xde >> modrm >> noPrefix <$> instr "PMAXUB" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xde >> modrm >>              instr "PMAXUB" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xdf >> modrm >> noPrefix <$> instr "PANDN" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xdf >> modrm >>              instr "PANDN" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xe0 >> modrm >> noPrefix <$> instr "PAVGB" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xe0 >> modrm >>              instr "PAVGB" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xe1 >> modrm >> noPrefix <$> instr "PSRAW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xe1 >> modrm >>              instr "PSRAW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xe2 >> modrm >> noPrefix <$> instr "PSRAD" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xe2 >> modrm >>              instr "PSRAD" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xe3 >> modrm >> noPrefix <$> instr "PAVGW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xe3 >> modrm >>              instr "PAVGW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xe4 >> modrm >> noPrefix <$> instr "PMULHUW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xe4 >> modrm >>              instr "PMULHUW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xe5 >> modrm >> noPrefix <$> instr "PMULHW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xe5 >> modrm >>              instr "PMULHW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers

  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xe8 >> modrm >> noPrefix <$> instr "PSUBSB" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xe8 >> modrm >>              instr "PSUBSB" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xe9 >> modrm >> noPrefix <$> instr "PSUBSW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xe9 >> modrm >>              instr "PSUBSW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xea >> modrm >> noPrefix <$> instr "PMINSW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xea >> modrm >>              instr "PMINSW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xeb >> modrm >> noPrefix <$> instr "POR" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xeb >> modrm >>              instr "POR" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xec >> modrm >> noPrefix <$> instr "PADDSB" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xec >> modrm >>              instr "PADDSB" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xed >> modrm >> noPrefix <$> instr "PADDSW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xed >> modrm >>              instr "PADDSW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xee >> modrm >> noPrefix <$> instr "PMAXSW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xee >> modrm >>              instr "PMAXSW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xef >> modrm >> noPrefix <$> instr "pxor" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xef >> modrm >>              instr "pxor" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xf1 >> modrm >> noPrefix <$> instr "PSLLW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xf1 >> modrm >>              instr "PSLLW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xf2 >> modrm >> noPrefix <$> instr "PSLLD" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xf2 >> modrm >>              instr "PSLLD" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xf3 >> modrm >> noPrefix <$> instr "PSLLQ" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xf3 >> modrm >>              instr "PSLLQ" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xf4 >> modrm >> noPrefix <$> instr "PMULUDQ" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xf4 >> modrm >>              instr "PMULUDQ" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xf5 >> modrm >> noPrefix <$> instr "PMADDWD" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xf5 >> modrm >>              instr "PMADDW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xf6 >> modrm >> noPrefix <$> instr "PSADBW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xf6 >> modrm >>              instr "PSADBW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xf8 >> modrm >> noPrefix <$> instr "PSUBB" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xf8 >> modrm >>              instr "PSUBB" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xf9 >> modrm >> noPrefix <$> instr "PSUBW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xf9 >> modrm >>              instr "PSUBW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xfa >> modrm >> noPrefix <$> instr "PSUBD" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xfa >> modrm >>              instr "PSUBD" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xfb >> modrm >> noPrefix <$> instr "PSUBQ" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xfb >> modrm >>              instr "PSUBQ" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xfc >> modrm >> noPrefix <$> instr "PADDB" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xfc >> modrm >>              instr "PADDB" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xfd >> modrm >> noPrefix <$> instr "PADDW" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xfd >> modrm >>              instr "PADDW" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers
  , prefixSet 0x66 >> opcode 0x0f >> opcode 0xfe >> modrm >> noPrefix <$> instr "paddd" [modrm_xmm, opWidthF 128 >> modrm_xmm_m]
  ,                   opcode 0x0f >> opcode 0xfe >> modrm >>              instr "paddd" [pure $ Op_Reg RegNone, pure $ Op_Reg RegNone] -- TODO ST registers




  ]


--TODO
nops = choice
  [ fail "nop"
  , opcode 0x0f >> opcode 0x1f >> modrm >> opWidthF 32 >> instr "nop" []
  ]



modrm_xmm :: Disassembler Operand
modrm_xmm = do b'reg <- modRM_breg <$> fromJust <$> (gets dsModRM)
               rexR <- dsRexR
               let reg = select_xmm rexR b'reg
                 in pure $ Op_Reg reg

-- see table Registers at https://wiki.osdev.org/X86-64_Instruction_Encoding
modrm_xmm_m :: Disassembler Operand
modrm_xmm_m = do rm <- modRM_rm <$> fromJust <$> gets dsModRM
                 ow <- gets dsOpWidth
                 rexB <- dsRexB
                 let sr = select_xmm rexB
                   in pure (rm ow sr)

select_xmm :: Word8 -> Word8 -> Register
select_xmm rex reg = let
                rvec' = case rex of
                        1 -> [XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15]
                        0 -> [XMM0, XMM1, XMM2 , XMM3,  XMM4,  XMM5,  XMM6,  XMM7 ]
                rvec = map Reg128 rvec'
            in rvec !! (fromIntegral reg)




selectXmm 0 = XMM0
selectXmm 1 = XMM1
selectXmm 2 = XMM2
selectXmm 3 = XMM3
selectXmm 4 = XMM4
selectXmm 5 = XMM5
selectXmm 6 = XMM6
selectXmm 7 = XMM7
selectXmm 8 = XMM8
selectXmm 9 = XMM8
selectXmm 10 = XMM10
selectXmm 11 = XMM11
selectXmm 12 = XMM12
selectXmm 13 = XMM13
selectXmm 14 = XMM14
selectXmm 15 = XMM15


ifRexW w = do
  rexW <- dsRexW
  if rexW == 1 then opWidthF w else return ()


nop = do b <- dsRexB
         rep <- (gets dsRep)
         if b == 1 then fail "not nop"
            else if rep then instr "pause" []
                else instr "nop" []

ext1A i = ["add", "or", "adc", "sbb", "and", "sub", "xor", "cmp"] !! (fromIntegral i)
ext1A' i = ext1A (fromIntegral (bits 3 3 i))

fpuD8 i = [ "fadd", "fmul", "fcom", "fcomp", "fsub", "fsubr", "fdiv", "fdivr"] !! (fromIntegral i)

shortjmp i = ["jo", "jno", "jb", "jae", "jz", "jnz", "jbe", "ja", "js", "jns", "jp", "jnp", "jl", "jge", "jle", "jg"]
                        !! (fromIntegral (bits 0 4 i))

cmov i = ["cmovo", "cmovno", "cmovb", "cmovae", "cmovz", "cmovnz", "cmovbe", "cmova", "cmovs", "cmovns", "cmovp", "cmovnp", "cmovl", "cmovge", "cmovle", "cmovg"]
                        !! (fromIntegral (bits 0 4 i))

ext2A = do i <- modopcode
           case i of 0 -> pure "rol"
                     1 -> pure "ror"
                     2 -> pure "rcl"
                     3 -> pure "rcr"
                     4 -> pure "shl"
                     5 -> pure "shr"
                     6 -> pure "shl" -- intel says 6 is invalid but reportedly it's also shl
                     7 -> pure "sar"

opWidthB = opWidthX 8 8 8
opWidthW = opWidthX 64 32 16
opWidthF n = opWidthX n n n

forkPrefixes [] a' = a'
forkPrefixes ((p,a):ps) a' = choice [prefixSet p >> a, forkPrefixes ps a']

  

  

forkX q d w = do o16 <- dsO16
                 rexW <- dsRexW
                 case (o16, rexW) of (_, 1) -> q; (True, 0) -> w; (False, 0) -> d

forkX' d w = do o16 <- dsO16
                case (o16) of (True) -> w; (False) -> d


forkA q d  = do a32 <- dsA32
                case (a32) of (True) -> d; (False) -> q

opWidthX q d w = do o16 <- dsO16
                    rexW <- dsRexW
                    let ow = case (o16, rexW) of (_, 1) -> q; (True, 0) -> w; (False, 0) -> d
                      in modify (\x -> x { dsOpWidth = ow })

opWidthX' d w = do o16 <- dsO16
                   let ow = case (o16) of (True) -> w; (False) -> d
                     in modify (\x -> x { dsOpWidth = ow })

adWidth n1 n2 = dsA32 >>= (\f -> modify (\x -> x { dsAdWidth = if f then n1 else n2 } ) )

instr :: String -> [Disassembler Operand] -> Disassembler Instruction
instr i ops = Instruction <$> pure 0 <*> pfx <*> pure (read_opcode $ map toUpper i) <*> pure Nothing <*> sequence ops <*> pure 0

readBytes :: Int -> Disassembler ByteString
readBytes n = (lift $ (B.fromStrict <$> A.take n)) <* adv n

modopcode :: Disassembler Word8
modopcode = (gets dsModRM) >>= pure . modRM_breg . fromJust

opcodeMatch c = do o <- modopcode; if o == c then pure () else fail "no match"
opmodmatch c = do o <- modRM_mod <$> fromJust <$> (gets dsModRM); if o == c then pure () else fail "no match"
opmodnot3 = do o <- modRM_mod <$> fromJust <$> (gets dsModRM); if o == 3 then fail "no match" else pure ()

imm :: Disassembler ()
imm = do
    ow <- gets dsOpWidth
    case ow of  8 ->  (readBytes 1) >>= pure . (\s -> fromIntegral (G.runGet G.getWord8 (s)))
                16 -> (readBytes 2) >>= pure . (\s -> fromIntegral (G.runGet G.getWord16le (s)))
                32 -> (readBytes 4) >>= pure . (\s -> fromIntegral (G.runGet G.getWord32le (s)))
                64 -> (readBytes 4) >>= pure . (\s -> fromIntegral (G.runGet G.getWord32le (s)))
      >>= \i -> modify (\x -> x { dsImmed = Just $ Op_Imm (Immediate (BitSize $ min ow 32) (i)) } )

immL :: Disassembler ()
immL = do
    ow <- gets dsOpWidth
    case ow of  8 ->  (readBytes 1) >>= pure . (\s -> fromIntegral (G.runGet G.getWord8 (s)))
                16 -> (readBytes 2) >>= pure . (\s -> fromIntegral (G.runGet G.getWord16le (s)))
                32 -> (readBytes 4) >>= pure . (\s -> fromIntegral (G.runGet G.getWord32le (s)))
                64 -> (readBytes 8) >>= pure . (\s -> fromIntegral (G.runGet G.getWord64le (s)))
      >>= \i -> modify (\x -> x { dsImmed = Just $ Op_Imm (Immediate (BitSize ow) (i)) } )

imm'' :: Disassembler Word64
imm'' = fromIntegral <$> (lift anyWord8) <* adv 1

moffs :: Disassembler ()
moffs = do
    aw <- (\x -> if x then 32 :: Int else 64 :: Int) <$> dsA32
    ow <- gets dsOpWidth
    seg <- gets dsSegOverride
    case aw of 32 -> (readBytes 4) >>= pure . (\s -> fromIntegral (G.runGet G.getWord32le (s)))
               64 -> (readBytes 8) >>= pure . (\s -> fromIntegral (G.runGet G.getWord64le (s)))
      >>= \disp -> modify (\x -> x {
                dsMoffset = Just $ Op_Mem (BitSize ow) (BitSize aw) RegNone RegNone 0 disp seg
                })

moffset :: Disassembler Operand
moffset = (gets dsMoffset) >>= pure . fromJust


immB :: Disassembler ()
immB = imm' 1 G.getWord8

immW :: Disassembler ()
immW = imm' 2 G.getWord16le

imm' :: (Integral t) => Int -> G.Get t -> Disassembler ()
imm' b f = (readBytes b) >>= pure . (\s -> fromIntegral (G.runGet f (s)))
                         >>= \i -> modify (\x -> x { dsImmed = Just $ Op_Imm $ Immediate (BitSize $ 8 * b) i } )

displ :: Disassembler Operand
displ = do  s <- (readBytes 1)
            disp <- pure (fromIntegral (G.runGet G.getInt8 (s)))
            eip <- gets dsOffset
            let iv = bits 0 64 (eip + disp)
                imm = Immediate (BitSize 64) iv
              in pure (Op_Jmp imm)

displW :: Disassembler Operand
displW = do s <- (readBytes 2)
            disp <- pure (fromIntegral (G.runGet G.getInt16le (s)))
            eip <- gets dsOffset
            let iv = bits 0 64 (eip + disp)
                imm = Immediate (BitSize 64) iv
              in pure (Op_Jmp imm)

displD :: Disassembler Operand
displD = do s <- (readBytes 4)
            disp <- pure (fromIntegral (G.runGet G.getInt32le (s)))
            eip <- gets dsOffset
            let iv = bits 0 64 (eip + disp)
                imm = Immediate (BitSize 64) iv
              in pure (Op_Jmp imm)

pfx :: Disassembler [Prefix]
pfx = do st <- get
         let ff a b = if (a st) then [b] else []
             pfx = (ff dsLock PrefixLock)
                ++ (ff dsRepNE PrefixRepNE)
                ++ (ff dsRep PrefixRep)
                ++ (ff dsOpWidthOverride PrefixO16)
                ++ (ff dsAdWidthOverride PrefixA32)
                ++ (maybe [] ((:[]) . PrefixSeg) (dsSegOverride st))
                ++ (maybe [] ((:[]) . PrefixRex) (dsRex st))
            in pure pfx

modrm_rm :: Disassembler Operand
modrm_rm = do rm <- modRM_rm <$> fromJust <$> gets dsModRM
              ow <- gets dsOpWidth
              rexB <- dsRexB
              rex <- gets dsRex
              let sr = selectreg ow rexB rex
                in pure (rm ow sr)

modrm_reg :: Disassembler Operand
modrm_reg = do b'reg <- modRM_breg <$> fromJust <$> (gets dsModRM)
               opWidth <- gets dsOpWidth
               rexR <- dsRexR
               rex <- gets dsRex
               let reg = selectreg opWidth rexR rex b'reg
                 in pure $ Op_Reg reg

modrm_sreg :: Disassembler Operand
modrm_sreg = do b'reg <- modRM_breg <$> fromJust <$> (gets dsModRM)
                case b'reg of 0 -> pure (Op_Reg $ RegSeg ES)
                              1 -> pure (Op_Reg $ RegSeg CS)
                              2 -> pure (Op_Reg $ RegSeg SS)
                              3 -> pure (Op_Reg $ RegSeg DS)
                              4 -> pure (Op_Reg $ RegSeg FS)
                              5 -> pure (Op_Reg $ RegSeg GS)
                              6 -> pure (Op_Reg $ RegSeg SR6) -- not in Intel spec
                              7 -> pure (Op_Reg $ RegSeg SR7) -- not in Intel spec
                              _ -> fail "invalid"

immed :: Disassembler Operand
immed = (gets dsImmed) >>= pure . fromJust

reg :: Word8 -> (Int -> Int) -> Disassembler Operand
reg rr ov = do
    ow <- gets dsOpWidth
    rex <- gets dsRex
    rexB <- dsRexB
    let r = selectreg (ov ow) rexB rex rr
      in pure $ Op_Reg r

freg :: Word8 -> Disassembler Operand
freg rr = (pure . Op_Reg . RegFPU) ( [ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7] !! (fromIntegral rr) )

accum :: Disassembler Operand
accum = do
    ow <- gets dsOpWidth
    rex <- gets dsRex
    let r = selectreg ow 0 Nothing 0
      in pure $ Op_Reg r

-- type DisassemblerSingle = Word8 -> DisassemblerState -> Get Instruction
-- type OpcodeMap = Map.Map Word8 DisassemblerSingle
--
dsO16 :: Disassembler Bool
dsO16 = gets dsOpWidthOverride
dsA32 :: Disassembler Bool
dsA32 = gets dsAdWidthOverride

dsRexW :: Disassembler Word8
dsRexW = gets dsRex >>= \rex -> pure $ bits 3 1 (fromMaybe 0 rex)
dsRexR :: Disassembler Word8
dsRexR = gets dsRex >>= \rex -> pure $ bits 2 1 (fromMaybe 0 rex)
dsRexX :: Disassembler Word8
dsRexX = gets dsRex >>= \rex -> pure $ bits 1 1 (fromMaybe 0 rex)
dsRexB :: Disassembler Word8
dsRexB = gets dsRex >>= \rex -> pure $ bits 0 1 (fromMaybe 0 rex)

dsSeg :: Disassembler (Maybe SReg)
dsSeg = gets dsSegOverride

-- dsA32 ds = PrefixA32 `elem` (dsPfx ds)
-- dsRep ds = PrefixRep `elem` (dsPfx ds)
-- dsSeg ds = listToMaybe (catMaybes (map (\p -> case p of (PrefixSeg s) -> Just s; _ -> Nothing) (dsPfx ds)))

modrm :: Disassembler ()
modrm = do
    val      <- (lift $ anyWord8) <* adv 1
    aWidth   <- (\x -> if x then 32 :: Int else 64 :: Int) <$> dsA32
    rex      <- gets dsRex
    rexR     <- dsRexR
    rexX     <- dsRexX
    rexB     <- dsRexB
    opWidth  <- gets dsOpWidth
    so       <- dsSeg
    let b'mod = bits 6 2 val
        b'reg = bits 3 3 val
        b'rm  = bits 0 3 val
        hasSib = (b'mod /= 3 && b'rm == 4)
        dispSize = case (b'mod, b'rm) of
            (0,5) -> Just 32
            (1,_) -> Just 8
            (2,_) -> Just 32
            _     -> Nothing
      in do
        (sib,dispSize') <- if hasSib then (parseSib b'mod dispSize rex rexB rexX aWidth) <$> getWord8
                                     else return ((RegNone,RegNone,0),dispSize)
        disp <- case dispSize' of
                    Just 8 -> fromIntegral <$> getInt8
                    Just 32 -> fromIntegral <$> getInt32le
                    _  -> return 0
        let rm = case (b'mod, b'rm) of
                (3,_) -> \ow sr -> Op_Reg (sr b'rm)
                (0,5) -> \ow sr -> Op_Mem (BitSize ow) (BitSize aWidth) ((if aWidth == 64 then Reg64 else Reg32) RIP) RegNone 0 disp so
                (0,4) -> \ow sr -> let (br, ir, sc) = sib in Op_Mem (BitSize ow) (BitSize aWidth) br ir sc disp so
                (1,4) -> \ow sr -> let (br, ir, sc) = sib in Op_Mem (BitSize ow) (BitSize aWidth) br ir sc disp so
                (2,4) -> \ow sr -> let (br, ir, sc) = sib in Op_Mem (BitSize ow) (BitSize aWidth) br ir sc disp so
                (_,_) -> \ow sr -> Op_Mem (BitSize ow) (BitSize aWidth) (selectreg aWidth rexB rex b'rm ) RegNone 0 disp so
          in
            modify $ \x -> x { dsModRM = Just $ ModRM rm b'reg b'mod b'rm }
    where
        getWord8   = (lift $ anyWord8) <* adv 1
        getInt8    = (readBytes 1) >>= \s -> pure (G.runGet G.getInt8 (s))
        getInt32le = (readBytes 4) >>= \s -> pure (G.runGet G.getInt32le (s))

        parseSib m dispSize rex rexB rexX aw sib = let
                                 br = (bits 0 3 sib)
                                 ir = (bits 3 3 sib)
                                 ss = (bits 6 2 sib)
                                 sp = (case aw of 16 -> Reg16; 32 -> Reg32; 64 -> Reg64) RSP
                                 breg = selectreg aw rexB rex br
                                 ireg = selectreg aw rexX rex ir
                                 sf = case ss of { 0 -> 1; 1 -> 2; 2 -> 4; 3 -> 8 }
                            in case (m, br) of (0, 5) -> ((RegNone, if ireg == sp then RegNone else ireg, sf), Just 32)
                                               _      -> ((breg, if ireg == sp then RegNone else ireg, sf), dispSize)

selectreg :: Int -> Word8 -> Maybe Word8 -> Word8 -> Register
selectreg opWidth rex rex' reg = let
                rvec' = case rex of
                        1 -> [R8, R9, R10, R11, R12, R13, R14, R15]
                        0 -> [RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI]
                rvec = case (opWidth, rex') of
                        (8, Just _)  -> map (\i -> Reg8 i HalfL) rvec'
                        (8, Nothing) -> [Reg8 RAX HalfL, Reg8 RCX HalfL, Reg8 RDX HalfL, Reg8 RBX HalfL,
                                         Reg8 RAX HalfH, Reg8 RCX HalfH, Reg8 RDX HalfH, Reg8 RBX HalfH]
                        (16, _) -> map Reg16 rvec'
                        (32, _) -> map Reg32 rvec'
                        (64, _) -> map Reg64 rvec'
                        _ -> error$ show (rvec,rvec',opWidth,rex,rex',reg)
            in rvec !! (fromIntegral reg)

bits :: (Integral a, Bits a) => Int-> Int -> a -> a
bits s l i = fromIntegral $ (i `shiftR` s) .&. ((1 `shiftL` l) - 1)

bitTest :: Int -> Maybe Prefix -> Bool
bitTest i v = case v of
                Nothing -> False
                Just (PrefixRex n) -> n .&. (bit i) /= 0


data ModRM = ModRM {
        modRM_rm :: Int -> (Word8 -> Register) -> Operand
      , modRM_breg :: Word8
      , modRM_mod :: Word8
      , modRM_brm :: Word8
      }
