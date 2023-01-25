{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict, DeriveGeneric#-}

{-|
Module      : JSON
Description : Provides a taxonomy for the generated output, as well as JSON functionality.
-}

module Data.JSON where


import Base

import Generic.Binary

import Analysis.Context

import qualified Generic.Address as GA
import qualified Generic.Operand as GO
import qualified Generic.Instruction as GI
import qualified X86.Instruction as X86
import X86.Opcode
import X86.Prefix
import X86.Register
import Generic.HasSize (HasSize(sizeof))

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Maybe (fromJust,catMaybes,mapMaybe)
import Data.List 
import Data.Foldable
import Data.Word
import Data.Aeson
import GHC.Generics





-- * Taxonomy

-- | __Address__
data Address =
    AddressRegister Register      -- ^ Reading a pointer from a storage
  | AddressImm Word64             -- ^ Immediate value 
  | AddressPlus Address Address   -- ^ Plus
  | AddressMinus Address Address  -- ^ Minus
  | AddressTimes Address Address  -- ^ Times
  deriving Generic


-- | __Operand__
data Operand =
    Memory Address Int           -- ^ A region in memory, whose address is stored in the given state part and whose size in bytes is given in the Int
  | EffectiveAddress Address     -- ^ An address itself, but not the value stored at the address.
  | Reg Register                 -- ^ A storage location such as a register or a variable
  | Immediate Word64             -- ^ An immediate value
  deriving Generic


-- | __Instruction__
data Instruction = Instruction {
  addr   :: Word64,            -- ^ address
  prefix :: Maybe Prefix,      -- ^ prefix, e.g., lock or repz
  opcode :: Opcode,            -- ^ opcode/mnemonic
  dest   :: Maybe Operand,     -- ^ -- ^ optional: destination operand
  srcs   :: [Operand],         -- ^ optional: operand
  size   :: Int                -- ^ size of instruction
 }



