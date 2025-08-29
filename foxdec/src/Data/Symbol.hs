{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict, DeriveGeneric, StandaloneDeriving #-}


module Data.Symbol where

import Base
import Data.Word 
import GHC.Generics
import qualified Data.Serialize as Cereal hiding (get,put)
import qualified Numeric (showHex)

-- | An address a0 can have a symbol.
--
-- PointerToFunction:
-- E.g:
-- 		0xcfe0 --> malloc
-- Means that reading 8 bytes from address 0xcfe0 produces a pointer to malloc.
-- Thus an instruction: "CALL qword ptr [0xcfe0]" can be seen as "CALL malloc".
--
-- PointerToObject:
-- E.g.:
--    0xd0a8 --> stdout
-- Means that "mov rdi,QWORD PTR [0xd0a8]" can be seen as "mov rdi, QWORD PTR [stdout]"
--
-- Relocated_ResolvedObject
-- E.g.:
--    0xc0fc0 "environ" -> 0xc1340
-- Sometimes, a relocation has been resolved during linking. In that case, it is no longer an external object.
-- For example, there may be a relocation that maps address 0xc0fc0 to symbol "environ".
-- However, that symbol is an object with an address (e.g., 0xc1340) that itself has been relocated.
-- Symbol "environ" now no longer is an external symbol.
-- Instead, we have *environ = &object, where "object" is the object that 0xc1340 is relocated to.
data Symbol = 
    PointerToExternalFunction String -- ^ Address a0 is a pointer to memory storing the entry of a function
  | PointerToInternalFunction String Word64 -- ^ Address a0 is a pointer to memory storing the entry of a function
  | PointerToObject           String Bool -- ^ Address a0 can be replaced by the GOT entry of the string, e.g., "stdout wrt ..got" or "optind wrt ..got"
  | AddressOfObject           String Bool -- ^ Address a0 can be replaced by the string, e.g., "stdout" or "optind"
  | AddressOfLabel            String Bool -- ^ Address a0 can be replaced by the string.
  | Relocated_ResolvedObject  String Word64 -- ^ At linking time internally resolved relocation
  deriving (Generic,Eq,Ord)

instance Show Symbol where
  show (PointerToExternalFunction l)   = "&" ++ l
  show (PointerToInternalFunction l a) = "&" ++ l ++ "@0x" ++ showHex a
  show (PointerToObject o ex)          = "&" ++ o ++ "_" ++ show_ex ex
  show (AddressOfObject o ex)          = o ++ "_" ++ show_ex ex
  show (AddressOfLabel  l ex)          = l ++ "_" ++ show_ex ex
  show (Relocated_ResolvedObject o a)  = o ++ "@0x" ++ (if a < 0 then Numeric.showHex (fromIntegral a :: Word64) "" else Numeric.showHex a "")

show_ex True  = "ex"
show_ex False = "in"



instance Cereal.Serialize Symbol

