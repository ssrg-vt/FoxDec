{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, Strict, DeriveGeneric, StandaloneDeriving #-}


module Data.Symbol where

import Base
import Data.Word 
import Data.Int
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
  | PointerToObject           String Bool Int64 (Maybe String)-- ^ Address a0 can be replaced by the GOT entry of the string, e.g., "stdout wrt ..got" or "optind wrt ..got" + an addend
  | AddressOfObject           String Bool -- ^ Address a0 can be replaced by the string, e.g., "stdout" or "optind"
  | AddressOfLabel            String Bool -- ^ Address a0 can be replaced by the string.
  | Relocated_ResolvedObject  String Word64 Int64 -- ^ At linking time internally resolved relocation with addend
  | TLS_Relative              String -- ^ The symbol is relative to the thread local storage
  | TLS_Module                String -- ^ The symbolis a TLS module ID
  deriving (Generic,Eq,Ord)

show_symbol_table_entry (a0,sym) = showHex a0 ++ show_symbol sym
 where 
  show_symbol (PointerToExternalFunction f)         = " --> " ++ f
  show_symbol (PointerToInternalFunction f a1)      = " --> " ++ f ++ "@0x" ++ showHex a1
  show_symbol (PointerToObject o b addend Nothing)  = " --> " ++ o ++ show_addend addend ++ show_in_ex b "object"
  show_symbol (PointerToObject o b addend (Just l)) = " === " ++ l ++ " --> " ++ o ++ show_addend addend ++ show_in_ex b "object"
  show_symbol (AddressOfObject l b)                 = " === " ++ l ++ show_in_ex b "object"
  show_symbol (AddressOfLabel f b)                  = " === " ++ f ++ show_in_ex b "label"
  show_symbol (Relocated_ResolvedObject l a addend) = " --> " ++ l ++ "@0x" ++ showHex a ++ show_addend addend ++ " (object)"
  show_symbol (TLS_Relative l)                      = " === " ++ l ++ "@TLS"
  show_symbol (TLS_Module l)                        = " === " ++ l ++ "@DTPMOD"

  show_addend 0      = ""
  show_addend addend = "+0x"++showHex addend

  show_export (a,f) = showHex a ++ ": " ++ f ++ " (exported)"

  show_in_ex True  ty = " (external " ++ ty ++ ")" 
  show_in_ex False ty = " (internal " ++ ty ++ ")" 


instance Cereal.Serialize Symbol

