module Data.SSA
    ( Label
    , Storage
    , Special
    , Instruction
    , Statement
    , Program
    , fromPreSSA
    ) where

import           Data.Generic                   ( ImmutableVariable
                                                , Phi(Phi)
                                                , Variable
                                                    ( ImmutableVar
                                                    , MutableVar
                                                    )
                                                , mapI
                                                , mapP
                                                )
import qualified Data.Generic                  as Generic
import qualified Data.PreSSA                   as PreSSA
import           Generic_Datastructures         ( AddressWord64 )
import qualified Generic_Datastructures        as GD
import           X86_Datastructures             ( Opcode
                                                , Prefix
                                                )

--------------------------------------------------------------------------------
-- DATA
--------------------------------------------------------------------------------

type Label = AddressWord64

type Storage = ImmutableVariable

type Special = Phi Storage

type Instruction = GD.Instruction Label Storage Prefix Opcode Int

type Statement = Generic.Statement Label Storage Prefix Opcode Int Special

type Program = Generic.Program Label Storage Prefix Opcode Int Special


--------------------------------------------------------------------------------
-- PreSSA -> SSA
--------------------------------------------------------------------------------


fromPreSSA :: PreSSA.Program -> Program
fromPreSSA = mapP (map instructionFromPreSSA) specialFromPreSSA

specialFromPreSSA :: PreSSA.Special -> Special
specialFromPreSSA (Phi args) = Phi $ storageFromPreSSA <$> args

storageFromPreSSA :: PreSSA.Storage -> Storage
storageFromPreSSA (ImmutableVar v) = v
storageFromPreSSA (MutableVar _) =
    error "Only SSA verified PreSSA programs may be translated"

instructionFromPreSSA :: PreSSA.Instruction -> Instruction
instructionFromPreSSA = mapI storageFromPreSSA
