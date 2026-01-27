{-# LANGUAGE DeriveGeneric #-}


module Data.Indirection where



import Base

import Data.JumpTarget
import Data.X86.Instruction



import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import Data.Word

import qualified Data.Serialize as Cereal hiding (get,put)
import Control.DeepSeq
import GHC.Generics (Generic)





-- TODO MOVE
-- | A jump table with :
--   index: an operand containing a bounded index at the beginning of execution of the block
--   bound: the bound on idx
--   trgt: the operand containg the jump target at the end of execution of the block
--   tbl: a table from values of idx to resulting jump targets
data JumpTable = JumpTable {
  jtbl_index  :: Operand,
  jtbl_bound  :: Int,
  jtbl_target :: Operand,
  jtbl_table  :: IM.IntMap Word64,
  jtbl_base   :: Word64
 }
 deriving (Generic, Eq,Ord)

-- | An indirection is either a jump table or a set of resolved jump targets
data Indirection = Indirection_JumpTable JumpTable | Indirection_Resolved ResolvedJumpTarget | Indirection_Unresolved
 deriving (Generic, Eq,Ord)

type Indirections = S.Set Indirection

instance Show JumpTable where
  show (JumpTable idx bnd trgt tbl base) = "JumpTable@0x" ++ showHex base ++ ": " ++ show idx ++ " < " ++ show bnd ++ " --> " ++ show trgt ++ " in " ++ showHex_set (IS.fromList $ map fromIntegral $ IM.elems tbl)

instance Show Indirection where
  show (Indirection_JumpTable tbl)  = show tbl
  show (Indirection_Resolved  trgt) = show trgt
  show (Indirection_Unresolved)     = "Unresolved"


instance Cereal.Serialize JumpTable
instance Cereal.Serialize Indirection

instance NFData JumpTable
instance NFData Indirection
