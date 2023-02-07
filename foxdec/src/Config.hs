{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}



{-|
Module      : Config
Description : Some customizable constants.
-}





module Config
 where

import Dhall
import GHC.Generics
import Data.Text (pack)
import qualified Data.Serialize as Cereal hiding (get,put)
import Control.DeepSeq


-- | A datastructure storing all configurable options
data Config = Config { 
  continue_on_unknown_instruction :: Bool,
  -- ^ When encountering an unknown instruction do we either
  --   * report it to stderr but continue (True), or
  --   * exit with an error message (False)?
  -- Sane default: True
  
  generate_pdfs :: Bool,
  -- ^ Do we call graphviz to generate PDFs?
  -- Set to true for small examples, false for larger ones.
  
  verbose_logs :: Bool,
  -- ^ Must the invariants be stored in the logs?
  -- Sane default: False 

  store_preconditions_in_L0 :: Bool,
  -- ^ Must preconditions be stored in the .L0 file?
  -- Set to true for small examples, false for larger ones.
  -- Sane default: False 

  store_assertions_in_L0 :: Bool,
  -- ^ Must assertions be stored in the .L0 file?
  -- Set to true for small examples, false for larger ones.
  -- Sane default: False 

  max_time :: Natural,
  -- ^ The maximum verification time in seconds per function
  -- Sane default: 30 minutes = 000000 * 60 * 30 = 1800000000
  
  max_num_of_cases :: Natural,
  -- ^ The maximum number of separate concrete cases considered non-deterministically, before abstraction is applied.
  -- Has no effect on soundness, but lower values cause more abstraction.
  -- Sane default: 5

  max_num_of_bases :: Natural,
  -- ^ The maximum number of pointer bases a bottom-expression may have, before more asbtraction is applied.
  -- Has no effect on soundness, but lower values cause more abstraction.
  -- Sane default: 25
  
  max_num_of_sources :: Natural,
  -- | The maximum number of sources a bottom-expression may have, before resorting to rock-bottom.
  -- Has no effect on soundness, but lower values cause more abstraction.
  -- Sane default: 100


  max_jump_table_size :: Natural,
  -- ^ A coarse overapproximation of the maximum number of entries in a jump table.
  -- Does not affect soundness, but if the value is set too low, then more indirections may be left unresolved.
  -- Sane default: 20000
  
  max_expr_size :: Natural
  -- ^ The maximum size of an expression (counting each operator and each leaf as 1), before a symbolic expression is abstracted to rock bottom.
  -- Does not affect soundness, but if the value is set too low, then the results becomes overly overapproximative.
  -- Sane default: 3000
 }
 deriving (Generic, Show)


instance FromDhall Config
instance Cereal.Serialize Config
instance NFData Config

-- | Given a filename, parse a config in the Dhall language
-- See: https://dhall-lang.org
parse_config ::
     String    -- ^ The filename
  -> IO Config
parse_config = input auto . pack


