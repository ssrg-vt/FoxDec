-----------------------------------------------------------------------------
-- |
-- Some customizable constants
-----------------------------------------------------------------------------

module Config
 where

-- | The maximum number of separate concrete cases considered non-deterministically
max_num_of_cases :: Int
max_num_of_cases   = 5

-- | The maximum number of pointer bases a bottom-expression may have
max_num_of_bases :: Int
max_num_of_bases   = 25

-- | The maximum number of sources a bottom-expression may have
max_num_of_sources :: Int
max_num_of_sources = 150

-- | An overapproximation of the maximum number of entries in a jump table.
-- Does not affect soundness, but if the value is set too low, then more indirections may be left unresolved.
max_jump_table_size = 20000
