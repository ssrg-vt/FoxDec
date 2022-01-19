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


