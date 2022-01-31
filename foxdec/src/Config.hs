{-|
Module      : Config
Description : Some customizable constants.
-}

module Config
 where


-- | When encountering an unknown instruction do we either
--   * report it to stderr but continue (True), or
--   * exit with an error message (False)?
continue_on_unknown_instruction = True

-- | The maximum number of separate concrete cases considered non-deterministically, before abstraction is applied.
max_num_of_cases :: Int
max_num_of_cases   = 5

-- | The maximum number of pointer bases a bottom-expression may have, before more asbtraction is applied.
max_num_of_bases :: Int
max_num_of_bases   = 25

-- | The maximum number of sources a bottom-expression may have, before resoring to rock-bottom.
max_num_of_sources :: Int
max_num_of_sources = 150

-- | A coarse overapproximation of the maximum number of entries in a jump table.
-- Does not affect soundness, but if the value is set too low, then more indirections may be left unresolved.
max_jump_table_size :: Int
max_jump_table_size = 20000

-- | The maximum size of an expression (counting each operator and each leaf as 1), before a symbolic expression is abstracted to rock bottom.
-- Does not affect soundness, but if the value is set too low, then the results becomes overly overapproximative.
max_expr_size :: Int
max_expr_size = 3000

