{
  continue_on_unknown_instruction = True,
  -- ^ When encountering an unknown instruction do we either
  --   * report it to stderr but continue (True), or
  --   * exit with an error message (False)?
  -- NOTE: this is the only configurable option that may affect soundness,
  -- formally this should be set to False.
  -- Sane default: True


  generate_pdfs = False,
  -- ^ Do we call graphviz to generate PDFs?
  -- Set to true for small examples, false for larger ones.
  -- Sane default: False 


  verbose_logs = False,
  -- ^ Must the invariants be stored in the logs?
  -- Sane default: False 


  store_preconditions_in_report = False,
  -- ^ Must preconditions be stored in the .report file?
  -- Set to true for small examples, false for larger ones.
  -- Sane default: False 


  store_assertions_in_report = False,
  -- ^ Must assertions be stored in the .report file?
  -- Set to true for small examples, false for larger ones.
  -- Sane default: False 


  max_time = 1000000 * 60 * 30,
  -- ^ The maximum verification time in microseconds per function
  -- Sane default: 30 minutes = 1000000 * 60 * 30 = 1800000000


  max_num_of_cases = 5,
  -- ^ The maximum number of separate concrete cases considered non-deterministically, before abstraction is applied.
  -- Has no effect on soundness, but lower values cause more abstraction.
  -- Sane default: 5


  max_num_of_bases = 25,
  -- ^ The maximum number of pointer bases a bottom-expression may have, before more asbtraction is applied.
  -- Has no effect on soundness, but lower values cause more abstraction.
  -- Sane default: 25
  

  max_num_of_sources = 75,
  -- | The maximum number of sources a bottom-expression may have, before resorting to rock-bottom.
  -- Has no effect on soundness, but lower values cause more abstraction.
  -- Sane default: 75


  max_jump_table_size = 20000,
  -- ^ A coarse overapproximation of the maximum number of entries in a jump table.
  -- Does not affect soundness, but if the value is set too low, then more indirections may be left unresolved.
  -- Sane default: 20000
  

  max_expr_size = 3000
  -- ^ The maximum size of an expression (counting each operator and each leaf as 1), before a symbolic expression is abstracted to rock bottom.
  -- Does not affect soundness, but if the value is set too low, then the results becomes overly overapproximative.
  -- Sane default: 3000
}
