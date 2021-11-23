

theory X86_Parse_Hoare_Triples
  imports X86_InstructionSemantics
  keywords "htriple" :: thy_decl

  and "Separations" "Assertions" "Pre" "Post" "Instruction" "FunctionConstraints" :: quasi_command

begin

context semantics
begin 

lemma ucast_take_bits_8[simp]:
  fixes a :: "'b::len word" 
  assumes "LENGTH('b) > 8"
  shows "((ucast (\<langle>0,8\<rangle>a)::'a ::len word) = ucast (ucast a :: 8 word))"
  using assms
  by (auto simp add: word_eq_iff word_size nth_ucast nth_takebits)

lemma ucast_take_bits_16[simp]:
  fixes a :: "'b::len word" 
  assumes "LENGTH('b) > 16"
  shows "((ucast (\<langle>0,16\<rangle>a)::'a::len word) = ucast (ucast a :: 16 word))"
  using assms
  by (auto simp add: word_eq_iff word_size nth_ucast nth_takebits)

lemma ucast_take_bits_32[simp]:
  fixes a :: "'b::len word" 
  assumes "LENGTH('b) > 32"
  shows "((ucast (\<langle>0,32\<rangle>a)::'a::len word) = ucast (ucast a :: 32 word))"
  using assms
  by (auto simp add: word_eq_iff word_size nth_ucast nth_takebits)

lemma ucast_ucast_ucast[simp]:
  assumes "LENGTH('c) < LENGTH('b)"
  shows  "(ucast (ucast (ucast (w::'a::len word)::'b::len word)::'c::len word)::'d::len word) = ucast (ucast w::'c::len word)"
  using assms
  by (auto simp add: word_eq_iff nth_ucast)

lemma scast_ucast_ucast[simp]:
  assumes "LENGTH('c) < LENGTH('b)"
  shows  "(scast (ucast (ucast (w::'a::len word)::'b::len word)::'c::len word)::'d::len word) = scast (ucast w::'c::len word)"
  using assms
  by (auto simp add: word_eq_iff nth_ucast nth_scast)

lemma test_bit_mem_read:
  shows "mem_read \<sigma> a si !! n \<Longrightarrow> n < si*8"
  by (auto simp add: mem_read_def test_bit_rcat word_size rev_nth length_read_bytes)

lemma ucast_ucast_mem_read_8[simp]:
assumes "LENGTH('a) > 8"
  shows "(ucast (ucast (mem_read \<sigma> a 1)::'a::len word) :: 8 word) = ucast (mem_read \<sigma> a 1)"
  apply (subst word_eq_iff)+
  using test_bit_mem_read[of \<sigma> a 4] assms
  by (auto simp add: word_size nth_ucast)

lemma ucast_ucast_mem_read_16[simp]:
assumes "LENGTH('a) > 16"
  shows "(ucast (ucast (mem_read \<sigma> a 2)::'a::len word) :: 16 word) = ucast (mem_read \<sigma> a 2)"
  apply (subst word_eq_iff)+
  using test_bit_mem_read[of \<sigma> a 4] assms
  by (auto simp add: word_size nth_ucast)

lemma ucast_ucast_mem_read_32[simp]:
  assumes "LENGTH('a) > 32"
  shows "(ucast (ucast (mem_read \<sigma> a 4)::'a::len word) :: 32 word) = ucast (mem_read \<sigma> a 4)"
  apply (subst word_eq_iff)+
  using test_bit_mem_read[of \<sigma> a 4] assms
  by (auto simp add: word_size nth_ucast)

lemma ucast_ucast_mem_read_1byte[simp]:
assumes "LENGTH('a) > 8" and "LENGTH('b) > 8"
  shows "(ucast (ucast (mem_read \<sigma> a 1)::'a::len word) :: 'b::len word) = ucast (mem_read \<sigma> a 1)"
  apply (subst word_eq_iff)+
  using test_bit_mem_read[of \<sigma> a 1] assms
  apply (auto simp add: word_size nth_ucast)
  by (meson le_eq_less_or_eq le_less_trans)

lemma ucast_ucast_mem_read_2bytes[simp]:
assumes "LENGTH('a) > 16" and "LENGTH('b) > 16"
  shows "(ucast (ucast (mem_read \<sigma> a 2)::'a::len word) :: 'b::len word) = ucast (mem_read \<sigma> a 2)"
  apply (subst word_eq_iff)+
  using test_bit_mem_read[of \<sigma> a 2] assms
  apply (auto simp add: word_size nth_ucast)
  by (meson le_eq_less_or_eq le_less_trans)

lemma ucast_ucast_mem_read_4bytes[simp]:
assumes "LENGTH('a) > 32" and "LENGTH('b) > 32"
  shows "(ucast (ucast (mem_read \<sigma> a 4)::'a::len word) :: 'b::len word) = ucast (mem_read \<sigma> a 4)"
  apply (subst word_eq_iff)+
  using test_bit_mem_read[of \<sigma> a 4] assms
  apply (auto simp add: word_size nth_ucast)
  by (meson le_eq_less_or_eq le_less_trans)

lemma ucast_mem_read_cmp_8[simp]:
fixes \<sigma> :: state and b :: "'a::len word"
assumes "LENGTH('a) > 8"
  shows "(ucast (mem_read \<sigma> a 1)) = b \<longleftrightarrow> ucast (ucast (mem_read \<sigma> a 1)::8 word) = b"
  using test_bit_mem_read[of \<sigma> a 1] test_bit_size[of b]
  by (auto simp add: word_eq_iff word_size nth_ucast nth_takebits)

lemma ucast_mem_read_cmp_16[simp]:
fixes \<sigma> :: state and b :: "'a::len word"
assumes "LENGTH('a) > 16"
  shows "(ucast (mem_read \<sigma> a 2)) = b \<longleftrightarrow> ucast (ucast (mem_read \<sigma> a 2)::16 word) = b"
  using test_bit_mem_read[of \<sigma> a 2] test_bit_size[of b]
  by (auto simp add: word_eq_iff word_size nth_ucast nth_takebits)

lemma ucast_mem_read_cmp_32[simp]:
fixes \<sigma> :: state and b :: "'a::len word"
assumes "LENGTH('a) > 32"
  shows "(ucast (mem_read \<sigma> a 4)) = b \<longleftrightarrow> ucast (ucast (mem_read \<sigma> a 4)::32 word) = b"
  using test_bit_mem_read[of \<sigma> a 4] test_bit_size[of b]
  by (auto simp add: word_eq_iff word_size nth_ucast nth_takebits)






lemma ucast_overwrite[simp]:
  shows "ucast (overwrite 0 n a b) = overwrite 0 n (ucast a) (ucast b)"
  using test_bit_size[of a] test_bit_size[of b]
  by (auto simp add: word_eq_iff nth_ucast overwrite_def nth_shiftl nth_takebits word_size)

lemma takebits_overwrite[simp]:
  shows "(\<langle>0,h\<rangle> (overwrite 0 n a b)) = overwrite 0 n (\<langle>0,h\<rangle> a) (\<langle>0,h\<rangle> b)"
  using test_bit_size[of a] test_bit_size[of b]
  by (auto simp add: word_eq_iff nth_ucast overwrite_def nth_shiftl nth_takebits word_size)

lemma overwrite_overwrite_l[simp]:
  shows "overwrite 0 h (overwrite 0 h a b) c = overwrite 0 h a c"
  by (auto simp add: word_eq_iff nth_ucast overwrite_def nth_shiftl nth_takebits word_size)

lemma overwrite_overwrite_r[simp]:
  shows "overwrite 0 h a (overwrite 0 h b c) = overwrite 0 h a c"
  by (auto simp add: word_eq_iff nth_ucast overwrite_def nth_shiftl nth_takebits word_size)

lemma overwrite_all_8[simp]:
  fixes a :: "8 word"
  shows "overwrite 0 8 a b = b"
  by (auto simp add: word_eq_iff overwrite_def nth_shiftl nth_takebits)

lemma overwrite_all_16[simp]:
  fixes a :: "16 word"
  shows "overwrite 0 16 a b = b"
  by (auto simp add: word_eq_iff overwrite_def nth_shiftl nth_takebits)



lemma ucast_AND[simp]:
  shows "ucast (a AND b) = ucast a AND ucast b"
  by (auto simp add: word_eq_iff nth_ucast)

lemma ucast_OR[simp]:
  shows "ucast (a OR b) = ucast a OR ucast b"
  by (auto simp add: word_eq_iff nth_ucast)

lemma ucast_XOR[simp]:
  shows "ucast (a XOR b) = ucast a XOR ucast b"
  by (auto simp add: word_eq_iff nth_ucast)

lemma take_bits_AND[simp]:
  shows "(\<langle>l,h\<rangle> (a AND b)) = (\<langle>l,h\<rangle> a) AND (\<langle>l,h\<rangle> b)"
  by (auto simp add: word_eq_iff nth_ucast nth_takebits)

lemma take_bits_OR[simp]:
  shows "(\<langle>l,h\<rangle> (a OR b)) = (\<langle>l,h\<rangle> a) OR (\<langle>l,h\<rangle> b)"
  by (auto simp add: word_eq_iff nth_ucast nth_takebits)

lemma take_bits_XOR[simp]:
  shows "(\<langle>l,h\<rangle> (a XOR b)) = (\<langle>l,h\<rangle> a) XOR (\<langle>l,h\<rangle> b)"
  by (auto simp add: word_eq_iff nth_ucast nth_takebits)



lemma dummy:
  assumes "LENGTH('a) < 0" and "LENGTH('b) < 0"
  shows "(ucast (a::'a::len word) ::'b::len word) = x"
  using assms
  by auto

lemma dummy':
  assumes "LENGTH('a) < 0" and "LENGTH('b) < 0"
  shows "(scast (a::'a::len word) ::'b::len word) = x"
  using assms
  by auto








ML_file \<open>X86_Parse_Hoare_Triple.ML\<close>


ML \<open>
(*
  If thm is of the form (thm0 \<and> thm1), return the conjuncts.
*)
fun split_conj thm =
  case (Thm.prop_of thm) of
    (Const ("HOL.Trueprop", _) $ (Const ("HOL.conj", _) $ _ $ _)) => SOME (thm RS @{thm conjunct1} ,thm RS @{thm conjunct2})
  | _ => NONE;

(*
  conjs = thm0 \<and> thm1 \<and> ...
  Searches a conjunct equal to thm.
  If found, insert the found conjunct as assumption.
*)

fun insert_assumption_from_conjunct ctxt conjs thm =
  case split_conj conjs of
    NONE => 
      if Envir.aeconv (Thm.prop_of conjs,Thm.major_prem_of thm) then
        SOME (Method.insert_tac ctxt [conjs])
      else if Envir.aeconv (Thm.prop_of conjs,Thm.major_prem_of thm) then
        SOME (Method.insert_tac ctxt [conjs])
      else
        NONE  
  | SOME (conj0,conj1) =>
      case insert_assumption_from_conjunct ctxt conj0 thm of
        NONE => insert_assumption_from_conjunct ctxt conj1 thm
      | SOME thm' => SOME thm'


fun from_conjunctive_assm_tactic assm ctxt i thm =
   if Thm.no_prems thm then
     Seq.single thm
   else case insert_assumption_from_conjunct ctxt assm thm of
        NONE => Seq.empty
      | SOME tac => Seq.maps (Seq.single o Thm.eq_assumption i) (tac i thm)


infix 1 DETERM_THEN AND_IF_NOT_SOLVED;

fun tac1 DETERM_THEN tac2 = fn thm =>
  let val result = tac1 thm 
  in
    case Seq.pull result of
      NONE => Seq.empty
    | SOME (thm',_) => tac2 thm'
  end

fun tac1 AND_IF_NOT_SOLVED tac2 = fn thm =>
  let val result = tac1 thm 
  in
    case Seq.pull result of
      NONE => tac2 thm
    | SOME (thm',_) => if Thm.nprems_of thm' < Thm.nprems_of thm then Seq.single thm' else tac2 thm'
  end

fun add_simps simp_theorems = fold Simplifier.add_simp simp_theorems

(*
  apply (subst mem_read_mem_write_separate)
  apply (from_conjunctive_assm conjI[OF seps asserts,simplified])
  apply (insert conjI[OF seps asserts,simplified])[1]
  apply (erule conjE)+
  apply (simp (no_asm_simp) add: state_simps BitByte_simps separate_simps)
  apply (simp only: algeba_simps_small)?
  apply (simp add: state_simps BitByte_simps separate_simps)
  apply (simp only: algeba_simps_small)?
*)
fun resolve_mem_read_tactic seps ctxt i =
  let val tac1 = CHANGED (EqSubst.eqsubst_tac ctxt [0] [@{thm mem_read_mem_write_separate}] i)
      val tac2 = HEADGOAL (from_conjunctive_assm_tactic seps ctxt)
      val tac3 = HEADGOAL (Method.insert_tac ctxt [seps])
      val tac4 = REPEAT_DETERM (HEADGOAL (eresolve_tac ctxt [@{thm conjE}]))
      val simp_theorems = @{thms state_simps BitByte_simps separate_simps}
      val ctxt' = add_simps simp_theorems ctxt
      val tac5 = HEADGOAL (asm_simp_tac ctxt')
      val ctxt_only_commutes = add_simps @{thms algebra_simps} (Raw_Simplifier.clear_simpset ctxt)
      val tac6 = HEADGOAL (full_simp_tac ctxt_only_commutes)
      val tac7 = HEADGOAL (asm_full_simp_tac ctxt')
      val tac8 = tac6
  in
    CHANGED (
      tac1 DETERM_THEN 
        (  (DETERM tac2)
        ORELSE 
           (tac3 DETERM_THEN tac4 DETERM_THEN (tac5 AND_IF_NOT_SOLVED tac6 AND_IF_NOT_SOLVED tac7 AND_IF_NOT_SOLVED tac8))
        )
    )
  end
\<close>                                 
method_setup from_conjunctive_assm = \<open>
   Attrib.thm >> (fn (assm) => fn ctxt =>
     SIMPLE_METHOD' (fn i => fn thm =>
        if Thm.no_prems thm then
          Seq.single thm
        else case insert_assumption_from_conjunct ctxt assm thm of
             NONE => Seq.empty
           | SOME tac => Seq.maps (Seq.single o Thm.eq_assumption i) (tac i thm)
        ))
\<close>

method_setup resolve_mem_read = \<open>
   Attrib.thm >> (fn (assm) => fn ctxt => 
      SIMPLE_METHOD' (fn i => fn thm =>
        if Thm.no_prems thm then
          Seq.single thm
        else
          resolve_mem_read_tactic assm ctxt i thm
     ))
\<close>

method_setup eq_assumption = \<open>Scan.succeed (fn ctxt => SIMPLE_METHOD' (PRIMITIVE o Thm.eq_assumption))\<close>

ML \<open>
fun REPEAT_WHILE_CHANGING tac = fn thm =>
  let val result = (CHANGED tac) thm 
  in
    case Seq.pull result of
      NONE => Seq.single thm
    | SOME (thm',_) => REPEAT_WHILE_CHANGING tac thm'
  end

fun REPEAT_WHILE_SOLVING tac = fn thm =>
  let val result = (CHANGED tac) thm 
  in
    case Seq.pull result of
      NONE => Seq.single thm
    | SOME (thm',_) => if Thm.nprems_of thm' < Thm.nprems_of thm then REPEAT_WHILE_SOLVING tac thm' else Seq.single thm
  end

fun FAIL_IF_GENERATES_NEW_GOALS tac = fn thm =>
  let val result = (CHANGED tac) thm 
  in
    case Seq.pull result of
      NONE => Seq.empty
    | SOME (thm',_) => if Thm.nprems_of thm' > Thm.nprems_of thm then Seq.empty else Seq.single thm'
  end
\<close>

method_setup repeat_while_changing = \<open>
  Method.text_closure >> (fn m => fn ctxt => fn _ =>
      CONTEXT_TACTIC (REPEAT_WHILE_CHANGING (NO_CONTEXT_TACTIC ctxt (Method.evaluate m ctxt []))))
\<close>

method_setup fail_if_generates_new_goals = \<open>
  Method.text_closure >> (fn m => fn ctxt => fn _ =>
    CONTEXT_TACTIC (FAIL_IF_GENERATES_NEW_GOALS (NO_CONTEXT_TACTIC ctxt (Method.evaluate m ctxt []))))
\<close>

method_setup repeat_while_solving = \<open>
  Method.text_closure >> (fn m => fn ctxt => fn _ =>
    CONTEXT_TACTIC (REPEAT_WHILE_SOLVING (NO_CONTEXT_TACTIC ctxt (Method.evaluate m ctxt []))))
\<close>


method se_step uses assms seps = (
  (insert assms)[1],
  (erule conjE)+,
  (  (simp (no_asm_use) add: state_simps BitByte_simps separate_simps)? ;
     (repeat_while_changing
         \<open>(simp only: algebra_simps)?,
          (simp (no_asm_simp) add: state_simps BitByte_simps separate_simps)?,
          (fail_if_generates_new_goals \<open>resolve_mem_read seps\<close>)?\<close>
     )
  )
 )

method htriple_solver uses seps assms = (
    (simp add: semantics_simps state_simps BitByte_simps separate_simps del: Let_def),
    (auto simp add: semantics_simps state_simps BitByte_simps separate_simps)?,
    (repeat_while_solving \<open>se_step assms: assms seps: seps\<close>)
 )

(*
  apply (simp add: semantics_simps state_simps BitByte_simps separate_simps del: Let_def)
  apply (auto simp add: semantics_simps state_simps BitByte_simps separate_simps)?
  apply (repeat_while_changing \<open>se_step assms: assms seps: conjI[OF seps asserts,simplified]\<close>)
  apply (se_step assms: assms seps: conjI[OF seps asserts,simplified])

  apply (insert assms)[1]
  apply (erule conjE)+
  apply (simp (no_asm_use) add: state_simps BitByte_simps separate_simps)
  (* repeat *)
  apply (simp only: algebra_simps)
  apply (simp (no_asm_simp) add: state_simps BitByte_simps separate_simps)
  apply (simp only: algebra_simps)
  apply (fail_if_generates_new_goals \<open>resolve_mem_read conjI[OF seps asserts,simplified]\<close>)

*)

end



abbreviation ucast\<^sub>3\<^sub>2\<^sub>_\<^sub>6\<^sub>4 :: "32 word \<Rightarrow> 64 word"
  where "ucast\<^sub>3\<^sub>2\<^sub>_\<^sub>6\<^sub>4 \<equiv> ucast"
abbreviation ucast\<^sub>6\<^sub>4\<^sub>_\<^sub>3\<^sub>2 :: "64 word \<Rightarrow> 32 word"
  where "ucast\<^sub>6\<^sub>4\<^sub>_\<^sub>3\<^sub>2 \<equiv> ucast"
abbreviation ucast\<^sub>1\<^sub>6\<^sub>_\<^sub>6\<^sub>4 :: "16 word \<Rightarrow> 64 word"
  where "ucast\<^sub>1\<^sub>6\<^sub>_\<^sub>6\<^sub>4 \<equiv> ucast"
abbreviation ucast\<^sub>6\<^sub>4\<^sub>_\<^sub>1\<^sub>6 :: "64 word \<Rightarrow> 16 word"
  where "ucast\<^sub>6\<^sub>4\<^sub>_\<^sub>1\<^sub>6 \<equiv> ucast"
abbreviation ucast\<^sub>8\<^sub>_\<^sub>6\<^sub>4 :: "8 word \<Rightarrow> 64 word"
  where "ucast\<^sub>8\<^sub>_\<^sub>6\<^sub>4 \<equiv> ucast"
abbreviation ucast\<^sub>6\<^sub>4\<^sub>_\<^sub>8 :: "64 word \<Rightarrow> 8 word"
  where "ucast\<^sub>6\<^sub>4\<^sub>_\<^sub>8 \<equiv> ucast"
abbreviation ucast\<^sub>3\<^sub>2\<^sub>_\<^sub>1\<^sub>6 :: "32 word \<Rightarrow> 16 word"
  where "ucast\<^sub>3\<^sub>2\<^sub>_\<^sub>1\<^sub>6 \<equiv> ucast"
abbreviation ucast\<^sub>8\<^sub>_\<^sub>3\<^sub>2 :: "8 word \<Rightarrow> 32 word"
  where "ucast\<^sub>8\<^sub>_\<^sub>3\<^sub>2 \<equiv> ucast"
abbreviation ucast\<^sub>3\<^sub>2\<^sub>_\<^sub>8 :: "32 word \<Rightarrow> 8 word"
  where "ucast\<^sub>3\<^sub>2\<^sub>_\<^sub>8 \<equiv> ucast"
abbreviation ucast\<^sub>1\<^sub>6\<^sub>_\<^sub>8 :: "16 word \<Rightarrow> 8 word"
  where "ucast\<^sub>1\<^sub>6\<^sub>_\<^sub>8 \<equiv> ucast"
abbreviation ucast\<^sub>8\<^sub>_\<^sub>1\<^sub>6 :: "8 word \<Rightarrow> 16 word"
  where "ucast\<^sub>8\<^sub>_\<^sub>1\<^sub>6 \<equiv> ucast"


end