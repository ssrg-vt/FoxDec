theory X86_Concrete_Semantics
  imports X86_Base
begin


text \<open>We define a concrete state. A concrete value is either an immediate value or  the value  ``Tainted''.
A value can become tainted if it is the result from reading partially overlapping regions.
Even though a value can be tainted, this is still a concrete semantics in the sense that pointers have
immediate 64-bit values and immediate sizes.
Thus, there is no pointer aliasing decision problems: a write is always to a concrete pointer.
If a write occurs to Tainted, then all memory is trashed.
\<close>
datatype cvalue = CValue "64 word" | Tainted



text\<open>Concrete memory assigns concrete values to concrete pointers (+region sizes). 
This allows storing cvalues in regions with different sizes, and stores current alignment relations.\<close>
type_synonym cmem = "((cvalue \<times> 64 word) \<times> cvalue) list"


text\<open>
We explain this by example.
Consider the program:
  MOV QWORD PTR [RAX], RAX
  MOV QWORD PTR [RAX+8], 42
  MOV RBX, DWORD PTR [RAX]
  MOV QWORD PTR [RBX], 43
  MOV RCX, DWORD PTR [RAX+4]


Consider a state with a @{typ cvalue} in register RAX: @{term "CValue 4000"}.
Running the above program writes that value to memory, writes 42 to a separate region, retrieves a value from memory, and writes to it as a pointer. 
This produces for memory:
@{term "[((CValue 4000,8), CValue 43), 
         ((CValue 4008,8), CValue 42)] :: cmem"}
The last read partially overlaps with earlier ones, and thus RCX becomes tainted.
See lemma example_program_concrete below.
\<close>


text \<open>A concrete state assigns concrete values to registers (identified by strings), and contains memory.\<close>
record cstate =
  cregs :: "string \<Rightarrow> cvalue"
  cmem ::  cmem


text \<open>
  Second, we define what an instruction is. 
  An operand is either a register (identified by a string), or an unresolved memory address consisting of a base-register, 
  a constant times an index-register, and the non-zero size of the accessed region in bytes.
\<close>
typedef pos = "{ n :: 64 word  . n > 0}"
  using word_less_1 by blast
datatype dst_operand = ToReg string | ToMemory string "64 word" pos
datatype src_operand = FromReg string | FromMemory string "64 word" pos | FromImmediate "64 word"

datatype operation = Mov | Add | Sub | Mul | GenericOp string "64 word \<Rightarrow> 64 word \<Rightarrow> 64 word"

record instruction =
  i_dest :: dst_operand
  i_srcs :: "src_operand list"
  i_op   :: operation
  i_size :: "64 word"


locale binary = 
  fixes fetch :: "64 word \<Rightarrow> instruction" \<comment> \<open>Given an address, fetch a single instruction.\<close>
    and entry :: "64 word" \<comment> \<open>The entry address.\<close>
    and within_local_segment :: "64 word \<Rightarrow> bool" \<comment> \<open>Check whether an address is in the range of a local stack frame.\<close>
    and within_global_segment :: "64 word \<Rightarrow> bool" \<comment> \<open>Check whether an address is in the range of the global data sections.\<close>
assumes non_empty_global: "\<exists> a . within_global_segment a"
begin
end


fun binop :: "(64 word \<Rightarrow> 64 word \<Rightarrow> 64 word) \<Rightarrow> cvalue list \<Rightarrow> cvalue" 
  where 
    "binop f [CValue v0,CValue v1] = CValue (f v0 v1)"
  | "binop f _ = Tainted"

fun cplus:: "cvalue \<Rightarrow> cvalue \<Rightarrow> cvalue" (infixl "+\<^sub>c" 65)
  where "v0 +\<^sub>c v1 = binop (+) [v0,v1]" 

fun cminus:: "cvalue \<Rightarrow> cvalue \<Rightarrow> cvalue" (infixl "-\<^sub>c" 65)
  where "v0 -\<^sub>c v1 = binop (-) [v0,v1]" 

fun ctimes:: "cvalue \<Rightarrow> cvalue \<Rightarrow> cvalue" (infixl "*\<^sub>c" 70)
  where "v0 *\<^sub>c v1 = binop (*) [v0,v1]" 

fun cgenop:: "_ \<Rightarrow> cvalue \<Rightarrow> cvalue \<Rightarrow> cvalue" 
  where "cgenop f v0 v1 = binop f [v0,v1]" 

primrec cop_semantics :: "operation \<Rightarrow> cvalue list \<Rightarrow> cvalue"
  where 
     "cop_semantics Mov = id o hd"
   | "cop_semantics Add = binop (+)"
   | "cop_semantics Sub = binop (-)"
   | "cop_semantics Mul = binop (*)"
   | "cop_semantics (GenericOp m f) = binop f"






text \<open>Reading/writing registers\<close>

definition cread_reg
  where "cread_reg s r \<equiv> cregs s r"

definition cwrite_reg :: "string \<Rightarrow> cvalue \<Rightarrow> cstate \<Rightarrow> cstate"
  where "cwrite_reg r v s \<equiv> s\<lparr>cregs := (cregs s)(r := v) \<rparr>"

definition cmodify_reg 
  where "cmodify_reg r f s \<equiv> s\<lparr> cregs := (cregs s)(r := f (cregs s r)) \<rparr>"



text \<open>Reading/writing memory\<close>

text \<open>To define separation, we use a plus that cannot overflow. Note that @{term "-1"} is the maximum value.\<close>
definition capped_plus :: "64 word \<Rightarrow> 64 word \<Rightarrow> 64 word" (infixl "\<oplus>" 65)  
  where "capped_plus a b \<equiv> if unat a + unat b \<ge> 2^64 then -1 else a + b"

fun cseparate :: "cvalue \<times> 64 word \<Rightarrow> cvalue \<times> 64 word \<Rightarrow> bool" 
  where 
    "cseparate (CValue a,si) (CValue a',si') = (si > 0 \<and> si' > 0 \<and> a \<noteq> a' \<and> (a \<oplus> si \<le> a' \<or> a' \<oplus> si' \<le> a))"
  | "cseparate _ _ = False"

text \<open>Tainted values are not separate.\<close>
fun cval_separate
  where 
    "cval_separate (a, CValue si) (a', CValue si') = cseparate (a,si) (a',si')"
  | "cval_separate _ _   = False"

declare cseparate.simps[simp del]


definition cread_segment :: "cmem \<Rightarrow> cvalue \<Rightarrow> 64 word \<Rightarrow> cvalue"
  where 
    "cread_segment m a si \<equiv> 
        let (separate,overlap) = partition (cseparate (a,si) o fst) m in
           case overlap of
             [(r',v')] \<Rightarrow> if (a,si) = r' then v' else Tainted
           | _       \<Rightarrow> Tainted"

definition cread_mem :: "cstate \<Rightarrow> cvalue \<Rightarrow> 64 word \<Rightarrow>  cvalue"
  where "cread_mem s a si \<equiv> cread_segment (cmem s) a si"

text \<open>Writing overlaps may split memory regions, and write @{term Tainted} to the split regions.\<close>
fun write_overlaps :: "64 word \<Rightarrow> 64 word \<Rightarrow> cmem \<Rightarrow> cmem"
  where 
    "write_overlaps a si [] = []"
  | "write_overlaps a si (((CValue a',si'),v')#m) = 
        (if a' < a then [((CValue a', a - a'),Tainted)] else []) @
        (if a' \<oplus> si' > a \<oplus> si then [((CValue (a \<oplus> si), a' \<oplus> si' - (a \<oplus> si)),Tainted)] else []) @
        write_overlaps a si m"

text \<open>Writing to memory leaves all separate regions alone, and splits all partially overlapping regions if any.
      Then, the new region is inserted.\<close>
fun cwrite_segment :: "64 word \<Rightarrow> 64 word \<Rightarrow> cvalue \<Rightarrow> cmem \<Rightarrow> cmem"
  where "cwrite_segment a si v m =
      (let (separate,overlap) = partition (cseparate (CValue a,si) o fst) m in
        [((CValue a,si),v)] @ write_overlaps a si overlap @ separate)"


definition cwrite_mem :: "64 word \<Rightarrow> 64 word \<Rightarrow> cvalue \<Rightarrow> cstate \<Rightarrow> cstate"
  where "cwrite_mem a si v s \<equiv> s \<lparr> cmem := cwrite_segment a si v (cmem s) \<rparr>"


definition cresolve_address :: "cstate \<Rightarrow> string \<Rightarrow> 64 word \<Rightarrow> cvalue"
  where "cresolve_address s base offset \<equiv> cread_reg s base +\<^sub>c CValue offset"

primrec cread_operand :: "cstate \<Rightarrow> src_operand \<Rightarrow> cvalue"
  where
    "cread_operand s (FromImmediate imm) = CValue imm"
  | "cread_operand s (FromReg r) = cread_reg s r"
  | "cread_operand s (FromMemory base offset si) = 
      (case cresolve_address s base offset of
         CValue a \<Rightarrow> cread_mem s (CValue a) (Rep_pos si)
       | _        \<Rightarrow> Tainted)"

definition thrash_memory :: "cstate \<Rightarrow> cstate"
  where "thrash_memory s \<equiv> s \<lparr> cmem := []\<rparr> "

text \<open>Writing to @{term Tainted} causes entire memory to get thrased.\<close>
primrec cwrite_operand :: "dst_operand \<Rightarrow> cvalue \<Rightarrow> cstate \<Rightarrow> cstate"
  where "cwrite_operand (ToReg r) v s = cwrite_reg r v s"
  | "cwrite_operand (ToMemory base offset si) v s = 
      (case cresolve_address s base offset of
         CValue a \<Rightarrow> cwrite_mem a (Rep_pos si) v s
       | _ \<Rightarrow> thrash_memory s)"

text \<open>Executing an instruction: first increment the instruction pointer, then read all sources, then
      execute the semantics producing a value $v$ and write that value to the destination.\<close>
definition cexec_instr :: "instruction \<Rightarrow> cstate \<Rightarrow> cstate"
  where "cexec_instr i s \<equiv>
          let srcs_values = map (cread_operand s) (i_srcs i);
              v = cop_semantics (i_op i) srcs_values in
            cwrite_operand (i_dest i) v s"

definition cset_rip
  where "cset_rip i \<equiv> cmodify_reg ''rip'' ((+\<^sub>c) (CValue (i_size i)))"

definition cstep :: "instruction \<Rightarrow> cstate \<Rightarrow> cstate"
  where "cstep i \<equiv> cexec_instr i o cset_rip i"


text \<open>The following will be proven invariant over any execution: all regions in the concrete memory 
      have untainted addresses and are separate.\<close>
fun cseparation_invariant
  where "cseparation_invariant [] = True"
  | "cseparation_invariant (((a,si),v)#m) = ((\<forall> ((a',si'),v') \<in> set m . cseparate (a,si) (a',si')) \<and> a \<noteq> Tainted \<and> cseparation_invariant m)"






text\<open>
As example, we use the following program:
  MOV QWORD PTR [RAX], RAX
  MOV QWORD PTR [RAX+8], 42
  MOV RBX, DWORD PTR [RAX]
  MOV QWORD PTR [RBX], 43
  MOV RCX, DWORD PTR [RAX+4]

\<close>
definition example_program
  where "example_program \<equiv> [
    \<lparr> i_dest = ToMemory ''rax'' 0 (Abs_pos 8), i_srcs = [FromReg ''rax''],                   i_op = Mov, i_size = 3 \<rparr>,
    \<lparr> i_dest = ToMemory ''rax'' 8 (Abs_pos 8), i_srcs = [FromImmediate 42],                  i_op = Mov, i_size = 3 \<rparr>,  
    \<lparr> i_dest = ToReg    ''rbx''              , i_srcs = [FromMemory ''rax'' 0 (Abs_pos 8)],  i_op = Mov, i_size = 3 \<rparr>,
    \<lparr> i_dest = ToMemory ''rbx'' 0 (Abs_pos 8), i_srcs = [FromImmediate 43],                  i_op = Mov, i_size = 3 \<rparr>,
    \<lparr> i_dest = ToReg    ''rcx''              , i_srcs = [FromMemory ''rax'' 4 (Abs_pos 8)],  i_op = Mov, i_size = 3 \<rparr>
  ]"

text \<open>We run the simplifier on the five step program provided by @{term example_program}.
This shows the state changes induced by the program.
Registers are modified, as well as memory.
\<close>
lemma example_program_concrete:
  assumes "cregs s ''rax'' = CValue 4000"
      and "cregs s ''rip'' = CValue rip\<^sub>0"
      and "cmem s = []"
    shows "fold cstep (take 5 example_program) s = 
              s\<lparr>cmem   := [((CValue 4000, 8), CValue 43), 
                           ((CValue 4008, 8), CValue 42)],
                cregs := (cregs s)(''rbx'' := CValue 4000, 
                                   ''rip'' := CValue (15 + rip\<^sub>0), 
                                   ''rcx'' := Tainted)\<rparr>"
  by (auto simp add: cstep_def example_program_def cexec_instr_def cresolve_address_def cset_rip_def cmodify_reg_def
          cread_reg_def Abs_pos_inverse cwrite_mem_def capped_plus_def cseparate.simps cwrite_reg_def cread_mem_def cread_segment_def assms)







(* SEPARATION/ENCLOSURE *)
lemma capped_plus_lt_mono:
  shows "a' \<oplus> si < a' \<oplus> si' \<Longrightarrow> si < si'"
  apply (auto simp add: capped_plus_def split: if_split_asm)
  apply unat_arith
  by (auto simp add: plus_le_left_cancel_nowrap capped_plus_def uno_simps unat_of_nat no_olen_add_nat split: if_split_asm)


lemma capped_plus_lt_mono2:
  shows "a' \<oplus> si < a' \<oplus> si' \<Longrightarrow> a' \<oplus> si \<le> a' + si"
  by (auto simp add: capped_plus_def split: if_split_asm)


lemma capped_plus_minus_order:
  shows "a \<oplus> sia > a' \<Longrightarrow> a' < a \<Longrightarrow> a' \<oplus> (a - a') \<le> a"
  apply (auto simp add: capped_plus_def split: if_split_asm)
  apply unat_arith
  by (auto simp add: capped_plus_def uno_simps unat_of_nat order_less_le unat_sub word_le_nat_alt split: if_split_asm)

lemma capped_plus_0[simp]:
  shows "a \<oplus> 0 = a"
  apply (auto simp add: capped_plus_def split: if_split_asm)
  apply unat_arith
  by auto

lemma capped_plus_increases:
  shows "a' \<oplus> si' \<le> a' \<longleftrightarrow> (si' = 0 \<or> a' = -1)"
  apply (auto simp add: capped_plus_def split: if_split_asm)
  apply unat_arith
  apply (auto simp add: capped_plus_def uno_simps unat_of_nat order_less_le unat_sub word_le_nat_alt split: if_split_asm)
  using word_le_nat_alt word_order.extremum_uniqueI apply blast
  apply unat_arith
  apply (auto simp add: capped_plus_def uno_simps unat_of_nat order_less_le unat_sub word_le_nat_alt split: if_split_asm)
  apply unat_arith
  apply (auto simp add: capped_plus_def uno_simps unat_of_nat order_less_le unat_sub word_le_nat_alt split: if_split_asm)
  done

lemma capped_plus_lemma2:
  shows "x > 0 \<Longrightarrow> a' < z \<Longrightarrow> z \<oplus> x > a'"
  apply (auto simp add: capped_plus_def split: if_split_asm)
    apply unat_arith
   apply auto
  apply (metis linorder_neq_iff word_less_nat_alt word_not_simps(3))
    apply unat_arith
  by auto

lemma capped_plus_lemma1:
  shows "aa \<oplus> sia < a' \<oplus> si' \<Longrightarrow> a' < aa \<Longrightarrow> \<not> aa \<oplus> sia \<le> a' \<Longrightarrow> aa \<oplus> sia \<oplus> (a' \<oplus> si' - (aa \<oplus> sia)) > a'"
  using capped_plus_lemma2[of "a' \<oplus> si' - (aa \<oplus> sia)" a' "aa \<oplus> sia"]
  by (metis capped_plus_0 linorder_not_le word_neq_0_conv)






lemma sep_irreflexive:
  shows "\<not>cseparate r r"
  by (cases "(r,r)" rule: cseparate.cases,auto simp add: cseparate.simps capped_plus_increases)
                                  
lemma csep_symmetric[symmetric]:
  shows "cseparate a b \<longleftrightarrow> cseparate b a"
  by (cases "(a,b)" rule: cseparate.cases,auto simp add: cseparate.simps)

lemma sep_write_overlaps:
assumes "si > 0"
    and "\<forall> ((a,si),v) \<in> set m . a \<noteq> Tainted"
  shows "filter (Not \<circ> cseparate (CValue a, si) o fst) (write_overlaps a si m) = []"
  using assms 
  apply (induct m rule: write_overlaps.induct,auto simp add: )
  apply (metis capped_plus_minus_order cseparate.simps(1) eq_iff_diff_eq_0 linorder_neq_iff not_le_imp_less word_greater_zero_iff)
  apply (metis capped_plus_increases cseparate.simps(1) eq_iff_diff_eq_0 linorder_not_less verit_comp_simplify1(1) word_greater_zero_iff)
  apply (metis capped_plus_minus_order cseparate.simps(1) eq_iff_diff_eq_0 linorder_neq_iff not_le_imp_less word_greater_zero_iff)
  by (metis capped_plus_increases cseparate.simps(1) eq_iff_diff_eq_0 linorder_not_less verit_comp_simplify1(1) word_greater_zero_iff word_n1_ge)

lemma cseparation_invariant_untainted:
  assumes "cseparation_invariant m"
  and "(r,v) \<in> set m"
  shows "fst r \<noteq> Tainted"
  using assms
  by (induct m,auto)


lemma cread_write_mem_alias:
  assumes "cseparation_invariant (cmem s)"
      and "si > 0"
  shows "cread_segment (cmem (cwrite_mem a si v s)) (CValue a) si = v"
  apply (auto simp add: cwrite_mem_def cread_segment_def sep_write_overlaps sep_irreflexive split: list.splits)
  by (smt (verit, best) assms case_prodI2 case_prod_unfold comp_assoc cseparation_invariant_untainted list.distinct(1) mem_Collect_eq sep_write_overlaps set_filter)




lemma cseparate_write_overlap1:
assumes "a0 < a"
    and "\<not> cseparate (CValue a, si) (CValue a0, si0)"
    and "cseparate (CValue a', si') (CValue a0, si0)"
    and "si > 0"
  shows "cseparate (CValue a', si') (CValue a0, a - a0)"
  using assms
  apply (auto simp add: cseparate.simps unat_gt_0)
  using word_neq_0_conv apply fastforce
  using word_neq_0_conv apply fastforce
  by (metis capped_plus_0 capped_plus_minus_order linorder_le_cases order_trans)

lemma cseparate_write_overlap2:
  assumes "cseparate (CValue a', si') (CValue a0, si0)"
      and "\<not> cseparate (CValue a, si) (CValue a0, si0)"
      and "a \<oplus> si < a0 \<oplus> si0"
      and "si > 0"
    shows "cseparate (CValue a', si') (CValue (a \<oplus> si), a0 \<oplus> si0 - (a \<oplus> si))"
  using assms
  apply (auto simp add: cseparate.simps unat_gt_0)
  using word_neq_0_conv apply fastforce
  apply (smt (verit, best) capped_plus_0 capped_plus_lt_mono leD linorder_neq_iff word_le_def)
  apply (smt (verit, best) capped_plus_0 capped_plus_lt_mono leD linorder_neq_iff word_le_def)
  using word_neq_0_conv apply fastforce
  apply (metis capped_plus_0 capped_plus_minus_order dual_order.trans)
  using word_neq_0_conv apply fastforce
  apply (smt (verit, best) capped_plus_0 capped_plus_lt_mono leD linorder_neq_iff word_le_def)
  using word_neq_0_conv apply fastforce
  apply (metis capped_plus_0 capped_plus_minus_order dual_order.trans)
  done

lemma cseparate_write_overlaps:
assumes "\<forall> (r,_) \<in> set m . cseparate (CValue a', si') r"
    and "\<forall> (r,_) \<in> set m . \<not>cseparate (CValue a, si) r"
    and "(r',v') \<in> set (write_overlaps a si m)"
    and "\<forall> ((a,si),v) \<in> set m . a \<noteq> Tainted"
    and "si > 0"
  shows "cseparate (CValue a', si') r'"
  using assms 
  apply (induct m rule: write_overlaps.induct)
  using cseparate_write_overlap1 cseparate_write_overlap2
  by auto


lemma list_is_singleton:
  shows "length x = 1 \<Longrightarrow> \<exists> a . x = [a]"
  by (simp add: length_Suc_conv)

lemma append_singleton:
  shows "x @ y = [a] \<longleftrightarrow> (x=[a]\<and>y=[]) \<or> (x=[] \<and>y=[a])"
  by (cases x,auto)

lemma tainted_write_overlap:
assumes "filter (Not \<circ> (cseparate (CValue a', si') \<circ> fst)) (write_overlaps a si (filter (Not \<circ> (cseparate (CValue a, si) \<circ> fst)) m)) = [((CValue a', si'), v')]"
    and "\<forall> ((a,si),v) \<in> set m . a \<noteq> Tainted"
  shows "v' = Tainted"
  using assms
  by (induct a si m rule : write_overlaps.induct,auto split: if_split_asm)



lemma same_region_found_in_search_for_overlap:
  assumes "filter (\<lambda>x. cseparate (fst x) (CValue a, si) \<and> \<not> cseparate (fst x) (CValue a', si')) m = [(r', v')]"
      and "filter (Not \<circ> (cseparate (CValue a', si') \<circ> fst)) m = [(r'', v'')]"
    shows "v' = v'' \<and> r' =r''"
  using assms
  apply (induct m, auto split: if_split_asm)
  by (smt (verit, best) comp_apply csep_symmetric empty_filter_conv list.distinct(1))+

lemma cseparation_invariant_unique_read:
assumes "cseparation_invariant m"
shows "\<forall> (r,v) \<in> set m . filter (Not \<circ> (cseparate r \<circ> fst)) m = [(r,v)]"
  using assms
  apply (induct m,auto simp add: csep_symmetric sep_irreflexive case_prod_unfold)
  using csep_symmetric by fastforce+

lemma cread_write_mem_separate:
  assumes "cseparation_invariant (cmem s)"
      and "cseparate (CValue a,si) (CValue a',si')"
    and "v' = cread_segment (cmem (cwrite_mem a si v s)) (CValue a') si'"
    and "si > 0"
  shows "v'= Tainted \<or> v' = cread_segment (cmem s) (CValue a') si'"
proof-
  have 1: "(a,si) \<noteq> (a',si')"
    using assms cseparate.simps
    by force
  show ?thesis
  proof(cases "filter (Not \<circ> (cseparate (CValue a', si') \<circ> fst)) (cmem s) = []")
    case True
    hence " filter (\<lambda>x. cseparate (fst x) (CValue a, si) \<and> \<not> cseparate (fst x) (CValue a', si')) (cmem s) = []"
      by (smt (verit, best) comp_def csep_symmetric empty_filter_conv)
    moreover
    {
      fix e
      assume "e \<in> set (write_overlaps a si (filter (Not \<circ> (cseparate (CValue a, si) \<circ> fst)) (cmem s)))"
      hence "\<not> (Not \<circ> (cseparate (CValue a', si') \<circ> fst)) e"
        using True assms(1,3) 
        using cseparate_write_overlaps[of "filter (Not \<circ> (cseparate (CValue a, si) \<circ> fst)) (cmem s)" a' si' a si "fst e" "snd e"]
        apply (cases e,auto simp add: )
        by (metis (mono_tags, lifting) assms(4) comp_apply cseparate.simps(3) filter_empty_conv fst_conv)+
    }
    hence "filter (Not \<circ> (cseparate (CValue a', si') \<circ> fst)) (write_overlaps a si (filter (Not \<circ> (cseparate (CValue a, si) \<circ> fst)) (cmem s))) = []"
      by (auto)
    ultimately
    show ?thesis
      using assms 1
      by (auto simp add: csep_symmetric cwrite_mem_def cread_segment_def)
    next
    case False
    note 0 = this
    show ?thesis
    proof(cases "length (filter (Not \<circ> (cseparate (CValue a', si') \<circ> fst)) (write_overlaps a si (filter (Not \<circ> (cseparate (CValue a, si) \<circ> fst)) (cmem s))) @
             filter (\<lambda>x. cseparate (fst x) (CValue a, si) \<and> \<not> cseparate (fst x) (CValue a', si')) (cmem s)) = 1")
      case False
      thus ?thesis
        using 1 assms cseparation_invariant_unique_read[OF assms(1)]
        apply (cases "length (filter (Not \<circ> (cseparate (CValue a', si') \<circ> fst)) (write_overlaps a si (filter (Not \<circ> (cseparate (CValue a, si) \<circ> fst)) (cmem s))) @
           filter (\<lambda>x. cseparate (fst x) (CValue a, si) \<and> \<not> cseparate (fst x) (CValue a', si')) (cmem s))")
        using False
        apply (auto simp add: csep_symmetric cwrite_mem_def cread_segment_def split: if_split_asm list.splits)[1]
        using False apply auto
        apply (auto simp add: csep_symmetric cwrite_mem_def cread_segment_def split: if_split_asm list.splits)[1]
        using False apply auto
        apply (auto simp add: csep_symmetric cwrite_mem_def cread_segment_def split: if_split_asm list.splits)[1]
        using False apply auto
        done
    next
      case True
      then obtain r' v' where 1: "filter (Not \<circ> (cseparate (CValue a', si') \<circ> fst)) (write_overlaps a si (filter (Not \<circ> (cseparate (CValue a, si) \<circ> fst)) (cmem s))) @
           filter (\<lambda>x. cseparate (fst x) (CValue a, si) \<and> \<not> cseparate (fst x) (CValue a', si')) (cmem s) = [(r',v')]"
        by (smt (verit, ccfv_threshold) eq_fst_iff list_is_singleton)
      show ?thesis
      proof(cases "length (filter (Not \<circ> (cseparate (CValue a', si') \<circ> fst)) (cmem s)) = 1")
        case True
        then obtain r'' v'' where "filter (Not \<circ> (cseparate (CValue a', si') \<circ> fst)) (cmem s)  = [(r'',v'')]"
          by (metis eq_fst_iff list_is_singleton)
        thus ?thesis
          using 0 1 assms cseparation_invariant_unique_read[OF assms(1)] cseparation_invariant_untainted[OF assms(1)]
          using False
          using tainted_write_overlap[of a' si' a si "cmem s" v']
          using same_region_found_in_search_for_overlap[of a si a' si' "cmem s" ]
          apply (auto simp add: csep_symmetric cwrite_mem_def cread_segment_def append_singleton split: if_split_asm)[1]
          apply blast+
          by (metis eq_fst_iff)
      next
        case False
        thus ?thesis
          using 0 1 assms cseparation_invariant_untainted[OF assms(1)] cseparation_invariant_unique_read[OF assms(1)]
          using False
          apply (auto simp add: csep_symmetric cwrite_mem_def cread_segment_def split: if_split_asm)[1]
          apply (cases "filter (Not \<circ> (cseparate (CValue a', si') \<circ> fst)) (cmem s)",auto)
          apply (cases "tl (filter (Not \<circ> (cseparate (CValue a', si') \<circ> fst)) (cmem s))",auto simp add: append_singleton tainted_write_overlap)
          apply (smt (verit, best) cseparation_invariant_untainted[OF assms(1)] tainted_write_overlap case_prodI2 case_prod_beta')
          by (smt (verit, ccfv_threshold) case_prod_conv list.inject list.set_intros(1) mem_Collect_eq neq_Nil_conv set_filter)
      qed
    qed
  qed
qed

  

lemma cread_write_mem_overlap:
assumes "\<not> cseparate (CValue a,si) (CValue a',si')" 
    and "(a,si) \<noteq> (a',si')"
shows "cread_segment (cmem (cwrite_mem a si v s)) (CValue a') si' = Tainted"
  using assms 
  apply (auto simp add: cwrite_mem_def cread_segment_def sep_write_overlaps sep_irreflexive split: list.splits)
  using csep_symmetric apply blast+
  done



lemma write_overlaps_not_tainted:
  shows "((Tainted, si'), v) \<in> set (write_overlaps a si m) \<Longrightarrow> \<exists> (r,v) \<in> set m . fst r = Tainted"
  by (induct a si m rule: write_overlaps.induct,auto)



lemma cseparation_invariant_appendI:
  assumes "cseparation_invariant m0"
      and "cseparation_invariant m1"
      and "\<forall> (r0,v0) \<in> set m0 . \<forall> (r1,v1) \<in> set m1 . cseparate r0 r1"
    shows "cseparation_invariant (m0@m1)"
  using assms
  apply (induct m0 arbitrary: m1)
  apply (auto simp add:  csep_symmetric sep_irreflexive simp del: split: if_split_asm prod.splits)
  by fastforce+


lemma cseparation_invariant_filter:
  assumes "cseparation_invariant m"
  shows "cseparation_invariant (filter P m)"
  using assms
  by (induct m,auto)



lemma cseparate_from_overlap3:
  shows "si > 0 \<Longrightarrow> \<not>cseparate (CValue a,si) (CValue a',si') \<Longrightarrow> cseparate r0 (CValue a', si') \<Longrightarrow> a' < a \<Longrightarrow> cseparate r0 (CValue a', a - a')"
  apply (cases r0;cases "fst r0";auto simp add: capped_plus_def cseparate.simps word_gt_0 unat_sub_if' unat_plus_if' no_olen_add_nat split: if_split_asm)
  by unat_arith+


lemma csep_write_o:
assumes "\<forall> (r,v) \<in> set m . cseparate r0 r"
    and "\<forall> (r,v) \<in> set m . \<not>cseparate (CValue a,si) r"
    and "(r1,v1)\<in> set (write_overlaps a si m)"
    and "si > 0"
  shows "cseparate r0 r1"
  using assms
  apply (induct a si m rule: write_overlaps.induct)
  apply (auto simp add: case_prod_unfold)
  using cseparate_from_overlap3 apply blast
  apply (metis cseparate.elims(2) cseparate_write_overlap2)
  using cseparate.simps(3) by blast

lemma cseparate_from_overlap2:
  shows "\<not> cseparate (CValue a, si) (CValue a', si') \<Longrightarrow>
         a' < a \<Longrightarrow> a \<oplus> si < a' \<oplus> si' \<Longrightarrow> cseparate (CValue a', a - a') (CValue (a \<oplus> si), a' \<oplus> si' - (a \<oplus> si))"
  apply (auto simp add: capped_plus_def cseparate.simps word_gt_0 unat_sub_if' unat_plus_if' no_olen_add_nat split: if_split_asm)
  apply unat_arith+
  done

lemma cseparation_invariant_write_overlaps:
  assumes "si > 0"
      and "\<forall> (r,v) \<in> set m . \<not>cseparate (CValue a ,si) r"
      and "cseparation_invariant m"
  shows "cseparation_invariant (write_overlaps a si m)"
  using assms
proof (induct a si m rule: write_overlaps.induct)
  case (1 a si)
  thus ?case
    by auto
next
  case (2 a si a' si' v' m)
  have 1: "a' < a \<Longrightarrow> \<forall>(r, v)\<in>set m. cseparate (CValue a', a - a') r"
    using 2 csep_symmetric cseparate_from_overlap3
    by auto
  have 3: "a \<oplus> si < a' \<oplus> si' \<Longrightarrow> \<forall>(r, v)\<in>set m. cseparate (CValue (a \<oplus> si), a' \<oplus> si' - (a \<oplus> si)) r"
    using 2
    apply auto
    by (smt (verit, ccfv_threshold) csep_symmetric case_prodD case_prodD' cseparate.elims(2) cseparate_write_overlap2)
  have 5: "\<forall>(r, v)\<in>set m. \<not> cseparate (CValue a, si) r"
    using 2
    by auto
  show ?case
    apply auto
    apply (metis cseparate.simps(1) order_less_imp_not_less order_neq_le_trans cseparate_from_overlap2 capped_plus_lemma2)
    using csep_write_o[of m "(CValue a', a - a')" a si] 1 5 2
    using csep_write_o[of m  "(CValue (a \<oplus> si), a' \<oplus> si' - (a \<oplus> si))" a si] 3 
    by (auto simp add: csep_symmetric)
next 
  case (3 a b ve vc va)
  thus ?case
    by (auto simp add:)
qed

lemma cseparate_from_overlap1:
  shows "si > 0 \<Longrightarrow> si' > 0 \<Longrightarrow>  a' < a \<Longrightarrow> cseparate (CValue a, si') (CValue a', a - a')"
  apply (auto simp add: cseparate.simps word_gt_0)
  apply (metis capped_plus_0 capped_plus_minus_order)+
  done

lemma cseparate_write_overlaps2:
assumes "\<forall> (r,_) \<in> set m . \<not>cseparate (CValue a, si) r"
    and "(r',v') \<in> set (write_overlaps a si m)"
    and "\<forall> ((a,si),v) \<in> set m . a \<noteq> Tainted"
    and "si > 0"
  shows "cseparate (CValue a, si) r'"
  using assms 
  apply (induct m rule: write_overlaps.induct)
  using cseparate_from_overlap1
  apply auto
  apply blast
  by (metis (no_types, lifting) capped_plus_increases cseparate.simps(1) linorder_not_less verit_comp_simplify1(1) word_n1_ge word_neq_0_conv)




lemma cseparate_write_overlaps3:
  assumes "cseparation_invariant m"
      and "0 < si"
      and "((a0, si0), v0) \<in> set (write_overlaps a si (filter (Not \<circ> (cseparate (CValue a, si) \<circ> fst)) m))"
      and "cseparate (CValue a, si) (a1,si1)"
      and "((a1,si1), v1) \<in> set m"
    shows "cseparate (a0, si0) (a1,si1)"
  using assms
  apply (induct m,auto simp add: case_prod_unfold)
  apply (smt (z3) csep_write_o case_prod_unfold comp_apply csep_symmetric mem_Collect_eq set_filter)
  apply (auto simp add: case_prod_unfold split: if_split_asm)

  subgoal for a' si' v' m
    apply (cases a',auto)
    apply (metis csep_symmetric fst_conv cseparate_from_overlap3)
    by (smt (z3) csep_symmetric cseparate.elims(1) cseparate_write_overlap2 fst_conv)
  done


lemma cseparation_invariant_cwrite_mem:
  assumes "cseparation_invariant (cmem s)"
      and "si > 0"
    shows "cseparation_invariant (cmem (cwrite_mem a si v s))"
  using assms
  apply (auto simp add: cwrite_mem_def comp_assoc append_singleton)
  apply (smt (verit, best) case_prodI2 case_prod_unfold comp_apply cseparation_invariant_filter cseparation_invariant_untainted filter_empty_conv fst_conv sep_write_overlaps)

  apply (rule cseparation_invariant_appendI)
  using cseparation_invariant_write_overlaps[of si "filter (Not \<circ> (cseparate (CValue a, si) \<circ> fst)) (cmem s)" a] cseparate_write_overlaps3
  by (auto simp add: cseparation_invariant_filter)


lemma cseparation_invariant_thrash:
  shows "cseparation_invariant (cmem (thrash_memory s))"
  by (auto simp add: thrash_memory_def)

lemma cseparation_invariant_cwrite_operand:
assumes "cseparation_invariant (cmem s)"
shows "cseparation_invariant (cmem (cwrite_operand op v s))"
  using assms cseparation_invariant_thrash cseparation_invariant_cwrite_mem Rep_pos
  by (cases op,auto simp add: cwrite_reg_def split: cvalue.splits)

lemma cseparation_invariant_cstep:
assumes "cseparation_invariant (cmem s)"
shows "cseparation_invariant (cmem (cstep i s))"
  using cseparation_invariant_cwrite_operand assms
  by (auto simp add: cstep_def cexec_instr_def cset_rip_def cmodify_reg_def)


end

