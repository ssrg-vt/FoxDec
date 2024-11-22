theory X86_Instantiation_B
  imports X86_Concrete_Semantics X86_Symbolic_Generic_Semantics
begin

datatype Bases = Stackframe | Global | Heap "64 word" 

context binary
begin

fun union_option (infixl "\<union>\<^sub>O" 65)
where
    "Some x \<union>\<^sub>O Some y = Some (x \<union> y)"
 |  "Some x \<union>\<^sub>O None = Some x"
 |  "None \<union>\<^sub>O Some y = Some y"
 |  "None \<union>\<^sub>O None = None"

definition Union_option ("\<Union>\<^sub>o _" [900] 900)
  where "\<Union>\<^sub>o x \<equiv> if None \<in> x then None else Some ((\<lambda> s . case s of Some s \<Rightarrow> s) ` x)"


primrec bases_of'
  where 
    "bases_of' (SE_Immediate imm) = (if within_global_segment imm then Some {Global} else None)"
  | "bases_of' (SE_Malloc imm) = Some {Heap imm}"
  | "bases_of' (SE_Var sp) = (if sp = SP_Reg ''rsp'' then Some {Stackframe} else None)"
  | "bases_of' (SE_Plus e0 e1) = bases_of' e0 \<union>\<^sub>O bases_of' e1"
  | "bases_of' (SE_Minus e0 e1) = bases_of' e0"
  | "bases_of' (SE_Times e0 e1) = None"
  | "bases_of' (SE_GenericOp m f e0 e1) = None"
  | "bases_of' (SE_Bot) = None"

definition bases_of
  where "bases_of e \<equiv> case bases_of' e of None \<Rightarrow> {} | Some bs \<Rightarrow> bs"


definition is_base_of
  where "is_base_of b e \<equiv> b \<in> bases_of e"

fun separate_bases
  where 
  "separate_bases Stackframe Global     = True"
| "separate_bases Stackframe (Heap imm) = True"
| "separate_bases Global Stackframe     = True"
| "separate_bases Global (Heap imm)     = True"
| "separate_bases (Heap imm) Global     = True"
| "separate_bases (Heap imm) Stackframe = True"
| "separate_bases _ _ = False"

end

locale Bases = binary +
  assumes bases_separation: "is_base_of b0 e0 \<Longrightarrow> is_base_of b1 e1 \<Longrightarrow> separate_bases b0 b1 \<Longrightarrow> eval \<Gamma>\<^sub>0 e0 v0 \<Longrightarrow> eval \<Gamma>\<^sub>0 e1 v1 \<Longrightarrow> cseparate (CValue v0,si0) (CValue v1,si1)"
begin



datatype spointer = Bases "Bases set" 


fun \<gamma>_val :: "(StatePart \<Rightarrow> 64 word) \<Rightarrow> spointer \<Rightarrow> cvalue set"
  where 
     "\<gamma>_val \<Gamma>\<^sub>0 (Bases bs) = (if bs = {} then UNIV else { v . \<exists> b \<in> bs . \<exists> e . \<exists> w . is_base_of b e \<and> eval \<Gamma>\<^sub>0 e w \<and> v = CValue w  })"


lemma exists_expr_for_base:
  shows "\<exists> e . is_base_of b e"
proof(cases b)
  case Stackframe
  thus ?thesis
    apply simp
    apply (rule exI[of _ "SE_Var (SP_Reg ''rsp'')"])
    by (auto simp add: is_base_of_def bases_of_def)
next
  case Global
  obtain a where "within_global_segment a"
    using non_empty_global
    by auto
  thus ?thesis
    using Global
    apply simp
    apply (rule exI[of _ "SE_Immediate a"])
    by (auto simp add: is_base_of_def bases_of_def)
next
  case (Heap imm)
  thus ?thesis
    apply simp
    apply (rule exI[of _ "SE_Malloc imm"])
    by (auto simp add: is_base_of_def bases_of_def)
qed

lemma non_empty:
  shows "\<gamma>_val \<Gamma>\<^sub>0 a \<noteq> {}"
  apply (cases a,auto)
  using exists_expr_for_base exists_valuation by blast





fun sseparate_B
  where "sseparate_B (SValue (Bases bs0), SValue (Bases si0)) (SValue (Bases bs1), SValue (Bases si1)) = (bs0 \<noteq> {} \<and> bs1 \<noteq> {} \<and> si0 \<noteq> {} \<and> si1 \<noteq> {} \<and> (\<forall> b0 \<in> bs0 . \<forall> b1 \<in> bs1 . separate_bases b0 b1))"
  | "sseparate_B _ _ = False"

fun subset'
  where "subset' (SValue (Bases x)) (SValue (Bases y)) = (if x = {} then y = {} else if y = {} then True else x \<subseteq> y)"
  | "subset' a \<top> = True"
  | "subset' \<top> a = False"


fun senclosed_B
  where "senclosed_B (a0,si0) (a1,si1) = (subset' a0 a1 \<and> subset' si0 si1)"

definition sequal_B 
  where "sequal_B r0 r1 \<equiv> False"


lemma tainted_iff_empty_bases:
  shows "Tainted \<in> \<gamma>_val \<Gamma>\<^sub>0 a \<longleftrightarrow> a = Bases {}"
  by (cases a,auto)




fun \<gamma>_val_B
  where "\<gamma>_val_B \<Gamma>\<^sub>0 \<top> = UNIV"
  | "\<gamma>_val_B \<Gamma>\<^sub>0 (SValue (Bases es)) = \<gamma>_val \<Gamma>\<^sub>0 (Bases es)"

fun join_B
  where 
    "join_B (Bases bs0) (Bases bs1) = Bases (if bs0 = {} \<or> bs1 = {} then {} else (bs0 \<union> bs1))" 


definition mk_svalue_B
  where "mk_svalue_B a \<equiv> SValue (Bases (bases_of a))" 

definition mk_init_mem_svalue_B 
  where "mk_init_mem_svalue_B a si \<equiv> \<top>"



lemma sseparation:
  assumes "sseparate_B (ptr0',si0') (ptr1', si1')"
       and "ptr0 \<in> \<gamma>_val_B \<Gamma>\<^sub>0 ptr0'"
      and "si0 \<in> \<gamma>_val_B \<Gamma>\<^sub>0 si0'"
      and "ptr1 \<in> \<gamma>_val_B \<Gamma>\<^sub>0 ptr1'"
      and "si1 \<in> \<gamma>_val_B \<Gamma>\<^sub>0 si1'"
    shows "cval_separate (ptr0,si0) (ptr1,si1)"
  using assms
  apply (cases si0;cases si1;cases ptr0';cases ptr1';cases si0';cases si1';auto split: if_split_asm simp add: tainted_iff_empty_bases bases_separation)
  subgoal for _ _ a0 a1
    apply (cases a0;cases a1;auto split: if_split_asm)
    apply (metis \<gamma>_val_B.elims sseparate_B.simps(1) svalue.simps(3))
    apply (metis \<gamma>_val_B.elims sseparate_B.simps(1) svalue.simps(3))
    apply (metis \<gamma>_val_B.elims sseparate_B.simps(1) svalue.simps(3))
    by (smt (verit, ccfv_threshold) Pair_inject bases_separation spointer.inject sseparate_B.elims(2) svalue.inject)
  apply (metis Bases.\<gamma>_val_B.simps(2) Bases_axioms local.tainted_iff_empty_bases spointer.exhaust sseparate_B.simps(1))
  apply (metis \<gamma>_val_B.elims local.tainted_iff_empty_bases sseparate_B.simps(1) svalue.simps(3))
  apply (metis Bases.\<gamma>_val_B.elims Bases.sseparate_B.simps(1) Bases.tainted_iff_empty_bases Bases_axioms svalue.simps(3))
  done

lemma senclosed:
  shows "senclosed_B (ptr0', si0') (ptr1', si1') \<Longrightarrow> \<forall>\<Gamma>\<^sub>0. \<forall>ptr0\<in>\<gamma>_val_B \<Gamma>\<^sub>0 ptr0'. \<forall>si0\<in>\<gamma>_val_B \<Gamma>\<^sub>0 si0'. \<exists>ptr1\<in>\<gamma>_val_B \<Gamma>\<^sub>0 ptr1'. \<exists>si1\<in>\<gamma>_val_B \<Gamma>\<^sub>0 si1'. (ptr0, si0) = (ptr1, si1)"
  by (cases "((ptr0', si0'),(ptr1', si1'))" rule: senclosed_B.cases;cases "(ptr0',ptr1')" rule: subset'.cases;cases "(si0',si1')" rule: subset'.cases;auto split: if_split_asm)

lemma sequal:
  shows "sequal_B (ptr0', si0') (ptr1', si1') \<Longrightarrow> \<forall>\<Gamma>\<^sub>0. \<forall>ptr0\<in>\<gamma>_val_B \<Gamma>\<^sub>0 ptr0'. \<forall>si0\<in>\<gamma>_val_B \<Gamma>\<^sub>0 si0'. \<forall>ptr1\<in>\<gamma>_val_B \<Gamma>\<^sub>0 ptr1'. \<forall>si1\<in>\<gamma>_val_B \<Gamma>\<^sub>0 si1'. (ptr0, si0) = (ptr1, si1)"
  by (simp add: sequal_B_def)


lemma sequal_senclosure_B:
assumes "senclosed_B r0 r1"
    and "sequal_B r1 r2"
    and "senclosed_B r2 r1"
  shows "sequal_B r0 r2"
  using assms
  by(cases "(r0,r1)" rule: senclosed_B.cases;cases "(r2,r1)" rule: senclosed_B.cases;cases "(fst r0,fst r1)" rule: subset'.cases;auto split: if_split_asm simp add: sequal_B_def)


lemma sequal_symmetric_B:
  shows "sequal_B r0 r1 = sequal_B r1 r0"
  by (simp add: sequal_B_def)

lemma sequal_implies_senclosed_B:
  shows "sequal_B r0 r1 \<Longrightarrow> senclosed_B r0 r1"
  by (simp add: sequal_B_def)

lemma senclosed_reflexive_B:
  shows "senclosed_B r r"
  by (cases "(r,r)" rule: senclosed_B.cases;cases "(fst r,fst r)" rule: subset'.cases;cases "(snd r,snd r)" rule: subset'.cases;auto simp add: ex_in_conv non_empty)

lemma senclosed_Top_L_B:
  shows "senclosed_B r (a, si) \<Longrightarrow> senclosed_B r (\<top>, si)"
  by (cases "(r,(a, si))" rule: senclosed_B.cases;simp add: ex_in_conv non_empty split: if_split_asm)
  

lemma senclosed_Top_R_B:
  shows "senclosed_B r (a, si) \<Longrightarrow> senclosed_B r (a, \<top>)"
  by (cases a;cases si;cases r;auto)


lemma senclosed_transitive_B:
  shows "senclosed_B r0 r1 \<Longrightarrow> senclosed_B r1 r2 \<Longrightarrow> senclosed_B r0 r2"
  apply (cases r0;cases r1;cases r2;cases "(fst r0,fst r1)" rule: subset'.cases;cases "(snd r0,snd r1)" rule: subset'.cases;
          cases "(fst r0,fst r2)" rule: subset'.cases;cases "(snd r0,snd r2)" rule: subset'.cases;simp add: ex_in_conv non_empty)
  by (smt (z3) subset'.elims(2) subset'.simps(1) subset_eq svalue.distinct(1))+



lemma senclosed_sseparate_B:
  shows "senclosed_B r0 r1 \<Longrightarrow> sseparate_B r2 r1 \<Longrightarrow> sseparate_B r2 r0"
  using sseparation[of "fst r2" "snd r2" "fst r1" "snd r1"]
  apply (cases r0;cases r1;cases r2;cases "(fst r0,fst r1)" rule: subset'.cases;cases "(snd r0,snd r1)" rule: subset'.cases;
          cases "(fst r0,fst r2)" rule: subset'.cases;cases "(snd r0,snd r2)" rule: subset'.cases;simp add: ex_in_conv non_empty)
  by (smt (z3) fst_conv snd_conv spointer.inject sseparate_B.elims(1) subset'.elims(2) subset_eq svalue.distinct(1) svalue.inject)+



lemma cval_separate_symmetric[symmetric]:
  shows "cval_separate r0 r1 \<longleftrightarrow> cval_separate r1 r0"
  apply (cases r1;cases r0;auto)
  apply (metis csep_symmetric cval_separate.elims(2) cval_separate.simps(1))+
  done

lemma sseprate_symmetric_B:
  shows "sseparate_B r0 r1 = sseparate_B r1 r0"
  apply (cases "(r0,r1)" rule: sseparate_B.cases;auto simp add: ex_in_conv non_empty)
  by (metis separate_bases.elims(3) separate_bases.simps(10))+

lemma join_assoc_B:
  shows "join_B (join_B x y) z = join_B x (join_B y z)"
  by (cases x;cases y;cases z;auto)

lemma join_symmetric_B[symmetric]:
  shows "join_B x y = join_B y x"
  by (cases x;cases y;auto simp add: Un_commute)

lemma spec_of_mk_svalue_B:
  shows "eval \<Gamma>\<^sub>0 e v \<Longrightarrow> CValue v \<in> \<gamma>_val_B \<Gamma>\<^sub>0 (mk_svalue_B e)"
  by (auto simp add: is_base_of_def mk_svalue_B_def)

lemma \<gamma>_mk_init_mem_svalue:
  assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
      and "senclosed_B (a, si) (a', si')"
    shows "\<gamma>_val_B \<Gamma>\<^sub>0 (mk_init_mem_svalue_B a si) \<subseteq> \<gamma>_val_B \<Gamma>\<^sub>0 (mk_init_mem_svalue_B a' si')"
  by (auto simp add: mk_init_mem_svalue_B_def)


lemma \<gamma>_val_join_B:
  shows "\<gamma>_val_B \<Gamma>\<^sub>0 (SValue v0) \<subseteq> \<gamma>_val_B \<Gamma>\<^sub>0 (SValue (join_B v0 v1))"
  by (cases v0;cases v1;auto simp add: is_base_of_def bases_of_def split: option.splits)

lemma sseparate_join_B:
  shows "sseparate_B r (SValue (join_B x y), si) \<Longrightarrow> sseparate_B r (SValue x, si)"
  by (cases "(r,SValue x, si)" rule: sseparate_B.cases;cases y;auto split: if_split_asm)


lemma sseparate_join_size_B:
  shows "sseparate_B r (a, SValue (join_B x y)) \<Longrightarrow> sseparate_B r (a, SValue x)"
  apply (cases "(r,(a,SValue x))" rule: sseparate_B.cases;cases a;auto split: if_split_asm)
  using sseparate_B.elims(2) apply blast
  using sseparate_B.elims(2) apply blast
  using sseparate_B.elims(2) apply blast
  apply (metis (mono_tags) join_B.simps spointer.exhaust sseparate_B.simps(1))
  using sseparate_B.elims(2) apply blast
  done

lemma senclosed_join_B:
  shows "senclosed_B (SValue x, si) (SValue (join_B x y), si)"
  apply (cases "((SValue x, si),(SValue (join_B x y), si))" rule: senclosed_B.cases;cases x;cases y;auto)
  using senclosed_B.simps senclosed_reflexive_B by blast+

lemma senclosed_join_size_B:
  shows "senclosed_B (a,SValue x) (a,SValue (join_B x y))"
  apply (cases "((a,SValue x),(a,SValue (join_B x y)))" rule: senclosed_B.cases;cases x;cases y;auto)
  using senclosed_reflexive_B by force


fun sapply_op_concrete
  where "sapply_op_concrete op (SValue (Bases bs0)) (SValue (Bases bs1)) = 
        (     if op = Add  then SValue (Bases (if bs0 = {} \<or> bs1 = {} then {} else (bs0 \<union> bs1)))
         else if op = Sub then SValue (Bases bs0) 
         else \<top>)"
  | "sapply_op_concrete _ _ _ = \<top>"

lemma base_of_plus:
  shows "is_base_of b e \<Longrightarrow> eval \<Gamma>\<^sub>0 e v \<Longrightarrow> \<exists> e. is_base_of b e \<and> eval \<Gamma>\<^sub>0 e (v + v') "
  apply (rule exI[of _ " SE_Plus e (SE_Immediate v')"])
  by (auto simp add: is_base_of_def bases_of_def  split: option.splits)

lemma base_of_minus:
  shows "is_base_of b e \<Longrightarrow> eval \<Gamma>\<^sub>0 e v \<Longrightarrow> \<exists> e. is_base_of b e \<and> eval \<Gamma>\<^sub>0 e (v - v') "
  apply (rule exI[of _ "SE_Minus e (SE_Immediate v')"])
  by (auto simp add: is_base_of_def bases_of_def)

lemma overapprox_ops_B:
assumes "v\<^sub>0 \<in> \<gamma>_val_B \<Gamma>\<^sub>0 v\<^sub>0' \<union> { Tainted }"
    and "v\<^sub>1 \<in> \<gamma>_val_B \<Gamma>\<^sub>0 v\<^sub>1' \<union> { Tainted }"
    and "(cop, sop) \<in> symb_concrete_match"
  shows "cop v\<^sub>0 v\<^sub>1 \<in> \<gamma>_val_B \<Gamma>\<^sub>0 (sapply_op_concrete sop v\<^sub>0' v\<^sub>1') \<union> {Tainted}"
  apply (cases "(sop,v\<^sub>0',v\<^sub>1')" rule: sapply_op_concrete.cases,auto split: if_split_asm simp add: symb_concrete_match_def)
  apply (metis add.commute add_diff_cancel_right' base_of_minus cvalue.exhaust diff_add_cancel exists_expr_for_base exists_valuation)
  by (metis (no_types, opaque_lifting) add.commute add_diff_cancel_right' base_of_minus cvalue.exhaust diff_add_cancel ex_in_conv exists_expr_for_base exists_valuation sup.left_idem sup_bot.right_neutral)



interpretation int_B: smemory_relations sseparate_B senclosed_B sequal_B \<gamma>_val_B join_B mk_init_mem_svalue_B mk_svalue_B sapply_op_concrete
  apply (unfold_locales)
  using sseparation apply blast
  using senclosed apply blast
  using sequal apply blast
  using sequal_senclosure_B apply blast
  apply (simp add: sequal_B_def)
  using sequal_implies_senclosed_B apply blast
  using senclosed_reflexive_B apply blast
  using senclosed_Top_L_B apply blast
  using senclosed_Top_R_B apply blast
  using senclosed_transitive_B apply blast
  using senclosed_sseparate_B apply blast
  using sseprate_symmetric_B apply blast
  apply simp
  apply (metis UNIV_I \<gamma>_val_B.elims empty_iff non_empty) 
  using spec_of_mk_svalue_B apply blast
  using \<gamma>_mk_init_mem_svalue apply blast
  using join_assoc_B apply blast
  using join_symmetric_B apply blast
  using \<gamma>_val_join_B apply blast
  using sseparate_join_B apply blast
  using sseparate_join_size_B apply blast
  using senclosed_join_B apply blast
  using senclosed_join_size_B apply blast
  using overapprox_ops_B apply blast
  .


end
end
