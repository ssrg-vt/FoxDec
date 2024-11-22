theory X86_Instantiation_S
  imports X86_Concrete_Semantics X86_Symbolic_Generic_Semantics
begin

datatype Sources = Src_Malloc "64 word" |  Src_Immediate "64 word" | Src_Reg string | Src_Mem SE_Expr SE_Expr

context binary
begin

primrec srcs_of
  where 
    "srcs_of (SE_Immediate imm) = {Src_Immediate imm}"
  | "srcs_of (SE_Malloc imm) = {Src_Malloc imm}"
  | "srcs_of (SE_Var sp) = {case sp of SP_Reg r \<Rightarrow> Src_Reg r | SP_Mem a si \<Rightarrow> Src_Mem a si}"
  | "srcs_of (SE_Plus e0 e1) = srcs_of e0 \<union> srcs_of e1"
  | "srcs_of (SE_Minus e0 e1) = srcs_of e0 \<union> srcs_of e1"
  | "srcs_of (SE_Times e0 e1) = srcs_of e0 \<union> srcs_of e1"
  | "srcs_of (SE_GenericOp m f e0 e1) = srcs_of e0 \<union> srcs_of e1"
  | "srcs_of (SE_Bot) = {}"



fun separate_sources
  where 
  "separate_sources (Src_Malloc imm0) (Src_Malloc imm1) = (imm0 \<noteq> imm1)"
| "separate_sources (Src_Malloc imm0) _ = True"
| "separate_sources _ (Src_Malloc imm0) = True"
| "separate_sources _ _ = False"

end

locale Sources = binary +
  assumes separate_sources: "eval \<Gamma>\<^sub>0 e0 v0 \<Longrightarrow> eval \<Gamma>\<^sub>0 e1 v1 \<Longrightarrow> srcs_of e0 \<noteq> {} \<Longrightarrow> srcs_of e1 \<noteq> {} \<Longrightarrow> \<forall>s0\<in> srcs_of e0 . \<forall>s1\<in>srcs_of e1. separate_sources s0 s1 \<Longrightarrow> cseparate (CValue v0, si0) (CValue v1, si1)"
begin



typedef 'a finite = "{ x :: 'a set . finite x}"
  by auto
datatype spointer = Sources "Sources finite" 


fun \<gamma>_val :: "(StatePart \<Rightarrow> 64 word) \<Rightarrow> spointer \<Rightarrow> cvalue set"
  where 
     "\<gamma>_val \<Gamma>\<^sub>0 (Sources srcs) = (if Rep_finite srcs = {} then UNIV else { v . \<exists> e . \<exists> w . srcs_of e \<noteq> {} \<and> srcs_of e \<subseteq> Rep_finite srcs \<and> eval \<Gamma>\<^sub>0 e w \<and> v = CValue w })"

primrec src_to_expr
  where 
    "src_to_expr (Src_Malloc a) = SE_Malloc a"
  | "src_to_expr (Src_Immediate imm) = SE_Immediate imm"
  | "src_to_expr (Src_Reg r) = SE_Var (SP_Reg r)"
  | "src_to_expr (Src_Mem a si) = SE_Var (SP_Mem a si)"

fun build_expr_for_sources
  where 
    "build_expr_for_sources [] = undefined"
  | "build_expr_for_sources [s] = src_to_expr s"
  | "build_expr_for_sources (s0#s1#srcs) = SE_Plus (src_to_expr s0) (build_expr_for_sources (s1#srcs))"

lemma srcs_of_src_to_expr:
  shows "srcs_of (src_to_expr s) = {s}"
  by (cases s,auto)

lemma src_of_build_expr_for_sources:
  assumes "srcs \<noteq> []"
  shows "srcs_of (build_expr_for_sources srcs) = set srcs"
  using assms
  apply (induct srcs rule: build_expr_for_sources.induct)
  by (auto simp add: srcs_of_src_to_expr)


lemma exists_expr_for_sources:
  assumes "finite srcs"
      and "srcs \<noteq> {}"
  shows "\<exists> e . srcs_of e = srcs"
  apply (rule exI[of _ "build_expr_for_sources (SOME l . set l = srcs)"])
  apply (subst src_of_build_expr_for_sources)
  apply (smt (verit, ccfv_threshold) assms(1) assms(2) empty_set finite_list verit_sko_ex')
  by (meson assms(1) finite_list someI)


lemma non_empty:
  shows "\<gamma>_val \<Gamma>\<^sub>0 a \<noteq> {}"
  apply (cases a,auto)
  by (metis Rep_finite all_not_in_conv dual_order.refl exists_expr_for_sources exists_valuation mem_Collect_eq)




fun sseparate_S
  where "sseparate_S (SValue (Sources srcs0), SValue (Sources si0)) (SValue (Sources srcs1), SValue (Sources si1)) = (Rep_finite srcs0 \<noteq> {} \<and> Rep_finite srcs1 \<noteq> {} \<and> Rep_finite si0 \<noteq> {} \<and> Rep_finite si1 \<noteq> {} \<and> (\<forall> s0 \<in> Rep_finite srcs0 . \<forall> s1 \<in> Rep_finite srcs1 . separate_sources s0 s1))"
  | "sseparate_S _ _ = False"

fun subset'
  where "subset' (SValue (Sources x)) (SValue (Sources y)) = (if Rep_finite x = {} then Rep_finite y = {} else if Rep_finite y = {} then True else Rep_finite x \<subseteq> Rep_finite y)"
  | "subset' a \<top> = True"
  | "subset' \<top> a = False"

fun senclosed_S
  where "senclosed_S (a0,si0) (a1,si1) = (subset' a0 a1 \<and> subset' si0 si1)"

definition sequal_S 
  where "sequal_S r0 r1 \<equiv> False"




fun \<gamma>_val_S
  where "\<gamma>_val_S \<Gamma>\<^sub>0 \<top> = UNIV"
  | "\<gamma>_val_S \<Gamma>\<^sub>0 (SValue (Sources es)) = \<gamma>_val \<Gamma>\<^sub>0 (Sources es)"
                                              
fun join_S
  where 
    "join_S (Sources bs0) (Sources bs1) = (if Rep_finite bs0 = {} \<or> Rep_finite bs1 = {} then Sources (Abs_finite {}) else Sources (Abs_finite (Rep_finite bs0 \<union> Rep_finite bs1)))" 


definition mk_svalue_S
  where "mk_svalue_S a \<equiv> SValue (Sources (Abs_finite (srcs_of a)))" 

definition mk_init_mem_svalue_S 
  where "mk_init_mem_svalue_S a si \<equiv> \<top>"


lemma sseparation:
  assumes "sseparate_S (ptr0',si0') (ptr1', si1')"
       and "ptr0 \<in> \<gamma>_val_S \<Gamma>\<^sub>0 ptr0'"
      and "si0 \<in> \<gamma>_val_S \<Gamma>\<^sub>0 si0'"
      and "ptr1 \<in> \<gamma>_val_S \<Gamma>\<^sub>0 ptr1'"
      and "si1 \<in> \<gamma>_val_S \<Gamma>\<^sub>0 si1'"
    shows "cval_separate (ptr0,si0) (ptr1,si1)"
  using assms
  apply (cases "((ptr0',si0'),(ptr1', si1'))" rule: sseparate_S.cases,auto simp add: separate_sources)
  by (smt (verit, best) empty_iff in_mono separate_sources)


lemma senclosed:
  shows "senclosed_S (ptr0', si0') (ptr1', si1') \<Longrightarrow> \<forall>\<Gamma>\<^sub>0. \<forall>ptr0\<in>\<gamma>_val_S \<Gamma>\<^sub>0 ptr0'. \<forall>si0\<in>\<gamma>_val_S \<Gamma>\<^sub>0 si0'. \<exists>ptr1\<in>\<gamma>_val_S \<Gamma>\<^sub>0 ptr1'. \<exists>si1\<in>\<gamma>_val_S \<Gamma>\<^sub>0 si1'. (ptr0, si0) = (ptr1, si1)"
  apply (cases "((ptr0', si0'),(ptr1', si1'))" rule: senclosed_S.cases;cases "(ptr0',ptr1')" rule: subset'.cases;cases "(si0',si1')" rule: subset'.cases;auto split: if_split_asm)
  by (metis dual_order.trans emptyE)+

lemma sequal:
  shows "sequal_S (ptr0', si0') (ptr1', si1') \<Longrightarrow> \<forall>\<Gamma>\<^sub>0. \<forall>ptr0\<in>\<gamma>_val_S \<Gamma>\<^sub>0 ptr0'. \<forall>si0\<in>\<gamma>_val_S \<Gamma>\<^sub>0 si0'. \<forall>ptr1\<in>\<gamma>_val_S \<Gamma>\<^sub>0 ptr1'. \<forall>si1\<in>\<gamma>_val_S \<Gamma>\<^sub>0 si1'. (ptr0, si0) = (ptr1, si1)"
  by (simp add: sequal_S_def)


lemma sequal_senclosure_S:
assumes "senclosed_S r0 r1"
    and "sequal_S r1 r2"
    and "senclosed_S r2 r1"
  shows "sequal_S r0 r2"
  using assms
  by(cases "(r0,r1)" rule: senclosed_S.cases;cases "(r2,r1)" rule: senclosed_S.cases;cases "(fst r0,fst r1)" rule: subset'.cases;auto split: if_split_asm simp add: sequal_S_def)


lemma sequal_symmetric_S:
  shows "sequal_S r0 r1 = sequal_S r1 r0"
  by (simp add: sequal_S_def)

lemma sequal_implies_senclosed_S:
  shows "sequal_S r0 r1 \<Longrightarrow> senclosed_S r0 r1"
  by (simp add: sequal_S_def)

lemma senclosed_reflexive_S:
  shows "senclosed_S r r"
  by (cases "(r,r)" rule: senclosed_S.cases;cases "(fst r,fst r)" rule: subset'.cases;cases "(snd r,snd r)" rule: subset'.cases;auto simp add: ex_in_conv non_empty)

lemma senclosed_Top_L_S:
  shows "senclosed_S r (a, si) \<Longrightarrow> senclosed_S r (\<top>, si)"
  by (cases "(r,(a, si))" rule: senclosed_S.cases;simp add: ex_in_conv non_empty split: if_split_asm)
  

lemma senclosed_Top_R_S:
  shows "senclosed_S r (a, si) \<Longrightarrow> senclosed_S r (a, \<top>)"
  by (cases a;cases si;cases r;auto)


lemma senclosed_transitive_S:
  shows "senclosed_S r0 r1 \<Longrightarrow> senclosed_S r1 r2 \<Longrightarrow> senclosed_S r0 r2"
  apply (cases r0;cases r1;cases r2;cases "(fst r0,fst r1)" rule: subset'.cases;cases "(snd r0,snd r1)" rule: subset'.cases;
          cases "(fst r0,fst r2)" rule: subset'.cases;cases "(snd r0,snd r2)" rule: subset'.cases;simp add: ex_in_conv non_empty)
  by (smt (z3) subset'.elims(2) subset'.simps(1) subset_eq svalue.distinct(1))+



lemma senclosed_sseparate_S:
  shows "senclosed_S r0 r1 \<Longrightarrow> sseparate_S r2 r1 \<Longrightarrow> sseparate_S r2 r0"
  using sseparation[of "fst r2" "snd r2" "fst r1" "snd r1"]
  apply (cases r0;cases r1;cases r2;cases "(fst r0,fst r1)" rule: subset'.cases;cases "(snd r0,snd r1)" rule: subset'.cases;
          cases "(fst r0,fst r2)" rule: subset'.cases;cases "(snd r0,snd r2)" rule: subset'.cases;auto simp add: ex_in_conv non_empty)
  by (metis equals0D subsetD)



lemma separate_sources_symmetric[symmetric]:
  shows "separate_sources s0 s1 \<longleftrightarrow> separate_sources s1 s0"
  apply (cases s1;cases s0;auto)
  done

lemma sseprate_symmetric_S:
  shows "sseparate_S r0 r1 = sseparate_S r1 r0"
  apply (cases "(r0,r1)" rule: sseparate_S.cases;auto simp add: ex_in_conv non_empty)
  using separate_sources_symmetric by blast+

lemma join_assoc_S:
  shows "join_S (join_S x y) z = join_S x (join_S y z)"
  apply (cases x;cases y;cases z;auto simp add: Rep_finite Abs_finite_inverse)
  apply (subst (asm) Abs_finite_inverse)
  apply auto
  using Sources.Rep_finite Sources_axioms apply blast
  using Sources.Rep_finite Sources_axioms apply force
  apply (subst Abs_finite_inverse)
  apply auto
  using Sources.Rep_finite Sources_axioms apply blast
  using Sources.Rep_finite Sources_axioms apply force
  apply (subst (asm) Abs_finite_inverse)
  apply auto
  using Sources.Rep_finite Sources_axioms apply blast
  using Sources.Rep_finite Sources_axioms apply force
  apply (subst (asm) Abs_finite_inverse)
  apply auto
  using Sources.Rep_finite Sources_axioms apply blast
  using Sources.Rep_finite Sources_axioms apply force
  apply (subst (asm) Abs_finite_inverse)
  apply auto
  using Sources.Rep_finite Sources_axioms apply blast
  using Sources.Rep_finite Sources_axioms apply force
  apply (subst (asm) Abs_finite_inverse)
  apply auto
  using Sources.Rep_finite Sources_axioms apply blast
  using Sources.Rep_finite Sources_axioms apply force
  apply (subst (asm) Abs_finite_inverse)
  apply auto
  using Sources.Rep_finite Sources_axioms apply blast
  using Sources.Rep_finite Sources_axioms apply force
  apply (subst (asm) Abs_finite_inverse)
  apply auto
  using Sources.Rep_finite Sources_axioms apply blast
  using Sources.Rep_finite Sources_axioms apply force
  apply (subst (asm) Abs_finite_inverse)
  apply auto
  using Sources.Rep_finite Sources_axioms apply blast
  using Sources.Rep_finite Sources_axioms apply force
  apply (simp add: sup_assoc)
  apply (simp add: sup_assoc)
  apply (subst (asm) Abs_finite_inverse)
  apply auto
  using Sources.Rep_finite Sources_axioms apply blast
  using Sources.Rep_finite Sources_axioms apply force
  apply (subst (asm) Abs_finite_inverse)
  apply auto
  using Sources.Rep_finite Sources_axioms apply blast
  using Sources.Rep_finite Sources_axioms apply force
  apply (subst (asm) Abs_finite_inverse)
  apply auto
  using Sources.Rep_finite Sources_axioms apply blast
  using Sources.Rep_finite Sources_axioms apply force
  apply (simp add: sup_assoc)
  apply (simp add: sup_assoc)
  done

lemma join_symmetric_S[symmetric]:
  shows "join_S x y = join_S y x"
  by (cases x;cases y;auto simp add: Un_commute)


lemma srcs_finite:
  shows "finite (srcs_of e)"
  by (induct e,auto)

lemma spec_of_mk_svalue_S:
  shows "eval \<Gamma>\<^sub>0 e v \<Longrightarrow> CValue v \<in> \<gamma>_val_S \<Gamma>\<^sub>0 (mk_svalue_S e)"
  by (auto simp add: mk_svalue_S_def srcs_finite Abs_finite_inverse)

lemma \<gamma>_mk_init_mem_svalue:
  assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
      and "senclosed_S (a, si) (a', si')"
    shows "\<gamma>_val_S \<Gamma>\<^sub>0 (mk_init_mem_svalue_S a si) \<subseteq> \<gamma>_val_S \<Gamma>\<^sub>0 (mk_init_mem_svalue_S a' si')"
  by (auto simp add: mk_init_mem_svalue_S_def)


lemma \<gamma>_val_join_S:
  shows "\<gamma>_val_S \<Gamma>\<^sub>0 (SValue v0) \<subseteq> \<gamma>_val_S \<Gamma>\<^sub>0 (SValue (join_S v0 v1))"
  apply (cases v0;cases v1;auto simp add: srcs_finite Abs_finite_inverse Rep_finite split: option.splits)
  apply (subst (asm) Abs_finite_inverse)
    apply auto
  using Rep_finite apply blast
  apply (subst (asm) Abs_finite_inverse)
    apply auto
  using Rep_finite by blast+

lemma sseparate_join_S:
  shows "sseparate_S r (SValue (join_S x y), si) \<Longrightarrow> sseparate_S r (SValue x, si)"
  apply (cases "(r,SValue x, si)" rule: sseparate_S.cases;cases y;auto split: if_split_asm)
  apply (metis Rep_finite_inverse empty_iff)
  apply (metis Rep_finite_inverse empty_iff)
  apply (metis Rep_finite_inverse empty_iff)
  apply (subst (asm) Abs_finite_inverse)
  apply (auto simp add: Rep_finite)
  using Rep_finite by blast+


lemma sseparate_join_size_S:
  shows "sseparate_S r (a, SValue (join_S x y)) \<Longrightarrow> sseparate_S r (a, SValue x)"
  apply (cases "(r,(a,SValue x))" rule: sseparate_S.cases;cases a;auto split: if_split_asm)
  using sseparate_S.elims(2) apply blast
  using sseparate_S.elims(2) apply blast
  using sseparate_S.elims(2) apply blast
  apply (metis Rep_finite_inverse join_S.simps spointer.exhaust sseparate_S.simps(1))
  using sseparate_S.elims(2) apply blast
  done

lemma senclosed_join_S:
  shows "senclosed_S (SValue x, si) (SValue (join_S x y), si)"
  apply (cases "((SValue x, si),(SValue (join_S x y), si))" rule: senclosed_S.cases;cases x;cases y;auto)
  apply (metis Rep_finite_inverse empty_iff)
  apply (metis dual_order.refl subset'.elims(3) subset'.simps(1) subset'.simps(2))
  apply (metis Rep_finite_inverse empty_iff)
  apply (metis Rep_finite_inverse empty_iff)
  apply (metis dual_order.refl subset'.elims(3) subset'.simps(1) subset'.simps(2))
  apply (subst (asm) Abs_finite_inverse)
  apply (auto simp add: Rep_finite)
  using Rep_finite apply blast
  using Rep_finite apply blast
  apply (subst (asm) Abs_finite_inverse)
  apply (auto simp add: Rep_finite)
  using Rep_finite apply blast
  using Rep_finite apply blast
  apply (subst (asm) Abs_finite_inverse)
  apply (auto simp add: Rep_finite)
  using Rep_finite apply blast
  using Rep_finite apply blast
  using senclosed_S.simps senclosed_reflexive_S by blast


lemma senclosed_join_size_B:
  shows "senclosed_S (a,SValue x) (a,SValue (join_S x y))"
  apply (cases "((a,SValue x),(a,SValue (join_S x y)))" rule: senclosed_S.cases;cases x;cases y;auto simp add: Abs_finite_inverse)
  using senclosed_reflexive_S apply force
  using senclosed_reflexive_S apply force
  using senclosed_reflexive_S apply force
  using senclosed_reflexive_S apply force
  using senclosed_reflexive_S apply force
  apply (subst (asm) Abs_finite_inverse)
  apply (auto simp add: Rep_finite)
  apply (metis Abs_finite_cases Abs_finite_inverse mem_Collect_eq)
  using Rep_finite apply blast
  apply (metis Abs_finite_inverse Rep_finite UnCI finite_Un mem_Collect_eq)
  apply (metis Abs_finite_inverse Rep_finite UnCI finite_Un mem_Collect_eq)
  done



fun sapply_op_concrete
  where "sapply_op_concrete op (SValue (Sources bs0)) (SValue (Sources bs1)) = (if Rep_finite bs0 = {} \<or> Rep_finite bs1 = {} then \<top> else SValue (Sources (Abs_finite (Rep_finite bs0 \<union> Rep_finite bs1))))"
  | "sapply_op_concrete _ _ _ = \<top>"

lemma base_of_plus:
  shows "srcs_of e \<subseteq> srcs0 \<Longrightarrow> eval \<Gamma>\<^sub>0 e v \<Longrightarrow> srcs_of e' \<subseteq> srcs1 \<Longrightarrow> eval \<Gamma>\<^sub>0 e' v'  \<Longrightarrow> srcs_of e \<noteq> {} \<Longrightarrow> srcs_of e' \<noteq> {} \<Longrightarrow> \<exists> e. srcs_of e \<subseteq> srcs0 \<union> srcs1 \<and> srcs_of e \<noteq> {} \<and> eval \<Gamma>\<^sub>0 e (v + v') "
  apply (rule exI[of _ " SE_Plus e e'"])
  by (auto simp add: split: option.splits)

lemma base_of_minus:
  shows "srcs_of e \<subseteq> srcs0 \<Longrightarrow> eval \<Gamma>\<^sub>0 e v \<Longrightarrow> srcs_of e' \<subseteq> srcs1 \<Longrightarrow> eval \<Gamma>\<^sub>0 e' v'  \<Longrightarrow> srcs_of e \<noteq> {} \<Longrightarrow> srcs_of e' \<noteq> {} \<Longrightarrow> \<exists> e. srcs_of e \<subseteq> srcs0 \<union> srcs1 \<and> srcs_of e \<noteq> {} \<and> eval \<Gamma>\<^sub>0 e (v - v') "
  apply (rule exI[of _ " SE_Minus e e'"])
  by (auto simp add: split: option.splits)

lemma base_of_times:
  shows "srcs_of e \<subseteq> srcs0 \<Longrightarrow> eval \<Gamma>\<^sub>0 e v \<Longrightarrow> srcs_of e' \<subseteq> srcs1 \<Longrightarrow> eval \<Gamma>\<^sub>0 e' v'  \<Longrightarrow> srcs_of e \<noteq> {} \<Longrightarrow> srcs_of e' \<noteq> {} \<Longrightarrow> \<exists> e. srcs_of e \<subseteq> srcs0 \<union> srcs1 \<and> srcs_of e \<noteq> {} \<and> eval \<Gamma>\<^sub>0 e (v * v') "
  apply (rule exI[of _ " SE_Times e e'"])
  by (auto simp add: split: option.splits)

lemma base_of_genop:
  shows "srcs_of e \<subseteq> srcs0 \<Longrightarrow> eval \<Gamma>\<^sub>0 e v \<Longrightarrow> srcs_of e' \<subseteq> srcs1 \<Longrightarrow> eval \<Gamma>\<^sub>0 e' v'  \<Longrightarrow> srcs_of e \<noteq> {} \<Longrightarrow> srcs_of e' \<noteq> {} \<Longrightarrow> \<exists> e. srcs_of e \<subseteq> srcs0 \<union> srcs1 \<and> srcs_of e \<noteq> {} \<and> eval \<Gamma>\<^sub>0 e (f v v') "
  apply (rule exI[of _ "(SE_GenericOp m f) e e'"])
  by (auto simp add: split: option.splits)

lemma overapprox_ops_S:
assumes "v\<^sub>0 \<in> \<gamma>_val_S \<Gamma>\<^sub>0 v\<^sub>0' \<union> { Tainted }"
    and "v\<^sub>1 \<in> \<gamma>_val_S \<Gamma>\<^sub>0 v\<^sub>1' \<union> { Tainted }"
    and "(cop, sop) \<in> symb_concrete_match"
  shows "cop v\<^sub>0 v\<^sub>1 \<in> \<gamma>_val_S \<Gamma>\<^sub>0 (sapply_op_concrete sop v\<^sub>0' v\<^sub>1') \<union> {Tainted}"
  using assms
  apply (cases "(sop,v\<^sub>0',v\<^sub>1')" rule: sapply_op_concrete.cases,auto split: if_split_asm simp add: symb_concrete_match_def)
  apply (smt (verit, best) Abs_finite_inverse Rep_finite base_of_plus emptyE finite_UnI mem_Collect_eq)
  apply (smt (verit, best) Abs_finite_inverse Rep_finite base_of_minus equals0D finite_UnI mem_Collect_eq)
  apply (smt (verit, best) Abs_finite_inverse Rep_finite base_of_times empty_iff finite_Un mem_Collect_eq)
  apply (subst (asm) Abs_finite_inverse)
  apply (auto simp add: Rep_finite)
  using Rep_finite apply blast
  using Rep_finite apply blast
  apply (subst (asm) Abs_finite_inverse)
  apply (auto simp add: Rep_finite)
  using Rep_finite apply blast
  using Rep_finite apply blast
  apply (smt (verit, best) base_of_genop equals0D)
  apply (smt (verit, best) base_of_genop equals0D)
  done



interpretation int_S: smemory_relations sseparate_S senclosed_S sequal_S \<gamma>_val_S join_S mk_init_mem_svalue_S mk_svalue_S sapply_op_concrete
  apply (unfold_locales)
  using sseparation apply blast
  using senclosed apply blast
  using sequal apply blast
  using sequal_senclosure_S apply blast
  apply (simp add: sequal_S_def)
  using sequal_implies_senclosed_S apply blast
  using senclosed_reflexive_S apply blast
  using senclosed_Top_L_S apply blast
  using senclosed_Top_R_S apply blast
  using senclosed_transitive_S apply blast
  using senclosed_sseparate_S apply blast
  using sseprate_symmetric_S apply blast
  apply simp
  apply (metis UNIV_I \<gamma>_val_S.elims empty_iff non_empty) 
  using spec_of_mk_svalue_S apply blast
  using \<gamma>_mk_init_mem_svalue apply blast
  using join_assoc_S apply blast
  using join_symmetric_S apply blast
  using \<gamma>_val_join_S apply blast
  using sseparate_join_S apply blast
  using sseparate_join_size_S apply blast
  using senclosed_join_S apply blast
  using senclosed_join_size_B apply blast
  using overapprox_ops_S apply blast
  .

end

end
