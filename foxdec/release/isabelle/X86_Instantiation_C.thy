theory X86_Instantiation_C
  imports X86_Concrete_Semantics X86_Symbolic_Generic_Semantics
begin


typedef 'a nonempty = "{ x :: 'a set . x \<noteq> {}}"
  by auto
datatype spointer = Concrete "SE_Expr nonempty" 



fun \<gamma>_val :: "(StatePart \<Rightarrow> 64 word) \<Rightarrow> spointer \<Rightarrow> cvalue set"
  where 
     "\<gamma>_val \<Gamma>\<^sub>0 (Concrete es) = { v . \<exists> e \<in> Rep_nonempty es . \<exists> w . eval \<Gamma>\<^sub>0 e w \<and> v = CValue w  }"


lemma non_empty:
  shows "\<gamma>_val \<Gamma>\<^sub>0 a \<noteq> {}"
  apply (cases a,auto)
  by (metis (full_types) Rep_nonempty ex_in_conv exists_valuation mem_Collect_eq)



fun sequal_C'
  where "sequal_C' (SValue ptr0') (SValue ptr1') = (\<forall> \<Gamma>\<^sub>0 . \<forall> ptr0 \<in> \<gamma>_val \<Gamma>\<^sub>0 ptr0' . \<forall> ptr1 \<in> \<gamma>_val \<Gamma>\<^sub>0 ptr1' . ptr0 = ptr1)"
  | "sequal_C' a b = False"
fun senclosed_C'
  where "senclosed_C' (SValue ptr0') (SValue ptr1') = (\<forall> \<Gamma>\<^sub>0 . \<forall> ptr0 \<in> \<gamma>_val \<Gamma>\<^sub>0 ptr0' . \<exists> ptr1 \<in> \<gamma>_val \<Gamma>\<^sub>0 ptr1' . ptr0 = ptr1)"
  | "senclosed_C' a \<top> = True"
  | "senclosed_C' \<top> a = False"

fun sseparate_C
  where "sseparate_C (SValue ptr0', SValue si0') (SValue ptr1', SValue si1') = (\<forall> \<Gamma>\<^sub>0 . \<forall> ptr0 \<in> \<gamma>_val \<Gamma>\<^sub>0 ptr0' . \<forall> si0 \<in> \<gamma>_val \<Gamma>\<^sub>0 si0' . \<forall> ptr1 \<in> \<gamma>_val \<Gamma>\<^sub>0 ptr1' . \<forall> si1 \<in> \<gamma>_val \<Gamma>\<^sub>0 si1' . cval_separate (ptr0,si0) (ptr1,si1))"
  | "sseparate_C _ _ = False"
fun senclosed_C
  where "senclosed_C (a,si) (a',si') = (senclosed_C' a a' \<and> senclosed_C' si si')"

fun sequal_C
  where "sequal_C (ptr0', si0') (ptr1', si1') = (sequal_C' ptr0' ptr1' \<and> sequal_C' si0' si1')"


fun \<gamma>_val_C
  where "\<gamma>_val_C \<Gamma>\<^sub>0 \<top> = UNIV"
  | "\<gamma>_val_C \<Gamma>\<^sub>0 (SValue (Concrete es)) = \<gamma>_val \<Gamma>\<^sub>0 (Concrete es)"

fun join_C
  where 
    "join_C (Concrete ptrs0) (Concrete ptrs1) = Concrete (Abs_nonempty ((Rep_nonempty ptrs0 \<union> Rep_nonempty ptrs1)))" 

primrec (nonexhaustive) symbolize_op
  where 
    "symbolize_op Add = SE_Plus"
  | "symbolize_op Sub = SE_Minus"
  | "symbolize_op Mul = SE_Times"
  | "symbolize_op (GenericOp m f) = SE_GenericOp m f"

fun sapply_op_concrete
  where
    "sapply_op_concrete op (SValue (Concrete es0)) (SValue (Concrete es1)) = SValue (Concrete (Abs_nonempty ((\<lambda>(e0,e1) . (symbolize_op op) e0 e1) ` ((Rep_nonempty es0) \<times> (Rep_nonempty es1)))))"
  | "sapply_op_concrete op (SValue (Concrete es0)) \<top> = SValue (Concrete (Abs_nonempty ((\<lambda> e0 . (symbolize_op op) e0 SE_Bot) ` (Rep_nonempty es0))))"
  | "sapply_op_concrete op \<top> (SValue (Concrete es0)) = SValue (Concrete (Abs_nonempty ((\<lambda> e0 . (symbolize_op op) SE_Bot e0) ` (Rep_nonempty es0))))"
  | "sapply_op_concrete op \<top> \<top>  = \<top>"

definition mk_svalue_C
  where "mk_svalue_C a \<equiv> SValue (Concrete (Abs_nonempty {a}))" 

definition mk_init_value_C
  where "mk_init_value_C as sizes \<equiv> SValue (Concrete (Abs_nonempty ((\<lambda>(a,si) . SE_Var (SP_Mem a si)) ` (as \<times> sizes))))"

definition is_deterministic_spointer_C
  where "is_deterministic_spointer_C a si \<equiv> (\<forall> a' si' . senclosed_C (a', si') (a, si) \<longrightarrow> sequal_C (a', si') (a, si))"

definition mk_init_mem_svalue_C 
  where "mk_init_mem_svalue_C a si \<equiv> 
          case (a,si) of
            (SValue (Concrete as),SValue (Concrete sizes)) \<Rightarrow> if is_deterministic_spointer_C a si then mk_init_value_C (Rep_nonempty as) (Rep_nonempty sizes) else \<top>
          | _ \<Rightarrow> \<top>"




lemma sequal_senclosure_C:
assumes "senclosed_C r0 r1"
    and "sequal_C r1 r2"
    and "senclosed_C r2 r1"
  shows "sequal_C r0 r2"
  using assms
  apply(cases "(r0,r1)" rule: senclosed_C.cases;cases "(r2,r1)" rule: senclosed_C.cases;auto)
  apply (smt (verit, del_insts) senclosed_C'.elims(2) sequal_C'.elims(2) sequal_C'.simps(1) svalue.distinct(1))
  apply (smt (verit, del_insts) senclosed_C'.elims(2) sequal_C'.elims(2) sequal_C'.simps(1) svalue.distinct(1))
  done


lemma sequal_symmetric_C:
  shows "sequal_C r0 r1 = sequal_C r1 r0"
  apply (cases r0;cases r1;cases "(fst r0,fst r1)" rule: sequal_C'.cases;cases "(snd r0,snd r1)" rule: sequal_C'.cases;simp add: ex_in_conv non_empty)
  by force

lemma sequal_implies_senclosed_C:
  shows "sequal_C r0 r1 \<Longrightarrow> senclosed_C r0 r1"
  by (cases r0;cases r1;cases "(fst r0,fst r1)" rule: sequal_C'.cases;cases "(snd r0,snd r1)" rule: sequal_C'.cases;simp add: ex_in_conv non_empty)

lemma senclosed_reflexive_C:
  shows "senclosed_C r r"
  by (cases r;cases "(fst r,fst r)" rule: sequal_C'.cases;cases "(snd r,snd r)" rule: sequal_C'.cases;simp add: ex_in_conv non_empty)

lemma senclosed_Top_L_C:
  shows "senclosed_C r (a, si) \<Longrightarrow> senclosed_C r (\<top>, si)"
  by (cases "(r,(a, si))" rule: senclosed_C.cases;simp add: ex_in_conv non_empty)

lemma senclosed_Top_R_C:
  shows "senclosed_C r (a, si) \<Longrightarrow> senclosed_C r (a, \<top>)"
  by (cases a;cases si;cases r;auto)


lemma senclosed_transitive_C:
  shows "senclosed_C r0 r1 \<Longrightarrow> senclosed_C r1 r2 \<Longrightarrow> senclosed_C r0 r2"
  apply (cases r0;cases r1;cases r2;cases "(fst r0,fst r1)" rule: sequal_C'.cases;cases "(snd r0,snd r1)" rule: sequal_C'.cases;
          cases "(fst r0,fst r2)" rule: sequal_C'.cases;cases "(snd r0,snd r2)" rule: sequal_C'.cases;simp add: ex_in_conv non_empty)
  using senclosed_C'.elims(1) by auto

lemma senclosed_sseparate_C:
  shows "senclosed_C r0 r1 \<Longrightarrow> sseparate_C r2 r1 \<Longrightarrow> sseparate_C r2 r0"
  apply (cases r0;cases r1;cases r2;cases "(fst r0,fst r1)" rule: sequal_C'.cases;cases "(snd r0,snd r1)" rule: sequal_C'.cases;
          cases "(fst r0,fst r2)" rule: sequal_C'.cases;cases "(snd r0,snd r2)" rule: sequal_C'.cases;simp add: ex_in_conv non_empty)
  using sseparate_C.elims(2) by fastforce+

lemma cval_separate_symmetric[symmetric]:
  shows "cval_separate r0 r1 \<longleftrightarrow> cval_separate r1 r0"
  apply (cases r1;cases r0;auto)
  apply (metis csep_symmetric cval_separate.elims(2) cval_separate.simps(1))+
  done

lemma sseprate_symmetric_C:
  shows "sseparate_C r0 r1 = sseparate_C r1 r0"
  apply (cases r0;cases r1;cases "(fst r0,fst r1)" rule: sequal_C'.cases;cases "(snd r0,snd r1)" rule: sequal_C'.cases;simp add: ex_in_conv non_empty)
  using cval_separate_symmetric by blast

lemma join_assoc_C:
  shows "join_C (join_C x y) z = join_C x (join_C y z)"
  apply (cases x;cases y;cases z;auto)
  by (smt (verit, best) Abs_nonempty_inverse Rep_nonempty Un_empty_right inf_sup_aci(8) mem_Collect_eq sup.assoc)

lemma join_symmetric_C[symmetric]:
  shows "join_C x y = join_C y x"
  by (cases x;cases y;auto simp add: Un_commute)

lemma spec_of_mk_svalue_C:
  shows "eval \<Gamma>\<^sub>0 e v \<Longrightarrow> CValue v \<in> \<gamma>_val_C \<Gamma>\<^sub>0 (mk_svalue_C e)"
  by (auto simp add: Abs_nonempty_inverse mk_svalue_C_def)


lemma \<gamma>_val_mk_init_value:
  assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
    and "\<forall> e \<in> a . \<forall> e' \<in> a' . eval_eq \<Gamma>\<^sub>0 e e'"
    and "\<forall> e \<in> si . \<forall> e' \<in> si' . eval_eq \<Gamma>\<^sub>0 e e'"
    and "a \<noteq> {}"
    and "si \<noteq> {}"
    and "a' \<noteq> {}"
    and "si' \<noteq> {}"
  shows "\<gamma>_val_C \<Gamma>\<^sub>0 (mk_init_value_C a si) \<subseteq> \<gamma>_val_C \<Gamma>\<^sub>0 (mk_init_value_C a' si')"
  using assms
  by (auto simp add: mk_init_value_C_def \<Gamma>_sanity_def Abs_nonempty_inverse split: spointer.splits) 

lemma sequal_implies_eval_eq:
assumes "sequal_C (SValue (Concrete a), SValue (Concrete si)) (SValue (Concrete a'), SValue (Concrete si'))"
shows "\<forall> e \<in> (Rep_nonempty a) . \<forall> e' \<in> (Rep_nonempty a') . eval_eq \<Gamma>\<^sub>0 e e'"
  and "\<forall> e \<in> (Rep_nonempty si) . \<forall> e' \<in> (Rep_nonempty si') . eval_eq \<Gamma>\<^sub>0 e e'"
  using assms
  apply (auto simp add: eval_eq_def)
  by (smt (verit, best) cvalue.inject exists_valuation)+

lemma \<gamma>_mk_init_mem_svalue:
  assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
      and "senclosed_C (a, si) (a', si')"
    shows "\<gamma>_val_C \<Gamma>\<^sub>0 (mk_init_mem_svalue_C a si) \<subseteq> \<gamma>_val_C \<Gamma>\<^sub>0 (mk_init_mem_svalue_C a' si')"
  apply (auto simp add: mk_init_mem_svalue_C_def split: svalue.splits spointer.splits)
  using assms(2) senclosed_C'.simps(3) senclosed_C.simps apply blast
  using assms(2) senclosed_C'.simps(3) senclosed_C.simps apply blast
  subgoal for a0 si0 a1 si1 x
    using \<gamma>_val_mk_init_value[OF assms(1),of "Rep_nonempty a0" "Rep_nonempty a1" "Rep_nonempty si0" "Rep_nonempty si1"]
    using sequal_implies_eval_eq[of a0 si0 a1 si1 \<Gamma>\<^sub>0]
    by (smt (verit, best) Rep_nonempty assms(2) in_mono is_deterministic_spointer_C_def mem_Collect_eq)
  by (metis assms(2) is_deterministic_spointer_C_def sequal_symmetric_C senclosed_reflexive_C sequal_senclosure_C)

lemma nonempty_is_non_empty:
  shows "Rep_nonempty x \<noteq> {}"  
  using Rep_nonempty by force


lemma \<gamma>_val_join_C:
  shows "\<gamma>_val_C \<Gamma>\<^sub>0 (SValue v0) \<subseteq> \<gamma>_val_C \<Gamma>\<^sub>0 (SValue (join_C v0 v1))"
  apply (cases v0;cases v1;auto)
  by (metis (full_types) Abs_nonempty_inverse Un_iff equals0D mem_Collect_eq)

lemma sseparate_join_C:
  shows "sseparate_C r (SValue (join_C x y), si) \<Longrightarrow> sseparate_C r (SValue x, si)"
  apply (cases r;cases "fst r";cases "snd r";cases x;cases y;cases si;auto)
  by (smt (verit, del_insts) Abs_nonempty_inverse UnI2 emptyE mem_Collect_eq sup_commute)

lemma sseparate_join_size_C:
  shows "sseparate_C r (a, SValue (join_C x y)) \<Longrightarrow> sseparate_C r (a, SValue x)"
  apply (cases r;cases "fst r";cases "snd r";cases x;cases y;cases a;auto)
  by (smt (verit, del_insts) Abs_nonempty_inverse Un_empty inf_sup_ord(3) mem_Collect_eq subsetD nonempty_is_non_empty)

lemma senclosed_join_C:
  shows "senclosed_C (SValue x, si) (SValue (join_C x y), si)"
  apply (cases x;cases y;cases si;auto)
  apply (smt (verit, ccfv_threshold) Abs_nonempty_inverse Un_iff equals0D mem_Collect_eq)+
  done

lemma senclosed_join_size_C:
  shows "senclosed_C (a,SValue x) (a,SValue (join_C x y))"
  apply (cases x;cases y;cases a;auto)
  by (metis (full_types) Abs_nonempty_inverse UnI1 emptyE mem_Collect_eq)+
   


lemma overapprox_ops_C:
assumes "v\<^sub>0 \<in> \<gamma>_val_C \<Gamma>\<^sub>0 v\<^sub>0' \<union> { Tainted }"
    and "v\<^sub>1 \<in> \<gamma>_val_C \<Gamma>\<^sub>0 v\<^sub>1' \<union> { Tainted }"
    and "(cop, sop) \<in> symb_concrete_match"     
  shows "cop v\<^sub>0 v\<^sub>1 \<in> \<gamma>_val_C \<Gamma>\<^sub>0 (sapply_op_concrete sop v\<^sub>0' v\<^sub>1') \<union> {Tainted}"
  using assms
  apply (cases "(sop,v\<^sub>0',v\<^sub>1')" rule: sapply_op_concrete.cases;cases v\<^sub>0';cases v\<^sub>1';cases v\<^sub>1;cases v\<^sub>0;auto simp add: symb_concrete_match_def Abs_nonempty_inverse nonempty_is_non_empty)
  apply blast
  apply blast
  apply blast
  using add.commute apply blast
  done


interpretation int_C: smemory_relations sseparate_C senclosed_C sequal_C \<gamma>_val_C join_C mk_init_mem_svalue_C mk_svalue_C sapply_op_concrete
  apply (unfold_locales)
  subgoal for ptr0' si0' ptr1' si1
    apply (cases ptr0';cases si0';cases ptr1';cases si1;auto)
    by (metis \<gamma>_val_C.simps(2) spointer.exhaust)
  subgoal for ptr0' si0' ptr1' si1
    apply (cases ptr0';cases si0';cases ptr1';cases si1;auto)
    by (metis \<gamma>_val_C.elims svalue.distinct(1) svalue.inject)+
  subgoal for ptr0' si0' ptr1' si1
    apply (cases ptr0';cases si0';cases ptr1';cases si1;auto)
    by (metis \<gamma>_val_C.simps(2) spointer.exhaust)+
  using sequal_senclosure_C apply blast
  using sequal_symmetric_C apply blast
  using sequal_implies_senclosed_C apply blast
  using senclosed_reflexive_C apply blast
  using senclosed_Top_L_C apply blast
  using senclosed_Top_R_C apply blast
  using senclosed_transitive_C apply blast
  using senclosed_sseparate_C apply blast
  using sseprate_symmetric_C apply blast
  apply simp
  apply (metis UNIV_I \<gamma>_val_C.elims empty_iff non_empty) 
  using spec_of_mk_svalue_C apply blast
  using \<gamma>_mk_init_mem_svalue apply blast
  using join_assoc_C apply blast
  using join_symmetric_C apply blast
  using \<gamma>_val_join_C apply blast
  using sseparate_join_C apply blast
  using sseparate_join_size_C apply blast
  using senclosed_join_C apply blast
  using senclosed_join_size_C apply blast
  using overapprox_ops_C apply blast
  .

end