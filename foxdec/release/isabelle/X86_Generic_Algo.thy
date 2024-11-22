theory X86_Generic_Algo
  imports X86_Concrete_Semantics X86_Symbolic_Generic_Semantics
begin

context smemory_relations
begin


termination normalize1
  apply (relation "measure length")
    apply auto
  by (metis comp_def filter_False length_filter_less)

lemma length_normalize_not_increases:
  shows "length (normalize1 m) \<le> length m"
apply(induct m rule: normalize1.induct)
  apply (auto simp add: ssep_symmetric length_filter_less swrite_overlap_def)
  by (meson Suc_le_mono le_trans length_filter_le)

lemma length_normalize1:
assumes "(r0,v0) \<in> set m"
    and "(r1,v1) \<in> set m"
    and "(r0,v0) \<noteq> (r1,v1)"
    and "\<not>sseparate r0 r1"
shows "length (normalize1 m) < length m"
using assms
apply(induct m )
  apply (auto simp add: ssep_symmetric length_filter_less swrite_overlap_def)
  apply (metis assms(4) comp_apply empty_filter_conv fst_conv)
  apply (rule le_less_trans[OF length_normalize_not_increases])
  apply auto
  apply (metis comp_def filter_False length_filter_less)
  apply (metis assms(4) comp_def empty_filter_conv fst_conv)
  apply (metis (mono_tags, lifting) comp_apply filter_False le_SucE le_imp_less_Suc le_trans length_Cons length_filter_le length_filter_less not_less_eq length_normalize_not_increases)
  apply (metis comp_apply filter_empty_conv fst_conv ssep_symmetric)
  apply (metis (mono_tags, lifting) length_normalize_not_increases comp_def filter_False length_Cons length_filter_less less_Suc_eq_le less_trans_Suc not_less_eq)
  apply (metis comp_def empty_filter_conv fst_conv ssep_symmetric)
  apply (metis (mono_tags, lifting) comp_apply filter_False le_SucE le_imp_less_Suc le_trans length_Cons length_filter_le length_filter_less not_less_eq length_normalize_not_increases)
  apply (rule le_less_trans[OF length_normalize_not_increases])
  apply auto
  apply (metis comp_def filter_False length_filter_less)
  apply (rule le_less_trans[OF length_normalize_not_increases])
  apply auto
  apply (metis comp_def filter_False length_filter_less)
  done

termination normalize 
  apply (relation "measure length")
  using length_normalize1
  apply (auto simp add: sseparation_invariant_def)
  by blast+


lemma length_normalize:
  shows "length (normalize m) \<le> length m"
  apply (induct m rule: normalize.induct)
  apply (subst normalize.simps)
  using length_normalize_not_increases
  by (metis (mono_tags, lifting) nle_le order_trans)


declare normalize.simps [simp del]




















lemma join_overapproximative:
  shows "\<gamma>_sval \<Gamma>\<^sub>0 a \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (join a b)"
  by (cases a;cases b;auto simp add: join_\<A>_overapproximative)

lemma join_overapproximative_R:
  shows "\<gamma>_sval \<Gamma>\<^sub>0 b \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (join a b)"
  apply (cases a;cases b;auto simp add: join_\<A>_overapproximative)
  by (metis in_mono join_\<A>_overapproximative join_\<A>_symmetric)

lemma fold_join_overapproximative:
  shows "\<gamma>_sval \<Gamma>\<^sub>0 ptr \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (fold join ptrs ptr)"
  apply (induct ptrs arbitrary: ptr,auto)
  by (meson in_mono join_overapproximative_R)

lemma fold_join_overapproximative_R:
assumes "ptr \<in> set ptrs"
  shows "\<gamma>_sval \<Gamma>\<^sub>0 ptr \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (fold join ptrs ptr')"
  using assms
  apply (induct ptrs arbitrary: ptr',auto)
  by (meson in_mono fold_join_overapproximative join_overapproximative)

lemma mono_gamma_supremum:
assumes "ptr \<in> set ptrs"
  shows "\<gamma>_sval \<Gamma>\<^sub>0 ptr \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (supremum ptrs)"
  using assms fold_join_overapproximative_R fold_join_overapproximative
  apply (cases "ptr \<in> set (tl ptrs)",auto simp add: supremum_def)
  by (metis in_mono list.collapse list.sel(2) set_ConsD)




lemma append_subset:
  shows "set ptrs0 \<subseteq> set (ptrs0@ptrs1)"
  by (induct ptrs0, auto)


lemma sseparate_join_L:
  shows "sseparate r (join a b, si) \<Longrightarrow> sseparate r (a,si)"
  by (cases "(a,b)" rule: join.cases;cases r;auto simp add: join_\<A>_ssep no_separation_BP_Top)

lemma sseparate_join_sizes_L:
  shows "sseparate r (a, join si0 si1) \<Longrightarrow> sseparate r (a,si0)"
  by (cases "(si0,si1)" rule: join.cases;cases r;auto simp add: join_\<A>_ssep_si no_separation_BP_Top)

lemma sseparate_join_sizes_R:
  shows "sseparate r (a, join si0 si1) \<Longrightarrow> sseparate r (a,si1)"
  apply (cases "(si0,si1)" rule: join.cases;cases r;auto simp add: join_\<A>_ssep_si no_separation_BP_Top)
  by (metis join_\<A>_ssep_si join_\<A>_symmetric)

lemma join_symmetric[symmetric]:
  shows "join a b = join b a"
  by (cases "(a,b)" rule: join.cases;auto simp add: join_\<A>_ssep no_separation_BP_Top join_\<A>_symmetric)

lemma join_assoc:
  shows "join (join a b) c = join a (join b c)"
  by (cases "(a,b)" rule: join.cases;cases c;auto simp add: join_\<A>_assoc)

lemma sseparate_fold_join_acc:
assumes "sseparate r (fold join ptrs ptr, si)"
  shows "sseparate r (ptr,si)"
  using assms
proof (induct ptrs arbitrary: ptr)
  case Nil
  thus ?case
    by auto
next
  case (Cons ptr0 ptrs ptr1)
  show ?case
    using Cons(2)
    using Cons(1)[of "join ptr0 ptr1"] sseparate_join_L[of r ptr1 ptr0 si]
    apply (auto simp add: )
    using senclosed_in_sseparate_region[of "(join ptr1 ptr0,si)" "(join ptr0 ptr1,si)" r] 
          join_symmetric
    by auto
qed


lemma sseparate_in_fold_join:
  assumes "sseparate r (fold join ptrs ptr', si)"
      and "a \<in> set ptrs"
    shows "sseparate r (a,si)"
  using assms
  apply (induct ptrs arbitrary: ptr',auto)
  using sseparate_fold_join_acc sseparate_join_L
  by blast


lemma sseparate_fold_join_acc_sizes:
assumes "sseparate r (ptr,fold join sizes si)"
  shows "sseparate r (ptr,si)"
  using assms
proof (induct sizes arbitrary: si)
  case Nil
  thus ?case
    by auto
next
  case (Cons si0 sizes si1)
  show ?case
    using Cons(2)
    using Cons(1)[of "join si0 si1"]
    using  sseparate_join_sizes_R[of r ptr si0 si1]
    by (auto simp add:)
qed


lemma sseparate_fold_in_join_sizes:
  assumes "sseparate r (a, fold join sizes si')"
      and "si \<in> set sizes"
    shows "sseparate r (a,si)"
  using assms
  apply (induct sizes arbitrary: si',auto)
  using sseparate_fold_join_acc_sizes sseparate_join_sizes_L
  by blast

lemma sseparate_supremum:
  assumes "sseparate r (supremum ptrs, supremum sizes)"
      and "a \<in> set ptrs"
      and "si \<in> set sizes"
    shows "sseparate r (a,si)"
  using assms
  apply (auto simp add: supremum_def)
  by (smt (verit, del_insts) list.sel(1) list.sel(3) list.set_cases sseparate_in_fold_join sseparate_fold_join_acc sseparate_fold_join_acc_sizes sseparate_fold_in_join_sizes)

lemma sseparate_supremum_regions_Cons:
assumes "sseparate r0 (supremum_regions (r1 # rs))"
shows "sseparate r0 r1"
  using assms 
  using sseparate_supremum[of r0 "fst r1 # map fst rs" "snd r1 # map snd rs" ]
  apply (auto simp add: supremum_regions_def split: if_split_asm)
  by force

lemma sseparate_fold_join:
assumes "sseparate r0 (fold join ptrs (join a ptr), si)"
  shows "sseparate r0 (fold join ptrs a, si)"
  using assms
proof (induct ptrs arbitrary: ptr a)
  case Nil
  thus ?case
    using sseparate_join_L
    by auto
next
  case (Cons a' ptrs)
  show ?case
    using Cons(2)
    using Cons(1)[of "join a' a" ptr]
    apply (cases "(a,a')" rule: join.cases,auto split: if_split_asm)
    by (cases ptr,auto split: if_split_asm simp add: join_\<A>_assoc)
qed

lemma sseparate_fold_join_sizes:
assumes "sseparate r0 (ptr, fold join sizes (join si0 si))"
  shows "sseparate r0 (ptr, fold join sizes si0)"
  using assms
proof (induct sizes arbitrary: si0 si)
  case Nil
  thus ?case
    using sseparate_join_sizes_L
    by auto
next
  case (Cons si' sizes)
  thus ?case
    apply (cases "(si,si')" rule: join.cases;cases si0;auto)
    apply (metis join.simps(1) join_assoc)
    by (metis join.simps(3))
qed

lemma sseparate_supremum_Cons:
  shows "as \<noteq> [] \<Longrightarrow> sizes \<noteq> [] \<Longrightarrow> sseparate r0 (supremum (a # as), supremum (si # sizes)) \<Longrightarrow> sseparate r0 (supremum as, supremum sizes)"
  apply (auto simp add: supremum_def split: if_split_asm)
  by (metis fold_simps(2) list.exhaust_sel sseparate_fold_join sseparate_fold_join_sizes)



lemma sseparate_supremum_regions_Cons_over:
assumes "sseparate r0 (supremum_regions (r1 # rs))"
    and "rs \<noteq> []"
  shows "sseparate r0 (supremum_regions rs)"
  using assms 
  using sseparate_supremum_Cons[of "map fst rs" "map snd rs" r0 "fst r1" "snd r1"]
  by (auto simp add: supremum_regions_def split: if_split_asm)




lemma separate_supremum:
  assumes "sseparate r0 (supremum_regions rs)"
      and "r1 \<in> set rs"
    shows "sseparate r0 r1"
  using assms
  apply (induct rs,auto)
  using ssep_symmetric sseparate_supremum_regions_Cons apply blast
  by (metis sseparate_supremum_regions_Cons_over empty_iff list.set(1))


lemma overlap_implies_not_find_sequal:
assumes "\<not>sseparate (a,si) (a',si')"
shows "find (sequal (a',si') \<circ> fst) (filter (sseparate (a,si) \<circ> fst) m) = None"
  using assms sequal_to_sseparate_region
  by (induct m,auto)
  

lemma supremum_single[simp]:
  shows "supremum [a] = a"
  by (auto simp add: supremum_def)








lemma enclosing_region_is_unique:
assumes "sseparation_invariant m"
    and "senclosed (a', si') (fst r0)"
    and "senclosed (a', si') (fst r1)"      
    and "r0 \<in> set m"
    and "r1 \<in> set m"
    shows "r0 = r1"
  using assms
  apply (induct m, auto split: if_split_asm simp add: case_prod_unfold sseparation_invariant_def)
  apply (metis prod.exhaust_sel senclosed_not_sseparate ssep_symmetric senclosed_in_sseparate_region)
  by (metis prod.exhaust_sel senclosed_not_sseparate ssep_symmetric senclosed_in_sseparate_region)

lemma find_enclosed_Some:
assumes "sseparation_invariant m"
  assumes "senclosed (a', si') (fst r0)"
      and "r0 \<in> set m"
    shows "find (senclosed (a', si') \<circ> fst) m = Some r0"
  using assms senclosed[of a' si' "fst (fst r0)" "snd (fst r0)"]
  apply (induct m, auto split: if_split_asm simp add: sseparation_invariant_def case_prod_unfold)
  by (smt (z3) prod.exhaust_sel senclosed_not_sseparate ssep_symmetric senclosed_in_sseparate_region)


lemma find_senclosed_removeAll:
assumes "\<forall> (r0,v0) \<in> set m . \<forall> (r1,v1) \<in> set m . (r0, v0) = (r1, v1) \<or> sseparate r0 r1"
assumes "find (senclosed (a', si') \<circ> fst) (removeAll r0 m) = Some r"
  shows "find (senclosed (a', si') \<circ> fst) m = Some r"
  using assms
  apply (induct m, auto split: if_split_asm simp add: case_prod_unfold)
  by (smt (z3) comp_apply find_some prod.collapse senclosed_not_sseparate ssep_symmetric senclosed_in_sseparate_region)


lemma senclosed_union_join:
 assumes "senclosed r (a,si)"
  shows "senclosed r (join a a0,si)"
  using assms 
  apply (cases "(a,a0)" rule: join.cases;cases r;auto simp add: senclosed_BP_Top)
  using join_\<A>_senc senclosed_transitive by blast


lemma senclosed_fold_join:
 assumes "senclosed r (a,si)"
    and "a \<in> set as \<or> senclosed r (a0,si)"
  shows "senclosed r (fold join as a0,si)"
  using assms
proof (induct as arbitrary: a0)
  case Nil
  thus ?case
    by auto
next
  case (Cons a' as)
  show ?case
    using Cons(2-) Cons(1)[of "join a' a0"] 
    apply auto
    using senclosed_union_join apply blast
    apply (cases "a \<in> set as";cases "a=a'";auto simp add: senclosed_union_join)
    by (metis join_symmetric senclosed_union_join)
qed

lemma senclosed_union_join_sizes_L:
 assumes "senclosed r (a,si)"
  shows "senclosed r (a,join si si0)"
  using assms  
  apply (cases "(si,si0)" rule: join.cases;cases r;auto simp add: senclosed_BP_Top_size)
  by (meson join_\<A>_senc_si senclosed_transitive)

lemma senclosed_union_join_sizes_R:
 assumes "senclosed r (a,si0)"
  shows "senclosed r (a,join si si0)"
  using assms  
  apply (cases "(si,si0)" rule: join.cases;cases r;auto simp add: senclosed_BP_Top_size)
  by (metis join.simps(1) join_\<A>_symmetric senclosed_union_join_sizes_L)



lemma senclosed_fold_join_sizes:
 assumes "senclosed r (a,si)"
    and "si \<in> set sizes \<or> senclosed r (a,si0)"
  shows "senclosed r (a,fold join sizes si0)"
  using assms
proof (induct sizes arbitrary: si0)
  case Nil
  thus ?case
    by auto
next
  case (Cons si' sizes)
  show ?case
    using Cons(2-) Cons(1)[of "join si' si0"] 
    apply auto
    using senclosed_union_join_sizes_L apply blast
    by (cases "si \<in> set sizes";cases "si=si'";auto simp add: senclosed_union_join_sizes_R)
qed

lemma senclosed_supremum:
assumes "senclosed r0 r"
    and "r \<in> set rs"
  shows "senclosed r0 (supremum (map fst rs),supremum (map snd rs))"
  using assms
  apply (cases r;cases rs;cases "hd rs = r";auto simp add: supremum_def)
  using senclosed_fold_join[of r0 "fst r" "snd r" "tl (map fst rs)" "hd (map fst rs)"] senclosed_fold_join_sizes
  by (auto simp add: rev_image_eqI)


lemma senclosed_supremum_regions:
assumes "senclosed r0 r"
    and "r \<in> set rs"
  shows "senclosed r0 (supremum_regions rs)"
  using assms
  using senclosed_supremum[of r0 r rs]
  by (auto simp add: supremum_regions_def)







lemma snd_write_overlap_is_Tainted[simp]:
  shows "snd (swrite_overlap a si v overlap) = \<top>"
  by (auto simp add: swrite_overlap_def)


lemma sseparation_invariant_normalize:
  shows "sseparation_invariant (normalize m)"
  by (induct m rule: normalize.induct,auto simp add: normalize.simps)

lemma filter_separation_lemma:
  assumes "filter (Not \<circ> (sseparate (a, si) \<circ> fst)) m \<noteq> []"
      and "\<forall> r \<in> set ((a0, si0) # map fst (filter (Not \<circ> (sseparate (a0, si0) \<circ> fst)) m)) . sseparate (a, si) r"
    shows "filter (\<lambda>x. sseparate (a0, si0) (fst x) \<and> \<not> sseparate (a, si) (fst x)) m \<noteq> []"
  using assms
  by (induct m,auto)

lemma terge:
  assumes "find (senclosed (a, si) \<circ> fst) m = Some (r0, ba)"
      and "find (senclosed (a, si) \<circ> fst) (filter (sseparate (a0, si0) \<circ> fst) m) = Some (r1, bc)"
      and "sseparate (a0, si0) r0"
    shows "(r0,ba) = (r1,bc)"
  using assms
  by (induct m,auto split: if_split_asm)


lemma mono_gamma_read_write_overlap:
assumes "filter (Not \<circ> (sseparate (a0, si0) \<circ> fst)) m \<noteq> []"
shows "\<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (((a0, si0), v0) # m) a si) \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (swrite_overlap a0 si0 v0 (filter (Not \<circ> (sseparate (a0, si0) \<circ> fst)) m) # (filter (sseparate (a0, si0) \<circ> fst) m)) a si)"
  using assms
  apply (auto simp add: sread_mem_def swrite_overlap_def find_filter_None sseparate_not_enclosed split: option.splits prod.splits)
  using sseparate_supremum_regions_Cons apply blast
  using sseparate_supremum_regions_Cons apply blast
  apply (meson list.set_intros(1) senclosed_supremum_regions)
  apply (smt (verit) comp_apply find_some fst_conv mem_Collect_eq set_filter ssep_symmetric senclosed_in_sseparate_region)
  using sseparate_supremum_regions_Cons apply blast
  using sseparate_supremum_regions_Cons apply blast
  using sseparate_supremum_regions_Cons apply blast
  using sseparate_supremum_regions_Cons apply blast
  using sseparate_supremum_regions_Cons apply blast
  using sseparate_supremum_regions_Cons apply blast
  using sseparate_supremum_regions_Cons apply blast
  using sseparate_supremum_regions_Cons apply blast
  apply (metis (mono_tags, opaque_lifting) comp_apply empty_filter_conv find_some senclosed_not_sseparate)
  apply (metis (mono_tags, opaque_lifting) comp_apply empty_filter_conv find_some senclosed_not_sseparate)
  apply (metis (mono_tags, opaque_lifting) comp_apply empty_filter_conv find_some senclosed_not_sseparate)
  apply (metis (mono_tags, opaque_lifting) comp_apply empty_filter_conv find_some senclosed_not_sseparate)
  apply (metis (mono_tags, opaque_lifting) comp_apply empty_filter_conv find_some senclosed_not_sseparate)
  using separate_supremum filter_separation_lemma apply blast

  apply (cases "~ sseparate (a0,si0) (fst (the (find (senclosed (a, si) \<circ> fst) m)))")
  using senclosed_supremum_regions[of "(a,si)" "fst (the (find (senclosed (a, si) \<circ> fst) m))" "(a0, si0) # map fst (filter (Not \<circ> (sseparate (a0, si0) \<circ> fst)) m)"]
  using find_some[of "senclosed (a, si) \<circ> fst" m]
       apply auto
  using mem_Collect_eq apply force
  using terge apply fastforce

  using separate_supremum filter_separation_lemma apply blast
  using separate_supremum filter_separation_lemma apply blast

  apply (cases "sseparate (a0,si0) (fst (the (find (senclosed (a, si) \<circ> fst) m)))",auto)
  using terge[of a si m _ _ a0 si0]
  apply blast
  using image_iff senclosed_not_sseparate apply fastforce

  apply (cases "sseparate (a0,si0) (fst (the (find (senclosed (a, si) \<circ> fst) m)))",auto)
  using terge[of a si m _ _ a0 si0]
  apply blast
  using image_iff senclosed_not_sseparate apply fastforce
  done


lemma tmep1225_1:
  assumes "senclosed (a, si) (a',si')"
      and "((a',si'),v') \<in> set m"
    shows "\<gamma>_sval \<Gamma>\<^sub>0 v' \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0  (snd (the (find (senclosed (a, si) \<circ> fst) (normalize1 m))))"
  using assms
proof(induct m arbitrary: a' si' v' rule: normalize1.induct)
  case 1
  thus ?case
    by auto
next
  case (2 a0 si0 v0 m)
  show ?case
    using 2(2) 2(1,3-) 
    apply (auto split: option.splits if_split_asm)
   apply (auto split: option.splits if_split_asm simp add: swrite_overlap_def)
  apply (smt (verit, best) join.simps(2) list.set_intros(1) senclosed_supremum_regions subsetD supremum_regions_def join_overapproximative_R)
  apply (metis comp_apply filter_empty_conv fst_conv senclosed_not_sseparate ssep_symmetric senclosed_in_sseparate_region)
  apply (smt (verit, best) join.simps(2) list.set_intros(1) senclosed_supremum_regions subsetD supremum_regions_def join_overapproximative_R)
  apply (smt (verit, best) join.simps(2) list.set_intros(1) senclosed_supremum_regions subsetD supremum_regions_def join_overapproximative_R)

    apply (cases "senclosed (a,si) (supremum_regions ((a0, si0) # map fst (filter (Not \<circ> (sseparate (a0, si0) \<circ> fst)) m)))")
    apply (smt (verit, best) in_mono join.simps(3) supremum_regions_def join_overapproximative)
    using 2(2)[of "partition (sseparate (a0, si0) \<circ> fst) m" "fst (partition (sseparate (a0, si0) \<circ> fst) m)" "snd (partition (sseparate (a0, si0) \<circ> fst) m)" a' si' v']
    apply (cases "sseparate (a0, si0) (a', si')",auto simp add: swrite_overlap_def)
    apply blast

    using senclosed_supremum_regions[of "(a,si)" "(a',si')" "(a0, si0) # map fst (filter (Not \<circ> (sseparate (a0, si0) \<circ> fst)) m)"]
    apply auto
    by (smt (verit, best) fst_eqD image_iff mem_Collect_eq) 
qed

lemma tmep1225_1':
  assumes "senclosed (a, si) r"
      and "(r,\<top>) \<in> set m"
    shows "\<gamma>_sval \<Gamma>\<^sub>0 (snd (the (find (senclosed (a, si) \<circ> fst) (normalize1 m)))) = UNIV"
  using tmep1225_1[of a si "fst r" "snd r" \<top>] assms
  by (cases r, auto) 

lemma tmep1225_2:
  assumes "find (senclosed (a, si) \<circ> fst) (normalize1 m) = Some (r, v)"
  shows "case find (senclosed (a, si) \<circ> fst) m of None \<Rightarrow> UNIV \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 v | Some (r0,v0) \<Rightarrow> True "(*\<gamma>_val \<Gamma>\<^sub>0 v0 \<subseteq> \<gamma>_val \<Gamma>\<^sub>0 v"*)
  using assms
proof(induct m arbitrary: r v rule: normalize1.induct)
  case 1
  thus ?case
    by auto
next
  case (2 a0 si0 v0 m)
  thus ?case
    apply (auto split: option.splits if_split_asm)
     apply (cases "senclosed (a, si) (supremum_regions ((a0, si0) # map fst (filter (Not \<circ> (sseparate (a0, si0) \<circ> fst)) m)))",auto simp add: swrite_overlap_def split: if_split_asm prod.splits)
    apply (metis UNIV_I in_mono prod.collapse)
    apply (metis UNIV_I in_mono prod.collapse)
    apply (cases "senclosed (a, si) (supremum_regions ((a0, si0) # map fst (filter (Not \<circ> (sseparate (a0, si0) \<circ> fst)) m)))",auto simp add: swrite_overlap_def split: if_split_asm prod.splits)
    apply (metis (mono_tags, lifting) tmep1225_1' UNIV_I comp_apply find_cong list.set_intros(1) option.sel snd_conv)
    using find_filter_None apply fastforce
    done
qed

lemma mono_gamma_read_normalize1:
  shows "\<gamma>_sval \<Gamma>\<^sub>0 (sread_mem m a si) \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (normalize1 m) a si)"
proof(induct m rule: normalize1.induct)
  case 1
  thus ?case
    by auto
next
  case (2 a0 si0 v0 m)
  show ?case
  proof(cases "filter (Not \<circ> (sseparate (a0, si0) \<circ> fst)) m = []")
    case False
    thus ?thesis
      using 2(2)[of "partition (sseparate (a0, si0) \<circ> fst) m" "fst (partition (sseparate (a0, si0) \<circ> fst) m)" "snd (partition (sseparate (a0, si0) \<circ> fst) m)"] mono_gamma_read_write_overlap
      apply auto
      by blast
  next
    case True
    thus ?thesis
      using 2(1)[of "partition (sseparate (a0, si0) \<circ> fst) m" "fst (partition (sseparate (a0, si0) \<circ> fst) m)" "snd (partition (sseparate (a0, si0) \<circ> fst) m)"]
      apply (cases "senclosed (a,si) (a0,si0)",auto)
      apply (auto simp add: sread_mem_def[of "((a0, si0), v0) # m"] sread_mem_def[of "((a0, si0), v0) # normalize1 m"])[1]
      apply (cases "sseparate (a,si) (a0,si0)")
      using tmep1225_2[of a si m]
      apply (auto simp add: sread_mem_def[of "((a0, si0), v0) # m"] sread_mem_def[of "((a0, si0), v0) # normalize1 m"] split: option.splits if_split_asm)[1]
      apply (auto simp add: sread_mem_def split: option.splits if_split_asm)[1]
      apply (auto simp add: sread_mem_def split: option.splits if_split_asm)[1]
      apply (auto simp add: sread_mem_def split: option.splits if_split_asm)[1]
      apply (auto simp add: sread_mem_def split: option.splits if_split_asm)[1]
      apply (auto simp add: sread_mem_def split: option.splits if_split_asm)[1]
      using tmep1225_2[of a si m]
      apply (auto simp add: sread_mem_def[of "((a0, si0), v0) # m"] sread_mem_def[of "((a0, si0), v0) # normalize1 m"] split: option.splits if_split_asm)
      apply (simp add: sread_mem_def subset_iff)
      apply (simp add: sread_mem_def subset_iff)
      apply (simp add: sread_mem_def subset_iff)
      apply (simp add: sread_mem_def subset_iff)
      apply (simp add: sread_mem_def subset_iff)
      apply (simp add: sread_mem_def subset_iff)
      done
  qed
qed

lemma mono_gamma_read_normalize:
  shows "\<gamma>_sval \<Gamma>\<^sub>0 (sread_mem m a si) \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (normalize m) a si)"
  apply (induct m rule: normalize.induct)
  apply (subst normalize.simps)
  apply (auto simp del: normalize1.simps)
  by (meson subset_iff mono_gamma_read_normalize1)





lemma enclosed_overlapping_becomes_equal:
assumes "\<forall> (r1,v1) \<in> set m . (r0 = r1 \<and> ba = v1) \<or> sseparate r0 r1"
    and "find (sequal (a, si) \<circ> fst) m = Some r"
    and "\<not> sseparate (a, si) (a', si')"
    and "senclosed (a', si') r0"
  shows "r = (r0, ba)"
  using assms
  apply (induct m,auto split: if_split_asm)
  apply (metis (no_types, opaque_lifting) prod.exhaust_sel senclosed_in_sseparate_region ssep_symmetric sequal_to_sseparate_region)
  by (metis (mono_tags, opaque_lifting) prod.exhaust_sel senclosed_in_sseparate_region ssep_symmetric sequal_to_sseparate_region)




lemma enclosed_after_removing_equal:
assumes "sseparation_invariant m"
and "find (sequal (a, si) \<circ> fst) m = Some r"
and "\<not> sseparate (a, si) (a', si')"
and "\<not> senclosed (a', si') (a, si)"
shows "find (senclosed (a', si') \<circ> fst) (removeAll r m) = None"
  using assms
proof(induct m)
  case Nil
  thus ?case 
    by auto
next
  case (Cons r0 m)
  obtain a0 si0 v0 where r: "r = ((a0,si0),v0)"
    by (cases r,auto)
  moreover
  have 1: "sseparation_invariant m \<and> ( \<forall>(r1, v1)\<in>set m. r0 = (r1, v1) \<or> sseparate (fst r0) r1)"
    using Cons
    by (auto simp add: sseparation_invariant_def)
  ultimately
  show ?case
    using Cons
    apply (auto split: if_split_asm simp add:  enclosed_overlapping_becomes_equal)
    using find_None_iff sequal_implies_senclosed senclosed_in_sseparate_regions apply fastforce
    by (smt (verit, ccfv_SIG) Cons.prems(2) case_prodD case_prodI2 enclosed_overlapping_becomes_equal fst_conv prod.exhaust_sel set_ConsD snd_conv)
qed

lemma mono_gamma_read_write_alias1:
assumes "sseparation_invariant m"
  and "\<not>sseparate (a,si) (a',si')"
shows "\<gamma>_sval \<Gamma>\<^sub>0 v \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (swrite_mem1 a si v m) a' si')"
proof(cases "find (sequal (a, si) \<circ> fst) m")
  case Some
  thus ?thesis
    using assms(2)
    using enclosed_after_removing_equal[OF assms(1),of a si _ a' si']
    by (auto simp add: mono_gamma_supremum sread_mem_def Let_def swrite_overlap_def ssep_symmetric split: prod.splits list.splits option.splits)
next
  case None
  note 0 = this
  show ?thesis
  proof(cases "find (senclosed (a, si) \<circ> fst) m")
    case None
    have "find (senclosed (a', si') \<circ> fst) (filter (sseparate (a, si) \<circ> fst) m) = None"
      apply (induct m,auto)
      using assms(2) senclosed_in_sseparate_region
      by blast
    thus ?thesis
      using None 0
      apply (auto simp add: mono_gamma_supremum sread_mem_def Let_def swrite_overlap_def split: prod.splits list.splits option.splits)
      using sseparate_supremum_regions_Cons apply blast
      using sseparate_supremum_regions_Cons apply blast
      using assms(2) ssep_symmetric apply blast+
      done
  next
    case (Some r0)
    thus ?thesis
      using 0
      apply (auto simp add: sseparate_not_enclosed sread_mem_def Let_def split: prod.splits list.splits option.splits)
      apply (meson subsetD join_overapproximative)
      apply (meson subsetD join_overapproximative)
      subgoal for a0 si0 v0 a1 si1 v1 x
        using senclosed_in_sseparate_regions[of a "si" a0 si0 a' "si'" a1 si1]
              find_some [of "senclosed (a, si) \<circ> fst" m "((a0, si0), v0)"]
              find_some [of "senclosed (a',si') \<circ> fst" "removeAll ((a0, si0), v0) m" "((a1, si1), v1)"] assms
        apply (auto simp add: sseparation_invariant_def)
      apply force+
      done
      apply (metis assms(2) comp_apply find_some fst_conv ssep_symmetric senclosed_in_sseparate_region)
      subgoal for a0 si0 v0 a1 si1 v1 x
        using senclosed_in_sseparate_regions[of a "si" a0 si0 a' "si'" a1 si1]
              find_some [of "senclosed (a, si) \<circ> fst" m "((a0, si0), v0)"]
              find_some [of "senclosed (a',si') \<circ> fst" "removeAll ((a0, si0), v0) m" "((a1, si1), v1)"] assms
        apply (auto simp add: sseparation_invariant_def)
        apply force
        by (meson ssep_symmetric senclosed_in_sseparate_region)+
      apply (metis assms(2) comp_apply find_some fst_conv ssep_symmetric senclosed_in_sseparate_region)+
      done
  qed
qed


lemma mono_gamma_read_write_alias:
assumes "sseparation_invariant m"
  and "\<not>sseparate (a,si) (a',si')"
shows "\<gamma>_sval \<Gamma>\<^sub>0 v \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (swrite_mem a si v m) a' si')"
  using mono_gamma_read_normalize mono_gamma_read_write_alias1 assms
  apply (auto simp del: swrite_mem1.simps simp add: swrite_mem_def)
  by blast

lemma some_concrete_value_no_sseparate:
  assumes "a \<in> \<gamma>_sval \<Gamma>\<^sub>0 a'"
      and "a \<in> \<gamma>_sval \<Gamma>\<^sub>0 a''"
    shows "\<not>sseparate (a',simmediate si) (a'',simmediate si')"
proof-
  have "\<exists> ptr0 \<in> \<gamma>_sval \<Gamma>\<^sub>0 a' . \<exists> si0 \<in> \<gamma>_sval \<Gamma>\<^sub>0 (simmediate si) . \<exists> ptr1 \<in> \<gamma>_sval \<Gamma>\<^sub>0 a'' . \<exists> si1 \<in> \<gamma>_sval \<Gamma>\<^sub>0 (simmediate si') . \<not>cval_separate (ptr0,si0) (ptr1,si1)"
    apply (rule bexI[of _ a])
    apply (rule bexI[of _ "CValue si"])
    apply (rule bexI[of _ a])
       apply (rule bexI[of _ "CValue si'"])
    using assms
        apply (cases a;cases si;cases si';auto simp add: cseparate.simps simmediate_def)
    using assms
    by (auto simp add: simmediate_def \<gamma>_mk_svalue)
  thus ?thesis
    using sseparate[of a' "simmediate si" a'' "simmediate si'"]
    by (auto simp add: )
qed











lemma mono_gamma_read_write1: 
assumes "sseparation_invariant m"
    and "\<not> sequal (a,si) (a',si')"
    and "a' \<noteq> \<top>"
  shows "\<gamma>_sval \<Gamma>\<^sub>0 (sread_mem m a' si') \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (swrite_mem1 a si v m) a' si')"
proof(cases "find (sequal (a',si') o fst) (swrite_mem1 a si v m)")
  case (Some r)
  thus ?thesis
    using assms(2)
    apply (auto simp add: sequal_symmetric find_filter_None split: option.splits if_split_asm)

    apply (smt (verit, ccfv_threshold) sread_mem_def case_prodD case_prod_conv comp_apply comp_apply empty_filter_conv filter_True find.simps(1) find.simps(2) find_None_iff fst_conv fst_conv option.distinct(1) partition_filter_conv senclosed_not_sseparate sequal_not_sseparate set_ConsD sread_mem_def ssep_symmetric sequal_to_sseparate_region overlap_implies_not_find_sequal)

    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a', si') \<circ> fst) m)" "the (find (senclosed (a', si') \<circ> fst) (filter (sseparate (a, si) \<circ> fst) m))"]
    apply (auto split: if_split_asm option.splits simp add: find_filter_None sequal_not_sseparate sread_mem_def swrite_overlap_def)[1]
    apply (smt (verit) find_some comp_eq_dest_lhs fst_eqD mem_Collect_eq set_filter snd_conv)

    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a', si') \<circ> fst) m)" "the (find (senclosed (a', si') \<circ> fst) (filter (sseparate (a, si) \<circ> fst) m))"]
    apply (auto split: if_split_asm option.splits simp add: sequal_implies_senclosed find_filter_None sequal_not_sseparate sread_mem_def swrite_overlap_def)[1]

    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a', si') \<circ> fst) m)" "the (find (senclosed (a', si') \<circ> fst) (filter (sseparate (a, si) \<circ> fst) m))"]
    apply (auto split: if_split_asm option.splits simp add: sequal_implies_senclosed find_filter_None sequal_not_sseparate sread_mem_def swrite_overlap_def)[1]
    apply (smt (verit) comp_def empty_filter_conv find_some mem_Collect_eq sequal_not_sseparate set_filter)
    apply (smt (verit) find_some comp_eq_dest_lhs fst_eqD mem_Collect_eq set_filter snd_conv)
    apply (smt (verit) comp_apply empty_filter_conv find_some mem_Collect_eq sequal_not_sseparate set_filter)
    apply (smt (verit) comp_def empty_filter_conv find_some mem_Collect_eq sequal_not_sseparate set_filter)
    apply (smt (verit) find_some comp_eq_dest_lhs fst_eqD mem_Collect_eq set_filter snd_conv)
    apply (smt (verit) find_some comp_eq_dest_lhs fst_eqD mem_Collect_eq set_filter snd_conv)

    using assms(3) 
    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a', si') \<circ> fst) (removeAll (the (find (senclosed (a, si) \<circ> fst) m)) m))" "the (find (senclosed (a', si') \<circ> fst) m)"]
    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a', si') \<circ> fst) m)" "the (find (senclosed (a', si') \<circ> fst) (filter (sseparate (a, si) \<circ> fst) m))"]
    apply (auto simp add: sequal_implies_senclosed find_filter_None Let_def sread_mem_def find_removeAll sequal_not_sseparate sseparate_not_enclosed ssep_symmetric case_prod_unfold no_separation_BP_Top overlap_implies_not_find_sequal split: option.splits if_split_asm)[1]
    apply (metis (mono_tags, lifting) comp_apply find_None_iff option.distinct(1) find_filter_None sequal_implies_senclosed)
    apply (metis comp_eq_dest_lhs filter_is_subset find_some fst_conv subsetD)
    apply (metis (mono_tags, lifting) comp_apply find_None_iff option.distinct(1) sequal_implies_senclosed)
    apply (metis (no_types, lifting) comp_apply filter_is_subset find_some fst_conv subset_code(1))
    apply (metis (no_types, lifting) comp_apply filter_is_subset find_some fst_conv subset_eq)

    using assms(3) 
    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a', si') \<circ> fst) (removeAll (the (find (senclosed (a, si) \<circ> fst) m)) m))" "the (find (senclosed (a', si') \<circ> fst) m)"]
    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a', si') \<circ> fst) m)" "the (find (senclosed (a', si') \<circ> fst) (filter (sseparate (a, si) \<circ> fst) m))"]
    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a, si) \<circ> fst) m)" "the (find (senclosed (a', si') \<circ> fst) m)"]
    apply (auto simp add: Let_def sread_mem_def find_removeAll sequal_not_sseparate ssep_symmetric sseparate_not_enclosed no_separation_BP_Top overlap_implies_not_find_sequal split: option.splits if_split_asm)[1]
    apply (metis (no_types, lifting) comp_def filter_empty_conv find_None_iff option.simps(3) sseparate_not_enclosed)
    apply (metis (no_types, lifting) comp_def filter_empty_conv find_None_iff option.simps(3) sseparate_not_enclosed)
    apply (metis comp_def find_some fst_conv sequalI)
    apply (metis comp_apply find_some fst_eqD sequalI)
    apply (metis sequal_implies_senclosed sequal_symmetric)
    apply (metis (no_types, lifting) comp_def filter_empty_conv find_some sseparate_not_enclosed)
    apply (metis comp_def find_some fst_conv sequalI sequal_implies_senclosed sequal_symmetric)

    
    using assms(3)
    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a', si') \<circ> fst) (removeAll (the (find (senclosed (a, si) \<circ> fst) m)) m))" "the (find (senclosed (a', si') \<circ> fst) m)"]
    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a', si') \<circ> fst) m)" "the (find (senclosed (a', si') \<circ> fst) (filter (sseparate (a, si) \<circ> fst) m))"]
    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a, si) \<circ> fst) m)" "the (find (senclosed (a', si') \<circ> fst) m)"]
    apply (auto simp add: Let_def sread_mem_def find_removeAll sequal_not_sseparate ssep_symmetric sseparate_not_enclosed no_separation_BP_Top overlap_implies_not_find_sequal split: option.splits if_split_asm)[1]
    apply (metis comp_apply find_not_None find_some fst_conv)
    apply (metis comp_apply find_not_None find_some fst_conv)
    apply (metis (mono_tags, lifting) comp_apply find_cong find_not_None option.distinct(1) find_removeAll sequal_implies_senclosed)
    apply (metis snd_conv subsetD join_overapproximative_R find_removeAll_unique)
    apply (metis comp_def find_some fst_conv subsetD join_overapproximative_R)
    apply (smt (verit, del_insts) comp_apply filter_is_subset find_some fst_conv removeAll_filter_not_eq subsetD)
    apply (metis comp_def find_not_None find_some sequal_implies_senclosed)
    apply (metis (no_types, opaque_lifting) comp_eq_dest_lhs filter_empty_conv find_some sequal_not_sseparate)
    apply (smt (verit, best) comp_apply filter_is_subset find_some fst_conv removeAll_filter_not_eq subsetD)
    apply (metis comp_def find_some fst_conv sequalI sequal_implies_senclosed sequal_symmetric)


    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a, si) \<circ> fst) m)" "the (find (senclosed (a', si') \<circ> fst) m)"]
    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a', si') \<circ> fst) (removeAll (the (find (senclosed (a, si) \<circ> fst) m)) m))" "the (find (senclosed (a', si') \<circ> fst) m)"]
    using assms(3)
    apply (auto simp add: Let_def sread_mem_def find_removeAll sequal_not_sseparate ssep_symmetric sseparate_not_enclosed no_separation_BP_Top overlap_implies_not_find_sequal split: option.splits if_split_asm)[1]
    apply (metis comp_apply find_None_iff sequal_implies_senclosed find_remove_All_Some)
    apply (metis comp_apply find_None_iff sequal_implies_senclosed find_remove_All_Some)
    apply (metis comp_def find_None_iff find_some find_removeAll sequal_implies_senclosed)
    apply (metis comp_def find_None_iff option.distinct(1) sequal_implies_senclosed)
    apply (metis comp_def find_some fst_conv subsetD join_overapproximative_R)
    apply (smt (verit, best) comp_def filter_is_subset find_some fst_conv removeAll_filter_not_eq subsetD)
    apply (metis comp_def find_None_iff option.simps(3) sequal_implies_senclosed)
    apply (metis (mono_tags, lifting) comp_apply empty_filter_conv find_None_iff option.simps(3) sequal_not_sseparate)
    apply (smt (verit, best) comp_def filter_is_subset find_some fst_conv removeAll_filter_not_eq subsetD)
    apply (metis comp_def find_not_None find_some sequal_implies_senclosed)
    apply (metis comp_eq_dest_lhs empty_filter_conv find_some smemory_relations_axioms sseparate_not_enclosed)
    apply (metis comp_def find_not_None find_some sequal_implies_senclosed)

    
    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a, si) \<circ> fst) m)" "the (find (senclosed (a', si') \<circ> fst) m)"]
    using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a', si') \<circ> fst) (removeAll (the (find (senclosed (a, si) \<circ> fst) m)) m))" "the (find (senclosed (a', si') \<circ> fst) m)"]
    using assms(3)
    apply (auto simp add: Let_def sread_mem_def find_removeAll sequal_not_sseparate ssep_symmetric sseparate_not_enclosed no_separation_BP_Top overlap_implies_not_find_sequal split: option.splits if_split_asm)[1]
    apply (metis comp_def find_some sequalI sequal_implies_senclosed sequal_symmetric)
    apply (metis comp_def find_not_None find_some senclosed_transitive)
    apply (metis (no_types, lifting) comp_def filter_empty_conv find_some sequal_not_sseparate)
    apply (metis comp_def find_some sequalI sequal_implies_senclosed sequal_symmetric)
    apply (metis comp_apply find_some sequalI sequal_implies_senclosed sequal_symmetric)
    apply (metis assms(1) enclosed_after_removing_equal option.simps(3))
    apply (metis (no_types, opaque_lifting) comp_def find_some senclosed_not_sseparate senclosed_transitive sequal_implies_senclosed sequal_symmetric ssep_symmetric find_removeAll_unique)
    apply (metis (no_types, opaque_lifting) comp_eq_dest_lhs empty_filter_conv find_some sequal_not_sseparate)
    apply (smt (z3) assms(1) comp_def filter_is_subset find_some not_None_eq option.discI option.inject removeAll_filter_not_eq senclosed_refl sequal_implies_senclosed sequal_symmetric snd_conv subsetD find_enclosed_Some enclosing_region_is_unique)
    apply (metis comp_def find_not_None find_some sequal_implies_senclosed)
    apply (metis (no_types, lifting) comp_eq_dest_lhs is_deterministic_spointer_def sequalI sequal_implies_senclosed sequal_symmetric find_some)
    done
next
  case None
  note 00 = this
  thus ?thesis
  proof(cases "find (senclosed (a',si') o fst) (swrite_mem1 a si v m)")
  case None
  have 0: "find (senclosed (a', si') o fst) m = None"
  proof-
    {
      assume "find (senclosed (a', si') \<circ> fst) m \<noteq> None"
      then obtain r v where 3: "find (senclosed (a', si') \<circ> fst) m = Some (r,v)"
        by auto
      moreover
      note 2 = senclosed_supremum_regions[of "(a', si')" r "(a, si) # map fst (filter (Not \<circ> (sseparate (a, si) \<circ> fst)) m)"]
      moreover
      from 3 have 1: "senclosed (a', si') r \<and> (r,v) \<in> set m"
        by (metis comp_eq_dest_lhs find_some fst_eqD)
      moreover
      {
        assume "find (senclosed (a', si') \<circ> fst) (filter (sseparate (a, si) \<circ> fst) m) = None"
        from this and 1 
        have "r \<in> set ((a, si) # map fst (filter (Not \<circ> (sseparate (a, si) \<circ> fst)) m))"
          by (induct m,auto split: if_split_asm)
      }
      ultimately
      have False
        using None 
        apply (auto split: if_split_asm option.splits simp add: swrite_overlap_def)
        apply (simp add: filter_empty_conv)
        using 2
          apply (auto simp add: filter_empty_conv)
          apply (metis Pair_inject find_removeAll_unique)
        by (metis (no_types, lifting) find_some senclosed_transitive sequal_symmetric sequal_implies_senclosed comp_eq_dest_lhs fst_eqD find_removeAll_unique)
    }
    thus ?thesis
      by blast
  qed



  show ?thesis
  proof(cases "filter (Not \<circ> (sseparate (a', si') \<circ> fst)) (swrite_mem1 a si v m) = []")
    case True
    have 1: "filter (Not \<circ> (sseparate (a', si') \<circ> fst)) m = []"
    proof-
      {
        assume "filter (Not \<circ> (sseparate (a', si') \<circ> fst)) m \<noteq> []"
        then obtain r where "r \<in> set m \<and> \<not>sseparate (a', si') (fst r)"
          using filter_empty_conv by fastforce
        hence False
          using True 
          apply (auto split: if_split_asm option.splits simp add: swrite_overlap_def)
          apply (smt (verit, best) comp_apply empty_filter_conv)
          using separate_supremum[of "(a', si')" "(a, si) # map fst (filter (Not \<circ> (sseparate (a, si) \<circ> fst)) m)" "fst r"]
          apply (cases "sseparate (a, si) (fst r)",auto simp add: filter_empty_conv)
          by (metis (no_types, lifting) Diff_iff all_not_in_conv find_some sequal_symmetric sequal_to_sseparate_region comp_eq_dest_lhs fst_eqD insert_iff)
      }
      thus ?thesis
        by blast
    qed
    show ?thesis
      using None 0 1
      by (auto simp add: sread_mem_def case_prod_unfold simp del: swrite_mem1.simps split: if_split_asm option.splits)
  next
    case False
    thus ?thesis
      using None
      by (auto simp add: sread_mem_def case_prod_unfold split: if_split_asm option.splits)
  qed
next
  case (Some r')
  note 0 = this
  show ?thesis
  proof(cases "find (senclosed (a, si) o fst) m")
    case (Some r0)
    note 4 = find_senclosed_removeAll[OF assms(1)[unfolded sseparation_invariant_def],of a' si' r0 r']
    thus ?thesis
      using 0 00 assms(3) Some
      apply (auto simp add: sread_mem_def case_prod_unfold sseparate_not_enclosed find_removeAll 4 split: option.splits if_split_asm)


      apply (metis (no_types, lifting) find_some sseparate_not_enclosed comp_eq_dest_lhs filter_empty_conv)
      apply (metis find_some sseparate_not_enclosed comp_eq_dest_lhs filter_empty_conv)
      apply (metis comp_def find_not_None find_some)

      using find_not_None[of r0 m "senclosed (a',  si') \<circ> fst"] 
      using find_some[of "senclosed (a,  si) \<circ> fst" m r0]
      using find_some[of "senclosed (a', si') \<circ> fst" m "the (find (senclosed (a', si') \<circ> fst) m)"]
      using find_enclosed_Some[OF assms(1),of a' si' r0] 
      using 4
      using enclosing_region_is_unique[OF assms(1),of a' si' "the (find (senclosed (a', si') \<circ> fst) (removeAll (the (find (sequal (a, si) \<circ> fst) m)) m))" "the (find (senclosed (a', si') \<circ> fst) m)"]
      apply (auto simp add: sread_mem_def case_prod_unfold find_removeAll no_separation_BP_Top split: option.splits if_split_asm)[1]
      apply (meson in_mono join_overapproximative_R)
      apply (metis \<open>\<lbrakk>senclosed (a', si') (fst r0); r0 \<in> set m\<rbrakk> \<Longrightarrow> find (senclosed (a', si') \<circ> fst) m = Some r0\<close> \<open>find (senclosed (a, si) \<circ> fst) m = Some r0 \<Longrightarrow> (senclosed (a, si) \<circ> fst) r0 \<and> r0 \<in> set m\<close> comp_def option.distinct(1) senclosed_transitive)
      apply (metis (no_types, lifting) comp_apply find_some sequalI sequal_implies_senclosed sequal_symmetric)
      apply (metis \<open>find (senclosed (a, si) \<circ> fst) m = Some r0 \<Longrightarrow> (senclosed (a, si) \<circ> fst) r0 \<and> r0 \<in> set m\<close> comp_apply find_not_None senclosed_transitive)
      apply (metis comp_def find_some sequalI sequal_implies_senclosed sequal_symmetric)
      apply (metis (no_types, lifting) comp_apply filter_empty_conv find_None_iff senclosed_not_sseparate find_remove_All_Some)
      apply (metis Diff_iff \<open>\<lbrakk>senclosed (a', si') (fst (the (find (senclosed (a', si') \<circ> fst) (removeAll (the (find (sequal (a, si) \<circ> fst) m)) m)))); senclosed (a', si') (fst (the (find (senclosed (a', si') \<circ> fst) m))); the (find (senclosed (a', si') \<circ> fst) (removeAll (the (find (sequal (a, si) \<circ> fst) m)) m)) \<in> set m; the (find (senclosed (a', si') \<circ> fst) m) \<in> set m\<rbrakk> \<Longrightarrow> the (find (senclosed (a', si') \<circ> fst) (removeAll (the (find (sequal (a, si) \<circ> fst) m)) m)) = the (find (senclosed (a', si') \<circ> fst) m)\<close> find_some comp_eq_dest_lhs option.sel set_removeAll snd_eqD)
      done
  next
    case None
    thus ?thesis
      using 0 00 assms(3)
      using enclosing_region_is_unique[OF assms(1),of a' si' r' "the (find (senclosed (a', si') \<circ> fst) m)"]
      apply (auto simp add: sread_mem_def case_prod_unfold find_filter_None swrite_overlap_def Let_def split: option.splits if_split_asm)
      using is_deterministic_spointer_def apply blast
      apply (metis comp_apply empty_filter_conv find_some senclosed_not_sseparate)
      using is_deterministic_spointer_def apply blast
      using is_deterministic_spointer_def apply blast
      apply (metis comp_apply filter_is_subset find_some fst_conv snd_conv subsetD)
      apply (metis comp_apply filter_is_subset find_some fst_conv snd_conv subsetD)
      apply (metis comp_apply empty_filter_conv find_some senclosed_not_sseparate)
      apply (metis comp_apply filter_is_subset find_some fst_conv snd_conv subsetD)
      apply (metis comp_apply empty_filter_conv find_some senclosed_not_sseparate)
      apply (metis comp_apply filter_is_subset find_some fst_conv in_mono snd_conv)
      apply (metis comp_apply find_None_iff find_some sequal_implies_senclosed)
      apply (metis comp_apply empty_filter_conv find_some senclosed_not_sseparate)
      apply (metis comp_apply find_None_iff find_some sequal_implies_senclosed)
      apply (metis comp_apply find_None_iff find_some sequal_implies_senclosed)
      apply (metis comp_apply find_None_iff find_some sequal_implies_senclosed)
      apply (metis comp_apply find_None_iff find_some sequal_implies_senclosed)
      apply (metis comp_apply find_None_iff find_some sequal_implies_senclosed)
      apply (metis comp_apply find_None_iff find_some sequal_implies_senclosed)
      done
    qed
  qed
qed

lemma mono_gamma_read_write:
assumes "sseparation_invariant m"
    and "\<not> sequal (a,si) (a',si')"
    and "a' \<noteq> \<top>"
  shows "\<gamma>_sval \<Gamma>\<^sub>0 (sread_mem m a' si') \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (swrite_mem a si v m) a' si')"
  using mono_gamma_read_normalize mono_gamma_read_write1 assms
  apply (auto simp del: swrite_mem1.simps simp add: swrite_mem_def)
  by blast





  



definition regs_are_overapprox'd
  where "regs_are_overapprox'd \<Gamma>\<^sub>0 s \<sigma> \<equiv> \<forall> r . cregs s r \<in> \<gamma>_sval \<Gamma>\<^sub>0 (sread_reg \<sigma> r) \<union> { Tainted }"

definition mem_is_overapprox'd
  where "mem_is_overapprox'd \<Gamma>\<^sub>0 s \<sigma> \<equiv> \<forall> a a' si . si > 0 \<and> a \<in> \<gamma>_sval \<Gamma>\<^sub>0 a' \<and> a' \<noteq> \<top> \<longrightarrow> cread_segment (cmem s) a si \<in> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (smem \<sigma>) a' (simmediate si)) \<union> { Tainted }"

definition \<gamma> :: "(StatePart \<Rightarrow> 64 word) \<Rightarrow> '\<A> sstate \<Rightarrow> cstate set"
  where "\<gamma> \<Gamma>\<^sub>0 \<sigma> \<equiv> {s . regs_are_overapprox'd \<Gamma>\<^sub>0 s \<sigma> \<and> mem_is_overapprox'd \<Gamma>\<^sub>0 s \<sigma> }"

lemma overapprox_ops:
assumes "v\<^sub>0 \<in> \<gamma>_sval \<Gamma>\<^sub>0 v\<^sub>0' \<union> { Tainted }"
    and "v\<^sub>1 \<in> \<gamma>_sval \<Gamma>\<^sub>0 v\<^sub>1' \<union> { Tainted }"
    and "(cop, sop) \<in> symb_concrete_match"
  shows "cop v\<^sub>0 v\<^sub>1 \<in> \<gamma>_sval \<Gamma>\<^sub>0 (sop_semantics sop [v\<^sub>0', v\<^sub>1']) \<union> { Tainted }"
  using ssemantics_overapproximative[OF assms(1-2), of cop sop]
  using assms(3)
  by (cases v\<^sub>0';cases v\<^sub>1';cases v\<^sub>1;cases v\<^sub>0;auto simp add: symb_concrete_match_def)


lemmas overapprox_plus  = overapprox_ops[where cop="(+\<^sub>c)" and sop=Add,unfolded symb_concrete_match_def,simplified]
lemmas overapprox_minus = overapprox_ops[where cop="(-\<^sub>c)" and sop=Sub,unfolded symb_concrete_match_def,simplified]
lemmas overapprox_times = overapprox_ops[where cop="(*\<^sub>c)" and sop=Mul,unfolded symb_concrete_match_def,simplified]


lemma overapprox_semantics:
assumes "\<forall> i < length srcs' . srcs ! i \<in> \<gamma>_sval \<Gamma>\<^sub>0 (srcs' ! i) \<union> { Tainted }"
    and "length srcs = length srcs'"
  shows "cop_semantics op srcs \<in> \<gamma>_sval \<Gamma>\<^sub>0 (sop_semantics op srcs') \<union> { Tainted }"
  using assms
  using overapprox_plus[of "srcs ! 0" \<Gamma>\<^sub>0 "srcs' ! 0" "srcs ! 1" "srcs' ! 1"]
        overapprox_minus[of "srcs ! 0" \<Gamma>\<^sub>0 "srcs' ! 0" "srcs ! 1" "srcs' ! 1"]
        overapprox_times[of "srcs ! 0" \<Gamma>\<^sub>0 "srcs' ! 0" "srcs ! 1" "srcs' ! 1"]
  apply(cases "(op,srcs')" rule: sop_semantics.cases;cases "srcs";cases "tl srcs";cases op)
  apply (auto simp add: less_Suc_eq_0_disj)
  apply (smt (verit, best) binop.elims binop.simps(1) cvalue.simps(3))
  apply (smt (verit, best) binop.elims binop.simps(1) cvalue.simps(3))
  apply (smt (verit, best) binop.elims binop.simps(1) cvalue.simps(3))
  apply (smt (verit, best) binop.elims binop.simps(1) cvalue.simps(3))
  apply (smt (verit, best) binop.elims binop.simps(1) cvalue.simps(3))
  apply (smt (verit, best) binop.elims binop.simps(1) cvalue.simps(3))
  apply (smt (verit, best) binop.elims binop.simps(1) cvalue.simps(3))
  subgoal for m f a' b' a b
    using overapprox_ops[of a \<Gamma>\<^sub>0 a' b b' "cgenop f" "GenericOp m f"]
    by (auto simp add: symb_concrete_match_def)
  done


lemma regs_overapprox_write_operand:
assumes "s \<in> \<gamma> \<Gamma>\<^sub>0 \<sigma>" 
    and "v \<in> \<gamma>_sval \<Gamma>\<^sub>0 v' \<union> { Tainted }"
  shows "cregs (cwrite_operand dst v s) r \<in> \<gamma>_sval \<Gamma>\<^sub>0 (sread_reg (swrite_operand dst v' \<sigma>) r) \<union> { Tainted }"
  using assms
  apply (cases dst,auto split: option.splits cvalue.splits bool.splits simp add: cwrite_mem_def cwrite_reg_def swrite_reg_def thrash_memory_def \<gamma>_def Let_def regs_are_overapprox'd_def sread_reg_def)
  by (metis not_Some_eq)+

lemma regs_overapprox_op_semantics:
  assumes "s \<in> \<gamma> \<Gamma>\<^sub>0 \<sigma>" 
      and "\<forall> i < length srcs' . srcs ! i \<in> \<gamma>_sval \<Gamma>\<^sub>0 (srcs' ! i) \<union> { Tainted }"
      and "length srcs = length srcs'"
  shows "cregs (cwrite_operand dst (cop_semantics op srcs) s) r
            \<in> \<gamma>_sval \<Gamma>\<^sub>0 (sread_reg (swrite_operand dst (sop_semantics op srcs') \<sigma>) r) \<union> { Tainted }"
  apply (rule regs_overapprox_write_operand[OF assms(1)])
  by (rule overapprox_semantics[OF assms(2-)])


lemma overapprox_resolve:
assumes "s \<in> \<gamma> \<Gamma>\<^sub>0 \<sigma>" 
  shows "cresolve_address s base offset \<in> \<gamma>_sval \<Gamma>\<^sub>0 (sresolve_address \<sigma> base offset) \<union> { Tainted }"
  using assms
  using overapprox_plus[of "cregs s base" \<Gamma>\<^sub>0 "the (sregs \<sigma> base)" "CValue offset" "mk_svalue (SE_Immediate offset)"]
  apply (cases "sread_reg \<sigma> base";cases "cregs s base";auto simp add: cread_reg_def cresolve_address_def sresolve_address_def \<gamma>_def regs_are_overapprox'd_def sread_reg_def simmediate_def split: option.splits)
  apply (metis \<gamma>_mk_svalue binop.simps(1) cvalue.distinct(1) eval.simps(2) option.distinct(1) overapprox_plus)
  using \<gamma>_mk_svalue eval.simps(2) apply presburger
  apply (metis \<gamma>_mk_svalue binop.simps(1) cvalue.distinct(1) eval.simps(2) option.distinct(1) overapprox_plus)
  by (metis \<gamma>_mk_svalue cvalue.simps(3) eval.simps(2))



lemma regs_overapprox_exec_instr:
assumes "s \<in> \<gamma> \<Gamma>\<^sub>0 \<sigma>" 
  shows "regs_are_overapprox'd \<Gamma>\<^sub>0 (cexec_instr i s) (sexec_instr i \<sigma>)"
proof-
  {
    fix n
    assume "n < length (i_srcs i)"
    hence "cread_operand s (i_srcs i ! n) \<in> \<gamma>_sval \<Gamma>\<^sub>0 (sread_operand \<sigma> (i_srcs i ! n)) \<union> { Tainted }"
    proof(cases "i_srcs i ! n")
      case (FromReg r)
      thus ?thesis
        using assms
        by (auto simp add: \<gamma>_def cread_reg_def sread_reg_def Let_def regs_are_overapprox'd_def split: option.splits)
    next
      case (FromMemory base offset si)
      have 1: "\<And> a a' si . si > 0 \<and> a \<in> \<gamma>_sval \<Gamma>\<^sub>0 a' \<and> a' \<noteq> \<top> \<longrightarrow> cread_segment (cmem s) a si \<in> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (smem \<sigma>) a' (simmediate si)) \<union> { Tainted }"
        using assms
        by (auto simp add: \<gamma>_def mem_is_overapprox'd_def)
      show ?thesis
        using FromMemory Rep_pos 1 
        apply (auto simp add: \<gamma>_def cread_reg_def sread_reg_def Let_def regs_are_overapprox'd_def mem_is_overapprox'd_def cread_mem_def simmediate_def split: cvalue.splits bool.splits option.splits)
        by (metis UnE assms cvalue.distinct(1) singleton_iff overapprox_resolve)
    next
      case (FromImmediate x3)
      thus ?thesis
        by (auto simp add: \<gamma>_mk_svalue simmediate_def)
    qed
  }
  thus ?thesis
    using assms regs_overapprox_op_semantics[OF assms(1), of "map (sread_operand \<sigma>) (i_srcs i)" "map (cread_operand s) (i_srcs i)" "i_dest i" "i_op i"]
    by (auto simp add:  sexec_instr_def cexec_instr_def regs_are_overapprox'd_def) 
qed



lemma sep_values_not_sequal:
  assumes "CValue a \<in> \<gamma>_sval \<Gamma>\<^sub>0 a'"
      and "CValue a_write \<in> \<gamma>_sval \<Gamma>\<^sub>0 a'_write"
      and "cseparate (CValue a, si) (CValue a_write, si')"
    shows "\<not>sequal (a', simmediate si) (a'_write, simmediate si')"
  using assms \<gamma>_non_empty
  using sequal[of a' "simmediate si" a'_write "simmediate si'"]
  apply (cases a'_write;cases a_write;cases a';auto simp add: simmediate_def cseparate.simps)
  apply auto
  apply (metis (full_types) cvalue.distinct(1) ex_in_conv)
  apply (metis (full_types) cvalue.distinct(1) equals0I)
  apply (metis cseparate.simps(1) equals0I)
  apply (metis cseparate.simps(1) equals0I)
  done

lemma cread_segment_tainted:
  shows "cread_segment (cmem (cwrite_mem a si v s)) Tainted si' = Tainted"
  by (auto simp add: cread_segment_def cwrite_mem_def append_singleton cseparate.simps split: cvalue.splits if_split_asm list.splits)



lemma mem_overapprox_op_semantics:
  assumes "sseparation_invariant (smem \<sigma>)"
      and "cseparation_invariant (cmem s)"
      and "s \<in> \<gamma> \<Gamma>\<^sub>0 \<sigma>" 
      and "v \<in> \<gamma>_sval \<Gamma>\<^sub>0 v' \<union> { Tainted }"
      and "a \<in> \<gamma>_sval \<Gamma>\<^sub>0 a'"
      and "si > 0"
      and "a' \<noteq> \<top>"
    shows "cread_segment (cmem (cwrite_operand dst v s)) a si \<in> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (smem (swrite_operand dst v' \<sigma>)) a' (simmediate si)) \<union> { Tainted }"
proof(cases dst)
  case (ToReg x1)
  thus ?thesis
    using assms
    by (auto simp add: cwrite_reg_def swrite_reg_def \<gamma>_def mem_is_overapprox'd_def)
next
  case (ToMemory base offset si')
  show ?thesis
  proof(cases "cresolve_address s base offset")
    case Tainted
    have "cread_segment (cmem (thrash_memory s)) a si = Tainted"
      by (auto simp add: thrash_memory_def cread_segment_def)
    thus ?thesis
      using ToMemory Tainted 
      by (auto split: cvalue.splits bool.splits)
  next
    case (CValue a_write)
    have 0: "cresolve_address s base offset \<in> \<gamma>_sval \<Gamma>\<^sub>0 (sresolve_address \<sigma> base offset) \<union> { Tainted }"
      using overapprox_resolve assms
      by auto
    consider "(CValue a_write,Rep_pos si') = (a,si)" | "cseparate (a,si) (CValue a_write,Rep_pos si')" | "(CValue a_write,Rep_pos si') \<noteq> (a,si) \<and> \<not>cseparate (a,si) (CValue a_write,Rep_pos si')"
      by auto
    thus ?thesis
    proof (cases)
      case 1
      thus ?thesis
        using ToMemory assms 0 CValue
        using mono_gamma_read_write_alias[of "smem \<sigma>" "sresolve_address \<sigma> base offset" "simmediate si" a' "simmediate si" \<Gamma>\<^sub>0 v']
              some_concrete_value_no_sseparate[of a \<Gamma>\<^sub>0 "sresolve_address \<sigma> base offset" a' si si]
        by (auto simp add: cread_write_mem_alias case_prod_unfold simp del: swrite_mem1.simps split: cvalue.splits)
    next
      case 2
      obtain a_v where 1: "a = CValue a_v"
        apply (cases a,auto)
        using 2 cseparate.simps(2) assms(7)
        by blast
      moreover
      have "\<not> sequal (a', simmediate si) (sresolve_address \<sigma> base offset, simmediate (Rep_pos si'))"
        using 0 1 assms(5) 2 CValue sep_values_not_sequal
        by auto
      ultimately
      show ?thesis
        using ToMemory assms CValue 0 2 Rep_pos 
        using cread_write_mem_separate[of s a_write "Rep_pos si'" a_v si "cread_segment (cmem (cwrite_mem a_write (Rep_pos si') v s)) (CValue a_v) si" v]
        using mono_gamma_read_write 
        using sep_values_not_sequal[of a_v \<Gamma>\<^sub>0 a' a_write "sresolve_address \<sigma> base offset" si "Rep_pos si'"]
        apply (auto simp add: csep_symmetric  mem_is_overapprox'd_def \<gamma>_def case_prod_unfold sequal_symmetric simp del: swrite_mem1.simps split: cvalue.splits)
        using sequal_symmetric by blast+
    next
      case 3
      show ?thesis
        using ToMemory CValue 3 
        using cread_write_mem_overlap[of a_write "Rep_pos si'" _ si v s]
        by (cases a,auto split: bool.splits option.splits simp add: cread_segment_tainted csep_symmetric)
    qed
  qed
qed


(*
lemma no_bases_of_resolved_sym_address:
assumes "s \<in> \<gamma> \<Gamma>\<^sub>0 \<sigma>" 
    and "\<not>tainted (cresolve_address s base offset)"
    and "within_local_segment (\<Gamma>\<^sub>0 ''rsp'')"
  shows "bases_of_ptr (sresolve_address \<sigma> base offset) = None"
proof-
  obtain imm b where 1: "cread_reg s base +\<^sub>c CValue (of_nat offset) True = CValue imm b \<and> \<not> within_local_segment imm \<and> \<not> within_global_segment imm"
    using assms(2)
    apply auto
    apply (cases "cread_reg s base +\<^sub>c CValue (of_nat offset) True",auto simp add: cresolve_address_def split: if_split_asm)



  show ?thesis
  proof(cases "sregs \<sigma> base")
    case (Concrete bs)
    hence "cregs s base \<in> { v . \<exists> e \<in> bs . eval \<Gamma>\<^sub>0 e = v }"
      using assms(1)
      apply (auto simp add: \<gamma>_def regs_are_overapprox'd_def)
      by (smt (verit, ccfv_SIG) \<gamma>_val.simps(2) mem_Collect_eq)
    then obtain e where e: "e \<in> bs \<and> (eval \<Gamma>\<^sub>0 e) = cregs s base"
      by auto
    then obtain es where es: "sresolve_address \<sigma> base offset = Concrete es \<and> SE_Plus e (SE_Immediate (of_nat offset)) \<in> es"
      using Concrete
      by (cases "sread_reg \<sigma> base",auto simp add: sresolve_address_def sapply_op_concrete_def sread_reg_def)



    have 2: "bases_of e = None"
      apply (rule expression_has_no_bases[of \<Gamma>\<^sub>0 _ "cregs s base"])
      using e 1 unescapable_local unescapable_global assms(3)
      by (auto simp add: cread_reg_def unescapable_def)
    moreover
    have "\<not> within_global_segment (word_of_nat offset)"
      using "1" unescapable_def unescapable_global by force
    ultimately
    have 3: "bases_of (SE_Plus e (SE_Immediate (of_nat offset))) = None"
      by auto
    thus ?thesis
      apply (auto split: if_split_asm)
      using union_option.elims apply blast
      by (metis "3" bases_of_ptr.simps(1) Union_option_None es imageI)
  next
    case BP_Top
    thus ?thesis
      by (auto simp add: sresolve_address_def sread_reg_def)
  qed
qed







lemma Union_option_None[simp]:
  shows "\<Union>\<^sub>o x = None \<longleftrightarrow> None \<in> x"
  by (auto simp add: Union_option_def)



lemma expression_has_no_bases:
assumes "eval \<Gamma>\<^sub>0 e = imm"
    and "\<not> within_local_segment imm"
    and "\<not> within_global_segment imm"
    and "within_local_segment (\<Gamma>\<^sub>0 ''rsp'')"
  shows "bases_of e = None"
  using assms
  apply (induct e arbitrary: imm,auto)
  apply (metis bases_of.simps(3) eval.simps(3) unescapable_def unescapable_global unescapable_local union_option.simps(4))
  by (metis eval.simps(4) unescapable_def unescapable_global unescapable_local)
*)





lemma mem_overapprox_exec_instr:
  assumes "sseparation_invariant (smem \<sigma>)"
      and "cseparation_invariant (cmem s)"
      and "s \<in> \<gamma> \<Gamma>\<^sub>0 \<sigma>" 
  shows "mem_is_overapprox'd \<Gamma>\<^sub>0 (cexec_instr i s) (sexec_instr i \<sigma>)"
proof-
  {
    fix n
    assume "n < length (i_srcs i)"
    hence "cread_operand s (i_srcs i ! n) \<in> \<gamma>_sval \<Gamma>\<^sub>0 (sread_operand \<sigma> (i_srcs i ! n)) \<union> { Tainted }"
    proof(cases "i_srcs i ! n")
      case (FromReg r)
      thus ?thesis
        using assms
        by (auto simp add: \<gamma>_def cread_reg_def sread_reg_def Let_def regs_are_overapprox'd_def  split: option.splits)
    next
      case (FromMemory base offset si)
      have "cresolve_address s base offset \<in> \<gamma>_sval \<Gamma>\<^sub>0 (sresolve_address \<sigma> base offset) \<union> { Tainted }"
        using overapprox_resolve assms
        by auto
      thus ?thesis
        using assms FromMemory Rep_pos
        apply (auto simp add: \<gamma>_def cread_reg_def sread_reg_def Let_def regs_are_overapprox'd_def mem_is_overapprox'd_def cread_mem_def simmediate_def split: cvalue.splits bool.splits option.splits)
        by blast
    next
      case (FromImmediate x3)
      thus ?thesis
        by (auto simp add: simmediate_def \<gamma>_mk_svalue)
    qed
  }

  thus ?thesis
    using assms
    apply (auto simp add: sexec_instr_def cexec_instr_def mem_is_overapprox'd_def ) 
    subgoal for a a' si
      using mem_overapprox_op_semantics[OF assms(1,2,3),of "cop_semantics (i_op i) (map (cread_operand s) (i_srcs i))" "sop_semantics (i_op i) (map (sread_operand \<sigma>) (i_srcs i))" a a' si
              "i_dest i"] 
      using overapprox_semantics
      by auto
    done
qed


lemma overapprox_step:
  assumes "sseparation_invariant (smem \<sigma>)"
      and "cseparation_invariant (cmem s)"
      and "s \<in> \<gamma> \<Gamma>\<^sub>0 \<sigma>" 
  shows "cstep i s \<in> \<gamma> \<Gamma>\<^sub>0 (sstep i \<sigma>)"
proof-
  have rip: "cregs s ''rip'' \<in> \<gamma>_sval \<Gamma>\<^sub>0 (sread_reg \<sigma> ''rip'') \<union> { Tainted }"
    using assms
    by (auto simp add: \<gamma>_def regs_are_overapprox'd_def)
  hence "(cset_rip i s) \<in> \<gamma> \<Gamma>\<^sub>0 (sset_rip i \<sigma>)"
    using overapprox_plus[of "cregs s ''rip''" \<Gamma>\<^sub>0 "the (sregs \<sigma> ''rip'')" "CValue (i_size i)" "mk_svalue (SE_Immediate (i_size i))"]
    using assms(3)
    apply (cases "cregs s ''rip''";cases "sregs \<sigma> ''rip''";cases "the (sregs \<sigma> ''rip'')";auto simp add: sread_reg_def cset_rip_def sset_rip_def smodify_reg_def cmodify_reg_def \<gamma>_def regs_are_overapprox'd_def mem_is_overapprox'd_def simmediate_def)
    apply (metis \<gamma>_mk_svalue binop.simps(1) cvalue.simps(3) eval.simps(2) overapprox_plus)
    apply (metis \<gamma>_mk_svalue binop.simps(1) cvalue.simps(3) eval.simps(2) overapprox_plus)
    apply (metis \<gamma>_mk_svalue binop.simps(1) cvalue.simps(3) eval.simps(2) iso_tuple_UNIV_I overapprox_plus \<gamma>_top)
    apply (metis \<gamma>_mk_svalue binop.simps(1) cvalue.simps(3) eval.simps(2) overapprox_plus)
    done
  moreover
  have "cmem (cset_rip i s) = cmem s"
    by (auto simp add: cset_rip_def cmodify_reg_def)
  moreover
  have "smem (sset_rip i \<sigma>) = smem \<sigma>"
    by (auto simp add: sset_rip_def smodify_reg_def)
  ultimately
  have "(cexec_instr i (cset_rip i s)) \<in> \<gamma> \<Gamma>\<^sub>0 (sexec_instr i (sset_rip i \<sigma>))"
    using assms
    using mem_overapprox_exec_instr
    by (auto simp add: \<gamma>_def regs_overapprox_exec_instr)
  thus ?thesis
    by (auto simp add: cstep_def sstep_def)
qed





(* INVARIANT *)

lemma sseparation_invariant_removeAll:
assumes "sseparation_invariant m"
  shows "sseparation_invariant (removeAll (r, v) m)"
  using assms
  apply(induct m)
  by (auto simp add: ssep_symmetric sseparation_invariant_def)

lemma sseparation_invariant_Cons:
assumes "sseparation_invariant m"
    and "\<forall> (r0, v0)\<in>set m . sseparate (fst r) r0"
  shows "sseparation_invariant (r # m)"
  using assms
  apply(induct m)
  by (auto simp add: ssep_symmetric sseparation_invariant_def)

lemma sseparation_invariant_ConsE:
  shows "sseparation_invariant (r#m) \<longleftrightarrow> sseparation_invariant m \<and> (\<forall> r0 \<in> set m . r0 = r\<or> sseparate (fst r0) (fst r))"
  by (auto simp add: sseparation_invariant_def ssep_symmetric)


lemma sseparation_invariant_enclosed:
  shows"sseparation_invariant m \<Longrightarrow>
       find (senclosed (a, si) \<circ> fst) m = Some (r, v) \<Longrightarrow>
       sseparation_invariant ((r, v') # removeAll (r,v) m)"
  apply (rule sseparation_invariant_Cons)
  apply (rule sseparation_invariant_removeAll)
  apply (auto simp add: sseparation_invariant_def)
  by (metis (mono_tags, lifting) case_prodD find_some)+


lemma sseparation_invariant_filter:
assumes "sseparation_invariant m"
shows "sseparation_invariant (filter P m)"
  using assms
  by (induct m,auto simp add: sseparation_invariant_def)



lemma sseparation_invariant_swrite_mem:
assumes "sseparation_invariant m"
shows "sseparation_invariant (swrite_mem a si v m)"
  using assms sseparation_invariant_normalize
  by (auto simp add: Let_def swrite_mem_def simp del: swrite_mem1.simps split: option.splits)

lemma sseparation_invariant_swrite_operand:
assumes "sseparation_invariant (smem \<sigma>)"
shows "sseparation_invariant (smem (swrite_operand op v \<sigma>))"
  using assms sseparation_invariant_swrite_mem
  by (cases op,auto simp add: swrite_reg_def)

lemma sseparation_invariant_sset_rip:
assumes "sseparation_invariant (smem \<sigma>)"
shows "sseparation_invariant (smem (sset_rip i \<sigma>))"
  using assms
  by (auto simp add: sset_rip_def smodify_reg_def)

lemma sseparation_invariant_sstep:
assumes "sseparation_invariant (smem \<sigma>)"
shows "sseparation_invariant (smem (sstep i \<sigma>))"
  using assms sseparation_invariant_swrite_operand sseparation_invariant_sset_rip
  by (auto simp add: sstep_def sexec_instr_def sset_rip_def)









fun join_mem
  where "join_mem m0 m1 [] = []"
  | "join_mem m0 m1 ((a,si)#regions) = ((a,si),join (sread_mem m0 a si) (sread_mem m1 a si))#join_mem m0 m1 regions"

lemma find_append:
  shows "find P (x@y) = (case find P x of None \<Rightarrow> find P y | Some a \<Rightarrow> Some a)"
  by (induct x,auto)

lemma sread_mem_append:
  shows "sread_mem (m0@m1) a si = 
          (if find (senclosed (a, si) \<circ> fst) m0 \<noteq> None then sread_mem m0 a si
           else if find (senclosed (a, si) \<circ> fst) m1 \<noteq> None then sread_mem m1 a si
           else if snd (partition (sseparate (a, si) \<circ> fst) (m0 @ m1)) = [] \<and> a \<noteq> \<top> then mk_init_mem_svalue a si
           else \<top>
          )"
  by (auto simp add: find_append sread_mem_def split: option.splits)

lemma sread_mem_singleton:
  shows "sread_mem [((a',si'),v')] a si = 
(case find (senclosed (a, si) \<circ> fst) [((a', si'), v')] of
     None \<Rightarrow> let (separate, overlap) = partition (sseparate (a, si) \<circ> fst) [((a', si'), v')] in if overlap = [] \<and> a \<noteq> \<top> then mk_init_mem_svalue a si else \<top> | Some (r, v) \<Rightarrow> v)"
  by (auto simp add: sread_mem_def)



lemma mono_gamma_read_enclosed:
  assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
      and "senclosed (a, si) (a', si')"
      and "a \<noteq> \<top>"
      and "sseparation_invariant m0"
  shows "\<gamma>_sval \<Gamma>\<^sub>0 (sread_mem m0 a si) \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem m0 a' si')"
  using assms
  using enclosing_region_is_unique[of m0 a si "the (find (senclosed (a, si) \<circ> fst) m0)"  "the (find (senclosed (a', si') \<circ> fst) m0)"]
  apply (auto simp add: sread_mem_def filter_empty_conv split: option.splits)
  apply (meson subsetD \<gamma>_mk_init_mem_svalue)
  apply (metis (mono_tags, lifting) find_some senclosed_transitive comp_eq_dest_lhs find_None_iff fst_eqD)
  apply (metis (mono_tags, lifting) find_some senclosed_transitive comp_eq_dest_lhs find_None_iff fst_eqD)
  apply (metis (mono_tags, lifting) find_some senclosed_transitive comp_eq_dest_lhs find_None_iff fst_eqD)
  apply (metis (mono_tags, opaque_lifting) comp_def find_None_iff option.distinct(1) senclosed_not_sseparate)
  apply (metis (mono_tags, opaque_lifting) comp_def find_None_iff option.distinct(1) senclosed_not_sseparate)
  apply (metis (mono_tags, opaque_lifting) comp_def find_None_iff option.distinct(1) senclosed_not_sseparate)
  apply (metis (mono_tags, opaque_lifting) comp_def find_None_iff option.distinct(1) senclosed_not_sseparate)
  apply (metis fst_conv ssep_symmetric senclosed_in_sseparate_region)
  apply (metis (mono_tags, opaque_lifting) comp_def find_None_iff option.distinct(1) senclosed_not_sseparate)
  apply (metis (mono_tags, lifting) comp_apply find_None_iff find_some fst_conv senclosed_transitive)
  apply (metis (mono_tags, lifting) comp_apply find_None_iff find_some fst_conv senclosed_transitive)
  apply (metis (no_types, lifting) fst_conv ssep_symmetric senclosed_in_sseparate_region)
  apply (metis (mono_tags, opaque_lifting) comp_def find_None_iff option.distinct(1) senclosed_not_sseparate)
  apply (smt (verit, best) find_some senclosed_transitive enclosing_region_is_unique comp_eq_dest_lhs fst_eqD sseparation_invariant_def snd_conv)
  apply (metis (mono_tags) comp_apply find_some fst_conv senclosed_transitive)
  done




lemma filter_overlap_join_is_empty:
  shows "filter (Not \<circ> (sseparate (a, si) \<circ> fst)) (join_mem m0 m1 regions) = [] \<longleftrightarrow> filter (Not \<circ> (sseparate (a, si))) regions = []"
  apply(induct m0 m1 regions rule: join_mem.induct)
  by (auto split: if_split_asm)

lemma mono_gamma_read_insert_region:
assumes "\<not> senclosed (a, si) (a', si')"
  shows "\<gamma>_sval \<Gamma>\<^sub>0 (sread_mem m a si) \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (((a',si'),v')#m) a si)"
  using assms
  by (auto simp add: sread_mem_def split: option.splits)

lemma read_from_inserted_enclosure_region:
  assumes "senclosed (a, si) (a', si')"
  shows "sread_mem (((a', si'), v') # m) a si = v'"
  using assms
  by (auto simp add: sread_mem_def split: option.splits)

lemma mono_gamma_read_join_L:
  assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
      and "sseparation_invariant m0"
      and "a \<noteq> \<top>"
      and "find (senclosed (a, si) \<circ> fst) (join_mem m0 m1 regions) \<noteq> None"
  shows "\<gamma>_sval \<Gamma>\<^sub>0 (sread_mem m0 a si) \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (join_mem m0 m1 regions) a si)"
  using assms
  apply(induct m0 m1 regions rule: join_mem.induct)
  apply (auto split: if_split_asm)
  apply (metis in_mono mono_gamma_read_enclosed read_from_inserted_enclosure_region join_overapproximative)
  apply (meson in_mono mono_gamma_read_insert_region)
  done

lemma mono_gamma_read_join_R:
  assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
      and "sseparation_invariant m1"
      and "a \<noteq> \<top>"
      and "find (senclosed (a, si) \<circ> fst) (join_mem m0 m1 regions) \<noteq> None"
  shows "\<gamma>_sval \<Gamma>\<^sub>0 (sread_mem m1 a si) \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (join_mem m0 m1 regions) a si)"
  using assms
  apply(induct m0 m1 regions rule: join_mem.induct)
  apply (auto split: if_split_asm)
  apply (metis in_mono mono_gamma_read_enclosed read_from_inserted_enclosure_region join_overapproximative_R)
  apply (meson in_mono mono_gamma_read_insert_region)
  done

lemma mono_gamma_read_join_expanded_L:
  assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
      and "sseparation_invariant m0"
      and "a \<noteq> \<top>"
      and "set (map fst m0) \<subseteq> set regions"
    shows "\<gamma>_sval \<Gamma>\<^sub>0 (sread_mem m0 a si) \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (join_mem m0 m1 regions) a si)"
  using assms
proof(induct m0 m1 regions rule: join_mem.induct)
  case (1 m0 m1)
  thus ?case
    by auto
next
  case (2 m0 m1 a' si' regions)
  {
    assume "sseparate (a, si) (a', si')"
       and "filter (Not \<circ> sseparate (a, si)) regions = []"
    hence "filter (Not \<circ> (sseparate (a, si) \<circ> fst)) m0 = []"
      using 2
      by (auto simp add: filter_empty_conv)
  }
  note 1 = this
  thus ?case
    using 2
    using sread_mem_append[of "[((a', si'), join (sread_mem m0 a' si') (sread_mem m1 a' si'))]"  "join_mem m0 m1 regions" a si,simplified]
    apply (cases "fst ` set m0 \<subseteq> set regions", auto simp add: 1 sread_mem_singleton filter_overlap_join_is_empty senclosed_not_sseparate split: if_split_asm)
    apply (meson subsetD mono_gamma_read_enclosed join_overapproximative)
    apply (meson subsetD mono_gamma_read_enclosed join_overapproximative)
    apply (metis subsetD mono_gamma_read_insert_region)
    apply (meson subset_iff mono_gamma_read_enclosed join_overapproximative)
    apply (meson subset_iff mono_gamma_read_enclosed join_overapproximative)
    using mono_gamma_read_join_L apply fastforce
    using mono_gamma_read_join_L apply fastforce
    apply (subst (asm) sread_mem_def[of m0 a si])
    apply (auto split: if_split_asm option.splits)
    using "1" apply blast
    by (metis "1" comp_apply empty_filter_conv find_some senclosed_not_sseparate)
qed


lemma mono_gamma_read_join_expanded_R:
  assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
      and "sseparation_invariant m1"
      and "a \<noteq> \<top>"
      and "set (map fst m1) \<subseteq> set regions"
    shows "\<gamma>_sval \<Gamma>\<^sub>0 (sread_mem m1 a si) \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (join_mem m0 m1 regions) a si)"
  using assms
proof(induct m0 m1 regions rule: join_mem.induct)
  case (1 m0 m1)
  thus ?case
    by auto
next
  case (2 m0 m1 a' si' regions)
  {
    assume "sseparate (a, si) (a', si')"
       and "filter (Not \<circ> sseparate (a, si)) regions = []"
    hence "filter (Not \<circ> (sseparate (a, si) \<circ> fst)) m1 = []"
      using 2
      by (auto simp add: filter_empty_conv)
  }
  note 1 = this
  thus ?case
    using 2
    using sread_mem_append[of "[((a', si'), join (sread_mem m0 a' si') (sread_mem m1 a' si'))]"  "join_mem m0 m1 regions" a si,simplified]
    apply (cases "fst ` set m1 \<subseteq> set regions", auto simp add: 1 sread_mem_singleton filter_overlap_join_is_empty senclosed_not_sseparate split: if_split_asm)
    apply (meson subsetD mono_gamma_read_enclosed join_overapproximative_R)
    apply (meson subset_eq mono_gamma_read_enclosed join_overapproximative_R)
    apply (metis subsetD mono_gamma_read_insert_region)
    apply (meson subset_iff mono_gamma_read_enclosed join_overapproximative_R)
    apply (meson subset_iff mono_gamma_read_enclosed join_overapproximative_R)
    using mono_gamma_read_join_R apply fastforce
    using mono_gamma_read_join_R apply fastforce
    apply (subst (asm) sread_mem_def[of m1 a si])
    apply (auto split: if_split_asm option.splits)
    using "1" apply blast
    by (metis "1" comp_apply empty_filter_conv find_some senclosed_not_sseparate)
qed


definition sjoin_mem
  where "sjoin_mem m0 m1 \<equiv> normalize (join_mem m0 m1 (map fst m0 @ map fst m1))"

lemma mono_gamma_sjoin_mem:
  assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
      and "sseparation_invariant m0"
      and "sseparation_invariant m1"
      and "a \<noteq> \<top>"
  shows "\<gamma>_sval \<Gamma>\<^sub>0 (sread_mem m0 a si) \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (sjoin_mem m0 m1) a si)"
    and "\<gamma>_sval \<Gamma>\<^sub>0 (sread_mem m1 a si) \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (sread_mem (sjoin_mem m0 m1) a si)"
  using mono_gamma_read_normalize[of \<Gamma>\<^sub>0 "join_mem m0 m1 (map fst m0 @ map fst m1)" a si]
        mono_gamma_read_join_expanded_L[OF assms(1,2,4),of "map fst m0 @ map fst m1" si m1]
        mono_gamma_read_join_expanded_R[OF assms(1,3,4),of "map fst m0 @ map fst m1" si m0] assms
  by (auto simp add: sjoin_mem_def)




definition join_sstate :: "'\<A> sstate \<Rightarrow> '\<A> sstate \<Rightarrow> '\<A> sstate"
  where "join_sstate p q \<equiv> \<lparr> sregs = \<lambda> r . Some (join (sread_reg p r) (sread_reg q r)), smem = sjoin_mem (smem p) (smem q) \<rparr>"

lemma regs_are_overapprox'd_join_L:
  shows "regs_are_overapprox'd \<Gamma>\<^sub>0 s p \<Longrightarrow> regs_are_overapprox'd \<Gamma>\<^sub>0 s \<lparr>sregs = \<lambda>r. Some (join (sread_reg p r) (sread_reg q r)), smem = m \<rparr>"
  apply (auto simp add: regs_are_overapprox'd_def sread_reg_def)
  by (meson subset_eq join_overapproximative)

lemma regs_are_overapprox'd_join_R:
  shows "regs_are_overapprox'd \<Gamma>\<^sub>0 s q \<Longrightarrow> regs_are_overapprox'd \<Gamma>\<^sub>0 s \<lparr>sregs = \<lambda>r. Some (join (sread_reg p r) (sread_reg q r)), smem = m \<rparr>"
  apply (auto simp add: regs_are_overapprox'd_def sread_reg_def)
  using join_overapproximative_R by blast

lemma sseparation_invariant_join_sstate:
assumes "sseparation_invariant (smem \<sigma>0)"
assumes "sseparation_invariant (smem \<sigma>1)"
shows "sseparation_invariant (smem (join_sstate \<sigma>0 \<sigma>1))"
  using assms 
  by (auto simp add: join_sstate_def sjoin_mem_def sseparation_invariant_normalize)










lemma mem_is_overapprox'd_join_mem:
assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
    and "sseparation_invariant (smem p)"
    and "sseparation_invariant (smem q)"
  shows "mem_is_overapprox'd \<Gamma>\<^sub>0 x p \<Longrightarrow> mem_is_overapprox'd \<Gamma>\<^sub>0 x \<lparr>sregs = r, smem = sjoin_mem (smem p) (smem q)\<rparr>"
    and "mem_is_overapprox'd \<Gamma>\<^sub>0 x q \<Longrightarrow> mem_is_overapprox'd \<Gamma>\<^sub>0 x \<lparr>sregs = r, smem = sjoin_mem (smem p) (smem q)\<rparr>"
  using mono_gamma_sjoin_mem assms
  apply (auto simp add: mem_is_overapprox'd_def)
  by blast+


lemma mono_gamma_join_sstate:
assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
    and "sseparation_invariant (smem p)"
    and "sseparation_invariant (smem q)"
shows "\<gamma> \<Gamma>\<^sub>0 p \<subseteq> \<gamma> \<Gamma>\<^sub>0 (join_sstate p q)"
  and "\<gamma> \<Gamma>\<^sub>0 q \<subseteq> \<gamma> \<Gamma>\<^sub>0 (join_sstate p q)"
  using assms
  by (auto simp add: join_sstate_def \<gamma>_def regs_are_overapprox'd_join_L regs_are_overapprox'd_join_R mem_is_overapprox'd_join_mem)
  

end




text \<open>ALGORITHM\<close>
record '\<A> invariants =
  sstates :: "(64 word \<times> '\<A> sstate) list"
  edges :: "(64 word \<times> 64 word) set"

locale algo = smemory_relations + binary + 
  fixes sstate_size :: "'a sstate \<Rightarrow> nat"
  assumes wf_measure_for_joins: "join_sstate p q \<noteq> q \<Longrightarrow> sstate_size (join_sstate p q) < sstate_size q"
begin



definition cget_rip
  where "cget_rip s \<equiv> case cread_reg s ''rip'' of CValue a \<Rightarrow> a"


inductive creachable
  where "cget_rip s \<in> set (map fst m) \<Longrightarrow> s = s\<^sub>0 \<Longrightarrow> creachable s\<^sub>0 s m"
  | "creachable s\<^sub>0 s' m \<Longrightarrow> cget_rip s'' \<in> set (map fst m) \<Longrightarrow> s'' = cstep (fetch (cget_rip s')) s' \<Longrightarrow> creachable s\<^sub>0 s'' m"

definition holds
  where "holds \<Gamma>\<^sub>0 s\<^sub>0 a \<sigma> m \<equiv> \<forall> s . creachable s\<^sub>0 s m \<and> cget_rip s = a \<longrightarrow> s \<in> \<gamma> \<Gamma>\<^sub>0 \<sigma>"



type_synonym cfg = "64 word \<Rightarrow> 64 word list" 

definition add_fst 
  where "add_fst a0 \<equiv> map (\<lambda> a1 . (a0,a1))"


function algo :: "cfg \<Rightarrow> (64 word \<times> 64 word) list \<Rightarrow> 'a invariants \<Rightarrow> 'a invariants"
  where "algo g [] m = m"
  | "algo g ((a0,a1)#bag) m = 
  (
      let p = the (mlookup (sstates m) a0);
          i = fetch a0;
          q = sstep i p;
          bag' = add_fst a1 (g a1) @ bag;
          edges' = insert (a0,a1) (edges m) in
        case mlookup (sstates m) a1 of
          None   \<Rightarrow> algo g bag' (m \<lparr>sstates := minsert a1 q (sstates m), edges := edges'\<rparr>)
        | Some q\<^sub>c \<Rightarrow> let j = join_sstate q q\<^sub>c in
                       if j = q\<^sub>c then
                          algo g bag m 
                       else
                          algo g bag' (m \<lparr>sstates := minsert a1 j (sstates m), edges := edges'\<rparr>)
  )"
  by (pat_completeness,auto)

fun num_of_unvisited_addresses :: "cfg \<times> (64 word \<times> 64 word) list \<times> 'a invariants \<Rightarrow> nat" 
  where "num_of_unvisited_addresses (g,bag,invs) = card { v . mlookup (sstates invs) v = None }" 

lemma num_of_unvisited_addresses_decreases:
assumes "mlookup (sstates m) a1 = None"
shows "num_of_unvisited_addresses (g, bag, m\<lparr>sstates := minsert a1 \<sigma> (sstates m), edges := es\<rparr>) < num_of_unvisited_addresses (g, bag', m)"
  using assms
  apply (auto simp add: mlookup_def minsert_def split: option.splits)
  apply (rule psubset_card_mono)
  apply force
  apply (auto)
  apply (metis (mono_tags) comp_eq_dest_lhs filter_id_conv find_not_None)
  by fastforce

lemma algo_termination_lemma:
  shows " \<forall>a b. find ((=) x \<circ> fst) (filter ((\<noteq>) a1 \<circ> fst) ss) \<noteq> Some (a, b) \<Longrightarrow> find ((=) x \<circ> fst) ss = Some (a, b) \<Longrightarrow> x \<noteq> a1 \<Longrightarrow> False"
  by (induct ss,auto split: if_split_asm)

lemma num_of_unvisited_addresses_not_increases:
  shows "num_of_unvisited_addresses (g, bag, m\<lparr>sstates := minsert a1 \<sigma> (sstates m), edges := es\<rparr>) \<le> num_of_unvisited_addresses (g, bag', m)"
  apply (auto simp add: mlookup_def minsert_def split: option.splits)
  apply (rule card_mono)
  apply force
  apply auto
  by (metis algo_termination_lemma)


definition invariants_size :: "'a invariants \<Rightarrow> nat"
  where "invariants_size invs \<equiv> sum_list (map (sstate_size o snd) (sstates invs))"


lemma sum_list_not_increases:
  fixes  f :: "'A \<Rightarrow> nat"
  assumes "\<forall> a \<in> set A . f a \<le> g a"
  shows "sum_list (map f A) \<le> sum_list (map g A)"
  using assms
  by (induct A,auto)

lemma sum_list_decreases_helper:
  fixes  f :: "'A \<Rightarrow> nat"
  assumes "\<forall> a \<in> set A . f a \<le> g a"
      and "f a < g a" 
      and "a \<in> set A"
  shows "sum_list (map f A) < sum_list (map g A)"
  using assms
proof(induct A)
  case Nil
  thus ?case
    by auto
next
  case (Cons a A)
  thus ?case
    using sum_list_not_increases add_mono_thms_linordered_field(3)
    apply auto
    by blast
qed



lemma sum_list_decreases:
assumes "mlookup m k = Some q"
    and "(f v::nat) < f q"
  shows "sum_list (map (f o snd) (minsert k v m)) < sum_list (map (f o snd) m)"
  using assms
  by (induct m,auto simp add: mlookup_def minsert_def add_less_le_mono sum_list_filter_le_nat split: option.splits if_split_asm)

lemma invariants_size_decreases:
  assumes "join_sstate p q \<noteq> q"
      and "mlookup (sstates m) a = Some q"
  shows "invariants_size (m\<lparr>sstates := minsert a (join_sstate p q) (sstates m), edges := es\<rparr>) < invariants_size m"
  apply (auto simp add: invariants_size_def)
  apply (rule sum_list_decreases[of _ _ q ])
  using assms wf_measure_for_joins
  by auto

termination algo
  apply (relation "measures [num_of_unvisited_addresses, invariants_size o snd o snd,length o fst o snd]")
  apply (auto simp del: num_of_unvisited_addresses.simps)
  using num_of_unvisited_addresses_decreases apply blast
  using num_of_unvisited_addresses_decreases apply blast
  using num_of_unvisited_addresses_decreases apply blast
  subgoal for g a0 a1 bag m q\<^sub>c
    using num_of_unvisited_addresses_not_increases[of g "add_fst a1 (g a1) @ bag" "insert (a0, a1) (edges m)" a1 "join_sstate (sstep (fetch a0) (the (mlookup (sstates m) a0))) q\<^sub>c" m "(a0, a1) # bag"]
    by auto
  subgoal for g a0 a1 bag m q\<^sub>c
    using num_of_unvisited_addresses_not_increases[of g "add_fst a1 (g a1) @ bag" "insert (a0, a1) (edges m)" a1 "join_sstate (sstep (fetch a0) (the (mlookup (sstates m) a0))) q\<^sub>c" m "(a0, a1) # bag"]
    by auto
  using invariants_size_decreases apply blast
  using invariants_size_decreases apply blast
  done








definition cis_initial 
  where "cis_initial \<Gamma>\<^sub>0 s \<equiv> cget_rip s = entry \<and> cmem s = [] \<and> (\<forall> r . cread_reg s r = CValue (\<Gamma>\<^sub>0 (SP_Reg r)))"

fun is_cpath
  where "is_cpath \<Gamma>\<^sub>0 [] m = False"
  | "is_cpath \<Gamma>\<^sub>0 [s] m = cis_initial \<Gamma>\<^sub>0 s"
  | "is_cpath \<Gamma>\<^sub>0 (s'#s#\<pi>) m = (s' = cstep (fetch (cget_rip s)) s \<and> (cget_rip s,cget_rip s') \<in> m \<and> is_cpath \<Gamma>\<^sub>0 (s#\<pi>) m)"

definition correct :: "'a invariants \<Rightarrow> (64 word \<times> 64 word) list \<Rightarrow> bool"
  where "correct m bag \<equiv> \<forall> \<Gamma>\<^sub>0 . \<Gamma>_sanity \<Gamma>\<^sub>0 \<longrightarrow> (\<forall> (a0,a1) \<in> edges m - set bag . \<gamma> \<Gamma>\<^sub>0 (sstep (fetch a0) (the (mlookup (sstates m) a0))) \<subseteq> \<gamma> \<Gamma>\<^sub>0 (the (mlookup (sstates m) a1)))"

definition sstateI
  where "sstateI = \<lparr> sregs = \<lambda> r . Some (mk_svalue (SE_Var (SP_Reg r))) , smem = []\<rparr>"


lemma sstateI:
assumes "cis_initial \<Gamma>\<^sub>0 s"
shows "s \<in> \<gamma> \<Gamma>\<^sub>0 sstateI"
  using assms
  by (auto simp add: \<gamma>_mk_svalue cis_initial_def cread_reg_def sstateI_def \<gamma>_def regs_are_overapprox'd_def sread_reg_def mem_is_overapprox'd_def sread_mem_def cread_segment_def split: cvalue.splits)

lemma cseparation_invariant_hd_path:
  assumes "is_cpath \<Gamma>\<^sub>0 \<pi> (edges m)"
  shows "cseparation_invariant (cmem (hd \<pi>))"
  using assms cseparation_invariant_cstep
  by (induct \<Gamma>\<^sub>0 \<pi> "edges m" rule: is_cpath.induct,auto simp add: cis_initial_def)

lemma main0:
assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
    and "correct m []"
    and "\<forall> (a,\<sigma>) \<in> set (sstates m) . sseparation_invariant (smem \<sigma>)"
    and "\<forall>(a, a')\<in>edges m. mlookup (sstates m) a \<noteq> None \<and> mlookup (sstates m) a' \<noteq> None"
    and "is_cpath \<Gamma>\<^sub>0 \<pi> (edges m)"
    and "mlookup (sstates m) entry = Some sstateI"
  shows "hd \<pi> \<in> \<gamma> \<Gamma>\<^sub>0 (the (mlookup (sstates m) (cget_rip (hd \<pi>))))"
  using assms
proof (induct \<Gamma>\<^sub>0 \<pi> "edges m" rule: is_cpath.induct)
  case 1
  thus ?case
    by auto
next
  case (2 s)
  thus ?case
    by (auto simp add: case_prod_unfold sstateI cis_initial_def)
next
  case (3 \<Gamma>\<^sub>0 s' s \<pi>)
  let ?\<sigma>0 = "the (mlookup (sstates m) (cget_rip s))"
  have \<sigma>0: "s \<in> \<gamma> \<Gamma>\<^sub>0 ?\<sigma>0"
    using 3
    by (auto simp add: case_prod_unfold mlookup_def find_None_iff split: option.splits)
  moreover
  have "sseparation_invariant (smem ?\<sigma>0)"
    using 3
    apply (cases "mlookup (sstates m) (cget_rip s)",auto simp add: mlookup_None)
    using mlookup_member 
    by fastforce
  ultimately
  have "cstep (fetch (cget_rip s)) s \<in> \<gamma> \<Gamma>\<^sub>0 (sstep (fetch (cget_rip s)) ?\<sigma>0)"
    using overapprox_step[of ?\<sigma>0 s \<Gamma>\<^sub>0  "fetch (cget_rip s)"] 3(6) cseparation_invariant_hd_path
    apply auto
    by force
  thus ?case
    using 3
    apply (auto simp add: case_prod_unfold correct_def)
    by fastforce
qed





lemma main:
  assumes "correct m ((a0, a1) # bag)"
      and "mlookup (sstates m) a1 = None"
      and "\<forall> (a, a') \<in> edges m . mlookup (sstates m) a \<noteq> None \<and> mlookup (sstates m) a' \<noteq> None"
      and "a0 \<noteq> a1"
  shows "correct (m \<lparr>sstates := minsert a1 (sstep (fetch a0) (the (mlookup (sstates m) a0))) (sstates m), edges := insert (a0,a1) (edges m)\<rparr>) (add_fst a1 (g a1) @ bag)"
proof-
  let ?m' = "(m \<lparr>sstates := minsert a1 (sstep (fetch a0) (the (mlookup (sstates m) a0))) (sstates m), edges := insert (a0,a1) (edges m)\<rparr>)"
  let ?bag' = "add_fst a1 (g a1) @ bag"

  {
    fix \<Gamma>\<^sub>0 a a'
    assume a: "(a,a') \<in> edges ?m'"
       and b: "\<Gamma>_sanity \<Gamma>\<^sub>0"
       and c: "(a,a') \<notin> set ?bag'"
    then consider "(a,a') = (a0,a1)" | "(a,a') \<noteq> (a0,a1) \<and> (a,a') \<in> edges m"
      by auto
    hence "\<gamma> \<Gamma>\<^sub>0 (sstep (fetch a) (the (mlookup (sstates ?m') a))) \<subseteq> \<gamma> \<Gamma>\<^sub>0 (the (mlookup (sstates ?m') a'))"
    proof(cases)
      case 1
      thus ?thesis
        using assms(4)
        by auto
    next
      case 2
      thus ?thesis
        using 2 b c assms(1)[unfolded correct_def,THEN spec, THEN mp,of \<Gamma>\<^sub>0, unfolded Ball_def,THEN spec,of "(a,a')"] assms(2-3)
        by auto
    qed
  }
  thus ?thesis
    apply (auto simp add: correct_def)
    by fastforce+
qed


lemma main2:
  assumes "correct m ((a0, a1) # bag)"
      and "\<forall> (a,\<sigma>) \<in> set (sstates m) . sseparation_invariant (smem \<sigma>)"
      and "\<forall>(a, a') \<in> edges m. a' \<in> set (g a)"
      and "mlookup (sstates m) a0 = Some \<sigma>0"
      and "mlookup (sstates m) a1 = Some \<sigma>1"
      and "a0 \<noteq> a1"
      and "\<forall> a . a \<notin> set (g a)" 
  shows "correct (m \<lparr>sstates := minsert a1 (join_sstate (sstep (fetch a0) \<sigma>0) \<sigma>1) (sstates m), edges := insert (a0,a1) (edges m)\<rparr>) (add_fst a1 (g a1) @ bag)"
proof-
  let ?m' = "m \<lparr>sstates := minsert a1 (join_sstate (sstep (fetch a0) \<sigma>0) \<sigma>1) (sstates m), edges := insert (a0,a1) (edges m)\<rparr>"
  let ?bag' = "add_fst a1 (g a1) @ bag"

  {
    fix \<Gamma>\<^sub>0 a a'
    assume a: "(a,a') \<in> edges ?m'"
       and b: "\<Gamma>_sanity \<Gamma>\<^sub>0"
       and c: "(a,a') \<notin> set ?bag'"
    then consider "(a,a') = (a0,a1)" | "(a,a') \<noteq> (a0,a1) \<and> (a,a') \<in> edges m"
      by auto
    hence "\<gamma> \<Gamma>\<^sub>0 (sstep (fetch a) (the (mlookup (sstates ?m') a))) \<subseteq> \<gamma> \<Gamma>\<^sub>0 (the (mlookup (sstates ?m') a'))"
    proof(cases)
      case 1
      moreover
      have "sseparation_invariant (smem \<sigma>0)"
        using 1 assms(2,4) mlookup_member[of "sstates m" a0 \<sigma>0]
        by (cases "mlookup (sstates m) a0",auto)
      moreover
      have "sseparation_invariant (smem \<sigma>1)"
        using assms(2,5) mlookup_member
        apply auto
        by fastforce
      ultimately
      show ?thesis
        using assms(4,6) b
        apply auto
        using sseparation_invariant_sstep[of \<sigma>0 "fetch a0"]
        using mono_gamma_join_sstate(1)[of \<Gamma>\<^sub>0 "sstep (fetch a0) \<sigma>0" \<sigma>1]
        by auto
    next
      case 2
      moreover
      have "sseparation_invariant (smem \<sigma>0)"
        using a assms(2,4) mlookup_member[of "sstates m" a0 \<sigma>0]
        by (cases "mlookup (sstates m) a0",auto)
      moreover
      have "sseparation_invariant (smem \<sigma>1)"
        using assms(2,5) mlookup_member
        apply auto
        by fastforce
      moreover
      have "(a, a') \<in> set (add_fst a (g a))"
        using assms(3) 2 a
        by(auto simp add: add_fst_def)
      moreover
      have "a' \<noteq> a" 
        using assms(3,7) 2
        by blast
      ultimately
      show ?thesis
        using 2 a b c assms
        using assms(1)[unfolded correct_def,THEN spec,THEN mp,unfolded Ball_def,THEN spec,of \<Gamma>\<^sub>0 "(a,a')"]
        using mono_gamma_join_sstate(2)[of \<Gamma>\<^sub>0 "sstep (fetch a0) \<sigma>0" \<sigma>1] sseparation_invariant_sstep
        apply (auto simp add: correct_def)
        by blast
    qed
  }
  thus ?thesis
    apply (auto simp add: correct_def)
    by fastforce+
qed

lemma main4:
  assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
      and "\<forall> (a,\<sigma>) \<in> set (sstates m) . sseparation_invariant (smem \<sigma>)"
      and "\<forall>(a, a') \<in> edges m. a' \<in> set (g a)"
      and "mlookup (sstates m) a0 = Some \<sigma>0"
      and "mlookup (sstates m) a1 = Some q\<^sub>c \<and> join_sstate (sstep (fetch a0) \<sigma>0) q\<^sub>c = q\<^sub>c"
      and "correct m ((a0, a1) # bag)"
    shows "correct m bag"
proof-
  {
    fix \<Gamma>\<^sub>0 a a'
    assume a: "(a,a') \<in> edges m"
       and b: "\<Gamma>_sanity \<Gamma>\<^sub>0"
       and c: "(a,a') \<notin> set bag"
    hence "\<gamma> \<Gamma>\<^sub>0 (sstep (fetch a) (the (mlookup (sstates m) a))) \<subseteq> \<gamma> \<Gamma>\<^sub>0 (the (mlookup (sstates m) a'))"
    proof(cases "(a,a') = (a0,a1)")
      case True
      moreover
      have "sseparation_invariant (smem \<sigma>0)"
        using a assms(2,4) mlookup_member[of "sstates m" a0 \<sigma>0]
        by (cases "mlookup (sstates m) a0",auto)
      moreover
      have "sseparation_invariant (smem q\<^sub>c)"
        using assms(2,5) mlookup_member
        apply auto
        by fastforce
      ultimately
      show ?thesis
        using assms(4,5) b
        using mono_gamma_join_sstate(1)[of \<Gamma>\<^sub>0 "sstep (fetch a0) \<sigma>0" q\<^sub>c] sseparation_invariant_sstep
        by (auto simp add: assms(2))
    next
      case False
      thus ?thesis
        using assms a b c
        apply (auto simp add: correct_def)
        by blast+
    qed
  }
  thus ?thesis
    by (auto simp add: correct_def)
qed

lemma main3:
  assumes "\<Gamma>_sanity \<Gamma>\<^sub>0"
      and "correct m bag"
      and "\<forall>(a, \<sigma>) \<in> set (sstates m). sseparation_invariant (smem \<sigma>)"
      and "\<forall> (a, a') \<in> edges m. a' \<in> set (g a)"
      and "\<forall> (a, a') \<in> set bag . mlookup (sstates m) a \<noteq> None"
      and "\<forall> (a, a') \<in> set bag . a' \<in> set (g a)"
      and "\<forall> (a, a') \<in> edges m. mlookup (sstates m) a \<noteq> None \<and> mlookup (sstates m) a' \<noteq> None"
      and "\<forall> a . a \<notin> set (g a)" 
    shows "correct (algo g bag m) []"
  using assms
proof(induct g bag m rule: algo.induct)
  case (1 g m)
  thus ?case
    by auto
next
  case (2 g a0 a1 bag m)
  note IH = this
  let ?p = "the (mlookup (sstates m) a0)"
  let ?q = "sstep (fetch a0) ?p"
  consider "mlookup (sstates m) a1 = None" | "\<exists> q\<^sub>c . mlookup (sstates m) a1 = Some q\<^sub>c \<and> join_sstate ?q q\<^sub>c = q\<^sub>c" | "\<exists> q\<^sub>c . mlookup (sstates m) a1 = Some q\<^sub>c \<and> join_sstate ?q q\<^sub>c \<noteq> q\<^sub>c"
    by auto
  thus ?case 
  proof(cases)
    case 1
    have 3: " \<forall>a\<in>set (sstates (m\<lparr>sstates := minsert a1 (sstep (fetch a0) (the (mlookup (sstates m) a0))) (sstates m), edges := insert (a0, a1) (edges m)\<rparr>)). case a of (a, \<sigma>) \<Rightarrow> sseparation_invariant (smem \<sigma>)"
      using IH(4-)
      apply (auto split: if_split_asm)
      using mlookup_member sseparation_invariant_sstep 
      by fastforce+
    have 4: "\<forall>a\<in>edges (m\<lparr>sstates := minsert a1 (sstep (fetch a0) (the (mlookup (sstates m) a0))) (sstates m), edges := insert (a0, a1) (edges m)\<rparr>). case a of (a, a') \<Rightarrow> a' \<in> set (g a)"
      using IH(4-)
      by (auto split: if_split_asm)
    have 5: "\<forall>a\<in>set (add_fst a1 (g a1) @ bag). case a of (a, a') \<Rightarrow> mlookup (sstates (m\<lparr>sstates := minsert a1 (sstep (fetch a0) (the (mlookup (sstates m) a0))) (sstates m), edges := insert (a0, a1) (edges m)\<rparr>)) a \<noteq> None"
      using IH(4-)
      by (auto split: if_split_asm simp add: add_fst_def)
    have 6: "\<forall>a\<in>set (add_fst a1 (g a1) @ bag). case a of (a, a') \<Rightarrow> a' \<in> set (g a)"
      using IH(4-)
      by (auto split: if_split_asm simp add: add_fst_def)
    have 7: "\<forall>a\<in>edges (m\<lparr>sstates := minsert a1 (sstep (fetch a0) (the (mlookup (sstates m) a0))) (sstates m), edges := insert (a0, a1) (edges m)\<rparr>).
       case a of
       (a, a') \<Rightarrow>
         mlookup (sstates (m\<lparr>sstates := minsert a1 (sstep (fetch a0) (the (mlookup (sstates m) a0))) (sstates m), edges := insert (a0, a1) (edges m)\<rparr>)) a \<noteq> None \<and>
         mlookup (sstates (m\<lparr>sstates := minsert a1 (sstep (fetch a0) (the (mlookup (sstates m) a0))) (sstates m), edges := insert (a0, a1) (edges m)\<rparr>)) a' \<noteq> None "
      using IH(4-)
      by (auto split: if_split_asm simp add: add_fst_def)
    have 8: "\<forall>x\<in>edges m. case x of (a, a') \<Rightarrow> (\<exists>y. mlookup (sstates m) a = Some y) \<and> (\<exists>y. mlookup (sstates m) a' = Some y)"
      using IH(4-)
      by (auto split: if_split_asm simp add: add_fst_def)
    have 9: "a0 \<noteq> a1"
      using 1 IH(8) 
      by force

    show ?thesis
      using 1 IH(1)[of ?p "fetch a0" ?q] 3 4 5 6 7 IH(4,11)
      using main[OF IH(5),of g,simplified,OF 1 8 9]
      apply (auto simp add: mlookup_None Let_def split: option.splits if_split_asm)
      apply (metis "1" not_Some_eq)
      by (metis "1" not_None_eq)
  next
    case 2
    show ?thesis
      apply (subst algo.simps)
      using 2 IH(2,4-) 
      using main4[of \<Gamma>\<^sub>0 m g a0]
      by auto
  next
    case 3
    let ?\<sigma>0 = "the (mlookup (sstates m) a0)"
    obtain q\<^sub>c where q\<^sub>c: "mlookup (sstates m) a1 = Some q\<^sub>c \<and> join_sstate (sstep (fetch a0) ?\<sigma>0) q\<^sub>c \<noteq> q\<^sub>c"
      using 3
      by auto

    have 4: "\<forall>a\<in>set (sstates (m\<lparr>sstates := minsert a1 (join_sstate (sstep (fetch a0) (the (mlookup (sstates m) a0))) q\<^sub>c) (sstates m), edges := insert (a0, a1) (edges m)\<rparr>)). case a of (a, \<sigma>) \<Rightarrow> sseparation_invariant (smem \<sigma>)"
      using IH(4-)
      apply (auto split: if_split_asm)
      apply (metis (mono_tags, lifting) case_prod_conv mlookup_member q\<^sub>c sseparation_invariant_join_sstate sseparation_invariant_sstep)
      by fastforce

    have 5: "\<forall>a\<in>edges (m\<lparr>sstates := minsert a1 (join_sstate (sstep (fetch a0) (the (mlookup (sstates m) a0))) q\<^sub>c) (sstates m), edges := insert (a0, a1) (edges m)\<rparr>). case a of (a, a') \<Rightarrow> a' \<in> set (g a)"
      using IH(4-)
      by (auto split: if_split_asm)
    have 6: "\<forall>a\<in>set (add_fst a1 (g a1) @ bag). case a of (a, a') \<Rightarrow> mlookup (sstates (m\<lparr>sstates := minsert a1 (join_sstate (sstep (fetch a0) (the (mlookup (sstates m) a0))) q\<^sub>c) (sstates m), edges := insert (a0, a1) (edges m)\<rparr>)) a \<noteq> None"
      using IH(4-)
      by (auto split: if_split_asm simp add: add_fst_def)
    have 7: "\<forall>a\<in>set (add_fst a1 (g a1) @ bag). case a of (a, a') \<Rightarrow> a' \<in> set (g a)"
      using IH(4-)
      by (auto split: if_split_asm simp add: add_fst_def)
    have 8: "\<forall>a\<in>edges (m\<lparr>sstates := minsert a1 (join_sstate (sstep (fetch a0) (the (mlookup (sstates m) a0))) q\<^sub>c) (sstates m), edges := insert (a0, a1) (edges m)\<rparr>).
       case a of
       (a, a') \<Rightarrow>
         mlookup (sstates (m\<lparr>sstates := minsert a1 (join_sstate (sstep (fetch a0) (the (mlookup (sstates m) a0))) q\<^sub>c) (sstates m), edges := insert (a0, a1) (edges m)\<rparr>)) a \<noteq> None \<and>
         mlookup (sstates (m\<lparr>sstates := minsert a1 (join_sstate (sstep (fetch a0) (the (mlookup (sstates m) a0))) q\<^sub>c) (sstates m), edges := insert (a0, a1) (edges m)\<rparr>)) a' \<noteq> None"
      using IH(4-)
      by (auto split: if_split_asm simp add: add_fst_def)
    have 9: "a0 \<noteq> a1"
      using IH(9,11) 
      by force

    
    show ?thesis
      using IH(4,5,6,7,8,11) 3 9 q\<^sub>c 4 5 6 7 8
      using main2[of m a0 a1 bag g "the (mlookup (sstates m) a0)" q\<^sub>c ]
      using IH(3)[of "the (mlookup (sstates m) a0)" "fetch a0" "sstep (fetch a0) (the (mlookup (sstates m) a0))" ]
      by auto
  qed
qed


lemma mainI:
  shows "correct \<lparr> sstates = [(entry, sstateI)], edges = empty \<rparr> []"
  by (auto simp add: correct_def)

lemma sseparation_invariantI:
  shows "sseparation_invariant (smem sstateI)"
  by (auto simp add: sseparation_invariant_def sstateI_def)



lemma main5:
assumes "\<Gamma>_sanity \<Gamma>\<^sub>0" \<comment> \<open>The initial valuation is internally consistent.\<close>
    and "\<forall> a . a \<notin> set (g a)" \<comment> \<open>No self-loops\<close>
    and "result = algo g [] \<lparr> sstates = [(entry, sstateI)], edges = empty \<rparr>" \<comment> \<open>The result of running the algorithm on the entry-point with an empty initial state.\<close>
  shows "is_cpath \<Gamma>\<^sub>0 \<pi> (edges result) \<Longrightarrow> hd \<pi> \<in> \<gamma> \<Gamma>\<^sub>0 (the (mlookup (sstates result) (cget_rip (hd \<pi>))))" \<comment> \<open>Any concrete state reachable through explored edges is overapproximated by the abstract state\<close>
  apply (subst assms(3))
  apply (rule main0[OF assms(1)])
  apply (rule main3[OF assms(1)])
  apply (rule mainI)
  using assms(2-)
  apply (auto simp add: mlookup_def sseparation_invariantI)
  done

end

end




