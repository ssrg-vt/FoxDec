theory X86_Base
  imports Main "HOL-Library.Word" "HOL-Library.MultiSet"
begin



definition mlookup :: "('a \<times> 'b) list \<Rightarrow> 'a \<Rightarrow> 'b option"
  where "mlookup m k \<equiv> case find ((=) k o fst) m of None \<Rightarrow> None | Some (k,v) \<Rightarrow> Some v"

definition minsert :: "'a \<Rightarrow> 'b \<Rightarrow> ('a \<times> 'b) list \<Rightarrow> ('a \<times> 'b) list"
  where "minsert k v m \<equiv> (k,v)#(filter ((\<noteq>) k o fst) m)"

lemma mlookup_member:
  assumes "mlookup m a = Some b"
  shows "(a,b) \<in> set m"
  using assms
  apply (induct m,auto simp add: mlookup_def)
  apply presburger
  by (metis (mono_tags, lifting) case_prod_conv option.inject option.simps(5))

lemma mlookup_None:
  shows "mlookup m a = None \<longleftrightarrow> a \<notin> set (map fst m)"
  by (induct m,auto simp add: mlookup_def)

lemma lookup_insert[simp]:
  shows "mlookup (minsert k v m) k' = (if k=k' then Some v else mlookup m k')"
  by (induct m,auto simp add: minsert_def mlookup_def)


lemma member_map[simp]:
  shows "(k,v) \<in> set (minsert k' v' m) \<longleftrightarrow> (if k=k' then v=v' else (k,v) \<in> set m)"
  by (induct m,auto simp add: minsert_def mlookup_def)


lemma find_not_None:
  shows "a \<in> set x \<Longrightarrow> P a \<Longrightarrow> find P x \<noteq> None"
  by (meson find_None_iff)

lemma find_some:
  shows "find P x = Some a \<Longrightarrow> P a \<and> a \<in> set x"
  by (metis find_Some_iff nth_mem)

lemma find_filter_None:
assumes "find P x = None"
  shows "find P (filter P' x) = None"
  using assms
  by (induct x,auto)

lemma find_removeAll:
assumes "find P x = None"
  shows "find P (removeAll a x) = None"
  using assms
  by (induct x,auto)

lemma find_remove_All_Some:
assumes "find P (removeAll a x) = Some b"
  shows "find P x \<noteq> None"
  using assms
  by (induct x,auto split: if_split_asm)

lemma find_removeAll_unique:
  assumes "find P (removeAll b x) = None"
      and "find P x = Some a"
    shows "a = b"
  using assms
  by (induct x,auto split: if_split_asm)



(*
From List_Permutation.thy, but don't know where that went in 2021-1.
*)

abbreviation (input) perm :: \<open>'a list \<Rightarrow> 'a list \<Rightarrow> bool\<close>  (infixr \<open><~~>\<close> 50)
  where \<open>xs <~~> ys \<equiv> mset xs = mset ys\<close>

proposition perm_append_swap: "xs @ ys <~~> ys @ xs"
  by simp





end