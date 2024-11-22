theory X86_Symbolic_Generic_Semantics
  imports X86_Concrete_Semantics
begin


text \<open>We define an abstract semantics that is generic wrt. the abstract domain.\<close>

text \<open>First, symbolic expressions.\<close>
datatype SE_Expr = SE_Plus SE_Expr SE_Expr | SE_Minus SE_Expr SE_Expr | SE_Times SE_Expr SE_Expr | 
                   SE_GenericOp string "64 word \<Rightarrow> 64 word \<Rightarrow> 64 word" SE_Expr SE_Expr |
                   SE_Malloc "64 word" |  SE_Immediate "64 word" | SE_Var StatePart | SE_Bot
    and StatePart = SP_Reg string | SP_Mem SE_Expr SE_Expr


text \<open>Second, abstract values.\<close>
datatype '\<A> svalue = STop ("\<top>") | SValue '\<A>

text \<open>Third, abstract memory.\<close>
type_synonym '\<A> smem = "(('\<A> svalue \<times> '\<A> svalue) \<times> '\<A> svalue) list"

text \<open>Fourth, abstract state.\<close>
record '\<A> sstate =
  sregs :: "string \<Rightarrow> '\<A> svalue option"
  smem ::  "'\<A> smem"





text \<open>Evaluation is relative to an initialization, over which will be universally quantified.\<close>
primrec eval :: "(StatePart \<Rightarrow> 64 word) \<Rightarrow> SE_Expr \<Rightarrow> 64 word \<Rightarrow> bool"
  where
    "eval \<Gamma>\<^sub>0 SE_Bot v = True"
  | "eval \<Gamma>\<^sub>0 (SE_Immediate imm) v = (v=imm)"
  | "eval \<Gamma>\<^sub>0 (SE_Malloc imm) v = (v=imm)"
  | "eval \<Gamma>\<^sub>0 (SE_Var sp) v = (v=\<Gamma>\<^sub>0 sp)"
  | "eval \<Gamma>\<^sub>0 (SE_Plus e0 e1) v = (\<exists> v0 v1 . v = v0+v1 \<and> eval \<Gamma>\<^sub>0 e0 v0 \<and> eval \<Gamma>\<^sub>0 e1 v1)"
  | "eval \<Gamma>\<^sub>0 (SE_Minus e0 e1) v = (\<exists> v0 v1 . v = v0-v1 \<and> eval \<Gamma>\<^sub>0 e0 v0 \<and> eval \<Gamma>\<^sub>0 e1 v1)"
  | "eval \<Gamma>\<^sub>0 (SE_Times e0 e1) v = (\<exists> v0 v1 . v = v0*v1 \<and> eval \<Gamma>\<^sub>0 e0 v0 \<and> eval \<Gamma>\<^sub>0 e1 v1)"
  | "eval \<Gamma>\<^sub>0 ((SE_GenericOp m f) e0 e1) v = (\<exists> v0 v1 . v = f v0 v1 \<and> eval \<Gamma>\<^sub>0 e0 v0 \<and> eval \<Gamma>\<^sub>0 e1 v1)"




lemma exists_valuation:
  shows "\<exists> v . eval \<Gamma>\<^sub>0 e v"
  by (induct e,auto)

definition symb_concrete_match
  where "symb_concrete_match \<equiv> {((+\<^sub>c),Add),((-\<^sub>c),Sub),((*\<^sub>c),Mul)} \<union> { x . \<exists> m f . x = (cgenop f, GenericOp m f)}"

definition eval_eq
  where "eval_eq \<Gamma>\<^sub>0 e e' \<equiv> \<forall> w . eval \<Gamma>\<^sub>0 e w \<longleftrightarrow> eval \<Gamma>\<^sub>0 e' w"

definition \<Gamma>_sanity
  where "\<Gamma>_sanity \<Gamma>\<^sub>0 \<equiv> \<forall> a si a' si' . eval_eq \<Gamma>\<^sub>0 a a' \<and> eval_eq \<Gamma>\<^sub>0 si si' \<longrightarrow> \<Gamma>\<^sub>0 (SP_Mem a si) = \<Gamma>\<^sub>0 (SP_Mem a' si')"

text \<open>This locale defines proof obligations over generic constituents. The algorithm is defined within this locale.\<close>
locale smemory_relations =
  fixes sseparate  :: "'\<A> svalue \<times> '\<A> svalue \<Rightarrow> '\<A> svalue \<times> '\<A> svalue \<Rightarrow> bool" 
    and senclosed  :: "'\<A> svalue \<times> '\<A> svalue \<Rightarrow> '\<A> svalue \<times> '\<A> svalue \<Rightarrow> bool" 
    and sequal     :: "'\<A> svalue \<times> '\<A> svalue \<Rightarrow> '\<A> svalue \<times> '\<A> svalue \<Rightarrow> bool" 
    and \<gamma>_sval     :: "(StatePart \<Rightarrow> 64 word) \<Rightarrow> '\<A> svalue \<Rightarrow> cvalue set"
    and join_\<A>     :: "'\<A> \<Rightarrow> '\<A> \<Rightarrow> '\<A>"
    and mk_init_mem_svalue :: "'\<A> svalue \<Rightarrow> '\<A> svalue \<Rightarrow> '\<A> svalue"
    and mk_svalue :: "SE_Expr \<Rightarrow> '\<A> svalue"
    and ssemantics :: "operation \<Rightarrow> '\<A> svalue \<Rightarrow> '\<A> svalue \<Rightarrow> '\<A> svalue"
 assumes sseparate: "sseparate (ptr0', si0') (ptr1', si1') \<Longrightarrow> \<forall> \<Gamma>\<^sub>0 . \<forall> ptr0 \<in> \<gamma>_sval \<Gamma>\<^sub>0 ptr0' . \<forall> si0 \<in> \<gamma>_sval \<Gamma>\<^sub>0 si0' . \<forall> ptr1 \<in> \<gamma>_sval \<Gamma>\<^sub>0 ptr1' . \<forall> si1 \<in> \<gamma>_sval \<Gamma>\<^sub>0 si1' . cval_separate (ptr0,si0) (ptr1,si1)"
     and senclosed: "senclosed (ptr0', si0') (ptr1', si1') \<Longrightarrow> \<forall> \<Gamma>\<^sub>0 . \<forall> ptr0 \<in> \<gamma>_sval \<Gamma>\<^sub>0 ptr0' . \<forall> si0 \<in> \<gamma>_sval \<Gamma>\<^sub>0 si0' . \<exists> ptr1 \<in> \<gamma>_sval \<Gamma>\<^sub>0 ptr1' . \<exists> si1 \<in> \<gamma>_sval \<Gamma>\<^sub>0 si1' . (ptr0,si0) = (ptr1,si1)"
     and sequal:    "sequal (ptr0', si0') (ptr1', si1') \<Longrightarrow> \<forall> \<Gamma>\<^sub>0 . \<forall> ptr0 \<in> \<gamma>_sval \<Gamma>\<^sub>0 ptr0' . \<forall> si0 \<in> \<gamma>_sval \<Gamma>\<^sub>0 si0' . \<forall> ptr1 \<in> \<gamma>_sval \<Gamma>\<^sub>0 ptr1' . \<forall> si1 \<in> \<gamma>_sval \<Gamma>\<^sub>0 si1' . (ptr0,si0) = (ptr1,si1)"
     and sequalI:   "senclosed r0 r1 \<Longrightarrow> sequal r1 r2 \<Longrightarrow> senclosed r2 r1 \<Longrightarrow> sequal r0 r2"
     and sequal_symmetric[symmetric]: "sequal r0 r1 \<longleftrightarrow> sequal r1 r0"
     and sequal_implies_senclosed: "sequal r0 r1 \<Longrightarrow> senclosed r0 r1"
     and senclosed_refl: "senclosed r r"
     and senclosed_BP_Top: "senclosed r (a, si) \<Longrightarrow> senclosed r (\<top>, si)"
     and senclosed_BP_Top_size: "senclosed r (a, si) \<Longrightarrow> senclosed r (a, \<top>)"
     and senclosed_transitive: "senclosed r0 r1 \<Longrightarrow> senclosed r1 r2 \<Longrightarrow> senclosed r0 r2"
     and senclosed_in_sseparate_region: "senclosed r0 r1 \<Longrightarrow> sseparate r2 r1 \<Longrightarrow> sseparate r2 r0"
     and ssep_symmetric[symmetric]: "sseparate r0 r1 \<longleftrightarrow> sseparate r1 r0"

     and \<gamma>_top[simp]: "\<gamma>_sval \<Gamma>\<^sub>0 \<top> = UNIV"
     and \<gamma>_non_empty: "\<gamma>_sval \<Gamma>\<^sub>0 a \<noteq> {}"
     and \<gamma>_mk_svalue: "eval \<Gamma>\<^sub>0 e v \<Longrightarrow> CValue v \<in> \<gamma>_sval \<Gamma>\<^sub>0 (mk_svalue e)"
     and \<gamma>_mk_init_mem_svalue: "\<Gamma>_sanity \<Gamma>\<^sub>0 \<Longrightarrow> senclosed (a, si) (a', si') \<Longrightarrow> \<gamma>_sval \<Gamma>\<^sub>0 (mk_init_mem_svalue a si) \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (mk_init_mem_svalue a' si')"

     and join_\<A>_assoc: "join_\<A> (join_\<A> x y) z = join_\<A> x (join_\<A> y z)"
     and join_\<A>_symmetric: "join_\<A> x y = join_\<A> y x"
     and join_\<A>_overapproximative: "\<gamma>_sval \<Gamma>\<^sub>0 (SValue v0) \<subseteq> \<gamma>_sval \<Gamma>\<^sub>0 (SValue (join_\<A> v0 v1))"
     and join_\<A>_ssep: "sseparate r (SValue (join_\<A> x y), si) \<Longrightarrow> sseparate r (SValue x, si)"
     and join_\<A>_ssep_si: "sseparate r (a,SValue (join_\<A> x y)) \<Longrightarrow> sseparate r (a,SValue x)"
     and join_\<A>_senc: "senclosed (SValue x, si) (SValue (join_\<A> x y), si)"
     and join_\<A>_senc_si: "senclosed (a, SValue x) (a,SValue (join_\<A> x y))"

     and ssemantics_overapproximative: "v\<^sub>0 \<in> \<gamma>_sval \<Gamma>\<^sub>0 v\<^sub>0' \<union> { Tainted } \<Longrightarrow> v\<^sub>1 \<in> \<gamma>_sval \<Gamma>\<^sub>0 v\<^sub>1' \<union> { Tainted } \<Longrightarrow> (cop, sop) \<in> symb_concrete_match \<Longrightarrow> cop v\<^sub>0 v\<^sub>1 \<in> \<gamma>_sval \<Gamma>\<^sub>0 (ssemantics sop v\<^sub>0' v\<^sub>1') \<union> { Tainted }"
begin

fun join
  where 
    "join (SValue a) (SValue b) = SValue (join_\<A> a b)" 
  | "join _ _ = \<top>"

definition supremum
  where "supremum ptrs \<equiv> fold join (tl ptrs) (hd ptrs)"




fun sop_semantics :: "operation \<Rightarrow> '\<A> svalue list \<Rightarrow> '\<A> svalue"
  where 
    "sop_semantics Mov [v] = v"
  | "sop_semantics Mov _ = \<top>"
  | "sop_semantics m [a,b] = ssemantics m a b"
  | "sop_semantics _ _ = \<top>"

abbreviation splus:: "'\<A> svalue \<Rightarrow> '\<A> svalue \<Rightarrow> '\<A> svalue" (infixl "+\<^sub>s" 65)
  where "v0 +\<^sub>s v1 \<equiv> sop_semantics Add [v0,v1]" 

abbreviation sminus :: "'\<A> svalue \<Rightarrow> '\<A> svalue \<Rightarrow> '\<A> svalue" (infixl "-\<^sub>s" 65)
  where "v0 -\<^sub>s v1 \<equiv> sop_semantics Sub [v0,v1]" 

abbreviation stimes:: "'\<A> svalue \<Rightarrow> '\<A> svalue \<Rightarrow> '\<A> svalue" (infixl "*\<^sub>s" 65)
  where "v0 *\<^sub>s v1 \<equiv> sop_semantics Mul [v0,v1]" 



text \<open>Registers\<close>
definition sread_reg
  where "sread_reg s r \<equiv> 
          case sregs s r of
            None   \<Rightarrow> mk_svalue (SE_Var (SP_Reg r))
          | Some v \<Rightarrow> v"

definition swrite_reg :: "string \<Rightarrow> '\<A> svalue \<Rightarrow> '\<A> sstate \<Rightarrow> '\<A> sstate"
  where "swrite_reg r v s \<equiv> s\<lparr>sregs := (sregs s)(r := Some v) \<rparr>"

definition smodify_reg :: "string \<Rightarrow> ('\<A> svalue \<Rightarrow> '\<A> svalue) \<Rightarrow> '\<A> sstate \<Rightarrow> '\<A> sstate"
  where "smodify_reg r f s \<equiv> s\<lparr> sregs := (sregs s)(r := Some (f (sread_reg s r))) \<rparr>"


text \<open>Memory\<close>
definition sread_mem :: "'\<A> smem \<Rightarrow> '\<A> svalue \<Rightarrow> '\<A> svalue \<Rightarrow> '\<A> svalue"
  where 
    "sread_mem m a si \<equiv> 
      case find (senclosed (a,si) o fst) m of
        Some (r,v) \<Rightarrow> v
      | _ \<Rightarrow> let (separate,overlap) = partition (sseparate (a,si) o fst) m in
                if overlap = [] \<and> a \<noteq> \<top> then mk_init_mem_svalue a si else \<top>"




definition supremum_regions :: "('\<A> svalue \<times> '\<A> svalue) list \<Rightarrow> '\<A> svalue \<times> '\<A> svalue"
  where "supremum_regions regions \<equiv>
          let a  = supremum (map fst regions);
              si = supremum (map snd regions) in
            (a,si)"

definition swrite_overlap
  where "swrite_overlap ptr0 si0 v overlap \<equiv> 
    let r' = supremum_regions ((ptr0,si0)#map fst overlap);
        v' = supremum (v#map snd overlap) in
      (r',\<top>)"

definition is_deterministic_spointer
  where "is_deterministic_spointer a si \<equiv> (\<forall> a' si' . senclosed (a', si') (a, si) \<longrightarrow> sequal (a', si') (a, si))"


fun swrite_mem1 :: "'\<A> svalue \<Rightarrow> '\<A> svalue \<Rightarrow> '\<A> svalue \<Rightarrow> '\<A> smem \<Rightarrow> '\<A> smem"
  where "swrite_mem1 a si v m =
      (case find (sequal (a,si) o fst) m of
         Some ((a',si'),v') \<Rightarrow> ((a,si),v)#(removeAll ((a',si'),v') m)
       | _ \<Rightarrow> case find (senclosed (a,si) o fst) m of
        Some ((a',si'),v') \<Rightarrow> ((a',si'),join v v')#(removeAll ((a',si'),v') m)
      | _ \<Rightarrow> let (separate,overlap) = partition (sseparate (a,si) o fst) m in
                if overlap = [] \<and> is_deterministic_spointer a si then ((a,si),v)#separate else (swrite_overlap a si v overlap)#separate)"

function normalize1
  where "normalize1 [] = []"
  | "normalize1 (((a0,si0),v0)#m0) = 
      (let (separate, overlap) = partition (sseparate (a0, si0) o fst) m0 in
        if overlap = [] then 
          ((a0,si0),v0) # normalize1 m0
        else
          normalize1 (swrite_overlap a0 si0 v0 overlap # separate)
      )"
  by (pat_completeness,auto)

definition sseparation_invariant
  where "sseparation_invariant m \<equiv> \<forall>(r0, v0)\<in>set m. \<forall>(r1, v1)\<in>set m. (r0, v0) = (r1, v1) \<or> sseparate r0 r1"

text \<open>Normalization ensures that all abstract regions are separate. Termination of this function will be proven later on.\<close>
function normalize
  where "normalize m = (if sseparation_invariant m then m else normalize (normalize1 m))"
  by (pat_completeness,auto)

definition swrite_mem :: "'\<A> svalue \<Rightarrow> '\<A> svalue \<Rightarrow> '\<A> svalue \<Rightarrow> '\<A> smem \<Rightarrow> '\<A> smem"
  where "swrite_mem a si v m \<equiv> normalize (swrite_mem1 a si v m)"

definition simmediate
  where "simmediate i \<equiv> mk_svalue (SE_Immediate i)"

definition sresolve_address :: "'\<A> sstate \<Rightarrow> string \<Rightarrow> 64 word \<Rightarrow> '\<A> svalue"
  where "sresolve_address s base offset \<equiv> sread_reg s base +\<^sub>s simmediate offset"




text \<open>
The static information assumed to be available from the binary.
\<close>
definition unescapable :: "(64 word \<Rightarrow> bool) \<Rightarrow> bool"
  where "unescapable within_segment \<equiv> \<forall> a b . (within_segment a \<or> within_segment b \<longrightarrow> within_segment (a+b)) \<and> (within_segment a \<longrightarrow> within_segment (a-b))"

primrec sread_operand :: "'\<A> sstate \<Rightarrow> src_operand \<Rightarrow> '\<A> svalue"
  where
    "sread_operand s (FromImmediate imm) = simmediate imm" 
  | "sread_operand s (FromReg r) = sread_reg s r"
  | "sread_operand s (FromMemory base offset si) = 
        (let a = sresolve_address s base offset in
           if a = \<top> then \<top> else sread_mem (smem s) a (simmediate (Rep_pos si)))"

definition sthrash_memory :: "'\<A> sstate \<Rightarrow> '\<A> sstate"
  where "sthrash_memory s \<equiv> s \<lparr> smem := undefined\<rparr>"

primrec swrite_operand :: "dst_operand \<Rightarrow> '\<A> svalue \<Rightarrow> '\<A> sstate \<Rightarrow> '\<A> sstate"
  where "swrite_operand (ToReg r) v s = swrite_reg r v s"
  | "swrite_operand (ToMemory base offset si) v s = s\<lparr> smem := swrite_mem (sresolve_address s base offset) (simmediate (Rep_pos si)) v (smem s) \<rparr>"


definition sexec_instr :: "instruction \<Rightarrow> '\<A> sstate \<Rightarrow> '\<A> sstate"
  where "sexec_instr i s \<equiv>
          let srcs_values = map (sread_operand s) (i_srcs i);
              v = sop_semantics (i_op i) srcs_values in
            swrite_operand (i_dest i) v s"

definition sset_rip :: "instruction \<Rightarrow> '\<A> sstate \<Rightarrow> '\<A> sstate"
  where "sset_rip i \<equiv> smodify_reg ''rip'' ((+\<^sub>s) (simmediate (i_size i)))"

definition sstep :: "instruction \<Rightarrow> '\<A> sstate \<Rightarrow> '\<A> sstate"
  where "sstep i \<equiv> sexec_instr i o sset_rip i"






lemma no_separation_BP_Top1:
  shows "\<not>sseparate (\<top>,si) (a',si')" 
proof-
  {
    fix \<Gamma>\<^sub>0 
    assume "\<gamma>_sval \<Gamma>\<^sub>0 si \<noteq> {} \<and> \<gamma>_sval \<Gamma>\<^sub>0 a' \<noteq> {} \<and> \<gamma>_sval \<Gamma>\<^sub>0 si' \<noteq> {}"
    then obtain si0 ptr1 si1 where "si0\<in>\<gamma>_sval \<Gamma>\<^sub>0 si \<and> ptr1\<in>\<gamma>_sval \<Gamma>\<^sub>0 a' \<and> si1\<in>\<gamma>_sval \<Gamma>\<^sub>0 si'"
      by auto
    moreover
    have "\<not>cval_separate (ptr1, si0) (ptr1, si1)"
      by (cases ptr1;cases si0;cases si1;auto simp add: cseparate.simps)
    ultimately
    have "\<exists> ptr0 . \<exists>si0\<in>\<gamma>_sval \<Gamma>\<^sub>0 si. \<exists>ptr1\<in>\<gamma>_sval \<Gamma>\<^sub>0 a'. \<exists>si1\<in>\<gamma>_sval \<Gamma>\<^sub>0 si'. \<not>cval_separate (ptr0, si0) (ptr1, si1)"
      by auto
  }
  thus ?thesis
    using sseparate[of \<top> si a' si'] 
    by (auto simp add: \<gamma>_non_empty \<gamma>_top)
qed

lemma no_separation_BP_Top2:
  shows "\<not>sseparate (a,\<top>) (a',si')" 
  using sseparate[of a \<top> a' si'] 
  apply auto
  by (metis \<gamma>_non_empty \<gamma>_top cval_separate.simps(2) ex_in_conv iso_tuple_UNIV_I)

lemma no_separation_BP_Top:
  shows "\<not>sseparate (\<top>,si) (a',si')" and "\<not>sseparate (a,si) (\<top>,si')" and "\<not>sseparate (a,\<top>) (a',si')" and "\<not>sseparate (a,si) (a',\<top>)"
  using no_separation_BP_Top1 no_separation_BP_Top2 ssep_symmetric
  by blast+

lemma senclosed_BP_Top_L:
assumes "senclosed (\<top>,si) (a', si')"
  shows "\<gamma>_sval \<Gamma>\<^sub>0 a' = UNIV"
  using senclosed[OF assms(1)] 
  apply (cases si;cases a';auto simp add: \<gamma>_top)
  using \<gamma>_non_empty by blast

lemma senclosed_BP_Top_R:
assumes "senclosed (a, \<top>) (a', si')"
  shows "\<gamma>_sval \<Gamma>\<^sub>0 si' = UNIV"
  using senclosed[OF assms(1)] 
  apply (cases a;cases si';auto simp add: \<gamma>_top)
  using \<gamma>_non_empty by blast




lemma cval_separate_asymmetric:
  shows "\<not>cval_separate r r"
  apply (cases r,auto)
  by (metis cval_separate.elims(2) cvalue.inject old.prod.inject sep_irreflexive)

lemma sseparate_not_enclosed:
assumes "sseparate r0 r1"
shows "\<not> senclosed r0 r1"
proof-
  let ?\<Gamma>\<^sub>0 = "SOME \<Gamma>\<^sub>0 . True"
  {
    fix ptr0' si0' ptr1' si1'
    have "r0 = (ptr0', si0') \<Longrightarrow> r1 = (ptr1', si1') \<Longrightarrow> (\<exists>ptr0\<in>\<gamma>_sval ?\<Gamma>\<^sub>0 ptr0'. \<exists>si0\<in>\<gamma>_sval ?\<Gamma>\<^sub>0 si0'. ptr0 \<in> \<gamma>_sval ?\<Gamma>\<^sub>0 ptr1' \<longrightarrow> si0 \<notin> \<gamma>_sval ?\<Gamma>\<^sub>0 si1')"
      using assms sseparate[of "fst r0" "snd r0" "fst r1" "snd r1"] no_separation_BP_Top
      apply (cases ptr0';cases si0';cases ptr1';cases si1';auto simp add: \<gamma>_top)
      by (meson \<gamma>_non_empty cval_separate_asymmetric equals0I)
  }
  thus ?thesis
    using senclosed[of "fst r0" "snd r0" "fst r1" "snd r1"] 
    apply (cases r0;cases r1;auto)
    by blast
qed

lemma senclosed_not_sseparate:
assumes "senclosed r0 r1"
shows "\<not> sseparate r0 r1"
  using assms sseparate_not_enclosed by blast



lemma sequal_not_sseparate:
assumes "sequal r0 r1"
shows "\<not> sseparate  r0 r1"
proof-
  let ?\<Gamma>\<^sub>0 = "SOME \<Gamma>\<^sub>0 . True"
  {
    obtain ptr0 si0 where 0: "r0 = (ptr0,si0)"
      by (cases r0,auto)
    obtain ptr1 si1 where 1: "r1 = (ptr1,si1)"
      by (cases r1,auto)
    moreover
    obtain v0 s0 where 
           2: "v0 \<in> \<gamma>_sval ?\<Gamma>\<^sub>0 ptr0" 
       and 3: "s0 \<in> \<gamma>_sval ?\<Gamma>\<^sub>0 si0"
       and 4: "v0 \<in> \<gamma>_sval ?\<Gamma>\<^sub>0 ptr1" 
       and 5: "s0 \<in> \<gamma>_sval ?\<Gamma>\<^sub>0 si1"
      using sequal[of "fst r0" "snd r0" "fst r1" "snd r1"] assms 0 1 
      apply (cases r0;cases r1;auto)
      by (metis \<gamma>_non_empty ex_in_conv)
    moreover
    have "\<not>cval_separate(v0,s0) (v0,s0)"
      by (cases v0;cases s0;auto simp add: sep_irreflexive)
    ultimately
    have "\<exists> v0 \<in> \<gamma>_sval ?\<Gamma>\<^sub>0 (fst r0) . \<forall> s0 \<in> \<gamma>_sval ?\<Gamma>\<^sub>0 (snd r0) . \<exists> v1 \<in> \<gamma>_sval ?\<Gamma>\<^sub>0 (fst r1) . \<forall> s1 \<in> \<gamma>_sval ?\<Gamma>\<^sub>0 (snd r1) . \<not>cval_separate (v0,s0) (v1,s1)"
      using sequal[of "fst r0" "snd r0" "fst r1" "snd r1"] assms
      by (smt (verit, del_insts) "0" fst_conv snd_conv)
  }
  thus ?thesis
    apply (cases r0;cases r1;simp add: )
    using assms 
    using senclosed_not_sseparate sequal_implies_senclosed
    by presburger
qed
  







lemma sequal_to_sseparate_region:
 assumes "sequal (a0, si0) (a1, si1)"
    and "sseparate (a2, si2) (a1, si1)"
  shows "sseparate (a2, si2) (a0, si0)"
  using assms senclosed_in_sseparate_region sequal_implies_senclosed 
  by blast


lemma senclosed_in_sseparate_regions:
 assumes "senclosed (a0, si0) (a0', si0')"
    and "senclosed (a1, si1) (a1', si1')"
    and "sseparate (a0',si0') (a1',si1')"
  shows "sseparate (a0,si0) (a1,si1)"
  using assms
  by (meson assms(1) assms(2) ssep_symmetric senclosed_in_sseparate_region)






end



end



