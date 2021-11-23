(*  Title:       X86 instruction semantics and basic block symbolic execution
    Authors:     Freek Verbeek, Abhijith Bharadwaj, Joshua Bockenek, Ian Roessle, Timmy Weerwag, Binoy Ravindran
    Year:        2020
    Maintainer:  Freek Verbeek (freek@vt.edu)
*)

section "Bit and byte-level theorems"

theory BitByte
  imports Main Word_Lib.Bitwise
begin


definition take_bits :: "nat \<Rightarrow> nat \<Rightarrow> 'a::len word \<Rightarrow> 'a::len word" ("\<langle>_,_\<rangle>_" 51) \<comment> \<open>little-endian\<close>
  where "take_bits l h w \<equiv> (w >> l) AND mask (h-l)"

text \<open>
  @{term "take_bits l h w"} takes a subset of bits from word @{term w}, from low (inclusive) to high (exclusive).
  For example, @{term [show_types] "take_bits 2 5 (28::8word) = 7"}.
\<close>

definition take_byte :: "nat \<Rightarrow> 'a::len word \<Rightarrow> 8word" \<comment> \<open>little-endian\<close>
  where "take_byte n w \<equiv> ucast (\<langle>n*8,n*8+8\<rangle>w)"

text \<open>
  @{term "take_byte n w"} takes the nth byte from word @{term w}.
  For example, @{term [show_types] "take_byte 1 (42 << 8::16word) = 42"}.
\<close>

definition overwrite :: "nat \<Rightarrow> nat \<Rightarrow> 'a::len word \<Rightarrow> 'a::len word \<Rightarrow> 'a::len word"
  where "overwrite l h w0 w1 \<equiv> ((\<langle>h,LENGTH('a)\<rangle>w0) << h) OR ((\<langle>l,h\<rangle>w1) << l) OR (\<langle>0,l\<rangle>w0)"

text \<open>
  @{term "overwrite l h w0 w1"} overwrites low (inclusive) to high (exclusive) bits in
  word @{term w0} with bits from word @{term w1}.
  For example, @{term [show_types] "(overwrite 2 4 28 227 :: 8word) = 16"}.
\<close>


text \<open>We prove some theorems about taking the nth bit/byte of an operation. These are useful to prove
      equaltiy between different words, by first applying rule @{thm word_eqI}.\<close>

lemma nth_takebits:
  fixes w :: "'a::len word"
  shows "(\<langle>l,h\<rangle>w) !! n = (if n < LENGTH('a) \<and> n < h - l then w !! (n + l) else False)"
  by (auto simp add: take_bits_def word_ao_nth word_size nth_shiftr)

lemma nth_takebyte:
  fixes w :: "'a::len word"
  shows "take_byte (n div 8) w !! (n mod 8) = (if n mod 8 < LENGTH('a) then w!!n else False )"
  by (auto simp add: take_byte_def nth_ucast nth_takebits split: if_split_asm)

lemma nth_take_byte_overwrite:
  fixes v v' :: "'a::len word"
  shows "take_byte n (overwrite l h v v') !! i = (if i + n * 8 < l \<or> i + n * 8 \<ge> h then take_byte n v !! i else take_byte n v' !! i)"
  by (auto simp add: take_byte_def overwrite_def nth_ucast nth_takebits word_ao_nth nth_shiftl)
     (metis test_bit_bin)+

lemma nth_bitNOT:
  fixes a :: "'a::len word"
  shows "(NOT a) !! n \<longleftrightarrow> (if n < LENGTH('a)  then \<not>(a !! n) else False)"
  apply (auto simp add: word_ops_nth_size word_size)
  by (simp add: bin_nth_uint word_test_bit_def)


text \<open>Various simplification rules\<close>

lemma ucast_take_bits:
  fixes w :: "'a::len word"
  assumes "h = LENGTH('b)"
      and "LENGTH('b) \<le> LENGTH('a)"
  shows "ucast (\<langle>0,h\<rangle>w) = (ucast w ::'b :: len word)"
  apply (rule word_eqI)
  using assms
  by (auto simp add: word_size nth_ucast nth_takebits)

lemma take_bits_ucast:
  fixes w :: "'b::len word"
  assumes "h \<ge> LENGTH('b) \<or> h \<ge> LENGTH('a)"
  shows "(\<langle>0,h\<rangle> (ucast w ::'a :: len word)) = (ucast w ::'a :: len word)"
  using assms test_bit_size[of w]
  apply (auto simp add: word_eq_iff word_size nth_ucast nth_takebits)
  using order.strict_trans2
  by blast

lemma take_bits_take_bits:
  fixes w :: "'a::len word"
  shows "(\<langle>l,h\<rangle>(\<langle>l',h'\<rangle>w)) = (if min LENGTH('a) h \<ge> h' - l' then \<langle>l+l',h'\<rangle>w else (\<langle>l+l',l'+min LENGTH('a) h\<rangle>w))"
  apply (rule word_eqI)
  by (auto simp add: word_size nth_takebits word_ao_nth nth_shiftl min_def)
     (metis semiring_normalization_rules(25))+

lemma take_bits_overwrite:
  shows "\<langle>l,h\<rangle>(overwrite l h w0 w1) = \<langle>l,h\<rangle>w1"
  apply (auto simp add: overwrite_def)
  apply (rule word_eqI)
  by (auto simp add: word_size nth_takebits word_ao_nth nth_shiftl)
     (simp add: bin_nth_uint_imp word_test_bit_def)

lemma overwrite_0_take_bits_0:
  shows "h' \<le> h \<Longrightarrow> overwrite 0 h (\<langle>0,h'\<rangle>w0) w1 = \<langle>0,h\<rangle>w1"
  apply (auto simp add: overwrite_def)
  apply (rule word_eqI)
  by (auto simp add: word_size nth_takebits word_ao_nth nth_shiftl)

lemma take_byte_shiftlr_256:
fixes v :: "256 word"
  assumes "m \<le> n"
  shows "take_byte n (v << m*8) = (if (n+1)*8 \<le> 256 then take_byte (n-m) v else 0)"
  apply (rule word_eqI)
  using assms
  by (auto simp add: take_byte_def nth_ucast nth_takebits word_size nth_shiftl diff_mult_distrib)



subsection \<open> Take\_Bits and arithmetic\<close>

text \<open>This definition is based on @{thm to_bl_plus_carry}, which formulates addition as bitwise operations using @{term xor3} and @{term carry}.\<close>

definition bitwise_add :: "(bool \<times> bool) list \<Rightarrow> bool \<Rightarrow> bool list"
  where "bitwise_add x c \<equiv> foldr (\<lambda>(x, y) res car. xor3 x y car # res (carry x y car)) x (\<lambda>_. []) c"

lemma length_foldr_bitwise_add:
  shows "length (bitwise_add x c) = length x"
  unfolding bitwise_add_def
  by(induct x arbitrary: c) auto

text \<open>This is the "heart" of the proof: bitwise addition of two appended zipped lists can be expressed as
      two consecutive bitwise additions.
      Here, I need to make the assumption that the final carry is False.
 \<close>
lemma bitwise_add_append:
  assumes "x = [] \<or> \<not>carry (fst (last x)) (snd (last x)) True"
  shows "bitwise_add (x @ y) (x\<noteq>[] \<and> c) = bitwise_add x (x\<noteq>[] \<and> c) @ bitwise_add y False"
  using assms
  unfolding bitwise_add_def
  by(induct x arbitrary: c) (auto simp add: case_prod_unfold xor3_def carry_def split: if_split_asm)

lemma bitwise_add_take_append:
  shows "take (length x) (bitwise_add (x @ y) c) = bitwise_add x c"
  unfolding bitwise_add_def
  by(induct x arbitrary: c) (auto simp add: case_prod_unfold xor3_def carry_def split: if_split_asm)

lemma bitwise_add_zero:
  shows "bitwise_add (replicate n (False, False)) False = replicate n False "
  unfolding bitwise_add_def
  by(induct n) (auto simp add: xor3_def carry_def)

lemma bitwise_add_take:
  shows "take n (bitwise_add x c) = bitwise_add (take n x) c"
  unfolding bitwise_add_def
  by (induct n arbitrary: x c,auto)
     (metis append_take_drop_id bitwise_add_def bitwise_add_take_append diff_is_0_eq' length_foldr_bitwise_add length_take nat_le_linear rev_min_pm1 take_all)


lemma fst_hd_drop_zip:
  assumes "n < length x"
      and "length x = length y"
  shows "fst (hd (drop n (zip x y))) = hd (drop n x)"
  using assms
  by (induct x arbitrary: n y,auto)
     (metis (no_types, lifting) Cons_nth_drop_Suc drop_zip fst_conv length_Cons list.sel(1) zip_Cons_Cons)

lemma snd_hd_drop_zip:
  assumes "n < length x"
      and "length x = length y"
  shows "snd (hd (drop n (zip x y))) = hd (drop n y)"
  using assms
  by (induct x arbitrary: n y,auto)
     (metis (no_types, lifting) Cons_nth_drop_Suc drop_zip snd_conv length_Cons list.sel(1) zip_Cons_Cons)

text \<open>
  Ucasting of @{term "a+b"} can be rewritten to taking bits of @{term a} and @{term b}.
\<close>

lemma ucast_plus:
  fixes a b :: "'a::len word"
  assumes "LENGTH('a) > LENGTH('b)"
  shows "(ucast (a + b) ::'b::len word) = (ucast a + ucast b::'b::len word)"
proof-
  have "to_bl (ucast (a + b) ::'b::len word) = to_bl (ucast a + ucast b::'b::len word)"
    using assms
    apply (auto simp add: to_bl_ucast to_bl_plus_carry word_rep_drop length_foldr_bitwise_add drop_zip[symmetric] rev_drop bitwise_add_def simp del: foldr_replicate foldr_append)
    apply (simp only: bitwise_add_def[symmetric] length_foldr_bitwise_add)
    by (auto simp add: drop_take bitwise_add_take[symmetric] rev_take length_foldr_bitwise_add)
  thus ?thesis
    using word_bl.Rep_eqD
    by blast
qed

lemma ucast_uminus:
  fixes a b :: "'a::len word"
assumes "LENGTH('a) > LENGTH('b)"
  shows "ucast (- a) = (- ucast a :: 'b::len word)"
  apply (subst twos_complement)+
  apply (subst word_succ_p1)+
  apply (subst ucast_plus)
  apply (rule assms)
   apply simp
   apply (rule word_eqI)
   apply (auto simp add: word_size nth_ucast nth_bitNOT)
   using assms order.strict_trans
   by blast

lemma ucast_minus:
  fixes a b :: "'a::len word"
  assumes "LENGTH('a) > LENGTH('b)"
  shows "(ucast (a - b) ::'b::len word) = (ucast a - ucast b::'b::len word)"
  using ucast_plus[OF assms,of a "-b"] ucast_uminus[OF assms,of b]
  by auto



lemma to_bl_takebits:
  fixes a :: "'a::len word"
  shows "to_bl (\<langle>0,h\<rangle>a) = replicate (LENGTH('a) - h) False @ drop (LENGTH('a) - h) (to_bl a)"
  apply (auto simp add: take_bits_def bl_word_and to_bl_mask)
  apply (rule nth_equalityI)
  by (auto simp add: min_def nth_append)
(*
lemma ucast_overwrite_eq:
  fixes a b :: "256 word" and c d :: "32 word"
  assumes "(ucast a :: 64 word) = ucast c"
      and "(ucast b :: 8word) = ucast d"
    shows "ucast (overwrite 0 8 a b) = (ucast (overwrite 0 8 c d)::64word)" (is "?x=?y")
proof-
  {
    fix n  :: nat
    assume "n < 64"
    hence "?x !! n = ?y !! n"
      using word_eqD[OF assms(1),of n] word_eqD[OF assms(2),of n] test_bit_size[of c n]
      by (auto simp add: word_size overwrite_def word_ao_nth nth_shiftl nth_ucast nth_takebits)
  }
  note 1= this
  thus ?thesis
    using word_eqI[of ?x ?y]
    by (auto simp add: word_size)
qed
*)



lemma word_mult_pred: "word_pred a * b = a * b - b"
  by (transfer, simp add: algebra_simps)

lemma ucast_times:
  fixes a b :: "'a::len word"
  assumes "LENGTH('a) > LENGTH('b)"
  shows "(ucast (a * b) ::'b::len word) = (ucast a * ucast b::'b::len word)"
  using assms
proof(induct "unat a" arbitrary: a)
  case 0
  thus ?case 
    by (auto simp add: unat_eq_0)
next
  case (Suc x)
  have 1: "a * b = (a-1)*b +b"
    using word_mult_succ[of "a-1" b]
    by (auto simp add: word_succ_p1)
  have 2: "a * b - b = (a-1)*b"
    using word_mult_succ[of "a-1" b]
    by (auto simp add: word_succ_p1)
  have 3: "x = unat (a - 1)"
    using Suc(2)
    by unat_arith
  show ?case
    apply (subst 1)
    apply (subst word_mult_pred[unfolded word_pred_m1])
    apply (subst ucast_plus[OF assms])
    apply (subst 2)
    apply (subst Suc(1)[of "a-1",OF 3 Suc(3)])
    apply (subst ucast_minus[OF assms])
    apply simp
    apply (subst word_mult_pred[unfolded word_pred_m1])
    by simp
qed



definition word_sdiv :: "'a ::len word \<Rightarrow> 'a ::len word \<Rightarrow> 'a ::len word" (infixl "sdiv" 70)
  where "word_sdiv a b \<equiv> word_of_int (sint a div sint b)"

lemma sshiftr_sdiv:
  fixes w :: "'a :: len word"
  assumes "- (2 ^ (LENGTH('a) - Suc 0)) \<le> (2::int) ^ n"
      and "n < (LENGTH('a) - 1)"
    shows "(w >>> n) = w sdiv (2 ^ n :: 'a :: len word)"
proof-
  have "sint (w >>> n) = sint w div (sint (2 ^ n :: 'a :: len word))"
    apply (subst sshiftr_div_2n)
    apply (subst sint_of_int_eq[of "2^n",where 'a='a,simplified])
    using assms
    by auto
  thus ?thesis
    using word_sbin.inverse_norm[of "sint w div (sint (2 ^ n :: 'a :: len word))" "w >>> n" ]
    apply (auto simp add: word_sdiv_def)
    by (metis More_Word.of_int_sint)
qed


lemma ucast_scast:
  fixes a :: "'a::len word"
  assumes "LENGTH('b) < LENGTH('c)"
  shows "(ucast ((scast a)::'c ::len word)::'b::len word) = scast a"
  apply (rule word_eqI)
  using assms
  by (auto simp add: word_size nth_ucast nth_scast)




text \<open> All simplification rules that are used during symbolic execution.\<close>
named_theorems BitByte_simps 
declare ucast_plus [BitByte_simps]
declare ucast_minus [BitByte_simps]
declare ucast_uminus [BitByte_simps]
declare ucast_times [BitByte_simps]
declare take_bits_overwrite [BitByte_simps]
declare take_bits_take_bits [BitByte_simps]
declare ucast_take_bits [BitByte_simps]
declare overwrite_0_take_bits_0 [BitByte_simps]
declare mask_eq [BitByte_simps]
declare ucast_down_ucast_id [BitByte_simps]
declare is_down [BitByte_simps]
declare take_bits_ucast [BitByte_simps]
declare ucast_up_ucast_id [BitByte_simps]
declare ucast_up_ucast [BitByte_simps]
declare is_up [BitByte_simps]
(*declare ucast_overwrite_eq [BitByte_simps]*)
declare word_size[BitByte_simps]
declare scast_up_scast[BitByte_simps]
declare ucast_scast[BitByte_simps]

lemmas shiftr_div_2n_w_numeral[BitByte_simps] = shiftr_div_2n_w[of "numeral w"] for w
lemmas sshiftr_sdiv_numeral[BitByte_simps] = sshiftr_sdiv[of "numeral n"] for n


text \<open>Simplification for immediate (numeral) values.\<close>
lemmas take_bits_numeral[simp] = take_bits_def[of _ _ "numeral n"] for n
lemmas take_bits_num0[simp] = take_bits_def[of _ _ "0"] for n
lemmas take_bits_num1[simp] = take_bits_def[of _ _ "1"] for n
lemmas overwrite_numeral_numeral[simp] = overwrite_def[of _ _ "numeral n" "numeral m"] for n m
lemmas overwrite_num0_numeral[simp] = overwrite_def[of _ _ 0 "numeral m"] for n m
lemmas overwrite_numeral_num0[simp] = overwrite_def[of _ _ "numeral m" 0] for n m
lemmas overwrite_numeral_00[simp] = overwrite_def[of _ _ 0 0]

lemmas shiftr_def_numeral[simp] = shiftr_def[of "- numeral n" "numeral m"] for n m
lemmas shiftr1_def_numeral[simp] = shiftr1_eq[of "- numeral n"] for n
lemmas bintrunc_numeral_minus_1[simp] = bintrunc_numeral[of k "-1"] for k

lemmas word_less_minus_numeral_numeral [simp] = word_less_def [of "- numeral a" "numeral b"] for a b

lemmas ucast_minus_numeral[simp] = ucast_eq[of "- numeral w"] for w
lemmas mask_numeral[simp] = mask_eq[of "numeral w"] for w
lemmas shiftl_int_def_numeral[BitByte_simps] = shiftl_int_def[of _ "numeral w"] for w
lemmas shiftl_t2n_numeral[BitByte_simps] = shiftl_t2n[of _ "numeral w"] for w

end