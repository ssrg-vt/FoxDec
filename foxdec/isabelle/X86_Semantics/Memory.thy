(*  Title:       X86 instruction semantics and basic block symbolic execution
    Authors:     Freek Verbeek, Abhijith Bharadwaj, Joshua Bockenek, Ian Roessle, Timmy Weerwag, Binoy Ravindran
    Year:        2020
    Maintainer:  Freek Verbeek (freek@vt.edu)
*)

section "Memory-related theorems"

theory Memory
  imports BitByte
begin

context
  fixes dummy_type :: "'a::len"
begin

primrec read_bytes :: "('a word \<Rightarrow> 8 word) \<Rightarrow> 'a word \<Rightarrow> nat \<Rightarrow> 8word list"
  where "read_bytes m a 0   = []"
  | "read_bytes m a (Suc n) = m (a + of_nat n) # read_bytes m a n"

text \<open>Read bytes from memory. Memory is represented by a term of @{typ "64 word \<Rightarrow> 8 word"}.
      Given an address @{term [show_types] "a::64 word"} and a size @{term n}, retrieve the bytes in
      the order they are stored in memory.\<close>

definition region_addresses :: "'a word \<Rightarrow> nat \<Rightarrow> 'a word set"
  where "region_addresses a si \<equiv> {a' . \<exists> i < si . a' = a + of_nat (si - i - 1)}"

text \<open>The set of addresses belonging to a region starting at address @{term "a::64 word"} of @{term si} bytes.\<close>

definition region_overflow :: "'a word \<Rightarrow> nat \<Rightarrow> bool"
  where "region_overflow a si \<equiv> unat a + si \<ge> 2^LENGTH('a)"

text \<open>An overflow occurs if the address plus the size is greater equal @{term "2^64"}\<close>

definition enclosed :: "'a word \<Rightarrow> nat \<Rightarrow> 'a word \<Rightarrow> nat \<Rightarrow> bool"
  where "enclosed a' si' a si \<equiv> unat a + si < 2^LENGTH('a) \<and> unat a \<le> unat a' \<and> unat a' + si' \<le> unat a + si"

text \<open>A region is enclosed in another if its @{const region_addresses} is a subset of the other.\<close>

definition separate :: "'a word \<Rightarrow> nat \<Rightarrow> 'a word \<Rightarrow> nat \<Rightarrow> bool"
  where "separate a si a' si' \<equiv> si \<noteq>0 \<and> si' \<noteq> 0 \<and> region_addresses a si \<inter> region_addresses a' si' = {}"

text \<open>A region is separate from another if they do not overlap.\<close>



lemma region_addresses_iff: "a' \<in> region_addresses a si \<longleftrightarrow> unat (a' - a) < si"
  apply (auto simp add: region_addresses_def)
   apply (metis diff_Suc_less le_less_trans less_imp_Suc_add take_bit_nat_less_eq_self zero_less_Suc)
  by (smt (z3) add.commute add_Suc_right add_diff_cancel_left' diff_add_cancel less_add_Suc2 less_imp_Suc_add word_unat.Rep_inverse)

lemma notin_region_addresses:
  assumes "x \<notin> region_addresses a si"
  shows "unat x < unat a \<or> unat a + si \<le> unat x"
  by (metis assms add.commute less_diff_conv2 not_le_imp_less region_addresses_iff unat_sub_if')

lemma notin_region_addresses_sub:
  assumes "x \<notin> region_addresses a si"
  shows "unat (x - a') < unat (a - a') \<or> unat (a - a') + si \<le> unat (x - a')"
  using assms notin_region_addresses region_addresses_iff by auto

lemma region_addresses_eq_empty_iff: "region_addresses a si = {} \<longleftrightarrow> si = 0"
  by (metis region_addresses_iff add_diff_cancel_left' ex_in_conv neq0_conv not_less0 unsigned_0)

lemma length_read_bytes:
  shows "length (read_bytes m a si) = si"
  by (induct si,auto)

lemma nth_read_bytes:
assumes "n < si"
shows "read_bytes m a si ! n = m (a + of_nat (si - 1 - n))"
  using assms
  apply (induct si arbitrary: n,auto)
  subgoal for si n
    by(cases n,auto)
  done

text \<open>Writing to memory occurs via function @{const override_on}.
      In case of enclosure, reading bytes from memory overridden on a set of region addresses can
      be simplified to reading bytes from the overwritten memory only.
      In case of separation, reading bytes from overridden memory can be simplified to reading
      from the original memory.\<close>

lemma read_bytes_override_on_enclosed:
assumes "offset' \<le> offset"
    and "si' \<le> si"
    and "unat offset + si' \<le> si + unat offset'"
  shows "read_bytes (override_on m m' (region_addresses (a - offset) si)) (a - offset') si' = read_bytes m' (a - offset') si'"
proof-
  {
    fix i
    assume 1: "i < si'"
    let ?i = "(si + i + unat offset') - unat offset - si'"

    have "i + unat offset' < si' + unat offset"
      using 1 assms(1)
      by unat_arith
    hence 2: "si + i + unat offset' - (unat offset + si') < si"
      using diff_less[of "(si' + unat offset - i) - unat offset'" si] assms(3)
      by auto
    moreover
    have "of_nat (si' - Suc i) - offset' = of_nat (si - Suc ?i) - offset"
      using assms 1 2 by (auto simp add: of_nat_diff)
    ultimately
    have "\<exists> i' < si. of_nat (si' - Suc i) - offset' = of_nat (si - Suc i') - offset"
      by auto
  }
  note 1 = this
  show ?thesis
    apply (rule nth_equalityI)
    using 1
    by (auto simp add: length_read_bytes nth_read_bytes override_on_def region_addresses_def)
qed

lemmas read_bytes_override_on = read_bytes_override_on_enclosed[where offset=0 and offset'=0,simplified]

lemma read_bytes_override_on_enclosed_plus:
  assumes "unat offset + si' \<le> si"
      and "si \<le> 2^LENGTH('a)"
  shows "read_bytes (override_on m m' (region_addresses a si)) (offset+a) si' = read_bytes m' (offset+a) si'"
proof-
  {
    fix i
    have "i < si' \<Longrightarrow> \<exists> i' < si. offset + (of_nat (si' - Suc i)::'a word) = of_nat (si - Suc i')"
      apply (rule exI[of _"si - si' + i - unat offset"])
      using assms by (auto simp add: of_nat_diff)
  }
  note 1 = this
  show ?thesis
    apply (rule nth_equalityI)
    using assms 1
    by (auto simp add: override_on_def length_read_bytes nth_read_bytes region_addresses_def)
qed


lemma read_bytes_override_on_separate:
assumes "separate a si a' si'"
  shows "read_bytes (override_on m m' (region_addresses a si)) a' si' = read_bytes m a' si'"
  apply (rule nth_equalityI)
  using assms
  by (auto simp add: length_read_bytes nth_read_bytes override_on_def separate_def region_addresses_def)


text\<open>Bytes are are written to memory one-by-one, then read by @{const read_bytes} producing a list of bytes.
    That list is concatenated again using @{const word_rcat}. Writing @{term si} bytes of word @{term w} into
    memory, reading the byte-list and concatenating again produces @{term si} bytes of the original word.\<close>


lemma word_rcat_read_bytes_enclosed:
  fixes w :: "'b::len word"
  assumes "LENGTH('b) \<le> 2^LENGTH('a)"
    and "unat offset + si \<le> 2^LENGTH('a)"
  shows "word_rcat (read_bytes (\<lambda>a'. take_byte (unat (a' - a)) w) (a + offset) si) = \<langle>unat offset * 8,(unat offset + si) * 8\<rangle>w"
  apply (rule word_eqI)
  using assms
  apply (auto simp add: test_bit_rcat word_size length_read_bytes rev_nth nth_read_bytes unat_of_nat nth_takebyte unat_word_ariths )
    apply (auto simp add: take_byte_def nth_ucast nth_takebits take_bit_eq_mod split: if_split_asm)[1]
   apply (auto simp add: nth_takebits split: if_split_asm)[1]
  apply (auto simp add: take_byte_def nth_ucast nth_takebits split: if_split_asm)[1]
  by (auto simp add: rev_nth length_read_bytes take_byte_def nth_ucast nth_takebits nth_read_bytes unat_word_ariths unat_of_nat take_bit_eq_mod split: if_split_asm)

lemmas word_rcat_read_bytes = word_rcat_read_bytes_enclosed[where offset=0,simplified]



text \<open>The following theorems allow reasoning over enclosure and separation, for example as linear arithmetic.\<close>

lemma enclosed_spec:
  assumes enclosed: "enclosed a' si' a si"
      and x_in: "x \<in> region_addresses a' si'"
    shows "x \<in> region_addresses a si"
proof -
  from x_in have "unat (x - a') < si'"
    using region_addresses_iff by blast
  with enclosed have "unat (x - a) < si"
    unfolding enclosed_def by (auto simp add: unat_sub_if' split: if_split_asm)
  then show ?thesis
    using region_addresses_iff by blast
qed

lemma address_in_enclosed_region_as_linarith:
  assumes "enclosed a' si' a si"
      and "x \<in> region_addresses a' si'"
    shows "a \<le> x \<and> a' \<le> x \<and> x < a' + of_nat si' \<and> x < a + of_nat si"
  using assms
  by (auto simp add: enclosed_def region_addresses_def word_le_nat_alt word_less_nat_alt unat_of_nat unat_word_ariths unat_sub_if' take_bit_eq_mod)


lemma address_of_enclosed_region_ge:
  assumes "enclosed a' si' a si"
    shows "a' \<ge> a"
  using assms word_le_nat_alt
  by (auto simp add: enclosed_def)

lemma address_in_enclosed_region:
  assumes "enclosed a' si' a si"
      and "x \<in> region_addresses a' si'"
    shows "unat (x - a) \<ge> unat (a' - a) \<and> unat (a' - a) + si' > unat (x - a) \<and> unat (x - a) < si"
  by (smt (z3) address_in_enclosed_region_as_linarith add_diff_cancel_left' address_of_enclosed_region_ge assms(1) assms(2) diff_diff_add enclosed_spec le_iff_add nat_add_left_cancel_less region_addresses_iff unat_sub_if' word_le_minus_mono_left word_unat.Rep_inverse word_unat_less_le)

lemma enclosed_minus_minus:
  fixes a :: "'a word"
  assumes "offset \<ge> offset'"
      and "unat offset - si \<le> unat offset' - si'"
      and "unat offset' \<ge> si'"
      and "unat offset \<ge> si"
      and "a \<ge> offset"
    shows "enclosed (a - offset') si' (a - offset) si"
proof-
  have "unat offset' \<le> unat a"
    using assms(1,5)
    by unat_arith
  thus ?thesis
    using assms
    apply (auto simp add: enclosed_def unat_sub_if_size word_size)
    apply unat_arith
    using diff_le_mono2[of "unat offset - si" "unat offset' - si'" "unat a"]
    apply (auto simp add: enclosed_def unat_sub_if_size word_size)
     by unat_arith+
qed

lemma enclosed_plus:
  fixes a :: "'a word"
  assumes "si' < si"
      and "unat a + si < 2^LENGTH('a)"
    shows "enclosed a si' a si"
  using assms
  by (auto simp add: enclosed_def)

lemma separate_symm[symmetric]: "separate a si a' si' = separate a' si' a si"
  by (metis inf.commute separate_def)

lemma separate_iff: "separate a si a' si' \<longleftrightarrow> si > 0 \<and> si' > 0 \<and> unat (a' - a) \<ge> si \<and> unat (a - a') \<ge> si'"
proof
  assume assm: "separate a si a' si'"
  have "unat (a' - a) \<ge> si" if "separate a si a' si'" for a si a' si'
  proof (rule ccontr)
    assume "\<not> unat (a' - a) \<ge> si"
    then have "a' \<in> region_addresses a si"
      by (simp add: region_addresses_iff)
    moreover from that have "a' \<in> region_addresses a' si'"
      using region_addresses_iff separate_def by auto
    ultimately have "\<not>separate a si a' si'"
      by (meson disjoint_iff separate_def)
    with that show False
      by blast
  qed
  with assm have "unat (a' - a) \<ge> si" and "unat (a - a') \<ge> si'"
    using separate_symm by auto
  with assm show "si > 0 \<and> si' > 0 \<and> unat (a' - a) \<ge> si \<and> unat (a - a') \<ge> si'"
    using separate_def by auto
next
  assume assm: "si > 0 \<and> si' > 0 \<and> unat (a' - a) \<ge> si \<and> unat (a - a') \<ge> si'"
  then have "unat (x - a') \<ge> si'" if "unat (x - a) < si" for x
    using that apply (auto simp add: unat_sub_if' split: if_split_asm)
     apply (meson Nat.le_diff_conv2 add_increasing le_less_trans less_imp_le_nat unsigned_greater_eq unsigned_less)
    by (smt (z3) Nat.le_diff_conv2 add_leD2 le_less_trans linorder_not_less nat_le_linear unat_lt2p)
  then have "region_addresses a si \<inter> region_addresses a' si' = {}"
    by (simp add: region_addresses_iff disjoint_iff leD)
  with assm show "separate a si a' si'"
    by (simp add: separate_def)
qed

lemma separate_as_linarith:
assumes "\<not>region_overflow a si"
    and "\<not>region_overflow a' si'"
  shows "separate a si a' si' \<longleftrightarrow> 0 < si \<and> 0 < si' \<and> (a + of_nat si \<le> a' \<or> a' + of_nat si' \<le> a)"
    (is "?lhs \<longleftrightarrow> ?rhs")
proof
  assume ?lhs then show ?rhs
   by (meson separate_iff le_less le_plus not_le_imp_less word_of_nat_le)
next
  have *: "separate a si a' si'"
    if "si > 0" and "si' > 0" and "a + of_nat si \<le> a'"
      and "\<not>region_overflow a si" and "\<not>region_overflow a' si'"
    for a si a' si'
  proof -
    from that have "unat a + si < 2^LENGTH('a)" and "unat a' + si' < 2^LENGTH('a)"
      by (meson not_le region_overflow_def)+
    have "x < a + of_nat si" if "x \<in> region_addresses a si" for x
      by (smt (z3) Abs_fnat_hom_add \<open>unat a + si < 2 ^ LENGTH('a)\<close> add.commute dual_order.trans le_add1 less_diff_conv2 not_less region_addresses_iff that unat_of_nat_len unat_sub_if' word_less_iff_unsigned word_unat.Rep_inverse)
    moreover have "x \<ge> a'" if "x \<in> region_addresses a' si'" for x
      using address_in_enclosed_region_as_linarith enclosed_def \<open>unat a' + si' < 2 ^ LENGTH('a)\<close> that by blast
    ultimately show ?thesis
      using separate_def that by fastforce
  qed
  assume ?rhs then show ?lhs
    by (meson "*" assms separate_symm)
qed

text \<open>Compute separation in case the addresses and sizes are immediate values.\<close>
lemmas separate_as_linarith_numeral [simp] =
  separate_as_linarith [of "numeral a::'a word" "numeral si" "numeral a'::'a word" "numeral si'"] for a si a' si'
lemmas separate_as_linarith_numeral_1 [simp] =
  separate_as_linarith [of "numeral a::'a word" "numeral si" "numeral a'::'a word" "Suc 0"] for a si a'
lemmas separate_as_linarith_numeral1_ [simp] =
  separate_as_linarith [of "numeral a::'a word" "Suc 0" "numeral a'::'a word" "numeral si'"] for a a' si'
lemmas separate_as_linarith_numeral11 [simp] =
  separate_as_linarith [of "numeral a::'a word" "Suc 0" "numeral a'::'a word" "Suc 0"] for a a'
lemmas region_overflow_numeral[simp] =
  region_overflow_def [of "numeral a::'a word" "numeral si"] for a si
lemmas region_overflow_numeral1[simp] =
  region_overflow_def [of "numeral a::'a word" "Suc 0"] for a


lemma separate_plus_none:
  assumes "si' \<le> unat offset"
      and "0 < si"
      and "0 < si'"
      and "unat offset + si \<le> 2^LENGTH('a)"
    shows "separate (a + offset) si a si'"
  using assms apply (auto simp add: separate_iff)
  by (smt (z3) Nat.diff_diff_right add.commute add_leD1 diff_0 diff_is_0_eq diff_zero not_gr_zero unat_sub_if' unsigned_0)


lemma separate_plus_plus:
  assumes "unat offset + si \<le> unat offset' \<or> unat offset' + si' \<le> unat offset"
      and "0 < si"
      and "0 < si'"
      and "unat offset + si \<le> 2^LENGTH('a)"
      and "unat offset' + si' \<le> 2^LENGTH('a)"
  shows "separate (a + offset) si (a + offset') si'"
  using assms by (auto simp add: separate_iff unat_sub_if')


lemmas unat_minus = unat_sub_if'[of 0,simplified]

lemma separate_minus_minus':
  assumes "si \<noteq> 0"
      and "si' \<noteq> 0"
      and "unat offset \<ge> si"
      and "unat offset' \<ge> si'"
      and "unat offset - si \<ge> unat offset'"
    shows "separate (a - offset) si (a - offset') si'"
  using assms apply (auto simp add: separate_iff)
   apply (metis Nat.le_diff_conv2 add.commute add_leD2 unat_sub_if')
  by (smt (z3) add.commute add_diff_cancel_right' diff_add_cancel diff_le_self diff_less less_le_trans nat_le_linear not_le unat_sub_if')

lemma separate_minus_minus:
  assumes "si \<noteq> 0"
      and "si' \<noteq> 0"
      and "unat offset \<ge> si"
      and "unat offset' \<ge> si'"
      and "unat offset - si \<ge> unat offset' \<or> unat offset' - si' \<ge> unat offset"
    shows "separate (a - offset) si (a - offset') si'"
  by (meson assms separate_minus_minus' separate_symm)

lemma separate_minus_none:
  assumes "si \<noteq> 0"
      and "si' \<noteq> 0"
      and "unat offset \<ge> si"
      and "si' \<le> 2^LENGTH('a) - unat offset"
    shows "separate (a - offset) si a si'"
proof -
  have "0 < si" and "0 < si'"
    using assms(1,2) by blast+
  moreover have "\<not> offset \<le> 0"
    using assms(1) assms(3) by fastforce
  ultimately show ?thesis
    by (smt (z3) add_diff_cancel_left' assms(3,4) diff_diff_eq2 diff_zero le_add_diff_inverse less_or_eq_imp_le separate_iff unat_sub_if' unsigned_0 unsigned_less word_less_eq_iff_unsigned)
qed


lemma separate_plus_none2:
  fixes a :: "'a::len word"
  assumes "si \<le> 1"
      and "0 < si"
      and "0 < si'"
      and "1 + si' \<le> 2^LENGTH('a)"
    shows "separate a si (1 + a) si'"
  using assms by (auto simp add: separate_iff unat_minus)

lemma separate_plus_none3:
  fixes a :: "'a::len word"
  assumes "si \<le> 1"
      and "0 < si"
      and "0 < si'"
      and "1 + si' \<le> 2^LENGTH('a)"
    shows "separate (1 + a) si' a si"
  using assms by (auto simp add: separate_iff unat_minus)

lemma separate_plus_none4:
  fixes a :: "'a::len word"
  assumes "si' \<le> unat offset"
      and "0 < si"
      and "0 < si'"
      and "unat offset + si \<le> 2^LENGTH('a)"
    shows "separate a si' (a + offset) si"
  using assms apply (auto simp add: separate_iff)
  by (smt (z3) Nat.diff_diff_right add.commute add_leD1 diff_0 diff_is_0_eq diff_zero not_gr_zero unat_sub_if' unsigned_0)


text \<open>The following theorems are used during symbolic execution to determine whether two regions are separate.\<close>

named_theorems separate_simps
declare separate_plus_none [separate_simps]
declare separate_plus_plus[separate_simps]
declare separate_minus_none [separate_simps]
declare separate_minus_minus [separate_simps]
declare separate_plus_none2 [separate_simps]
declare separate_plus_none3 [separate_simps]
declare separate_plus_none4 [separate_simps]


end
end