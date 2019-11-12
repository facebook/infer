(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Misc. theorems that aren't specific to the semantics of LLVM or Sledge. These
 * could be upstreamed to HOL, and should eventually. *)

open HolKernel boolLib bossLib Parse;
open listTheory rich_listTheory arithmeticTheory integerTheory llistTheory pathTheory;
open integer_wordTheory wordsTheory pred_setTheory alistTheory;
open finite_mapTheory open logrootTheory numposrepTheory;
open settingsTheory;

new_theory "misc";

numLib.prefer_num ();

(* Labels for the transitions to make externally observable behaviours apparent.
 * For now, we'll consider this to be writes to global variables.
 * *)
Datatype:
  obs =
  | Tau
  | W 'a (word8 list)
  | Exit int
  | Error
End

Datatype:
  trace_type =
  | Stuck
  | Complete int
  | Partial
End

Inductive observation_prefixes:
  (∀l i. observation_prefixes (Complete i, l) (Complete i, filter ($≠ Tau) l)) ∧
  (∀l. observation_prefixes (Stuck, l) (Stuck, filter ($≠ Tau) l)) ∧
  (∀l1 l2 x.
    l2 ≼ l1 ∧ (l2 = l1 ⇒ x = Partial)
    ⇒
    observation_prefixes (x, l1) (Partial, filter ($≠ Tau) l2))
End

Definition take_prop_def:
  (take_prop P n [] = []) ∧
  (take_prop P n (x::xs) =
    if n = 0 then
      []
    else if P x then
      x :: take_prop P (n - 1) xs
    else
      x :: take_prop P n xs)
End

Theorem filter_take_prop[simp]:
  ∀P n l. filter P (take_prop P n l) = take n (filter P l)
Proof
  Induct_on `l` >> rw [take_prop_def]
QED

Theorem take_prop_prefix[simp]:
  ∀P n l. take_prop P n l ≼ l
Proof
  Induct_on `l` >> rw [take_prop_def]
QED

(*
Theorem take_prop_eq:
  ∀P n l. take_prop P n l = l ∧ l ≠ [] ∧ n ≤ length (filter P l) ⇒ P (last l)
Proof
  Induct_on `l` >> simp_tac (srw_ss()) [take_prop_def] >> rpt gen_tac >>
  Cases_on `n = 0` >> pop_assum mp_tac >> simp_tac (srw_ss()) [] >>
  Cases_on `P h` >> pop_assum mp_tac >> simp_tac (srw_ss()) [] >>
  strip_tac >> strip_tac >>
  Cases_on `l` >> pop_assum mp_tac >> simp_tac (srw_ss()) []
  >- metis_tac [take_prop_def] >>
  ntac 2 strip_tac >> first_x_assum drule >>
  simp []
QED
*)

Theorem take_prop_eq:
  ∀P n l. take_prop P n l = l ⇒ length (filter P l) ≤ n
Proof
  Induct_on `l` >> simp_tac (srw_ss()) [take_prop_def] >> rpt gen_tac >>
  Cases_on `n = 0` >> pop_assum mp_tac >> simp_tac (srw_ss()) [] >>
  Cases_on `P h` >> pop_assum mp_tac >> simp_tac (srw_ss()) [] >>
  strip_tac >> strip_tac >>
  Cases_on `l` >> pop_assum mp_tac >> simp_tac (srw_ss()) [] >>
  ntac 2 strip_tac >> first_x_assum drule >> simp []
QED

(* ----- Theorems about list library functions ----- *)

Theorem dropWhile_map:
  ∀P f l. dropWhile P (map f l) = map f (dropWhile (P o f) l)
Proof
  Induct_on `l` >> rw []
QED

Theorem dropWhile_prop:
  ∀P l x. x < length l - length (dropWhile P l) ⇒ P (el x l)
Proof
  Induct_on `l` >> rw [] >>
  Cases_on `x` >> fs []
QED

Theorem dropWhile_rev_take:
  ∀P n l x.
    let len = length (dropWhile P (reverse (take n l))) in
      x + len < n ∧ n ≤ length l ⇒ P (el (x + len) l)
Proof
  rw [] >>
  `P (el ((n - 1 - x - length (dropWhile P (reverse (take n l))))) (reverse (take n l)))`
  by (irule dropWhile_prop >> simp [LENGTH_REVERSE]) >>
  rfs [EL_REVERSE, EL_TAKE, PRE_SUB1]
QED

Theorem take_replicate:
  ∀m n x. take m (replicate n x) = replicate (min m n) x
Proof
  Induct_on `n` >> rw [TAKE_def, MIN_DEF] >> fs [] >>
  Cases_on `m` >> rw []
QED

Theorem length_take_less_eq:
  ∀n l. length (take n l) ≤ n
Proof
  Induct_on `l` >> rw [TAKE_def] >>
  Cases_on `n` >> fs []
QED

Theorem flat_drop:
  ∀n m ls. flat (drop m ls) = drop (length (flat (take m ls))) (flat ls)
Proof
  Induct_on `ls` >> rw [DROP_def, DROP_APPEND] >>
  irule (GSYM DROP_LENGTH_TOO_LONG) >> simp []
QED

Theorem take_is_prefix:
  ∀n l. take n l ≼ l
Proof
  Induct_on `l` >> rw [TAKE_def]
QED

Theorem sum_prefix:
  ∀l1 l2. l1 ≼ l2 ⇒ sum l1 ≤ sum l2
Proof
  Induct >> rw [] >> Cases_on `l2` >> fs []
QED

Theorem flookup_fdiff:
  ∀m s k.
    flookup (fdiff m s) k =
      if k ∈ s then None else flookup m k
Proof
  rw [FDIFF_def, FLOOKUP_DRESTRICT] >> fs []
QED

Theorem inj_map_prefix_iff:
  ∀f l1 l2. INJ f (set l1 ∪ set l2) UNIV ⇒ (map f l1 ≼ map f l2 ⇔ l1 ≼ l2)
Proof
  Induct_on `l1` >> rw [] >>
  Cases_on `l2` >> rw [] >>
  `INJ f (set l1 ∪ set t) UNIV`
  by (
    irule INJ_SUBSET >> qexists_tac `(h INSERT set l1) ∪ (set (h'::t))` >>
    simp [SUBSET_DEF] >> fs [] >>
    metis_tac []) >>
  fs [INJ_IFF] >> metis_tac []
QED

Theorem is_prefix_subset:
  ∀l1 l2. l1 ≼ l2 ⇒ set l1 ⊆ set l2
Proof
  Induct_on `l1` >> rw [] >>
  Cases_on `l2` >> fs [SUBSET_DEF]
QED

Theorem mem_el_front:
  ∀n l. Suc n < length l ⇒ mem (el n l) (front l)
Proof
  Induct >> rw [] >> Cases_on `l` >> fs [FRONT_DEF] >> rw [] >> fs []
QED

Theorem last_take[simp]:
  ∀n l. n < length l ⇒ last (take (Suc n) l) = el n l
Proof
  Induct >> rw [] >> Cases_on `l` >> rw [] >> fs [LAST_DEF] >>
  rw [] >> fs []
QED

Theorem filter_is_prefix:
  ∀l1 l2 P. l1 ≼ l2 ⇒ filter P l1 ≼ filter P l2
Proof
  Induct >> rw [] >> Cases_on `l2` >> fs []
QED

Theorem alookup_map_key:
  ∀al x f g.
    (∀y z. f y = f z ⇒ y = z)
    ⇒
    alookup (map (\(k, v). (f k, g k v)) al) (f x) =
    option_map (g x) (alookup al x)
Proof
  Induct >> rw [] >> pairarg_tac >> rw [] >> metis_tac []
QED

Theorem map_is_some:
  ∀f l1. (∃l2. map f l1 = map Some l2) ⇔ every IS_SOME (map f l1)
Proof
  Induct_on `l1` >> rw [] >> eq_tac >> rw []
  >- (Cases_on `l2` >> fs [])
  >- (Cases_on `l2` >> fs [EVERY_MAP] >> metis_tac [])
  >- (fs [optionTheory.IS_SOME_EXISTS] >> metis_tac [MAP])
QED

Theorem list_rel_flat:
  ∀r ls l. list_rel r (flat ls) l ⇔ ∃ls2. list_rel (list_rel r) ls ls2 ∧ l = flat ls2
Proof
  Induct_on `ls` >> rw [LIST_REL_SPLIT1] >> eq_tac >> rw [PULL_EXISTS] >>
  metis_tac []
QED

Theorem alookup_some:
  ∀l k v.
    alookup l k = Some v ⇔ ∃l1 l2. l = l1 ++ [(k,v)] ++ l2 ∧ alookup l1 k = None
Proof
  ho_match_mp_tac ALOOKUP_ind >> rw [] >> eq_tac >> rw []
  >- (qexists_tac `[]` >> qexists_tac `l` >> rw [])
  >- (Cases_on `l1` >> fs [] >> rw [] >> fs [])
  >- (qexists_tac `(x,y)::l1` >> rw [])
  >- (Cases_on `l1` >> fs [] >> rw [] >> rfs [] >> metis_tac [])
QED

Theorem reverse_eq_append:
  ∀l1 l2 l3. reverse l1 = l2 ++ l3 ⇔ l1 = reverse l3 ++ reverse l2
Proof
  rw [] >> eq_tac >> rw [] >>
  `reverse (reverse l1) = reverse (l2 ++ l3)` by metis_tac [] >>
  fs [] >>
  metis_tac [REVERSE_REVERSE, REVERSE_APPEND]
QED

Theorem zip_eq_append:
  ∀l1 l2 l3 l4.
    length l1 = length l2 ⇒
    (zip (l1,l2) = l3 ++ l4 ⇔
     ∃l11 l12 l21 l22.
       l1 = l11 ++ l12 ∧ l2 = l21 ++
       l22 ∧ length l11 = length l3 ∧ length l21 = length l3 ∧
       length l12 = length l4 ∧ length l22 = length l4 ∧
       l3 = zip (l11, l21) ∧ l4 = zip (l12, l22))
Proof
  Induct_on `l1` >> rw []
  >- metis_tac [] >>
  Cases_on `l2` >> fs [] >> Cases_on `l3` >> fs []
  >- (eq_tac >> rw [] >> rw [] >> metis_tac [LENGTH_ZIP]) >>
  eq_tac >> rw [] >> fs [] >> rw []
  >- (
    qexists_tac `h::l11` >> rw [] >>
    metis_tac [APPEND, ZIP_def, LENGTH]) >>
  Cases_on `l11` >> Cases_on `l21` >> fs [] >> rw [] >> rfs [] >>
  metis_tac []
QED

Theorem append_split_last:
  ∀l1 l2 l3 l4 x.
    l1 ++ [x] ++ l2 = l3 ++ [x] ++ l4 /\
    ¬mem x l2 ∧ ¬mem x l4 ⇒
    l1 = l3 ∧ l2 = l4
Proof
  Induct_on `l1`
  >- (rw [] >> Cases_on `l3` >> fs [] >> rfs []) >>
  rpt gen_tac >>
  Cases_on `l3`
  >- (simp_tac (srw_ss()) [] >> CCONTR_TAC >> fs [] >> rw [] >> fs []) >>
  simp_tac (srw_ss()) [] >> metis_tac []
QED

Theorem append_split_eq:
  ∀l1 l2 l3 l4 x y.
    l1 ++ [x] ++ l2 = l3 ++ [y] ++ l4 /\
    length l2 = length l4 ⇒
    l1 = l3 ∧ x = y ∧ l2 = l4
Proof
  Induct_on `l1` >-
  (rw [] >> Cases_on `l3` >> fs [] >> rw [] >> fs []) >>
  rpt gen_tac >> Cases_on `l3`
  >- (simp_tac (srw_ss()) [] >> CCONTR_TAC >> fs [] >> rw [] >> fs []) >>
  simp_tac (srw_ss()) [] >> metis_tac []
QED

(* ----- Theorems about log ----- *)

Theorem mul_div_bound:
  ∀m n. n ≠ 0 ⇒ m - (n - 1) ≤ n * (m DIV n) ∧ n * (m DIV n) ≤ m
Proof
  rw [] >>
  `0 < n` by decide_tac >>
  drule DIVISION >> disch_then (qspec_then `m` mp_tac) >>
  decide_tac
QED

Theorem exp_log_bound:
  ∀b n. 1 < b ∧ n ≠ 0 ⇒ n DIV b + 1 ≤ b ** (log b n) ∧ b ** (log b n) ≤ n
Proof
  rw [] >> `0 < n` by decide_tac >>
  drule LOG >> disch_then drule >> rw [] >>
  fs [ADD1, EXP_ADD] >>
  simp [DECIDE ``∀x y. x + 1 ≤ y ⇔ x < y``] >>
  `∃x. b = Suc x` by intLib.COOPER_TAC >>
  `b * (n DIV b) < b * b ** log b n` suffices_by metis_tac [LESS_MULT_MONO] >>
  pop_assum kall_tac >>
  `b ≠ 0` by decide_tac >>
  drule mul_div_bound >> disch_then (qspec_then `n` mp_tac) >>
  decide_tac
QED

Theorem log_base_power:
  ∀n b. 1 < b ⇒ log b (b ** n) = n
Proof
  Induct >> rw [EXP, LOG_1] >>
  Cases_on `n` >> rw [LOG_BASE] >>
  first_x_assum drule >> rw [] >>
  simp [Once EXP, LOG_MULT]
QED

Theorem log_change_base_power:
  ∀m n b. 1 < b ∧ m ≠ 0 ∧ n ≠ 0 ⇒ log (b ** n) m = log b m DIV n
Proof
  rw [] >> irule LOG_UNIQUE >>
  rw [ADD1, EXP_MUL, LEFT_ADD_DISTRIB] >>
  qmatch_goalsub_abbrev_tac `x DIV _` >>
  drule mul_div_bound >> disch_then (qspec_then `x` mp_tac) >> rw []
  >- (
    irule LESS_LESS_EQ_TRANS >>
    qexists_tac `b ** (x+1)` >> rw [] >>
    unabbrev_all_tac >>
    simp [EXP_ADD] >>
    `b * (m DIV b + 1) ≤ b * b ** log b m`
    by metis_tac [exp_log_bound, LESS_MONO_MULT, MULT_COMM] >>
    `m < b * (m DIV b + 1)` suffices_by decide_tac >>
    simp [LEFT_ADD_DISTRIB] >>
    `b ≠ 0` by decide_tac >>
    `m - (b - 1) ≤ b * (m DIV b)` by metis_tac [mul_div_bound] >>
    fs [])
  >- (
    irule LESS_EQ_TRANS >>
    qexists_tac `b ** (log b m)` >> rw [] >>
    unabbrev_all_tac >>
    metis_tac [exp_log_bound])
QED

(* ----- Theorems about word stuff ----- *)

Theorem l2n_padding:
  ∀ws n. l2n 256 (ws ++ map w2n (replicate n 0w)) = l2n 256 ws
Proof
  Induct >> rw [l2n_def] >>
  Induct_on `n` >> rw [l2n_def]
QED

Theorem l2n_0:
  ∀l b. b ≠ 0 ∧ every ($> b)  l⇒ (l2n b l = 0 ⇔ every ($= 0) l)
Proof
  Induct >> rw [l2n_def] >>
  eq_tac >> rw []
QED

Theorem mod_n2l:
  ∀d n. 0 < d ⇒ map (λx. x MOD d) (n2l d n) = n2l d n
Proof
  rw [] >> drule n2l_BOUND >> disch_then (qspec_then `n` mp_tac) >>
  qspec_tac (`n2l d n`, `l`) >>
  Induct >> rw []
QED

Definition truncate_2comp_def:
  truncate_2comp (i:int) size =
    (i + 2 ** (size - 1)) % 2 ** size - 2 ** (size - 1)
End

Theorem truncate_2comp_i2w_w2i:
  ∀i size. dimindex (:'a) = size ⇒ truncate_2comp i size = w2i (i2w i : 'a word)
Proof
  rw [truncate_2comp_def, w2i_def, word_msb_i2w, w2n_i2w] >>
  qmatch_goalsub_abbrev_tac `(_ + s1) % s2` >>
  `2 * s1 = s2` by rw [Abbr `s1`, Abbr `s2`, GSYM EXP, DIMINDEX_GT_0] >>
  `0 ≠ s2 ∧ ¬(s2 < 0)` by rw [Abbr `s2`] >>
  fs [MULT_MINUS_ONE, w2n_i2w] >>
  fs [GSYM dimword_def, dimword_IS_TWICE_INT_MIN]
  >- (
    `-i % s2 = -((i + s1) % s2 - s1)` suffices_by intLib.COOPER_TAC >>
    simp [] >>
    irule INT_MOD_UNIQUE >>
    simp [GSYM PULL_EXISTS] >>
    conj_tac
    >- (
      simp [int_mod, INT_ADD_ASSOC,
            intLib.COOPER_PROVE ``∀x y (z:int). x - (y + z - a) = x - y - z + a``] >>
      qexists_tac `-((i + s1) / s2)` >>
      intLib.COOPER_TAC) >>
    `&INT_MIN (:α) = s1` by (unabbrev_all_tac >> rw [INT_MIN_def]) >>
    fs [INT_SUB_LE] >>
    `0 ≤ (i + s1) % s2` by metis_tac [INT_MOD_BOUNDS] >>
    strip_tac
    >- (
      `(i + s1) % s2 = (i % s2 + s1 % s2) % s2`
      by (irule (GSYM INT_MOD_PLUS) >> rw []) >>
      simp [] >>
      `(i % s2 + s1 % s2) % s2 = (-1 * s2 + (i % s2 + s1 % s2)) % s2`
      by (metis_tac [INT_MOD_ADD_MULTIPLES]) >>
      simp [GSYM INT_NEG_MINUS1, INT_ADD_ASSOC] >>
      `i % s2 < s2 ∧ s1 % s2 < s2 ∧ i % s2 ≤ s2` by metis_tac [INT_MOD_BOUNDS, INT_LT_IMP_LE] >>
      `0 ≤ s1 ∧ s1 < s2 ∧ -s2 + i % s2 + s1 % s2 < s2` by intLib.COOPER_TAC >>
      `0 ≤ -s2 + i % s2 + s1 % s2`
      by (
        `s2 = s1 + s1` by intLib.COOPER_TAC >>
        fs [INT_LESS_MOD] >>
        intLib.COOPER_TAC) >>
      simp [INT_LESS_MOD] >>
      intLib.COOPER_TAC)
    >- intLib.COOPER_TAC)
  >- (
    `(i + s1) % s2 = i % s2 + s1` suffices_by intLib.COOPER_TAC >>
    `(i + s1) % s2 = i % s2 + s1 % s2`
    suffices_by (
      rw [] >>
      irule INT_LESS_MOD >> rw [] >>
      intLib.COOPER_TAC) >>
    `(i + s1) % s2 = (i % s2 + s1 % s2) % s2`
    suffices_by (
      fs [Abbr `s2`] >>
      `s1 = &INT_MIN (:'a)` by intLib.COOPER_TAC >> rw [] >>
      irule INT_LESS_MOD >> rw [] >>
      fs [intLib.COOPER_PROVE ``∀(x:int) y. ¬(x ≤ y) ⇔ y < x``] >> rw [] >>
      full_simp_tac std_ss [GSYM INT_MUL] >>
      qpat_abbrev_tac `s = &INT_MIN (:α)`
      >- (
        `2*s ≠ 0 ∧ ¬(2*s < 0) ∧ ¬(s < 0)`
        by (unabbrev_all_tac >> rw []) >>
        drule INT_MOD_BOUNDS >> simp [] >>
        disch_then (qspec_then `i` mp_tac) >> simp [] >>
        intLib.COOPER_TAC)
      >- intLib.COOPER_TAC) >>
    simp [INT_MOD_PLUS])
QED

(* The integer, interpreted as 2's complement, fits in the given number of bits *)
Definition ifits_def:
  ifits (i:int) size ⇔
    0 < size ∧ -(2 ** (size - 1)) ≤ i ∧ i < 2 ** (size - 1)
End
Theorem ifits_w2i:
  ∀(w : 'a word). ifits (w2i w) (dimindex (:'a))
Proof
  rw [ifits_def, GSYM INT_MIN_def] >>
  metis_tac [INT_MIN, w2i_ge, integer_wordTheory.INT_MAX_def, w2i_le,
             intLib.COOPER_PROVE ``!(x:int) y. x ≤ y - 1 ⇔ x < y``]
QED

Theorem truncate_2comp_fits:
  ∀i size. 0 < size ⇒ ifits (truncate_2comp i size) size
Proof
  rw [truncate_2comp_def, ifits_def] >>
  qmatch_goalsub_abbrev_tac `(i + s1) % s2` >>
  `s2 ≠ 0 ∧ ¬(s2 < 0)` by rw [Abbr `s2`]
  >- (
    `0 ≤ (i + s1) % s2` suffices_by intLib.COOPER_TAC >>
    drule INT_MOD_BOUNDS >>
    rw [])
  >- (
    `(i + s1) % s2 < 2 * s1` suffices_by intLib.COOPER_TAC >>
    `2 * s1 = s2` by rw [Abbr `s1`, Abbr `s2`, GSYM EXP] >>
    drule INT_MOD_BOUNDS >>
    rw [Abbr `s1`, Abbr `s2`])
QED

Theorem ifits_mono:
  ∀i s1 s2. s1 ≤ s2 ∧ ifits i s1 ⇒ ifits i s2
Proof
  rw [ifits_def]
  >- (
    `&(2 ** (s1 − 1)) ≤ &(2 ** (s2 − 1))` suffices_by intLib.COOPER_TAC >>
    rw [])
  >- (
    `&(2 ** (s1 − 1)) ≤ &(2 ** (s2 − 1))` suffices_by intLib.COOPER_TAC >>
    rw [])
QED

Theorem fits_ident:
  ∀i size. 0 < size ⇒ (ifits i size ⇔ truncate_2comp i size = i)
Proof
  rw [ifits_def, truncate_2comp_def] >>
  rw [intLib.COOPER_PROVE ``!(x:int) y z. x - y = z <=> x = y + z``] >>
  qmatch_goalsub_abbrev_tac `(_ + s1) % s2` >>
  `s2 ≠ 0 ∧ ¬(s2 < 0)` by rw [Abbr `s2`] >>
  `2 * s1 = s2` by rw [Abbr `s1`, Abbr `s2`, GSYM EXP] >>
  eq_tac >>
  rw []
  >- (
    simp [Once INT_ADD_COMM] >>
    irule INT_LESS_MOD >>
    rw [] >>
    intLib.COOPER_TAC)
  >- (
   `0 ≤ (i + s1) % (2 * s1)` suffices_by intLib.COOPER_TAC >>
    drule INT_MOD_BOUNDS >>
    simp [])
  >- (
   `(i + s1) % (2 * s1) < 2 * s1` suffices_by intLib.COOPER_TAC >>
    drule INT_MOD_BOUNDS >>
    simp [])
QED

Theorem i2w_w2i_extend:
  i2w (w2i (w : 'a word)) : 'b word =
    if ¬word_msb w then
      w2w w
    else
      -w2w (-w)
Proof
  rw [i2w_def, w2i_def] >>
  BasicProvers.FULL_CASE_TAC >> fs [] >>
  fs [] >>
  full_simp_tac std_ss [GSYM WORD_NEG_MUL] >>
  full_simp_tac std_ss [w2w_def]
QED

Theorem w21_sw2sw_extend:
  dimindex (:'b) ≤ dimindex (:'a)
  ⇒
  w2i (sw2sw (w :'b word) :'a word) = w2i (w : 'b word)
Proof
  rw [] >>
  `∃j. INT_MIN (:'b) ≤ j ∧ j ≤ INT_MAX (:'b) ∧ w = i2w j` by metis_tac [ranged_int_word_nchotomy] >>
  rw [sw2sw_i2w] >>
  `INT_MIN (:'a) ≤ j ∧ j ≤ INT_MAX (:'a)`
  by (
    fs [INT_MIN_def, INT_MAX_def] >>
    `2 ** (dimindex (:'b) - 1) ≤ 2 ** (dimindex (:'a) - 1)`
    by rw [EXP_BASE_LE_IFF] >>
    qmatch_assum_abbrev_tac `b ≤ a` >>
    rw []
    >- intLib.COOPER_TAC >>
    `&(b - 1) ≤ &(a - 1)` by intLib.COOPER_TAC >>
    full_simp_tac bool_ss [GSYM INT_LE] >>
    intLib.COOPER_TAC) >>
  rw [w2i_i2w]
QED

Theorem sw2sw_trunc:
  dimindex (:'a) ≤ dimindex (:'b)
  ⇒
  (sw2sw (w :'b word) :'a word) = (w2w (w :'b word) :'a word)
Proof
  rw [sw2sw_def, w2w_def, bitTheory.SIGN_EXTEND_def] >>
  fs [dimword_def] >>
  qmatch_assum_abbrev_tac `a ≤ b` >>
  `2 ** a ≤ 2 ** b` by rw [EXP_BASE_LE_IFF] >>
  `2 ** a - 2 ** b = 0` by rw [] >>
  rw [] >>
  `∃x. b = a + x` by (qexists_tac `b - a` >> decide_tac) >>
  rw [EXP_ADD] >>
  metis_tac [MOD_MULT_MOD, bitTheory.ZERO_LT_TWOEXP]
QED

Theorem w2i_w2w_expand:
  dimindex (:'a) < dimindex (:'b) ⇒ w2i (w2w (w : 'a word) : 'b word) = &w2n w
Proof
  rw [w2i_def, w2n_w2w, word_msb, word_bit_thm, w2w]
QED

(* ----- Theorems about lazy lists ----- *)

Theorem toList_some:
  ∀ll l. toList ll = Some l ⇔ ll = fromList l
Proof
  Induct_on `l` >> rw [] >>
  Cases_on `ll` >> rw [toList_THM] >>
  metis_tac []
QED

Theorem lmap_fromList:
  !f l. LMAP f (fromList l) = fromList (map f l)
Proof
  Induct_on `l` >> rw []
QED

Theorem fromList_11[simp]:
  !l1 l2. fromList l1 = fromList l2 ⇔ l1 = l2
Proof
  Induct >> rw [] >>
  Cases_on `l2` >> fs []
QED

(* ----- Theorems about labelled transition system paths ----- *)

Theorem take_all:
  ∀p n. length p = Some n ⇒ take (n - 1) p = p
Proof
  Induct_on `n` >> rw []
  >- metis_tac [length_never_zero] >>
  qspec_then `p` mp_tac path_cases >> rw [] >> fs [alt_length_thm] >>
  first_x_assum drule >> rw [] >>
  Cases_on `n` >> fs [length_never_zero]
QED

Theorem el_plink:
  ∀n p1 p2.
    n ∈ PL (plink p1 p2) ∧ last p1 = first p2 ⇒
    el n (plink p1 p2) = (if n ∈ PL p1 then el n p1 else el (Suc n - THE (length p1)) p2)
Proof
  Induct_on `n` >> rw [first_plink] >>
  qspec_then `p1` mp_tac path_cases >> rw [] >> fs [] >>
  rw [alt_length_thm] >>
  first_x_assum drule >> rw [] >>
  Cases_on `length q` >> fs [PL_def, length_def]
QED

Theorem el_pcons:
  ∀n x l p. el n (pcons x l p) = if n = 0 then x else el (n - 1) p
Proof
  Induct_on `n` >>
  rw []
QED

Theorem first_pconcat[simp]:
  ∀p1 l p2. first (pconcat p1 l p2) = first p1
Proof
  rw [] >> qspec_then `p1` mp_tac path_cases >> rw [] >> rw [pconcat_thm]
QED

Theorem el_pconcat:
  ∀n p1 l p2.
    n ∈ PL (pconcat p1 l p2) ⇒
    el n (pconcat p1 l p2) = (if n ∈ PL p1 then el n p1 else el (n - THE (length p1)) p2)
Proof
  Induct_on `n` >> rw [] >>
  qspec_then `p1` mp_tac path_cases >> rw [] >> fs [pconcat_thm] >>
  rw [alt_length_thm] >>
  first_x_assum drule >> rw [] >>
  Cases_on `length q` >> fs [PL_def, length_def]
QED

Theorem labels_pconcat[simp]:
  ∀p1 l p2. labels (pconcat p1 l p2) = LAPPEND (labels p1) (l:::labels p2)
Proof
  rw [pconcat_def, labels_LMAP, path_rep_bijections_thm, LMAP_APPEND]
QED

Theorem length_pconcat:
  ∀p1 l p2 l1 l2.
    length p1 = Some l1 ∧ length p2 = Some l2
    ⇒
    length (pconcat p1 l p2) = Some (l1 + l2)
Proof
  rw [pconcat_def, length_def, path_rep_bijections_thm, finite_def,
      LFINITE_APPEND] >>
  rw [] >>
  `LFINITE (LAPPEND (snd (fromPath p1)) ((l,first p2):::snd (fromPath p2)))`
  by rw [LFINITE_APPEND] >>
  imp_res_tac LFINITE_toList >> rw [] >>
  imp_res_tac toList_LAPPEND_APPEND >> fs [toList_THM]
QED

Theorem take_pconcat:
  ∀n p1 l p2.
    take n (pconcat p1 l p2) =
      if n ∈ PL p1 then
        take n p1
      else
        pconcat p1 l (take (n - THE (length p1)) p2)
Proof
  Induct_on `n` >> rw []
  >- (
    fs [PL_def] >>
    qspec_then `p1` mp_tac path_cases >> rw [] >> rw [pconcat_thm] >>
    fs [finite_def, alt_length_thm])
  >- (
    qspec_then `p1` mp_tac path_cases >> rw [] >> rw [pconcat_thm] >>
    fs [PL_def])
  >- (
    qspec_then `p1` mp_tac path_cases >> rw [] >> rw [pconcat_thm] >>
    fs [PL_def, alt_length_thm, finite_length])
QED

Theorem last_pconcat[simp]:
  ∀p1. finite p1 ⇒ ∀l p2. last (pconcat p1 l p2) = last p2
Proof
  ho_match_mp_tac finite_path_ind >>
  rw [pconcat_thm]
QED

Theorem length_labels:
  ∀p n. length p = Some (Suc n) ⇔ LLENGTH (labels p) = Some n
Proof
  Induct_on `n` >> rw [] >>
  qspec_then `p` mp_tac path_cases >> rw [] >> rw [alt_length_thm, length_never_zero]
QED

Theorem ltake_fromList2:
  ∀n l. n ≤ length l ⇒ LTAKE n (fromList l) = Some (take n l)
Proof
  Induct_on `l` >> rw [] >>
  Cases_on `n` >> fs []
QED

Theorem el_take:
  ∀p m n. n ∈ PL p ∧ m ≤ n ⇒ el m (take n p) = el m p
Proof
  Induct_on `n` >> rw [] >> rw [el_pcons] >>
  first_x_assum (qspecl_then [`tail p`, `m-1`] mp_tac) >>
  impl_tac
  >- (
    fs [PL_def] >> rw [] >>
    qspec_then `p` mp_tac path_cases >> rw [] >> fs [alt_length_thm] >>
    fs [finite_length] >> fs []) >>
  rw [] >>
  Cases_on `m` >> rw []
QED

Theorem nth_label_pcons:
  (∀n s l p. nth_label 0 (pcons s l p) = l) ∧
  (∀n s l p. nth_label (Suc n) (pcons s l p) = nth_label n p)
Proof
  rw []
QED

Theorem okpath_pointwise_imp1:
  ∀p. (∀n. Suc n ∈ PL p ⇒ r (el n p) (nth_label n p) (el (Suc n) p)) ⇒ okpath r p
Proof
  ho_match_mp_tac okpath_co_ind >> rw [] >>
  qspec_then `p` mp_tac path_cases >> rw [] >> rw [first_thm] >>
  fs [PL_def]
  >- (first_x_assum (qspec_then `0` mp_tac) >> rw []) >>
  rw [el_pcons]
  >- (first_x_assum (qspec_then `1` mp_tac) >> rw [] >> fs [el_pcons, nth_label_compute])
  >- (
    first_x_assum (qspec_then `Suc n` mp_tac) >> rw [] >>
    Cases_on `n` >> fs [])
QED

Theorem okpath_pointwise_imp2:
  ∀p. okpath r p ∧ finite p ⇒ (∀n. Suc n ∈ PL p ⇒ r (el n p) (nth_label n p) (el (Suc n) p))
Proof
  ho_match_mp_tac finite_okpath_ind >> rw [] >>
  Cases_on `n` >> fs []
QED

Theorem okpath_pointwise:
  ∀r p. okpath r p ⇔ (∀n. Suc n ∈ PL p ⇒ r (el n p) (nth_label n p) (el (Suc n) p))
Proof
  rw [] >> eq_tac >> rw [okpath_pointwise_imp1] >>
  `okpath r (take (Suc n) p)` by metis_tac [okpath_take] >>
  `finite (take (Suc n) p)` by metis_tac [finite_take] >>
  drule okpath_pointwise_imp2 >> simp [] >>
  disch_then (qspec_then `n` mp_tac) >> simp [el_pcons] >>
  Cases_on `n = 0` >> simp [] >>
  `n ∈ PL (tail p)`
  by (
    fs [PL_def] >>
    qspec_then `p` mp_tac path_cases >> rw [] >> rw [first_thm] >>
    fs [alt_length_thm] >> fs [finite_length] >> fs []) >>
  simp [el_take] >>
  `el (n - 1) (tail p) = el n p` by (Cases_on `n` >> rw []) >>
  simp [] >>
  `∃m. n = Suc m` by intLib.COOPER_TAC >>
  `Suc m ∈ PL (tail p)` by fs [PL_def] >>
  ASM_REWRITE_TAC [nth_label_pcons] >>
  simp [nth_label_take]
QED

Theorem length_plink:
  ∀p1 p2 l1 l2.
    length p1 = Some l1 ∧ length p2 = Some l2
    ⇒
    length (plink p1 p2) = Some (l1 + l2 - 1)
Proof
  Induct_on `l1` >> rw [] >> fs [length_never_zero] >>
  qspec_then `p1` mp_tac path_cases >> rw [plink_def] >>
  fs [alt_length_thm] >> res_tac >> fs [ADD1] >>
  `l1 ≠ 0` by metis_tac [length_never_zero] >>
  decide_tac
QED

Theorem take_plink:
  ∀n p1 p2.
    take n (plink p1 p2) =
      if Suc n ∈ PL p1 then
        take n p1
      else
        plink p1 (take ((Suc n) - THE (length p1)) p2)
Proof
  Induct_on `n` >> rw []
  >- (
    fs [PL_def] >>
    qspec_then `p1` mp_tac path_cases >> rw [] >>
    fs [finite_def, alt_length_thm])
  >- (
    fs [PL_def] >>
    qspec_then `p1` mp_tac path_cases >> rw []>> rw [plink_def] >>
    fs [finite_length, alt_length_thm] >> rfs [] >>
    Cases_on `n` >> fs [length_never_zero])
  >- (
    qspec_then `p1` mp_tac path_cases >> rw []>> rw [plink_def] >>
    fs [PL_def])
  >- (
    qspec_then `p1` mp_tac path_cases >> rw []>> rw [plink_def] >>
    fs [PL_def])
  >- (
    qspec_then `p1` mp_tac path_cases >> rw [] >> rw [] >>
    fs [PL_def, alt_length_thm])
  >- (
    qspec_then `p1` mp_tac path_cases >> rw [] >> fs [alt_length_thm] >>
    `finite q` by fs [PL_def] >>
    fs [finite_length])
QED

Theorem unfold_last_lem:
  ∀path. finite path ⇒
    ∀proj f s. path = unfold proj f s ⇒
    ∃y. proj y = last path ∧ f y = None ∧ (1 ∈ PL path ⇒ ∃x l. f x = Some (y, l))
Proof
  ho_match_mp_tac finite_path_ind >> rw []
  >- (
    fs [Once unfold_thm] >> Cases_on `f s` >> fs []
    >- metis_tac [] >>
    split_pair_case_tac >> fs []) >>
  pop_assum mp_tac >> simp [Once unfold_thm] >> Cases_on `f s` >> simp [] >>
  split_pair_case_tac >> rw [] >>
  first_x_assum (qspecl_then [`proj`, `f`, `s'`] mp_tac) >> simp [] >>
  Cases_on `1 ∈ PL (unfold proj f s')` >> rw [] >>
  fs [PL_def] >>
  fs [Once unfold_thm] >>
  Cases_on `f s'` >> fs [alt_length_thm] >> rw [] >-
  metis_tac [] >>
  split_pair_case_tac >> fs [] >> rw [] >> fs [alt_length_thm, finite_length] >>
  rfs [] >>
   `n = 0 ∨ n = 1` by decide_tac >> fs [length_never_zero]
QED

Theorem unfold_last:
  ∀proj f s.
    finite (unfold proj f s)
    ⇒
    ∃y. proj y = last (unfold proj f s) ∧ f y = None ∧
    (1 ∈ PL (unfold proj f s) ⇒ ∃x l. f x = Some (y, l))
Proof
  metis_tac [unfold_last_lem]
QED

Theorem pconcat_to_plink_finite:
  ∀p1. finite p1 ⇒ ∀l p2. pconcat p1 l p2 = plink p1 (pcons (last p1) l p2)
Proof
  ho_match_mp_tac finite_path_ind >> rw [pconcat_thm]
QED

Definition opt_funpow_def:
  (opt_funpow f 0 x = Some x) ∧
  (opt_funpow f (Suc n) x = option_join (option_map f (opt_funpow f n x)))
End

Theorem opt_funpow_alt:
  ∀n f s.
    opt_funpow f (Suc n) s = option_join (option_map (opt_funpow f n) (f s))
Proof
  Induct_on `n` >> rw [] >> Cases_on `f s` >> rw [] >>
  `1 = Suc 0` by decide_tac >>
  ASM_REWRITE_TAC [] >>
  rw [opt_funpow_def] >>
  fs [opt_funpow_def]
QED

Theorem unfold_finite_funpow_lem:
  ∀f proj s x.
    opt_funpow (option_map fst ∘ f) m s = Some x ∧ f x = None
    ⇒
    finite (unfold proj f s)
Proof
  Induct_on `m` >> rw [opt_funpow_def] >>
  simp [Once unfold_thm] >>
  CASE_TAC >> fs [] >> split_pair_case_tac >> fs [] >> rw [] >>
  Cases_on `opt_funpow (option_map fst ∘ f) m s` >> rw [] >>
  fs [optionTheory.OPTION_MAP_DEF] >>
  first_x_assum irule >> qexists_tac `x` >> rw [] >>
  `opt_funpow (option_map fst ∘ f) (Suc m) s = Some (fst z)` by fs [opt_funpow_def] >>
  rfs [opt_funpow_alt]
QED

Theorem unfold_finite_funpow:
  ∀f proj s m.
    opt_funpow (option_map fst ∘ f) m s = None
    ⇒
    finite (unfold proj f s)
Proof
  rw [] >> irule unfold_finite_funpow_lem >>
  Induct_on `m` >> rw [] >> fs [opt_funpow_def] >>
  Cases_on `opt_funpow (option_map fst ∘ f) m s` >> fs [] >>
  metis_tac []
QED

Theorem unfold_finite:
  ∀proj f s.
    (∃R. WF R ∧ ∀n s2 l s3. opt_funpow (option_map fst o f) n s = Some s2 ∧ f s2 = Some (s3, l) ⇒ R s3 s2)
    ⇒
    finite (unfold proj f s)
Proof
  rw [] >> drule relationTheory.WF_INDUCTION_THM >>
  disch_then (qspecl_then [`λx. ∀n. opt_funpow (option_map fst o f) n s = Some x ⇒
                                    ∃m. opt_funpow (option_map fst o f) m x = None`,
                           `s`] mp_tac) >>
  simp [] >>
  impl_tac
  >- (
    rw [] >>
    first_x_assum drule >> Cases_on `f x` >> simp []
    >- (qexists_tac `Suc n` >> simp [opt_funpow_alt]) >>
    PairCases_on `x'` >> rw [] >>
    first_x_assum drule >> rw [] >>
    first_x_assum (qspec_then `Suc n` mp_tac) >> simp [opt_funpow_def] >>
    rw [] >>
    qexists_tac `Suc m` >> rw [opt_funpow_alt]) >>
  metis_tac [unfold_finite_funpow, opt_funpow_def]
QED

(* ----- pred_set theorems ----- *)

Theorem drestrict_union_eq:
  !m1 m2 s1 s2.
    DRESTRICT m1 (s1 ∪ s2) = DRESTRICT m2 (s1 ∪ s2)
    ⇔
    DRESTRICT m1 s1 = DRESTRICT m2 s1 ∧
    DRESTRICT m1 s2 = DRESTRICT m2 s2
Proof
  rw [DRESTRICT_EQ_DRESTRICT_SAME] >> eq_tac >> rw [] >> fs [EXTENSION] >>
  metis_tac []
QED

(* ----- finite map theorems ----- *)

Theorem drestrict_fupdate_list[simp]:
  ∀l m s. DRESTRICT (m |++ l) s = DRESTRICT m s |++ filter (\(x,y). x ∈ s) l
Proof
  Induct_on `l` >> rw [FUPDATE_LIST_THM] >> pairarg_tac >> fs []
QED

export_theory ();
