(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Misc. theorems that aren't specific to the semantics of LLVM or Sledge. These
 * could be upstreamed to HOL, and should eventually. *)

open HolKernel boolLib bossLib Parse;
open listTheory rich_listTheory arithmeticTheory integerTheory;
open integer_wordTheory wordsTheory;
open finite_mapTheory open logrootTheory numposrepTheory;
open settingsTheory;

new_theory "misc";

numLib.prefer_num ();

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
  ∀d n. 0 < d ⇒ map (\x. x MOD d) (n2l d n) = n2l d n
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
            intLib.COOPER_PROVE ``!x y (z:int). x - (y + z - a) = x - y - z + a``] >>
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
      fs [intLib.COOPER_PROVE ``!(x:int) y. ¬(x ≤ y) ⇔ y < x``] >> rw [] >>
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

export_theory ();
