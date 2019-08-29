(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Misc. theorems that aren't specific to the semantics of LLVM or Sledge. These
 * could be upstreamed to HOL, and should eventually. *)

open HolKernel boolLib bossLib Parse;
open listTheory rich_listTheory arithmeticTheory wordsTheory;
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

export_theory ();
