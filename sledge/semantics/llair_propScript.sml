(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Properties of the llair model *)

open HolKernel boolLib bossLib Parse;
open arithmeticTheory integerTheory integer_wordTheory wordsTheory;
open settingsTheory miscTheory llairTheory;

new_theory "llair_prop";

numLib.prefer_num ();

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

Theorem i2n_n2i:
  !n size. 0 < size ⇒ (nfits n size ⇔ (i2n (n2i n size) = n))
Proof
  rw [nfits_def, n2i_def, i2n_def] >> rw []
  >- intLib.COOPER_TAC
  >- (
    `2 ** size ≤ n` by intLib.COOPER_TAC >> simp [INT_SUB] >>
    Cases_on `n = 0` >> fs [] >>
    `n - 2 ** size < n` suffices_by intLib.COOPER_TAC >>
    irule SUB_LESS >> simp [])
  >- (
    `2 ** (size - 1) < 2 ** size` suffices_by intLib.COOPER_TAC >>
    fs [])
QED

Theorem n2i_i2n:
  !i size. 0 < size ⇒ (ifits i size ⇔ (n2i (i2n (IntV i size)) size) = IntV i size)
Proof
  rw [ifits_def, n2i_def, i2n_def] >> rw [] >> fs []
  >- (
    eq_tac >> rw []
    >- (
      simp [intLib.COOPER_PROVE ``∀(x:int) y z. x - y = z ⇔ x = y + z``] >>
      `2 ** (size - 1) < 2 ** size` suffices_by intLib.COOPER_TAC >>
      fs [INT_OF_NUM])
    >- (
      fs [intLib.COOPER_PROVE ``∀(x:int) y z. x - y = z ⇔ x = y + z``] >>
      fs [INT_OF_NUM] >>
      `?j. i = -j` by intLib.COOPER_TAC >> rw [] >> fs [] >>
      qpat_x_assum `_ ≤ Num _` mp_tac >>
      fs [GSYM INT_OF_NUM] >>
      ASM_REWRITE_TAC [GSYM INT_LE] >> rw [] >>
      `2 ** size = 2 * 2 ** (size - 1)` by rw [GSYM EXP, ADD1] >> fs [] >>
      intLib.COOPER_TAC)
    >- intLib.COOPER_TAC)
  >- (
    eq_tac >> rw []
    >- intLib.COOPER_TAC
    >- intLib.COOPER_TAC >>
    `0 ≤ i` by intLib.COOPER_TAC >>
    fs [GSYM INT_OF_NUM] >>
    `&(2 ** size) = 0` by intLib.COOPER_TAC >>
    fs [])
  >- (
    eq_tac >> rw []
    >- (
      `2 ** size = 2 * 2 ** (size - 1)` by rw [GSYM EXP, ADD1] >> fs [] >>
      intLib.COOPER_TAC)
    >- intLib.COOPER_TAC
    >- intLib.COOPER_TAC)
  >- intLib.COOPER_TAC
QED

Theorem w2n_i2n:
  ∀w. w2n (w : 'a word) = i2n (IntV (w2i w) (dimindex (:'a)))
Proof
  rw [i2n_def] >> Cases_on `w` >> fs []
  >- (
    `INT_MIN (:α) ≤ n`
    by (
      fs [w2i_def] >> rw [] >>
      BasicProvers.EVERY_CASE_TAC >> fs [word_msb_n2w_numeric] >>
      rfs []) >>
    rw [w2i_n2w_neg, dimword_def, int_arithTheory.INT_NUM_SUB])
  >- (
    `n < INT_MIN (:'a)`
    by (
      fs [w2i_def] >> rw [] >>
      BasicProvers.EVERY_CASE_TAC >> fs [word_msb_n2w_numeric] >>
      rfs []) >>
    rw [w2i_n2w_pos])
QED

export_theory ();
