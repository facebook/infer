(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Properties of the llair model *)

open HolKernel boolLib bossLib Parse;
open arithmeticTheory integerTheory integer_wordTheory wordsTheory listTheory;
open pred_setTheory finite_mapTheory;
open settingsTheory miscTheory llairTheory;

new_theory "llair_prop";

numLib.prefer_num ();

Theorem signed2unsigned_fits:
  0 < n ∧ ifits i n ⇒ ifits (&signed2unsigned i n) (n + 1)
Proof
  rw [signed2unsigned_def, ifits_def]
  >- (
    `?j. i = -&j` by intLib.COOPER_TAC >>
    rw [] >> fs [] >>
    rfs [EXP_SUB] >>
    `j ≤ 2 ** n` by intLib.COOPER_TAC >>
    rw [INT_SUB, GSYM int_sub])
  >- (
    `?j. i = &j` by intLib.COOPER_TAC >>
    rw [] >> fs [] >>
    rw [INT_SUB, GSYM int_sub] >>
    rfs [EXP_SUB] >>
    intLib.COOPER_TAC)
QED

Theorem i2n_n2i:
  ∀n size. 0 < size ⇒ (nfits n size ⇔ (i2n (n2i n size) = n))
Proof
  rw [nfits_def, n2i_def, i2n_def, signed2unsigned_def] >> rw []
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
  ∀i size. 0 < size ⇒ (ifits i size ⇔ (n2i (i2n (IntV i size)) size) = IntV i size)
Proof
  rw [ifits_def, n2i_def, i2n_def, signed2unsigned_def] >> rw [] >> fs []
  >- (
    eq_tac >> rw []
    >- (
      simp [intLib.COOPER_PROVE ``∀(x:int) y z. x - y = z ⇔ x = y + z``] >>
      `2 ** (size - 1) < 2 ** size` suffices_by intLib.COOPER_TAC >>
      fs [INT_OF_NUM])
    >- (
      fs [intLib.COOPER_PROVE ``∀(x:int) y z. x - y = z ⇔ x = y + z``] >>
      fs [INT_OF_NUM] >>
      `∃j. i = -j` by intLib.COOPER_TAC >> rw [] >> fs [] >>
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

Theorem w2n_signed2unsigned:
  ∀w. w2n (w : 'a word) = signed2unsigned (w2i w) (dimindex (:'a))
Proof
  rw [signed2unsigned_def] >> Cases_on `w` >> fs []
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

Theorem w2n_i2n:
  ∀w. w2n (w : 'a word) = i2n (IntV (w2i w) (dimindex (:'a)))
Proof
  rw [i2n_def] >> metis_tac [w2n_signed2unsigned]
QED

Theorem w2i_n2w:
  ∀n. n < dimword (:'a) ⇒ IntV (w2i (n2w n : 'a word)) (dimindex (:'a)) = n2i n (dimindex (:'a))
Proof
  rw [n2i_def]
  >- (
    qspec_then `n` mp_tac w2i_n2w_neg >>
    fs [dimword_def, INT_MIN_def] >> rw [GSYM INT_SUB])
  >- (irule w2i_n2w_pos >> rw [INT_MIN_def])
QED

Theorem eval_exp_ignores_lem:
  ∀s1 e v. eval_exp s1 e v ⇒ ∀s2. s1.locals = s2.locals ⇒ eval_exp s2 e v
Proof
  ho_match_mp_tac eval_exp_ind >>
  rw [] >> simp [Once eval_exp_cases] >>
  TRY (qexists_tac `vals` >> rw [] >> fs [LIST_REL_EL_EQN] >> NO_TAC) >>
  TRY (fs [LIST_REL_EL_EQN] >> NO_TAC) >>
  metis_tac []
QED

Theorem eval_exp_ignores:
  ∀s1 e v s2. s1.locals = s2.locals ⇒ (eval_exp s1 e v ⇔ eval_exp s2 e v)
Proof
  metis_tac [eval_exp_ignores_lem]
QED

Definition exp_uses_def:
  (exp_uses (Var x) = {x}) ∧
  (exp_uses Nondet = {}) ∧
  (exp_uses (Label _) = {}) ∧
  (exp_uses (Splat e1 e2) = exp_uses e1 ∪ exp_uses e2) ∧
  (exp_uses (Memory e1 e2) = exp_uses e1 ∪ exp_uses e2) ∧
  (exp_uses (Concat es) = bigunion (set (map exp_uses es))) ∧
  (exp_uses (Integer _ _) = {}) ∧
  (exp_uses (Eq e1 e2) = exp_uses e1 ∪ exp_uses e2) ∧
  (exp_uses (Lt e1 e2) = exp_uses e1 ∪ exp_uses e2) ∧
  (exp_uses (Ult e1 e2) = exp_uses e1 ∪ exp_uses e2) ∧
  (exp_uses (Sub _ e1 e2) = exp_uses e1 ∪ exp_uses e2) ∧
  (exp_uses (Record es) = bigunion (set (map exp_uses es))) ∧
  (exp_uses (Select e1 e2) = exp_uses e1 ∪ exp_uses e2) ∧
  (exp_uses (Update e1 e2 e3) = exp_uses e1 ∪ exp_uses e2 ∪ exp_uses e3) ∧
  (exp_uses (Unsigned _ e _) = exp_uses e) ∧
  (exp_uses (Signed _ e _) = exp_uses e)
Termination
  WF_REL_TAC `measure exp_size` >> rw [] >>
  Induct_on `es` >> rw [exp_size_def] >> res_tac >> rw []
End

Theorem eval_exp_ignores_unused_lem:
  ∀s1 e v.
    eval_exp s1 e v ⇒
    ∀s2. DRESTRICT s1.locals (exp_uses e) = DRESTRICT s2.locals (exp_uses e) ⇒
    eval_exp s2 e v
Proof
  ho_match_mp_tac eval_exp_ind >>
  rw [exp_uses_def] >> simp [Once eval_exp_cases]
  >- (
    fs [DRESTRICT_EQ_DRESTRICT, EXTENSION, FDOM_DRESTRICT] >>
    imp_res_tac FLOOKUP_SUBMAP >>
    fs [FLOOKUP_DRESTRICT]) >>
  fs [drestrict_union_eq]
  >- metis_tac []
  >- metis_tac []
  >- (
    rpt (pop_assum mp_tac) >>
    qid_spec_tac `vals` >>
    Induct_on `es` >> rw [] >> Cases_on `vals` >> rw [PULL_EXISTS] >> fs [] >>
    rw [] >> fs [drestrict_union_eq] >>
    rename [`v1++flat vs`] >>
    first_x_assum (qspec_then `vs` mp_tac) >> rw [] >>
    qexists_tac `v1 :: vals'` >> rw [])
  >- metis_tac []
  >- metis_tac []
  >- metis_tac []
  >- metis_tac []
  >- (
    rpt (pop_assum mp_tac) >>
    qid_spec_tac `vals` >>
    Induct_on `es` >> rw [] >> fs [drestrict_union_eq])
  >- metis_tac []
  >- metis_tac []
  >- metis_tac []
  >- metis_tac []
QED

Theorem eval_exp_ignores_unused:
  ∀s1 e v s2. DRESTRICT s1.locals (exp_uses e) = DRESTRICT s2.locals (exp_uses e) ⇒ (eval_exp s1 e v ⇔ eval_exp s2 e v)
Proof
  metis_tac [eval_exp_ignores_unused_lem]
QED

Triviality num_mod_to_int_mod:
  y ≠ 0 ⇒ x MOD y = Num (&x % &y)
Proof
  fs [INT_MOD]
QED

Triviality int_of_num2:
  0 ≤ x ⇒ &Num x = x
Proof
  metis_tac [INT_OF_NUM]
QED

Theorem int_sub_mod:
  ∀i j. j ≠ 0 ⇒ (i - j) % j = i % j
Proof
  rw [int_mod] >>
  `-j % j = 0 ∧ -j / j = -1`
  by (
    ONCE_REWRITE_TAC [INT_NEG_MINUS1] >> rw [] >>
    rw [INT_MUL_DIV]) >>
  rw [INT_ADD_DIV, int_sub, INT_RDISTRIB] >>
  rw [] >>
  intLib.COOPER_TAC
QED

Theorem mod_halfway:
  ∀i b. 0 < b ⇒ ((i + b) % (2 * b) - b < 0 ⇔ 0 ≤ i % (2 * b) - b)
Proof
  rw [] >> `b ≠ 0` by intLib.COOPER_TAC >>
  rw [Once (GSYM INT_MOD_PLUS)] >>
  `b < 2 * b` by intLib.COOPER_TAC >>
  rw [INT_LESS_MOD] >>
  `0 ≤ i % (2 * b) ∧ i % (2 * b) < 2 * b`
  by (
    `~(2 * b < 0) ∧ 2 * b ≠ 0` by intLib.COOPER_TAC >>
    drule INT_MOD_BOUNDS >>
    rw []) >>
  `0 ≤ i % (2 * b) + b` by intLib.COOPER_TAC >>
  Cases_on `i % (2 * b) + b < 2 * b` >> rw [INT_LESS_MOD]
  >- intLib.COOPER_TAC >>
  simp [Once (GSYM int_sub_mod)] >>
  rw [intLib.COOPER_PROVE ``∀x (b:int). x + b - (2 * b) = x - b``] >>
  `i % (2 * b) − b < 2 * b` by intLib.COOPER_TAC >>
  `0 ≤ i % (2 * b) − b` by intLib.COOPER_TAC >>
  rw [INT_LESS_MOD] >>
  intLib.COOPER_TAC
QED

Theorem unsigned_truncate:
  ∀m n i.
    0 < m ∧ m ≤ n ∧ -i ≤ 2 ** n
    ⇒
    signed2unsigned (truncate_2comp i m) m = signed2unsigned i n MOD (2 ** m)
Proof
  rw [signed2unsigned_def, truncate_2comp_def] >>
  qabbrev_tac `b = &(2 ** (m - 1))` >>
  `&((2:num) ** m) = 2 * b`
  by (rw [Abbr `b`] >> Cases_on `m` >> fs [ADD1, EXP_ADD]) >>
  `0 < b` by rw [Abbr `b`] >>
  `0 < 2 * b ∧ 0 ≠ 2 * b ∧ b < 2 * b` by (rw [Abbr `b`] >> intLib.COOPER_TAC) >>
  asm_simp_tac std_ss [num_mod_to_int_mod] >>
  fs [mod_halfway] >>
  `∃x. &(2 ** n) = 2 * b * 2 ** x`
  by (
    rw [Abbr `b`, GSYM EXP_ADD] >>
    `2 = 2 ** 1` by rw [] >>
    `∀x. 2 * 2 ** (m + x - 1) = 2 ** (1 + (m + x - 1))` by metis_tac [EXP_ADD] >>
    rw [] >>
    qexists_tac `n - m` >> rw []) >>
  irule (METIS_PROVE [] ``x = y ⇒ f x = f y``) >>
  fs [GSYM int_le] >>
  rw [int_of_num2] >>
  rw [intLib.COOPER_PROVE ``∀(x:int) b. 2 * b + (x - b) = b + x``] >>
  `0 ≤ i % (2 * b) ∧ i % (2 * b) < 2 * b`
  by (
    `~(2 * b < 0) ∧ 2 * b ≠ 0` by intLib.COOPER_TAC >>
    drule INT_MOD_BOUNDS >>
    rw [])
  >- (
    `0 ≤ 2 * b * &(2 ** x) + i` by intLib.COOPER_TAC >>
    rw [int_of_num2] >>
    `2 * b ≠ 0` by intLib.COOPER_TAC >>
    drule INT_MOD_ADD_MULTIPLES >>
    rw [Once INT_MUL_COMM] >>
    rw [Once (GSYM INT_MOD_PLUS)] >>
    rw [INT_LESS_MOD] >>
    simp [Once (GSYM int_sub_mod)] >>
    rw [intLib.COOPER_PROVE ``∀x (b:int). x + b - (2 * b) = x - b``] >>
    `i % (2 * b) − b < 2 * b` by intLib.COOPER_TAC >>
    rw [INT_LESS_MOD])
  >- (
    rw [Once (GSYM INT_MOD_PLUS)] >>
    rw [INT_LESS_MOD] >>
    simp [Once (GSYM int_sub_mod)] >>
    rw [intLib.COOPER_PROVE ``∀x (b:int). x + b - (2 * b) = x - b``] >>
    `i % (2 * b) − b < 2 * b` by intLib.COOPER_TAC >>
    rw [INT_LESS_MOD])
  >- (
    `0 ≤ 2 * b * &(2 ** x) + i` by intLib.COOPER_TAC >>
    rw [int_of_num2] >>
    `2 * b ≠ 0` by intLib.COOPER_TAC >>
    drule INT_MOD_ADD_MULTIPLES >>
    rw [Once INT_MUL_COMM] >>
    rw [Once (GSYM INT_MOD_PLUS)] >>
    rw [INT_LESS_MOD] >>
    `i % (2 * b) + b < 2 * b` by intLib.COOPER_TAC >>
    rw [INT_LESS_MOD] >>
    intLib.COOPER_TAC)
  >- (
    rw [Once (GSYM INT_MOD_PLUS)] >>
    rw [INT_LESS_MOD] >>
    `i % (2 * b) + b < 2 * b` by intLib.COOPER_TAC >>
    rw [INT_LESS_MOD] >>
    intLib.COOPER_TAC)
QED

(* Relate the semantics of Convert to something more closely following the
 * implementation *)

Definition Zextract_def:
  Zextract (:'a) z off len = &w2n ((len+off-1 -- off) (i2w z : 'a word))
End

Definition Zsigned_extract_def:
  Zsigned_extract (:'a) z off len = w2i ((len+off-1 --- off) (i2w z : 'a word))
End

(*
 * Some tests of extract and signed_extract in both HOL and OCaml to check that
 * we are defining the same thing *)

(*
 EVAL ``
   let bp1 = 0b11001100w : word8 in
   let bp2 = 0b01011011w : word8 in
   let i1 = &(w2n bp1) in
   let i2 = w2i bp1 in
   let i3 = &(w2n bp2) in
     Zextract (:128) i1 0 8 = i1 ∧
     Zextract (:128) i2 0 8 = i1 ∧
     Zextract (:128) i3 0 8 = i3 ∧
     Zsigned_extract (:128) i1 0 8 = i2 ∧
     Zsigned_extract (:128) i2 0 8 = i2 ∧
     Zsigned_extract (:128) i3 0 8 = i3 ∧

     Zextract (:128) i1 2 4 = 3 ∧
     Zextract (:128) i2 2 4 = 3 ∧
     Zextract (:128) i1 2 5 = 19 ∧
     Zextract (:128) i2 2 5 = 19 ∧
     Zextract (:128) i3 1 2 = 1 ∧
     Zextract (:128) i3 1 3 = 5 ∧

     Zsigned_extract (:128) i1 2 4 = 3 ∧
     Zsigned_extract (:128) i2 2 4 = 3 ∧
     Zsigned_extract (:128) i1 2 5 = -13 ∧
     Zsigned_extract (:128) i2 2 5 = -13 ∧
     Zsigned_extract (:128) i3 1 2 = 1 ∧
     Zsigned_extract (:128) i3 1 3 = -3``

   let i1 = Z.of_int 0b11001100 in
   let i2 = Z.of_int (-52) in
   let i3 = Z.of_int 0b01011011 in
     Z.extract i1 0 8 = i1 &&
     Z.extract i2 0 8 = i1 &&
     Z.extract i3 0 8 = i3 &&
     Z.signed_extract i1 0 8 = i2 &&
     Z.signed_extract i2 0 8 = i2 &&
     Z.signed_extract i3 0 8 = i3 &&

     Z.extract i1 2 4 = Z.of_int 3 &&
     Z.extract i2 2 4 = Z.of_int 3 &&
     Z.extract i1 2 5 = Z.of_int 19 &&
     Z.extract i2 2 5 = Z.of_int 19 &&
     Z.extract i3 1 2 = Z.of_int 1 &&
     Z.extract i3 1 3 = Z.of_int 5 &&

     Z.signed_extract i1 2 4 = Z.of_int 3 &&
     Z.signed_extract i2 2 4 = Z.of_int 3 &&
     Z.signed_extract i1 2 5 = Z.of_int (-13) &&
     Z.signed_extract i2 2 5 = Z.of_int (-13) &&
     Z.signed_extract i3 1 2 = Z.of_int 1 &&
     Z.signed_extract i3 1 3 = Z.of_int (-3);;
     *)

Theorem Zextract0:
  dimindex (:'b) ≤ dimindex (:'a)
  ⇒
  Zextract (:'a) i 0 (dimindex (:'b)) = &w2n (i2w i : 'b word)
Proof
  rw [Zextract_def] >>
  `w2n ((dimindex (:β) − 1 -- 0) (i2w i : 'a word)) =
   w2n (w2w (i2w i : 'a word) : 'b word)`
  by (
    rw [w2n_w2w] >>
    `dimindex (:'b) = dimindex (:'a)` by decide_tac >>
    fs [WORD_ALL_BITS]) >>
  rw [w2w_i2w]
QED

Theorem Zsigned_extract0:
  dimindex (:'b) ≤ dimindex (:'a)
  ⇒
  Zsigned_extract (:'a) i 0 (dimindex (:'b)) = w2i (i2w i : 'b word)
Proof
  rw [Zsigned_extract_def] >>
  rw [word_sign_extend_bits, word_sign_extend_def, ADD1] >>
  `0 < dimindex (:'b) ⇒ dimindex (:'b) - 1 + 1 = dimindex (:'b)` by decide_tac >>
  `min (dimindex (:β)) (dimindex (:α)) = dimindex (:β)` by fs [MIN_DEF] >>
  rw [] >>
  `w2n ((dimindex (:β) − 1 -- 0) (i2w i : 'a word)) =
   w2n (w2w (i2w i : 'a word) : 'b word)`
  by (
    rw [w2n_w2w] >>
    `dimindex (:'b) = dimindex (:'a)` by decide_tac >>
    fs [WORD_ALL_BITS]) >>
  rw [GSYM sw2sw_def, w2w_i2w] >>
  rw [w21_sw2sw_extend]
QED

Theorem signed_extract_truncate_2comp:
  dimindex (:'b) ≤ dimindex (:'a)
  ⇒
  Zsigned_extract (:'a) i 0 (dimindex (:'b)) = truncate_2comp i (dimindex (:'b))
Proof
  rw [] >>
  drule Zsigned_extract0 >> rw [] >>
  metis_tac [truncate_2comp_i2w_w2i]
QED

Theorem unsigned_extract_truncate_2comp:
  dimindex (:'b) ≤ dimindex (:'a)
  ⇒
  Zextract (:'a) i 0 (dimindex (:'b)) = &signed2unsigned (truncate_2comp i (dimindex (:'b))) (dimindex (:'b))
Proof
  rw [] >> drule Zextract0 >> rw [w2n_i2w] >>
  `∃n. -i ≤ 2 ** n ∧ dimindex (:'b) ≤ n`
  by (
    Cases_on `i < 0` >> rw []
    >- (
      `∃j. i = -&j` by intLib.COOPER_TAC >>
      rw [] >>
      `1 < 2` by decide_tac >>
      drule EXP_ALWAYS_BIG_ENOUGH >>
      disch_then (qspec_then `j` mp_tac) >>
      rw [] >>
      qexists_tac `MAX m (dimindex (:'b))` >>
      rw [MAX_DEF] >>
      drule bitTheory.TWOEXP_MONO >>
      intLib.COOPER_TAC)
    >- (
      `∃j. i = &j` by intLib.COOPER_TAC >>
      rw [] >>
      metis_tac [])) >>
  `0 < dimword (:'b) ∧ 0 < dimindex (:'b)` by rw [DIMINDEX_GT_0, ZERO_LT_dimword] >>
  `0 ≠ dimindex (:'b) ∧ 0 ≠ dimword (:'b)` by decide_tac >>
  drule unsigned_truncate >>
  ntac 2 (disch_then drule) >>
  rw [GSYM dimword_def] >>
  rw [signed2unsigned_def]
  >- (
    asm_simp_tac std_ss [GSYM INT_MOD] >>
    `0 ≤ &(2 ** n) + i`
    by (fs [INT_EXP] >> intLib.COOPER_TAC) >>
    asm_simp_tac std_ss [int_of_num2] >>
    `∃j. i = -&j` by intLib.COOPER_TAC >>
    rw [] >>
    `∃x. &(2 ** n) = dimword (:'b) * 2 ** x`
    by (
      rw [GSYM EXP_ADD, dimword_def] >>
      qexists_tac `n - dimindex (:'b)` >> rw []) >>
    rw [] >>
    `&dimword (:β) ≠ 0` by intLib.COOPER_TAC >>
    drule INT_MOD_ADD_MULTIPLES >>
    simp_tac std_ss [Once INT_MUL_COMM, GSYM INT_MUL])
  >- (
    `∃j. i = &j` by intLib.COOPER_TAC >>
    rw [])
QED

Definition simp_signed_def:
  simp_signed (:'a) bits arg to_t =
    case arg of
    | Integer data _ => Integer (Zsigned_extract (:'a) data 0 bits) to_t
    | _ => Signed bits arg to_t
End

Definition simp_unsigned_def:
  simp_unsigned (:'a) bits arg to_t =
    case arg of
    | Integer data _ => Integer (Zextract (:'a) data 0 bits) to_t
    | _ => Signed bits arg to_t
End

Theorem signed_implementation_fits:
  ∀const i to_t from_t.
    dimindex (:'b) ≤ sizeof_bits to_t ∧
    dimindex (:'b) ≤ dimindex (:'a)
    ⇒
    ∃i2.
      simp_signed (:'a) (dimindex (:'b)) (Integer i from_t) to_t =
      Integer i2 to_t ∧ ifits i2 (sizeof_bits to_t)
Proof
  rw [simp_signed_def] >>
  drule Zsigned_extract0 >> rw [] >>
  `ifits (w2i (i2w i : 'b word)) (dimindex (:'b))` by metis_tac [ifits_w2i] >>
  metis_tac [ifits_mono]
QED

Theorem unsigned_implementation_fits:
  ∀const i to_t from_t.
    dimindex (:'b) < sizeof_bits to_t ∧
    dimindex (:'b) ≤ dimindex (:'a)
    ⇒
    ∃i2.
      simp_unsigned (:'a) (dimindex (:'b)) (Integer i from_t) to_t =
      Integer i2 to_t ∧ ifits i2 (sizeof_bits to_t)
Proof
  rw [simp_unsigned_def] >>
  drule Zextract0 >> rw [] >> rw [w2n_i2w] >>
  fs [ifits_def, dimword_def] >> rw [] >>
  qspecl_then [`i`, `&(2 ** dimindex (:β))`] mp_tac INT_MOD_BOUNDS >>
  rw []
  >- (
    `0 <= (2:num) ** (sizeof_bits to_t − 1)` by intLib.COOPER_TAC >>
    intLib.COOPER_TAC)
  >- (
    `2 ** dimindex (:'b) ≤ 2 ** (sizeof_bits to_t - 1)` suffices_by intLib.COOPER_TAC >>
    rw [])
QED

Theorem signed_implementation:
  ∀to_t i from_t h m n.
    dimindex (:'b) ≤ sizeof_bits to_t ∧
    dimindex (:'b) ≤ dimindex (:'a) ∧
    from_t = IntegerT m ∧
    to_t = IntegerT n ∧
    0 < m ∧
    ifits i m
    ⇒
    eval_exp h (Signed (dimindex (:'b)) (Integer i from_t) to_t) =
    eval_exp h (simp_signed (:'a) (dimindex (:'b)) (Integer i from_t) to_t)
Proof
  rw [EXTENSION, IN_DEF] >> simp [simp_signed_def] >>
  ONCE_REWRITE_TAC [eval_exp_cases] >>
  fs [] >>
  ONCE_REWRITE_TAC [eval_exp_cases] >> rw [] >>
  `0 < m` by decide_tac >>
  `truncate_2comp i m = i` by metis_tac [fits_ident] >>
  rw [] >> fs [sizeof_bits_def] >>
  irule (METIS_PROVE [] ``y = z ⇒ (x = y ⇔ x = z)``) >> rw [] >>
  rw [signed_extract_truncate_2comp] >>
  `0 < dimindex (:'b)` by metis_tac [DIMINDEX_GT_0] >>
  `0 < n` by decide_tac >>
  `ifits (truncate_2comp i (dimindex (:β))) n` suffices_by metis_tac [fits_ident] >>
  metis_tac [truncate_2comp_fits, ifits_mono]
QED

Theorem unsigned_implementation:
  ∀to_t i from_t h m n.
    dimindex (:'b) < sizeof_bits to_t ∧
    dimindex (:'b) ≤ dimindex (:'a) ∧
    from_t = IntegerT m ∧
    to_t = IntegerT n ∧
    0 < m ∧
    ifits i m
    ⇒
    eval_exp h (Unsigned (dimindex (:'b)) (Integer i from_t) to_t) =
    eval_exp h (simp_unsigned (:'a) (dimindex (:'b)) (Integer i from_t) to_t)
Proof
  rw [EXTENSION, IN_DEF] >> simp [simp_unsigned_def] >>
  ONCE_REWRITE_TAC [eval_exp_cases] >>
  fs [] >>
  ONCE_REWRITE_TAC [eval_exp_cases] >> rw [] >>
  `0 < m` by decide_tac >>
  `truncate_2comp i m = i` by metis_tac [fits_ident] >>
  rw [] >> fs [sizeof_bits_def] >>
  irule (METIS_PROVE [] ``y = z ⇒ (x = y ⇔ x = z)``) >> rw [] >>
  rw [unsigned_extract_truncate_2comp] >>
  `0 < dimindex (:'b)` by metis_tac [DIMINDEX_GT_0] >>
  `0 < n` by decide_tac >>
  `ifits (&signed2unsigned (truncate_2comp i (dimindex (:β))) (dimindex (:'b))) n` suffices_by metis_tac [fits_ident] >>
  irule ifits_mono >>
  qexists_tac `dimindex (:'b) + 1` >> rw [] >>
  metis_tac [truncate_2comp_fits, signed2unsigned_fits]
QED

export_theory ();
