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
  (exp_uses (Convert _ _ _ e) = exp_uses e)
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

Definition extract_def:
  extract (:'a) unsigned bits z =
    if unsigned then Zextract (:'a) z 0 bits else Zsigned_extract (:'a) z 0 bits
End

Definition simp_convert_def:
  simp_convert (:'a) unsigned dst src arg =
    case (dst, src) of
    | (IntegerT m, IntegerT n) =>
      (if m ≤ n then
        case arg of
        | Integer data _ => Integer (extract (:'a) F m data) dst
        | _ => Convert F dst src arg
      else
        case arg of
        | Integer data _ => Integer (extract (:'a) unsigned n data) dst
        | _ =>
          if unsigned then Convert unsigned dst src arg
          else arg)
    | _ =>
      if dst = src then arg
      else Convert unsigned dst src arg
End

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

Theorem convert_implementation_fits:
  ∀unsigned dst src const i m n.
    const = Integer i src ∧
    src = IntegerT n ∧
    dst = IntegerT m ∧ 0 < m ∧
    ifits i (sizeof_bits src) ∧
    dimindex (:'b) = min m n ∧
    dimindex (:'b) ≤ dimindex (:'a)
    ⇒
    ∃i2. simp_convert (:'a) unsigned dst src const = Integer i2 dst ∧ ifits i2 m
Proof
  rw [simp_convert_def, extract_def, MIN_DEF] >> fs []
  >- (drule Zsigned_extract0 >> rw [] >> rw [ifits_w2i])
  >- (
    `m = dimindex (:'b)` by decide_tac >>
     drule Zsigned_extract0 >> rw [] >> rw [ifits_w2i])
  >- (
    drule Zextract0 >> rw [] >> rw [w2n_i2w] >> fs [sizeof_bits_def] >>
    fs [ifits_def, dimword_def] >> rw [] >>
    qspecl_then [`i`, `&(2 ** dimindex (:β))`] mp_tac INT_MOD_BOUNDS >>
    rw []
    >- intLib.COOPER_TAC >>
    `dimindex (:'b) < m` by decide_tac >>
    `2 ** dimindex (:'b) ≤ 2 ** (m - 1)` suffices_by intLib.COOPER_TAC >>
    rw [])
  >- (
    drule Zsigned_extract0 >> rw [] >>
    irule ifits_mono >> qexists_tac `dimindex (:'b)` >> rw [ifits_w2i])
QED

Theorem convert_implementation:
  ∀h unsigned dst src const i m n.
    const = Integer i src ∧
    src = IntegerT n ∧ 0 < n ∧
    dst = IntegerT m ∧
    ifits i (sizeof_bits src) ∧
    dimindex (:'b) = min m n ∧
    dimindex (:'b) ≤ dimindex (:'a)
    ⇒
    eval_exp h (Convert unsigned dst src const) =
    eval_exp h (simp_convert (:'a) unsigned dst src const)
Proof
  rw [EXTENSION, IN_DEF] >>
  simp [simp_convert_def] >>
  CASE_TAC >>
  ONCE_REWRITE_TAC [eval_exp_cases] >>
  fs [sizeof_bits_def] >>
  ONCE_REWRITE_TAC [eval_exp_cases] >> rw [] >>
  `0 < n` by decide_tac >>
  `truncate_2comp i n = i` by metis_tac [fits_ident] >>
  rw [] >>
  Cases_on `unsigned` >> fs [extract_def] >>
  irule (METIS_PROVE [] ``y = z ⇒ (x = y ⇔ x = z)``) >> rw []
  >- ( (* Truncating, unsigned convert *)
    drule Zsigned_extract0 >> rw [] >>
    `min m n = m` by fs [MIN_DEF] >>
    `∀i. truncate_2comp i (dimindex (:β)) = w2i (i2w i : 'b word)`
    by rw [GSYM truncate_2comp_i2w_w2i] >>
    fs [] >> rw [i2w_pos, i2n_def, i2w_def] >>
    `?j. 0 ≤ j ∧ -i = j` by rw [] >>
    `i = -j` by intLib.COOPER_TAC >>
    simp [] >>
    simp [GSYM int_sub] >>
    `?k. j = &k` by metis_tac [NUM_POSINT_EXISTS] >>
    simp [] >>
    `k < 2 ** n`
    by (fs [ifits_def] >> Cases_on `n` >> fs [ADD1, EXP_ADD]) >>
    simp [INT_SUB, word_2comp_n2w, dimword_def] >>
    qabbrev_tac `d = dimindex (:'b)` >>
    `∃x. (2:num) ** n = 2 ** x * 2 ** d`
    by (
      `?x. n = x + d` by (qexists_tac `n - d` >> fs [MIN_DEF]) >>
      metis_tac [EXP_ADD]) >>
    metis_tac [MOD_COMPLEMENT, bitTheory.ZERO_LT_TWOEXP, MULT_COMM])
  >- ( (* Truncating, signed convert *)
    `min m n = m` by rw [MIN_DEF] >>
    drule Zsigned_extract0 >> rw [] >> fs [] >>
    `w2i (i2w i : 'b word) = truncate_2comp i m` by metis_tac [truncate_2comp_i2w_w2i] >>
    rw [] >>
    `0 < dimindex (:'b)` by rw [] >>
    metis_tac [fits_ident, truncate_2comp_fits]) >>
  (* extending *)
    drule Zsigned_extract0 >> drule Zextract0 >> fs [MIN_DEF] >> rw [w2n_i2n] >>
    `INT_MIN (:'b) ≤ i ∧ i ≤ INT_MAX (:'b)` suffices_by metis_tac [w2i_i2w] >>
    fs [ifits_def, INT_MAX_def, INT_MIN_def, int_arithTheory.INT_NUM_SUB] >>
    rw [DECIDE ``!(x:num). x < 1 ⇔ x = 0``,
        intLib.COOPER_PROVE``!(x:int). x ≤ y -1 ⇔ x < y``]
QED

export_theory ();
