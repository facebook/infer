(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Properties of the mini-LLVM model *)

open HolKernel boolLib bossLib Parse;
open pairTheory listTheory rich_listTheory arithmeticTheory wordsTheory;
open logrootTheory numposrepTheory;
open settingsTheory llvmTheory;

new_theory "llvm_prop";

numLib.prefer_num();

(* ----- Theorems about list library functions ----- *)
(* Could be upstreamed to HOL *)

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

(* ----- Theorems about log ----- *)

(* Could be upstreamed to HOL *)
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

(* ----- Theorems about converting between values and byte lists ----- *)

Theorem le_write_w_length:
  ∀l x. length (le_write_w l w) = l
Proof
  rw [le_write_w_def]
QED

Theorem v2b_size:
  ∀t v. value_type t v ⇒ length (value_to_bytes v) = sizeof t
Proof
  ho_match_mp_tac value_type_ind >>
  rw [value_to_bytes_def, sizeof_def]
  >- metis_tac [le_write_w_length]
  >- metis_tac [le_write_w_length]
  >- metis_tac [le_write_w_length]
  >- (Induct_on `vs` >> rw [ADD1] >> fs [])
  >- (
    pop_assum mp_tac >>
    qid_spec_tac `vs` >> qid_spec_tac `ts` >>
    ho_match_mp_tac LIST_REL_ind >> rw [])
QED

Theorem b2v_size:
  (∀t bs. first_class_type t ∧ sizeof t ≤ length bs ⇒
    ∃v. bytes_to_value t bs = (v, drop (sizeof t) bs)) ∧
  (∀n t bs. first_class_type t ∧ n * sizeof t ≤ length bs ⇒
    ∃vs. read_array n t bs = (vs, drop (n * sizeof t) bs)) ∧
  (∀ts bs. every first_class_type ts ∧ sum (map sizeof ts) ≤ length bs ⇒
    ∃vs. read_str ts bs = (vs, drop (sum (map sizeof ts)) bs))
Proof
  ho_match_mp_tac bytes_to_value_ind >>
  rw [sizeof_def, bytes_to_value_def, le_read_w_def] >> 
  fs [first_class_type_def]
  >- (simp [PAIR_MAP] >> metis_tac [SND])
  >- (
    pairarg_tac >> rw [] >> pairarg_tac >> rw [] >>
    fs [ADD1] >> rw [] >> fs [DROP_DROP_T, LEFT_ADD_DISTRIB])
  >- fs [DROP_DROP_T, LEFT_ADD_DISTRIB]
QED

Theorem b2v_smaller:
   ∀t bs. first_class_type t ∧ sizeof t ≤ length bs ⇒
    length (snd (bytes_to_value t bs)) = length bs - sizeof t
Proof
  rw [] >> imp_res_tac b2v_size >>
  Cases_on `bytes_to_value t bs` >> fs []
QED

Theorem b2v_append:
  (∀t bs. first_class_type t ∧ sizeof t ≤ length bs ⇒
    bytes_to_value t (bs ++ bs') = (I ## (λx. x ++ bs')) (bytes_to_value t bs)) ∧
  (∀n t bs. first_class_type t ∧ n * sizeof t ≤ length bs ⇒
    ∃vs. read_array n t (bs ++ bs') = (I ## (λx. x ++ bs')) (read_array n t bs)) ∧
  (∀ts bs. every first_class_type ts ∧ sum (map sizeof ts) ≤ length bs ⇒
    ∃vs. read_str ts (bs ++ bs') = (I ## (λx. x ++ bs')) (read_str ts bs))
Proof
  ho_match_mp_tac bytes_to_value_ind >>
  rw [sizeof_def, bytes_to_value_def, le_read_w_def] >> 
  fs [first_class_type_def, TAKE_APPEND, DROP_APPEND,
      DECIDE ``!x y. x ≤ y ⇒ x - y = 0n``, ETA_THM]
  >- (simp [PAIR_MAP] >> metis_tac [SND])
  >- (simp [PAIR_MAP] >> metis_tac [SND])
  >- (
    rpt (pairarg_tac >> simp []) >> fs [ADD1] >> 
    BasicProvers.VAR_EQ_TAC >> fs [LEFT_ADD_DISTRIB] >>
    first_x_assum irule >>
    `sizeof t ≤ length bs` by decide_tac >> 
    imp_res_tac b2v_smaller >> rfs [])
  >- (
    rpt (pairarg_tac >> simp []) >> fs [ADD1] >> 
    BasicProvers.VAR_EQ_TAC >> fs [LEFT_ADD_DISTRIB] >>
    first_x_assum irule >>
    `sizeof t ≤ length bs` by decide_tac >> 
    imp_res_tac b2v_smaller >> rfs [])
QED

Theorem le_read_write:
  ∀n w bs.
    n ≠ 0 ∧ dimword (:'a) ≤ 256 ** n ⇒ le_read_w n (le_write_w n (w :'a word) ⧺ bs) = (w, bs)
Proof
  rw [le_read_w_def, le_write_w_length]
  >- (
    `take n (le_write_w n w ⧺ bs) = le_write_w n w`
    by metis_tac [le_write_w_length, TAKE_LENGTH_APPEND] >>
    simp [le_write_w_def, w2l_def, l2w_def] >>
    Cases_on `w` >> simp [] >> fs [l2n_padding, TAKE_APPEND, take_replicate] >>
    simp [MAP_TAKE, MAP_MAP_o, combinTheory.o_DEF, mod_n2l] >>
    rename1 `n2l 256 m` >>
    `length (n2l 256 m) ≤ n`
    by (
      rw [LENGTH_n2l] >>
      `256 = 2 ** 8` by EVAL_TAC >>
      ASM_REWRITE_TAC [] >> simp [log_change_base_power, GSYM LESS_EQ] >>
      `n2w m ≠ 0w` by simp [] >>
      drule LOG2_w2n_lt >> rw [] >> fs [bitTheory.LOG2_def, dimword_def] >>
      `8 * (log 2 m DIV 8) ≤ log 2 m` by metis_tac [mul_div_bound, EVAL ``8 ≠ 0n``] >>
      `LOG 2 (2 ** dimindex (:'a)) ≤ LOG 2 (256 ** n)` by simp [LOG_LE_MONO] >>
      pop_assum mp_tac >>
      `256 = 2 ** 8` by EVAL_TAC >>
      ASM_REWRITE_TAC [EXP_MUL] >> simp [log_base_power]) >>
    simp [mod_n2l, l2n_n2l, TAKE_LENGTH_TOO_LONG])
  >- metis_tac [le_write_w_length, DROP_LENGTH_APPEND]
QED

Theorem le_write_read:
  ∀n w bs bs'.
    256 ** n ≤ dimword (:'a) ∧
    n ≤ length bs ∧
    le_read_w n bs = (w:'a word, bs')
    ⇒
    le_write_w n w ++ bs' = bs
Proof
  rw [le_read_w_def] >>
  qmatch_goalsub_abbrev_tac `l2w _ l` >>
  `le_write_w n (l2w 256 l) = take n bs` suffices_by metis_tac [TAKE_DROP] >>
  simp [le_write_w_def, w2l_l2w] >>
  `l2n 256 l < 256 ** n`
  by (
    `n ≤ length bs` by decide_tac >>
    metis_tac [l2n_lt, LENGTH_TAKE, LENGTH_MAP, EVAL ``0n < 256``]) >>
  fs [] >>
  `every ($> 256) l`
  by (
    simp [EVERY_MAP, Abbr `l`] >> irule EVERY_TAKE >> simp [] >>
    rpt (pop_assum kall_tac) >>
    Induct_on `bs` >> rw [] >>
    Cases_on `h` >> fs []) >>
  rw [n2l_l2n]
  >- (
    rw [TAKE_def, take_replicate] >>
    Cases_on `n` >> fs [] >>
    rfs [l2n_0] >> unabbrev_all_tac >> fs [EVERY_MAP] >>
    ONCE_REWRITE_TAC [GSYM REPLICATE] >>
    qmatch_goalsub_abbrev_tac `take n _` >>
    qpat_assum `¬(_ < _)` mp_tac >>
    qpat_assum `every (\x. 0 = w2n x) _` mp_tac >>
    rpt (pop_assum kall_tac) >>
    qid_spec_tac `bs` >>
    Induct_on `n` >> rw [] >>
    Cases_on `bs` >> rw [] >> fs [] >>
    Cases_on `h` >> fs [] >>
    first_x_assum irule >> rw [] >>
    irule MONO_EVERY >>
    qexists_tac `(λx. 0 = w2n x)` >> rw []) >>
  fs [MAP_TAKE, MAP_MAP_o, combinTheory.o_DEF] >>
  `exists (\y. 0 ≠ y) l`
  by (
    fs [l2n_eq_0, combinTheory.o_DEF] >> fs [EXISTS_MEM, EVERY_MEM] >>
    qexists_tac `x` >> rfs [MOD_MOD, GREATER_DEF]) >>
  simp [LOG_l2n_dropWhile] >>
  `length (dropWhile ($= 0) (reverse l)) ≠ 0`
  by (
    Cases_on `l` >> fs [dropWhile_eq_nil, combinTheory.o_DEF, EXISTS_REVERSE]) >>
  `0 < length (dropWhile ($= 0) (reverse l))` by decide_tac >>
  fs [SUC_PRE] >>
  `map n2w l = take n bs`
  by (simp [Abbr `l`, MAP_TAKE, MAP_MAP_o, combinTheory.o_DEF, n2w_w2n]) >>
  simp [TAKE_TAKE_MIN] >>
  `length l = n` by simp [Abbr `l`] >>
  `length (dropWhile ($= 0) (reverse l)) ≤ n`
  by metis_tac [LESS_EQ_TRANS, LENGTH_dropWhile_LESS_EQ, LENGTH_REVERSE] >> 
  rw [MIN_DEF] >> fs []
  >- (
    simp [TAKE_APPEND, TAKE_TAKE_MIN, MIN_DEF, take_replicate] >>
    `replicate (length l − length (dropWhile ($= 0) (reverse l))) 0w =
     take (length l − (length (dropWhile ($= 0) (reverse l)))) (drop (length (dropWhile ($= 0) (reverse l))) bs)`
    suffices_by (rw [] >> irule take_drop_partition >> simp []) >>
    rw [LIST_EQ_REWRITE, EL_REPLICATE, EL_TAKE, EL_DROP] >>
   `length (dropWhile ($= 0) (reverse l)) =
     length (dropWhile (λx. 0 = w2n x) (reverse (take (length l) bs)))`
    by (
      `reverse l = reverse (take (length l) (map w2n bs))` by metis_tac [] >>
      ONCE_ASM_REWRITE_TAC [] >>
      qpat_x_assum `Abbrev (l = _)` kall_tac >>
      simp [GSYM MAP_TAKE, GSYM MAP_REVERSE, dropWhile_map, combinTheory.o_DEF]) >>
    fs [] >>
    `x + length (dropWhile (λx. 0 = w2n x) (reverse (take (length l) bs))) < length l` by decide_tac >>
    drule (SIMP_RULE std_ss [LET_THM] dropWhile_rev_take) >>
    rw [] >>
    REWRITE_TAC [GSYM w2n_11, word_0_n2w] >>
    simp [])
  >- rw [TAKE_APPEND, TAKE_TAKE]
QED 

Theorem b2v_v2b:
  ∀v t bs. value_type t v ⇒ bytes_to_value t (value_to_bytes v ++ bs) = (v, bs)
Proof
  gen_tac >> completeInduct_on `v_size v` >>
  rw [] >>
  pop_assum mp_tac >> simp [value_type_cases] >>
  rw [] >>
  rw [bytes_to_value_def, value_to_bytes_def, le_read_write]
  >- wordsLib.WORD_DECIDE_TAC
  >- (
    qmatch_abbrev_tac `_ x = _` >>
    `x = (vs, bs)` suffices_by (simp [PAIR_MAP] >> metis_tac [PAIR_EQ, FST, SND]) >>
    unabbrev_all_tac >>
    qid_spec_tac `bs` >> Induct_on `vs` >> simp [bytes_to_value_def] >>
    rw [] >> fs [v_size_def] >>
    pairarg_tac >> fs [] >>
    pairarg_tac >> fs [] >>
    rename1 `value_type t v1` >>
    first_x_assum (qspec_then `v_size v1` mp_tac) >> simp [] >>
    disch_then (qspec_then `v1` mp_tac) >> simp [] >>
    disch_then (qspec_then `t` mp_tac) >> simp [] >>
    qmatch_assum_abbrev_tac `bytes_to_value _ (_ ++ bs1 ++ _) = _` >>
    disch_then (qspec_then `bs1++bs` mp_tac) >> simp [] >>
    unabbrev_all_tac >> strip_tac >> fs [] >>
    first_x_assum (qspec_then `bs` mp_tac) >> rw [])
  >- (
    qmatch_abbrev_tac `_ x = _` >>
    `x = (vs, bs)` suffices_by (simp [PAIR_MAP] >> metis_tac [PAIR_EQ, FST, SND]) >>
    unabbrev_all_tac >>
    pop_assum mp_tac >>
    qid_spec_tac `bs` >> qid_spec_tac `ts` >> Induct_on `vs` >> simp [bytes_to_value_def] >>
    rw [] >> fs [v_size_def, bytes_to_value_def] >>
    pairarg_tac >> fs [] >>
    pairarg_tac >> fs [] >>
    rename1 `value_type t v1` >>
    first_x_assum (qspec_then `v_size v1` mp_tac) >> simp [] >>
    disch_then (qspec_then `v1` mp_tac) >> simp [] >>
    disch_then (qspec_then `t` mp_tac) >> simp [] >>
    qmatch_assum_abbrev_tac `bytes_to_value _ (_ ++ bs1 ++ _) = _` >>
    disch_then (qspec_then `bs1++bs` mp_tac) >> simp [] >>
    unabbrev_all_tac >> strip_tac >> fs [] >>
    first_x_assum drule >> metis_tac [PAIR_EQ])
QED

(* ----- Theorems about insert/extract value and get_offset ----- *)

Theorem can_extract:
  ∀v indices t.
    indices_ok t indices ∧ value_type t v ⇒ extract_value v indices ≠ None
Proof
  recInduct extract_value_ind >> rw [extract_value_def]
  >- (
    pop_assum mp_tac >> rw [value_type_cases] >> fs [indices_ok_def] >>
    metis_tac [LIST_REL_LENGTH])
  >- (
    pop_assum mp_tac >> rw [value_type_cases] >> fs [indices_ok_def] >>
    metis_tac [EVERY_EL, LIST_REL_EL_EQN]) >>
  Cases_on `t` >> fs [indices_ok_def] >> simp [value_type_cases]
QED

Theorem can_insert:
  ∀v v2 indices t.
    indices_ok t indices ∧ value_type t v ⇒ insert_value v v2 indices ≠ None
Proof
  recInduct insert_value_ind >> rw [insert_value_def]
  >- (
    pop_assum mp_tac >> rw [value_type_cases] >> fs [indices_ok_def] >>
    metis_tac [LIST_REL_LENGTH])
  >- (
    pop_assum mp_tac >> rw [value_type_cases] >> fs [indices_ok_def] >>
    CASE_TAC >> fs [] >> rfs [] >>
    metis_tac [EVERY_EL, LIST_REL_EL_EQN]) >>
  Cases_on `t` >> fs [indices_ok_def] >> simp [value_type_cases]
QED

Theorem extract_insertvalue:
  ∀v1 v2 indices v3.
    insert_value v1 v2 indices = Some v3
    ⇒
    extract_value v3 indices = Some v2
Proof 
  recInduct insert_value_ind >> rw [insert_value_def, extract_value_def] >>
  pop_assum mp_tac >> CASE_TAC >> fs [] >> rfs [] >>
  rw [] >> simp [extract_value_def, EL_LUPDATE]
QED

Theorem extract_insertvalue_other:
  ∀v1 v2 indices1 indices2 v3.
    insert_value v1 v2 indices1 = Some v3 ∧
    ¬(indices1 ≼ indices2) ∧ ¬(indices2 ≼ indices1)
    ⇒
    extract_value v3 indices2 = extract_value v1 indices2
Proof 
  recInduct insert_value_ind >> rw [insert_value_def, extract_value_def] >>
  qpat_x_assum `_ = SOME _` mp_tac >> CASE_TAC >> rw [] >> rfs [] >>
  qpat_x_assum `¬case _ of [] => F | h::t => P h t` mp_tac >>
  CASE_TAC >> fs [] >> rename1 `idx::is` >>
  fs [extract_value_def] >> rw [EL_LUPDATE]
QED

Theorem insert_extractvalue:
  ∀v1 indices v2.
    extract_value v1 indices = Some v2
    ⇒
    insert_value v1 v2 indices = Some v1
Proof 
  recInduct extract_value_ind >> rw [insert_value_def, extract_value_def] >> fs [] >>
  rw [LUPDATE_SAME]
QED

Definition indices_in_range_def:
  (indices_in_range t [] ⇔ T) ∧
  (indices_in_range (ArrT n t) (i::is) ⇔
    i < n ∧ indices_in_range t is) ∧
  (indices_in_range (StrT ts) (i::is) ⇔
    i < length ts ∧ indices_in_range (el i ts) is) ∧
  (indices_in_range _ _ ⇔ F)
End

Definition extract_type_def:
  (extract_type t [] = Some t) ∧
  (extract_type (ArrT n t) (i::idx) =
    if i < n then
      extract_type t idx
    else
      None) ∧
  (extract_type (StrT ts) (i::idx) =
    if i < length ts then
      extract_type (el i ts) idx
    else 
      None) ∧
  (extract_type _ _ = None)
End

(* The strict inequality does not hold because of 0 length arrays *)
Theorem offset_size_leq:
  ∀t indices n.
    indices_in_range t indices ∧ get_offset t indices = Some n
    ⇒
    n ≤ sizeof t
Proof
  recInduct get_offset_ind >> rw [get_offset_def, sizeof_def, indices_in_range_def] >>
  BasicProvers.EVERY_CASE_TAC >> fs [] >> rw [] >> rfs []
  >- (
    `x + i * sizeof t ≤ (i + 1) * sizeof t` by decide_tac >>
    `i + 1 ≤ v1` by decide_tac >>
    metis_tac [LESS_MONO_MULT, LESS_EQ_TRANS]) >>
  rw [MAP_TAKE, ETA_THM] >>
  `take (Suc i) (map sizeof ts) = take i (map sizeof ts) ++ [sizeof (el i ts)]`
  by rw [GSYM SNOC_EL_TAKE, EL_MAP] >>
  `take (Suc i) (map sizeof ts) ≼ (map sizeof ts)` by rw [take_is_prefix] >>
  drule sum_prefix >> rw [SUM_APPEND]
QED

Theorem value_type_is_fc:
  ∀t v. value_type t v ⇒ first_class_type t
Proof
  ho_match_mp_tac value_type_ind >> rw [first_class_type_def] >>
  fs [LIST_REL_EL_EQN, EVERY_EL]
QED

Theorem extract_type_fc:
  ∀t is t'. extract_type t is = Some t' ∧ first_class_type t ⇒ first_class_type t'
Proof
  recInduct extract_type_ind >> rw [extract_type_def, first_class_type_def] >>
  rw [] >> fs [] >> fs [EVERY_EL]
QED

Theorem extract_offset_size:
  ∀t indices n t'.
    extract_type t indices = Some t' ∧
    get_offset t indices = Some n
    ⇒
    sizeof t' ≤ sizeof t - n
Proof
  recInduct get_offset_ind >> rw [get_offset_def, extract_type_def] >>
  BasicProvers.EVERY_CASE_TAC >> fs [sizeof_def] >> rfs [] >> rw [ETA_THM]
  >- (
    `sizeof t ≤ (v1  − i) * sizeof t` suffices_by decide_tac  >>
    `1 ≤ v1 - i` by decide_tac >>
    rw []) >>
  rw [MAP_TAKE] >>
  `sizeof (el i ts) ≤ sum (map sizeof ts) − (sum (take i (map sizeof ts)))`
  suffices_by decide_tac >>
  qpat_x_assum `_ < _` mp_tac >> rpt (pop_assum kall_tac) >> qid_spec_tac `i` >>
  Induct_on `ts` >> rw [TAKE_def, EL_CONS, PRE_SUB1]
QED

Theorem read_from_offset_extract:
  ∀t indices n v t'.
    indices_in_range t indices ∧
    get_offset t indices = Some n ∧
    value_type t v ∧
    extract_type t indices = Some t'
    ⇒
    extract_value v indices = Some (fst (bytes_to_value t' (drop n (value_to_bytes v))))
Proof
  recInduct get_offset_ind >>
  rw [extract_value_def, get_offset_def, extract_type_def, indices_in_range_def] >>
  simp [DROP_0]
  >- metis_tac [APPEND_NIL, FST, b2v_v2b] >>
  qpat_x_assum `value_type _ _` mp_tac >>
  simp [Once value_type_cases] >> rw [] >> simp [extract_value_def] >>
  qpat_x_assum `_ = Some n` mp_tac >> CASE_TAC >> rw [] >> rfs [] >>
  simp [value_to_bytes_def]
  >- (
    `value_type t (el i vs)` by metis_tac [EVERY_EL] >>
    first_x_assum drule >>
    rw [] >> simp [GSYM DROP_DROP_T, ETA_THM] >>
    `i * sizeof t = length (flat (take i (map value_to_bytes vs)))`
    by (
      simp [LENGTH_FLAT, MAP_TAKE, MAP_MAP_o, combinTheory.o_DEF] >>
      `map (λx. length (value_to_bytes x)) vs = replicate (length vs) (sizeof t)`
      by (
        qpat_x_assum `every _ _` mp_tac >> rpt (pop_assum kall_tac) >>
        Induct_on `vs` >> rw [v2b_size]) >>
      rw [take_replicate, MIN_DEF]) >>
    rw [GSYM flat_drop, GSYM MAP_DROP] >>
    drule DROP_CONS_EL >> simp [DROP_APPEND] >> disch_then kall_tac >>
    `first_class_type t'` by metis_tac [value_type_is_fc, extract_type_fc] >>
    `sizeof t' ≤ length (drop x (value_to_bytes (el i vs)))`
    by (simp [LENGTH_DROP] >> drule v2b_size >> rw [] >> metis_tac [extract_offset_size]) >>
    simp [b2v_append])
  >- metis_tac [LIST_REL_LENGTH]
  >- (
    `value_type (el i ts) (el i vs)` by metis_tac [LIST_REL_EL_EQN] >>
    first_x_assum drule >>
    rw [] >> simp [GSYM DROP_DROP_T, ETA_THM] >>
    `sum (map sizeof (take i ts)) = length (flat (take i (map value_to_bytes vs)))`
    by (
      simp [LENGTH_FLAT, MAP_TAKE, MAP_MAP_o, combinTheory.o_DEF] >>
      `map sizeof ts = map (\x. length (value_to_bytes x)) vs`
      by (
        qpat_x_assum `list_rel _ _ _` mp_tac >> rpt (pop_assum kall_tac) >>
        qid_spec_tac `ts` >>
        Induct_on `vs` >> rw [] >> rw [v2b_size]) >>
      rw []) >>
    rw [GSYM flat_drop, GSYM MAP_DROP] >>
    `i < length vs` by metis_tac [LIST_REL_LENGTH] >>
    drule DROP_CONS_EL >> simp [DROP_APPEND] >> disch_then kall_tac >>
    `first_class_type t'` by metis_tac [value_type_is_fc, extract_type_fc] >>
    `sizeof t' ≤ length (drop x (value_to_bytes (el i vs)))`
    by (simp [LENGTH_DROP] >> drule v2b_size >> rw [] >> metis_tac [extract_offset_size]) >>
    simp [b2v_append])
QED

(* ----- Theorems about the step function ----- *)

Theorem inc_pc_invariant:
  ∀p s i. prog_ok p ∧ next_instr p s i ∧ ¬terminator i ∧ state_invariant p s ⇒ state_invariant p (inc_pc s)
Proof
  rw [state_invariant_def, inc_pc_def, allocations_ok_def, globals_ok_def,
      stack_ok_def, frame_ok_def, heap_ok_def, EVERY_EL, ip_ok_def]
  >- (
    qexists_tac `dec` >> qexists_tac `block'` >> rw [] >>
    fs [prog_ok_def, next_instr_cases] >> res_tac >> rw [] >>
    `s.ip.i ≠ length block'.body - 1` suffices_by decide_tac >>
    CCONTR_TAC >> fs [] >> rfs [LAST_EL, PRE_SUB1]) >>
  metis_tac []
QED

Theorem next_instr_update:
  ∀p s i r v. next_instr p (update_result r v s) i <=> next_instr p s i
Proof
  rw [next_instr_cases, update_result_def]
QED

Theorem update_invariant:
  ∀r v s. state_invariant p (update_result r v s) ⇔ state_invariant p s
Proof
  rw [update_result_def, state_invariant_def, ip_ok_def, allocations_ok_def,
      globals_ok_def, stack_ok_def, heap_ok_def, EVERY_EL, frame_ok_def]
QED

Theorem step_instr_invariant:
  ∀i s2. step_instr p s1 i s2 ⇒ prog_ok p ∧ next_instr p s1 i ∧ state_invariant p s1 ⇒ state_invariant p s2
Proof
  ho_match_mp_tac step_instr_ind >> rw []
  >- cheat
  >- cheat
  >- cheat
  >- (
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant]>>
    metis_tac [terminator_def])
  >- (
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant] >>
    metis_tac [terminator_def])
  >- (
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant] >>
    metis_tac [terminator_def])
  >- (
    (* Allocation *)
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant]
    >- cheat
    >- (fs [next_instr_cases, allocate_cases] >> metis_tac [terminator_def]))
  >- (
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant] >>
    fs [next_instr_cases] >>
    metis_tac [terminator_def])
  >- (
    (* Store *)
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant]
    >- cheat
    >- (fs [next_instr_cases] >> metis_tac [terminator_def]))
  >- (
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant] >>
    metis_tac [terminator_def])
  >- (
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant] >>
    metis_tac [terminator_def])
  >- (
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant] >>
    metis_tac [terminator_def])
  >- (
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant] >>
    metis_tac [terminator_def])
  >- cheat
QED

export_theory ();
