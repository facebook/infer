(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Properties of the mini-LLVM model *)

open HolKernel boolLib bossLib Parse;
open pairTheory listTheory rich_listTheory arithmeticTheory wordsTheory;
open pred_setTheory finite_mapTheory relationTheory llistTheory pathTheory;
open optionTheory;
open logrootTheory numposrepTheory;
open settingsTheory miscTheory memory_modelTheory llvmTheory;

new_theory "llvm_prop";

numLib.prefer_num();

(* ----- Theorems about converting between values and byte lists ----- *)

Theorem value_type_is_fc:
  ∀t v. value_type t v ⇒ first_class_type t
Proof
  ho_match_mp_tac value_type_ind >> rw [first_class_type_def] >>
  fs [LIST_REL_EL_EQN, EVERY_EL]
QED

Theorem sizeof_type_to_shape:
  ∀t. first_class_type t ⇒ sizeof (type_to_shape t) = sizeof t
Proof
  recInduct type_to_shape_ind >>
  rw [type_to_shape_def, memory_modelTheory.sizeof_def, llvmTheory.sizeof_def,
      first_class_type_def, EVERY_MEM] >>
    qid_spec_tac `vs` >> Induct_on `ts` >> rw [] >> fs []
QED

Theorem value_type_to_shape:
  ∀t v.
    value_type t v ⇒
    ∀s.
    value_shape (\n t x. n = fst (unconvert_value x) ∧ value_type t (FlatV x)) (type_to_shape t) v
Proof
  ho_match_mp_tac value_type_ind >>
  rw [memory_modelTheory.sizeof_def, llvmTheory.sizeof_def, type_to_shape_def,
      unconvert_value_def, value_shape_def] >>
  fs [value_shapes_list_rel, LIST_REL_CONJ, ETA_THM, EVERY2_MAP] >>
  metis_tac [value_type_rules]
QED

Theorem llvm_v2b_size:
  ∀t v. value_type t v ⇒ length (llvm_value_to_bytes v) = sizeof t
Proof
  rw [llvm_value_to_bytes_def] >>
  drule value_type_to_shape >> rw [] >>
  drule value_type_is_fc >> rw [] >>
  drule sizeof_type_to_shape >>
  disch_then (mp_tac o GSYM) >> rw [] >>
  irule v2b_size >> rw [] >>
  qmatch_assum_abbrev_tac `value_shape f _ _` >>
  qexists_tac `f` >> rw [] >>
  unabbrev_all_tac >> fs []
QED

Theorem b2llvm_v_size:
  ∀t bs. first_class_type t ∧ sizeof t ≤ length bs ⇒
    ∃v. bytes_to_llvm_value t bs = (v, drop (sizeof t) bs)
Proof
  rw [bytes_to_llvm_value_def] >>
  drule sizeof_type_to_shape >> disch_then (mp_tac o GSYM) >> rw [] >>
  fs [PAIR_MAP] >>
  metis_tac [SND, b2v_size]
QED

Theorem b2llvm_v_smaller:
   ∀t bs. first_class_type t ∧ sizeof t ≤ length bs ⇒
    length (snd (bytes_to_llvm_value t bs)) = length bs - sizeof t
Proof
  rw [bytes_to_llvm_value_def] >>
  metis_tac [b2v_smaller, sizeof_type_to_shape]
QED

Theorem b2llvm_v_append:
  ∀t bs bs'. first_class_type t ∧ sizeof t ≤ length bs ⇒
    bytes_to_llvm_value t (bs ++ bs') = (I ## (λx. x ++ bs')) (bytes_to_llvm_value t bs)
Proof
  rw [bytes_to_llvm_value_def] >>
  drule sizeof_type_to_shape >> disch_then (mp_tac o GSYM) >> rw [] >> fs [] >>
  rw [PAIR_MAP, b2v_append]
QED

Theorem b2v_llvm_v2b:
  ∀v t bs. value_type t v ⇒ bytes_to_llvm_value t (llvm_value_to_bytes v ++ bs) = (v, bs)
Proof
  rw [bytes_to_llvm_value_def, llvm_value_to_bytes_def] >>
  drule value_type_to_shape >> rw [] >>
  qmatch_assum_abbrev_tac `value_shape f _ _` >>
  irule b2v_v2b >>
  qexists_tac `f` >> rw [] >>
  unabbrev_all_tac >> fs [] >>
  fs [unconvert_value_def, convert_value_def, value_type_cases, pointer_size_def] >>
  wordsLib.WORD_DECIDE_TAC
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

(* The strict inequality does not hold because of 0 length arrays *)
Theorem offset_size_leq:
  ∀t indices n.
    indices_in_range t indices ∧ get_offset t indices = Some n
    ⇒
    n ≤ sizeof t
Proof
  recInduct get_offset_ind >> rw [get_offset_def, llvmTheory.sizeof_def, indices_in_range_def] >>
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
  BasicProvers.EVERY_CASE_TAC >> fs [llvmTheory.sizeof_def] >> rfs [] >> rw [ETA_THM]
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

Theorem llvm_value_to_bytes_agg:
  ∀vs. llvm_value_to_bytes (AggV vs) = flat (map llvm_value_to_bytes vs)
Proof
  Induct >> rw [] >> fs [llvm_value_to_bytes_def, value_to_bytes_def]
QED

Theorem read_from_offset_extract:
  ∀t indices n v t'.
    indices_in_range t indices ∧
    get_offset t indices = Some n ∧
    value_type t v ∧
    extract_type t indices = Some t'
    ⇒
    extract_value v indices = Some (fst (bytes_to_llvm_value t' (drop n (llvm_value_to_bytes v))))
Proof
  recInduct get_offset_ind >>
  rw [extract_value_def, get_offset_def, extract_type_def, indices_in_range_def] >>
  simp [DROP_0]
  >- metis_tac [APPEND_NIL, FST, b2v_llvm_v2b] >>
  qpat_x_assum `value_type _ _` mp_tac >>
  simp [Once value_type_cases] >> rw [] >> simp [extract_value_def] >>
  qpat_x_assum `_ = Some n` mp_tac >> CASE_TAC >> rw [] >> rfs [] >>
  simp [llvm_value_to_bytes_agg]
  >- (
    `value_type t (el i vs)` by metis_tac [EVERY_EL] >>
    first_x_assum drule >>
    rw [] >> simp [GSYM DROP_DROP_T, ETA_THM] >>
    `i * sizeof t = length (flat (take i (map llvm_value_to_bytes vs)))`
    by (
      simp [LENGTH_FLAT, MAP_TAKE, MAP_MAP_o, combinTheory.o_DEF] >>
      `map (λx. length (llvm_value_to_bytes x)) vs = replicate (length vs) (sizeof t)`
      by (
        qpat_x_assum `every _ _` mp_tac >> rpt (pop_assum kall_tac) >>
        Induct_on `vs` >> rw [llvm_v2b_size]) >>
      rw [take_replicate, MIN_DEF]) >>
    rw [GSYM flat_drop, GSYM MAP_DROP] >>
    drule DROP_CONS_EL >> simp [DROP_APPEND] >> disch_then kall_tac >>
    `first_class_type t'` by metis_tac [value_type_is_fc, extract_type_fc] >>
    `sizeof t' ≤ length (drop x (llvm_value_to_bytes (el i vs)))`
    by (simp [LENGTH_DROP] >> drule llvm_v2b_size >> rw [] >> metis_tac [extract_offset_size]) >>
    simp [b2llvm_v_append])
  >- metis_tac [LIST_REL_LENGTH]
  >- (
    `value_type (el i ts) (el i vs)` by metis_tac [LIST_REL_EL_EQN] >>
    first_x_assum drule >>
    rw [] >> simp [GSYM DROP_DROP_T, ETA_THM] >>
    `sum (map sizeof (take i ts)) = length (flat (take i (map llvm_value_to_bytes vs)))`
    by (
      simp [LENGTH_FLAT, MAP_TAKE, MAP_MAP_o, combinTheory.o_DEF] >>
      `map sizeof ts = map (\x. length (llvm_value_to_bytes x)) vs`
      by (
        qpat_x_assum `list_rel _ _ _` mp_tac >> rpt (pop_assum kall_tac) >>
        qid_spec_tac `ts` >>
        Induct_on `vs` >> rw [] >> rw [llvm_v2b_size]) >>
      rw []) >>
    rw [GSYM flat_drop, GSYM MAP_DROP] >>
    `i < length vs` by metis_tac [LIST_REL_LENGTH] >>
    drule DROP_CONS_EL >> simp [DROP_APPEND] >> disch_then kall_tac >>
    `first_class_type t'` by metis_tac [value_type_is_fc, extract_type_fc] >>
    `sizeof t' ≤ length (drop x (llvm_value_to_bytes (el i vs)))`
    by (simp [LENGTH_DROP] >> drule llvm_v2b_size >> rw [] >> metis_tac [extract_offset_size]) >>
    simp [b2llvm_v_append])
QED

(* ----- Theorems about the step function ----- *)

Theorem w64_cast_some:
  ∀w t.
    (w64_cast w t = Some v)
    ⇔
    v = FlatV (W1V (w2w w)) ∧ t = IntT W1 ∨
    v = FlatV (W8V (w2w w)) ∧ t = IntT W8 ∨
    v = FlatV (W32V (w2w w)) ∧ t = IntT W32 ∨
    v = FlatV (W64V (w2w w)) ∧ t = IntT W64
Proof
  Cases_on `t` >> rw [w64_cast_def] >> Cases_on `s` >> rw [w64_cast_def] >>
  metis_tac []
QED

Theorem unsigned_v_to_num_some:
  ∀v n.
    (unsigned_v_to_num v = Some n)
    ⇔
    (∃w. v = FlatV (W1V w) ∧ n = w2n w) ∨
    (∃w. v = FlatV (W8V w) ∧ n = w2n w) ∨
    (∃w. v = FlatV (W32V w) ∧ n = w2n w) ∨
    (∃w. v = FlatV (W64V w) ∧ n = w2n w)
Proof
  Cases_on `v` >> rw [unsigned_v_to_num_def] >>
  Cases_on `a` >> rw [unsigned_v_to_num_def]
QED

Theorem signed_v_to_int_some:
  ∀v n.
    (signed_v_to_int v = Some n)
    ⇔
    (∃w. v = FlatV (W1V w) ∧ n = w2i w) ∨
    (∃w. v = FlatV (W8V w) ∧ n = w2i w) ∨
    (∃w. v = FlatV (W32V w) ∧ n = w2i w) ∨
    (∃w. v = FlatV (W64V w) ∧ n = w2i w)
Proof
  Cases_on `v` >> rw [signed_v_to_int_def] >>
  Cases_on `a` >> rw [signed_v_to_int_def] >>
  metis_tac []
QED

Theorem signed_v_to_num_some:
  ∀v n.
    (signed_v_to_num v = Some n)
    ⇔
    ∃m. 0 ≤ m ∧ n = Num m ∧
      ((∃w. v = FlatV (W1V w) ∧ m = w2i w) ∨
       (∃w. v = FlatV (W8V w) ∧ m = w2i w) ∨
       (∃w. v = FlatV (W32V w) ∧ m = w2i w) ∨
       (∃w. v = FlatV (W64V w) ∧ m = w2i w))
Proof
  rw [signed_v_to_num_def, OPTION_JOIN_EQ_SOME, signed_v_to_int_some] >>
  metis_tac [intLib.COOPER_PROVE ``!x:int. 0 ≤ x ⇔ ¬(x < 0)``]
QED

Theorem mk_ptr_some:
  ∀n p. mk_ptr n = Some p ⇔ n < 256 ** pointer_size ∧ p = FlatV (PtrV (n2w n))
Proof
  rw [mk_ptr_def] >> metis_tac []
QED

(* How many bytes a value of the given type occupies *)
Definition sizeof_bits_def:
  (sizeof_bits (IntT W1) = 1) ∧
  (sizeof_bits (IntT W8) = 8) ∧
  (sizeof_bits (IntT W32) = 32) ∧
  (sizeof_bits (IntT W64) = 64) ∧
  (sizeof_bits (PtrT _) = pointer_size) ∧
  (sizeof_bits (ArrT n t) = n * sizeof_bits t) ∧
  (sizeof_bits (StrT ts) = sum (map sizeof_bits ts))
Termination
  WF_REL_TAC `measure ty_size` >> simp [] >>
  Induct >> rw [ty_size_def] >> simp [] >>
  first_x_assum drule >> decide_tac
End

Theorem do_cast_zext:
  (∃t'. sizeof_bits t' < sizeof_bits t ∧ value_type t' v) ∧ do_cast Zext v t = Some v'
  ⇒
  unsigned_v_to_num v = unsigned_v_to_num v'
Proof
  rw [do_cast_def, OPTION_JOIN_EQ_SOME, w64_cast_some] >>
  fs [unsigned_v_to_num_def, unsigned_v_to_num_some, sizeof_bits_def, value_type_cases] >>
  rw [w2n_11, w2w_n2w]  >>
  fs [sizeof_bits_def]
  >- (
    `w2n w' < dimword (:1)` by metis_tac [w2n_lt] >>
    fs [dimword_1])
  >- (
    `w2n w' < dimword (:1)` by metis_tac [w2n_lt] >>
    fs [dimword_1])
  >- (
    `w2n w' < dimword (:8)` by metis_tac [w2n_lt] >>
    fs [dimword_1])
  >- (
    `w2n w' < dimword (:1)` by metis_tac [w2n_lt] >>
    fs [dimword_1])
  >- (
    `w2n w' < dimword (:8)` by metis_tac [w2n_lt] >>
    fs [dimword_1])
  >- (
    `w2n w' < dimword (:32)` by metis_tac [w2n_lt] >>
    fs [dimword_1])
QED

val trunc_thms =
  LIST_CONJ (map (fn x => SIMP_RULE (srw_ss()) [] (INST_TYPE [``:'a`` |-> x] (GSYM truncate_2comp_i2w_w2i)))
                 [``:1``, ``:8``, ``:32``, ``:64``]);

val ifits_thms =
  LIST_CONJ (map (fn x => SIMP_RULE (srw_ss()) [] (INST_TYPE [``:'a`` |-> x] (ifits_w2i )))
                 [``:1``, ``:8``, ``:32``, ``:64``]);

Theorem do_cast_sext:
  (∃t'. sizeof_bits t' < sizeof_bits t ∧ value_type t' v) ∧ do_cast Sext v t = Some v'
  ⇒
  signed_v_to_int v = signed_v_to_int v'
Proof
  rw [do_cast_def, OPTION_JOIN_EQ_SOME, w64_cast_some] >>
  fs [signed_v_to_int_def, signed_v_to_num_some, sizeof_bits_def, value_type_cases] >>
  rw [] >>
  fs [sizeof_bits_def, signed_v_to_int_def] >> rw [integer_wordTheory.w2w_i2w] >>
  rw [trunc_thms, GSYM fits_ident] >>
  rw [ifits_thms] >>
  metis_tac [ifits_thms, ifits_mono, DECIDE ``1 ≤ 8 ∧ 1 ≤ 32 ∧ 8 ≤ 32 ∧ 1 ≤ 64 ∧ 8 ≤ 64 ∧ 32 ≤ 64``]
QED

Theorem get_instr_func:
  ∀p ip i1 i2. get_instr p ip i1 ∧ get_instr p ip i2 ⇒ i1 = i2
Proof
  rw [get_instr_cases] >> fs [] >> rw [] >> fs [] >> rw [] >> fs []
QED

Theorem inc_pc_invariant:
  ∀p s i. prog_ok p ∧ get_instr p s.ip (Inl i) ∧ ¬terminator i ∧ state_invariant p s ⇒ state_invariant p (inc_pc s)
Proof
  rw [state_invariant_def, inc_pc_def, allocations_ok_def, globals_ok_def,
      stack_ok_def, frame_ok_def, heap_ok_def, EVERY_EL, ip_ok_def, inc_bip_def,
      METIS_PROVE [] ``x ∨ y ⇔ ~x ⇒ y``]
  >- (
    qexists_tac `dec` >> qexists_tac `block'` >> rw [] >>
    fs [prog_ok_def, get_instr_cases] >> res_tac >> rw [] >>
    Cases_on `s.ip.i` >> fs [] >> rw [] >> fs [inc_bip_def] >>
    `idx ≠ length block'.body - 1` suffices_by decide_tac >>
    CCONTR_TAC >> fs [] >> rfs [LAST_EL, PRE_SUB1]) >>
  metis_tac []
QED

Theorem get_instr_update:
  ∀p s i r v. get_instr p (update_result r v s).ip i <=> get_instr p s.ip i
Proof
  rw [get_instr_cases, update_result_def]
QED

Theorem update_invariant:
  ∀r v s. state_invariant p (update_result r v s) ⇔ state_invariant p s
Proof
  rw [update_result_def, state_invariant_def, ip_ok_def, allocations_ok_def,
      globals_ok_def, stack_ok_def, heap_ok_def, EVERY_EL, frame_ok_def]
QED

Theorem allocate_invariant:
  ∀p s1 v1 t v2 h2.
    state_invariant p s1 ∧ allocate s1.heap v1 t (v2,h2) ⇒ state_invariant p (s1 with heap := h2)
Proof
  rw [state_invariant_def, ip_ok_def, globals_ok_def, stack_ok_def,
      METIS_PROVE [] ``x ∨ y ⇔ ~x ⇒ y``]
  >- metis_tac [allocate_heap_ok]
  >- (fs [is_allocated_def] >> metis_tac [allocate_unchanged, SUBSET_DEF])
  >- (
    fs [EVERY_EL, frame_ok_def, allocate_unchanged] >> rw [] >>
    metis_tac [allocate_unchanged, SUBSET_DEF])
QED

Theorem set_bytes_invariant:
  ∀s poison bytes n prog b.
    state_invariant prog s ∧ is_allocated (Interval b n (n + length bytes)) s.heap
    ⇒
    state_invariant prog (s with heap := set_bytes poison bytes n s.heap)
Proof
  rw [state_invariant_def]
  >- metis_tac [set_bytes_heap_ok]
  >- (fs [globals_ok_def, is_allocated_def, set_bytes_unchanged] >> metis_tac [])
  >- (fs [stack_ok_def, EVERY_EL, frame_ok_def, set_bytes_unchanged])
QED

Triviality not_none_eq:
  !x. x ≠ None ⇔ ∃y. x = Some y
Proof
  Cases >> rw []
QED

Theorem step_instr_invariant:
  ∀p s1 i l s2.
    step_instr p s1 i l s2 ⇒ prog_ok p ∧ get_instr p s1.ip (Inl i) ∧ state_invariant p s1
    ⇒
    state_invariant p s2
Proof
  ho_match_mp_tac step_instr_ind >> rw []
  >- ( (* Ret *)
    rw [update_invariant] >> fs [state_invariant_def] >> rw []
    >- (
      fs [stack_ok_def] >> rfs [EVERY_EL, frame_ok_def] >>
      first_x_assum (qspec_then `0` mp_tac) >> simp [])
    >- (
      fs [heap_ok_def, deallocate_def, allocations_ok_def] >> rw []
      >- metis_tac []
      >- metis_tac [] >>
      fs [deallocate_def, heap_ok_def] >> rw [flookup_fdiff] >>
      eq_tac >> rw []
      >- metis_tac [NOT_IS_SOME_EQ_NONE]
      >- metis_tac [NOT_IS_SOME_EQ_NONE] >>
      fs [allocations_ok_def, stack_ok_def, EXTENSION] >> metis_tac [])
    >- (
      fs [globals_ok_def, deallocate_def] >> rw [] >>
      first_x_assum drule >> rw [] >> fs [is_allocated_def] >>
      qexists_tac `b2` >> rw [] >> CCONTR_TAC >> fs [interval_freeable_def])
    >- (
      fs [stack_ok_def, EVERY_MEM, frame_ok_def, deallocate_def] >> rfs [] >>
      rw []
      >- (
        res_tac >> rw [] >> qexists_tac `stop` >> rw [] >>
        fs [ALL_DISTINCT_APPEND, MEM_FLAT, MEM_MAP] >>
        metis_tac [])
      >- (
        fs [ALL_DISTINCT_APPEND])))
  >- ( (* Br *)
    fs [state_invariant_def] >> rw []
    >- (
      rw [ip_ok_def] >> fs [prog_ok_def] >>
      qpat_x_assum `alookup _ (Fn "main") = _` kall_tac >>
      fs [get_instr_cases] >>
      last_x_assum drule >> disch_then drule >> fs [] >> rw [] >>
      `terminator (el idx b.body)` by metis_tac [terminator_def] >>
      `last b.body = el idx b.body`
      by (
        Cases_on `idx = PRE (length b.body)` >> fs [EL_PRE_LENGTH] >>
        `Suc idx < length b.body` by decide_tac >>
        drule mem_el_front >> rw [] >> fs [EVERY_MEM] >>
        metis_tac []) >>
      qpat_x_assum `Br _ _ _ = _` (assume_tac o GSYM) >> fs [] >>
      fs [instr_to_labs_def, not_none_eq] >>
      metis_tac [])
    >- (fs [globals_ok_def] >> metis_tac [])
    >- (fs [stack_ok_def, frame_ok_def, EVERY_MEM] >> metis_tac []))
  >- ( (* Br *)
    fs [state_invariant_def] >> rw []
    >- (
      rw [ip_ok_def] >> fs [prog_ok_def] >>
      qpat_x_assum `alookup _ (Fn "main") = _` kall_tac >>
      fs [get_instr_cases] >>
      last_x_assum drule >> disch_then drule >> fs [] >> rw [] >>
      `terminator (el idx b.body)` by metis_tac [terminator_def] >>
      `last b.body = el idx b.body`
      by (
        Cases_on `idx = PRE (length b.body)` >> fs [EL_PRE_LENGTH] >>
        `Suc idx < length b.body` by decide_tac >>
        drule mem_el_front >> rw [] >> fs [EVERY_MEM] >>
        metis_tac []) >>
      qpat_x_assum `Br _ _ _ = _` (assume_tac o GSYM) >> fs [] >>
      fs [instr_to_labs_def, not_none_eq] >>
      metis_tac [])
    >- (fs [globals_ok_def] >> metis_tac [])
    >- (fs [stack_ok_def, frame_ok_def, EVERY_MEM] >> metis_tac []))
  >- (
    fs [state_invariant_def, globals_ok_def, stack_ok_def, frame_ok_def,
        EVERY_MEM] >>
    metis_tac [])
  >- (
    irule inc_pc_invariant >> rw [get_instr_update, update_invariant]>>
    metis_tac [terminator_def])
  >- (
    irule inc_pc_invariant >> rw [get_instr_update, update_invariant] >>
    metis_tac [terminator_def])
  >- (
    irule inc_pc_invariant >> rw [get_instr_update, update_invariant] >>
    metis_tac [terminator_def])
  >- ( (* Allocation *)
    irule inc_pc_invariant >> rw [get_instr_update, update_invariant]
    >- metis_tac [allocate_invariant]
    >- (fs [get_instr_cases, allocate_cases] >> metis_tac [terminator_def]))
  >- (
    irule inc_pc_invariant >> rw [get_instr_update, update_invariant] >>
    fs [get_instr_cases] >>
    metis_tac [terminator_def])
  >- ( (* Store *)
    irule inc_pc_invariant >> rw [get_instr_update, update_invariant]
    >- (irule set_bytes_invariant >> rw [] >> metis_tac [])
    >- (fs [get_instr_cases] >> metis_tac [terminator_def]))
  >- (
    irule inc_pc_invariant >> rw [get_instr_update, update_invariant] >>
    metis_tac [terminator_def])
  >- (
    irule inc_pc_invariant >> rw [get_instr_update, update_invariant] >>
    metis_tac [terminator_def])
  >- (
    irule inc_pc_invariant >> rw [get_instr_update, update_invariant] >>
    metis_tac [terminator_def])
  >- ( (* Call *)
    rw [state_invariant_def]
    >- (fs [prog_ok_def, ip_ok_def] >> metis_tac [NOT_NIL_EQ_LENGTH_NOT_0])
    >- (fs [state_invariant_def, heap_ok_def] >> metis_tac [])
    >- (fs [state_invariant_def, globals_ok_def] >> metis_tac [])
    >- (
      fs [state_invariant_def, stack_ok_def] >> rw []
      >- (
        rw [frame_ok_def] >> fs [ip_ok_def, prog_ok_def, inc_bip_def] >>
        last_x_assum drule >> disch_then drule >> rw [] >>
        CCONTR_TAC >> fs [] >> rfs [LAST_EL] >>
        Cases_on `length block'.body = idx + 1` >> fs [PRE_SUB1] >>
        fs [get_instr_cases] >>
        metis_tac [terminator_def])
      >- (fs [EVERY_MEM, frame_ok_def] >> metis_tac [])))
QED

Theorem step_invariant:
  ∀p s1 l s2.
    prog_ok p ∧ step p s1 l s2 ∧ state_invariant p s1
    ⇒
    state_invariant p s2
Proof
  rw [step_cases]
  >- metis_tac [step_instr_invariant] >>
  fs [get_instr_cases, inc_pc_def, inc_bip_def, state_invariant_def] >>
  rw []
  >- (
    fs [ip_ok_def, prog_ok_def] >>
    metis_tac [NOT_NIL_EQ_LENGTH_NOT_0])
  >- (fs [globals_ok_def] >> metis_tac [])
  >- (fs [stack_ok_def, frame_ok_def, EVERY_MEM] >> metis_tac [])
QED

Definition is_call_def:
  (is_call (Call _ _ _ _) ⇔ T) ∧
  (is_call _ ⇔ F)
End

Theorem step_same_block:
  ∀p s1 l s2.
    get_instr p s1.ip i ∧ step p s1 l s2 ∧
    (∀i'. i = Inl i' ⇒ ¬terminator i' ∧ ¬is_call i') ⇒
    s1.ip.f = s2.ip.f ∧
    s1.ip.b = s2.ip.b ∧
    s2.ip.i = inc_bip s1.ip.i
Proof
  simp [step_cases] >>
  rpt gen_tac >> disch_tac >> fs [inc_pc_def] >>
  `i = Inl i'` by metis_tac [get_instr_func] >>
  fs [step_instr_cases] >> rfs [] >>
  fs [terminator_def, is_call_def, inc_pc_def, update_result_def]
QED

(* ----- Initial state is ok ----- *)

Theorem init_invariant:
  ∀p s init. prog_ok p ∧ is_init_state s init ⇒ state_invariant p s
Proof
  rw [is_init_state_def, state_invariant_def]
  >- (rw [ip_ok_def] >> fs [prog_ok_def] >> metis_tac [NOT_NIL_EQ_LENGTH_NOT_0])
  >- rw [stack_ok_def]
QED

(* ----- A bigger-step semantics ----- *)

Inductive last_step:
  (∀p s1 l s2 i.
    step p s1 l s2 ∧ get_instr p s1.ip i ∧
    ((∃x. i = Inr x) ∨ (∃i'. i = Inl i' ∧ (terminator i' ∨ is_call i')))
    ⇒
    last_step p s1 l s2) ∧
  (∀p s1.
   (¬∃l s2. step p s1 l s2)
   ⇒
   last_step p s1 Error (s1 with status := Stuck))
End

(* Run all of the instructions up-to-and-including the next Call or terminator.
 * Stop after the phis too.
 * *)
Inductive multi_step:
  (∀p s1 s2 l.
   last_step p s1 l s2 ∧
   s1.status = Partial
   ⇒
   multi_step p s1 [l] s2) ∧

  (∀p s1 s2 s3 i l ls.
   step p s1 l s2 ∧
   s1.status = Partial ∧
   get_instr p s1.ip (Inl i) ∧
   ¬(terminator i ∨ is_call i) ∧
   multi_step p s2 ls s3
   ⇒
   multi_step p s1 (l::ls) s3)
End

Definition multi_step_sem_def:
  multi_step_sem p s1 =
    { l1 | ∃path l2. l1 ∈ observation_prefixes ((last path).status, flat l2) ∧
           toList (labels path) = Some l2 ∧
           finite path ∧ okpath (multi_step p) path ∧ first path = s1 }
End

Theorem multi_step_to_step_path:
  ∀p s1 l s2.
    multi_step p s1 l s2 ⇒
      ∃path.
        finite path ∧ okpath (sem_step p) path ∧ first path = s1 ∧ last path = s2 ∧
        toList (labels path) = Some l
Proof
  ho_match_mp_tac multi_step_ind >> conj_tac
  >- (rw [] >> qexists_tac `pcons s1 l (stopped_at s2)` >> fs [sem_step_cases, toList_THM, last_step_cases]) >>
  rw [] >>
  qexists_tac `pcons s1 l path` >> rw [toList_THM] >>
  `LFINITE (labels path)` by metis_tac [finite_labels] >>
  simp [sem_step_cases]
QED

Theorem expand_multi_step_path:
  ∀path. okpath (multi_step prog) path ∧ finite path ⇒
    !l. toList (labels path) = Some l ⇒
    ∃path'.
      toList (labels path') = Some (flat l) ∧ finite path' ∧
      okpath (sem_step prog) path' ∧ first path' = first path ∧ last path' = last path
Proof
  ho_match_mp_tac finite_okpath_ind >> rw []
  >- (qexists_tac `stopped_at x` >> fs [toList_THM] >> rw []) >>
  fs [toList_THM] >> rw [] >>
  first_x_assum drule >> rw [] >>
  drule multi_step_to_step_path >> rw [] >>
  qexists_tac `plink path'' path'` >> rw [] >>
  simp [toList_THM, labels_plink] >>
  `LFINITE (LAPPEND (labels path'') (labels path'))` by metis_tac [LFINITE_APPEND, finite_labels] >>
  drule LFINITE_toList >> rw [] >> drule toList_LAPPEND_APPEND >> rw []
QED

Theorem contract_step_path:
  ∀path. okpath (sem_step prog) path ∧ finite path ⇒
    ∀l1 l s.
      last_step prog (last path) l s ∧
      (last path).status = Partial ∧
      toList (labels path) = Some l1
    ⇒
    ∃path' l2.
      toList (labels path') = Some l2 ∧
      flat l2 = l1 ++ [l] ∧
      finite path' ∧
      okpath (multi_step prog) path' ∧ first path' = first path ∧ last path' = s
Proof
  ho_match_mp_tac finite_okpath_ind >> rw []
  >- (
    qexists_tac `pcons x [l] (stopped_at s)` >> fs [] >> simp [toList_THM] >>
    simp [Once multi_step_cases] >>
    fs [toList_THM]) >>
  fs [toList_THM] >>
  first_x_assum drule >> disch_then drule >> rw [] >>
  Cases_on `last_step prog x r (first path)`
  >- (
    qexists_tac `pcons x [r] path'` >> simp [] >>
    fs [sem_step_cases] >>
    simp [Once multi_step_cases, toList_THM] >>
    simp [last_step_cases])
  >- (
    qpat_x_assum `okpath (multi_step _) _` mp_tac >>
    simp [Once okpath_cases] >> rw [] >> fs [toList_THM] >> rw [] >> fs [] >>
    qexists_tac `pcons x (r::r') p` >> fs [toList_THM] >> rw [Once multi_step_cases] >>
    disj2_tac >> qexists_tac `first path` >> rw [] >> fs [sem_step_cases]
    >- (fs [last_step_cases, step_cases, get_instr_cases] >> metis_tac []) >>
    qpat_x_assum `okpath (sem_step _) _` mp_tac >>
    simp [Once okpath_cases, sem_step_cases] >> CCONTR_TAC >> fs [] >> rw [] >>
    fs [first_def, last_thm] >> rw [] >> fs [])
QED

Definition get_next_step_def:
  get_next_step p s1 =
    some (s2, l). sem_step p s1 l s2 ∧ ¬last_step p s1 l s2
End

Triviality finite_plink_trivial:
  ∀path. finite path ⇒ path = plink path (stopped_at (last path))
Proof
  ho_match_mp_tac finite_path_ind >> rw []
QED

Definition instrs_left_def:
  instrs_left prog s =
    case alookup prog s.ip.f of
    | None => 0
    | Some d =>
      case alookup d.blocks s.ip.b of
      | None => 0
      | Some b =>
        case s.ip.i of
        | Phi_ip _ => length b.body + 1
        | Offset idx => length b.body - idx
End

Theorem sem_step_stuck:
  ∀p s1. (∀l s2. ¬sem_step p s1 l s2) ⇔ s1.status ≠ Partial
Proof
  rw [sem_step_cases] >> metis_tac []
QED

Theorem sem_step_then_stuck:
  ∀p s1 l1 s2.
    sem_step p s1 l1 s2 ∧ (∀l2 s3. ¬sem_step p s2 l2 s3)
    ⇒
    (l1 = Error ∧ s2 = s1 with status := Stuck ∧ ∀l2 s3. ¬step p s1 l2 s3) ∨
    (∃i e. l1 = Exit i ∧ s2 = s1 with status := Complete i ∧
      get_instr p s1.ip (Inl (Exit e)))
Proof
  rw [sem_step_stuck] >>
  fs [sem_step_cases] >>
  disj2_tac >> fs [step_cases] >> rfs [inc_pc_def] >>
  fs [step_instr_cases] >> rfs [update_result_def, inc_pc_def] >>
  metis_tac []
QED

Theorem sem_step_not_last:
  ∀p s1 l1 s2.
    sem_step p s1 l1 s2 ∧ ¬last_step p s1 l1 s2 ⇒
    ∃l2 s3. sem_step p s2 l2 s3
Proof
  rw [] >> CCONTR_TAC >> fs [] >> drule sem_step_then_stuck >>
  simp [] >>
  CCONTR_TAC >> fs [] >> rw []
  >- fs [last_step_cases] >>
  fs [last_step_cases, sem_step_cases] >> rw [] >>
  first_x_assum (qspec_then `Inl (Exit e)` mp_tac) >>
  rw [terminator_def]
QED

Triviality some_lemma:
  ∀P a b. (some (x, y). P x y) = Some (a, b) ⇒ P a b
Proof
  rw [some_def] >>
  qmatch_assum_abbrev_tac `(@x. Q x) = _` >>
  `Q (@x. Q x)` suffices_by (rw [Abbr `Q`]) >>
  `∃x. Q x` suffices_by rw [SELECT_THM] >>
  unabbrev_all_tac >> rw [] >>
  pairarg_tac >> fs [] >> rw [EXISTS_PROD] >>
  metis_tac []
QED

Theorem extend_step_path:
  ∀path.
    okpath (sem_step p) path ∧ finite path
    ⇒
    (∀s. path = stopped_at s ⇒ ∃s' l. sem_step p s l s') ⇒
    ∃path' l s n.
      finite path' ∧ okpath (sem_step p) path' ∧ (last path').status = Partial ∧
      last_step p (last path') l s ∧
      length path = Some (Suc n) ∧ n ∈ PL (pconcat path' l (stopped_at s)) ∧
      path = take n (pconcat path' l (stopped_at s))
Proof
  rw [] >>
  Cases_on `get_next_step p (last path) = None ∧ ∀s. path ≠ stopped_at s`
  >- (
    fs [get_next_step_def, some_def, FORALL_PROD, METIS_PROVE [] ``~x ∨ y ⇔ (x ⇒ y)``] >>
    Cases_on `∃l2 s2. sem_step p (last path) l2 s2` >> fs []
    >- ( (* Can take a last step from the end of the path *)
      first_x_assum drule >> rw [] >>
      qexists_tac `path` >> qexists_tac `l2` >> qexists_tac `s2` >> rw [] >>
      fs [finite_length] >>
      qexists_tac `n - 1` >>
      `n ≠ 0` by metis_tac [length_never_zero] >>
      rw [PL_def] >>
      `length (pconcat path l2 (stopped_at s2)) = Some (n + 1)`
      by metis_tac [length_pconcat, alt_length_thm] >>
      rw [take_pconcat]
      >- fs [sem_step_cases]
      >- metis_tac [take_all] >>
      fs [PL_def] >> rfs [])
    >- ( (* The path is stuck, so we need to extract the last step from it *)
      drule finite_path_end_cases >>
      rw [] >> fs [] >> rfs [] >>
      qexists_tac `p'` >> rw [] >>
      qexists_tac `l` >> qexists_tac `s` >> rw [] >>
      fs [finite_length] >>
      qexists_tac `n` >> rw [] >>
      `length (plink p' (pcons (last p') l (stopped_at s))) = Some (n + Suc 1 - 1)`
      by metis_tac [length_plink, alt_length_thm, OPTION_MAP_DEF] >>
      rw []
      >- fs [sem_step_cases]
      >- metis_tac [sem_step_not_last]
      >- (
        rw [PL_def] >> fs [finite_length] >>
        `length (pconcat p' l (stopped_at s)) = Some (n + 1)`
        by metis_tac [length_pconcat, alt_length_thm] >>
        fs [])
      >- (
        rw [take_pconcat]
        >- (fs [PL_def, finite_length] >> rfs []) >>
        metis_tac [finite_length, pconcat_to_plink_finite]))) >>
  qexists_tac `plink path (unfold I (get_next_step p) (last path))` >> rw [] >>
  qmatch_goalsub_abbrev_tac `finite path1` >>
  `∃m. length path = Some (Suc m)`
  by (fs [finite_length] >> Cases_on `n` >> fs [length_never_zero]) >>
  simp [GSYM PULL_EXISTS] >>
  conj_asm1_tac
  >- (
    simp [Abbr `path1`] >> irule unfold_finite >>
    WF_REL_TAC `measure (instrs_left p)` >>
    rpt gen_tac >>
    rw [instrs_left_def, get_next_step_def] >>
    qabbrev_tac `P = (\s3 l. sem_step p s2 l s3 ∧ ¬last_step p s2 l s3)` >>
    `P s3 l` by (irule some_lemma >> simp [Abbr `P`]) >>
    pop_assum mp_tac >> simp [Abbr `P`] >> strip_tac >>
    drule sem_step_not_last >> simp [] >> strip_tac >>
    qpat_x_assum `sem_step p s2 l s3` mp_tac >> rw [Once sem_step_cases]
    >- (
      `∃i. get_instr p s2.ip i` by metis_tac [get_instr_cases, step_cases] >>
      `∃x. i = Inl x` by (fs [last_step_cases] >> metis_tac [sumTheory.sum_CASES]) >>
      drule step_same_block >> disch_then drule >> simp [] >>
      impl_tac
      >- (fs [last_step_cases] >> metis_tac []) >>
      fs [step_cases, get_instr_cases, inc_bip_def] >> rw [] >> fs [] >>
      rw [inc_bip_def] >> fs [])
    >- fs [last_step_cases]) >>
 `last path = first path1`
  by (
    unabbrev_all_tac >> simp [Once unfold_thm] >>
    CASE_TAC >> rw [] >> split_pair_case_tac >> rw []) >>
  simp [last_plink] >>
  conj_asm1_tac
  >- (
    unabbrev_all_tac >>
    irule okpath_unfold >> rw [] >>
    qexists_tac `\x.T` >> rw [get_next_step_def] >>
    qabbrev_tac `P = (\s2 l. sem_step p s l s2 ∧ ¬last_step p s l s2)` >>
    `P s' l` by (irule some_lemma >> simp [Abbr `P`]) >>
    pop_assum mp_tac >> simp [Abbr `P`]) >>
  `∃n. length path1 = Some n` by fs [finite_length] >>
  `n ≠ 0` by metis_tac [length_never_zero] >>
  `length (plink path path1) = Some (Suc m + n - 1)` by metis_tac [length_plink] >>
  simp [take_pconcat, PL_def, finite_pconcat, length_plink] >>
  `!l s. length (pconcat (plink path path1) l (stopped_at s)) = Some ((Suc m + n − 1) + 1)`
  by metis_tac [length_pconcat, alt_length_thm] >>
  simp [GSYM PULL_EXISTS] >>
  unabbrev_all_tac >> drule unfold_last >>
  qmatch_goalsub_abbrev_tac `last_step _ (last path1) _ _` >>
  simp [Once get_next_step_def, some_def, FORALL_PROD] >>
  strip_tac >>
  simp [CONJ_ASSOC, Once CONJ_SYM] >>
  simp [GSYM CONJ_ASSOC] >>
  conj_tac
  >- (
    rw [take_plink]
    >- (imp_res_tac take_all >> fs []) >>
    metis_tac [finite_plink_trivial]) >>
  pop_assum mp_tac >>
  Cases_on `1 ∈ PL path1` >> simp []
  >- (
    simp [get_next_step_def] >> strip_tac >>
    qabbrev_tac `P = (\s2 l. sem_step p x l s2 ∧ ¬last_step p x l s2)` >>
    `P (last path1) l` by (irule some_lemma >> simp [Abbr `P`]) >>
    pop_assum mp_tac >> simp [Abbr `P`] >>
    strip_tac >>
    drule sem_step_not_last >> rw []
    >- fs [sem_step_cases] >>
    metis_tac [])
  >- (
    `n = 1` by (rfs [PL_def, finite_length] >> decide_tac) >>
    qspec_then `path1` strip_assume_tac path_cases
    >- (
      unabbrev_all_tac >> simp [] >>
      fs [] >> fs [Once unfold_thm] >>
      Cases_on `get_next_step p (last path)` >> simp [] >> fs [] >> rw [] >>
      fs [get_next_step_def, some_def, FORALL_PROD] >>
      TRY split_pair_case_tac >> fs [sem_step_cases] >>
      metis_tac [])
    >- fs [alt_length_thm, length_never_zero])
QED

Theorem find_path_prefix:
  ∀path.
     okpath (sem_step p) path ∧ finite path
    ⇒
    !obs l1. toList (labels path) = Some l1 ∧
    obs ∈ observation_prefixes ((last path).status, l1)
    ⇒
    ∃n l2. n ∈ PL path ∧ toList (labels (take n path)) = Some l2 ∧
      obs = ((last (take n path)).status, filter ($≠ Tau) l2)
Proof
  ho_match_mp_tac finite_okpath_ind >> rw [toList_THM]
  >- fs [observation_prefixes_cases, IN_DEF] >>
  `∃s ls. obs = (s, ls)` by metis_tac [pairTheory.pair_CASES] >>
  fs [] >>
  `∃l. length path = Some l ∧ l ≠ 0` by metis_tac [finite_length, length_never_zero] >>
  `take (l-1) path = path` by metis_tac [take_all] >>
  Cases_on `s` >> fs []
  >- (
    qexists_tac `l` >> rw [toList_THM] >>
    Cases_on `l` >> fs [toList_THM] >>
    fs [observation_prefixes_cases, IN_DEF, PL_def])
  >- (
    qexists_tac `l` >> rw [toList_THM] >>
    Cases_on `l` >> fs [toList_THM] >>
    fs [observation_prefixes_cases, IN_DEF, PL_def]) >>
  qpat_x_assum `(Partial, _) ∈ _` mp_tac >>
  simp [observation_prefixes_cases, Once IN_DEF] >> rw [] >>
  rename1 `short_l ≼ first_l::long_l` >>
  Cases_on `short_l` >> fs []
  >- (
    qexists_tac `0` >> rw [toList_THM] >>
    fs [sem_step_cases]) >>
  rename1 `short_l ≼ long_l` >>
  rfs [] >>
  `(Partial, filter ($≠ Tau) short_l) ∈ observation_prefixes ((last path).status,long_l)`
  by (simp [observation_prefixes_cases, IN_DEF] >> metis_tac []) >>
  first_x_assum drule >> strip_tac >>
  qexists_tac `Suc n` >> simp [toList_THM] >> rw [] >> rfs [last_take]
QED

Triviality is_prefix_lem:
  ∀l1 l2 l3. l1 ≼ l2 ⇒ l1 ≼ l2 ++ l3
Proof
  Induct >> rw [] >> fs [] >>
  Cases_on `l2` >> fs []
QED

Theorem big_sem_equiv:
  ∀p s1. multi_step_sem p s1 = sem p s1
Proof
  rw [multi_step_sem_def, sem_def, EXTENSION] >> eq_tac >> rw []
  >- (
    drule expand_multi_step_path >> rw [] >>
    rename [`toList (labels m_path) = Some m_l`, `toList (labels s_path) = Some (flat m_l)`] >>
    `∃n short_l.
      n ∈ PL s_path ∧
      toList (labels (take n s_path)) = Some short_l ∧
      x = ((last (take n s_path)).status, filter ($≠ Tau) short_l)`
    by metis_tac [find_path_prefix] >>
    qexists_tac `take n s_path` >> rw [])
  >- (
    Cases_on `¬∀s. path = stopped_at s ⇒ ∃s' l. sem_step p s l s'`
    >- (
      fs [] >> rw [] >> fs [toList_THM] >> rw [] >>
      qexists_tac `stopped_at s` >> rw [toList_THM] >>
      rw [observation_prefixes_cases, IN_DEF] >>
      metis_tac [trace_type_nchotomy]) >>
    drule extend_step_path >> disch_then drule >>
    impl_tac >> rw []
    >- metis_tac [] >>
    rename1 `last_step _ (last s_ext_path) last_l last_s` >>
    `∃s_ext_l. toList (labels s_ext_path) = Some s_ext_l` by metis_tac [LFINITE_toList, finite_labels] >>
    qabbrev_tac `orig_path = take n (pconcat s_ext_path last_l (stopped_at last_s))` >>
    drule contract_step_path >> simp [] >> disch_then drule >> rw [] >>
    rename [`toList (labels m_path) = Some m_l`,
            `toList (labels s_ext_path) = Some s_ext_l`,
            `first m_path = first s_ext_path`,
            `okpath (multi_step _) m_path`] >>
    qexists_tac `m_path` >> rw [] >>
    TRY (rw [Abbr `orig_path`] >> NO_TAC) >>
    rfs [last_take, take_pconcat] >>
    Cases_on `length s_ext_path = Some n`
    >- (
      rfs [PL_def] >> fs [] >>
      rw [observation_prefixes_cases, IN_DEF] >> rw [] >>
      unabbrev_all_tac >> rw [last_pconcat] >> fs [] >>
      drule toList_LAPPEND_APPEND >> rw [toList_THM] >>
      Cases_on `(last m_path).status` >> simp [] >>
      qexists_tac `s_ext_l ++ [last_l]` >> rw []) >>
    fs [PL_def, finite_pconcat] >> rfs [] >>
    `∃m. length s_ext_path = Some m` by metis_tac [finite_length] >>
    `length s_ext_path = Some m` by metis_tac [finite_length] >>
    `length (pconcat s_ext_path last_l (stopped_at (last m_path))) = Some (m + 1)`
    by metis_tac [length_pconcat, alt_length_thm] >>
    fs [] >>
    `n < m` by decide_tac >> fs [] >> rw [] >>
    `n ∈ PL s_ext_path` by rw [PL_def] >>
    Cases_on `(last orig_path).status = Partial`
    >- (
      rw [observation_prefixes_cases, IN_DEF] >> rw [] >>
      unabbrev_all_tac >> fs [] >>
      `LTAKE n (labels s_ext_path) = Some l` by metis_tac [LTAKE_labels] >>
      fs [toList_some] >> rfs [] >>
      Cases_on `m` >> fs [length_labels] >>
      qexists_tac `l` >> rw [] >> rfs []
      >- (
        irule is_prefix_lem >>
        `n ≤ length s_ext_l` by decide_tac >>
        fs [ltake_fromList2] >>
        rw [take_is_prefix])
      >- (drule LTAKE_LENGTH >> rw [])) >>
    unabbrev_all_tac >> rfs [last_take] >>
    fs [okpath_pointwise] >>
    Cases_on `Suc n ∈ PL s_ext_path` >> rw []
    >- (last_x_assum (qspec_then `n` mp_tac) >> rw [sem_step_cases]) >>
    `n = m - 1` by (fs [PL_def] >> rfs []) >>
    rw [] >>
    `el (m - 1) s_ext_path = last s_ext_path` by metis_tac [take_all, pathTheory.last_take] >>
    fs [last_step_cases])
QED

export_theory ();
