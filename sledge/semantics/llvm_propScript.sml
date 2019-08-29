(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Properties of the mini-LLVM model *)

open HolKernel boolLib bossLib Parse;
open pairTheory listTheory rich_listTheory arithmeticTheory wordsTheory;
open pred_setTheory finite_mapTheory;
open logrootTheory numposrepTheory;
open settingsTheory miscTheory llvmTheory memory_modelTheory;

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
  fs [unconvert_value_def, convert_value_def, value_type_cases] >>
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

Theorem allocate_invariant:
  ∀p s1 v1 t v2 h2.
    state_invariant p s1 ∧ allocate s1.heap v1 t (v2,h2) ⇒ state_invariant p (s1 with heap := h2)
Proof
  rw [state_invariant_def, ip_ok_def, globals_ok_def, stack_ok_def]
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

Theorem step_instr_invariant:
  ∀i s2.
    step_instr p s1 i s2 ⇒ prog_ok p ∧ next_instr p s1 i ∧ state_invariant p s1
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
      >- metis_tac [optionTheory.NOT_IS_SOME_EQ_NONE]
      >- metis_tac [optionTheory.NOT_IS_SOME_EQ_NONE] >>
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
      rw [ip_ok_def] >> fs [prog_ok_def, NOT_NIL_EQ_LENGTH_NOT_0] >>
      qpat_x_assum `alookup _ (Fn "main") = _` kall_tac >>
      last_x_assum drule >> disch_then drule >> fs [])
    >- (fs [globals_ok_def] >> metis_tac [])
    >- (fs [stack_ok_def, frame_ok_def, EVERY_MEM] >> metis_tac []))
  >- ( (* Br *)
    fs [state_invariant_def] >> rw []
    >- (
      rw [ip_ok_def] >> fs [prog_ok_def, NOT_NIL_EQ_LENGTH_NOT_0] >>
      qpat_x_assum `alookup _ (Fn "main") = _` kall_tac >>
      last_x_assum drule >> disch_then drule >> fs [])
    >- (fs [globals_ok_def] >> metis_tac [])
    >- (fs [stack_ok_def, frame_ok_def, EVERY_MEM] >> metis_tac []))
  >- (
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant]>>
    metis_tac [terminator_def])
  >- (
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant] >>
    metis_tac [terminator_def])
  >- (
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant] >>
    metis_tac [terminator_def])
  >- ( (* Allocation *)
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant]
    >- metis_tac [allocate_invariant]
    >- (fs [next_instr_cases, allocate_cases] >> metis_tac [terminator_def]))
  >- (
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant] >>
    fs [next_instr_cases] >>
    metis_tac [terminator_def])
  >- ( (* Store *)
    irule inc_pc_invariant >> rw [next_instr_update, update_invariant]
    >- (irule set_bytes_invariant >> rw [] >> metis_tac [])
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
  >- ( (* Call *)
    rw [state_invariant_def]
    >- (fs [prog_ok_def, ip_ok_def] >> metis_tac [NOT_NIL_EQ_LENGTH_NOT_0])
    >- (fs [state_invariant_def, heap_ok_def] >> metis_tac [])
    >- (fs [state_invariant_def, globals_ok_def] >> metis_tac [])
    >- (
      fs [state_invariant_def, stack_ok_def] >> rw []
      >- (
        rw [frame_ok_def] >> fs [ip_ok_def, prog_ok_def] >>
        last_x_assum drule >> disch_then drule >> rw [] >>
        CCONTR_TAC >> fs [] >> rfs [LAST_EL] >>
        Cases_on `length block'.body = s1.ip.i + 1` >> fs [PRE_SUB1] >>
        fs [next_instr_cases] >>
        metis_tac [terminator_def])
      >- (fs [EVERY_MEM, frame_ok_def] >> metis_tac [])))
QED

(* ----- Initial state is ok ----- *)

Theorem init_invariant:
  ∀p s init. prog_ok p ∧ is_init_state s init ⇒ state_invariant p s
Proof
  rw [is_init_state_def, state_invariant_def]
  >- (rw [ip_ok_def] >> fs [prog_ok_def] >> metis_tac [NOT_NIL_EQ_LENGTH_NOT_0])
  >- rw [stack_ok_def]
QED

export_theory ();
