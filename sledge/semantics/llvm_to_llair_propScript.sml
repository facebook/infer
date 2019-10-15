(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Proofs about llvm to llair translation *)

open HolKernel boolLib bossLib Parse lcsymtacs;
open listTheory arithmeticTheory pred_setTheory finite_mapTheory wordsTheory integer_wordTheory;
open rich_listTheory pathTheory;
open settingsTheory miscTheory memory_modelTheory;
open llvmTheory llvm_propTheory llvm_ssaTheory llairTheory llair_propTheory llvm_to_llairTheory;

new_theory "llvm_to_llair_prop";

set_grammar_ancestry ["llvm", "llair", "llair_prop", "llvm_to_llair", "llvm_ssa"];

numLib.prefer_num ();

Inductive v_rel:
  (∀w. v_rel (FlatV (PtrV w)) (FlatV (IntV (w2i w) llair$pointer_size))) ∧
  (∀w. v_rel (FlatV (W1V w)) (FlatV (IntV (w2i w) 1))) ∧
  (∀w. v_rel (FlatV (W8V w)) (FlatV (IntV (w2i w) 8))) ∧
  (∀w. v_rel (FlatV (W32V w)) (FlatV (IntV (w2i w) 32))) ∧
  (∀w. v_rel (FlatV (W64V w)) (FlatV (IntV (w2i w) 64))) ∧
  (∀vs1 vs2.
    list_rel v_rel vs1 vs2
    ⇒
    v_rel (AggV vs1) (AggV vs2))
End

Definition take_to_call_def:
  (take_to_call [] = []) ∧
  (take_to_call (i::is) =
    if terminator i ∨ is_call i then [i] else i :: take_to_call is)
End

Definition dest_llair_lab_def:
  dest_llair_lab (Lab_name f b) = (f, b)
End

Inductive pc_rel:
 (* LLVM side points to a normal instruction *)
 (∀prog emap ip bp d b idx b' prev_i fname.
    (* Both are valid pointers to blocks n the same function *)
    dest_fn ip.f = fst (dest_llair_lab bp) ∧
    alookup prog ip.f = Some d ∧
    alookup d.blocks ip.b = Some b ∧
    ip.i = Offset idx ∧
    idx < length b.body ∧
    get_block (translate_prog prog) bp b' ∧
    (* The LLVM side is at the start of a block, or immediately following a
     * call, which will also start a new block in llair *)
    (ip.i ≠ Offset 0 ⇒ get_instr prog (ip with i := Offset (idx - 1)) (Inl prev_i) ∧ is_call prev_i) ∧
    ip.f = Fn fname ∧
    (∃regs_to_keep.
      b' = fst (translate_instrs fname emap regs_to_keep (take_to_call (drop idx b.body))))
    ⇒
    pc_rel prog emap ip bp) ∧

 (* If the LLVM side points to phi instructions, the llair side
  * should point to a block generated from them *)
 (∀prog emap ip bp b from_l phis.
    get_instr prog ip (Inr (from_l, phis)) ∧
    (* TODO: constrain b to be generated from the phis *)
    get_block (translate_prog prog) bp b
    ⇒
    pc_rel prog emap ip bp)
End

Definition untranslate_reg_def:
  untranslate_reg (Var_name x t) = Reg x
End

(* Define when an LLVM state is related to a llair one.
 * Parameterised on a map for locals relating LLVM registers to llair
 * expressions that compute the value in that register. This corresponds to part
 * of the translation's state.
 *)
Definition mem_state_rel_def:
  mem_state_rel prog emap (s:llvm$state) (s':llair$state) ⇔
    (* Live LLVM registers are mapped and have a related value in the emap
     * (after evaluating) *)
    (∀r. r ∈ live prog s.ip ⇒
      (∃v v' e.
        v_rel v.value v' ∧
        flookup s.locals r = Some v ∧
        flookup emap r = Some e ∧ eval_exp s' e v' ∧
        (* Each register used in e is dominated by an assignment to that
         * register for the entire live range of r. *)
        (∀ip1 r'. ip1.f = s.ip.f ∧ r ∈ live prog ip1 ∧ r' ∈ exp_uses e ⇒
          ∃ip2. untranslate_reg r' ∈ assigns prog ip2 ∧ dominates prog ip2 ip1))) ∧
    reachable prog s.ip ∧
    erase_tags s.heap = s'.heap ∧
    s'.status = get_observation prog s
End

(* Define when an LLVM state is related to a llair one
 * Parameterised on a map for locals relating LLVM registers to llair
 * expressions that compute the value in that register. This corresponds to part
 * of the translation's state.
 *)
Definition state_rel_def:
  state_rel prog emap (s:llvm$state) (s':llair$state) ⇔
    pc_rel prog emap s.ip s'.bp ∧
    mem_state_rel prog emap s s'
End

Theorem mem_state_ignore_bp:
  ∀prog emap s s' b. mem_state_rel prog emap s (s' with bp := b) ⇔ mem_state_rel prog emap s s'
Proof
  rw [mem_state_rel_def] >> eq_tac >> rw [] >>
  first_x_assum drule >> rw [] >>
  `eval_exp (s' with bp := b) e v' ⇔ eval_exp s' e v'`
  by (irule eval_exp_ignores >> rw []) >>
  metis_tac []
QED

Theorem mem_state_rel_no_update:
  ∀prog emap s1 s1' v res_v r i i'.
  assigns prog s1.ip = {} ∧
  mem_state_rel prog emap s1 s1' ∧
  v_rel v.value res_v ∧
  i ∈ next_ips prog s1.ip
  ⇒
  mem_state_rel prog emap (s1 with ip := i) s1'
Proof
  rw [mem_state_rel_def]
  >- (
    first_x_assum (qspec_then `r` mp_tac) >> simp [Once live_gen_kill, PULL_EXISTS] >>
    metis_tac [next_ips_same_func])
  >- metis_tac [next_ips_reachable]
  >- cheat
QED

Theorem mem_state_rel_update:
  ∀prog emap s1 s1' v res_v r e i.
  is_ssa prog ∧
  assigns prog s1.ip = {r} ∧
  mem_state_rel prog emap s1 s1' ∧
  eval_exp s1' e res_v ∧
  v_rel v.value res_v ∧
  i ∈ next_ips prog s1.ip ∧
  (∀r_use. r_use ∈ exp_uses e ⇒
    ∃r_tmp. r_use ∈ exp_uses (translate_arg emap (Variable r_tmp)) ∧ r_tmp ∈ live prog s1.ip)
  ⇒
  mem_state_rel prog (emap |+ (r, e))
        (s1 with <|ip := i; locals := s1.locals |+ (r, v) |>)
        s1'
Proof
  rw [mem_state_rel_def]
  >- (
    rw [FLOOKUP_UPDATE]
    >- (
      HINT_EXISTS_TAC >> rw [] >>
      first_x_assum drule >> rw [] >>
      first_x_assum drule >> rw [] >>
      fs [exp_uses_def, translate_arg_def] >>
      pop_assum (qspec_then `s1.ip` mp_tac) >> simp [] >>
      disch_then drule >> rw [] >>
      `dominates prog s1.ip ip1`
      by (
        irule ssa_dominates_live_range_lem >> rw [] >>
        metis_tac [next_ips_same_func]) >>
      metis_tac [dominates_trans]) >>
    `i.f = s1.ip.f` by metis_tac [next_ips_same_func] >> simp [] >>
    first_x_assum irule >>
    simp [Once live_gen_kill, PULL_EXISTS, METIS_PROVE [] ``x ∨ y ⇔ (~y ⇒ x)``] >>
    metis_tac [])
  >- metis_tac [next_ips_reachable]
  >- cheat
QED

Theorem mem_state_rel_update_keep:
  ∀prog emap s1 s1' v res_v r i ty.
  is_ssa prog ∧
  assigns prog s1.ip = {r} ∧
  mem_state_rel prog emap s1 s1' ∧
  v_rel v.value res_v ∧
  reachable prog s1.ip ∧
  i ∈ next_ips prog s1.ip
  ⇒
  mem_state_rel prog (emap |+ (r, Var (translate_reg r ty)))
        (s1 with <|ip := i; locals := s1.locals |+ (r, v)|>)
        (s1' with locals := s1'.locals |+ (translate_reg r ty, res_v))
Proof
  rw [mem_state_rel_def]
  >- (
    rw [FLOOKUP_UPDATE]
    >- (
      simp [Once eval_exp_cases] >>
      qexists_tac `res_v` >> rw [exp_uses_def] >>
      rw [FLOOKUP_UPDATE] >>
      Cases_on `r` >> simp [translate_reg_def, untranslate_reg_def] >>
      `∃ip. ip.f = ip1.f ∧ Reg s ∈ uses prog ip`
      by (
        qabbrev_tac `x = (ip1.f = i.f)` >>
        fs [live_def] >> qexists_tac `last (ip1::path')` >> rw [] >>
        irule good_path_same_func >>
        qexists_tac `ip1::path'` >> rw [MEM_LAST] >>
        metis_tac []) >>
      metis_tac [ssa_dominates_live_range]) >>
    first_x_assum (qspec_then `r'` mp_tac) >>
    simp [Once live_gen_kill, PULL_EXISTS] >>
    impl_tac >> rw []
    >- metis_tac [] >>
    ntac 3 HINT_EXISTS_TAC >> rw []
    >- (
      `DRESTRICT (s1' with locals := s1'.locals |+ (translate_reg r ty,res_v)).locals (exp_uses e) =
       DRESTRICT s1'.locals (exp_uses e)`
      suffices_by metis_tac [eval_exp_ignores_unused] >>
      rw [] >>
      first_x_assum (qspecl_then [`s1.ip`, `translate_reg r ty`] mp_tac) >> simp [Once live_gen_kill] >>
      impl_tac >- metis_tac [] >> rw [] >>
      `ip2 = s1.ip`
      by (
        fs [is_ssa_def, EXTENSION, IN_DEF] >>
        Cases_on `r` >> fs [translate_reg_def, untranslate_reg_def] >>
        metis_tac [reachable_dominates_same_func]) >>
      metis_tac [dominates_irrefl])
    >- (
      first_x_assum irule >> rw [] >>
      metis_tac [next_ips_same_func]))
  >- metis_tac [next_ips_reachable]
  >- cheat
QED

Theorem v_rel_bytes:
  ∀v v'. v_rel v v' ⇒ llvm_value_to_bytes v = llair_value_to_bytes v'
Proof
  ho_match_mp_tac v_rel_ind >>
  rw [v_rel_cases, llvm_value_to_bytes_def, llair_value_to_bytes_def] >>
  rw [value_to_bytes_def, llvmTheory.unconvert_value_def, w2n_i2n,
      llairTheory.unconvert_value_def, llairTheory.pointer_size_def,
      llvmTheory.pointer_size_def] >>
  pop_assum mp_tac >>
  qid_spec_tac `vs1` >>
  Induct_on `vs2` >> rw [] >> rw []
QED

Theorem translate_constant_correct_lem:
  (∀c s prog emap s' (g : glob_var |-> β # word64).
   mem_state_rel prog emap s s'
   ⇒
   ∃v'. eval_exp s' (translate_const c) v' ∧ v_rel (eval_const g c) v') ∧
  (∀(cs : (ty # const) list) s prog emap s' (g : glob_var |-> β # word64).
   mem_state_rel prog emap s s'
   ⇒
   ∃v'. list_rel (eval_exp s') (map (translate_const o snd) cs) v' ∧ list_rel v_rel (map (eval_const g o snd) cs) v') ∧
  (∀(tc : ty # const) s prog emap s' (g : glob_var |-> β # word64).
   mem_state_rel prog emap s s'
   ⇒
   ∃v'. eval_exp s' (translate_const (snd tc)) v' ∧ v_rel (eval_const g (snd tc)) v')
Proof
  ho_match_mp_tac const_induction >> rw [translate_const_def] >>
  simp [Once eval_exp_cases, eval_const_def]
  >- (
    Cases_on `s` >> simp [eval_const_def, translate_size_def, v_rel_cases] >>
    metis_tac [truncate_2comp_i2w_w2i, dimindex_1, dimindex_8, dimindex_32, dimindex_64])
  >- (
    simp [v_rel_cases, PULL_EXISTS, MAP_MAP_o] >>
    fs [combinTheory.o_DEF, pairTheory.LAMBDA_PROD] >>
    metis_tac [])
  >- (
    simp [v_rel_cases, PULL_EXISTS, MAP_MAP_o] >>
    fs [combinTheory.o_DEF, pairTheory.LAMBDA_PROD] >>
    metis_tac [])
  >- cheat
  >- cheat
  >- cheat
  >- cheat
QED

Theorem translate_constant_correct:
  ∀c s prog emap s' g.
   mem_state_rel prog emap s s'
   ⇒
   ∃v'. eval_exp s' (translate_const c) v' ∧ v_rel (eval_const g c) v'
Proof
  metis_tac [translate_constant_correct_lem]
QED

Theorem translate_const_no_reg[simp]:
  ∀c. r ∉ exp_uses (translate_const c)
Proof
  ho_match_mp_tac translate_const_ind >>
  rw [translate_const_def, exp_uses_def, MEM_MAP, METIS_PROVE [] ``x ∨ y ⇔ (~x ⇒ y)``] >>
  TRY pairarg_tac >> fs []
  >- metis_tac []
  >- metis_tac [] >>
  cheat
QED

Theorem translate_arg_correct:
  ∀s a v prog emap s'.
  mem_state_rel prog emap s s' ∧
  eval s a = Some v ∧
  arg_to_regs a ⊆ live prog s.ip
  ⇒
  ∃v'. eval_exp s' (translate_arg emap a) v' ∧ v_rel v.value v'
Proof
  Cases_on `a` >> rw [eval_def, translate_arg_def] >> rw []
  >- metis_tac [translate_constant_correct] >>
  CASE_TAC >> fs [PULL_EXISTS, mem_state_rel_def, arg_to_regs_def] >>
  res_tac >> rfs [] >> metis_tac []
QED

Theorem is_allocated_mem_state_rel:
  ∀prog emap s1 s1'.
    mem_state_rel prog emap s1 s1'
    ⇒
    (∀i. is_allocated i s1.heap ⇔ is_allocated i s1'.heap)
Proof
  rw [mem_state_rel_def, is_allocated_def, erase_tags_def] >>
  pop_assum mp_tac >> pop_assum (mp_tac o GSYM) >> rw []
QED

Theorem restricted_i2w_11:
  ∀i (w:'a word). INT_MIN (:'a) ≤ i ∧ i ≤ INT_MAX (:'a) ⇒ (i2w i : 'a word) = i2w (w2i w) ⇒ i = w2i w
Proof
  rw [i2w_def]
  >- (
    Cases_on `n2w (Num (-i)) = INT_MINw` >>
    rw [w2i_neg, w2i_INT_MINw] >>
    fs [word_L_def] >>
    `?j. 0 ≤ j ∧ i = -j` by intLib.COOPER_TAC >>
    rw [] >>
    fs [] >>
    `INT_MIN (:'a) < dimword (:'a)` by metis_tac [INT_MIN_LT_DIMWORD] >>
    `Num j MOD dimword (:'a) = Num j`
    by (irule LESS_MOD >> intLib.COOPER_TAC) >>
    fs []
    >- intLib.COOPER_TAC
    >- (
      `Num j < INT_MIN (:'a)` by intLib.COOPER_TAC >>
      fs [w2i_n2w_pos, integerTheory.INT_OF_NUM]))
  >- (
    fs [GSYM INT_MAX, INT_MAX_def] >>
    `Num i < INT_MIN (:'a)` by intLib.COOPER_TAC >>
    rw [w2i_n2w_pos, integerTheory.INT_OF_NUM] >>
    intLib.COOPER_TAC)
QED

Theorem translate_sub_correct:
  ∀prog emap s1 s1' nsw nuw ty v1 v1' v2 v2' e2' e1' result.
    mem_state_rel prog emap s1 s1' ∧
    do_sub nuw nsw v1 v2 ty = Some result ∧
    eval_exp s1' e1' v1' ∧
    v_rel v1.value v1' ∧
    eval_exp s1' e2' v2' ∧
    v_rel v2.value v2'
    ⇒
    ∃v3'.
      eval_exp s1' (Sub (translate_ty ty) e1' e2') v3' ∧
      v_rel result.value v3'
Proof
  rw [] >>
  simp [Once eval_exp_cases] >>
  fs [do_sub_def] >> rw [] >>
  rfs [v_rel_cases] >> rw [] >> fs [] >>
  BasicProvers.EVERY_CASE_TAC >> fs [PULL_EXISTS, translate_ty_def, translate_size_def] >>
  pairarg_tac >> fs [] >>
  fs [pairTheory.PAIR_MAP, wordsTheory.FST_ADD_WITH_CARRY] >>
  rw [] >>
  qmatch_goalsub_abbrev_tac `w2i (-1w * w1 + w2)` >>
  qexists_tac `w2i w2` >> qexists_tac `w2i w1` >> simp [] >>
  unabbrev_all_tac >> rw []
  >- (
    irule restricted_i2w_11 >> simp [word_sub_i2w] >>
    `dimindex (:1) = 1` by rw [] >>
    drule truncate_2comp_i2w_w2i >>
    rw [word_sub_i2w] >>
    metis_tac [w2i_ge, w2i_le, SIMP_CONV (srw_ss()) [] ``INT_MIN (:1)``,
               SIMP_CONV (srw_ss()) [] ``INT_MAX (:1)``])
  >- (
    irule restricted_i2w_11 >> simp [word_sub_i2w] >>
    `dimindex (:8) = 8` by rw [] >>
    drule truncate_2comp_i2w_w2i >>
    rw [word_sub_i2w] >>
    metis_tac [w2i_ge, w2i_le, SIMP_CONV (srw_ss()) [] ``INT_MIN (:8)``,
               SIMP_CONV (srw_ss()) [] ``INT_MAX (:8)``])
  >- (
    irule restricted_i2w_11 >> simp [word_sub_i2w] >>
    `dimindex (:32) = 32` by rw [] >>
    drule truncate_2comp_i2w_w2i >>
    rw [word_sub_i2w] >>
    metis_tac [w2i_ge, w2i_le, SIMP_CONV (srw_ss()) [] ``INT_MIN (:32)``,
               SIMP_CONV (srw_ss()) [] ``INT_MAX (:32)``])
  >- (
    irule restricted_i2w_11 >> simp [word_sub_i2w] >>
    `dimindex (:64) = 64` by rw [] >>
    drule truncate_2comp_i2w_w2i >>
    rw [word_sub_i2w] >>
    metis_tac [w2i_ge, w2i_le, SIMP_CONV (srw_ss()) [] ``INT_MIN (:64)``,
               SIMP_CONV (srw_ss()) [] ``INT_MAX (:64)``])
QED

Theorem translate_extract_correct:
  ∀prog emap s1 s1' a v v1' e1' cs ns result.
    mem_state_rel prog emap s1 s1' ∧
    map (λci. signed_v_to_num (eval_const s1.globals ci)) cs = map Some ns ∧
    extract_value v ns = Some result ∧
    eval_exp s1' e1' v1' ∧
    v_rel v v1'
    ⇒
    ∃v2'.
      eval_exp s1' (foldl (λe c. Select e (translate_const c)) e1' cs) v2' ∧
      v_rel result v2'
Proof
  Induct_on `cs` >> rw [] >> fs [extract_value_def]
  >- metis_tac [] >>
  first_x_assum irule >>
  Cases_on `ns` >> fs [] >>
  qmatch_goalsub_rename_tac `translate_const c` >>
  `?v2'. eval_exp s1' (translate_const c) v2' ∧ v_rel (eval_const s1.globals c) v2'`
  by metis_tac [translate_constant_correct] >>
  Cases_on `v` >> fs [extract_value_def] >>
  qpat_x_assum `v_rel (AggV _) _` mp_tac >>
  simp [Once v_rel_cases] >> rw [] >>
  simp [Once eval_exp_cases, PULL_EXISTS] >>
  fs [LIST_REL_EL_EQN] >>
  qmatch_assum_rename_tac `_ = map Some is` >>
  Cases_on `eval_const s1.globals c` >> fs [signed_v_to_num_def, signed_v_to_int_def] >> rw [] >>
  `?i. v2' = FlatV i` by fs [v_rel_cases] >> fs [] >>
  qmatch_assum_rename_tac `option_join _ = Some x` >>
  `?size. i = IntV (&x) size` suffices_by metis_tac [] >> rw [] >>
  qpat_x_assum `v_rel _ _` mp_tac >>
  simp [v_rel_cases] >> rw [] >> fs [signed_v_to_int_def] >> rw [] >>
  intLib.COOPER_TAC
QED

Theorem translate_update_correct:
  ∀prog emap s1 s1' a v1 v1' v2 v2' e2 e2' e1' cs ns result.
    mem_state_rel prog emap s1 s1' ∧
    map (λci. signed_v_to_num (eval_const s1.globals ci)) cs = map Some ns ∧
    insert_value v1 v2 ns = Some result ∧
    eval_exp s1' e1' v1' ∧
    v_rel v1 v1' ∧
    eval_exp s1' e2' v2' ∧
    v_rel v2 v2'
    ⇒
    ∃v3'.
      eval_exp s1' (translate_updatevalue e1' e2' cs) v3' ∧
      v_rel result v3'
Proof
  Induct_on `cs` >> rw [] >> fs [insert_value_def, translate_updatevalue_def]
  >- metis_tac [] >>
  simp [Once eval_exp_cases, PULL_EXISTS] >>
  Cases_on `ns` >> fs [] >>
  Cases_on `v1` >> fs [insert_value_def] >>
  rename [`insert_value (el x _) _ ns`] >>
  Cases_on `insert_value (el x l) v2 ns` >> fs [] >> rw [] >>
  qpat_x_assum `v_rel (AggV _) _` mp_tac >> simp [Once v_rel_cases] >> rw [] >>
  simp [v_rel_cases] >>
  qmatch_goalsub_rename_tac `translate_const c` >>
  qexists_tac `vs2` >> simp [] >>
  `?v4'. eval_exp s1' (translate_const c) v4' ∧ v_rel (eval_const s1.globals c) v4'`
  by metis_tac [translate_constant_correct] >>
  `?idx_size. v4' = FlatV (IntV (&x) idx_size)`
  by (
    pop_assum mp_tac >> simp [Once v_rel_cases] >>
    rw [] >> fs [signed_v_to_num_def, signed_v_to_int_def] >>
    intLib.COOPER_TAC) >>
  first_x_assum drule >>
  disch_then drule >>
  disch_then drule >>
  disch_then (qspecl_then [`el x vs2`, `v2'`, `e2'`, `Select e1' (translate_const c)`] mp_tac) >>
  simp [Once eval_exp_cases] >>
  metis_tac [EVERY2_LUPDATE_same, LIST_REL_LENGTH, LIST_REL_EL_EQN]
QED

Theorem prog_ok_nonterm:
  ∀prog i ip.
    prog_ok prog ∧ get_instr prog ip (Inl i) ∧ ¬terminator i ⇒ inc_pc ip ∈ next_ips prog ip
Proof
  rw [next_ips_cases, IN_DEF, get_instr_cases, PULL_EXISTS] >>
  `terminator (last b.body) ∧ b.body ≠ []` by metis_tac [prog_ok_def] >>
  Cases_on `length b.body = idx + 1`
  >- (
    drule LAST_EL >>
    rw [] >> fs [DECIDE ``PRE (x + 1) = x``]) >>
  Cases_on `el idx b.body` >>
  fs [instr_next_ips_def, terminator_def] >>
  rw [EXISTS_OR_THM, inc_pc_def, inc_bip_def]
QED

Theorem translate_instr_to_exp_correct:
  ∀emap instr r t s1 s1' s2 prog l.
    is_ssa prog ∧
    prog_ok prog ∧
    classify_instr instr = Exp r t ∧
    mem_state_rel prog emap s1 s1' ∧
    get_instr prog s1.ip (Inl instr) ∧
    step_instr prog s1 instr l s2 ⇒
    ∃pv emap' s2'.
      l = Tau ∧
      s2.ip = inc_pc s1.ip ∧
      mem_state_rel prog emap' s2 s2' ∧
      (r ∉ regs_to_keep ⇒ s1' = s2' ∧ emap' = emap |+ (r, translate_instr_to_exp emap instr)) ∧
      (r ∈ regs_to_keep ⇒
       emap' = emap |+ (r,Var (translate_reg r t)) ∧
       step_inst s1' (Move [(translate_reg r t, translate_instr_to_exp emap instr)]) Tau s2')
Proof
  recInduct translate_instr_to_exp_ind >>
  simp [translate_instr_to_exp_def, classify_instr_def] >>
  conj_tac
  >- ( (* Sub *)
    rw [step_instr_cases, get_instr_cases, update_result_def] >>
    qpat_x_assum `Sub _ _ _ _ _ _ = el _ _` (assume_tac o GSYM) >>
    `bigunion (image arg_to_regs {a1; a2}) ⊆ live prog s1.ip`
    by (
      simp [Once live_gen_kill, SUBSET_DEF, uses_cases, IN_DEF, get_instr_cases,
            instr_uses_def] >>
      metis_tac []) >>
    fs [] >>
    first_x_assum (mp_then.mp_then mp_then.Any mp_tac translate_arg_correct) >>
    disch_then drule >> disch_then drule >>
    first_x_assum (mp_then.mp_then mp_then.Any mp_tac translate_arg_correct) >>
    disch_then drule >> disch_then drule >> rw [] >>
    drule translate_sub_correct >> disch_then drule >>
    disch_then (qspecl_then [`v'`, `v''`] mp_tac) >> simp [] >>
    disch_then drule >> disch_then drule >> rw [] >>
    rename1 `eval_exp _ (Sub _ _ _) res_v` >>
    rename1 `r ∈ _` >>
    Cases_on `r ∈ regs_to_keep` >> rw []
    >- (
      simp [step_inst_cases, PULL_EXISTS] >>
      qexists_tac `res_v` >> rw []
      >- simp [inc_pc_def, llvmTheory.inc_pc_def] >>
      rw [update_results_def, GSYM FUPDATE_EQ_FUPDATE_LIST] >>
      simp [llvmTheory.inc_pc_def] >>
      irule mem_state_rel_update_keep >> rw []
      >- rw [assigns_cases, EXTENSION, IN_DEF, get_instr_cases, instr_assigns_def]
      >- (
        drule prog_ok_nonterm >>
        simp [get_instr_cases, PULL_EXISTS] >>
        ntac 3 (disch_then drule) >>
        simp [terminator_def, next_ips_cases, IN_DEF, inc_pc_def])
      >- fs [mem_state_rel_def])
    >- rw [inc_pc_def, llvmTheory.inc_pc_def]
    >- (
      simp [llvmTheory.inc_pc_def] >>
      irule mem_state_rel_update >> rw []
      >- (
        fs [exp_uses_def]
        >| [Cases_on `a1`, Cases_on `a2`] >>
        fs [translate_arg_def] >>
        rename1 `flookup _ r_tmp` >>
        qexists_tac `r_tmp` >> rw [] >>
        simp [Once live_gen_kill] >> disj2_tac >>
        simp [uses_cases, IN_DEF, get_instr_cases, instr_uses_def, arg_to_regs_def])
      >- rw [assigns_cases, EXTENSION, IN_DEF, get_instr_cases, instr_assigns_def]
      >- (
        drule prog_ok_nonterm >>
        simp [get_instr_cases, PULL_EXISTS] >>
        ntac 3 (disch_then drule) >>
        simp [terminator_def, next_ips_cases, IN_DEF, inc_pc_def]) >>
      metis_tac [])) >>
  conj_tac
  >- ( (* Extractvalue *)
    rw [step_instr_cases] >>
    simp [llvmTheory.inc_pc_def, update_result_def, FLOOKUP_UPDATE] >>
    drule translate_extract_correct >> rpt (disch_then drule) >>
    drule translate_arg_correct >> disch_then drule >>
    `arg_to_regs a ⊆ live prog s1.ip`
    by (
      fs [get_instr_cases] >>
      qpat_x_assum `Extractvalue _ _ _ = el _ _` (mp_tac o GSYM) >>
      simp [Once live_gen_kill, SUBSET_DEF, uses_cases, IN_DEF, get_instr_cases,
            instr_uses_def]) >>
    simp [] >> strip_tac >>
    disch_then drule >> simp [] >> rw [] >>
    rename1 `eval_exp _ (foldl _ _ _) res_v` >>
    rw [inc_bip_def, inc_pc_def] >>
    rename1 `r ∈ _` >>
    Cases_on `r ∈ regs_to_keep` >> rw []
    >- (
      simp [step_inst_cases, PULL_EXISTS] >>
      qexists_tac `res_v` >> rw [] >>
      rw [update_results_def] >>
      cheat)
    >- cheat) >>
  conj_tac
  >- ( (* Updatevalue *)
    rw [step_instr_cases] >>
    simp [llvmTheory.inc_pc_def, update_result_def, FLOOKUP_UPDATE] >>
    drule translate_update_correct >> rpt (disch_then drule) >>
    first_x_assum (mp_then.mp_then mp_then.Any mp_tac translate_arg_correct) >>
    disch_then drule >>
    first_x_assum (mp_then.mp_then mp_then.Any mp_tac translate_arg_correct) >>
    disch_then drule >>
    `arg_to_regs a1 ⊆ live prog s1.ip ∧
     arg_to_regs a2 ⊆ live prog s1.ip`
    by (
      fs [get_instr_cases] >>
      qpat_x_assum `Insertvalue _ _ _ _ = el _ _` (mp_tac o GSYM) >>
      ONCE_REWRITE_TAC [live_gen_kill] >>
      simp [SUBSET_DEF, uses_cases, IN_DEF, get_instr_cases,
            instr_uses_def]) >>
   simp [] >> strip_tac >> strip_tac >>
   disch_then (qspecl_then [`v'`, `v''`] mp_tac) >> simp [] >>
   disch_then drule >> disch_then drule >>
   rw [] >>
   rename1 `eval_exp _ (translate_updatevalue _ _ _) res_v` >>
   rw [inc_pc_def, inc_bip_def] >>
   rename1 `r ∈ _` >>
   Cases_on `r ∈ regs_to_keep` >> rw []
    >- (
      simp [step_inst_cases, PULL_EXISTS] >>
      qexists_tac `res_v` >> rw [] >>
      rw [update_results_def] >>
      cheat)
    >- cheat) >>
  cheat
QED

Triviality eval_exp_help:
  (s1 with heap := h).locals = s1.locals
Proof
  rw []
QED

Theorem erase_tags_set_bytes:
  ∀p v l h. erase_tags (set_bytes p v l h) = set_bytes () v l (erase_tags h)
Proof
  Induct_on `v` >> rw [set_bytes_def] >>
  irule (METIS_PROVE [] ``x = y ⇒ f a b c x = f a b c y``) >>
  rw [erase_tags_def]
QED

(*
Theorem translate_instr_to_inst_correct:
  ∀prog emap instr s1 s1' s2.
    classify_instr instr = Non_exp ∧
    state_rel prog emap s1 s1' ∧
    get_instr prog s1.ip instr ∧
    step_instr prog s1 instr s2 ⇒
    ∃s2'.
      step_inst s1' (translate_instr_to_inst emap instr) s2' ∧
      state_rel prog emap s2 s2'

Proof

  rw [step_instr_cases] >>
  fs [classify_instr_def, translate_instr_to_inst_def]
  >- ( (* Load *)
    cheat)
  >- ( (* Store *)
    simp [step_inst_cases, PULL_EXISTS] >>
    drule get_instr_live >> rw [uses_def] >>
    drule translate_arg_correct >> disch_then drule >> disch_then drule >>
    qpat_x_assum `eval _ _ = Some _` mp_tac >>
    drule translate_arg_correct >> disch_then drule >> disch_then drule >>
    rw [] >>
    qpat_x_assum `v_rel (FlatV _) _` mp_tac >> simp [Once v_rel_cases] >> rw [] >>
    HINT_EXISTS_TAC >> rw [] >>
    qexists_tac `freeable` >> rw [] >>
    HINT_EXISTS_TAC >> rw []
    >- metis_tac [v_rel_bytes]
    >- (
      fs [w2n_i2n, pointer_size_def] >>
      metis_tac [v_rel_bytes, is_allocated_state_rel, ADD_COMM]) >>
    fs [state_rel_def] >>
    rw []
    >- cheat
    >- (
      fs [llvmTheory.inc_pc_def] >>
      `r ∈ live prog s1.ip`
      by (
        drule live_gen_kill >>
        rw [next_ips_def, assigns_def, uses_def, inc_pc_def]) >>
      first_x_assum drule >> rw [] >>
      metis_tac [eval_exp_ignores, eval_exp_help])
    >- (
      rw [llvmTheory.inc_pc_def, w2n_i2n, pointer_size_def, erase_tags_set_bytes] >>
      metis_tac[v_rel_bytes]))
  >- cheat
  >- cheat
  >- cheat
QED


    simp [step_inst_cases, PULL_EXISTS] >>
    Cases_on `r` >> simp [translate_reg_def] >>
    drule get_instr_live >> rw [uses_def] >>
    drule translate_arg_correct >> disch_then drule >> disch_then drule >>
    simp [Once v_rel_cases] >> rw [] >>
    qexists_tac `IntV (w2i w) pointer_size` >> rw [] >>
    qexists_tac `freeable` >> rw []
    >- (fs [w2n_i2n, pointer_size_def] >> metis_tac [is_allocated_state_rel]) >>
    fs [state_rel_def] >> rw []
    >- cheat
    >- (
      fs [llvmTheory.inc_pc_def, update_results_def, update_result_def] >>
      rw [] >> fs [FLOOKUP_UPDATE] >> rw []
      >- (
        cheat)
      >- (
        `r ∈ live prog s1.ip`
        by (
          drule live_gen_kill >>
          rw [next_ips_def, assigns_def, uses_def, inc_pc_def]) >>
        first_x_assum drule >> rw [] >>
        qexists_tac `v` >>
        qexists_tac `v'` >>
        qexists_tac `e` >>
        rw []
        metis_tac [eval_exp_ignores, eval_exp_help])


    >- fs [update_results_def, llvmTheory.inc_pc_def, update_result_def]

*)

Definition translate_trace_def:
  (translate_trace types Tau = Tau ) ∧
  (translate_trace types (W gv bytes) = W (translate_glob_var gv (types gv)) bytes)
End

Definition untranslate_glob_var_def:
  untranslate_glob_var (Var_name n ty) = Glob_var n
End

Definition untranslate_trace_def:
  (untranslate_trace Tau = Tau ) ∧
  (untranslate_trace (W gv bytes) = W (untranslate_glob_var gv) bytes)
End

Theorem un_translate_glob_inv:
  ∀x t. untranslate_glob_var (translate_glob_var x t) = x
Proof
  Cases_on `x` >> rw [untranslate_glob_var_def, translate_glob_var_def]
QED

Theorem un_translate_trace_inv:
  ∀x. untranslate_trace (translate_trace types x) = x
Proof
  Cases >> rw [translate_trace_def, untranslate_trace_def] >>
  metis_tac [un_translate_glob_inv]
QED

Theorem translate_instrs_correct1:
  ∀prog s1 tr s2.
    multi_step prog s1 tr s2 ⇒
    !s1' b' emap regs_to_keep d b types idx.
      prog_ok prog ∧
      is_ssa prog ∧
      mem_state_rel prog emap s1 s1' ∧
      alookup prog s1.ip.f = Some d ∧
      alookup d.blocks s1.ip.b = Some b ∧
      s1.ip.i = Offset idx ∧
      b' = fst (translate_instrs (dest_fn s1.ip.f) emap regs_to_keep (take_to_call (drop idx b.body)))
      ⇒
      ∃emap s2' tr'.
        step_block (translate_prog prog) s1' b'.cmnd tr' b'.term s2' ∧
        filter ($≠ Tau) tr' = filter ($≠ Tau) (map (translate_trace types) tr)  ∧
        state_rel prog emap s2 s2'
Proof
  ho_match_mp_tac multi_step_ind >> rw_tac std_ss []
  >- (
    fs [last_step_def]
    >- ( (* Phi (not handled here) *)
      fs [get_instr_cases])
    >- ( (* Terminator *)
      `l = Tau`
      by (
        fs [llvmTheory.step_cases] >>
        `i' = i''` by metis_tac [get_instr_func, sumTheory.INL_11] >>
        fs [step_instr_cases] >> rfs [terminator_def]) >>
      fs [get_instr_cases] >> rw [] >>
      `el idx b.body = el 0 (drop idx b.body)` by rw [EL_DROP] >>
      fs [] >>
      Cases_on `drop idx b.body` >> fs [DROP_NIL] >> rw [] >>
      simp [take_to_call_def, translate_instrs_def] >>
      Cases_on `el idx b.body` >> fs [terminator_def, classify_instr_def, translate_trace_def] >> rw []
      >- ( (* Ret *)
        cheat)
      >- ( (* Br *)
        simp [translate_instr_to_term_def, Once step_block_cases] >>
        simp [step_term_cases, PULL_EXISTS] >>
        fs [llvmTheory.step_cases] >>
        drule get_instr_live >> disch_tac >>
        drule translate_arg_correct >>
        fs [step_instr_cases] >> fs [] >>
        TRY (fs [get_instr_cases] >> NO_TAC) >>
        `a = a'` by fs [get_instr_cases] >>
        disch_then drule >>
        impl_tac
        >- (
          fs [SUBSET_DEF, IN_DEF] >> rfs [uses_cases, get_instr_cases, instr_uses_def] >>
          fs [IN_DEF]) >>
        disch_tac >> fs [] >>
        fs [v_rel_cases, GSYM PULL_EXISTS] >>
        qexists_tac `emap` >> qexists_tac `w2i tf` >> simp [] >> conj_tac
        >- metis_tac [] >>
        Cases_on `s1'.bp` >> fs [dest_llair_lab_def] >>
        rename1 `el _ _ = Br e lab1 lab2` >>
        qexists_tac `dest_fn s1.ip.f` >>
        qexists_tac `if 0 = w2i tf then dest_label lab2 else dest_label lab1` >> simp [] >>
        qpat_abbrev_tac `target = if tf = 0w then l2 else l1` >>
        qpat_abbrev_tac `target' = if 0 = w2i tf then dest_label lab2 else dest_label lab1` >>
        rw [] >>
        `translate_label (dest_fn s1.ip.f) target = Lab_name (dest_fn s1.ip.f) target' `
        by (
          fs [get_instr_cases] >> rw [] >>
          unabbrev_all_tac >> rw [] >> fs [word_0_w2i] >>
          Cases_on `l2` >> Cases_on `l1` >> rw [translate_label_def, dest_label_def] >>
          `0 = w2i (0w:word1)` by rw [word_0_w2i] >>
          fs [w2i_11]) >>
        rw [state_rel_def]
        >- (Cases_on `lab2` >> rw [Abbr `target'`, translate_label_def, dest_label_def])
        >- (Cases_on `lab1` >> rw [Abbr `target'`, translate_label_def, dest_label_def])
        >- (
          rw [pc_rel_cases] >> cheat)
        >- (
          fs [mem_state_rel_def] >> rw []
          >- (
            qpat_x_assum `!r. r ∈ live _ _ ⇒ P r` mp_tac >>
            simp [Once live_gen_kill] >> disch_then (qspec_then `r` mp_tac) >>
            impl_tac >> rw []
            >- (
              rw [PULL_EXISTS] >>
              disj1_tac >>
              qexists_tac `<|f := s1.ip.f; b := Some target; i := Phi_ip s1.ip.b|>` >>
              rw [next_ips_cases, IN_DEF, assigns_cases]
              >- (
                disj1_tac >>
                qexists_tac `Br a l1 l2` >>
                rw [instr_next_ips_def, Abbr `target`] >>
                cheat) >>
              CCONTR_TAC >> fs [] >>
              imp_res_tac get_instr_func >> fs [] >> rw [] >>
              fs [instr_assigns_def])
            >- (
              rpt HINT_EXISTS_TAC >> rw [] >>
              qmatch_goalsub_abbrev_tac `eval_exp s3 _` >>
              `s1'.locals = s3.locals` by fs [Abbr `s3`] >>
              metis_tac [eval_exp_ignores]))
          >- cheat
          >- (
            cheat)))
      >- ( (* Invoke *)
        cheat)
      >- ( (* Unreachable *)
        cheat)
      >- ( (* Exit *)
        cheat)
      >- ( (* Throw *)
        cheat))
    >- ( (* Call *)
      cheat)
    >- ( (* Stuck *)
      cheat))
  >- ( (* Middle of the block *)
    fs [llvmTheory.step_cases] >> TRY (fs [get_instr_cases] >> NO_TAC) >>
    `i' = i` by metis_tac [get_instr_func, sumTheory.INL_11] >> fs [] >>
    rename [`step_instr _ _ _ _ s2`, `state_rel _ _ s3 _`,
            `mem_state_rel _ _ s1 s1'`] >>
    Cases_on `∃r t. classify_instr i = Exp r t` >> fs []
    >- ( (* instructions that compile to expressions *)
      drule translate_instr_to_exp_correct >>
      ntac 5 (disch_then drule) >>
      disch_then (qspec_then `regs_to_keep` mp_tac) >>
      rw [] >> fs [translate_trace_def] >>
      `reachable prog (inc_pc s1.ip)`
      by metis_tac [prog_ok_nonterm, next_ips_reachable, mem_state_rel_def] >>
      first_x_assum drule >>
      simp [inc_pc_def, inc_bip_def] >>
      disch_then (qspecl_then [`regs_to_keep`, `types`] mp_tac) >> rw [] >>
      rename1 `state_rel prog emap3 s3 s3'` >>
      qexists_tac `emap3` >> qexists_tac `s3'` >> rw [] >>
      `take_to_call (drop idx b.body) = i :: take_to_call (drop (idx + 1) b.body)`
      by cheat >>
      simp [translate_instrs_def] >>
      Cases_on `r ∉ regs_to_keep` >> fs [] >> rw []
      >- metis_tac [] >>
      qexists_tac `Tau::tr'` >> rw [] >>
      simp [Once step_block_cases] >> disj2_tac >>
      pairarg_tac >> rw [] >> fs [] >>
      metis_tac [])
    >- ( (* Non-expression instructions *)
      cheat))
QED

Theorem multi_step_to_step_block:
  ∀prog s1 tr s2 s1'.
    prog_ok prog ∧ is_ssa prog ∧
    multi_step prog s1 tr s2 ∧
    state_rel prog emap s1 s1'
    ⇒
    ∃s2' emap2 b tr'.
      get_block (translate_prog prog) s1'.bp b ∧
      step_block (translate_prog prog) s1' b.cmnd tr' b.term s2' ∧
      filter ($≠ Tau) tr' = filter ($≠ Tau) (map (translate_trace types) tr) ∧
      state_rel prog emap2 s2 s2'
Proof
  rw [] >> pop_assum mp_tac >> simp [Once state_rel_def] >> rw [pc_rel_cases]
  >- (
    drule translate_instrs_correct1 >> simp [] >>
    disch_then drule >>
    disch_then (qspecl_then [`regs_to_keep`, `types`] mp_tac) >> simp [] >>
    rw [] >>
    qexists_tac `s2'` >> simp [] >>
    ntac 3 HINT_EXISTS_TAC >>
    rw [] >> fs [dest_fn_def]) >>
  (* Phi nodes *)
  reverse (fs [Once multi_step_cases])
  >- metis_tac [get_instr_func, sumTheory.sum_distinct] >>
  qpat_x_assum `last_step _ _ _ _` mp_tac >>
  simp [last_step_def] >> simp [Once llvmTheory.step_cases] >>
  rw [] >> imp_res_tac get_instr_func >> fs [] >> rw [] >>
  fs [translate_trace_def] >>
  cheat
QED

Theorem step_block_to_multi_step:
  ∀prog s1 s1' tr s2' b.
    state_rel prog emap s1 s1' ∧
    get_block (translate_prog prog) s1'.bp b ∧
    step_block (translate_prog prog) s1' b.cmnd tr b.term s2'
    ⇒
    ∃s2.
      multi_step prog s1 (map untranslate_trace tr) s2 ∧
      state_rel prog emap s2 s2'
Proof
  cheat
QED

Theorem trans_trace_not_tau:
  ∀types. ($≠ Tau) ∘ translate_trace types = ($≠ Tau)
Proof
  rw [FUN_EQ_THM] >> eq_tac >> rw [translate_trace_def] >>
  TRY (Cases_on `y`) >> fs [translate_trace_def]
QED

Theorem untrans_trace_not_tau:
  ∀types. ($≠ Tau) ∘ untranslate_trace = ($≠ Tau)
Proof
  rw [FUN_EQ_THM] >> eq_tac >> rw [untranslate_trace_def] >>
  TRY (Cases_on `y`) >> fs [untranslate_trace_def]
QED

Theorem translate_prog_correct_lem1:
  ∀path.
    okpath (multi_step prog) path ∧ finite path
    ⇒
    ∀emap s1'.
    prog_ok prog ∧
    is_ssa prog ∧
    state_rel prog emap (first path) s1'
    ⇒
    ∃path' emap.
      finite path' ∧
      okpath (step (translate_prog prog)) path' ∧
      first path' = s1' ∧
      LMAP (filter ($≠ Tau)) (labels path') =
      LMAP (map (translate_trace types) o filter ($≠ Tau)) (labels path) ∧
      state_rel prog emap (last path) (last path')
Proof
  ho_match_mp_tac finite_okpath_ind >> rw []
  >- (qexists_tac `stopped_at s1'` >> rw [] >> metis_tac []) >>
  fs [] >>
  drule multi_step_to_step_block >> ntac 3 (disch_then drule) >>
  disch_then (qspec_then `types` mp_tac) >> rw [] >>
  first_x_assum drule >> rw [] >>
  qexists_tac `pcons s1' tr' path'` >> rw [] >>
  rw [FILTER_MAP, combinTheory.o_DEF, trans_trace_not_tau] >>
  HINT_EXISTS_TAC >> simp [] >>
  simp [step_cases] >> qexists_tac `b` >> simp [] >>
  fs [state_rel_def, mem_state_rel_def] >> simp [get_observation_def] >>
  fs [Once multi_step_cases, last_step_def] >> rw [] >>
  metis_tac [get_instr_func, exit_no_step]
QED

Theorem translate_prog_correct_lem2:
  ∀path'.
    okpath (step (translate_prog prog)) path' ∧ finite path'
    ⇒
    ∀s1.
    prog_ok prog ∧
    state_rel prog emap s1 (first path')
    ⇒
    ∃path.
      finite path ∧
      okpath (multi_step prog) path ∧
      first path = s1 ∧
      labels path = LMAP (map untranslate_trace) (labels path') ∧
      state_rel prog emap (last path) (last path')
Proof
  ho_match_mp_tac finite_okpath_ind >> rw []
  >- (qexists_tac `stopped_at s1` >> rw []) >>
  fs [step_cases] >>
  drule step_block_to_multi_step >> ntac 2 (disch_then drule) >> rw [] >>
  first_x_assum drule >> rw [] >>
  qexists_tac `pcons s1 (map untranslate_trace r) path` >> rw []
QED

Theorem translate_global_var_11:
  ∀path.
    okpath (step (translate_prog prog)) path ∧ finite path
    ⇒
    ∀x t1 bytes t2 l.
      labels path = fromList l ∧
      MEM (W (Var_name x t1) bytes) (flat l) ∧
      MEM (W (Var_name x t2) bytes) (flat l)
      ⇒
      t1 = t2
Proof
  cheat
QED

Theorem translate_prog_correct:
  ∀prog s1 s1'.
    prog_ok prog ∧ is_ssa prog ∧
    state_rel prog emap s1 s1'
    ⇒
    multi_step_sem prog s1 = image (I ## map untranslate_trace) (sem (translate_prog prog) s1')
Proof
  rw [sem_def, multi_step_sem_def, EXTENSION] >> eq_tac >> rw []
  >- (
    drule translate_prog_correct_lem1 >> ntac 4 (disch_then drule) >>
    disch_then (qspec_then `types` mp_tac) >> rw [pairTheory.EXISTS_PROD] >>
    PairCases_on `x` >> rw [] >>
    qexists_tac `map (translate_trace types) x1` >> rw []
    >- rw [MAP_MAP_o, combinTheory.o_DEF, un_translate_trace_inv] >>
    qexists_tac `path'` >> rw [] >>
    fs [IN_DEF, observation_prefixes_cases, toList_some] >> rw [] >>
    `?labs. labels path' = fromList labs` by cheat >>
    fs [] >>
    rfs [lmap_fromList, combinTheory.o_DEF, MAP_MAP_o] >>
    simp [FILTER_FLAT, MAP_FLAT, MAP_MAP_o, combinTheory.o_DEF, FILTER_MAP]
    >- fs [state_rel_def, mem_state_rel_def]
    >- fs [state_rel_def, mem_state_rel_def] >>
    rename [`labels path' = fromList l'`, `labels path = fromList l`,
            `state_rel _ _ (last path) (last path')`, `lsub ≼ flat l`] >>
    qexists_tac `map (translate_trace types) lsub` >>
    fs [FILTER_MAP, trans_trace_not_tau] >>
    cheat)
(*
    `INJ (translate_trace types) (set l2' ∪ set (flat l2)) UNIV`
    by (
      simp [INJ_DEF] >> rpt gen_tac >>
      Cases_on `x` >> Cases_on `y` >> simp [translate_trace_def] >>
      Cases_on `a` >> Cases_on `a'` >> simp [translate_glob_var_def]) >>
    fs [INJ_MAP_EQ_IFF, inj_map_prefix_iff] >> rw [] >>
    fs [state_rel_def, mem_state_rel_def])
    *)
  >- (
    fs [toList_some] >>
    drule translate_prog_correct_lem2 >> simp [] >>
    disch_then drule >> rw [] >>
    qexists_tac `path'` >> rw [] >>
    fs [IN_DEF, observation_prefixes_cases, toList_some] >> rw [] >>
    rfs [lmap_fromList] >>
    simp [GSYM MAP_FLAT, FILTER_MAP, untrans_trace_not_tau]
    >- fs [state_rel_def, mem_state_rel_def]
    >- fs [state_rel_def, mem_state_rel_def] >>
    qexists_tac `map untranslate_trace l2'` >>
    simp [GSYM MAP_FLAT, FILTER_MAP, untrans_trace_not_tau] >>
    `INJ untranslate_trace (set l2' ∪ set (flat l2)) UNIV`
    by (
      drule is_prefix_subset >> rw [SUBSET_DEF] >>
      `set l2' ∪ set (flat l2) = set (flat l2)` by (rw [EXTENSION] >> metis_tac []) >>
      simp [] >>
      simp [INJ_DEF] >> rpt gen_tac >>
      Cases_on `x` >> Cases_on `y` >> simp [untranslate_trace_def] >>
      Cases_on `a` >> Cases_on `a'` >> simp [untranslate_glob_var_def] >>
      metis_tac [translate_global_var_11]) >>
    fs [INJ_MAP_EQ_IFF, inj_map_prefix_iff] >> rw [] >>
    fs [state_rel_def, mem_state_rel_def])
QED

export_theory ();
