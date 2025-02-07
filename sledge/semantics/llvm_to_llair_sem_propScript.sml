(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Proofs about llvm to llair translation that do involve the semantics *)

open HolKernel boolLib bossLib Parse lcsymtacs;
open listTheory arithmeticTheory pred_setTheory finite_mapTheory wordsTheory integer_wordTheory;
open optionTheory rich_listTheory pathTheory alistTheory pairTheory sumTheory;
open settingsTheory miscTheory memory_modelTheory;
open llvmTheory llvm_propTheory llvm_ssaTheory llairTheory llair_propTheory llvm_to_llairTheory;
open llvm_to_llair_propTheory;

new_theory "llvm_to_llair_sem_prop";

set_grammar_ancestry ["llvm", "llair", "llair_prop", "llvm_to_llair", "llvm_ssa", "llvm_to_llair_prop"];

numLib.prefer_num ();

Definition translate_trace_def:
  (translate_trace gmap Tau = Tau) ∧
  (translate_trace gmap Error = Error) ∧
  (translate_trace gmap (Exit i) = (Exit i)) ∧
  (translate_trace gmap (W gv bytes) = W (translate_glob_var gmap gv) bytes)
End

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

Inductive pc_rel:
 (* LLVM side points to a normal instruction *)
 (∀prog emap ip bp d b idx b' prev_i gmap (*rest*).
    (* Both are valid pointers to blocks in the same function *)
    dest_fn ip.f = label_to_fname bp ∧
    alookup prog ip.f = Some d ∧
    alookup d.blocks ip.b = Some b ∧
    ip.i = Offset idx ∧
    idx < length b.body ∧
    get_block (translate_prog prog) bp b' ∧
    (* The LLVM side is at the start of a block, or immediately following a
     * call, which will also start a new block in llair *)
    (idx ≠ 0 ⇒ get_instr prog (ip with i := Offset (idx - 1)) (Inl prev_i) ∧
    is_call prev_i) ∧
    (bp, b')::rest =
    fst (translate_instrs (translate_label (dest_fn ip.f) ip.b (num_calls (take idx b.body)))
            gmap emap (get_regs_to_keep d) (take_to_call (drop idx b.body)))
    ⇒
    pc_rel prog gmap emap ip bp) ∧

 (* If the LLVM side points to phi instructions, the llair side
  * should point to a block generated from them *)
 (∀prog gmap emap ip from_l phis to_l bp.
    bp = Mov_name (dest_fn ip.f) (option_map dest_label from_l) to_l ∧
    get_instr prog ip (Inr (from_l, phis)) ∧
    ip.b = Some (Lab to_l) ∧
    (* We should have just jumped here from block from_l *)
    (∃d b. alookup prog ip.f = Some d ∧
       alookup d.blocks from_l = Some b ∧
       ip.b ∈ set (map Some (instr_to_labs (last b.body)))) (*∧
    get_block (translate_prog prog) bp
      (generate_move_block (dest_fn ip.f) gmap emap phis from_l (Lab to_l)) *)
    ⇒
    pc_rel prog gmap emap ip bp)
End

(* Define when an LLVM state is related to a llair one.
 * Parameterised on a map for locals relating LLVM registers to llair
 * expressions that compute the value in that register. This corresponds to part
 * of the translation's state.
 *)

Definition emap_invariant_def:
  emap_invariant prog emap s s' r =
    ∃v v' e.
      v_rel v.value v' ∧
      flookup s.locals r = Some v ∧
      flookup emap r = Some e ∧ eval_exp s' e v'
End

Definition local_state_rel_def:
  local_state_rel prog emap s s' ⇔
    (* Live LLVM registers are mapped and have a related value in the emap
     * (after evaluating) *)
    (∀r. r ∈ live prog s.ip ⇒ emap_invariant prog emap s s' r)
End

Definition globals_rel_def:
  globals_rel gmap gl gl' ⇔
    BIJ (translate_glob_var gmap) (fdom gl) (fdom gl') ∧
    ∀k. k ∈ fdom gl ⇒
      nfits (w2n (snd (gl ' k))) llair$pointer_size ∧
      w2n (snd (gl ' k)) = gl' ' (translate_glob_var gmap k)
End

Definition mem_state_rel_def:
  mem_state_rel prog gmap emap (s:llvm$state) (s':llair$state) ⇔
    local_state_rel prog emap s s' ∧
    reachable prog s.ip ∧
    globals_rel gmap s.globals s'.glob_addrs ∧
    heap_ok s.heap ∧
    erase_tags s.heap = s'.heap ∧
    s.status = s'.status
End

(* Define when an LLVM state is related to a llair one
 * Parameterised on a map for locals relating LLVM registers to llair
 * expressions that compute the value in that register. This corresponds to part
 * of the translation's state.
 *)
Definition state_rel_def:
  state_rel prog gmap emap (s:llvm$state) (s':llair$state) ⇔
    (s.status = Partial ⇒ pc_rel prog gmap emap s.ip s'.bp) ∧
    mem_state_rel prog gmap emap s s'
End

Theorem mem_state_ignore_bp[simp]:
  ∀prog gmap emap s s' b.
    mem_state_rel prog gmap emap s (s' with bp := b) ⇔
    mem_state_rel prog gmap emap s s'
Proof
  rw [local_state_rel_def, mem_state_rel_def, emap_invariant_def] >> eq_tac >> rw [] >>
  first_x_assum drule >> rw [] >>
  `eval_exp (s' with bp := b) e v' ⇔ eval_exp s' e v'`
  by (irule eval_exp_ignores >> rw []) >>
  metis_tac []
QED

Triviality lemma:
  ((s:llair$state) with status := Complete code).locals = s.locals ∧
  ((s:llair$state) with status := Complete code).glob_addrs = s.glob_addrs
Proof
  rw []
QED

Theorem mem_state_rel_exited:
  ∀prog gmap emap s s' code.
    mem_state_rel prog gmap emap s s'
    ⇒
    mem_state_rel prog gmap emap (s with status := Complete code) (s' with status := Complete code)
Proof
  rw [mem_state_rel_def, local_state_rel_def, emap_invariant_def] >>
  metis_tac [eval_exp_ignores, lemma]
QED

Theorem mem_state_rel_no_update:
  ∀prog gmap emap s1 s1' v res_v r i i'.
  assigns prog s1.ip = {} ∧
  mem_state_rel prog gmap emap s1 s1' ∧
  i ∈ next_ips prog s1.ip
  ⇒
  mem_state_rel prog gmap emap (s1 with ip := i) s1'
Proof
  rw [mem_state_rel_def, local_state_rel_def, emap_invariant_def]
  >- (
    first_x_assum (qspec_then `r` mp_tac) >> simp [Once live_gen_kill, PULL_EXISTS] >>
    metis_tac [next_ips_same_func])
  >- metis_tac [next_ips_reachable]
QED

Theorem exp_assigns_sing:
  ∀inst prog ip r t.
    get_instr prog ip (Inl inst) ∧ classify_instr inst = Exp r t ⇒ assigns prog ip = {(r,t)}
Proof
  rw [get_instr_cases, EXTENSION, IN_DEF, assigns_cases, PULL_EXISTS] >>
  Cases_on `el idx b.body` >> fs [classify_instr_def, instr_assigns_def] >>
  Cases_on `p` >> fs [classify_instr_def, instr_assigns_def]
QED

Theorem mem_state_rel_update:
  ∀prog gmap emap s1 s1' regs_to_keep v res_v r e i inst.
  good_emap s1.ip.f prog regs_to_keep gmap emap ∧
  get_instr prog s1.ip (Inl inst) ∧
  classify_instr inst = Exp r t ∧
  r ∉ regs_to_keep ∧
  mem_state_rel prog gmap emap s1 s1' ∧
  eval_exp s1' (translate_instr_to_exp gmap emap inst) res_v ∧
  v_rel v.value res_v ∧
  i ∈ next_ips prog s1.ip
  ⇒
  mem_state_rel prog gmap emap
        (s1 with <|ip := i; locals := s1.locals |+ (r, v) |>)
        s1'
Proof
  rw [mem_state_rel_def, local_state_rel_def, emap_invariant_def, good_emap_def]
  >- (
    rw [FLOOKUP_UPDATE]
    >- (
      HINT_EXISTS_TAC >> rw [] >>
      first_x_assum (qspec_then `s1.ip` mp_tac) >> simp [] >> rw [] >>
      first_x_assum (qspecl_then [`r`, `t`, `inst`] mp_tac) >> rw []) >>
    `i.f = s1.ip.f` by metis_tac [next_ips_same_func] >> simp [] >>
    first_x_assum irule >>
    simp [Once live_gen_kill, PULL_EXISTS, METIS_PROVE [] ``x ∨ y ⇔ (~y ⇒ x)``] >>
    drule exp_assigns_sing >> rw [] >>
    metis_tac [exp_assigns_sing])
  >- metis_tac [next_ips_reachable]
QED

Theorem emap_inv_updates_keep_same_ip1:
  ∀prog emap ip s s' vs res_vs rtys r t.
  list_rel v_rel (map (\v. v.value) vs) res_vs ∧
  length rtys = length vs ∧
  (r,t) ∈ set rtys ∧
  all_distinct (map fst rtys) ∧
  flookup emap r = Some (Var (translate_reg r t) F)
  ⇒
  emap_invariant prog emap
        (s with locals := s.locals |++ zip (map fst rtys, vs))
        (s' with locals := s'.locals |++ zip (map (\(r,ty). translate_reg r ty) rtys, res_vs))
        r
Proof
  rw [emap_invariant_def, flookup_fupdate_list] >>
  CASE_TAC >> rw []
  >- (
    fs [ALOOKUP_NONE, MAP_REVERSE] >> rfs [MAP_ZIP] >> fs [MEM_MAP] >>
    metis_tac [FST]) >>
  rename [`alookup (reverse (zip _)) _ = Some v`] >>
  fs [Once MEM_SPLIT_APPEND_last] >>
  fs [alookup_some, MAP_EQ_APPEND, reverse_eq_append] >> rw [] >>
  rfs [zip_eq_append] >> rw [] >> rw [] >>
  fs [] >> rw [] >>
  qpat_x_assum `reverse _ ++ _ = zip _` (mp_tac o GSYM) >> rw [zip_eq_append] >>
  fs [] >> rw [] >>
  rename [`[_] = zip (x,y)`] >>
  Cases_on `x` >> Cases_on `y` >> fs [] >>
  rw [] >> fs [LIST_REL_SPLIT1] >> rw [] >>
  HINT_EXISTS_TAC >> rw [] >>
  rw [Once eval_exp_cases, flookup_fupdate_list] >>
  qmatch_goalsub_abbrev_tac `reverse (zip (a, b))` >>
  `length a = length b`
  by (
    rw [Abbr `a`, Abbr `b`] >>
    metis_tac [LIST_REL_LENGTH, LENGTH_MAP, LENGTH_ZIP, LENGTH_REVERSE, ADD_COMM, ADD_ASSOC]) >>
  CASE_TAC >> rw [] >> fs [alookup_some, reverse_eq_append]
  >- (fs [ALOOKUP_NONE] >> rfs [MAP_REVERSE, MAP_ZIP] >> fs [Abbr `a`]) >>
  rfs [zip_eq_append] >>
  unabbrev_all_tac >>
  rw [] >>
  qpat_x_assum `reverse _ ++ _ = zip _` (mp_tac o GSYM) >> rw [zip_eq_append] >>
  fs [] >> rw [] >>
  rename [`[_] = zip (a,b)`] >>
  Cases_on `a` >> Cases_on `b` >> fs [] >>
  rw [] >> fs [] >> rw [] >>
  fs [ALOOKUP_NONE] >> fs [] >>
  rev_full_simp_tac pure_ss [SWAP_REVERSE_SYM] >>
  rw [] >> fs [MAP_REVERSE] >> rfs [MAP_ZIP] >>
  fs [MIN_DEF] >>
  BasicProvers.EVERY_CASE_TAC >> fs [] >>
  rfs [] >> rw [] >>
  fs [MAP_MAP_o, combinTheory.o_DEF, LAMBDA_PROD] >>
  rename [`map fst l1 ++ [_] ++ map fst l2 = l3 ++ [_] ++ l4`,
          `map _ l1 ++ [translate_reg _ _] ++ _ = l5 ++ _ ++ l6`,
          `l7 ++ [v1:llair$flat_v reg_v] ++ l8 = l9 ++ [v2] ++ l10`] >>
  `map fst l1 = l3 ∧ map fst l2 = l4`
  by (
    irule append_split_last >>
    qexists_tac `h` >> rw [MEM_MAP] >>
    CCONTR_TAC >> fs [] >>
    `all_distinct (map fst l1 ++ [fst y] ++ map fst l2)` by metis_tac [] >>
    fs [ALL_DISTINCT_APPEND, MEM_MAP] >>
    metis_tac [FST, pair_CASES]) >>
  `length l2 = length l6` suffices_by metis_tac [append_split_eq, LIST_REL_LENGTH, LENGTH_MAP] >>
  rename1 `translate_reg r t` >>
  `~mem (translate_reg r t) (map (λ(r,ty). translate_reg r ty) l2)`
  by (
    rw [MEM_MAP] >> pairarg_tac >> fs [] >>
    rename1 `translate_reg r1 t1 = translate_reg r2 t2` >>
    Cases_on `r1` >> Cases_on `r2` >> rw [translate_reg_def] >>
    metis_tac [MEM_MAP, FST]) >>
  metis_tac [append_split_last, LENGTH_MAP]
QED

Theorem emap_inv_updates_keep_same_ip2:
  ∀prog emap s s' vs res_vs rtys r regs_to_keep gmap.
  is_ssa prog ∧
  good_emap s.ip.f prog regs_to_keep gmap emap ∧
  r ∈ live prog s.ip ∧
  assigns prog s.ip = set rtys ∧
  emap_invariant prog emap s s' r ∧
  list_rel v_rel (map (\v. v.value) vs) res_vs ∧
  length rtys = length vs ∧
  reachable prog s.ip ∧
  ¬mem r (map fst rtys)
  ⇒
  emap_invariant prog emap
        (s with locals := s.locals |++ zip (map fst rtys, vs))
        (s' with locals := s'.locals |++ zip (map (\(r,ty). translate_reg r ty) rtys, res_vs))
        r
Proof
  rw [emap_invariant_def, alistTheory.flookup_fupdate_list] >> rw [] >>
  CASE_TAC >> rw []
  >- (
    qexists_tac `v'` >> rw [] >>
    qmatch_goalsub_abbrev_tac `eval_exp s_upd _ _` >>
    `DRESTRICT s_upd.locals (exp_uses e) = DRESTRICT s'.locals (exp_uses e) ∧
     s_upd.glob_addrs = s'.glob_addrs`
    suffices_by metis_tac [eval_exp_ignores_unused] >>
    rw [Abbr `s_upd`] >>
    qmatch_goalsub_abbrev_tac `_ |++ l = _` >>
    `l = []` suffices_by rw [FUPDATE_LIST_THM] >>
    rw [Abbr `l`, FILTER_EQ_NIL, LAMBDA_PROD] >>
    `(λ(p1,p2:llair$flat_v reg_v). p1 ∉ exp_uses e) = (\x. fst x ∉ exp_uses e)`
    by (rw [EXTENSION, IN_DEF] >> pairarg_tac >> rw []) >>
    `length rtys = length res_vs` by metis_tac [LIST_REL_LENGTH, LENGTH_MAP] >>
    rw [every_zip_fst, EVERY_MAP] >> rw [LAMBDA_PROD] >>
    rw [EVERY_EL] >> pairarg_tac >> rw [] >>
    qmatch_goalsub_rename_tac `translate_reg r1 ty1 ∉ exp_uses _` >>
    fs [good_emap_def] >>
    first_x_assum (qspec_then `s.ip` mp_tac) >> rw [] >>
    first_x_assum drule >> simp [] >>
    disch_then (qspec_then `translate_reg r1 ty1` mp_tac) >> rw [] >>
    CCONTR_TAC >> fs [] >>
    `ip_equiv ip2 s.ip`
    by (
      fs [is_ssa_def, EXTENSION, IN_DEF] >>
      Cases_on `r1` >> fs [translate_reg_def, untranslate_reg_def] >>
      rename1 `Reg reg = fst regty` >>
      Cases_on `regty` >> fs [] >> rw [] >>
      `assigns prog s.ip (Reg reg, ty1)`
      suffices_by metis_tac [reachable_dominates_same_func, FST] >>
      metis_tac [EL_MEM, IN_DEF]) >>
    metis_tac [dominates_irrefl, ip_equiv_dominates2]) >>
  drule ALOOKUP_MEM >> rw [MEM_MAP, MEM_ZIP] >>
  metis_tac [EL_MEM, LIST_REL_LENGTH, LENGTH_MAP]
QED

Theorem local_state_rel_next_ip:
  ∀prog emap ip2 s s'.
  local_state_rel prog emap s s' ∧
  ip2 ∈ next_ips prog s.ip ∧
  (∀r. r ∈ assigns prog s.ip ⇒ emap_invariant prog emap s s' (fst r))
  ⇒
  local_state_rel prog emap (s with ip := ip2) s'
Proof
  rw [local_state_rel_def, emap_invariant_def] >>
  Cases_on `r ∈ live prog s.ip` >> fs [] >>
  pop_assum mp_tac >> simp [Once live_gen_kill, PULL_EXISTS] >> rw [] >>
  first_x_assum (qspec_then `ip2` mp_tac) >> rw [] >>
  first_x_assum drule >> rw [] >>
  ntac 3 HINT_EXISTS_TAC >> rw [] >>
  first_x_assum irule >> rw [] >>
  metis_tac [next_ips_same_func]
QED

Theorem local_state_rel_updates_keep:
  ∀rtys prog emap s s' vs res_vs i regs_to_keep gmap.
  is_ssa prog ∧
  good_emap s.ip.f prog regs_to_keep gmap emap ∧
  all_distinct (map fst rtys) ∧
  set rtys = assigns prog s.ip ∧
  (¬∃inst r t. get_instr prog s.ip (Inl inst) ∧ classify_instr inst = Exp r t ∧ r ∉ regs_to_keep) ∧
  local_state_rel prog emap s s' ∧
  length vs = length rtys ∧
  list_rel v_rel (map (\v. v.value) vs) res_vs ∧
  i ∈ next_ips prog s.ip ∧
  reachable prog s.ip
  ⇒
  local_state_rel prog emap
        (s with <| ip := i; locals := s.locals |++ zip (map fst rtys, vs) |>)
        (s' with locals := s'.locals |++ zip (map (\(r,ty). translate_reg r ty) rtys, res_vs))
Proof
  rw [] >> irule local_state_rel_next_ip >>
  fs [local_state_rel_def] >> rw []
  >- (
    irule emap_inv_updates_keep_same_ip1 >> rw [] >>
    fs [good_emap_def] >>
    first_x_assum (qspec_then `s.ip` mp_tac) >> rw [] >>
    qexists_tac `snd r` >> rw [] >>
    first_x_assum irule >> rw [] >>
    fs [assigns_cases, IN_DEF, not_exp_def] >>
    metis_tac []) >>
  Cases_on `mem r (map fst rtys)`
  >- (
    irule emap_inv_updates_keep_same_ip1 >> rw [] >>
    `∃t. (r,t) ∈ set rtys` by (fs [MEM_MAP] >> metis_tac [FST, pair_CASES]) >>
    rfs [good_emap_def] >>
    first_x_assum (qspec_then `s.ip` mp_tac) >> rw [] >>
    `(∃inst. get_instr prog s.ip (Inl inst)) ∨ (∃phis. get_instr prog s.ip (Inr phis))`
    by (fs [IN_DEF, assigns_cases] >> metis_tac []) >>
    metis_tac [FST, PAIR_EQ, SND,not_exp_def])
  >- (
    irule emap_inv_updates_keep_same_ip2 >> rw [] >>
    metis_tac [])
QED

Theorem local_state_rel_update_keep:
  ∀prog emap s s' v res_v r i ty regs_to_keep gmap.
  is_ssa prog ∧
  good_emap s.ip.f prog regs_to_keep gmap emap ∧
  assigns prog s.ip = {(r,ty)} ∧
  (¬∃inst r t. get_instr prog s.ip (Inl inst) ∧ classify_instr inst = Exp r t ∧ r ∉ regs_to_keep) ∧
  local_state_rel prog emap s s' ∧
  v_rel v.value res_v ∧
  reachable prog s.ip ∧
  i ∈ next_ips prog s.ip
  ⇒
  local_state_rel prog emap
        (s with <| ip := i; locals := s.locals |+ (r, v) |>)
        (s' with locals := s'.locals |+ (translate_reg r ty, res_v))
Proof
  rw [] >>
  drule local_state_rel_updates_keep >>
  disch_then (qspecl_then [`[(r,ty)]`, `emap`, `s`, `s'`] mp_tac) >>
  simp [] >> disch_then drule >>
  disch_then (qspecl_then [`[v]`, `[res_v]`] mp_tac) >>
  simp [] >> disch_then drule >>
  rw [FUPDATE_LIST_THM]
QED

Theorem mem_state_rel_update_keep:
  ∀prog gmap emap s s' v res_v r ty i inst regs_to_keep.
  is_ssa prog ∧
  good_emap s.ip.f prog regs_to_keep gmap emap ∧
  assigns prog s.ip = {(r,ty)} ∧
  (¬∃inst r t. get_instr prog s.ip (Inl inst) ∧ classify_instr inst = Exp r t ∧ r ∉ regs_to_keep) ∧
  mem_state_rel prog gmap emap s s' ∧
  v_rel v.value res_v ∧
  reachable prog s.ip ∧
  i ∈ next_ips prog s.ip
  ⇒
  mem_state_rel prog gmap emap
        (s with <| ip := i; locals := s.locals |+ (r, v) |>)
        (s' with locals := s'.locals |+ (translate_reg r ty, res_v))
Proof
  rw [mem_state_rel_def]
  >- (
    irule local_state_rel_update_keep >> rw [] >>
    metis_tac [get_instr_func, INL_11, instr_class_11, instr_class_distinct,
               classify_instr_def])
  >- metis_tac [next_ips_reachable]
QED

Triviality lemma:
  ((s:llair$state) with heap := h).locals = s.locals ∧
  ((s:llair$state) with heap := h).glob_addrs = s.glob_addrs
Proof
  rw []
QED

Theorem mem_state_rel_heap_update:
  ∀prog gmap emap s s' h h'.
    mem_state_rel prog gmap emap s s' ∧
    heap_ok h ∧
    erase_tags h = erase_tags h'
    ⇒
    mem_state_rel prog gmap emap (s with heap := h) (s' with heap := h')
Proof
  rw [mem_state_rel_def, erase_tags_def, local_state_rel_def] >>
  rw [heap_component_equality] >>
  fs [fmap_eq_flookup, FLOOKUP_o_f] >> rw [] >>
  first_x_assum (qspec_then `x` mp_tac) >>
  BasicProvers.EVERY_CASE_TAC >> rw [] >>
  fs [emap_invariant_def]
  >- metis_tac [eval_exp_ignores, lemma]
  >- metis_tac [eval_exp_ignores, lemma] >>
  Cases_on `x'` >> Cases_on `x''` >> fs []
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

Theorem bytes_v_rel_lem:
  (∀f s bs t.
    f = (λn t w. convert_value t w) ∧
    s = type_to_shape t ∧
    first_class_type t
    ⇒
    (quotient_pair$### v_rel $=)
      (bytes_to_value f s bs)
      (bytes_to_value (λn t w. convert_value t w) (type_to_shape (translate_ty t)) bs)) ∧
  (∀f n s bs t.
    f = (λn t w. convert_value t w) ∧
    s = type_to_shape t ∧
    first_class_type t
    ⇒
    (quotient_pair$### (list_rel v_rel) $=)
      (read_array f n s bs)
      (read_array (λn t w. convert_value t w) n (type_to_shape (translate_ty t)) bs)) ∧
  (∀f ss bs ts.
    f = (λn t w. convert_value t w) ∧
    ss = map type_to_shape ts ∧
    every first_class_type ts
    ⇒
    (quotient_pair$### (list_rel v_rel) $=)
      (read_str f ss bs)
      (read_str (λn t w. convert_value t w) (map (type_to_shape o translate_ty) ts) bs))
Proof
  ho_match_mp_tac bytes_to_value_ind >>
  rw [llvmTheory.type_to_shape_def, translate_ty_def, type_to_shape_def,
      sizeof_def, llvmTheory.sizeof_def, bytes_to_value_def, pointer_size_def,
      convert_value_def, llvmTheory.convert_value_def, quotient_pairTheory.PAIR_REL]
  >- (
    Cases_on `t'` >>
    fs [llvmTheory.type_to_shape_def, llvmTheory.sizeof_def, llvmTheory.first_class_type_def] >>
    TRY (Cases_on `s`) >>
    rw [llvmTheory.sizeof_def, le_read_num_def, translate_size_def,
        convert_value_def, llvmTheory.convert_value_def, translate_ty_def,
        type_to_shape_def, bytes_to_value_def, sizeof_def, llvmTheory.sizeof_def] >>
    simp [v_rel_cases] >> rw [word_0_w2i, w2i_1] >>
    fs [pointer_size_def, llvmTheory.pointer_size_def] >>
    qmatch_goalsub_abbrev_tac `l2n 256 l` >>
    qmatch_goalsub_abbrev_tac `n2i n dim` >>
    `n < 2 ** dim`
    by (
      qspecl_then [`l`, `256`] mp_tac numposrepTheory.l2n_lt >>
      rw [] >>
      `256 ** length l ≤ 2 ** dim` suffices_by decide_tac >>
      `256 = 2 ** 8` by rw [] >>
      full_simp_tac bool_ss [] >>
      REWRITE_TAC [GSYM EXP_EXP_MULT] >>
      rw [EXP_BASE_LE_MONO] >>
      unabbrev_all_tac >> rw []) >>
    metis_tac [w2i_n2w, dimword_def, dimindex_8, dimindex_32, dimindex_64])
  >- (
    Cases_on `t` >>
    fs [llvmTheory.type_to_shape_def, llvmTheory.sizeof_def, llvmTheory.first_class_type_def] >>
    rw [PAIR_MAP] >>
    pairarg_tac >> fs [type_to_shape_def, translate_ty_def, bytes_to_value_def] >>
    first_x_assum (qspec_then `t'` mp_tac) >> simp [] >>
    simp [v_rel_cases] >>
    pairarg_tac >> fs [] >>
    pairarg_tac >> fs [] >> rw [])
  >- (
    Cases_on `t` >>
    fs [llvmTheory.type_to_shape_def, llvmTheory.sizeof_def, llvmTheory.first_class_type_def] >>
    rw [PAIR_MAP] >>
    fs [type_to_shape_def, translate_ty_def, bytes_to_value_def] >>
    pairarg_tac >> fs [PAIR_MAP] >>
    first_x_assum (qspec_then `l` mp_tac) >> simp [] >>
    simp [v_rel_cases] >>
    pairarg_tac >> fs [] >>
    pairarg_tac >> fs [MAP_MAP_o] >> rw [] >> fs [ETA_THM])
  >- (
    rpt (pairarg_tac >> fs []) >>
    first_x_assum (qspec_then `t` mp_tac) >> rw [] >>
    first_x_assum (qspec_then `t` mp_tac) >> rw [])
  >- (
    Cases_on `ts` >> fs [bytes_to_value_def] >>
    rpt (pairarg_tac >> fs []) >>
    first_x_assum (qspec_then `h` mp_tac) >> simp [] >> strip_tac >>
    fs [] >> rfs [] >> fs [] >>
    first_x_assum (qspec_then `t` mp_tac) >> simp [] >> strip_tac >>
    fs [MAP_MAP_o] >> rw [])
QED

Theorem bytes_v_rel:
  ∀t bs.
    first_class_type t ⇒
    v_rel (fst (bytes_to_llvm_value t bs))
          (fst (bytes_to_llair_value (translate_ty t) bs))
Proof
  rw [bytes_to_llvm_value_def, bytes_to_llair_value_def] >>
  qspecl_then [`bs`, `t`] mp_tac (CONJUNCT1 (SIMP_RULE (srw_ss()) [] bytes_v_rel_lem)) >>
  rw [quotient_pairTheory.PAIR_REL] >>
  pairarg_tac >> fs [] >>
  pairarg_tac >> fs []
QED

Triviality n2i_lem:
  ∀n. nfits n llair$pointer_size ⇒
    n2i n llair$pointer_size = IntV (w2i (n2w n : word64)) llair$pointer_size
Proof
  rw [pointer_size_def] >>
  `2 ** 64 = dimword (:64)` by rw [dimword_64] >>
  full_simp_tac bool_ss [nfits_def] >>
  drule w2i_n2w >> rw []
QED

Theorem translate_constant_correct_lem:
  (∀c s prog gmap emap s' v.
   mem_state_rel prog gmap emap s s' ∧
   eval_const s.globals c v
   ⇒
   ∃v'. eval_exp s' (translate_const gmap c) v' ∧ v_rel v v') ∧
  (∀(cs : (ty # const) list) s prog gmap emap s' vs.
   mem_state_rel prog gmap emap s s' ∧
   list_rel (eval_const s.globals o snd) cs vs
   ⇒
   ∃v'. list_rel (eval_exp s') (map (translate_const gmap o snd) cs) v' ∧ list_rel v_rel vs v') ∧
  (∀(tc : ty # const) s prog gmap emap s' v.
   mem_state_rel prog gmap emap s s' ∧
   eval_const s.globals (snd tc) v
   ⇒
   ∃v'. eval_exp s' (translate_const gmap (snd tc)) v' ∧ v_rel v v')
Proof
  ho_match_mp_tac const_induction >> rw [translate_const_def] >>
  pop_assum mp_tac >> simp [Once eval_const_cases]
  >- (
    rw [Once eval_exp_cases] >>
    simp [Once eval_const_cases, translate_size_def, v_rel_cases] >>
    metis_tac [truncate_2comp_i2w_w2i, dimindex_1, dimindex_8, dimindex_32, dimindex_64])
  >- (
    rw [Once eval_exp_cases] >>
    simp [v_rel_cases, PULL_EXISTS, MAP_MAP_o] >>
    fs [combinTheory.o_DEF, LAMBDA_PROD] >> rw [] >>
    first_x_assum drule >> disch_then irule >>
    fs [LIST_REL_EL_EQN] >> rw [] >> rfs [] >>
    first_x_assum drule  >>
    simp [EL_MAP] >>
    Cases_on `el n cs` >> simp [])
  >- (
    rw [Once eval_exp_cases] >>
    simp [v_rel_cases, PULL_EXISTS, MAP_MAP_o] >>
    fs [combinTheory.o_DEF, LAMBDA_PROD] >> rw [] >>
    first_x_assum drule >> disch_then irule >>
    fs [LIST_REL_EL_EQN] >> rw [] >> rfs [] >>
    first_x_assum drule  >>
    simp [EL_MAP] >>
    Cases_on `el n cs` >> simp [])
  (* TODO: unimplemented GEP stuff *)
  >- cheat
  >- (
    rw [Once eval_exp_cases] >> simp [v_rel_cases] >>
    fs [mem_state_rel_def, globals_rel_def] >>
    first_x_assum (qspec_then `g` mp_tac) >> rw [] >>
    qexists_tac `w2n w` >> rw [] >> fs [FLOOKUP_DEF]
    >- (
      rfs [] >>
      qpat_x_assum `w2n _ = _` (mp_tac o GSYM) >> rw [] >>
      simp [n2i_lem])
    >- rfs [BIJ_DEF, INJ_DEF, SURJ_DEF])
  >- metis_tac []
QED

Theorem translate_constant_correct:
  ∀c s prog gmap emap s' g v.
   mem_state_rel prog gmap emap s s' ∧
   eval_const s.globals c v
   ⇒
   ∃v'. eval_exp s' (translate_const gmap c) v' ∧ v_rel v v'
Proof
  metis_tac [translate_constant_correct_lem]
QED

Theorem translate_const_no_reg[simp]:
  ∀gmap c. r ∉ exp_uses (translate_const gmap c)
Proof
  ho_match_mp_tac translate_const_ind >>
  rw [translate_const_def, exp_uses_def, MEM_MAP, METIS_PROVE [] ``x ∨ y ⇔ (~x ⇒ y)``]
  >- (pairarg_tac >> fs [] >> metis_tac [])
  >- (pairarg_tac >> fs [] >> metis_tac [])
  (* TODO: unimplemented GEP stuff *)
  >- cheat
QED

Theorem translate_arg_correct:
  ∀s a v prog gmap emap s'.
  mem_state_rel prog gmap emap s s' ∧
  eval s a v ∧
  arg_to_regs a ⊆ live prog s.ip
  ⇒
  ∃v'. eval_exp s' (translate_arg gmap emap a) v' ∧ v_rel v.value v'
Proof
  Cases_on `a` >> rw [eval_cases, translate_arg_def] >> rw []
  >- metis_tac [translate_constant_correct] >>
  CASE_TAC >> fs [PULL_EXISTS, mem_state_rel_def, local_state_rel_def, emap_invariant_def, arg_to_regs_def] >>
  res_tac >> rfs [] >> metis_tac [eval_exp_ignores]
QED

Theorem is_allocated_mem_state_rel:
  ∀prog gmap emap s1 s1'.
    mem_state_rel prog gmap emap s1 s1'
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
    `∃j. 0 ≤ j ∧ i = -j` by intLib.COOPER_TAC >>
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
  ∀prog gmap emap s1 s1' nsw nuw ty v1 v1' v2 v2' e2' e1' result.
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
  fs [PAIR_MAP, wordsTheory.FST_ADD_WITH_CARRY] >>
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
  ∀prog gmap emap s1 s1' a v v1' e1' cs vs ns result.
    mem_state_rel prog gmap emap s1 s1' ∧
    list_rel (eval_const s1.globals) cs vs ∧
    map signed_v_to_num vs = map Some ns ∧
    extract_value v ns = Some result ∧
    eval_exp s1' e1' v1' ∧
    v_rel v v1'
    ⇒
    ∃v2'.
      eval_exp s1' (foldl (λe c. Select e (translate_const gmap c)) e1' cs) v2' ∧
      v_rel result v2'
Proof
  Induct_on `cs` >> rw [] >> rfs [] >> fs [extract_value_def]
  >- metis_tac [] >>
  first_x_assum irule >>
  Cases_on `ns` >> fs [] >>
  qmatch_goalsub_rename_tac `translate_const gmap c` >>
  qmatch_assum_rename_tac `eval_const _ _ v3` >>
  `∃v2'. eval_exp s1' (translate_const gmap c) v2' ∧ v_rel v3 v2'`
  by metis_tac [translate_constant_correct] >>
  Cases_on `v` >> fs [extract_value_def] >>
  qpat_x_assum `v_rel (AggV _) _` mp_tac >>
  simp [Once v_rel_cases] >> rw [] >>
  simp [Once eval_exp_cases, PULL_EXISTS] >>
  fs [LIST_REL_EL_EQN] >>
  qmatch_assum_rename_tac `_ = map Some is` >>
  Cases_on `v3` >> fs [signed_v_to_num_def, signed_v_to_int_def] >> rw [] >>
  `∃i. v2' = FlatV i` by fs [v_rel_cases] >> fs [] >>
  qmatch_assum_rename_tac `option_join _ = Some x` >>
  `∃size. i = IntV (&x) size` suffices_by metis_tac [] >> rw [] >>
  qpat_x_assum `v_rel _ _` mp_tac >>
  simp [v_rel_cases] >> rw [] >> fs [signed_v_to_int_def] >> rw [] >>
  intLib.COOPER_TAC
QED

Theorem translate_update_correct:
  ∀prog gmap emap s1 s1' a v1 v1' v2 v2' e2 e2' e1' cs vs ns result.
    mem_state_rel prog gmap emap s1 s1' ∧
    list_rel (eval_const s1.globals) cs vs ∧
    map signed_v_to_num vs = map Some ns ∧
    insert_value v1 v2 ns = Some result ∧
    eval_exp s1' e1' v1' ∧
    v_rel v1 v1' ∧
    eval_exp s1' e2' v2' ∧
    v_rel v2 v2'
    ⇒
    ∃v3'.
      eval_exp s1' (translate_updatevalue gmap e1' e2' cs) v3' ∧
      v_rel result v3'
Proof
  Induct_on `cs` >> rw [] >> rfs [] >> fs [insert_value_def, translate_updatevalue_def]
  >- metis_tac [] >>
  simp [Once eval_exp_cases, PULL_EXISTS] >>
  Cases_on `ns` >> fs [] >>
  Cases_on `v1` >> fs [insert_value_def] >>
  rename [`insert_value (el x _) _ ns`] >>
  Cases_on `insert_value (el x l) v2 ns` >> fs [] >> rw [] >>
  qpat_x_assum `v_rel (AggV _) _` mp_tac >> simp [Once v_rel_cases] >> rw [] >>
  simp [v_rel_cases] >>
  qmatch_goalsub_rename_tac `translate_const gmap c` >>
  qexists_tac `vs2` >> simp [] >>
  qmatch_assum_rename_tac `eval_const _ _ v3` >>
  `∃v4'. eval_exp s1' (translate_const gmap c) v4' ∧ v_rel v3 v4'`
  by metis_tac [translate_constant_correct] >>
  `∃idx_size. v4' = FlatV (IntV (&x) idx_size)`
  by (
    pop_assum mp_tac >> simp [Once v_rel_cases] >>
    rw [] >> fs [signed_v_to_num_def, signed_v_to_int_def] >>
    intLib.COOPER_TAC) >>
  first_x_assum drule >>
  disch_then drule >>
  disch_then drule >>
  disch_then drule >>
  disch_then (qspecl_then [`el x vs2`, `v2'`, `e2'`, `Select e1' (translate_const gmap c)`] mp_tac) >>
  simp [Once eval_exp_cases] >>
  metis_tac [EVERY2_LUPDATE_same, LIST_REL_LENGTH, LIST_REL_EL_EQN]
QED

val sizes = [``:1``, ``:8``, ``:32``, ``:64``];

val trunc_thms =
  LIST_CONJ (map (fn x => SIMP_RULE (srw_ss()) [] (INST_TYPE [``:'a`` |-> x] truncate_2comp_i2w_w2i))
                 sizes);

val signed2unsigned_thms =
  LIST_CONJ (map (fn x => SIMP_RULE (srw_ss()) [] (INST_TYPE [``:'a`` |-> x] (GSYM w2n_signed2unsigned)))
                 sizes);

Definition good_cast_def:
  (good_cast Trunc (FlatV (IntV i size)) from_bits to_t ⇔
    from_bits = size ∧ llair$sizeof_bits to_t < from_bits) ∧
  (good_cast Zext (FlatV (IntV i size)) from_bits to_t ⇔
    from_bits = size ∧ from_bits < sizeof_bits to_t) ∧
  (good_cast Sext (FlatV (IntV i size)) from_bits to_t ⇔
    from_bits = size ∧ from_bits < sizeof_bits to_t) ∧
  (good_cast Ptrtoint _ _ _ ⇔ T) ∧
  (good_cast Inttoptr _ _ _ ⇔ T)
End

Theorem translate_cast_correct:
  ∀prog gmap emap s1' cop from_bits to_ty v1 v1' e1' result.
    do_cast cop v1.value to_ty = Some result ∧
    eval_exp s1' e1' v1' ∧
    v_rel v1.value v1' ∧
    good_cast cop v1' from_bits (translate_ty to_ty)
    ⇒
    ∃v3'.
      eval_exp s1' ((if (cop = Zext) then Unsigned else Signed)
                    (if cop = Trunc then sizeof_bits (translate_ty to_ty) else from_bits)
                    e1' (translate_ty to_ty)) v3' ∧
      v_rel result v3'
Proof
  rw [] >> simp [Once eval_exp_cases, PULL_EXISTS, Once v_rel_cases]
  >- ( (* Zext *)
    fs [do_cast_def, OPTION_JOIN_EQ_SOME, unsigned_v_to_num_some, w64_cast_some,
       translate_ty_def, sizeof_bits_def, translate_size_def] >>
    rw [] >>
    rfs [v_rel_cases] >> rw [] >>
    qmatch_assum_abbrev_tac `eval_exp _ _ (FlatV (IntV i s))` >>
    qexists_tac `i` >> qexists_tac `s` >> rw [] >>
    unabbrev_all_tac >>
    fs [good_cast_def, translate_ty_def, sizeof_bits_def, translate_size_def] >>
    rw [trunc_thms, signed2unsigned_thms] >>
    rw [GSYM w2w_def, w2w_w2w, WORD_ALL_BITS] >>
    rw [w2i_w2w_expand])
  >- ( (* Trunc *)
    fs [do_cast_def] >> rw [] >>
    fs [OPTION_JOIN_EQ_SOME, w64_cast_some, unsigned_v_to_num_some,
        signed_v_to_int_some, mk_ptr_some] >>
    rw [sizeof_bits_def, translate_ty_def, translate_size_def] >>
    rfs [] >> fs [v_rel_cases] >>
    rw [] >>
    qmatch_assum_abbrev_tac `eval_exp _ _ (FlatV (IntV i s))` >>
    qexists_tac `s` >> qexists_tac `i` >> rw [] >>
    unabbrev_all_tac >>
    fs [good_cast_def, translate_ty_def, sizeof_bits_def, translate_size_def] >>
    rw [w2w_n2w, GSYM w2w_def, trunc_thms, pointer_size_def] >>
    rw [i2w_w2i_extend, WORD_w2w_OVER_MUL] >>
    rw [w2w_w2w, WORD_ALL_BITS, word_bits_w2w] >>
    rw [word_mul_def]) >>
  Cases_on `cop` >> fs [] >> rw []
  >- ( (* Sext *)
    fs [do_cast_def] >> rw [] >>
    fs [OPTION_JOIN_EQ_SOME, w64_cast_some, unsigned_v_to_num_some,
        signed_v_to_int_some, mk_ptr_some] >>
    rw [sizeof_bits_def, translate_ty_def, translate_size_def] >>
    rfs [] >> fs [v_rel_cases] >>
    rw [] >>
    qmatch_assum_abbrev_tac `eval_exp _ _ (FlatV (IntV i s))` >>
    qexists_tac `s` >> qexists_tac `i` >> rw [] >>
    unabbrev_all_tac >>
    fs [good_cast_def, translate_ty_def, sizeof_bits_def, translate_size_def] >>
    rw [trunc_thms, w2w_i2w] >>
    irule (GSYM w2i_i2w)
    >- (
      `w2i w ≤ INT_MAX (:1) ∧ INT_MIN (:1) ≤ w2i w` by metis_tac [w2i_le, w2i_ge] >>
      fs [] >> intLib.COOPER_TAC)
    >- (
      `w2i w ≤ INT_MAX (:1) ∧ INT_MIN (:1) ≤ w2i w` by metis_tac [w2i_le, w2i_ge] >>
      fs [] >> intLib.COOPER_TAC)
    >- (
      `w2i w ≤ INT_MAX (:1) ∧ INT_MIN (:1) ≤ w2i w` by metis_tac [w2i_le, w2i_ge] >>
      fs [] >> intLib.COOPER_TAC)
    >- (
      `w2i w ≤ INT_MAX (:8) ∧ INT_MIN (:8) ≤ w2i w` by metis_tac [w2i_le, w2i_ge] >>
      fs [] >> intLib.COOPER_TAC)
    >- (
      `w2i w ≤ INT_MAX (:8) ∧ INT_MIN (:8) ≤ w2i w` by metis_tac [w2i_le, w2i_ge] >>
      fs [] >> intLib.COOPER_TAC)
    >- (
      `w2i w ≤ INT_MAX (:32) ∧ INT_MIN (:32) ≤ w2i w` by metis_tac [w2i_le, w2i_ge] >>
      fs [] >> intLib.COOPER_TAC))
  (* TODO: pointer to int and int to pointer casts *)
  >> cheat
QED

Theorem const_idx_uses[simp]:
  ∀cs gmap e.
    exp_uses (foldl (λe c. Select e (translate_const gmap c)) e cs) = exp_uses e
Proof
  Induct_on `cs` >> rw [exp_uses_def] >> rw [EXTENSION]
QED

Theorem exp_uses_trans_upd_val[simp]:
  ∀cs gmap e1 e2. exp_uses (translate_updatevalue gmap e1 e2 cs) =
    (if cs = [] then {} else exp_uses e1) ∪ exp_uses e2
Proof
  Induct_on `cs` >> rw [exp_uses_def, translate_updatevalue_def] >>
  rw [EXTENSION] >>
  metis_tac []
QED

(* TODO: identify some lemmas to cut down on the duplicated proof in the very
 * similar cases *)
Theorem translate_instr_to_exp_correct:
  ∀gmap emap instr r t s1 s1' s2 prog l regs_to_keep.
    prog_ok prog ∧ is_ssa prog ∧
    good_emap s1.ip.f prog regs_to_keep gmap emap ∧
    classify_instr instr = Exp r t ∧
    mem_state_rel prog gmap emap s1 s1' ∧
    get_instr prog s1.ip (Inl instr) ∧
    step_instr prog s1 instr l s2 ∧
    is_implemented instr
    ⇒
    ∃pv s2'.
      l = Tau ∧
      s2.ip = inc_pc s1.ip ∧
      mem_state_rel prog gmap emap s2 s2' ∧
      (r ∉ regs_to_keep ⇒ s1' = s2') ∧
      (r ∈ regs_to_keep ⇒
       step_inst s1' (Move [(translate_reg r t, translate_instr_to_exp gmap emap instr)]) Tau s2')
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
    drule translate_sub_correct >>
    simp [] >>
    disch_then (qspecl_then [`s1'`, `v'`, `v''`] mp_tac) >> simp [] >>
    disch_then drule >> disch_then drule >> rw [] >>
    rename1 `eval_exp _ (Sub _ _ _) res_v` >>
    rename1 `r ∈ _` >>
    simp [inc_pc_def, llvmTheory.inc_pc_def] >>
    `assigns prog s1.ip = {(r,ty)}`
    by rw [assigns_cases, EXTENSION, IN_DEF, get_instr_cases, instr_assigns_def] >>
    `reachable prog s1.ip` by fs [mem_state_rel_def] >>
    `s1.ip with i := inc_bip (Offset idx) ∈ next_ips prog s1.ip`
    by (
      drule prog_ok_nonterm >>
      simp [get_instr_cases, PULL_EXISTS] >>
      ntac 3 (disch_then drule) >>
      simp [terminator_def, next_ips_cases, IN_DEF, inc_pc_def]) >>
    Cases_on `r ∉ regs_to_keep` >> rw []
    >- (
      irule mem_state_rel_update >> rw []
      >- (
        fs [get_instr_cases, classify_instr_def, translate_instr_to_exp_def] >>
        metis_tac [])
      >- metis_tac [])
    >- (
      simp [step_inst_cases, PULL_EXISTS] >>
      qexists_tac `res_v` >> rw [] >>
      rw [update_results_def, GSYM FUPDATE_EQ_FUPDATE_LIST] >>
      irule mem_state_rel_update_keep >> rw [] >>
      qexists_tac `regs_to_keep` >> rw [] >>
      CCONTR_TAC >> fs [] >>
      drule exp_assigns_sing >> disch_then drule >> rw [] >>
      metis_tac [])) >>
  conj_tac
  >- ( (* Extractvalue *)
    rw [step_instr_cases, get_instr_cases, update_result_def] >>
    qpat_x_assum `Extractvalue _ _ _ = el _ _` (assume_tac o GSYM) >>
    `arg_to_regs a ⊆ live prog s1.ip`
    by (
      simp [Once live_gen_kill, SUBSET_DEF, uses_cases, IN_DEF, get_instr_cases,
            instr_uses_def]) >>
    drule translate_extract_correct >> rpt (disch_then drule) >>
    drule translate_arg_correct >> disch_then drule >>
    simp [] >> strip_tac >>
    disch_then drule >> simp [] >> rw [] >>
    rename1 `eval_exp _ (foldl _ _ _) res_v` >>
    rw [inc_pc_def, llvmTheory.inc_pc_def] >>
    rename1 `r ∈ _` >>
    `assigns prog s1.ip = {(r,THE (extract_type t (map cidx_to_num cs)))}`
    by rw [assigns_cases, EXTENSION, IN_DEF, get_instr_cases, instr_assigns_def] >>
    `reachable prog s1.ip` by fs [mem_state_rel_def] >>
    `s1.ip with i := inc_bip (Offset idx) ∈ next_ips prog s1.ip`
    by (
      drule prog_ok_nonterm >>
      simp [get_instr_cases, PULL_EXISTS] >>
      ntac 3 (disch_then drule) >>
      simp [terminator_def, next_ips_cases, IN_DEF, inc_pc_def]) >>
    Cases_on `r ∈ regs_to_keep` >> rw []
    >- (
      simp [step_inst_cases, PULL_EXISTS] >>
      qexists_tac `res_v` >> rw [] >>
      rw [update_results_def, GSYM FUPDATE_EQ_FUPDATE_LIST] >>
      irule mem_state_rel_update_keep >> rw [] >>
      qexists_tac `regs_to_keep` >> rw [] >>
      CCONTR_TAC >> fs [] >>
      drule exp_assigns_sing >> disch_then drule >> rw [] >>
      metis_tac [])
    >- (
      irule mem_state_rel_update >> rw []
      >- (
        fs [get_instr_cases, classify_instr_def, translate_instr_to_exp_def] >>
        metis_tac [])
      >- metis_tac [])) >>
  conj_tac
  >- ( (* Updatevalue *)
    rw [step_instr_cases, get_instr_cases, update_result_def] >>
    qpat_x_assum `Insertvalue _ _ _ _ = el _ _` (assume_tac o GSYM) >>
    `arg_to_regs a1 ⊆ live prog s1.ip ∧
     arg_to_regs a2 ⊆ live prog s1.ip`
    by (
      ONCE_REWRITE_TAC [live_gen_kill] >>
      simp [SUBSET_DEF, uses_cases, IN_DEF, get_instr_cases,
            instr_uses_def]) >>
    drule translate_update_correct >> rpt (disch_then drule) >>
    first_x_assum (mp_then.mp_then mp_then.Any mp_tac translate_arg_correct) >>
    disch_then drule >> disch_then drule >>
    first_x_assum (mp_then.mp_then mp_then.Any mp_tac translate_arg_correct) >>
    disch_then drule >> disch_then drule >>
    simp [] >> strip_tac >> strip_tac >>
    disch_then (qspecl_then [`v'`, `v''`] mp_tac) >> simp [] >>
    disch_then drule >> disch_then drule >>
    rw [] >>
    rename1 `eval_exp _ (translate_updatevalue _ _ _ _) res_v` >>
    rw [inc_pc_def, llvmTheory.inc_pc_def] >>
    rename1 `r ∈ _` >>
    `assigns prog s1.ip = {(r,t1)}`
    by rw [assigns_cases, EXTENSION, IN_DEF, get_instr_cases, instr_assigns_def] >>
    `reachable prog s1.ip` by fs [mem_state_rel_def] >>
    `s1.ip with i := inc_bip (Offset idx) ∈ next_ips prog s1.ip`
    by (
      drule prog_ok_nonterm >>
      simp [get_instr_cases, PULL_EXISTS] >>
      ntac 3 (disch_then drule) >>
      simp [terminator_def, next_ips_cases, IN_DEF, inc_pc_def]) >>
    Cases_on `r ∈ regs_to_keep` >> rw []
    >- (
      simp [step_inst_cases, PULL_EXISTS] >>
      qexists_tac `res_v` >> rw [] >>
      rw [update_results_def, GSYM FUPDATE_EQ_FUPDATE_LIST] >>
      irule mem_state_rel_update_keep >> rw [] >>
      qexists_tac `regs_to_keep` >> rw [] >>
      CCONTR_TAC >> fs [] >>
      drule exp_assigns_sing >> disch_then drule >> rw [] >>
      metis_tac [])
    >- (
      irule mem_state_rel_update >> rw []
      >- (
        fs [get_instr_cases, classify_instr_def, translate_instr_to_exp_def] >>
        metis_tac [])
      >- metis_tac [])) >>
  conj_tac
  >- ( (* Cast *)
    simp [step_instr_cases, get_instr_cases, update_result_def] >>
    rpt strip_tac >>
    qpat_x_assum `Cast _ _ _ _ = el _ _` (assume_tac o GSYM) >>
    `arg_to_regs a1 ⊆ live prog s1.ip`
    by (
      simp [Once live_gen_kill, SUBSET_DEF, uses_cases, IN_DEF, get_instr_cases,
            instr_uses_def] >>
      metis_tac []) >>
    fs [] >>
    first_x_assum (mp_then.mp_then mp_then.Any mp_tac translate_arg_correct) >>
    disch_then drule >> disch_then drule >> strip_tac >>
    drule translate_cast_correct >> ntac 2 (disch_then drule) >>
    simp [] >>
    disch_then (qspec_then `sizeof_bits (translate_ty t1)` mp_tac) >>
    impl_tac
    (* TODO: prog_ok should enforce that the type is consistent *)
    >- cheat >>
    strip_tac >>
    rename1 `eval_exp _ _ res_v` >>
    simp [inc_pc_def, llvmTheory.inc_pc_def] >>
    rename1 `r ∈ _` >>
    `assigns prog s1.ip = {(r,t)}`
    by rw [assigns_cases, EXTENSION, IN_DEF, get_instr_cases, instr_assigns_def] >>
    `reachable prog s1.ip` by fs [mem_state_rel_def] >>
    `s1.ip with i := inc_bip (Offset idx) ∈ next_ips prog s1.ip`
    by (
      drule prog_ok_nonterm >>
      simp [get_instr_cases, PULL_EXISTS] >>
      ntac 3 (disch_then drule) >>
      simp [terminator_def, next_ips_cases, IN_DEF, inc_pc_def]) >>
    Cases_on `r ∈ regs_to_keep` >> simp []
    >- (
      simp [step_inst_cases, PULL_EXISTS] >>
      qexists_tac `res_v` >> rw [] >>
      fs [] >>
      rw [update_results_def, GSYM FUPDATE_EQ_FUPDATE_LIST] >>
      irule mem_state_rel_update_keep >> rw [] >>
      qexists_tac `regs_to_keep` >> rw [] >>
      CCONTR_TAC >> fs [] >>
      drule exp_assigns_sing >> disch_then drule >> rw [] >>
      metis_tac [])
    >- (
      irule mem_state_rel_update >> rw []
      >- (
        fs [get_instr_cases, classify_instr_def, translate_instr_to_exp_def] >>
        metis_tac [])
      >- metis_tac [])) >>
  rw [is_implemented_def]
QED

Triviality eval_exp_help:
  (s1 with heap := h).locals = s1.locals
Proof
  rw []
QED

Theorem translate_instr_to_inst_correct:
  ∀gmap emap instr r t s1 s1' s2 prog l regs_to_keep.
    classify_instr instr = Non_exp ∧
    prog_ok prog ∧ is_ssa prog ∧
    good_emap s1.ip.f prog regs_to_keep gmap emap ∧
    mem_state_rel prog gmap emap s1 s1' ∧
    get_instr prog s1.ip (Inl instr) ∧
    step_instr prog s1 instr l s2
    ⇒
    ∃pv s2'.
      s2.ip = inc_pc s1.ip ∧
      mem_state_rel prog gmap emap s2 s2' ∧
      step_inst s1' (translate_instr_to_inst gmap emap instr) (translate_trace gmap l) s2'
Proof
  rw [step_instr_cases] >>
  fs [classify_instr_def, translate_instr_to_inst_def]
  >- ( (* Load *)
    fs [step_inst_cases, get_instr_cases, PULL_EXISTS] >>
    qpat_x_assum `Load _ _ _ = el _ _` (assume_tac o GSYM) >>
    `arg_to_regs a1 ⊆ live prog s1.ip`
    by (
      simp [Once live_gen_kill, SUBSET_DEF, uses_cases, IN_DEF, get_instr_cases,
            instr_uses_def] >>
      metis_tac []) >>
    fs [] >>
    first_x_assum (mp_then.mp_then mp_then.Any mp_tac translate_arg_correct) >>
    disch_then drule >> disch_then drule >> rw [] >>
    qpat_x_assum `v_rel (FlatV _) _` mp_tac >> simp [Once v_rel_cases] >> rw [] >>
    `∃n. r = Reg n` by (Cases_on `r` >> metis_tac []) >>
    qexists_tac `n` >> qexists_tac `translate_ty t` >>
    HINT_EXISTS_TAC >> rw [] >>
    qexists_tac `freeable` >> rw [translate_trace_def]
    >- rw [inc_pc_def, llvmTheory.inc_pc_def, update_result_def]
    >- (
      simp [GSYM translate_reg_def, llvmTheory.inc_pc_def, update_result_def,
            update_results_def, GSYM FUPDATE_EQ_FUPDATE_LIST,
            extend_emap_non_exp_def] >>
      irule mem_state_rel_update_keep >>
      rw []
      >- rw [assigns_cases, IN_DEF, EXTENSION, get_instr_cases, instr_assigns_def]
      >- (
        `s1.ip with i := inc_bip (Offset idx) = inc_pc s1.ip` by rw [inc_pc_def] >>
        simp [] >> irule prog_ok_nonterm >>
        simp [get_instr_cases, terminator_def])
      >- metis_tac [next_ips_reachable, mem_state_rel_def]
      >- (
        fs [w2n_i2n, pointer_size_def, mem_state_rel_def] >>
        metis_tac [bytes_v_rel, get_bytes_erase_tags])
      >- (
        qexists_tac `regs_to_keep` >> rw [] >>
        CCONTR_TAC >> fs [get_instr_cases] >>
        fs [] >> rw [] >> fs [] >> rw [] >>
        rfs [classify_instr_def]))
    >- rw [translate_reg_def]
    >- (
      fs [w2n_i2n, pointer_size_def, mem_state_rel_def] >>
      metis_tac [is_allocated_erase_tags]))
  >- ( (* Store *)
    fs [step_inst_cases, get_instr_cases, PULL_EXISTS] >>
    qpat_x_assum `Store _ _ = el _ _` (assume_tac o GSYM) >>
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
    qpat_x_assum `v_rel (FlatV _) _` mp_tac >> simp [Once v_rel_cases] >> rw [] >>
    drule v_rel_bytes >> rw [] >>
    fs [w2n_i2n, pointer_size_def] >>
    HINT_EXISTS_TAC >> rw [] >>
    qexists_tac `freeable` >> rw [] >>
    qexists_tac `v'` >> rw []
    >- rw [llvmTheory.inc_pc_def, inc_pc_def]
    >- (
      simp [llvmTheory.inc_pc_def] >>
      irule mem_state_rel_no_update >> rw []
      >- rw [assigns_cases, EXTENSION, IN_DEF, get_instr_cases, instr_assigns_def]
      >- (
        `s1.ip with i := inc_bip (Offset idx) = inc_pc s1.ip` by rw [inc_pc_def] >>
        simp [] >> irule prog_ok_nonterm >>
        simp [get_instr_cases, terminator_def]) >>
      irule mem_state_rel_heap_update >>
      rw [set_bytes_unchanged, erase_tags_set_bytes] >>
      fs [mem_state_rel_def, extend_emap_non_exp_def] >>
      metis_tac [set_bytes_heap_ok])
    >- (
      fs [mem_state_rel_def] >>
      fs [is_allocated_def, heap_component_equality, erase_tags_def] >>
      metis_tac [])
    >- (
      fs [get_obs_cases, llvmTheory.get_obs_cases] >> rw [translate_trace_def] >>
      fs [mem_state_rel_def, globals_rel_def]
      >- (
        fs [FLOOKUP_DEF] >>
        first_x_assum drule >> rw []
        >- metis_tac [BIJ_DEF, SURJ_DEF] >>
        pop_assum (mp_tac o GSYM) >> rw [w2n_i2n])
      >- (
        CCONTR_TAC >> fs [FRANGE_DEF] >>
        fs [BIJ_DEF, SURJ_DEF, INJ_DEF] >>
        first_x_assum drule >> rw [] >>
        CCONTR_TAC >> fs [] >> rw [] >>
        first_x_assum drule >> rw [] >>
        CCONTR_TAC >> fs [] >> rw [] >>
        `i2n (IntV (w2i w) (dimindex (:64))) = w2n w` by metis_tac [w2n_i2n, dimindex_64] >>
        fs [] >> rw [] >>
        fs [METIS_PROVE [] ``~x ∨ y ⇔ (x ⇒ y)``] >>
        first_x_assum drule >> rw [] >>
        metis_tac [pair_CASES, SND])))
QED

Theorem classify_instr_term_call:
  ∀i. (classify_instr i = Term ⇒ terminator i) ∧
      (classify_instr i = Call ⇒ is_call i ∨ terminator i)
Proof
  Cases >> rw [classify_instr_def, is_call_def, terminator_def] >>
  Cases_on `p` >> rw [classify_instr_def]
QED

Definition untranslate_glob_var_def:
  untranslate_glob_var (Var_name n ty) = Glob_var n
End

Definition untranslate_trace_def:
  (untranslate_trace Tau = Tau) ∧
  (untranslate_trace Error = Error) ∧
  (untranslate_trace (Exit i) = (Exit i)) ∧
  (untranslate_trace (W gv bytes) = W (untranslate_glob_var gv) bytes)
End

Theorem un_translate_glob_inv:
  ∀x t. untranslate_glob_var (translate_glob_var gmap x) = x
Proof
  Cases_on `x` >> rw [translate_glob_var_def] >>
  CASE_TAC >> rw [untranslate_glob_var_def]
QED

Theorem un_translate_trace_inv:
  ∀x. untranslate_trace (translate_trace gmap x) = x
Proof
  Cases >> rw [translate_trace_def, untranslate_trace_def] >>
  metis_tac [un_translate_glob_inv]
QED

Theorem take_to_call_lem:
  ∀i idx body.
    idx < length body ∧ el idx body = i ∧ ¬terminator i ∧ ¬is_call i ⇒
    take_to_call (drop idx body) = i :: take_to_call (drop (idx + 1) body)
Proof
  Induct_on `idx` >> rw []
  >- (Cases_on `body` >> fs [take_to_call_def] >> rw []) >>
  Cases_on `body` >> fs [] >>
  simp [ADD1]
QED

Theorem inc_translate_label:
  ∀f l x. inc_label (translate_label f l x) = translate_label f l (x + 1)
Proof
  rw [] >> Cases_on `l` >> rw [translate_label_def, inc_label_def] >>
  Cases_on `x'` >> rw [translate_label_def, inc_label_def]
QED

Theorem translate_instrs_correct1:
  ∀prog s1 tr s2.
    multi_step prog s1 tr s2 ⇒
    ∀s1' regs_to_keep b' gmap emap d b idx rest l.
      prog_ok prog ∧ is_ssa prog ∧
      mem_state_rel prog gmap emap s1 s1' ∧
      good_emap s1.ip.f prog regs_to_keep gmap emap ∧
      alookup prog s1.ip.f = Some d ∧
      alookup d.blocks s1.ip.b = Some b ∧
      s1.ip.i = Offset idx ∧
      (l,b')::rest =
        fst (translate_instrs (translate_label (dest_fn s1.ip.f) s1.ip.b (num_calls (take idx b.body)))
               gmap emap regs_to_keep (take_to_call (drop idx b.body))) ∧
      every is_implemented b.body
      ⇒
      ∃s2' tr'.
        step_block (translate_prog prog) s1' b'.cmnd b'.term tr' s2' ∧
        filter ($≠ Tau) tr' = filter ($≠ Tau) (map (translate_trace gmap) tr) ∧
        state_rel prog gmap emap s2 s2'
Proof
  ho_match_mp_tac multi_step_ind >> rw_tac std_ss []
  >- (
    fs [last_step_cases]
    >- ( (* Phi (not handled here) *)
      fs [get_instr_cases])
    >- ( (* Terminator *)
      rename1 `Inl instr` >>
      `is_implemented instr`
      by (fs [get_instr_cases] >> metis_tac [EVERY_MEM, EL_MEM]) >>
      `(∃code. l = Exit code) ∨ l = Tau `
      by (
        fs [llvmTheory.step_cases] >>
        `i' = instr` by metis_tac [get_instr_func, INL_11] >>
        fs [step_instr_cases] >> rfs [terminator_def]) >>
      fs [get_instr_cases, translate_trace_def] >> rw [] >>
      `el idx b.body = el 0 (drop idx b.body)` by rw [EL_DROP] >>
      fs [] >>
      Cases_on `drop idx b.body` >> fs [DROP_NIL] >> rw []
      >- ( (* Exit *)
        fs [llvmTheory.step_cases, get_instr_cases, step_instr_cases,
            translate_instrs_def, take_to_call_def, classify_instr_def,
            translate_instr_to_term_def, translate_instr_to_inst_def,
            llvmTheory.get_obs_cases] >>
        simp [Once step_block_cases, step_term_cases, PULL_EXISTS, step_inst_cases] >>
        drule translate_arg_correct >>
        disch_then drule >> impl_tac
        >- (
          `get_instr prog s1.ip (Inl (Exit a))` by rw [get_instr_cases] >>
          drule get_instr_live >>
          simp [uses_cases, SUBSET_DEF, IN_DEF, PULL_EXISTS] >>
          rw [] >> first_x_assum irule >>
          disj1_tac >>
          metis_tac [instr_uses_def]) >>
        rw [] >>
        qexists_tac `s1' with status := Complete code` >>
        qexists_tac `[Exit code]` >>
        rw []
        >- (
          rfs [translate_instrs_def, classify_instr_def] >>
          rw [translate_instr_to_term_def] >>
          fs [v_rel_cases] >> fs [signed_v_to_int_def] >> metis_tac []) >>
        rw [state_rel_def] >>
        metis_tac [mem_state_rel_exited]) >>
      fs [take_to_call_def] >>
      rfs [] >>
      fs [translate_instrs_def] >>
      Cases_on `el idx b.body` >> fs [terminator_def, classify_instr_def, translate_trace_def] >> rw []
      >- fs [is_implemented_def]
      >- ( (* Br *)
        simp [translate_instr_to_term_def, Once step_block_cases] >>
        simp [step_term_cases, PULL_EXISTS, RIGHT_AND_OVER_OR, EXISTS_OR_THM] >>
        pairarg_tac >> rw [] >>
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
        GEN_EXISTS_TAC "idx'" `w2i tf` >> simp [GSYM PULL_EXISTS] >> conj_tac
        >- metis_tac [] >>
        rename1 `el _ _ = Br e lab1 lab2` >>
        qpat_abbrev_tac `target = if tf = 0w then l2 else l1` >>
        `last b.body = Br e l1 l2 ∧
         <|f := s1.ip.f; b := Some target; i := Phi_ip s1.ip.b|> ∈ next_ips prog s1.ip`
        by (
          fs [prog_ok_def, get_instr_cases] >>
          last_x_assum drule >> disch_then drule >>
          strip_tac >> conj_asm1_tac
          >- (
            CCONTR_TAC >>
            `Br a l1 l2 ∈ set (front (b.body))`
            by (
              `mem (Br a l1 l2) (front b.body ++ [last b.body])`
              by metis_tac [EL_MEM, APPEND_FRONT_LAST] >>
              fs [] >> metis_tac []) >>
            fs [EVERY_MEM] >> first_x_assum drule >> rw [terminator_def])
          >- (
            rw [next_ips_cases, IN_DEF, assigns_cases] >>
            disj1_tac >>
            qexists_tac `Br a l1 l2` >>
            rw [instr_next_ips_def, Abbr `target`] >>
            fs [get_instr_cases, instr_to_labs_def] >>
            metis_tac [blockHeader_nchotomy])) >>
        qmatch_goalsub_abbrev_tac `state_rel _ _ _ _ (_ with bp := target')` >>
        rw [state_rel_def]
        >- (
          fs [get_instr_cases] >>
          `every (λlab. ∃b phis landing. alookup d.blocks (Some lab) = Some b ∧ b.h = Head phis landing)
                 (instr_to_labs (last b.body))`
          by (fs [prog_ok_def, EVERY_MEM] >> metis_tac []) >>
          rfs [instr_to_labs_def] >>
          rw [Once pc_rel_cases, get_instr_cases, get_block_cases, PULL_EXISTS] >>
          fs [GSYM PULL_EXISTS, Abbr `target`] >>
          rw [MEM_MAP, instr_to_labs_def] >>
          `s1.ip.b = option_map Lab l' ∧ dest_fn s1.ip.f = f`
          by (
            Cases_on `s1.ip.b` >>
            fs [translate_label_def] >>
            Cases_on `x` >>
            fs [translate_label_def]) >>
          rw [OPTION_MAP_COMPOSE, combinTheory.o_DEF, dest_label_def, Abbr
              `target'`, word_0_w2i, METIS_PROVE [w2i_eq_0] ``∀w. 0 = w2i w ⇔ w = 0w``] >>
          TRY (Cases_on `l'` >> rw [] >> NO_TAC))
        >- (
          fs [mem_state_rel_def, local_state_rel_def, emap_invariant_def] >> rw []
          >- (
            qpat_x_assum `∀r. r ∈ live _ _ ⇒ P r` mp_tac >>
            simp [Once live_gen_kill] >> disch_then (qspec_then `r` mp_tac) >>
            impl_tac >> rw [] >>
            rw [PULL_EXISTS] >>
            disj1_tac >>
            qexists_tac `<|f := s1.ip.f; b := Some target; i := Phi_ip s1.ip.b|>` >>
            rw [] >>
            rw [IN_DEF, assigns_cases] >>
            CCONTR_TAC >> fs [] >>
            imp_res_tac get_instr_func >> fs [] >> rw [] >>
            fs [instr_assigns_def])
          >- (
            fs [reachable_def] >>
            qexists_tac `path ++ [<|f := s1.ip.f; b := Some target; i := Phi_ip s1.ip.b|>]` >>
            rw_tac std_ss [good_path_append, GSYM APPEND] >> rw []
            >- (rw [Once good_path_cases] >> fs [next_ips_cases, IN_DEF] >> metis_tac [])
            >- metis_tac [ip_equiv_next_ips, ip_equiv_sym]
            >- metis_tac [ip_equiv_refl])))
      >- fs [is_implemented_def]
      >- fs [is_implemented_def]
      >- ( (* Exit *)
        fs [llvmTheory.step_cases, get_instr_cases, step_instr_cases])
      >- fs [is_implemented_def])
    >- ( (* Call *)
      rename1 `Inl instr` >>
      `is_implemented instr`
      by (fs [get_instr_cases] >> metis_tac [EVERY_MEM, EL_MEM]) >>
      Cases_on `instr` >>
      fs [is_call_def, is_implemented_def])
    >- ( (* Stuck *)
      rw [translate_trace_def] >>
      (* TODO: need to know that stuck LLVM instructions translate to stuck
       * llair instructions. This will follow from knowing that when a llair
       * instruction takes a step, the LLVM source can take the same step, ie,
       * the backward direction of the proof. *)
      cheat))
  >- ( (* Middle of the block *)
    fs [llvmTheory.step_cases] >> TRY (fs [get_instr_cases] >> NO_TAC) >>
    `i' = i` by metis_tac [get_instr_func, INL_11] >> fs [] >>
    rename [`step_instr _ _ _ _ s2`, `state_rel _ _ _ s3 _`,
            `mem_state_rel _ _ _ s1 s1'`] >>
    Cases_on `∃r t. classify_instr i = Exp r t` >> fs [] >>
    `is_implemented i`
    by (fs [get_instr_cases] >> metis_tac [EVERY_MEM, EL_MEM])
    >- ( (* instructions that compile to expressions *)
      drule translate_instr_to_exp_correct >>
      ntac 6 (disch_then drule) >>
      rw [] >> fs [translate_trace_def] >>
      `reachable prog (inc_pc s1.ip)`
      by metis_tac [prog_ok_nonterm, next_ips_reachable, mem_state_rel_def] >>
      first_x_assum drule >>
      simp [inc_pc_def, inc_bip_def] >>
      disch_then drule >>
      `take_to_call (drop idx b.body) = i :: take_to_call (drop (idx + 1) b.body)`
      by (
        irule take_to_call_lem >> simp [] >>
        fs [get_instr_cases]) >>
      `num_calls (take (idx + 1) b.body) = num_calls (take idx b.body)`
      by (fs [get_instr_cases] >> rw [num_calls_def, TAKE_EL_SNOC, FILTER_SNOC]) >>
      fs [translate_instrs_def, inc_translate_label] >>
      Cases_on `r ∉ regs_to_keep` >> fs [] >> rw []
      >- (
        `emap = emap |+ (r,translate_instr_to_exp gmap emap i)`
        by (
          fs [good_emap_def, fmap_eq_flookup, FLOOKUP_UPDATE] >>
          rw [] >> metis_tac []) >>
        metis_tac []) >>
      pairarg_tac >> fs [] >> rw [] >>
      `emap |+ (r,Var (translate_reg r t) F) = emap`
      by (
        fs [good_emap_def, fmap_eq_flookup, FLOOKUP_UPDATE] >>
        rw [] >>
        `(r,t) ∈ assigns prog s1.ip`
        by (
          drule exp_assigns_sing >>
          disch_then drule >>
          rw []) >>
        metis_tac [FST, SND, not_exp_def]) >>
      fs [] >>
      rename1 `translate_instrs _ _ _ _ _ = (bs, emap1)` >>
      Cases_on `bs` >> fs [add_to_first_block_def] >>
      rename1 `translate_instrs _ _ _ _ _ = (b1::bs, _)` >>
      Cases_on `b1` >> fs [add_to_first_block_def] >> rw [] >>
      rename1 `state_rel prog gmap emap3 s3 s3'` >>
      qexists_tac `s3'` >> rw [] >>
      qexists_tac `Tau::tr'` >> rw [] >>
      simp [Once step_block_cases] >>
      metis_tac [])
    >- ( (* Non-expression instructions *)
      Cases_on `classify_instr i` >> fs [classify_instr_term_call]
      >- (
        drule translate_instr_to_inst_correct >>
        ntac 6 (disch_then drule) >>
        strip_tac >> fs [] >>
        first_x_assum drule >> simp [inc_pc_def, inc_bip_def] >>
        disch_then (qspecl_then [`regs_to_keep`] mp_tac) >> simp [] >>
        strip_tac >>
        `take_to_call (drop idx b.body) = i :: take_to_call (drop (idx + 1) b.body)`
        by (
          irule take_to_call_lem >> simp [] >>
          fs [get_instr_cases]) >>
        `num_calls (take (idx + 1) b.body) = num_calls (take idx b.body)`
        by (fs [get_instr_cases] >> rw [num_calls_def, TAKE_EL_SNOC, FILTER_SNOC]) >>
        fs [translate_instrs_def, inc_translate_label] >>
        pairarg_tac >> fs [] >>
        `extend_emap_non_exp emap i = emap`
        by (
          Cases_on `∀r t. instr_assigns i ≠ {(r,t)}`
          >- metis_tac [extend_emap_non_exp_no_assigns] >>
          fs [] >>
          drule extend_emap_non_exp_assigns >>
          rw [] >> fs [good_emap_def] >>
          first_x_assum (qspec_then `s1.ip` mp_tac) >> rw [] >>
          last_x_assum (qspec_then `i` mp_tac) >>
          simp [not_exp_def] >>
          disch_then (qspec_then `(r,t)` mp_tac) >>
          impl_tac
          >- (
            fs [IN_DEF, assigns_cases, EXTENSION] >>
            metis_tac []) >>
          rw [fmap_eq_flookup, FLOOKUP_UPDATE] >> rw [] >> rw []) >>
        fs [] >>
        rename1 `translate_instrs _ _ _ _ _ = (bs, emap1)` >>
        Cases_on `bs` >> fs [add_to_first_block_def] >>
        rename1 `translate_instrs _ _ _ _ _ = (b1::bs, _)` >>
        Cases_on `b1` >> fs [add_to_first_block_def] >> fs [] >>
        rw [] >>
        rename1 `state_rel prog gmap emap3 s3 s3'` >>
        qexists_tac `s3'` >> simp [] >>
        qexists_tac `translate_trace gmap l::tr'` >> rw [] >>
        simp [Once step_block_cases]
        >- (disj2_tac >> qexists_tac `s2'` >> rw [])
        >- (disj2_tac >> qexists_tac `s2'` >> rw [])
        >- metis_tac [])
      >- metis_tac [classify_instr_term_call]))
QED

Theorem do_phi_vals:
  ∀prog gmap emap from_l s s' phis updates.
    mem_state_rel prog gmap emap s s' ∧
    list_rel (do_phi from_l s) phis updates ∧
    BIGUNION (set (map (phi_uses from_l) phis)) ⊆ live prog s.ip
    ⇒
    ∃es vs.
      list_rel v_rel (map (λx. (snd x).value) updates) vs ∧
      list_rel (eval_exp s') es vs ∧
      map fst updates = map fst (map phi_assigns phis) ∧
      map (λx. case x of Phi r t largs =>
                 case option_map (λarg. translate_arg gmap emap arg) (alookup largs from_l) of
                   None => (translate_reg r t,Nondet)
                 | Some e => (translate_reg r t,e))
          phis
      = map2 (\p. λe. case p of Phi r t largs => (translate_reg r t, e)) phis es
Proof
  Induct_on `phis` >> rw [] >> fs [] >>
  first_x_assum drule >> disch_then drule >> rw [PULL_EXISTS] >>
  Cases_on `h` >> fs [do_phi_cases] >>
  drule translate_arg_correct >>
  disch_then drule >>
  impl_tac
  >- (fs [phi_uses_def] >> rfs []) >>
  rw [PULL_EXISTS, phi_assigns_def] >> metis_tac []
QED

Triviality case_phi_lift:
  ∀f g. f (case x of Phi x y z => g x y z) = case x of Phi x y z => f (g x y z)
Proof
  Cases_on `x` >> rw []
QED

Triviality id2:
  (λ(v,r). (v,r)) = I
Proof
  rw [FUN_EQ_THM] >> Cases_on `x` >> rw []
QED

Theorem build_phi_block_correct_helper[local]:
  ∀phis es.
    map (λx. case x of
               Phi r t largs =>
                 case option_map (λarg. translate_arg gmap emap arg) (alookup largs from_l) of
                   None => (translate_reg r t,Nondet)
                 | Some e => (translate_reg r t,e)) phis =
    map2 (λp e. case p of Phi r t largs => (translate_reg r t,e)) phis es ∧
    length phis = length es
    ⇒
    es = map (λx. case x of Phi r t largs =>
                    case option_map (λarg. translate_arg gmap emap arg) (alookup largs from_l) of
                      None => Nondet
                    | Some e => e)
            phis
Proof
  Induct >> rw [] >> Cases_on `es` >> fs [] >>
  CASE_TAC >> fs [] >> CASE_TAC >> fs []
QED

Theorem build_phi_block_correct:
  ∀prog s1 s1' to_l from_l phis updates f gmap emap entry bloc regs_to_keep.
    prog_ok prog ∧ is_ssa prog ∧
    good_emap s1.ip.f prog regs_to_keep gmap emap ∧
    get_instr prog s1.ip (Inr (from_l,phis)) ∧
    list_rel (do_phi from_l s1) phis updates ∧
    mem_state_rel prog gmap emap s1 s1' ∧
    BIGUNION (set (map (phi_uses from_l) phis)) ⊆ live prog s1.ip ∧
    bloc = generate_move_block f gmap emap phis from_l to_l
    ⇒
    ∃s2'.
      s2'.bp = translate_label f (Some to_l) 0 ∧
      step_block (translate_prog prog) s1' bloc.cmnd bloc.term [Tau; Tau] s2' ∧
      mem_state_rel prog gmap emap
        (inc_pc (s1 with locals := s1.locals |++ updates)) s2'
Proof
  rw [translate_header_def, generate_move_block_def] >>
  rw [Once step_block_cases] >>
  rw [Once step_block_cases] >>
  rw [step_term_cases, PULL_EXISTS] >>
  simp [Once eval_exp_cases, truncate_2comp_def] >>
  drule do_phi_vals >> ntac 2 (disch_then drule) >>
  rw [] >> drule build_phi_block_correct_helper >>
  pop_assum kall_tac >>
  `length phis = length es` by metis_tac [LENGTH_MAP, LIST_REL_LENGTH] >>
  disch_then drule >>
  rw [] >> fs [LIST_REL_MAP1, combinTheory.o_DEF, case_phi_lift] >>
  simp [step_inst_cases, PULL_EXISTS] >>
  qexists_tac `0` >> qexists_tac `vs` >> rw []
  >- (
    simp [LIST_REL_MAP1, combinTheory.o_DEF] >> fs [LIST_REL_EL_EQN] >>
    rw [] >>
    first_x_assum (qspec_then `n` mp_tac) >> simp [] >>
    CASE_TAC >> simp [] >> CASE_TAC >> simp [build_move_for_lab_def] >>
    CASE_TAC >> simp [] >> fs []) >>
  fs [header_to_emap_upd_def] >>
  simp [llvmTheory.inc_pc_def, update_results_def] >>
  `s1.ip with i := inc_bip s1.ip.i ∈ next_ips prog s1.ip`
  by (
    simp [next_ips_cases, IN_DEF, inc_pc_def] >> disj2_tac >>
    qexists_tac `from_l` >> qexists_tac `phis` >>
    fs [get_instr_cases, EXISTS_OR_THM, inc_bip_def, prog_ok_def] >>
    res_tac >> Cases_on `b.body` >> fs []) >>
  fs [mem_state_rel_def] >> rw []
  >- (
    first_assum (mp_then.mp_then mp_then.Any mp_tac local_state_rel_updates_keep) >>
    rpt (disch_then (fn x => first_assum (mp_then.mp_then mp_then.Any mp_tac x))) >>
    disch_then
      (qspecl_then [`map (λ(x:phi). case x of Phi r t _ => (r,t)) phis`,
                    `map snd updates`, `vs`] mp_tac) >>
    simp [] >> impl_tac >> rw [id2]
    >- (
      fs [prog_ok_def] >>
      first_x_assum drule >>
      simp [MAP_MAP_o, combinTheory.o_DEF] >>
      `(λp. case p of Phi r v1 v2 => r) = (λx. fst (case x of Phi r t v2 => (r,t)))`
      by (rw [FUN_EQ_THM] >> CASE_TAC >> rw []) >>
      rw [])
    >- (
      rw [EXTENSION, IN_DEF, assigns_cases] >> eq_tac >> rw [] >> fs [LIST_TO_SET_MAP]
      >- (
        disj2_tac >> qexists_tac `from_l` >> qexists_tac `phis` >> rw [] >>
        HINT_EXISTS_TAC >> CASE_TAC >> rw [phi_assigns_def])
      >- metis_tac [get_instr_func, sum_distinct]
      >- (
        rw [] >> rename1 `mem x1 phis1` >>
        qexists_tac `x1` >> Cases_on `x1` >> rw [phi_assigns_def] >>
        metis_tac [get_instr_func, INR_11, PAIR_EQ]))
    >- (
      rw [assigns_cases, EXTENSION, IN_DEF] >>
      metis_tac [get_instr_func, sum_distinct, INR_11, PAIR_EQ])
    >- metis_tac [LENGTH_MAP]
    >- rw [LIST_REL_MAP1, combinTheory.o_DEF] >>
    `map fst (map (λx. case x of Phi r t v2 => (r,t)) phis) =
     map fst (map phi_assigns phis)`
    by (rw [LIST_EQ_REWRITE, EL_MAP] >> CASE_TAC >> rw [phi_assigns_def]) >>
    fs [MAP_MAP_o, combinTheory.o_DEF, case_phi_lift] >>
    `zip (map (λx. fst (phi_assigns x)) phis, map snd updates) = updates`
    by (
      qpat_x_assum `map fst _ = map (λx. fst (phi_assigns x)) _` mp_tac >>
      simp [LIST_EQ_REWRITE, EL_MAP] >>
      `length phis = length updates` by metis_tac [LIST_REL_LENGTH] >>
      rw [EL_ZIP, LENGTH_MAP, EL_MAP] >>
      rename1 `_ = el n updates` >>
      first_x_assum drule >>
      Cases_on `el n updates` >> rw []) >>
    `(λx. case x of Phi r t v2 => translate_reg r t) = (λx. fst (build_move_for_lab gmap emap from_l x))`
    by (
      rw [FUN_EQ_THM] >>
      CASE_TAC >> rw [build_move_for_lab_def] >> CASE_TAC >> rw []) >>
    fs [])
  >- (irule next_ips_reachable >> qexists_tac `s1.ip` >> rw [])
QED

Theorem translate_instrs_take_to_call:
  ∀l gmap emap regs body.
    body ≠ [] ∧ terminator (last body) ⇒
    fst (translate_instrs l gmap emap regs (take_to_call body)) =
    [HD (fst (translate_instrs l gmap emap regs body))]
Proof
  Induct_on `body` >> rw [translate_instrs_def, take_to_call_def] >>
  rename1 `classify_instr inst` >> Cases_on `classify_instr inst` >> fs [] >>
  fs [] >> rw [] >> fs []
  >- metis_tac [classify_instr_lem, instr_class_distinct]
  >- metis_tac [classify_instr_lem, instr_class_distinct]
  >- metis_tac [classify_instr_lem, instr_class_distinct]
  >- (
    Cases_on `body` >> fs []
    >- rw [translate_instrs_def] >>
    pairarg_tac >> rw [])
  >- metis_tac [classify_instr_lem, instr_class_distinct]
  >- metis_tac [classify_instr_lem, instr_class_distinct]
  >- metis_tac [classify_instr_lem, instr_class_distinct]
  >- (
    Cases_on `body` >> fs []
    >- rw [translate_instrs_def] >>
    pairarg_tac >> rw [])
  >- (
    `body ≠ []` by (Cases_on `body` >> fs []) >>
    fs [LAST_DEF] >>
    pairarg_tac >> fs [] >> pairarg_tac >> fs [] >>
    `bs = [HD bs']` by metis_tac [FST] >>
    Cases_on `bs'` >> fs []
    >- metis_tac [translate_instrs_not_empty] >>
    Cases_on `h` >> fs [add_to_first_block_def])
  >- (
    `body ≠ []` by (Cases_on `body` >> fs []) >>
    fs [LAST_DEF] >>
    pairarg_tac >> fs [])
  >- (
    `body ≠ []` by (Cases_on `body` >> fs []) >>
    fs [LAST_DEF] >>
    pairarg_tac >> fs [] >> pairarg_tac >> fs [] >>
    `bs = [HD bs']` by metis_tac [FST] >>
    Cases_on `bs'` >> fs []
    >- metis_tac [translate_instrs_not_empty] >>
    Cases_on `h` >> fs [add_to_first_block_def])
  >- metis_tac [classify_instr_lem, instr_class_distinct]
QED

Theorem multi_step_to_step_block:
  ∀prog emap s1 tr s2 s1'.
    prog_ok prog ∧ is_ssa prog ∧
    dominator_ordered prog ∧
    good_emap s1.ip.f prog (get_regs_to_keep (THE (alookup prog s1.ip.f))) (get_gmap prog) emap ∧
    multi_step prog s1 tr s2 ∧
    s1.status = Partial ∧
    state_rel prog (get_gmap prog) emap s1 s1' ∧
    every (\(l, d). every (\(l, b). every is_implemented b.body) d.blocks) prog
    ⇒
    ∃s2' b tr'.
      get_block (translate_prog prog) s1'.bp b ∧
      step_block (translate_prog prog) s1' b.cmnd b.term tr' s2' ∧
      filter ($≠ Tau) tr' = filter ($≠ Tau) (map (translate_trace (get_gmap prog)) tr) ∧
      state_rel prog (get_gmap prog) emap s2 s2'
Proof
  rw [] >> ntac 2 (pop_assum mp_tac) >> simp [Once state_rel_def] >> rw [Once pc_rel_cases]
  >- (
    (* Non-phi instruction *)
    drule translate_instrs_correct1 >> simp [] >>
    disch_then drule >>
    disch_then drule >>
    rfs [] >> disch_then drule >>
    impl_tac
    >- (
      fs [EVERY_MEM] >> rw [] >>
      `mem (s1.ip.f, d) prog` by fs [alookup_some] >>
      first_x_assum drule >> rw [] >>
      `mem (s1.ip.b, b) d.blocks` by fs [alookup_some] >>
      first_x_assum drule >> rw []) >>
    rw [] >>
    rename1 `step_block _ s1' b1.cmnd b1.term tr1 s2'` >>
    qexists_tac `s2'` >> qexists_tac `b1` >> qexists_tac `tr1` >> simp []) >>
  (* Phi instruction *)
  reverse (fs [Once multi_step_cases])
  >- metis_tac [get_instr_func, sum_distinct] >>
  qpat_x_assum `last_step _ _ _ _` mp_tac >>
  simp [last_step_cases] >> strip_tac
  >- (
    fs [llvmTheory.step_cases]
    >- metis_tac [get_instr_func, sum_distinct] >>
    fs [translate_trace_def] >> rw [] >>
    `(from_l', phis') = (from_l, phis) ∧ x = (from_l, phis)` by metis_tac [get_instr_func, INR_11] >>
    fs [] >> rw [] >>
    rfs [MEM_MAP] >>
    Cases_on `s1.ip.f` >> fs [dest_fn_def] >>
    drule get_block_translate_prog_mov >> rpt (disch_then drule) >> rw [PULL_EXISTS] >>
    `∃block l. alookup d.blocks (Some (Lab to_l)) = Some block ∧ block.h = Head phis l`
    by (
      fs [prog_ok_def, EVERY_MEM] >>
      last_x_assum drule >> disch_then drule >> rw [] >>
      first_x_assum drule >> rw [] >>
      rw [] >>
      fs [get_instr_cases] >>
      rfs [] >> rw [] >> fs []) >>
    `every (λ(l,b). every is_implemented b.body) d.blocks`
    by (
      `mem (Fn s, d) prog` by fs [alookup_some] >>
      fs [EVERY_MEM] >> rw [] >> pairarg_tac >> fs [] >> rw [] >>
      first_x_assum drule >> simp [] >>
      disch_then drule >> simp []) >>
    first_x_assum drule >> rw [] >>
    qmatch_assum_abbrev_tac `get_block _ _ bloc` >>
    GEN_EXISTS_TAC "b" `bloc` >>
    rw [] >>
    qpat_x_assum `_ = Fn _` (assume_tac o GSYM) >> fs [] >>
    drule build_phi_block_correct >> rpt (disch_then drule) >>
    simp [Abbr `bloc`] >>
    disch_then (qspecl_then [`Lab to_l`, `s`] mp_tac) >>
    simp [] >>
    impl_tac
    >- (
      drule get_instr_live >> rw [SUBSET_DEF, uses_cases, IN_DEF] >>
      first_x_assum irule >> disj2_tac >> metis_tac []) >>
    rw [] >>
    qexists_tac `s2'` >>
    rw [CONJ_ASSOC, LEFT_EXISTS_AND_THM, RIGHT_EXISTS_AND_THM]
    >- (qexists_tac `[Tau; Tau]` >> rw []) >>
    fs [state_rel_def] >> rw [] >>
    fs [llvmTheory.inc_pc_def] >>
    fs [pc_rel_cases, get_instr_cases, PULL_EXISTS, translate_label_def,
        dest_fn_def, inc_bip_def, label_to_fname_def] >>
    fs [] >> rw [] >> fs [get_block_cases, PULL_EXISTS, label_to_fname_def] >>
    rfs [] >> rw [] >>
    qpat_x_assum `Fn _ = _` (assume_tac o GSYM) >> fs [] >>
    drule alookup_translate_prog >> rw [] >>
    rw [GSYM PULL_EXISTS, dest_fn_def]
    >- (fs [prog_ok_def] >> res_tac >> fs [] >> Cases_on `b'.body` >> fs []) >>
    rw [PULL_EXISTS, translate_def_def] >>
    `b'.body ≠ [] ∧ terminator (last b'.body) ∧
     every (λi. ¬terminator i) (front b'.body) ∧
     every (λb. (snd b).h = Entry ⇔ fst b = None) d.blocks ∧
     0 ≤ num_calls b'.body`
    by (
      fs [prog_ok_def] >> res_tac >> fs [] >>
      fs [EVERY_MEM]) >>
    qmatch_goalsub_abbrev_tac `translate_blocks f gmap _ regs edg _` >>
    `translate_blocks f gmap fempty regs edg d.blocks = translate_blocks f gmap emap regs edg d.blocks`
    by (
      irule translate_blocks_emap_restr_live >>
      unabbrev_all_tac >> rw []
      >- metis_tac [prog_ok_terminator_last]
      >- (
        fs [prog_ok_def] >> fs [EVERY_MEM] >> rw [] >>
        pairarg_tac >> rw [] >> metis_tac [FST, SND])
      >- metis_tac [dominator_ordered_linear_live, similar_emap_def, DRESTRICT_IS_FEMPTY]) >>
    rw [] >>
    drule alookup_translate_blocks >> rpt (disch_then drule) >>
    impl_tac
    >- (
      fs [prog_ok_def, EVERY_MEM] >> rw [] >>
      irule ALOOKUP_ALL_DISTINCT_MEM >> rw [] >>
      imp_res_tac ALOOKUP_MEM >>
      res_tac >> fs []) >>
    simp [translate_label_def] >>
    rw [] >> rw [dest_label_def, num_calls_def] >>
    rw [translate_instrs_take_to_call] >>
    qmatch_goalsub_abbrev_tac `_ = HD (fst (translate_instrs a1 b1 c1 d1 e1))` >>
    Cases_on `translate_instrs a1 b1 c1 d1 e1` >> rw [] >>
    rename1 `_ = HD bl` >> Cases_on `bl` >> rw []
    >- metis_tac [translate_instrs_not_empty, classify_instr_lem] >>
    rename1 `(_,_) = bl` >> Cases_on `bl` >> rw [] >>
    metis_tac [translate_instrs_first_lab])
  >- metis_tac [get_instr_func, sum_distinct]
  >- metis_tac [get_instr_func, sum_distinct]
  >- (
    (* TODO: LLVM "eval" gets stuck *)
    cheat)
QED

Theorem step_block_to_multi_step:
  ∀prog s1 s1' tr s2' b.
    state_rel prog gmap emap s1 s1' ∧
    get_block (translate_prog prog) s1'.bp b ∧
    step_block (translate_prog prog) s1' b.cmnd b.term tr s2'
    ⇒
    ∃s2.
      multi_step prog s1 (map untranslate_trace tr) s2 ∧
      state_rel prog gmap emap s2 s2'
Proof
  (* TODO, LLVM can simulate llair direction *)
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
    dominator_ordered prog ∧
    good_emap (first path).ip.f prog (get_regs_to_keep (THE (alookup prog (first path).ip.f))) (get_gmap prog) emap ∧
    state_rel prog (get_gmap prog) emap (first path) s1' ∧
    every (\(l, d). every (\(l, b). every is_implemented b.body) d.blocks) prog
    ⇒
    ∃path'.
      finite path' ∧
      okpath (step (translate_prog prog)) path' ∧
      first path' = s1' ∧
      LMAP (filter ($≠ Tau)) (labels path') =
      LMAP (map (translate_trace (get_gmap prog)) o filter ($≠ Tau)) (labels path) ∧
      state_rel prog (get_gmap prog) emap (last path) (last path')
Proof
  ho_match_mp_tac finite_okpath_ind >> rw []
  >- (qexists_tac `stopped_at s1'` >> rw [] >> metis_tac []) >>
  fs [] >>
  rename1 `state_rel _ _ _ s1 s1'` >>
  Cases_on `s1.status ≠ Partial`
  >- fs [Once multi_step_cases, llvmTheory.step_cases, last_step_cases] >>
  fs [] >>
  drule multi_step_to_step_block >> simp [] >>
  rpt (disch_then drule) >> rw [] >>
  (* TODO: this won't be true once calls are added *)
  `s1.ip.f = (first path).ip.f` by cheat >>
  fs [] >>
  first_x_assum drule >> disch_then drule >> rw [] >>
  qexists_tac `pcons s1' tr' path'` >> rw [] >>
  rw [FILTER_MAP, combinTheory.o_DEF, trans_trace_not_tau] >>
  simp [step_cases] >> qexists_tac `b` >> simp [] >>
  qpat_x_assum `state_rel _ _ _ _ s1'` mp_tac >>
  rw [state_rel_def, mem_state_rel_def]
QED

Theorem translate_prog_correct_lem2:
  ∀path'.
    okpath (step (translate_prog prog)) path' ∧ finite path'
    ⇒
    ∀s1.
    prog_ok prog ∧
    state_rel prog gmap emap s1 (first path')
    ⇒
    ∃path.
      finite path ∧
      okpath (multi_step prog) path ∧
      first path = s1 ∧
      labels path = LMAP (map untranslate_trace) (labels path') ∧
      state_rel prog gmap emap (last path) (last path')
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
  (* TODO, LLVM can simulate llair direction *)
  cheat
QED

Theorem prefix_take_filter_lemma:
  ∀l lsub.
    lsub ≼ l
    ⇒
    filter (λy. Tau ≠ y) lsub =
    take (length (filter (λy. Tau ≠ y) lsub)) (filter (λy. Tau ≠ y) l)
Proof
  Induct_on `lsub` >> rw [] >>
  Cases_on `l` >> fs [] >> rw []
QED

Theorem multi_step_lab_label:
  ∀prog s1 ls s2.
    multi_step prog s1 ls s2 ⇒ s2.status ≠ Partial
    ⇒
    ∃ls'. (∃i. ls = ls' ++ [Exit i]) ∨ ls = ls' ++ [Error]
Proof
  ho_match_mp_tac multi_step_ind >> rw [] >> fs [] >>
  fs [last_step_cases, llvmTheory.step_cases, step_instr_cases,
      update_result_def, llvmTheory.inc_pc_def] >>
  rw [] >> fs []
QED

Theorem prefix_filter_len_eq:
  ∀l1 l2 x.
    l1 ≼ l2 ++ [x] ∧
    length (filter P l1) = length (filter P (l2 ++ [x])) ∧
    P x
    ⇒
    l1 = l2 ++ [x]
Proof
  Induct_on `l1` >> rw [FILTER_APPEND] >>
  Cases_on `l2` >> fs [] >> rw [] >> rfs [ADD1] >>
  first_x_assum irule >> rw [FILTER_APPEND]
QED

Theorem translate_prog_correct:
  ∀prog s1 s1' emap.
    prog_ok prog ∧ is_ssa prog ∧ dominator_ordered prog ∧
    good_emap s1.ip.f prog (get_regs_to_keep (THE (alookup prog s1.ip.f))) (get_gmap prog) emap ∧
    state_rel prog (get_gmap prog) emap s1 s1' ∧
    every (\(l, d). every (\(l, b). every is_implemented b.body) d.blocks) prog
    ⇒
    multi_step_sem prog s1 = image (I ## map untranslate_trace) (sem (translate_prog prog) s1')
Proof
  rw [sem_def, multi_step_sem_def, EXTENSION] >> eq_tac >> rw []
  >- (
    drule translate_prog_correct_lem1 >> simp [] >>
    rpt (disch_then drule) >> rw [EXISTS_PROD] >>
    PairCases_on `x` >> rw [] >>
    qexists_tac `map (translate_trace (get_gmap prog)) x1` >> rw []
    >- rw [MAP_MAP_o, combinTheory.o_DEF, un_translate_trace_inv] >>
    qexists_tac `path'` >> rw [] >>
    fs [IN_DEF, observation_prefixes_cases, toList_some] >> rw [] >>
    `∃labs. labels path' = fromList labs`
    by (
      fs [GSYM finite_labels] >>
      imp_res_tac llistTheory.LFINITE_toList >>
      fs [toList_some]) >>
    fs [] >>
    rfs [lmap_fromList, combinTheory.o_DEF, MAP_MAP_o] >>
    simp [FILTER_FLAT, MAP_FLAT, MAP_MAP_o, combinTheory.o_DEF, FILTER_MAP]
    >- fs [state_rel_def, mem_state_rel_def]
    >- fs [state_rel_def, mem_state_rel_def] >>
    rename [`labels path' = fromList l'`, `labels path = fromList l`,
            `state_rel _ _ _ (last path) (last path')`, `lsub ≼ flat l`] >>
    Cases_on `lsub = flat l` >> fs []
    >- (
      qexists_tac `flat l'` >>
      rw [FILTER_FLAT, MAP_FLAT, MAP_MAP_o, combinTheory.o_DEF] >>
      fs [state_rel_def, mem_state_rel_def]) >>
    `filter (λy. Tau ≠ y) (flat l') = map (translate_trace (get_gmap prog)) (filter (λy. Tau ≠ y) (flat l))`
    by rw [FILTER_FLAT, MAP_FLAT, MAP_MAP_o, combinTheory.o_DEF, FILTER_MAP] >>
    qexists_tac `take_prop ($≠ Tau) (length (filter ($≠ Tau) lsub)) (flat l')` >>
    rw [] >> rw [GSYM MAP_TAKE]
    >- metis_tac [prefix_take_filter_lemma] >>
    CCONTR_TAC >> fs [] >>
    `(last path).status = (last path').status` by fs [state_rel_def, mem_state_rel_def] >>
    drule take_prop_eq >> strip_tac >>
    `length (filter (λy. Tau ≠ y) (flat l')) = length (filter (λy. Tau ≠ y) (flat l))`
    by rw [] >>
    fs [] >> drule filter_is_prefix >>
    disch_then (qspec_then `$≠ Tau` assume_tac) >>
    drule IS_PREFIX_LENGTH >> strip_tac >> fs [] >>
    `length (filter (λy. Tau ≠ y) lsub) = length (filter (λy. Tau ≠ y) (flat l))` by rw [] >>
    fs [] >> rw [] >>
    qspec_then `path` assume_tac finite_path_end_cases >> rfs [] >> fs [] >> rw []
    >- (`l = []` by metis_tac [llistTheory.fromList_EQ_LNIL] >> fs [] >> rfs []) >>
    rfs [labels_plink] >>
    rename1 `LAPPEND (labels path) [|last_l'|] = _` >>
    `toList (LAPPEND (labels path) [|last_l'|]) = Some l` by metis_tac [llistTheory.from_toList] >>
    drule llistTheory.toList_LAPPEND_APPEND >> strip_tac >>
    fs [llistTheory.toList_THM] >> rw [] >>
    drule multi_step_lab_label >> strip_tac >> rfs [] >> fs [] >>
    drule prefix_filter_len_eq >> rw [] >>
    qexists_tac `$≠ Tau` >> rw [])
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
