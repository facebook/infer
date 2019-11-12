(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Proofs about llvm to llair translation *)

open HolKernel boolLib bossLib Parse lcsymtacs;
open listTheory arithmeticTheory pred_setTheory finite_mapTheory wordsTheory integer_wordTheory;
open optionTheory rich_listTheory pathTheory alistTheory pairTheory sumTheory;
open settingsTheory miscTheory memory_modelTheory;
open llvmTheory llvm_propTheory llvm_ssaTheory llairTheory llair_propTheory llvm_to_llairTheory;

new_theory "llvm_to_llair_prop";

set_grammar_ancestry ["llvm", "llair", "llair_prop", "llvm_to_llair", "llvm_ssa"];

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

Definition dest_llair_lab_def:
  dest_llair_lab (Lab_name f b) = (f, b)
End

Definition build_phi_block_def:
  build_phi_block gmap emap f entry from_l to_l phis =
    generate_move_block [(to_l, (translate_header (dest_fn f) gmap emap entry (Head phis ARB), (ARB:block)))]
      (translate_label_opt (dest_fn f) entry from_l) to_l
End

Definition build_phi_emap_def:
  build_phi_emap phis =
    map (\x. case x of Phi r t _ => (r, Var (translate_reg r t))) phis
End

Inductive pc_rel:
 (* LLVM side points to a normal instruction *)
 (∀prog emap ip bp d b idx b' prev_i fname gmap.
    (* Both are valid pointers to blocks in the same function *)
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
      b' = fst (translate_instrs fname gmap emap regs_to_keep (take_to_call (drop idx b.body))))
    ⇒
    pc_rel prog gmap emap ip bp) ∧

 (* If the LLVM side points to phi instructions, the llair side
  * should point to a block generated from them *)
 (∀prog gmap emap ip bp from_l phis entry to_l.
    get_instr prog ip (Inr (from_l, phis)) ∧
    (* We should have just jumped here from block from_l *)
    (∃d b. alookup prog ip.f = Some d ∧
       alookup d.blocks from_l = Some b ∧
       ip.b ∈ set (map Some (instr_to_labs (last b.body)))) ∧
    get_block (translate_prog prog) bp (build_phi_block gmap emap ip.f entry from_l to_l phis) ∧
    pc_rel prog gmap (emap |++ build_phi_emap phis) (ip with i := inc_bip ip.i) to_l
    ⇒
    pc_rel prog gmap emap ip bp)
End

Definition untranslate_reg_def:
  untranslate_reg (Var_name x t) = Reg x
End

(* Define when an LLVM state is related to a llair one.
 * Parameterised on a map for locals relating LLVM registers to llair
 * expressions that compute the value in that register. This corresponds to part
 * of the translation's state.
 *)

Definition emap_invariant_def:
  emap_invariant prog emap ip locals locals' r =
    ∃v v' e.
      v_rel v.value v' ∧
      flookup locals r = Some v ∧
      flookup emap r = Some e ∧ eval_exp <| locals := locals' |> e v' ∧
      (* Each register used in e is dominated by an assignment to that
       * register for the entire live range of r. *)
      (∀ip1 r'. ip1.f = ip.f ∧ r ∈ live prog ip1 ∧ r' ∈ exp_uses e ⇒
        ∃ip2. untranslate_reg r' ∈ assigns prog ip2 ∧ dominates prog ip2 ip1)
End

Definition local_state_rel_def:
  local_state_rel prog emap ip locals locals' ⇔
    (* Live LLVM registers are mapped and have a related value in the emap
     * (after evaluating) *)
    (∀r. r ∈ live prog ip ⇒ emap_invariant prog emap ip locals locals' r)
End

Definition mem_state_rel_def:
  mem_state_rel prog gmap emap (s:llvm$state) (s':llair$state) ⇔
    local_state_rel prog emap s.ip s.locals s'.locals ∧
    reachable prog s.ip ∧
    fmap_rel (\(_,n) n'. w2n n = n')
      s.globals
      (s'.glob_addrs f_o translate_glob_var gmap) ∧
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
  ((s:llair$state) with status := Complete code).locals = s.locals
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

Triviality record_lemma:
  (<|locals := x|> :llair$state).locals = x
Proof
  rw []
QED

Theorem mem_state_rel_update:
  ∀prog gmap emap s1 s1' v res_v r e i.
  is_ssa prog ∧
  assigns prog s1.ip = {r} ∧
  mem_state_rel prog gmap emap s1 s1' ∧
  eval_exp s1' e res_v ∧
  v_rel v.value res_v ∧
  i ∈ next_ips prog s1.ip ∧
  (∀r_use. r_use ∈ exp_uses e ⇒
    ∃r_tmp. r_use ∈ exp_uses (translate_arg gmap emap (Variable r_tmp)) ∧ r_tmp ∈ live prog s1.ip)
  ⇒
  mem_state_rel prog gmap (emap |+ (r, e))
        (s1 with <|ip := i; locals := s1.locals |+ (r, v) |>)
        s1'
Proof
  rw [mem_state_rel_def, local_state_rel_def, emap_invariant_def]
  >- (
    rw [FLOOKUP_UPDATE]
    >- (
      HINT_EXISTS_TAC >> rw []
      >- metis_tac [eval_exp_ignores, record_lemma] >>
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
QED

Theorem emap_inv_updates_keep_same_ip1:
  ∀prog emap ip locals locals' vs res_vs rtys r.
  is_ssa prog ∧
  list_rel v_rel (map (\v. v.value) vs) res_vs ∧
  length rtys = length vs ∧
  r ∈ set (map fst rtys)
  ⇒
  emap_invariant prog (emap |++ map (\(r,ty). (r, Var (translate_reg r ty))) rtys) ip
        (locals |++ zip (map fst rtys, vs))
        (locals' |++ zip (map (\(r,ty). translate_reg r ty) rtys, res_vs))
        r
Proof
  rw [emap_invariant_def, flookup_fupdate_list] >>
  CASE_TAC >> rw []
  >- (fs [ALOOKUP_NONE, MAP_REVERSE] >> rfs [MAP_ZIP]) >>
  CASE_TAC >> rw []
  >- (
    fs [ALOOKUP_NONE, MAP_REVERSE, MAP_MAP_o, combinTheory.o_DEF] >>
    fs [MEM_MAP, FORALL_PROD] >>
    rw [] >> metis_tac [FST, pair_CASES]) >>
  rename [`alookup (reverse (zip _)) _ = Some v`,
          `alookup (reverse (map _ _)) _ = Some e`] >>
  fs [Once MEM_SPLIT_APPEND_last] >>
  fs [alookup_some, MAP_EQ_APPEND, reverse_eq_append] >> rw [] >>
  rfs [zip_eq_append] >> rw [] >> rw [] >>
  rename [`(fst rty, e)::reverse res = map _ rtys`] >>
  Cases_on `rtys` >> fs [] >> pairarg_tac >> fs [] >> rw [] >>
  fs [] >> rw [] >>
  qpat_x_assum `reverse _ ++ _ = zip _` (mp_tac o GSYM) >> rw [zip_eq_append] >>
  fs [] >> rw [] >>
  rename [`[_] = zip (x,y)`] >>
  Cases_on `x` >> Cases_on `y` >> fs [] >>
  rw [] >> fs [LIST_REL_SPLIT1] >> rw [] >>
  HINT_EXISTS_TAC >> rw []
  >- (
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
    rfs [SWAP_REVERSE_SYM] >> rw [] >> fs [MAP_REVERSE] >> rfs [MAP_ZIP] >>
    fs [MIN_DEF] >>
    BasicProvers.EVERY_CASE_TAC >> fs [] >>
    rfs [] >> rw [] >>
    fs [MAP_MAP_o, combinTheory.o_DEF, LAMBDA_PROD] >>
    `(\(x:reg,y:ty). x) = fst` by (rw [FUN_EQ_THM] >> pairarg_tac >> rw []) >>
    fs [] >>
    rename [`map fst l1 ++ [fst _] ++ map fst l2 = l3 ++ [_] ++ l4`,
            `map _ l1 ++ [translate_reg _ _] ++ _ = l5 ++ _ ++ l6`,
            `l7 ++ [v1:llair$flat_v reg_v] ++ l8 = l9 ++ [v2] ++ l10`] >>
    `map fst l2 = l4` by metis_tac [append_split_last] >>
    `~mem (translate_reg (fst rty) ty) (map (λ(r,ty). translate_reg r ty) l2)`
    by (
      rw [MEM_MAP] >> pairarg_tac >> fs [] >>
      Cases_on `rty` >>
      rename1 `fst (r2, ty2)` >> Cases_on `r2` >> Cases_on `r` >>
      fs [translate_reg_def, MEM_MAP] >> metis_tac [FST]) >>
    `map (λ(r,ty). translate_reg r ty) l2 = l6` by metis_tac [append_split_last] >>
    `length l8 = length l10` by metis_tac [LIST_REL_LENGTH, LENGTH_MAP] >>
    metis_tac [append_split_eq])
  >- (
    fs [exp_uses_def] >> rw [] >>
    Cases_on `fst rty` >> simp [translate_reg_def, untranslate_reg_def] >>
    `∃ip. ip.f = ip1.f ∧ Reg s ∈ uses prog ip`
    by (
      qabbrev_tac `x = (ip1.f = ip.f)` >>
      fs [live_def] >> qexists_tac `last (ip1::path)` >> rw [] >>
      irule good_path_same_func >>
      qexists_tac `ip1::path` >> rw [MEM_LAST] >>
      metis_tac []) >>
    metis_tac [ssa_dominates_live_range])
QED

Theorem emap_inv_updates_keep_same_ip2:
  ∀prog emap ip locals locals' vs res_vs rtys r.
  is_ssa prog ∧
  r ∈ live prog ip ∧
  assigns prog ip = set (map fst rtys) ∧
  emap_invariant prog emap ip locals locals' r ∧
  list_rel v_rel (map (\v. v.value) vs) res_vs ∧
  length rtys = length vs ∧
  reachable prog ip ∧
  ¬mem r (map fst rtys)
  ⇒
  emap_invariant prog (emap |++ map (\(r,ty). (r, Var (translate_reg r ty))) rtys) ip
        (locals |++ zip (map fst rtys, vs))
        (locals' |++ zip (map (\(r,ty). translate_reg r ty) rtys, res_vs))
        r
Proof
  rw [emap_invariant_def, alistTheory.flookup_fupdate_list] >> rw [] >>
  CASE_TAC >> rw []
  >- (
    CASE_TAC >> rw []
    >- (
      qexists_tac `v'` >> rw [] >>
      `DRESTRICT (locals' |++ zip (map (λ(r,ty). translate_reg r ty) rtys, res_vs)) (exp_uses e) =
       DRESTRICT locals' (exp_uses e)`
      suffices_by metis_tac [eval_exp_ignores_unused, record_lemma] >>
      rw [] >>
      qmatch_goalsub_abbrev_tac `_ |++ l = _` >>
      `l = []` suffices_by rw [FUPDATE_LIST_THM] >>
      rw [Abbr `l`, FILTER_EQ_NIL, LAMBDA_PROD] >>
      `(λ(p1,p2:llair$flat_v reg_v). p1 ∉ exp_uses e) = (\x. fst x ∉ exp_uses e)`
      by (rw [EXTENSION, IN_DEF] >> pairarg_tac >> rw []) >>
      `length rtys = length res_vs` by metis_tac [LIST_REL_LENGTH, LENGTH_MAP] >>
      rw [every_zip_fst, EVERY_MAP] >> rw [LAMBDA_PROD] >>
      rw [EVERY_EL] >> pairarg_tac >> rw [] >>
      qmatch_goalsub_rename_tac `translate_reg r1 ty1 ∉ exp_uses _` >>
      first_x_assum (qspecl_then [`ip`, `translate_reg r1 ty1`] mp_tac) >> rw [] >>
      CCONTR_TAC >> fs [] >>
      `ip2 = ip`
      by (
        fs [is_ssa_def, EXTENSION, IN_DEF] >>
        Cases_on `r1` >> fs [translate_reg_def, untranslate_reg_def] >>
        `assigns prog ip (Reg s)` suffices_by metis_tac [reachable_dominates_same_func] >>
        rw [LIST_TO_SET_MAP, MEM_EL] >>
        metis_tac [FST]) >>
      metis_tac [dominates_irrefl]) >>
    drule ALOOKUP_MEM >> rw [MEM_MAP] >>
    pairarg_tac >> fs [MEM_MAP] >> rw [] >>
    metis_tac [FST]) >>
  drule ALOOKUP_MEM >> rw [MEM_MAP, MEM_ZIP] >>
  metis_tac [EL_MEM, LIST_REL_LENGTH, LENGTH_MAP]
QED

Theorem local_state_rel_next_ip:
  ∀prog emap ip1 ip2 locals locals'.
  local_state_rel prog emap ip1 locals locals' ∧
  ip2 ∈ next_ips prog ip1 ∧
  (∀r. r ∈ assigns prog ip1 ⇒ emap_invariant prog emap ip1 locals locals' r)
  ⇒
  local_state_rel prog emap ip2 locals locals'
Proof
  rw [local_state_rel_def, emap_invariant_def] >>
  Cases_on `r ∈ live prog ip1` >> fs []
  >- (
    last_x_assum drule >> rw [] >>
    ntac 3 HINT_EXISTS_TAC >> rw [] >>
    first_x_assum irule >> rw [] >>
    metis_tac [next_ips_same_func]) >>
  pop_assum mp_tac >> simp [Once live_gen_kill, PULL_EXISTS] >> rw [] >>
  first_x_assum (qspec_then `ip2` mp_tac) >> rw [] >>
  first_x_assum drule >> rw [] >>
  ntac 3 HINT_EXISTS_TAC >> rw [] >>
  first_x_assum irule >> rw [] >>
  metis_tac [next_ips_same_func]
QED

Theorem local_state_rel_updates_keep:
  ∀rtys prog emap ip locals locals' vs res_vs i.
  is_ssa prog ∧
  set (map fst rtys) = assigns prog ip ∧
  local_state_rel prog emap ip locals locals' ∧
  length vs = length rtys ∧
  list_rel v_rel (map (\v. v.value) vs) res_vs ∧
  i ∈ next_ips prog ip ∧
  reachable prog ip
  ⇒
  local_state_rel prog (emap |++ map (\(r,ty). (r, Var (translate_reg r ty))) rtys) i
        (locals |++ zip (map fst rtys, vs))
        (locals' |++ zip (map (\(r,ty). translate_reg r ty) rtys, res_vs))
Proof
  rw [] >> irule local_state_rel_next_ip >>
  qexists_tac `ip` >> rw [] >>
  fs [local_state_rel_def] >> rw []
  >- (irule emap_inv_updates_keep_same_ip1 >> rw []) >>
  fs [local_state_rel_def] >> rw [] >>
  Cases_on `mem r (map fst rtys)`
  >- (irule emap_inv_updates_keep_same_ip1 >> rw []) >>
  irule emap_inv_updates_keep_same_ip2 >> rw []
QED

Theorem local_state_rel_update_keep:
  ∀prog emap ip locals locals' v res_v r i ty.
  is_ssa prog ∧
  assigns prog ip = {r} ∧
  local_state_rel prog emap ip locals locals' ∧
  v_rel v.value res_v ∧
  reachable prog ip ∧
  i ∈ next_ips prog ip
  ⇒
  local_state_rel prog (emap |+ (r, Var (translate_reg r ty)))
        i (locals |+ (r, v)) (locals' |+ (translate_reg r ty, res_v))
Proof
  rw [] >>
  drule local_state_rel_updates_keep >>
  disch_then (qspecl_then [`[(r,ty)]`, `emap`, `ip`] mp_tac) >>
  simp [] >> disch_then drule >>
  disch_then (qspecl_then [`[v]`, `[res_v]`] mp_tac) >>
  simp [] >> disch_then drule >>
  rw [FUPDATE_LIST_THM]
QED

Theorem mem_state_rel_update_keep:
  ∀prog gmap emap s s' v res_v r ty i.
  is_ssa prog ∧
  assigns prog s.ip = {r} ∧
  mem_state_rel prog gmap emap s s' ∧
  v_rel v.value res_v ∧
  reachable prog s.ip ∧
  i ∈ next_ips prog s.ip
  ⇒
  mem_state_rel prog gmap (emap |+ (r, Var (translate_reg r ty)))
        (s with <| ip := i; locals := s.locals |+ (r, v) |>)
        (s' with locals := s'.locals |+ (translate_reg r ty, res_v))
Proof
  rw [mem_state_rel_def]
  >- metis_tac [local_state_rel_update_keep] >>
  metis_tac [next_ips_reachable]
QED

Triviality lemma:
  ((s:llair$state) with heap := h).locals = s.locals
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

Theorem translate_constant_correct_lem:
  (∀c s prog gmap emap s'.
   mem_state_rel prog gmap emap s s'
   ⇒
   ∃v'. eval_exp s' (translate_const gmap c) v' ∧ v_rel (eval_const s.globals c) v') ∧
  (∀(cs : (ty # const) list) s prog gmap emap s'.
   mem_state_rel prog gmap emap s s'
   ⇒
   ∃v'. list_rel (eval_exp s') (map (translate_const gmap o snd) cs) v' ∧ list_rel v_rel (map (eval_const s.globals o snd) cs) v') ∧
  (∀(tc : ty # const) s prog gmap emap s'.
   mem_state_rel prog gmap emap s s'
   ⇒
   ∃v'. eval_exp s' (translate_const gmap (snd tc)) v' ∧ v_rel (eval_const s.globals (snd tc)) v')
Proof
  ho_match_mp_tac const_induction >> rw [translate_const_def] >>
  simp [Once eval_exp_cases, eval_const_def]
  >- (
    Cases_on `s` >> simp [eval_const_def, translate_size_def, v_rel_cases] >>
    metis_tac [truncate_2comp_i2w_w2i, dimindex_1, dimindex_8, dimindex_32, dimindex_64])
  >- (
    simp [v_rel_cases, PULL_EXISTS, MAP_MAP_o] >>
    fs [combinTheory.o_DEF, LAMBDA_PROD] >>
    metis_tac [])
  >- (
    simp [v_rel_cases, PULL_EXISTS, MAP_MAP_o] >>
    fs [combinTheory.o_DEF, LAMBDA_PROD] >>
    metis_tac [])
  (* TODO: unimplemented stuff *)
  >- cheat
  >- (
    fs [mem_state_rel_def, fmap_rel_OPTREL_FLOOKUP] >>
    CASE_TAC >> fs [] >> first_x_assum (qspec_then `g` mp_tac) >> rw [] >>
    rename1 `option_rel _ _ opt` >> Cases_on `opt` >> fs [OPTREL_def] >>
    (* TODO: false at the moment, need to work out the llair story on globals *)
    cheat)
  (* TODO: unimplemented stuff *)
  >- cheat
  >- cheat
QED

Theorem translate_constant_correct:
  ∀c s prog gmap emap s' g.
   mem_state_rel prog gmap emap s s'
   ⇒
   ∃v'. eval_exp s' (translate_const gmap c) v' ∧ v_rel (eval_const s.globals c) v'
Proof
  metis_tac [translate_constant_correct_lem]
QED

(* TODO: This isn't true, since the translation turns LLVM globals into llair
 * locals *)
Theorem translate_const_no_reg[simp]:
  ∀gmap c. r ∉ exp_uses (translate_const gmap c)
Proof
  ho_match_mp_tac translate_const_ind >>
  rw [translate_const_def, exp_uses_def, MEM_MAP, METIS_PROVE [] ``x ∨ y ⇔ (~x ⇒ y)``]
  >- (pairarg_tac >> fs [] >> metis_tac [])
  >- (pairarg_tac >> fs [] >> metis_tac [])
  >- cheat
  >- cheat
QED

Theorem translate_arg_correct:
  ∀s a v prog gmap emap s'.
  mem_state_rel prog gmap emap s s' ∧
  eval s a = Some v ∧
  arg_to_regs a ⊆ live prog s.ip
  ⇒
  ∃v'. eval_exp s' (translate_arg gmap emap a) v' ∧ v_rel v.value v'
Proof
  Cases_on `a` >> rw [eval_def, translate_arg_def] >> rw []
  >- metis_tac [translate_constant_correct] >>
  CASE_TAC >> fs [PULL_EXISTS, mem_state_rel_def, local_state_rel_def, emap_invariant_def, arg_to_regs_def] >>
  res_tac >> rfs [] >> metis_tac [eval_exp_ignores, record_lemma]
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
  ∀prog gmap emap s1 s1' a v v1' e1' cs ns result.
    mem_state_rel prog gmap emap s1 s1' ∧
    map (λci. signed_v_to_num (eval_const s1.globals ci)) cs = map Some ns ∧
    extract_value v ns = Some result ∧
    eval_exp s1' e1' v1' ∧
    v_rel v v1'
    ⇒
    ∃v2'.
      eval_exp s1' (foldl (λe c. Select e (translate_const gmap c)) e1' cs) v2' ∧
      v_rel result v2'
Proof
  Induct_on `cs` >> rw [] >> fs [extract_value_def]
  >- metis_tac [] >>
  first_x_assum irule >>
  Cases_on `ns` >> fs [] >>
  qmatch_goalsub_rename_tac `translate_const gmap c` >>
  `?v2'. eval_exp s1' (translate_const gmap c) v2' ∧ v_rel (eval_const s1.globals c) v2'`
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
  ∀prog gmap emap s1 s1' a v1 v1' v2 v2' e2 e2' e1' cs ns result.
    mem_state_rel prog gmap emap s1 s1' ∧
    map (λci. signed_v_to_num (eval_const s1.globals ci)) cs = map Some ns ∧
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
  Induct_on `cs` >> rw [] >> fs [insert_value_def, translate_updatevalue_def]
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
  `?v4'. eval_exp s1' (translate_const gmap c) v4' ∧ v_rel (eval_const s1.globals c) v4'`
  by metis_tac [translate_constant_correct] >>
  `?idx_size. v4' = FlatV (IntV (&x) idx_size)`
  by (
    pop_assum mp_tac >> simp [Once v_rel_cases] >>
    rw [] >> fs [signed_v_to_num_def, signed_v_to_int_def] >>
    intLib.COOPER_TAC) >>
  first_x_assum drule >>
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

Theorem const_idx_uses[simp]:
  ∀cs gmap e.
    exp_uses (foldl (λe c. Select e (translate_const gmap c)) e cs) = exp_uses e
Proof
  Induct_on `cs` >> rw [exp_uses_def] >>
  rw [translate_const_no_reg, EXTENSION]
QED

Theorem exp_uses_trans_upd_val[simp]:
  ∀cs gmap e1 e2. exp_uses (translate_updatevalue gmap e1 e2 cs) =
    (if cs = [] then {} else exp_uses e1) ∪ exp_uses e2
Proof
  Induct_on `cs` >> rw [exp_uses_def, translate_updatevalue_def] >>
  rw [translate_const_no_reg, EXTENSION] >>
  metis_tac []
QED

(* TODO: identify some lemmas to cut down on the duplicated proof in the very
 * similar cases *)
Theorem translate_instr_to_exp_correct:
  ∀gmap emap instr r t s1 s1' s2 prog l.
    is_ssa prog ∧ prog_ok prog ∧
    classify_instr instr = Exp r t ∧
    mem_state_rel prog gmap emap s1 s1' ∧
    get_instr prog s1.ip (Inl instr) ∧
    step_instr prog s1 instr l s2 ⇒
    ∃pv emap' s2'.
      l = Tau ∧
      s2.ip = inc_pc s1.ip ∧
      mem_state_rel prog gmap emap' s2 s2' ∧
      (r ∉ regs_to_keep ⇒ s1' = s2' ∧ emap' = emap |+ (r, translate_instr_to_exp gmap emap instr)) ∧
      (r ∈ regs_to_keep ⇒
       emap' = emap |+ (r,Var (translate_reg r t)) ∧
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
    `assigns prog s1.ip = {r}`
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
      irule mem_state_rel_update_keep >> rw [])
    >- (
      irule mem_state_rel_update >> rw []
      >- (
        fs [exp_uses_def]
        >| [Cases_on `a1`, Cases_on `a2`] >>
        fs [translate_arg_def] >>
        rename1 `flookup _ r_tmp` >>
        qexists_tac `r_tmp` >> rw [] >>
        simp [Once live_gen_kill] >> disj2_tac >>
        simp [uses_cases, IN_DEF, get_instr_cases, instr_uses_def, arg_to_regs_def]) >>
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
    `assigns prog s1.ip = {r}`
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
      irule mem_state_rel_update_keep >> rw [])
    >- (
      irule mem_state_rel_update >> rw []
      >- (
        Cases_on `a` >>
        fs [translate_arg_def] >>
        rename1 `flookup _ r_tmp` >>
        qexists_tac `r_tmp` >> rw [] >>
        simp [Once live_gen_kill] >> disj2_tac >>
        simp [uses_cases, IN_DEF, get_instr_cases, instr_uses_def, arg_to_regs_def]) >>
      metis_tac [])) >>
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
    disch_then drule >>
    first_x_assum (mp_then.mp_then mp_then.Any mp_tac translate_arg_correct) >>
    disch_then drule >>
    simp [] >> strip_tac >> strip_tac >>
    disch_then (qspecl_then [`v'`, `v''`] mp_tac) >> simp [] >>
    disch_then drule >> disch_then drule >>
    rw [] >>
    rename1 `eval_exp _ (translate_updatevalue _ _ _ _) res_v` >>
    rw [inc_pc_def, llvmTheory.inc_pc_def] >>
    rename1 `r ∈ _` >>
    `assigns prog s1.ip = {r}`
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
      irule mem_state_rel_update_keep >> rw [])
    >- (
      irule mem_state_rel_update >> strip_tac
      >- (
        Cases_on `a1` >> Cases_on `a2` >>
        rw [translate_arg_def] >>
        rename1 `flookup _ r_tmp` >>
        qexists_tac `r_tmp` >> rw [] >>
        simp [Once live_gen_kill] >> disj2_tac >>
        simp [uses_cases, IN_DEF, get_instr_cases, instr_uses_def, arg_to_regs_def]) >>
      rw [] >> metis_tac [] ))>>
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
    `assigns prog s1.ip = {r}`
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
      irule mem_state_rel_update_keep >> rw [])
    >- (
      irule mem_state_rel_update >> simp [] >> strip_tac
      >- (
        rw [] >>
        fs [exp_uses_def] >> Cases_on `a1` >> fs [translate_arg_def] >>
        rename1 `flookup _ r_tmp` >>
        qexists_tac `r_tmp` >> rw [] >>
        simp [Once live_gen_kill] >> disj2_tac >>
        simp [uses_cases, IN_DEF, get_instr_cases, instr_uses_def, arg_to_regs_def]) >>
      metis_tac [])) >>
  (* TODO: unimplemented instruction translations *)
  cheat
QED

Triviality eval_exp_help:
  (s1 with heap := h).locals = s1.locals
Proof
  rw []
QED

Theorem translate_instr_to_inst_correct:
  ∀gmap emap instr r t s1 s1' s2 prog l.
    classify_instr instr = Non_exp ∧
    prog_ok prog ∧ is_ssa prog ∧
    mem_state_rel prog gmap emap s1 s1' ∧
    get_instr prog s1.ip (Inl instr) ∧
    step_instr prog s1 instr l s2
    ⇒
    ∃pv s2'.
      s2.ip = inc_pc s1.ip ∧
      mem_state_rel prog gmap (extend_emap_non_exp emap instr) s2 s2' ∧
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
        metis_tac [bytes_v_rel, get_bytes_erase_tags]))
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
      (* TODO: mem_state_rel needs to relate the globals *)
      fs [get_obs_cases, llvmTheory.get_obs_cases] >> rw [translate_trace_def] >>
      fs [mem_state_rel_def, fmap_rel_OPTREL_FLOOKUP]
      >- (
        first_x_assum (qspec_then `x` mp_tac) >> rw [] >>
        rename1 `option_rel _ _ opt` >> Cases_on `opt` >>
        fs [OPTREL_def] >>
        cheat) >>
      cheat))
QED

Theorem classify_instr_term_call:
  ∀i. (classify_instr i = Term ⇔ terminator i) ∧
      (classify_instr i = Call ⇔ is_call i)
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
  first_x_assum drule >> simp [ADD1]
QED

Theorem translate_instrs_correct1:
  ∀prog s1 tr s2.
    multi_step prog s1 tr s2 ⇒
    ∀s1' b' gmap emap regs_to_keep d b idx.
      prog_ok prog ∧ is_ssa prog ∧
      mem_state_rel prog gmap emap s1 s1' ∧
      alookup prog s1.ip.f = Some d ∧
      alookup d.blocks s1.ip.b = Some b ∧
      s1.ip.i = Offset idx ∧
      b' = fst (translate_instrs (dest_fn s1.ip.f) gmap emap regs_to_keep (take_to_call (drop idx b.body)))
      ⇒
      ∃emap s2' tr'.
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
      `(∃code. l = Exit code) ∨ l = Tau `
      by (
        fs [llvmTheory.step_cases] >>
        `i' = i''` by metis_tac [get_instr_func, INL_11] >>
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
        qexists_tac `emap` >>
        qexists_tac `s1' with status := Complete code` >>
        qexists_tac `[Exit code]` >>
        rw []
        >- (fs [v_rel_cases] >> fs [signed_v_to_int_def] >> metis_tac []) >>
        rw [state_rel_def] >>
        metis_tac [mem_state_rel_exited]) >>
      simp [take_to_call_def, translate_instrs_def] >>
      Cases_on `el idx b.body` >> fs [terminator_def, classify_instr_def, translate_trace_def] >> rw []
      >- ( (* Ret *)
        cheat)
      >- ( (* Br *)
        simp [translate_instr_to_term_def, Once step_block_cases] >>
        simp [step_term_cases, PULL_EXISTS, RIGHT_AND_OVER_OR, EXISTS_OR_THM] >>
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
        `last b.body = Br a l1 l2 ∧
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
          fs [get_instr_cases] >>
          `every (λlab. ∃b phis landing. alookup d.blocks (Some lab) = Some b ∧ b.h = Head phis landing)
                 (instr_to_labs (last b.body))`
          by (fs [prog_ok_def, EVERY_MEM] >> metis_tac []) >>
          rfs [instr_to_labs_def] >>
          rw [Once pc_rel_cases, get_instr_cases, get_block_cases, PULL_EXISTS] >>
          fs [GSYM PULL_EXISTS, Abbr `target`] >>
          rw [MEM_MAP, instr_to_labs_def] >>
          fs [translate_prog_def] >>
          `∀y z. dest_fn y = dest_fn z ⇒ y = z`
          by (Cases_on `y` >> Cases_on `z` >> rw [dest_fn_def]) >>
          rw [alookup_map_key] >>
          (* TODO *)
          cheat)
        >- (
          fs [mem_state_rel_def, local_state_rel_def, emap_invariant_def] >> rw []
          >- (
            qpat_x_assum `!r. r ∈ live _ _ ⇒ P r` mp_tac >>
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
            rw_tac std_ss [good_path_append, GSYM APPEND] >> rw [] >>
            rw [Once good_path_cases] >> fs [next_ips_cases, IN_DEF] >> metis_tac [])))
      >- ( (* Invoke *)
        cheat)
      >- ( (* Unreachable *)
        cheat)
      >- ( (* Exit *)
        fs [llvmTheory.step_cases, get_instr_cases, step_instr_cases])
      >- ( (* Throw *)
        cheat))
    >- ( (* Call *)
      cheat)
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
      disch_then (qspecl_then [`regs_to_keep`] mp_tac) >> rw [] >>
      rename1 `state_rel prog gmap emap3 s3 s3'` >>
      qexists_tac `emap3` >> qexists_tac `s3'` >> rw [] >>
      `take_to_call (drop idx b.body) = i :: take_to_call (drop (idx + 1) b.body)`
      by (
        irule take_to_call_lem >> simp [] >>
        fs [get_instr_cases]) >>
      simp [translate_instrs_def] >>
      Cases_on `r ∉ regs_to_keep` >> fs [] >> rw []
      >- metis_tac [] >>
      qexists_tac `Tau::tr'` >> rw [] >>
      simp [Once step_block_cases] >> disj2_tac >>
      pairarg_tac >> rw [] >> fs [] >>
      metis_tac [])
    >- ( (* Non-expression instructions *)
      Cases_on `classify_instr i` >> fs [classify_instr_term_call] >>
      drule translate_instr_to_inst_correct >>
      ntac 5 (disch_then drule) >>
      strip_tac >> fs [] >>
      first_x_assum drule >> simp [inc_pc_def, inc_bip_def] >>
      disch_then (qspecl_then [`regs_to_keep`] mp_tac) >> simp [] >>
      strip_tac >>
      rename1 `state_rel prog gmap emap3 s3 s3'` >>
      qexists_tac `emap3` >> qexists_tac `s3'` >> simp [] >>
      `take_to_call (drop idx b.body) = i :: take_to_call (drop (idx + 1) b.body)`
      by (
        irule take_to_call_lem >> simp [] >>
        fs [get_instr_cases]) >>
      simp [translate_instrs_def] >>
      qexists_tac `translate_trace gmap l::tr'` >> rw [] >>
      simp [Once step_block_cases] >> pairarg_tac >> rw [] >> fs [] >>
      disj2_tac >>
      qexists_tac `s2'` >> rw []))
QED

Theorem do_phi_vals:
  ∀prog gmap emap from_l s s' phis updates.
    mem_state_rel prog gmap emap s s' ∧
    map (do_phi from_l s) phis = map Some updates ∧
    BIGUNION (set (map (phi_uses from_l) phis)) ⊆ live prog s.ip
    ⇒
    ∃es vs.
      list_rel v_rel (map (λx. (snd x).value) updates) vs ∧
      list_rel (eval_exp s') es vs ∧
      map fst updates = map phi_assigns phis ∧
      map (λx. case x of Phi r t largs =>
                 case option_map (λarg. translate_arg gmap emap arg) (alookup largs from_l) of
                   None => (translate_reg r t,Nondet)
                 | Some e => (translate_reg r t,e))
          phis
      = map2 (\p. λe. case p of Phi r t largs => (translate_reg r t, e)) phis es
Proof
  Induct_on `phis` >> rw [] >> Cases_on `updates` >> fs [] >>
  first_x_assum drule >> disch_then drule >> rw [] >>
  Cases_on `h` >> fs [do_phi_def, OPTION_JOIN_EQ_SOME] >>
  drule translate_arg_correct >>
  disch_then drule >>
  impl_tac
  >- (fs [phi_uses_def] >> rfs []) >>
  rw [PULL_EXISTS, phi_assigns_def] >> metis_tac []
QED

Triviality dest_phi_trip:
  ∀p f. (λ(x,y,z). f x y z) (dest_phi p) = (λx. case x of Phi x y z => f x y z) p
Proof
  Cases >> rw [dest_phi_def]
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

Triviality map_fst_map2:
  ∀l1 l2 f g.
    length l1 = length l2 ⇒
    map fst (map2 (λp e. case p of Phi r t largs => (f r t largs, g e)) l1 l2) =
    map (λp. case p of Phi r t largs => f r t largs) l1
Proof
  Induct_on `l1` >> rw [] >> Cases_on `l2` >> fs [] >>
  CASE_TAC >> rw []
QED

Theorem build_phi_block_correct:
  ∀prog s1 s1' to_l from_l phis updates f gmap emap entry bloc.
    prog_ok prog ∧ is_ssa prog ∧
    get_instr prog s1.ip (Inr (from_l,phis)) ∧
    map (do_phi from_l s1) phis = map Some updates ∧
    mem_state_rel prog gmap emap s1 s1' ∧
    BIGUNION (set (map (phi_uses from_l) phis)) ⊆ live prog s1.ip ∧
    bloc = build_phi_block gmap emap f entry from_l to_l phis
    ⇒
    ?s2'.
      s2'.bp = to_l ∧
      step_block (translate_prog prog) s1' bloc.cmnd bloc.term [Tau; Tau] s2' ∧
      mem_state_rel prog gmap
        (emap |++ build_phi_emap phis)
        (inc_pc (s1 with locals := s1.locals |++ updates)) s2'
Proof
  rw [build_phi_block_def, translate_header_def, generate_move_block_def] >>
  rw [Once step_block_cases] >>
  rw [Once step_block_cases] >>
  rw [step_term_cases, PULL_EXISTS] >>
  simp [Once eval_exp_cases, truncate_2comp_def] >>
  simp [MAP_MAP_o, combinTheory.o_DEF, PULL_EXISTS, dest_phi_trip] >>
  simp [case_phi_lift, build_move_for_lab_def] >>
  (* TODO: This is false because of how the entry block label is translated.
   * Needs fixing. *)
  `∀l1 l2. translate_label_opt (dest_fn f) entry l1 = translate_label_opt (dest_fn f) entry l2 ⇒ l1 = l2`
  by cheat >>
  qspecl_then [`l`, `from_l`, `translate_label_opt (dest_fn f) entry`,
               `\x arg. translate_arg gmap emap arg`]
    (mp_tac o Q.GEN `l`)
    alookup_map_key >>
  simp [] >>
  disch_then kall_tac >>
  drule do_phi_vals >> ntac 2 (disch_then drule) >>
  rw [] >> rw [] >>
  pop_assum kall_tac >>
  simp [step_inst_cases, PULL_EXISTS] >>
  qexists_tac `0` >> qexists_tac `vs` >> rw []
  >- (
    simp [LIST_REL_MAP1] >> fs [LIST_REL_EL_EQN, EL_MAP2] >> rw [MIN_DEF]
    >- metis_tac [LENGTH_MAP, DECIDE ``(x:num) = y ⇒ ~(x < y)``] >>
    CASE_TAC >> simp []) >>
  simp [llvmTheory.inc_pc_def, update_results_def, MAP_ID, id2] >>
  `length phis = length es` by metis_tac [LENGTH_MAP, LIST_REL_LENGTH] >>
  rw [map_fst_map2] >>
  `s1.ip with i := inc_bip s1.ip.i ∈ next_ips prog s1.ip`
  by (
    simp [next_ips_cases, IN_DEF, inc_pc_def] >> disj2_tac >>
    qexists_tac `from_l` >> qexists_tac `phis` >>
    fs [get_instr_cases, EXISTS_OR_THM, inc_bip_def, prog_ok_def] >>
    res_tac >> Cases_on `b.body` >> fs []) >>
  fs [mem_state_rel_def] >> rw []
  >- (
    `map fst (map (λx. case x of Phi r t v2 => (r,t)) phis) =
     map phi_assigns phis`
    by (rw [LIST_EQ_REWRITE, EL_MAP] >> CASE_TAC >> rw [phi_assigns_def]) >>
    first_assum (mp_then.mp_then mp_then.Any mp_tac local_state_rel_updates_keep) >>
    rpt (disch_then (fn x => first_assum (mp_then.mp_then mp_then.Any mp_tac x))) >>
    disch_then
      (qspecl_then [`map (λ(x:phi). case x of Phi r t _ => (r,t)) phis`,
                    `map snd updates`, `vs`] mp_tac) >>
    simp [] >> impl_tac >> rw []
    >- (
      rw [assigns_cases, EXTENSION, IN_DEF] >>
      metis_tac [get_instr_func, sum_distinct, INR_11, PAIR_EQ])
    >- metis_tac [LENGTH_MAP]
    >- rw [MAP_MAP_o, combinTheory.o_DEF] >>
    fs [MAP_MAP_o, combinTheory.o_DEF, case_phi_lift] >>
    `updates = zip (map fst updates,map snd updates)`
    suffices_by metis_tac [build_phi_emap_def] >>
    rw [ZIP_MAP] >>
    rw [LIST_EQ_REWRITE, EL_MAP])
  >- (irule next_ips_reachable >> qexists_tac `s1.ip` >> rw [])
QED

Theorem multi_step_to_step_block:
  ∀prog gmap emap s1 tr s2 s1'.
    prog_ok prog ∧ is_ssa prog ∧
    multi_step prog s1 tr s2 ∧
    s1.status = Partial ∧
    state_rel prog gmap emap s1 s1'
    ⇒
    ∃s2' emap2 b tr'.
      get_block (translate_prog prog) s1'.bp b ∧
      step_block (translate_prog prog) s1' b.cmnd b.term tr' s2' ∧
      filter ($≠ Tau) tr' = filter ($≠ Tau) (map (translate_trace gmap) tr) ∧
      state_rel prog gmap emap2 s2 s2'
Proof
  rw [] >> pop_assum mp_tac >> simp [Once state_rel_def] >> rw [Once pc_rel_cases]
  >- (
    (* Non-phi instruction *)
    drule translate_instrs_correct1 >> simp [] >>
    disch_then drule >>
    disch_then (qspecl_then [`regs_to_keep`] mp_tac) >> simp [] >>
    rw [] >>
    qexists_tac `s2'` >> simp [] >>
    ntac 3 HINT_EXISTS_TAC >>
    rw [] >> fs [dest_fn_def]) >>
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
    qmatch_assum_abbrev_tac `get_block _ _ bloc` >>
    GEN_EXISTS_TAC "b" `bloc` >>
    drule build_phi_block_correct >> ntac 2 (disch_then drule) >>
    simp [Abbr `bloc`] >>
    disch_then (qspecl_then [`s1'`, `to_l`, `updates`, `s1.ip.f`, `gmap`, `emap`, `entry`] mp_tac) >>
    simp [] >>
    impl_tac
    >- (
      drule get_instr_live >> rw [SUBSET_DEF, uses_cases, IN_DEF] >>
      first_x_assum irule >> disj2_tac >> metis_tac []) >>
    rw [] >>
    qexists_tac `s2'` >> qexists_tac `emap |++ build_phi_emap phis` >> qexists_tac `[Tau; Tau]` >> rw [] >>
    fs [state_rel_def] >> rw [] >>
    fs [llvmTheory.inc_pc_def])
  >- metis_tac [get_instr_func, sum_distinct]
  >- metis_tac [get_instr_func, sum_distinct]
  >- (
    fs [llvmTheory.step_cases] >> rw [translate_trace_def] >>
    `!i. ¬get_instr prog s1.ip (Inl i)`
    by metis_tac [get_instr_func, sum_distinct] >>
    fs [METIS_PROVE [] ``~x ∨ y ⇔ (x ⇒ y)``] >>
    first_x_assum drule >> rw [] >>
    `¬every IS_SOME (map (do_phi from_l s1) phis)` by metis_tac [map_is_some] >>
    fs [get_instr_cases] >>
    rename [`alookup _ s1.ip.b = Some b_targ`, `alookup _ from_l = Some b_src`] >>
    `every (phi_contains_label from_l) phis`
    by (
      fs [prog_ok_def, get_instr_cases] >>
      first_x_assum (qspecl_then [`s1.ip.f`, `d`, `from_l`] mp_tac) >> rw [] >>
      fs [EVERY_MEM, MEM_MAP] >>
      rfs [] >> rw [] >> first_x_assum drule >> rw [] >>
      first_x_assum irule >> fs [] >> rfs [] >> fs []) >>
    fs [EVERY_MEM, EXISTS_MEM, MEM_MAP] >>
    first_x_assum drule >> rw [] >>
    rename1 `phi_contains_label _ phi` >> Cases_on `phi` >>
    fs [do_phi_def, phi_contains_label_def] >>
    rename1 `alookup entries from_l ≠ None` >>
    Cases_on `alookup entries from_l` >> fs [] >>
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
    ∀gmap emap s1'.
    prog_ok prog ∧
    is_ssa prog ∧
    state_rel prog gmap emap (first path) s1'
    ⇒
    ∃path' emap.
      finite path' ∧
      okpath (step (translate_prog prog)) path' ∧
      first path' = s1' ∧
      LMAP (filter ($≠ Tau)) (labels path') =
      LMAP (map (translate_trace gmap) o filter ($≠ Tau)) (labels path) ∧
      state_rel prog gmap emap (last path) (last path')
Proof
  ho_match_mp_tac finite_okpath_ind >> rw []
  >- (qexists_tac `stopped_at s1'` >> rw [] >> metis_tac []) >>
  fs [] >>
  rename1 `state_rel _ _ _ s1 s1'` >>
  Cases_on `s1.status ≠ Partial`
  >- fs [Once multi_step_cases, llvmTheory.step_cases, last_step_cases] >>
  fs [] >>
  drule multi_step_to_step_block >> ntac 4 (disch_then drule) >> rw [] >>
  first_x_assum drule >> rw [] >>
  qexists_tac `pcons s1' tr' path'` >> rw [] >>
  rw [FILTER_MAP, combinTheory.o_DEF, trans_trace_not_tau] >>
  HINT_EXISTS_TAC >> simp [] >>
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
  ∀prog s1 s1'.
    prog_ok prog ∧ is_ssa prog ∧
    state_rel prog gmap emap s1 s1'
    ⇒
    multi_step_sem prog s1 = image (I ## map untranslate_trace) (sem (translate_prog prog) s1')
Proof
  rw [sem_def, multi_step_sem_def, EXTENSION] >> eq_tac >> rw []
  >- (
    drule translate_prog_correct_lem1 >> ntac 4 (disch_then drule) >> rw [EXISTS_PROD] >>
    PairCases_on `x` >> rw [] >>
    qexists_tac `map (translate_trace gmap) x1` >> rw []
    >- rw [MAP_MAP_o, combinTheory.o_DEF, un_translate_trace_inv] >>
    qexists_tac `path'` >> rw [] >>
    fs [IN_DEF, observation_prefixes_cases, toList_some] >> rw [] >>
    `?labs. labels path' = fromList labs`
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
    `filter (λy. Tau ≠ y) (flat l') = map (translate_trace gmap) (filter (λy. Tau ≠ y) (flat l))`
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
