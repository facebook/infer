(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Proofs about llvm to llair translation that don't involve the semantics *)

open HolKernel boolLib bossLib Parse lcsymtacs;
open listTheory arithmeticTheory pred_setTheory finite_mapTheory;
open optionTheory rich_listTheory alistTheory pairTheory sumTheory;
open settingsTheory miscTheory;
open llvmTheory llvm_propTheory llvm_ssaTheory llairTheory llair_propTheory llvm_to_llairTheory;

new_theory "llvm_to_llair_prop";

set_grammar_ancestry ["llvm", "llair", "llair_prop", "llvm_to_llair", "llvm_ssa"];

numLib.prefer_num ();

(* ----- Theorems about prog_ok ----- *)

Theorem prog_ok_terminator_last:
  ∀prog d f.
    prog_ok prog ∧
    alookup prog (Fn f) = Some d
    ⇒
    every (λ(l,b). ∀i. i < length b.body ∧ classify_instr (el i b.body) = Term ⇒ Suc i = length b.body) d.blocks
Proof
  rw [prog_ok_def] >> rpt (first_x_assum drule) >>
  rw [] >> fs [EVERY_MEM] >> rw [] >>
  pairarg_tac >> rw [] >>
  `alookup d.blocks l = Some b` by metis_tac [ALOOKUP_ALL_DISTINCT_MEM] >>
  last_x_assum drule >> rw [] >>
  CCONTR_TAC >> fs [] >>
  `Suc i < length b.body` by decide_tac >> fs [] >>
  `terminator (el i b.body)`
  by (
    Cases_on `el i b.body` >> fs [terminator_def, classify_instr_def] >>
    Cases_on `p` >> fs [terminator_def, classify_instr_def]) >>
  `~mem (el i b.body) (front b.body)` by metis_tac [] >>
  metis_tac [mem_el_front]
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

(* ----- The invariant on emaps ----- *)
(* Essentially that the emap is what the llvm->llair translation generates in
  * the end. Such emaps enjoy the nice property that they don't change when used
  * as the input to a translation function, since they have all of the bindings
  * in them already. *)
Definition untranslate_reg_def:
  untranslate_reg (Var_name x t) = Reg x
End

Definition not_exp_def:
  not_exp x regs_to_keep ⇔
    (∀r t. x ≠ Exp r t) ∨ (∃r t. x = Exp r t ∧ r ∈ regs_to_keep)
End

(* This captures the variable->expression mapping that the compiler will have
 * built *)
Definition good_emap_def:
  good_emap f prog regs_to_keep gmap emap ⇔
    ∀ip.
      f = ip.f ⇒
      (∀r t inst.
        get_instr prog ip (Inl inst) ∧ classify_instr inst = Exp r t ∧ r ∉ regs_to_keep
        ⇒
        flookup emap r = Some (translate_instr_to_exp gmap emap inst)) ∧
      (∀inst phis.
        (get_instr prog ip (Inl inst) ∧ not_exp (classify_instr inst) regs_to_keep ∨
         get_instr prog ip (Inr phis))
        ⇒
        ∀rt. rt ∈ assigns prog ip ⇒
          flookup emap (fst rt) = Some (Var (translate_reg (fst rt) (snd rt)) F)) ∧
      (* for each r |-> e mapping in emap, each register e is dominated by an
       * assignment to that register for the entire live range of r *)
      (∀r e r'. flookup emap r = Some e ∧ r ∈ live prog ip ∧ r' ∈ exp_uses e ⇒
        ∃ip2. untranslate_reg r' ∈ image fst (assigns prog ip2) ∧ dominates prog ip2 ip)
End

Theorem good_emap_header_unchanged:
  ∀f prog r gmap emap phis land from l.
    good_emap f prog r gmap emap ∧
    get_instr prog <| f := f; b := l; i := Phi_ip from |> (Inr (from, phis))
    ⇒
    emap |++ header_to_emap_upd (Head phis land) = emap
Proof
  rw [good_emap_def] >>
  irule fupdate_list_elim >> simp [header_to_emap_upd_def, MEM_MAP, PULL_EXISTS] >>
  rpt gen_tac >> CASE_TAC >> rw [] >>
  first_x_assum (qspec_then `<|f := f; b := l; i := Phi_ip from|>` mp_tac) >>
  rw [] >> res_tac >>
  rename1 `flookup _ r1 = Some (Var (translate_reg r1 ty) _)` >>
  first_x_assum (qspec_then `(r1,ty)` mp_tac) >> simp [] >>
  disch_then irule >>
  rw [assigns_cases, IN_DEF] >> disj2_tac >>
  qexists_tac `from` >> qexists_tac `phis` >> rw [LIST_TO_SET_MAP] >>
  metis_tac [phi_assigns_def]
QED

Theorem fupdate_elim_lookup:
  ∀k v f. flookup f k = Some v ⇒ f |+ (k,v) = f
Proof
  rw [FLOOKUP_DEF] >> metis_tac [FUPDATE_ELIM]
QED

Theorem good_emap_translate_unchanged_lem:
  ∀f prog gmap emap regs_to_keep bl bs emap' l i d j.
    good_emap (Fn f) prog regs_to_keep gmap emap ∧
    alookup prog (Fn f) = Some d ∧
    alookup d.blocks l = Some bl ∧
    j ≤ length bl.body ∧
    translate_instrs (Lab_name f (option_map dest_label l) i) gmap emap regs_to_keep (drop j bl.body) = (bs,emap')
    ⇒
    emap = emap'
Proof
  Induct_on `length bl.body - j` >>
  rw []
  >- (
    `length bl.body = j` by decide_tac >>
    rw [] >> fs [DROP_LENGTH_NIL, translate_instrs_def]) >>
  first_x_assum (qspecl_then [`bl`, `j + 1`] mp_tac) >> simp [] >>
  rpt (disch_then drule) >>
  `j < length bl.body` by decide_tac >>
  fs [DROP_EL_CONS, translate_instrs_def] >>
  BasicProvers.EVERY_CASE_TAC >> fs [] >>
  TRY (pairarg_tac >> fs []) >> disch_then irule >>
  rw [] >> fs [inc_label_def]
  >- (
    `emap |+ (r,Var (translate_reg r t) F) = emap` suffices_by metis_tac [] >>
    irule fupdate_elim_lookup >>
    fs [good_emap_def] >>
    first_x_assum (qspec_then `<| f := Fn f; b := l; i := Offset j |>` mp_tac) >>
    rw [get_instr_cases, not_exp_def] >>
    first_x_assum (qspec_then `(r,t)` mp_tac) >> simp [] >>
    disch_then irule >> simp [assigns_cases, IN_DEF] >>
    disj1_tac >> qexists_tac `el j bl.body` >>
    rw [get_instr_cases] >>
    Cases_on `el j bl.body` >> fs [classify_instr_def, instr_assigns_def] >>
    Cases_on `p` >> fs [classify_instr_def, instr_assigns_def])
  >- (
    `emap |+ (r,translate_instr_to_exp gmap emap (el j bl.body)) = emap`
    suffices_by metis_tac [] >>
    irule fupdate_elim_lookup >>
    fs [good_emap_def] >>
    first_x_assum (qspec_then `<| f := Fn f; b := l; i := Offset j |>` mp_tac) >>
    rw [get_instr_cases, not_exp_def])
  >- (
    `extend_emap_non_exp emap (el j bl.body) = emap` suffices_by metis_tac [] >>
    Cases_on `el j bl.body` >> rw [extend_emap_non_exp_def] >>
    irule fupdate_elim_lookup >>
    fs [good_emap_def] >>
    first_x_assum (qspec_then `<| f := Fn f; b := l; i := Offset j |>` mp_tac) >>
    rw [get_instr_cases, not_exp_def] >>
    qmatch_goalsub_abbrev_tac `translate_reg _ t'` >>
    first_x_assum (qspec_then `(r,t')` mp_tac) >> simp [] >>
    disch_then irule >> simp [assigns_cases, IN_DEF] >>
    disj1_tac >> qexists_tac `el j bl.body` >>
    rw [get_instr_cases] >>
    fs [classify_instr_def, instr_assigns_def])
  >- (
    `extend_emap_non_exp emap  (el j bl.body) = emap` suffices_by metis_tac [] >>
    Cases_on `el j bl.body` >> rw [extend_emap_non_exp_def] >>
    irule fupdate_elim_lookup >>
    fs [good_emap_def] >>
    first_x_assum (qspec_then `<| f := Fn f; b := l; i := Offset j |>` mp_tac) >>
    rw [get_instr_cases, not_exp_def] >>
    qmatch_goalsub_abbrev_tac `translate_reg _ t'` >>
    first_x_assum (qspec_then `(r,t')` mp_tac) >> simp [] >>
    disch_then irule >> simp [assigns_cases, IN_DEF] >>
    disj1_tac >> qexists_tac `el j bl.body` >>
    rw [get_instr_cases] >>
    fs [classify_instr_def, instr_assigns_def])
QED

Theorem good_emap_translate_unchanged:
  ∀f prog gmap emap regs_to_keep bl bs emap' l i d.
    good_emap (Fn f) prog regs_to_keep gmap emap ∧
    alookup prog (Fn f) = Some d ∧
    alookup d.blocks l = Some bl ∧
    translate_instrs (Lab_name f (option_map dest_label l) i) gmap emap regs_to_keep bl.body = (bs,emap')
    ⇒
    emap = emap'
Proof
  metis_tac [good_emap_translate_unchanged_lem, DECIDE ``!x. 0 ≤ length x``, DROP_0]
QED

(* ------ Reasoning about emaps that are equal up to the live variables ----- *)
(* We can then prove that the translation is invariant under switching similar
 * emaps *)

Definition similar_emap_def:
  similar_emap live emap1 emap2 ⇔
    DRESTRICT emap1 live = DRESTRICT emap2 live
End

Theorem similar_emap_sym:
  ∀s emap1 emap2. similar_emap s emap1 emap2 ⇔ similar_emap s emap2 emap1
Proof
  rw [similar_emap_def] >> metis_tac []
QED

Theorem similar_emap_subset:
  ∀s1 s2 emap1 emap2.
    similar_emap s1 emap1 emap2 ∧
    s2 ⊆ s1
    ⇒
    similar_emap s2 emap1 emap2
Proof
  rw [similar_emap_def, SUBSET_DEF, fmap_eq_flookup, FLOOKUP_DRESTRICT] >> rw [] >>
  last_x_assum (qspec_then `x` mp_tac) >> rw []
QED

Theorem extend_similar_emap:
  ∀s emap1 emap2 s2 l.
    similar_emap s emap1 emap2 ∧
    set (map fst l) = s2
    ⇒
    similar_emap (s ∪ s2) (emap1 |++ l) (emap2 |++ l)
Proof
  rw [similar_emap_def, fmap_eq_flookup, FLOOKUP_DRESTRICT] >>
  rw []
  >- (first_x_assum (qspec_then `x` mp_tac) >> rw [flookup_fupdate_list]) >>
  rw [flookup_fupdate_list] >>
  CASE_TAC >> fs [ALOOKUP_NONE, MEM_MAP] >> rfs [] >>
  metis_tac []
QED

Theorem similar_emap_lemma:
  ∀s emap1 emap2 l.
    similar_emap s emap1 emap2
    ⇒
    similar_emap s (emap1 |++ l) (emap2 |++ l)
Proof
  rw [similar_emap_def]
QED

Theorem similar_emap_diff:
  ∀s emap1 emap2 l s2.
    similar_emap (s DIFF s2) emap1 emap2 ∧
    s2 = set (map fst l)
    ⇒
    similar_emap s (emap1 |++ l) (emap2 |++ l)
Proof
  rw [similar_emap_def, fmap_eq_flookup, FLOOKUP_DRESTRICT] >> rw [] >>
  first_x_assum (qspec_then `x` mp_tac) >>
  rw [flookup_fupdate_list] >>
  CASE_TAC >> rw [] >> fs [ALOOKUP_NONE, MEM_MAP] >>
  metis_tac []
QED

Theorem extend_similar_emap2:
  ∀inst is emap1 emap2 reg t e.
    similar_emap (fst (instrs_live (inst::is))) emap1 emap2 ∧
    instr_assigns inst = {(reg, t)}
    ⇒
    similar_emap (fst (instrs_live is)) (emap1 |+ (reg,e)) (emap2 |+ (reg,e))
Proof
  rw [similar_emap_def, instrs_live_def, DRESTRICT_FUPDATE, fmap_eq_flookup,
      FLOOKUP_UPDATE, FLOOKUP_DRESTRICT] >>
  CASE_TAC >> rw [] >>
  first_x_assum (qspec_then `x` mp_tac) >> rw [] >>
  pairarg_tac >> fs [] >> fs []
QED

Theorem extend_similar_emap3:
  ∀inst emap1 emap2 reg t e.
    similar_emap {reg} (emap1 |+ (reg,e)) (emap2 |+ (reg,e))
Proof
  rw [similar_emap_def, fmap_eq_flookup, FLOOKUP_UPDATE, FLOOKUP_DRESTRICT]
QED

Theorem extend_similar_emap4:
  ∀inst emap1 emap2 is.
    similar_emap (fst (instrs_live (inst::is))) emap1 emap2 ∧
    classify_instr inst = Non_exp
    ⇒
    similar_emap (fst (instrs_live is)) (extend_emap_non_exp emap1 inst) (extend_emap_non_exp emap2 inst)
Proof
  rw [similar_emap_def, instrs_live_def, DRESTRICT_FUPDATE, fmap_eq_flookup,
      FLOOKUP_DRESTRICT] >>
  CASE_TAC >> rw [] >>
  first_x_assum (qspec_then `x` mp_tac) >> rw [] >>
  pairarg_tac >> fs [] >> fs [] >>
  Cases_on `inst` >>
  rw [extend_emap_non_exp_def, FLOOKUP_UPDATE] >>
  fs [instr_assigns_def, instr_uses_def, classify_instr_def] >> rw [] >>
  fs [] >>
  Cases_on `p` >> fs [classify_instr_def]
QED

Theorem similar_emap_union:
  ∀s1 s2 emap1 emap2.
    similar_emap (s1 ∪ s2) emap1 emap2 ⇔
    similar_emap s1 emap1 emap2 ∧ similar_emap s2 emap1 emap2
Proof
  rw [similar_emap_def, fmap_eq_flookup, FLOOKUP_DRESTRICT] >>
  metis_tac []
QED

Theorem similar_emap_insert:
  ∀s1 s2 emap1 emap2.
    similar_emap (s1 INSERT s2) emap1 emap2 ⇔
    similar_emap {s1} emap1 emap2 ∧ similar_emap s2 emap1 emap2
Proof
  rw [similar_emap_def, fmap_eq_flookup, FLOOKUP_DRESTRICT] >>
  metis_tac []
QED

Theorem similar_emap_translate_arg:
  ∀gmap emap1 a s emap2.
    similar_emap s emap1 emap2 ∧
    arg_to_regs a ⊆ s
    ⇒
    translate_arg gmap emap1 a = translate_arg gmap emap2 a
Proof
  Cases_on `a` >> rw [translate_arg_def] >>
  fs [arg_to_regs_def, similar_emap_def, fmap_eq_flookup, FLOOKUP_DRESTRICT] >>
  first_x_assum (qspec_then `r` mp_tac) >> rw []
QED

Theorem similar_emap_translate_instr_to_exp:
  ∀gmap emap1 inst is emap2 r t.
    similar_emap (fst (instrs_live (inst::is))) emap1 emap2 ∧
    classify_instr inst = Exp r t ∧
    is_implemented inst
    ⇒
    translate_instr_to_exp gmap emap1 inst = translate_instr_to_exp gmap emap2 inst
Proof
  ho_match_mp_tac translate_instr_to_exp_ind >>
  rw [translate_instr_to_exp_def, classify_instr_def, instrs_live_def] >>
  pairarg_tac >> fs [instr_uses_def, instr_assigns_def] >>
  drule similar_emap_translate_arg >>
  TRY (disch_then irule) >>
  rw [SUBSET_DEF] >>
  fs [is_implemented_def]
QED

Theorem similar_emap_translate_instr_to_inst:
  ∀gmap emap1 inst is emap2 r t.
    similar_emap (fst (instrs_live (inst::is))) emap1 emap2 ∧
    classify_instr inst = Non_exp ∧
    is_implemented inst
    ⇒
    translate_instr_to_inst gmap emap1 inst = translate_instr_to_inst gmap emap2 inst
Proof
  ho_match_mp_tac translate_instr_to_inst_ind >>
  rw [translate_instr_to_inst_def, classify_instr_def, instrs_live_def] >>
  pairarg_tac >> fs [instr_uses_def, instr_assigns_def] >>
  drule similar_emap_translate_arg >>
  TRY (disch_then irule) >>
  rw [SUBSET_DEF]
  >- (rename1 `Extractvalue _ x _` >> Cases_on `x` >> fs [classify_instr_def])
  >- (rename1 `Insertvalue _ x _ _` >> Cases_on `x` >> fs [classify_instr_def]) >>
  fs [is_implemented_def]
QED

Theorem similar_emap_translate_instr_to_term:
  ∀inst l gmap emap1 is emap2 r t.
    similar_emap (fst (instrs_live (inst::is))) emap1 emap2 ∧
    classify_instr inst = Term ∧
    is_implemented inst
    ⇒
    translate_instr_to_term l gmap emap1 inst = translate_instr_to_term l gmap emap2 inst
Proof
  ho_match_mp_tac classify_instr_ind >>
  rw [translate_instr_to_term_def, classify_instr_def, instrs_live_def,
      instr_uses_def]
  >- fs [is_implemented_def]
  >- (
    CASE_TAC >> rw [] >>
    irule similar_emap_translate_arg >>
    pairarg_tac >>
    fs [similar_emap_union, SUBSET_DEF] >>
    metis_tac [])
  >- fs [is_implemented_def]
  >- (
    irule similar_emap_translate_arg >>
    pairarg_tac >>
    fs [similar_emap_union, SUBSET_DEF] >>
    metis_tac [])
  >- fs [is_implemented_def]
QED

Theorem similar_emap_translate_call:
  ∀inst gmap emap1 is emap2 ret exret.
    similar_emap (fst (instrs_live (inst::is))) emap1 emap2 ∧
    classify_instr inst = Call ∧
    is_implemented inst
    ⇒
    translate_call gmap emap1 ret exret inst = translate_call gmap emap2 ret exret inst
Proof
  ho_match_mp_tac classify_instr_ind >>
  rw [translate_call_def, classify_instr_def, instrs_live_def, instr_uses_def,
      instr_assigns_def] >>
  pairarg_tac >> fs [similar_emap_union]
  >- (
    irule LIST_EQ >> rw [EL_MAP] >>
    rename1 `el x l` >> Cases_on `el x l` >> fs [] >>
    irule similar_emap_translate_arg >>
    qexists_tac `bigunion (set (map (arg_to_regs ∘ snd) l))` >>
    rw [SUBSET_DEF, MEM_MAP, PULL_EXISTS] >>
    metis_tac [EL_MEM, SND])
  >- fs [is_implemented_def]
QED

Theorem translate_header_similar_emap:
  ∀f from l gmap emap1 emap2 h.
    similar_emap (header_uses h) emap1 emap2 ∧
    (l = None ⇔ h = Entry)
    ⇒
    translate_header f from l gmap emap1 h = translate_header f from l gmap emap2 h ∧
    similar_emap (header_uses h ∪ header_assigns h)
      (emap1 |++ header_to_emap_upd h)
      (emap2 |++ header_to_emap_upd h)
Proof
  rw [] >> Cases_on `h` >>
  rw [translate_header_def] >> fs [header_uses_def, header_assigns_def] >>
  rw [header_to_emap_upd_def, FUPDATE_LIST_THM] >>
  `?l2. l = Some l2` by metis_tac [option_nchotomy] >>
  rw [translate_header_def]
  >- (
    rw [LIST_EQ_REWRITE, EL_MAP, generate_move_block_def] >>
    rename1 `build_move_for_lab _ _ _ (el i phis)` >>
    Cases_on `el i phis` >> rw [build_move_for_lab_def] >> CASE_TAC >>
    drule similar_emap_translate_arg >>
    disch_then irule >>
    rw [SUBSET_DEF, PULL_EXISTS] >>
    qexists_tac `el x from` >>
    qexists_tac `el i phis` >> rw [phi_uses_def] >>
    metis_tac [EL_MEM])
  >- (
    irule extend_similar_emap >> rw [] >>
    pop_assum kall_tac >>
    Induct_on `l'` >> rw [EXTENSION] >>
    CASE_TAC >>
    eq_tac >> rw [phi_assigns_def])
QED

Theorem extend_emap_non_exp_assigns:
  ∀inst r t emap.
    instr_assigns inst = {(r,t)} ∧
    (classify_instr inst = Non_exp ∨ classify_instr inst = Call ∨ classify_instr inst = Term) ⇒
    extend_emap_non_exp emap inst =  emap |+ (r,Var (translate_reg r t) F)
Proof
  ho_match_mp_tac instr_assigns_ind >>
  rw [instr_assigns_def, extend_emap_non_exp_def, classify_instr_def]
QED

Theorem extend_emap_non_exp_no_assigns:
  ∀inst emap.
    (∀r t. instr_assigns inst ≠ {(r,t)}) ∧
    classify_instr inst = Non_exp ⇒
    extend_emap_non_exp emap inst = emap ∧
    instr_assigns inst = {}
Proof
  ho_match_mp_tac instr_assigns_ind >>
  rw [instr_assigns_def, extend_emap_non_exp_def, classify_instr_def]
QED

Theorem translate_instrs_similar_emap:
  ∀f l gmap emap1 emap2 regs_to_keep is i.
    similar_emap (fst (instrs_live is)) emap1 emap2 ∧
    (∀i. i < length is ∧ classify_instr (el i is) = Term ⇒ Suc i = length is) ∧
    every is_implemented is
    ⇒
    (λ(b1, emap1') (b2, emap2').
       b1 = b2 ∧
       ∃l.
         set (map fst l) = bigunion (set (map (image fst) (map instr_assigns is))) ∧
         emap1' = emap1 |++ l ∧ emap2' = emap2 |++ l ∧
         similar_emap (fst (instrs_live is) ∪ set (map fst l)) emap1' emap2')
      (translate_instrs (Lab_name f (option_map dest_label l) i) gmap emap1 regs_to_keep is)
      (translate_instrs (Lab_name f (option_map dest_label l) i) gmap emap2 regs_to_keep is)
Proof
  Induct_on `is` >> rw [translate_instrs_def]
  >- metis_tac [FUPDATE_LIST_THM]
  >- metis_tac [FUPDATE_LIST_THM] >>
  ntac 2 (pairarg_tac >> fs []) >>
  rename1 `classify_instr inst` >>
  Cases_on `classify_instr inst` >> fs []
  >- (
    rename1 `reg ∈ regs_to_keep` >>
    `instr_assigns inst = {(reg, t)}`
    by (
      Cases_on `inst` >> fs [classify_instr_def, instr_assigns_def] >>
      Cases_on `p` >> fs [classify_instr_def, instr_assigns_def]) >>
    Cases_on `reg ∈ regs_to_keep` >> fs []
    >- (
      ntac 2 (pairarg_tac >> fs []) >> rpt var_eq_tac >>
      fs [] >>
      `similar_emap (fst (instrs_live is))
        (emap1 |+ (reg,Var (translate_reg reg t) F))
        (emap2 |+ (reg,Var (translate_reg reg t) F))`
      by (
        irule extend_similar_emap2 >>
        qexists_tac `inst` >> rw [] >>
        Cases_on `inst` >> fs [classify_instr_def, instr_assigns_def] >>
        Cases_on `p` >> fs [classify_instr_def, instr_assigns_def]) >>
      first_x_assum drule >>
      disch_then (qspecl_then [`f`, `l`, `gmap`, `regs_to_keep`, `i`] mp_tac) >>
      pairarg_tac >> fs [] >>
      rw []
      >- metis_tac [similar_emap_translate_instr_to_exp] >>
      qexists_tac `(reg,Var (translate_reg reg t) F) :: l'` >> rw []
      >- rw [EXTENSION, MEM_MAP]
      >- metis_tac [FUPDATE_LIST_THM]
      >- metis_tac [FUPDATE_LIST_THM] >>
      fs [instrs_live_def] >> pairarg_tac >> simp [] >>
      fs [similar_emap_union] >> rw []
      >- metis_tac [similar_emap_lemma, FUPDATE_LIST_THM]
      >- metis_tac [similar_emap_lemma, FUPDATE_LIST_THM] >>
      simp [Once similar_emap_insert] >> rw [] >>
      metis_tac [extend_similar_emap3, similar_emap_lemma])
    >- (
      drule similar_emap_translate_instr_to_exp >> simp [] >>
      disch_then (qspec_then `gmap` mp_tac) >>
      disch_tac >> fs [] >>
      `similar_emap (fst (instrs_live is))
        (emap1 |+ (reg,translate_instr_to_exp gmap emap2 inst))
        (emap2 |+ (reg,translate_instr_to_exp gmap emap2 inst))`
      by (irule extend_similar_emap2 >> qexists_tac `inst` >> rw []) >>
      first_x_assum drule >>
      disch_then (qspecl_then [`f`, `l`, `gmap`, `regs_to_keep`, `i`] mp_tac) >>
      pairarg_tac >> fs [] >> rw [] >>
      qexists_tac `(reg,translate_instr_to_exp gmap emap2 inst) :: l'` >> rw []
      >- rw [EXTENSION, MEM_MAP]
      >- metis_tac [FUPDATE_LIST_THM]
      >- metis_tac [FUPDATE_LIST_THM] >>
      fs [instrs_live_def] >> pairarg_tac >> simp [] >>
      fs [similar_emap_union] >> rw []
      >- metis_tac [similar_emap_lemma, FUPDATE_LIST_THM]
      >- metis_tac [similar_emap_lemma, FUPDATE_LIST_THM] >>
      simp [Once similar_emap_insert] >> rw [] >>
      metis_tac [extend_similar_emap3, similar_emap_lemma]))
  >- (
    ntac 2 (pairarg_tac >> fs []) >> rpt var_eq_tac >>
    drule extend_similar_emap4 >> simp [] >> disch_tac >>
    first_x_assum drule >>
    disch_then (qspecl_then [`f`, `l`, `gmap`, `regs_to_keep`, `i`] mp_tac) >>
    pairarg_tac >> fs [] >>
    rw []
    >- metis_tac [similar_emap_translate_instr_to_inst] >>
    Cases_on `∃r t. instr_assigns inst = {(r,t)}` >> fs []
    >- (
      drule extend_emap_non_exp_assigns >> rw [] >>
      qexists_tac `(r,Var (translate_reg r t) F) :: l'` >> rw []
      >- rw [EXTENSION]
      >- metis_tac [FUPDATE_LIST_THM]
      >- metis_tac [FUPDATE_LIST_THM] >>
      fs [instrs_live_def] >> pairarg_tac >> simp [] >>
      fs [similar_emap_union] >> rw []
      >- metis_tac [similar_emap_lemma, FUPDATE_LIST_THM]
      >- metis_tac [similar_emap_lemma, FUPDATE_LIST_THM] >>
      simp [Once similar_emap_insert] >> rw [] >>
      metis_tac [extend_similar_emap3, similar_emap_lemma])
    >- (
      drule extend_emap_non_exp_no_assigns >> rw [] >>
      qexists_tac `l'` >> rw [] >>
      fs [instrs_live_def] >> pairarg_tac >> simp [] >>
      fs [similar_emap_union] >> rw [] >>
      metis_tac [similar_emap_lemma, FUPDATE_LIST_THM]))
  >- (
    rw []
    >- metis_tac [similar_emap_translate_instr_to_term] >>
    `instr_assigns inst = {}`
     by (
       Cases_on `inst` >> fs [classify_instr_def, instr_assigns_def] >>
       Cases_on `p` >> fs [classify_instr_def, instr_assigns_def]) >>
    `is = []`
    by (first_x_assum (qspec_then `0` mp_tac) >> simp []) >>
    rw [FUPDATE_LIST_THM])
  >- (
      ntac 2 (pairarg_tac >> fs []) >> rpt var_eq_tac >> fs [] >>
      `?r t. instr_assigns inst = {(r,t)}`
      by (
        Cases_on `inst` >> fs [classify_instr_def, instr_assigns_def] >>
        Cases_on `p` >> fs [classify_instr_def, instr_assigns_def]) >>
      drule extend_emap_non_exp_assigns >> simp [] >>
      disch_tac >> fs [] >>
      `similar_emap (fst (instrs_live is))
        (emap1 |+ (r,Var (translate_reg r t) F))
        (emap2 |+ (r,Var (translate_reg r t) F))`
      by metis_tac [extend_similar_emap2] >>
      first_x_assum drule >>
      disch_then (qspecl_then [`f`, `l`, `gmap`, `regs_to_keep`, `i + 1`] mp_tac) >>
      fs [inc_label_def] >> rw []
      >- metis_tac [similar_emap_translate_call] >>
      qexists_tac `(r,Var (translate_reg r t) F) :: l'` >> rw []
      >- rw [EXTENSION, MEM_MAP]
      >- metis_tac [FUPDATE_LIST_THM]
      >- metis_tac [FUPDATE_LIST_THM] >>
      fs [instrs_live_def] >> pairarg_tac >> simp [] >>
      fs [similar_emap_union] >> rw []
      >- metis_tac [similar_emap_lemma, FUPDATE_LIST_THM]
      >- metis_tac [similar_emap_lemma, FUPDATE_LIST_THM] >>
      simp [Once similar_emap_insert] >> rw [] >>
      metis_tac [extend_similar_emap3, similar_emap_lemma])
QED

Theorem header_to_emap_upd_fst:
  ∀b.
    set (map fst (header_to_emap_upd b.h)) =
    {fst (phi_assigns p) | (land,p,phis) | b.h = Head phis land ∧ mem p phis}
Proof
  rw [EXTENSION] >> eq_tac >> rw [MEM_MAP]
  >- (
    Cases_on `b.h` >> fs [header_to_emap_upd_def, MEM_MAP] >> rw [] >>
    CASE_TAC >> rw [] >>
    metis_tac [phi_assigns_def, FST])
  >- (
    rw [header_to_emap_upd_def, MEM_MAP, PULL_EXISTS] >>
    qexists_tac `p` >> rw [] >>
    CASE_TAC >> rw [phi_assigns_def])
QED

Theorem header_to_emap_upd_fst2:
  !b. header_assigns b.h = set (map fst (header_to_emap_upd b.h))
Proof
  Cases_on `b.h` >> rw [header_assigns_def, header_to_emap_upd_def] >>
  rw [EXTENSION, MEM_MAP, PULL_EXISTS] >>
  eq_tac >> rw []
  >- (Cases_on `y` >> rw [phi_assigns_def] >> qexists_tac `Phi r t l'` >> rw [])
  >- (CASE_TAC >> rw [] >> metis_tac [phi_assigns_def, FST])
QED

Definition block_assigns_def:
  block_assigns b =
    { fst (phi_assigns p) | land,p,phis | b.h = Head phis land ∧ mem p phis } ∪
    bigunion (image (λi. image fst (instr_assigns i)) (set b.body))
End

Theorem translate_block_emap_restr_live:
  ∀emap1 emap2 b f gmap r x l.
    similar_emap (linear_live [b]) emap1 emap2 ∧
    (l = None ⇔ b.h = Entry) ∧
    (∀i. i < length b.body ∧ classify_instr (el i b.body) = Term ⇒ Suc i = length b.body) ∧
    every is_implemented b.body
    ⇒
    (λ(b1, emap1') (b2, emap2').
        b1 = b2 ∧
        ∃l.
          set (map fst l) = block_assigns b ∧
          emap1' = emap1 |++ l ∧ emap2' = emap2 |++ l ∧
          similar_emap (linear_live [b] ∪ block_assigns b) emap1' emap2')
      (translate_block f gmap emap1 r x (l,b))
      (translate_block f gmap emap2 r x (l,b))
Proof
  rw [translate_block_def] >>
  rpt (pairarg_tac >> fs []) >>
  `similar_emap (header_uses b.h) emap1 emap2`
  by (
    fs [similar_emap_def, linear_live_def, fmap_eq_flookup, FLOOKUP_DRESTRICT, PULL_EXISTS] >>
    rw [] >> last_x_assum (qspec_then `x'` mp_tac) >> rw [] >>
    pairarg_tac >> fs []) >>
  drule translate_header_similar_emap >>
  disch_then drule >>
  disch_then (qspecl_then [`f`, `THE (alookup x l)`, `gmap`] mp_tac) >>
  strip_tac >>
  `similar_emap (fst (instrs_live b.body))
    (emap1 |++ header_to_emap_upd b.h)
    (emap2 |++ header_to_emap_upd b.h)`
  by (
    fs [linear_live_def] >>
    pairarg_tac >> fs [similar_emap_union] >>
    irule similar_emap_diff >>
    metis_tac [header_to_emap_upd_fst2]) >>
  drule translate_instrs_similar_emap >>
  disch_then (qspecl_then [`f`, `l`, `gmap`, `r`, `0`] mp_tac) >>
  rpt (pairarg_tac >> simp []) >>
  rw [] >> fs [] >> rw [] >>
  simp [GSYM FUPDATE_LIST_APPEND] >>
  qexists_tac `header_to_emap_upd b.h ++ l'` >> simp [] >>
  conj_asm1_tac
  >- (
    rw [block_assigns_def] >>
    rw [GSYM header_to_emap_upd_fst] >>
    rw [EXTENSION, PULL_EXISTS, MEM_MAP] >>
    metis_tac []) >>
  irule extend_similar_emap >> rw [block_assigns_def]
QED

Theorem translate_blocks_emap_restr_live:
  ∀emap1 emap2 blocks f gmap r x.
    similar_emap (linear_live (map snd blocks)) emap1 emap2 ∧
    every (\(l,b). l = None ⇔ b.h = Entry) blocks ∧
    every (\(l,b). (∀i. i < length b.body ∧ classify_instr (el i b.body) = Term ⇒ Suc i = length b.body))
      blocks ∧
    every (\(l,b). every is_implemented b.body) blocks
    ⇒
    translate_blocks f gmap emap1 r x blocks = translate_blocks f gmap emap2 r x blocks
Proof
  Induct_on `blocks` >> rw [translate_blocks_def] >>
  pairarg_tac >> rw [] >>
  pairarg_tac >> rw [] >>
  qpat_x_assum `translate_block _ _ _ _ _ _ = _` mp_tac >>
  rename1 `translate_block _ _ _ _ _ p = (b3, emap3)` >>
  rw [] >>
  rename1 `translate_block _ _ _ _ _ p = (b4, emap4)` >>
  rw [APPEND_EQ_APPEND] >>
  disj1_tac >> qexists_tac `[]` >>
  simp [] >>
  `?l b. p = (l,b)` by metis_tac [pair_CASES] >> fs [] >>
  `similar_emap (linear_live [b]) emap1 emap2`
  by (
    fs [linear_live_def] >>
    pairarg_tac >> fs [similar_emap_union, union_diff]) >>
  drule translate_block_emap_restr_live >>
  disch_then drule >>
  simp [] >>
  disch_then (qspecl_then [`f`, `gmap`, `r`, `x`] mp_tac) >>
  pairarg_tac >> simp [] >>
  fs [] >> rw [] >>
  first_x_assum irule >>
  fs [linear_live_def] >>
  pairarg_tac >> fs [similar_emap_union, union_diff, block_assigns_def] >>
  fs [GSYM header_to_emap_upd_fst, GSYM header_to_emap_upd_fst2] >>
  irule similar_emap_diff >> rw [DIFF_UNION] >>
  qmatch_goalsub_abbrev_tac `similar_emap (_ DIFF _ DIFF a)` >>
  `kill ⊆ a` by metis_tac [instrs_kill_subset_assigns, SND] >>
  ONCE_REWRITE_TAC [similar_emap_sym] >>
  irule similar_emap_subset >>
  qexists_tac `linear_live (map snd blocks) DIFF kill DIFF header_assigns b.h` >>
  fs [SUBSET_DEF] >> metis_tac []
QED

(* ------ Theorems about lookups in the translated program ----- *)
(* This uses the dominor_ordered theorems and the similar_emap theorems to
 * switch the empty emap that the translator starts the translation with to the
 * good_emap that it ends with. The good_emap theory then lets us know that it
 * doesn't change throughout the compilation. *)

Theorem alookup_translate_prog:
  ∀prog f d.
    alookup prog (Fn f) = Some d
    ⇒
    alookup (translate_prog prog).functions f = Some (translate_def f d (get_gmap prog))
Proof
  rw [translate_prog_def] >>
  qspec_tac (`get_gmap prog:glob_var |-> ty`, `gmap`) >>
  Induct_on `prog` >>
  rw [] >>
  pairarg_tac >> fs [] >> rw [] >> Cases_on `fname` >> fs [dest_fn_def]
QED

Triviality dest_label_11:
  dest_label x = dest_label y ⇔ x = y
Proof
  Cases_on `x` >> Cases_on `y` >> rw [dest_label_def]
QED

Theorem alookup_translate_instrs_mov:
  ∀l gmap emap r is bs emap' f from to.
    translate_instrs l gmap emap r is = (bs, emap') ∧
    (∀f from to. l ≠ Mov_name f from to)
    ⇒
    alookup bs (Mov_name f from to) = None
Proof
  Induct_on `is` >> rw [translate_instrs_def] >> rw [] >>
  BasicProvers.EVERY_CASE_TAC >> fs [] >>
  TRY pairarg_tac >> fs [] >> rw []
  >- (
    rename1 `add_to_first_block _ bs1` >>
    `bs1 = [] ∨ ∃x y bs2. bs1 = (x,y)::bs2` by metis_tac [list_CASES, pair_CASES] >>
    fs [add_to_first_block_def] >> rw [] >>
    first_x_assum drule >> Cases_on `l` >> fs [inc_label_def] >>
    rw [] >> metis_tac [NOT_SOME_NONE])
  >- (first_x_assum drule >> Cases_on `l` >> fs [inc_label_def])
  >- (
    rename1 `add_to_first_block _ bs1` >>
    `bs1 = [] ∨ ∃x y bs2. bs1 = (x,y)::bs2` by metis_tac [list_CASES, pair_CASES] >>
    fs [add_to_first_block_def] >> rw [] >>
    first_x_assum drule >> Cases_on `l` >> fs [inc_label_def] >>
    rw [] >> metis_tac [NOT_SOME_NONE])
  >- (first_x_assum drule >> Cases_on `l` >> fs [inc_label_def])
QED

Theorem alookup_translate_header_mov:
  ∀gmap r f emap to_l from_l x to_l' from_ls h.
    (to_l' = None ⇒ h = Entry) ∧
    alookup (translate_header f from_ls to_l' gmap emap h)
       (Mov_name f (option_map dest_label from_l) to_l) = Some x
    ⇒
    to_l' = Some (Lab to_l)
Proof
  rw [] >>
  Cases_on `to_l'` >> Cases_on `h` >> fs [translate_header_def] >>
  drule ALOOKUP_MEM >> simp [MEM_MAP] >>
  rw [] >>
  Cases_on `x'` >> fs [dest_label_def]
QED

Theorem lab_dest_lab[simp]:
  Lab (dest_label l) = l
Proof
  Cases_on `l` >> rw [dest_label_def]
QED

Theorem alookup_translate_header:
  ∀f to_l gmap emap phis l from_l edges from_ls.
    (mem (Some (Lab to_l), from_ls) edges ∧ mem from_l from_ls)
    ⇒
    alookup (translate_header f from_ls (Some (Lab to_l)) gmap emap (Head phis l)) (Mov_name f (option_map dest_label from_l) to_l) ≠ None
Proof
  rw [translate_header_def, ALOOKUP_NONE, MAP_MAP_o, combinTheory.o_DEF,
      MEM_MAP, PULL_EXISTS, dest_label_def, MEM_FLAT] >>
  Cases_on `from_l` >> fs [] >> rw [PULL_EXISTS] >>
  metis_tac []
QED

Theorem mem_get_from_ls:
  ∀to_l blocks from_l.
    mem from_l (get_from_ls to_l blocks) ⇔
    ∃b. mem (from_l, b) blocks ∧ mem to_l (map Some (instr_to_labs (last b.body)))
Proof
  ho_match_mp_tac get_from_ls_ind >> rw [get_from_ls_def] >> metis_tac []
QED

Theorem alookup_translate_blocks_mov:
  ∀blocks to_l f gmap regs_to_keep edges from_l phis block emap prog.
    good_emap (Fn f) prog regs_to_keep gmap emap ∧
    (∀l b. mem (l, b) blocks ⇒ ∃d. alookup prog (Fn f) = Some d ∧ alookup d.blocks l = Some b) ∧
    mem (Some (Lab to_l)) (map fst blocks) ∧
    (∃from_ls. alookup edges (Some (Lab to_l)) = Some from_ls ∧ mem from_l from_ls) ∧
    every (\b. (snd b).h = Entry ⇔ fst b = None) blocks ∧
    alookup blocks (Some (Lab to_l)) = Some block ∧ (∃l. block.h = Head phis l)
    ⇒
    alookup
      (translate_blocks f gmap emap regs_to_keep edges blocks)
      (Mov_name f (option_map dest_label from_l) to_l) =
    Some (generate_move_block f gmap emap phis from_l (Lab to_l))
Proof
  Induct_on `blocks` >> rw [translate_blocks_def] >>
  rename1 `alookup (bloc::blocks) (Some _) = Some _` >>
  `∃l bl. bloc = (l,bl)` by metis_tac [pair_CASES] >>
  fs [] >> rw [translate_block_def]
  >- (
    pairarg_tac >> fs [] >>
    pairarg_tac >> fs [] >> rw [] >>
    rw [ALOOKUP_APPEND] >>
    BasicProvers.EVERY_CASE_TAC >> fs []
    >- metis_tac [alookup_translate_header, ALOOKUP_MEM]
    >- metis_tac [label_distinct, alookup_translate_instrs_mov, NOT_NONE_SOME] >>
    fs [translate_header_def, alookup_some, MAP_EQ_APPEND] >> rw [] >>
    Cases_on `from_l` >> Cases_on `l_from` >> fs [] >>
    metis_tac [dest_label_11]) >>
  BasicProvers.EVERY_CASE_TAC >> fs [] >> rw []
  >- (
    pairarg_tac >> fs [] >>
    pairarg_tac >> fs [] >> rw [] >>
    rw [ALOOKUP_APPEND] >>
    BasicProvers.EVERY_CASE_TAC >> fs []
    >- metis_tac [alookup_translate_header, ALOOKUP_MEM]
    >- metis_tac [label_distinct, alookup_translate_instrs_mov, NOT_NONE_SOME] >>
    fs [translate_header_def, alookup_some, MAP_EQ_APPEND] >> rw [] >>
    Cases_on `from_l` >> Cases_on `l_from` >> fs [] >>
    metis_tac [dest_label_11]) >>
  first_x_assum drule >> simp [PULL_EXISTS] >>
  rpt (disch_then drule) >>
  pairarg_tac >> fs [] >>
  fs [ALOOKUP_APPEND] >>
  BasicProvers.EVERY_CASE_TAC >> fs [] >>
  pairarg_tac >> fs [] >> rw [] >>
  fs [ALOOKUP_APPEND] >>
  BasicProvers.EVERY_CASE_TAC >> fs []
  >- (
    `emap |++ header_to_emap_upd bl.h = emap`
    by (
      Cases_on `bl.h`
      >- rw [header_to_emap_upd_def, FUPDATE_LIST_THM] >>
      first_assum (qspecl_then [`l'`, `bl`] assume_tac) >> fs [] >>
      rename1 `bl.h = Head phis1 land` >>
      `get_instr prog <| f := Fn f; b := l'; i := Phi_ip from_l |>
         (Inr (from_l, phis1))`
      by rw [get_instr_cases] >>
      metis_tac [good_emap_header_unchanged]) >>
    `emap' = emap` by metis_tac [good_emap_translate_unchanged] >>
    metis_tac [label_distinct, alookup_translate_instrs_mov, NOT_NONE_SOME]) >>
  rw [] >>
  metis_tac [label_distinct, alookup_translate_instrs_mov, NOT_NONE_SOME, alookup_translate_header_mov]
QED

Theorem get_block_translate_prog_mov:
  ∀prog f from_l to_l d b block phis d emap.
    prog_ok prog ∧
    dominator_ordered prog ∧
    good_emap (Fn f) prog (get_regs_to_keep d) (get_gmap prog) emap ∧
    alookup prog (Fn f) = Some d ∧
    alookup d.blocks from_l = Some b ∧
    mem (Lab to_l) (instr_to_labs (last b.body)) ∧
    alookup d.blocks (Some (Lab to_l)) = Some block ∧ (∃l. block.h = Head phis l) ∧
    every (\(l,b). every is_implemented b.body) d.blocks
    ⇒
    get_block (translate_prog prog)
      (Mov_name f (option_map dest_label from_l) to_l)
      (generate_move_block f (get_gmap prog) emap phis from_l (Lab to_l))
Proof
  rw [get_block_cases, label_to_fname_def] >>
  drule alookup_translate_prog >>
  rw [] >> rw [translate_def_def] >>
  qmatch_goalsub_abbrev_tac `translate_blocks a1 b1 _ c1 d1 e1` >>
  `translate_blocks a1 b1 fempty c1 d1 e1 = translate_blocks a1 b1 emap c1 d1 e1`
  by (
    irule translate_blocks_emap_restr_live >>
    unabbrev_all_tac >> rw []
    >- metis_tac [prog_ok_terminator_last]
    >- (
      fs [prog_ok_def] >> fs [EVERY_MEM] >> rw [] >>
      pairarg_tac >> rw [] >> metis_tac [FST, SND])
    >- metis_tac [dominator_ordered_linear_live, similar_emap_def, DRESTRICT_IS_FEMPTY]) >>
  unabbrev_all_tac >> rw [] >>
  irule alookup_translate_blocks_mov >> rw []
  >- (
    simp [ALOOKUP_MAP_2, MEM_MAP, EXISTS_PROD] >>
    drule ALOOKUP_MEM >> rw [mem_get_from_ls, MEM_MAP] >>
    imp_res_tac ALOOKUP_MEM >>
    metis_tac [])
  >- (fs [prog_ok_def] >> res_tac >> fs [EVERY_MEM])
  >- (
    imp_res_tac ALOOKUP_MEM >>
    fs [MEM_MAP] >>
    metis_tac [FST])
  >- metis_tac [ALOOKUP_ALL_DISTINCT_MEM, prog_ok_def]
QED

Theorem alookup_translate_header_lab:
  (l = None ⇒ h = Entry)
  ⇒
  alookup (translate_header f from_ls l gmap emap h) (translate_label f l' i) = None
Proof
  Cases_on `l` >> Cases_on `h` >> fs [translate_header_def] >>
  Cases_on `l'` >> fs [translate_label_def, ALOOKUP_NONE, MEM_MAP] >>
  rw [dest_label_def] >>
  CCONTR_TAC >> fs [] >> rw [] >> fs [] >>
  Cases_on `x'` >> fs [translate_label_def]
QED

Theorem alookup_translate_instrs_lab:
  ∀f l' i j gmap emap regs_to_keep b bs emap' l x.
    translate_instrs (Lab_name f (option_map dest_label l') i) gmap emap regs_to_keep b = (bs,emap') ∧
    alookup bs (translate_label f l j) = Some x
    ⇒
    l = l'
Proof
  Induct_on `b` >> rw [translate_instrs_def] >> fs [] >>
  rename1 `classify_instr ins` >>
  Cases_on `classify_instr ins` >> fs []
  >- (
    pairarg_tac >> fs [] >>
    Cases_on `r ∈ regs_to_keep` >> fs []
    >- (
      first_x_assum drule >>
      Cases_on `bs'` >> fs [add_to_first_block_def] >> rw [] >> fs [] >>
      rename1 `add_to_first_block _ (bl::_)` >>
      Cases_on `bl` >> fs [add_to_first_block_def] >>
      rename1 `lab = translate_label _ _ _` >>
      Cases_on `lab = translate_label f l j` >> fs [] >> metis_tac []) >>
    metis_tac [])
  >- (
    pairarg_tac >> fs [] >>
    first_x_assum drule >>
    Cases_on `bs'` >> fs [add_to_first_block_def] >> rw [] >> fs [] >>
    rename1 `add_to_first_block _ (bl::_)` >>
    Cases_on `bl` >> fs [add_to_first_block_def] >>
    rename1 `lab = translate_label _ _ _` >>
    Cases_on `lab = translate_label f l j` >> fs [] >> metis_tac [])
  >- (
    rw [] >> fs [ALOOKUP_def] >>
    Cases_on `l` >> Cases_on `l'` >> fs [translate_label_def] >>
    rename1 `translate_label _ (Some lname)` >>
    Cases_on `lname` >> fs [translate_label_def] >> rw [])
  >- (
    pairarg_tac >> fs [] >> rw [] >> fs [ALOOKUP_def] >>
    BasicProvers.EVERY_CASE_TAC >> fs [] >>
    rw []
    >- (
      Cases_on `l` >> fs [inc_label_def, translate_label_def] >>
      rename1 `translate_label _ (Some lb)` >>
      Cases_on `lb` >> fs [inc_label_def, translate_label_def]) >>
    fs [inc_label_def] >>
    metis_tac [])
QED

Triviality every_front:
  ∀P x y. y ≠ [] ∧ every P (front (x::y)) ⇒ every P (front y)
Proof
  Induct_on `y` >> rw []
QED

Theorem translate_instrs_first_lab:
  ∀dest_label l gmap emap regs_to_keep b bs emap l' b' emap'.
    translate_instrs l gmap emap regs_to_keep b = ((l',b')::bs,emap')
    ⇒
    l = l'
Proof
  Induct_on `b` >> rw [translate_instrs_def] >>
  BasicProvers.EVERY_CASE_TAC >> fs [] >>
  TRY pairarg_tac >> fs [] >> rw [] >>
  TRY (Cases_on `bs'`) >> fs [add_to_first_block_def] >>
  TRY (Cases_on `h'`) >> fs [add_to_first_block_def] >>
  metis_tac []
QED

Triviality lab_translate_label:
  ∀f l j f' l' j'.
    Lab_name f (option_map dest_label l) j = translate_label f' l' j'
    ⇔
    f = f' ∧ l = l' ∧ j = j'
Proof
  rw [] >> Cases_on `l` >> Cases_on `l'` >> fs [translate_label_def] >>
  Cases_on `x` >> fs [translate_label_def, dest_label_def] >>
  Cases_on `x'` >> fs [translate_label_def, dest_label_def]
QED

Definition num_calls_def:
  num_calls is = length (filter is_call is)
End

Theorem alookup_translate_instrs:
  ∀f l i j gmap emap regs_to_keep b bs emap' l x.
    b ≠ [] ∧
    terminator (last b) ∧
    every (λi. ¬terminator i) (front b) ∧
    i ≤ num_calls b ∧
    translate_instrs (Lab_name f (option_map dest_label l) j) gmap emap regs_to_keep b = (bs,emap')
    ⇒
    alookup bs (translate_label f l (i + j))
    =
    Some (snd (el i (fst (translate_instrs (Lab_name f (option_map dest_label l) j)
                             gmap emap regs_to_keep b))))
Proof
  Induct_on `b` >> rw [translate_instrs_def, num_calls_def] >>
  rename1 `classify_instr instr`
  >- (
    Cases_on `instr` >> fs [is_call_def, classify_instr_def] >>
    pairarg_tac >> fs [] >> rw [] >>
    fs [lab_translate_label]
    >- (
      `i = 0` by fs [] >>
      rw [] >> fs [] >>
      qexists_tac `emap` >> rw []) >>
    `b ≠ []` by (Cases_on `b` >> fs [terminator_def]) >>
    fs [LAST_DEF, inc_label_def] >>
    `0 < i` by fs [] >>
    `i - 1 ≤ num_calls b` by fs [num_calls_def] >>
    drule every_front >> disch_then drule >> rw [] >>
    first_x_assum drule >> disch_then drule >> disch_then drule >> rw [] >>
    rw [] >>
    rw [EL_CONS, PRE_SUB1]) >>
  Cases_on `classify_instr instr` >> fs [LAST_DEF]
  >- (
  `b ≠ []`
    by (
      Cases_on `b = []` >> Cases_on `instr` >> fs [is_call_def, classify_instr_def] >>
      rw [] >> fs [terminator_def]) >>
    fs [num_calls_def] >>
    pairarg_tac >> fs [] >>
    drule every_front >> disch_then drule >> rw [] >> fs [] >>
    Cases_on `r  ∉ regs_to_keep` >> fs []
    >- metis_tac [] >>
    Cases_on `bs'` >> fs [add_to_first_block_def] >>
    first_x_assum drule >> disch_then drule >> rw [] >> fs [] >>
    rename1 `add_to_first_block _ (i1::is)` >>
    Cases_on `i1` >> fs [add_to_first_block_def] >>
    rw [] >> fs [] >>
    drule translate_instrs_first_lab >> rw [] >>
    fs [lab_translate_label]
    >- (`i = 0` by fs [] >> rw []) >>
    `0 < i` by fs [] >>
    rw [EL_CONS])
  >- (
    `b ≠ []`
    by (
      Cases_on `b = []` >> Cases_on `instr` >> fs [is_call_def, classify_instr_def] >>
      rw [] >> fs [terminator_def]) >>
    fs [num_calls_def] >>
    pairarg_tac >> fs [] >>
    rw [] >>
    drule every_front >> disch_then drule >> rw [] >> fs [] >>
    Cases_on `bs'` >> fs [add_to_first_block_def] >>
    first_x_assum drule >> disch_then drule >> rw [] >> fs [] >>
    rename1 `add_to_first_block _ (i1::is)` >>
    Cases_on `i1` >> fs [add_to_first_block_def] >>
    rw [] >> fs [] >>
    drule translate_instrs_first_lab >> rw [] >>
    fs [lab_translate_label]
    >- (`i = 0` by fs [] >> rw []) >>
    `0 < i` by fs [] >>
    rw [EL_CONS])
  >- (
    `b = []`
    by (
      Cases_on `b` >> fs [] >>
      Cases_on `instr` >> fs [terminator_def, classify_instr_def] >>
      Cases_on `p` >> fs [classify_instr_def]) >>
    fs [] >> rw [] >>
    metis_tac [lab_translate_label])
  >- (
    `b = []`
    by (
      Cases_on `b` >> fs [] >>
      Cases_on `instr` >> fs [is_call_def, terminator_def, classify_instr_def] >>
      Cases_on `p` >> fs [classify_instr_def]) >>
    rw [] >> fs [] >>
    pairarg_tac >> fs [] >> rw [] >>
    Cases_on `l` >> fs [translate_label_def] >>
    Cases_on `x` >> fs [translate_label_def, dest_label_def])
QED

Theorem classify_instr_lem:
  (∀i. (terminator i ∨ is_call i) ⇔ classify_instr i = Term ∨ classify_instr i = Call)
Proof
  strip_tac >> Cases_on `i` >> rw [terminator_def, classify_instr_def, is_call_def] >>
  Cases_on `p` >> rw [classify_instr_def]
QED

Theorem translate_instrs_not_empty:
  ∀l gmap emap regs b.
    b ≠ [] ∧ terminator (last b) ⇒
    ∀emap2. translate_instrs l gmap emap regs b ≠ ([], emap2)
Proof
  Induct_on `b` >> rw [translate_instrs_def] >>
  CASE_TAC >> rw [] >> TRY pairarg_tac >> fs []
  >- (
    Cases_on `bs` >> fs [add_to_first_block_def] >>
    Cases_on `b` >> fs []
    >- metis_tac [classify_instr_lem, instr_class_distinct]
    >- metis_tac [] >>
    rename1 `add_to_first_block _ (b::bs)` >>
    Cases_on `b` >> fs [add_to_first_block_def]) >>
  Cases_on `b` >> fs []
  >- metis_tac [classify_instr_lem, instr_class_distinct]
  >- metis_tac [classify_instr_lem, instr_class_distinct] >>
  Cases_on `bs` >> fs [add_to_first_block_def]
  >- metis_tac [] >>
  rename1 `add_to_first_block _ (b::bs)` >>
  Cases_on `b` >> fs [add_to_first_block_def]
QED

Theorem alookup_translate_blocks:
  ∀blocks l f gmap emap regs_to_keep edges b i prog.
    b.body ≠ [] ∧
    terminator (last b.body) ∧
    every (λi. ¬terminator i) (front b.body) ∧
    every (\b. (snd b).h = Entry ⇔ fst b = None) blocks ∧
    alookup blocks l = Some b ∧
    i ≤ num_calls b.body ∧
    good_emap (Fn f) prog regs_to_keep gmap emap ∧
    (∀l b. mem (l, b) blocks ⇒ ∃d. alookup prog (Fn f) = Some d ∧ alookup d.blocks l = Some b)
    ⇒
    alookup (translate_blocks f gmap emap regs_to_keep edges blocks) (translate_label f l i)
    =
    Some (snd (el i (fst (translate_instrs (Lab_name f (option_map dest_label l) 0)
                            gmap emap regs_to_keep b.body))))
Proof
  ho_match_mp_tac ALOOKUP_ind >> simp [translate_blocks_def] >>
  rpt strip_tac >>
  pairarg_tac >> fs [ALOOKUP_APPEND] >>
  rename1 `(if l' = l then _ else _) = Some _` >>
  Cases_on `l = l'` >> fs [translate_block_def] >> rw []
  >- (
    pairarg_tac >> fs [] >> rw [] >> fs [ALOOKUP_APPEND] >>
    `l = None ⇒ b.h = Entry` by metis_tac [] >>
    rfs [alookup_translate_header_lab] >>
    imp_res_tac alookup_translate_instrs >>
    fs [] >> rw [] >> rfs [] >>
    `emap |++ header_to_emap_upd b.h = emap`
    by (
      drule good_emap_header_unchanged >>
      Cases_on `b.h` >> fs [header_to_emap_upd_def, FUPDATE_LIST_THM] >>
      disch_then irule >> rw [get_instr_cases] >>
      metis_tac []) >>
    fs [FUPDATE_LIST_THM])
  >- (
    pairarg_tac >> fs [ALOOKUP_APPEND] >> rw [] >>
    fs [ALOOKUP_APPEND] >>
    rename1 `header_to_emap_upd b1.h` >>
    `emap |++ header_to_emap_upd b1.h = emap`
    by (
      drule good_emap_header_unchanged >>
      Cases_on `b1.h` >> fs [header_to_emap_upd_def, FUPDATE_LIST_THM] >>
      disch_then irule >> rw [get_instr_cases] >>
      metis_tac []) >>
    rename1 `alookup (translate_header _ _ _ _ _ bloc.h)` >>
    `l' = None ⇒ bloc.h = Entry` by metis_tac [] >>
    fs [alookup_translate_header_lab] >>
    Cases_on `alookup bs (translate_label f l i)` >> fs [] >> rw []
    >- (
      `emap = emap'`
      by (
        drule good_emap_translate_unchanged >>
        disch_then irule >>
        first_x_assum (qspecl_then [`l'`, `bloc`] mp_tac) >>
        rw [] >>
        metis_tac []) >>
      first_x_assum drule >>
      disch_then (qspecl_then [`f`, `gmap`, `emap`, `regs_to_keep`, `edges`, `prog`] mp_tac) >>
      rw [])
    >- metis_tac [alookup_translate_instrs_lab])
QED

export_theory ();
