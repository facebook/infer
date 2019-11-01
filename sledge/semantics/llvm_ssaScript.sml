(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Define SSA form and the concept of variable liveness, and then show how SSA
 * simplifies it *)

open HolKernel boolLib bossLib Parse;
open pred_setTheory listTheory rich_listTheory;
open settingsTheory miscTheory llvmTheory llvm_propTheory;

new_theory "llvm_ssa";

numLib.prefer_num ();

(* ----- Static paths through a program *)

Definition inc_pc_def:
  inc_pc ip = ip with i := inc_bip ip.i
End

(* The set of program counters the given instruction and starting point can
 * immediately reach, within a function *)
Definition instr_next_ips_def:
  (instr_next_ips (Ret _) ip = {}) ∧
  (instr_next_ips (Br _ l1 l2) ip =
    { <| f := ip.f; b := Some l; i := Phi_ip ip.b |> | l | l ∈ {l1; l2} }) ∧
  (instr_next_ips (Invoke _ _ _ _ l1 l2) ip =
    { <| f := ip.f; b := Some l; i := Phi_ip ip.b |> | l | l ∈ {l1; l2} }) ∧
  (instr_next_ips Unreachable ip = {}) ∧
  (instr_next_ips (Exit _) ip = {}) ∧
  (instr_next_ips (Sub _ _ _ _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Extractvalue _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Insertvalue _ _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Alloca _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Load _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Store _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Gep _ _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Cast _ _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Icmp _ _ _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Call _ _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Cxa_allocate_exn _ _) ip = { inc_pc ip }) ∧
  (* TODO: revisit throw when dealing with exceptions *)
  (instr_next_ips (Cxa_throw _ _ _) ip = {  }) ∧
  (instr_next_ips (Cxa_begin_catch _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Cxa_end_catch) ip = { inc_pc ip }) ∧
  (instr_next_ips (Cxa_get_exception_ptr _ _) ip = { inc_pc ip })
End

Inductive next_ips:
 (∀prog ip i l i2.
    get_instr prog ip (Inl i) ∧
    l ∈ instr_next_ips i ip ∧
    get_instr prog l i2
    ⇒
    next_ips prog ip l) ∧
 (∀prog ip from_l phis i2.
    get_instr prog ip (Inr (from_l, phis)) ∧
    get_instr prog (inc_pc ip) i2
    ⇒
    next_ips prog ip (inc_pc ip))
End

(* The path is a list of program counters that represent a statically feasible
 * path through a function *)
Inductive good_path:
  (∀prog. good_path prog []) ∧

  (∀prog ip i.
    get_instr prog ip i
    ⇒
    good_path prog [ip]) ∧

  (∀prog path ip1 ip2.
    ip2 ∈ next_ips prog ip1 ∧
    good_path prog (ip2::path)
    ⇒
    good_path prog (ip1::ip2::path))
End

Theorem next_ips_same_func:
  ∀prog ip1 ip2. ip2 ∈ next_ips prog ip1 ⇒ ip1.f = ip2.f
Proof
  rw [next_ips_cases, IN_DEF, get_instr_cases, inc_pc_def, inc_bip_def] >> rw [] >>
  Cases_on `el idx b.body` >> fs [instr_next_ips_def, inc_pc_def, inc_bip_def]
QED

Theorem good_path_same_func:
  ∀prog path. good_path prog path ⇒ ∀ip1 ip2. mem ip1 path ∧ mem ip2 path ⇒ ip1.f = ip2.f
Proof
  ho_match_mp_tac good_path_ind >> rw [] >>
  metis_tac [next_ips_same_func]
QED

Theorem good_path_prefix:
  ∀prog path path'. good_path prog path ∧ path' ≼ path ⇒ good_path prog path'
Proof
  Induct_on `path'` >> rw []
  >- simp [Once good_path_cases] >>
  pop_assum mp_tac >> CASE_TAC >> rw [] >>
  qpat_x_assum `good_path _ _` mp_tac >>
  simp [Once good_path_cases] >> rw [] >> fs []
  >- (simp [Once good_path_cases] >> metis_tac []) >>
  first_x_assum drule >> rw [] >>
  simp [Once good_path_cases] >>
  Cases_on `path'` >> fs [next_ips_cases, IN_DEF] >>
  metis_tac []
QED

Theorem good_path_append:
  !prog p1 p2.
    good_path prog (p1++p2) ⇔
    good_path prog p1 ∧ good_path prog p2 ∧
    (p1 ≠ [] ∧ p2 ≠ [] ⇒ HD p2 ∈ next_ips prog (last p1))
Proof
  Induct_on `p1` >> rw []
  >- metis_tac [good_path_rules] >>
  Cases_on `p1` >> Cases_on `p2` >> rw []
  >- metis_tac [good_path_rules]
  >- (
    simp [Once good_path_cases] >>
    metis_tac [good_path_rules, next_ips_cases, IN_DEF])
  >- metis_tac [good_path_rules] >>
  rename1 `ip1::ip2::(ips1++ip3::ips2)` >>
  first_x_assum (qspecl_then [`prog`, `[ip3]++ips2`] mp_tac) >>
  rw [] >> simp [Once good_path_cases, LAST_DEF] >> rw [] >>
  eq_tac >> rw []
  >- metis_tac [good_path_rules]
  >- (qpat_x_assum `good_path _ [_;_]` mp_tac >> simp [Once good_path_cases])
  >- metis_tac [good_path_rules, next_ips_cases, IN_DEF]
  >- metis_tac [good_path_rules]
  >- (qpat_x_assum `good_path _ (ip1::ip2::ips1)` mp_tac >> simp [Once good_path_cases])
  >- (qpat_x_assum `good_path _ (ip1::ip2::ips1)` mp_tac >> simp [Once good_path_cases])
QED

(* ----- Helper functions to get registers out of instructions ----- *)

Definition arg_to_regs_def:
  (arg_to_regs (Constant _) = {}) ∧
  (arg_to_regs (Variable r) = {r})
End

(* The registers that an instruction uses *)
Definition instr_uses_def:
  (instr_uses (Ret (_, a)) = arg_to_regs a) ∧
  (instr_uses (Br a _ _) = arg_to_regs a) ∧
  (instr_uses (Invoke _ _ a targs _ _) =
    arg_to_regs a ∪ BIGUNION (set (map (arg_to_regs o snd) targs))) ∧
  (instr_uses Unreachable = {}) ∧
  (instr_uses (Exit a) = arg_to_regs a) ∧
  (instr_uses (Sub _ _ _ _ a1 a2) =
    arg_to_regs a1 ∪ arg_to_regs a2) ∧
  (instr_uses (Extractvalue _ (_, a) _) = arg_to_regs a) ∧
  (instr_uses (Insertvalue _ (_, a1) (_, a2) _) =
    arg_to_regs a1 ∪ arg_to_regs a2) ∧
  (instr_uses (Alloca _ _ (_, a)) = arg_to_regs a) ∧
  (instr_uses (Load _ _ (_, a)) = arg_to_regs a) ∧
  (instr_uses (Store (_, a1) (_, a2)) =
    arg_to_regs a1 ∪ arg_to_regs a2) ∧
  (instr_uses (Gep _ _ (_, a) targs) =
    arg_to_regs a ∪ BIGUNION (set (map (arg_to_regs o snd) targs))) ∧
  (instr_uses (Cast _ _ (_, a) _) = arg_to_regs a) ∧
  (instr_uses (Icmp _ _ _ a1 a2) =
    arg_to_regs a1 ∪ arg_to_regs a2) ∧
  (instr_uses (Call _ _ _ targs) =
    BIGUNION (set (map (arg_to_regs o snd) targs))) ∧
  (instr_uses (Cxa_allocate_exn _ a) = arg_to_regs a) ∧
  (instr_uses (Cxa_throw a1 a2 a3) =
    arg_to_regs a1 ∪ arg_to_regs a2 ∪ arg_to_regs a3) ∧
  (instr_uses (Cxa_begin_catch _ a) = arg_to_regs a) ∧
  (instr_uses (Cxa_end_catch) = {  }) ∧
  (instr_uses (Cxa_get_exception_ptr _ a) = arg_to_regs a)
End

Definition phi_uses_def:
  phi_uses from_l (Phi _ _ entries) =
    case alookup entries from_l of
    | None => {}
    | Some a => arg_to_regs a
End

Inductive uses:
 (∀prog ip i r.
    get_instr prog ip (Inl i) ∧
    r ∈ instr_uses i
    ⇒
    uses prog ip r) ∧
 (∀prog ip from_l phis r.
   get_instr prog ip (Inr (from_l, phis)) ∧
    r ∈ BIGUNION (set (map (phi_uses from_l) phis))
    ⇒
    uses prog ip r)
End

(* The registers that an instruction assigns *)
Definition instr_assigns_def:
  (instr_assigns (Invoke r _ _ _ _ _) = {r}) ∧
  (instr_assigns (Sub r _ _ _ _ _) = {r}) ∧
  (instr_assigns (Extractvalue r _ _) = {r}) ∧
  (instr_assigns (Insertvalue r _ _ _) = {r}) ∧
  (instr_assigns (Alloca r _ _) = {r}) ∧
  (instr_assigns (Load r _ _) = {r}) ∧
  (instr_assigns (Gep r _ _ _) = {r}) ∧
  (instr_assigns (Cast r _ _ _) = {r}) ∧
  (instr_assigns (Icmp r _ _ _ _) = {r}) ∧
  (instr_assigns (Call r _ _ _) = {r}) ∧
  (instr_assigns (Cxa_allocate_exn r _) = {r}) ∧
  (instr_assigns (Cxa_begin_catch r _) = {r}) ∧
  (instr_assigns (Cxa_get_exception_ptr r _) = {r}) ∧
  (instr_assigns _ = {})
End

Definition phi_assigns_def:
  phi_assigns (Phi r _ _) = r
End

Inductive assigns:
 (∀prog ip i r.
    get_instr prog ip (Inl i) ∧
    r ∈ instr_assigns i
    ⇒
    assigns prog ip r) ∧
 (∀prog ip from_l phis r.
    get_instr prog ip (Inr (from_l, phis)) ∧
    r ∈ set (map phi_assigns phis)
    ⇒
    assigns prog ip r)
End

(* ----- SSA form ----- *)

Definition entry_ip_def:
  entry_ip fname = <| f := fname; b := None; i := Offset 0 |>
End

Definition reachable_def:
  reachable prog ip ⇔
    ∃path. good_path prog (entry_ip ip.f :: path) ∧ last (entry_ip ip.f :: path) = ip
End

(* To get to ip2 from the entry, you must go through ip1 *)
Definition dominates_def:
  dominates prog ip1 ip2 ⇔
    ∀path.
      good_path prog (entry_ip ip2.f :: path) ∧
      last (entry_ip ip2.f :: path) = ip2 ⇒
      mem ip1 (front (entry_ip ip2.f :: path))
End

Definition is_ssa_def:
  is_ssa prog ⇔
    (* Operate function by function *)
    (∀fname.
      (* No register is assigned in two different instructions *)
      (∀r ip1 ip2.
        r ∈ assigns prog ip1 ∧ r ∈ assigns prog ip2 ∧ ip1.f = fname ∧ ip2.f = fname
        ⇒
        ip1 = ip2)) ∧
    (* Each use is dominated by its assignment *)
    (∀ip1 r. r ∈ uses prog ip1 ⇒ ∃ip2. ip2.f = ip1.f ∧ r ∈ assigns prog ip2 ∧ dominates prog ip2 ip1)
End

Theorem dominates_trans:
  ∀prog ip1 ip2 ip3.
    dominates prog ip1 ip2 ∧ dominates prog ip2 ip3 ⇒ dominates prog ip1 ip3
Proof
  rw [dominates_def] >> simp [FRONT_DEF] >> rw []
  >- (first_x_assum (qspec_then `[]` mp_tac) >> rw []) >>
  first_x_assum drule >> rw [] >>
  qpat_x_assum `mem _ (front _)` mp_tac >>
  simp [Once MEM_EL] >> rw [] >> fs [EL_FRONT] >>
  first_x_assum (qspec_then `take n path` mp_tac) >> simp [LAST_DEF] >>
  rw [] >> fs [entry_ip_def]
  >- (fs [Once good_path_cases] >> rw [] >> fs [next_ips_cases, IN_DEF]) >>
  rfs [EL_CONS] >>
  `?m. n = Suc m` by (Cases_on `n` >> rw []) >>
  rw [] >> rfs [] >>
  `(el m path).f = ip3.f`
  by (
    irule good_path_same_func >>
    qexists_tac `<| f:= ip3.f; b := NONE; i := Offset 0|> :: path` >>
    qexists_tac `prog` >>
    conj_tac >- rw [EL_MEM] >>
    metis_tac [MEM_LAST]) >>
  fs [] >> qpat_x_assum `_ ⇒ _` mp_tac >> impl_tac
  >- (
    irule good_path_prefix >> HINT_EXISTS_TAC >> rw [] >>
    metis_tac [take_is_prefix]) >>
  rw [] >> drule MEM_FRONT >> rw [] >>
  fs [MEM_EL, LENGTH_FRONT] >> rfs [EL_TAKE] >> rw [] >>
  disj2_tac >> qexists_tac `n'` >> rw [] >>
  irule (GSYM EL_FRONT) >>
  rw [NULL_EQ, LENGTH_FRONT]
QED

Theorem dominates_unreachable:
  ∀prog ip1 ip2. ¬reachable prog ip2 ⇒ dominates prog ip1 ip2
Proof
  rw [dominates_def, reachable_def] >>
  metis_tac []
QED

Theorem dominates_antisym_lem:
  ∀prog ip1 ip2. dominates prog ip1 ip2 ∧ dominates prog ip2 ip1 ⇒ ¬reachable prog ip1
Proof
  rw [dominates_def, reachable_def] >> CCONTR_TAC >> fs [] >>
  Cases_on `ip1 = entry_ip ip1.f` >> fs []
  >- (
    first_x_assum (qspec_then `[]` mp_tac) >> rw [] >>
    fs [Once good_path_cases, IN_DEF, next_ips_cases] >>
    metis_tac []) >>
  `path ≠ []` by (Cases_on `path` >> fs []) >>
  `(OLEAST n. n < length path ∧ el n path = ip1) ≠ None`
  by (
    rw [whileTheory.OLEAST_EQ_NONE] >>
    qexists_tac `PRE (length path)` >> rw [] >>
    fs [LAST_DEF, LAST_EL] >>
    Cases_on `path` >> fs []) >>
  qabbrev_tac `path1 = splitAtPki (\n ip. ip = ip1) (\x y. x) path` >>
  first_x_assum (qspec_then `path1 ++ [ip1]` mp_tac) >>
  simp [] >>
  conj_asm1_tac >> rw []
  >- (
    irule good_path_prefix >>
    HINT_EXISTS_TAC >> rw [] >>
    unabbrev_all_tac >> rw [splitAtPki_EQN] >>
    CASE_TAC >> rw [] >>
    fs [whileTheory.OLEAST_EQ_SOME] >>
    rw [GSYM SNOC_APPEND, SNOC_EL_TAKE] >>
    metis_tac [take_is_prefix])
  >- rw [LAST_DEF] >>
  simp [GSYM SNOC_APPEND, FRONT_SNOC, FRONT_DEF] >>
  CCONTR_TAC >> fs [MEM_EL]
  >- (
    first_x_assum (qspec_then `[]` mp_tac) >>
    fs [entry_ip_def, Once good_path_cases, IN_DEF, next_ips_cases] >>
    metis_tac []) >>
  rw [] >>
  rename [`n1 < length _`, `last _ = el n path`] >>
  first_x_assum (qspec_then `take (Suc n1) path1` mp_tac) >> rw []
  >- (
    irule good_path_prefix >> HINT_EXISTS_TAC >> rw [entry_ip_def]
    >- (
      irule good_path_same_func >>
      qexists_tac `entry_ip (el n path).f::(path1 ++ [el n path])` >>
      qexists_tac `prog` >> rw [EL_MEM]) >>
    metis_tac [IS_PREFIX_APPEND3, take_is_prefix, IS_PREFIX_TRANS])
  >- (rw [LAST_DEF] >> fs []) >>
  rw [METIS_PROVE [] ``~x ∨ y ⇔ (x ⇒ y)``] >>
  simp [EL_FRONT] >>
  rename [`n2 < Suc _`] >>
  Cases_on `¬(0 < n2)` >> rw [EL_CONS]
  >- (
    fs [entry_ip_def] >>
    `(el n path).f = (el n1 path1).f` suffices_by metis_tac [] >>
    irule good_path_same_func >>
    qexists_tac `<|f := (el n path).f; b := None; i := Offset 0|> ::(path1 ++ [el n path])` >>
    qexists_tac `prog` >>
    rw [EL_MEM]) >>
  fs [EL_TAKE, Abbr `path1`, splitAtPki_EQN] >>
  CASE_TAC >> rw [] >> fs []
  >- metis_tac [] >>
  fs [whileTheory.OLEAST_EQ_SOME] >>
  rfs [LENGTH_TAKE] >>
  `PRE n2 < x` by decide_tac >>
  first_x_assum drule >>
  rw [EL_TAKE]
QED

Theorem dominates_antisym:
  ∀prog ip1 ip2. reachable prog ip1 ∧ dominates prog ip1 ip2 ⇒ ¬dominates prog ip2 ip1
Proof
  metis_tac [dominates_antisym_lem]
QED

Theorem dominates_irrefl:
  ∀prog ip. reachable prog ip ⇒ ¬dominates prog ip ip
Proof
  metis_tac [dominates_antisym]
QED

(* ----- Liveness ----- *)

Definition live_def:
  live prog ip =
    { r | ∃path.
            good_path prog (ip::path) ∧
            r ∈ uses prog (last (ip::path)) ∧
            ∀ip2. ip2 ∈ set (front (ip::path)) ⇒ r ∉ assigns prog ip2 }
End

Theorem get_instr_live:
  ∀prog ip instr.
    get_instr prog ip instr
    ⇒
    uses prog ip ⊆ live prog ip
Proof
  rw [live_def, SUBSET_DEF] >>
  qexists_tac `[]` >> rw [Once good_path_cases] >>
  qexists_tac `instr` >> simp [] >> metis_tac [IN_DEF]
QED

Triviality set_rw:
  ∀s P. (∀x. x ∈ s ⇔ P x) ⇔ s = P
Proof
  rw [] >> eq_tac >> rw [IN_DEF] >> metis_tac []
QED

Theorem live_gen_kill:
  ∀prog ip ip'.
    live prog ip =
    BIGUNION {live prog ip' | ip' | ip' ∈ next_ips prog ip} DIFF assigns prog ip ∪ uses prog ip
Proof
  rw [live_def, EXTENSION] >> eq_tac >> rw []
  >- (
    Cases_on `path` >> fs [] >>
    rename1 `ip::ip2::path` >>
    qpat_x_assum `good_path _ _` mp_tac >> simp [Once good_path_cases] >> rw [] >>
    Cases_on `x ∈ uses prog ip` >> fs [] >> simp [set_rw, PULL_EXISTS] >>
    qexists_tac `ip2` >> qexists_tac `path` >> rw [])
  >- (
    fs [] >>
    qexists_tac `ip'::path` >> rw [] >>
    simp [Once good_path_cases])
  >- (
    qexists_tac `[]` >> rw [] >>
    fs [Once good_path_cases, uses_cases, IN_DEF] >>
    metis_tac [])
QED

Theorem ssa_dominates_live_range_lem:
  ∀prog r ip1 ip2.
    is_ssa prog ∧ ip1.f = ip2.f ∧ r ∈ assigns prog ip1 ∧ r ∈ live prog ip2 ⇒
    dominates prog ip1 ip2
Proof
  rw [dominates_def, is_ssa_def, live_def] >>
  `path ≠ [] ⇒ (last path).f = ip2.f`
  by (
    rw [] >>
    irule good_path_same_func >>
    qexists_tac `ip2::path` >> rw [] >>
    Cases_on `path` >> fs [MEM_LAST] >> metis_tac []) >>
  first_x_assum drule >> rw [] >>
  first_x_assum (qspec_then `path'++path` mp_tac) >>
  impl_tac
  >- (
    fs [LAST_DEF] >> rw [] >> fs []
    >- (
      simp_tac std_ss [GSYM APPEND, good_path_append] >> rw []
      >- (
        qpat_x_assum `good_path _ (_::_)` mp_tac >>
        qpat_x_assum `good_path _ (_::_)` mp_tac >>
        simp [Once good_path_cases] >>
        metis_tac [])
      >- (
        simp [LAST_DEF] >>
        qpat_x_assum `good_path _ (_::_)` mp_tac >>
        qpat_x_assum `good_path _ (_::_)` mp_tac >>
        simp [Once good_path_cases] >>
        rw [] >> rw []))
    >- (Cases_on `path` >> fs [])) >>
  rw [] >>
  `ip2'.f = ip2.f`
  by (
    irule good_path_same_func >>
    qexists_tac `entry_ip ip2.f::path'` >>
    qexists_tac `prog` >>
    conj_tac
    >- (
      Cases_on `path` >>
      full_simp_tac std_ss [GSYM APPEND, FRONT_APPEND, APPEND_NIL, LAST_CONS]
      >- metis_tac [MEM_FRONT] >>
      fs [] >> metis_tac [])
    >- metis_tac [MEM_LAST]) >>
  `ip2' = ip1` by metis_tac [] >>
  rw [] >>
  Cases_on `path` >> fs [] >>
  full_simp_tac std_ss [GSYM APPEND, FRONT_APPEND] >> fs [] >> rw [FRONT_DEF] >> fs []
  >- metis_tac []
  >- (
    `mem ip1 path' = mem ip1 (front path' ++ [last path'])` by metis_tac [APPEND_FRONT_LAST] >>
    fs [LAST_DEF] >>
    metis_tac [])
  >- metis_tac []
  >- metis_tac []
QED

Theorem ssa_dominates_live_range:
  ∀prog r ip.
    is_ssa prog ∧ r ∈ uses prog ip
    ⇒
    ∃ip1. ip1.f = ip.f ∧ r ∈ assigns prog ip1 ∧
      ∀ip2. ip2.f = ip.f ∧ r ∈ live prog ip2 ⇒
        dominates prog ip1 ip2
Proof
  rw [] >> drule ssa_dominates_live_range_lem >> rw [] >>
  fs [is_ssa_def] >>
  first_assum drule >> rw [] >> metis_tac []
QED

Theorem reachable_dominates_same_func:
  ∀prog ip1 ip2. reachable prog ip2 ∧ dominates prog ip1 ip2 ⇒ ip1.f = ip2.f
Proof
  rw [reachable_def, dominates_def] >> res_tac >>
  irule good_path_same_func >>
  metis_tac [MEM_LAST, MEM_FRONT]
QED

Theorem next_ips_reachable:
  ∀prog ip1 ip2. reachable prog ip1 ∧ ip2 ∈ next_ips prog ip1 ⇒ reachable prog ip2
Proof
  rw [] >> imp_res_tac next_ips_same_func >>
  fs [reachable_def] >>
  qexists_tac `path ++ [ip2]` >>
  simp_tac std_ss [GSYM APPEND, LAST_APPEND_CONS, LAST_CONS] >>
  simp [good_path_append] >>
  simp [Once good_path_cases] >>
  fs [next_ips_cases, IN_DEF] >>
  metis_tac []
QED

export_theory ();
