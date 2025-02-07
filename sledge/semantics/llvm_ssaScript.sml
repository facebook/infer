(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Define SSA form and the concept of variable liveness, and then show how SSA
 * simplifies it *)

open HolKernel boolLib bossLib Parse;
open pred_setTheory listTheory rich_listTheory pairTheory arithmeticTheory;
open alistTheory set_relationTheory;
open settingsTheory miscTheory llvmTheory llvm_propTheory;

new_theory "llvm_ssa";

numLib.prefer_num ();

(* ----- The syntactic things we need to know about a program, just for this file ---- *)

Definition loc_prog_ok_def:
  loc_prog_ok p ⇔
    (∀fname dec bname block.
      alookup p fname = Some dec ∧
      alookup dec.blocks bname = Some block ⇒
      block.body ≠ [] ∧ terminator (last block.body) ∧
      every (λi. ~terminator i) (front block.body)) ∧
    (∀fname dec.
      alookup p fname = Some dec ⇒
      every (λb. fst b = None ⇔ (snd b).h = Entry) dec.blocks) ∧
    (every (\(fname, dec). all_distinct (map fst dec.blocks)) p)
End

(* ----- Static paths through a program ----- *)

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

Definition cidx_to_num_def:
  (cidx_to_num (IntC _ n) = Num (ABS n)) ∧
  (cidx_to_num _ = 0)

End
(* Convert index lists as for GEP into number lists, for the purpose of
 * calculating types. Everything goes to 0 but for positive integer constants,
 * because those things can't be used to index anything but arrays, and the type
 * for the array contents doesn't depend on the index's value. *)
Definition idx_to_num_def:
  (idx_to_num (_, (Constant (IntC _ n))) = Num (ABS n)) ∧
  (idx_to_num (_, _) = 0)
End

(* The registers that an instruction assigns *)
Definition instr_assigns_def:
  (instr_assigns (Invoke r t _ _ _ _) = {(r,t)}) ∧
  (instr_assigns (Sub r _ _ t _ _) = {(r,t)}) ∧
  (instr_assigns (Extractvalue r (t,_) idx) = {(r,THE (extract_type t (map cidx_to_num idx)))}) ∧
  (instr_assigns (Insertvalue r (t,_) _ _) = {(r, t)}) ∧
  (instr_assigns (Alloca r t _) = {(r,PtrT t)}) ∧
  (instr_assigns (Load r t _) = {(r,t)}) ∧
  (instr_assigns (Gep r t _ idx) = {(r,PtrT (THE (extract_type t (map idx_to_num idx))))}) ∧
  (instr_assigns (Cast r _ _ t) = {(r,t)}) ∧
  (instr_assigns (Icmp r _ _ _ _) = {(r, IntT W1)}) ∧
  (instr_assigns (Call r t _ _) = {(r,t)}) ∧
  (instr_assigns (Cxa_allocate_exn r _) = {(r,ARB)}) ∧
  (instr_assigns (Cxa_begin_catch r _) = {(r,ARB)}) ∧
  (instr_assigns (Cxa_get_exception_ptr r _) = {(r,ARB)}) ∧
  (instr_assigns _ = {})
End

Definition phi_assigns_def:
  phi_assigns (Phi r t _) = (r,t)
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

(* Equivalent instruction pointers, since we don't want to distinguish pointers
 * to headers that have different from labels *)
Definition ip_equiv_def:
  ip_equiv ip1 ip2 ⇔
    ip1.f = ip2.f ∧ ip1.b = ip2.b ∧
    (ip1.i ≠ ip2.i ⇒ ∃l1 l2. ip1.i = Phi_ip l1 ∧ ip2.i = Phi_ip l2)
End

Definition reachable_def:
  reachable prog ip ⇔
    ∃path. good_path prog (entry_ip ip.f :: path) ∧ ip_equiv (last (entry_ip ip.f :: path)) ip
End

(* To get to ip2 from the entry, you must go through ip1 *)
Definition dominates_def:
  dominates prog ip1 ip2 ⇔
    ∀path.
      good_path prog (entry_ip ip2.f :: path) ∧
      ip_equiv (last (entry_ip ip2.f :: path)) ip2 ⇒
      ∃ip1'. ip_equiv ip1 ip1' ∧ mem ip1' (front (entry_ip ip2.f :: path))
End

Definition is_ssa_def:
  is_ssa prog ⇔
    (* Operate function by function *)
    (∀fname.
      (* No register is assigned in two different instructions *)
      (∀r ip1 ip2.
        r ∈ image fst (assigns prog ip1) ∧ r ∈ image fst (assigns prog ip2) ∧
        ip1.f = fname ∧ ip2.f = fname
        ⇒
        ip_equiv ip1 ip2)) ∧
    (* Each use is dominated by its assignment *)
    (∀ip1 r. r ∈ uses prog ip1 ⇒
      ∃ip2. ip2.f = ip1.f ∧ r ∈ image fst (assigns prog ip2) ∧ dominates prog ip2 ip1) ∧
    (* All of the blocks are reachable. Otherwise, we could have dead code that
     * violates SSA, and this will wreck our treatment of a function body as a
     * list of blocks in dominator tree order *)
    (∀ip i. get_instr prog ip i ⇒ reachable prog ip)
End

Theorem ip_equiv_sym:
  ∀ip1 ip2. ip_equiv ip1 ip2 ⇔ ip_equiv ip2 ip1
Proof
  rw [ip_equiv_def] >> metis_tac []
QED

Theorem ip_equiv_refl:
  ∀ip. ip_equiv ip ip
Proof
  rw [ip_equiv_def]
QED

Theorem ip_equiv_trans:
  ∀ip1 ip2 ip3. ip_equiv ip1 ip2 ∧ ip_equiv ip2 ip3 ⇒ ip_equiv ip1 ip3
Proof
  rw [ip_equiv_def] >> metis_tac []
QED

Theorem ip_equiv_assigns:
  ∀prog ip1 ip2 rt.
    ip_equiv ip1 ip2 ∧ rt ∈ assigns prog ip1 ⇒ rt ∈ assigns prog ip2
Proof
  rw [ip_equiv_def, assigns_cases, IN_DEF] >>
  Cases_on `ip1 = ip2` >> rw []
  >- metis_tac []
  >- (fs [pc_component_equality] >> fs [get_instr_cases] >> fs [])
  >- metis_tac []
  >- (
    fs [pc_component_equality] >>
    fs [get_instr_cases, inc_pc_def, inc_bip_def, PULL_EXISTS] >>
    rw [] >> rfs [inc_bip_def] >>
    metis_tac [optionTheory.SOME_11])
QED

Theorem ip_equiv_next:
  ∀prog ip1 ip2 ip3.
    ip_equiv ip1 ip2 ∧ ip3 ∈ next_ips prog ip1 ⇒
    ip3 ∈ next_ips prog ip2
Proof
  rw [ip_equiv_def, next_ips_cases, IN_DEF] >>
  Cases_on `ip1 = ip2` >> rw []
  >- metis_tac []
  >- (fs [pc_component_equality] >> fs [get_instr_cases] >> fs [])
  >- metis_tac []
  >- (
    fs [pc_component_equality] >>
    fs [get_instr_cases, inc_pc_def, inc_bip_def, PULL_EXISTS] >>
    rw [] >> rfs [inc_bip_def] >>
    metis_tac [optionTheory.SOME_11])
QED

Theorem ip_equiv_dominates:
  ∀prog ip1 ip2 ip3.
   dominates prog ip1 ip2 ∧ ip_equiv ip2 ip3 ⇒ dominates prog ip1 ip3
Proof
  rw [dominates_def] >> metis_tac [ip_equiv_def]
QED

Theorem ip_equiv_dominates2:
  ∀prog ip1 ip2 ip3.
   dominates prog ip1 ip2 ∧ ip_equiv ip1 ip3 ⇒ dominates prog ip3 ip2
Proof
  rw [dominates_def] >> metis_tac [ip_equiv_def]
QED

Theorem ip_equiv_next_ips:
  ∀p i ip1 ip2. ip_equiv ip1 ip2 ∧ i ∈ next_ips p ip1 ⇒ i ∈ next_ips p ip2
Proof
  rw [ip_equiv_def] >>
  Cases_on `ip1.i = ip2.i`
  >- metis_tac [pc_component_equality] >>
  fs [] >> rw [] >> rfs [] >>
  fs [next_ips_cases, IN_DEF, inc_pc_def, inc_bip_def] >>
  fs [get_instr_cases] >> rw [] >> fs [] >>
  rw [pc_component_equality, PULL_EXISTS] >>
  rfs [] >> rw [] >> fs [inc_bip_def]
QED

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
  simp [Once ip_equiv_sym] >>
  rw [] >> fs [entry_ip_def]
  >- (
    fs [Once good_path_cases, ip_equiv_def] >> rw [] >>
    fs [next_ips_cases, IN_DEF] >> metis_tac [])
  >- (
    fs [Once good_path_cases, ip_equiv_def] >> rw [] >>
    fs [next_ips_cases, IN_DEF] >> metis_tac []) >>
  rfs [EL_CONS] >>
  `?m. n = Suc m` by (Cases_on `n` >> rw []) >>
  rw [] >> rfs [] >>
  `(el m path).f = (last (<|f := ip3.f; b := None; i := Offset 0|> ::path)).f`
  by (
    irule good_path_same_func >>
    qexists_tac `<| f:= ip3.f; b := NONE; i := Offset 0|> :: path` >>
    qexists_tac `prog` >>
    conj_tac >- rw [EL_MEM] >>
    metis_tac [MEM_LAST]) >>
  `(el m path).f = ip3.f` by metis_tac [ip_equiv_def] >>
  fs [] >> qpat_x_assum `_ ⇒ _` mp_tac >> impl_tac
  >- (
    irule good_path_prefix >> HINT_EXISTS_TAC >> rw [] >>
    metis_tac [take_is_prefix, ip_equiv_def]) >>
  rw [] >> drule MEM_FRONT >> rw []
  >- (qexists_tac `<|f := ip3.f; b := None; i := Offset 0|>` >> fs [ip_equiv_def]) >>
  fs [MEM_EL, LENGTH_FRONT] >> rfs [EL_TAKE] >> rw [] >>
  HINT_EXISTS_TAC >> rw [] >>
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
  Cases_on `ip_equiv ip1 (entry_ip ip1.f)` >> fs []
  >- (
    first_x_assum (qspec_then `[]` mp_tac) >> rw [] >>
    fs [Once good_path_cases, IN_DEF, next_ips_cases] >>
    metis_tac [ip_equiv_sym]) >>
  `path ≠ []` by (Cases_on `path` >> fs [] >> metis_tac [ip_equiv_sym]) >>
  `(OLEAST n. n < length path ∧ ip_equiv (el n path) ip1) ≠ None`
  by (
    rw [whileTheory.OLEAST_EQ_NONE] >>
    qexists_tac `PRE (length path)` >> rw [] >>
    fs [LAST_DEF, LAST_EL] >>
    Cases_on `path` >> fs []) >>
  qabbrev_tac `path1 = splitAtPki (\n ip. ip_equiv ip ip1) (\x y. x++[HD y]) path` >>
  first_x_assum (qspec_then `path1` mp_tac) >>
  simp [] >>
  `IS_PREFIX path path1`
  by (
    unabbrev_all_tac >> rw [splitAtPki_EQN] >>
    CASE_TAC >> rw [] >>
    fs [whileTheory.OLEAST_EQ_SOME] >>
    rw [GSYM SNOC_APPEND, SNOC_EL_TAKE, HD_DROP] >>
    metis_tac [take_is_prefix]) >>
  conj_asm1_tac >> rw []
  >- (irule good_path_prefix >> HINT_EXISTS_TAC >> rw [])
  >- (
    unabbrev_all_tac >> rw [splitAtPki_EQN] >>
    CASE_TAC >> rw [] >>
    fs [whileTheory.OLEAST_EQ_SOME] >>
    rw [LAST_DEF, HD_DROP]) >>
  `path1 ≠ []`
  by (fs [Abbr `path1`, splitAtPki_EQN] >> CASE_TAC >> rw []) >>
  simp [GSYM SNOC_APPEND, FRONT_SNOC, FRONT_DEF] >>
  CCONTR_TAC >> fs [MEM_EL]
  >- (
    first_x_assum (qspec_then `[]` mp_tac) >>
    fs [entry_ip_def, Once good_path_cases, IN_DEF, next_ips_cases] >>
    fs [ip_equiv_def] >>
    metis_tac []) >>
  rw [] >> rfs [] >>
  rename [`n1 < length (front _)`, `ip_equiv (el n _) _`,
          `ip_equiv _ (el n1 (front _))`] >>
  first_x_assum (qspec_then `take (Suc n1) path1` mp_tac) >> rw []
  >- (
    irule good_path_prefix >> HINT_EXISTS_TAC >> rw [entry_ip_def]
    >- (
      `(el n1 (front path1)).f = (entry_ip ip1.f).f` suffices_by fs [ip_equiv_def, entry_ip_def] >>
      irule good_path_same_func >>
      qexists_tac `entry_ip ip1.f::path1` >>
      qexists_tac `prog` >> rw [EL_MEM, entry_ip_def] >>
      rw [MEM_EL] >> disj2_tac >>
      qexists_tac `n1` >> rw [] >> rfs [LENGTH_FRONT] >>
      irule EL_FRONT >> rw [NULL_EQ, LENGTH_FRONT]) >>
    metis_tac [IS_PREFIX_APPEND3, take_is_prefix, IS_PREFIX_TRANS])
  >- (
    rfs [LENGTH_FRONT] >> rw [LAST_DEF]  >>
    metis_tac [EL_FRONT, NULL_EQ, ip_equiv_sym, LENGTH_FRONT]) >>
  rw [METIS_PROVE [] ``~x ∨ y ⇔ (x ⇒ y)``] >>
  simp [EL_FRONT] >>
  rfs [LENGTH_TAKE, LENGTH_FRONT] >>
  rename [`n2 < Suc _`] >>
  Cases_on `¬(0 < n2)` >> rw [EL_CONS]
  >- (fs [entry_ip_def] >> CCONTR_TAC >> fs [] >> fs [ip_equiv_def]) >>
  fs [EL_TAKE, Abbr `path1`, splitAtPki_EQN] >>
  CASE_TAC >> rw [] >> fs []
  >- metis_tac [] >>
  fs [whileTheory.OLEAST_EQ_SOME] >>
  rfs [LENGTH_TAKE] >>
  `PRE n2 < x` by decide_tac >>
  first_x_assum drule >>
  rfs [HD_DROP, LAST_DEF] >>
  rw [EL_TAKE, EL_APPEND_EQN] >>
  metis_tac [ip_equiv_sym]
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

Definition bip_less_def:
  (bip_less (Phi_ip _) (Offset _) ⇔ T) ∧
  (bip_less (Offset m) (Offset n) ⇔ m < n) ∧
  (bip_less _ _ ⇔ F)
End

Theorem bip_less_tri:
  ∀ip1 ip2. ip1.f = ip2.f ∧ ip1.b = ip2.b ⇒ bip_less ip1.i ip2.i ∨ bip_less ip2.i ip1.i ∨ ip_equiv ip1 ip2
Proof
  rw [] >> Cases_on `ip1.i` >> Cases_on `ip2.i`>> rw [bip_less_def, ip_equiv_def]
QED

Theorem ip_equiv_less:
  ∀ip1 ip2 ip3.
    ip_equiv ip2 ip3 ∧
    bip_less ip1.i ip2.i
    ⇒
    bip_less ip1.i ip3.i
Proof
  rw [ip_equiv_def] >>
  Cases_on `ip1.i` >> Cases_on `ip2.i` >> Cases_on `ip3.i` >> fs [bip_less_def]
QED

Theorem ip_equiv_less2:
  ∀ip1 ip2 ip3.
    ip_equiv ip2 ip3 ∧
    bip_less ip2.i ip1.i
    ⇒
    bip_less ip3.i ip1.i
Proof
  rw [ip_equiv_def] >>
  Cases_on `ip1.i` >> Cases_on `ip2.i` >> Cases_on `ip3.i` >> fs [bip_less_def]
QED


Triviality front_cons_snoc:
  ∀x y z. front (x::SNOC y z) = x::z
Proof
  Induct_on `z` >> fs [SNOC_APPEND]
QED

Triviality last_cons_snoc:
  ∀x y z. last (x::SNOC y z) = y
Proof
  Induct_on `z` >> fs []
QED

Triviality prefix_snoc:
  ∀x y. x ≼ SNOC y x
Proof
  Induct_on `x` >> fs []
QED

Triviality bip_lem:
  bip_less i1 i2 ⇒ ?n. i2 = Offset n
Proof
  Cases_on `i1` >> Cases_on `i2` >> fs [bip_less_def]
QED

Theorem next_ips_prev_entry:
  ∀prog ip1 ip2 ip3 f.
  (∀fname dec.
     alookup prog fname = Some dec ⇒
     every (λb. fst b = None ⇔ (snd b).h = Entry) dec.blocks) ⇒
  ip3 ∈ next_ips prog (entry_ip f) ∧
  (∃i. get_instr prog ip1 i) ∧
  ip1.f = ip2.f ∧
  ip1.b = ip2.b ∧
  bip_less ip1.i ip2.i ∧
  ip_equiv ip3 ip2
  ⇒
  ip1 = entry_ip f
Proof
  rw [IN_DEF, next_ips_cases, get_instr_cases, entry_ip_def, ip_equiv_def] >>
  rw [] >>
  Cases_on `HD b.body` >> fs [instr_next_ips_def] >>
  rw [] >> fs [inc_pc_def, inc_bip_def] >> rw [] >>
  fs [] >> rw [pc_component_equality] >>
  rfs [bip_less_def] >> rw [] >>
  res_tac >>
  fs [EVERY_MEM] >>
  metis_tac [blockHeader_distinct, FST, ALOOKUP_MEM, SND, EVERY_MEM, bip_lem, bip_distinct]
QED

Theorem next_ips_prev_less:
  ip2 ∈ next_ips prog ip3 ∧
  (∃i. get_instr prog ip1 i) ∧
  ip1.f = ip2.f ∧
  ip1.b = ip2.b ∧
  ~ip_equiv ip1 ip3 ∧
  bip_less ip1.i ip2.i
  ⇒
  ip3.b = ip2.b ∧ bip_less ip1.i ip3.i
Proof
  rw [IN_DEF, next_ips_cases, get_instr_cases, ip_equiv_def] >>
  rw [] >>
  Cases_on `el idx b.body` >> fs [instr_next_ips_def] >>
  rw [] >> fs [inc_pc_def, inc_bip_def] >> rw [bip_less_def] >>
  imp_res_tac bip_lem >>
  fs [] >>
  rfs [bip_less_def, inc_bip_def] >> rw [] >>
  fs [pc_component_equality] >> rfs []
QED

Theorem good_path_end_step:
  good_path prog (entry_ip f::SNOC ip2 (p1 ++ [ip3]))
  ⇒
  ip2 ∈ next_ips prog ip3
Proof
  rw [Once good_path_cases] >>
  qpat_x_assum `_ = _` (mp_tac o GSYM) >>
  rw [] >> fs [good_path_append] >>
  Cases_on `p1` >> fs [Once good_path_cases]
QED

Theorem same_block_dominates:
  ∀prog.
    (∀fname dec.
       alookup prog fname = Some dec ⇒
       every (λb. fst b = None ⇔ (snd b).h = Entry) dec.blocks) ⇒
    ∀ip1 ip2.
    ip1.f = ip2.f ∧ ip1.b = ip2.b ∧ (∃i. get_instr prog ip1 i) ⇒ bip_less ip1.i ip2.i ⇒ dominates prog ip1 ip2
Proof
  ntac 2 strip_tac >>
  simp [dominates_def, PULL_FORALL] >>
  ntac 3 gen_tac >>
  Q.ID_SPEC_TAC `ip2` >>
  Q.ID_SPEC_TAC `path` >>
  ho_match_mp_tac SNOC_INDUCT >> rw []
  >- (
    simp [Once good_path_cases, ip_equiv_def] >>
    Cases_on `ip1.i` >> Cases_on `ip2.i` >> fs [bip_less_def] >>
    rw [entry_ip_def, pc_component_equality] >>
    CCONTR_TAC >> fs [] >> rw [] >> fs [get_instr_cases] >> rw [] >>
    first_x_assum drule >> simp [EXISTS_MEM] >>
    qexists_tac `(ip1.b, b)` >> rw [] >>
    metis_tac [optionTheory.SOME_11, ALOOKUP_MEM]) >>
  simp [front_cons_snoc] >> fs [last_cons_snoc] >> rw [] >>
  `good_path prog (entry_ip ip2.f::path)`
  by (irule good_path_prefix >> HINT_EXISTS_TAC >> rw [prefix_snoc]) >>
  Cases_on `path = []` >> rw []
  >- (
    fs [Once good_path_cases] >>
    fs [Once good_path_cases] >>
    metis_tac [next_ips_prev_entry, ip_equiv_refl]) >>
  `?p1 ip3. path = p1 ++ [ip3]` by metis_tac [SNOC_CASES, SNOC_APPEND] >>
  fs [] >> rw [] >>
  rename1 `ip_equiv ip2' _` >>
  `ip3.f = ip2'.f`
  by (
    irule good_path_same_func >>
    qexists_tac `entry_ip ip2.f::SNOC ip2' (p1 ++ [ip3])` >>
    rw [] >> metis_tac []) >>
  Cases_on `ip_equiv ip1 ip3` >> rw []
  >- metis_tac [] >>
 `ip3.b = ip2'.b ∧ bip_less ip1.i ip3.i`
  by (
    drule good_path_end_step >> strip_tac >>
    irule next_ips_prev_less >>
    fs [] >> rw []
    >- fs [ip_equiv_def]
    >- fs [ip_equiv_def]
    >- metis_tac []
    >- metis_tac [ip_equiv_less, ip_equiv_sym]) >>
  first_x_assum (qspec_then `ip3` mp_tac) >> simp [] >>
  impl_tac
  >- metis_tac [ip_equiv_def] >>
  impl_tac >> rw []
  >- fs [ip_equiv_def]
  >- rw [LAST_DEF, ip_equiv_def] >>
  qexists_tac `ip1'` >> rw [] >>
  drule MEM_FRONT >> fs [ip_equiv_def]
QED

Theorem next_ips_still_leq:
  ip2 ∈ next_ips prog ip1 ∧
  ip2.f = ip1.f ∧
  ip2.f = ip3.f ∧
  ip1.b = ip3.b ∧
  (∀inst. get_instr prog ip1 (Inl inst) ⇒ ~terminator inst) ∧
  bip_less ip1.i ip3.i
  ⇒
  bip_less ip2.i ip3.i ∧ ip2.b = ip1.b ∨ ip2 = ip3
Proof
  rw [IN_DEF, next_ips_cases] >> rfs []
  >- (
    fs [get_instr_cases] >> rw [] >> rfs [] >> rw [] >> fs [] >>
    Cases_on `el idx b.body` >> fs [instr_next_ips_def] >> rw [] >> fs [inc_pc_def] >>
    rw [] >> rfs [inc_bip_def] >>
    Cases_on `ip3.i` >> fs [bip_less_def, pc_component_equality] >>
    rw [] >> fs [terminator_def])
  >- (
    fs [get_instr_cases, inc_pc_def] >> rw [] >> rfs [inc_bip_def] >> rw [pc_component_equality] >>
    Cases_on `ip3.i` >> fs [bip_less_def])
QED

Theorem good_path_find_ip:
  ∀prog path.
    good_path prog path ⇒
    ∀ip.
    loc_prog_ok prog ∧
    path ≠ [] ∧
    (last path).b ≠ ip.b ∧
    ip.f = (HD path).f ∧ ip.b = (HD path).b ∧ bip_less (HD path).i ip.i ∧
    (∃inst. get_instr prog ip inst)
    ⇒
    mem ip (front path)
Proof
  ho_match_mp_tac good_path_ind >> rw []
  >- metis_tac [] >>
  drule next_ips_same_func >> rw [] >>
  drule next_ips_still_leq >> simp [] >>
  disch_then (qspec_then `ip` mp_tac) >> simp [] >>
  impl_tac
  >- (
    rw [get_instr_cases] >>
    `every (λi. ~terminator i) (front b.body) ∧ b.body ≠ []` by metis_tac [loc_prog_ok_def] >>
    fs [EVERY_MEM, MEM_EL] >>
    first_x_assum (qspec_then `el idx b.body` mp_tac) >>
    impl_tac
    >- (
      qexists_tac `idx` >> conj_asm1_tac
      >- (
        Cases_on `ip.i` >> fs [bip_less_def, LENGTH_FRONT] >>
        fs [get_instr_cases] >>
        rw [] >> rfs [] >> rw [] >> fs []) >>
      metis_tac [EL_FRONT, NULL_EQ]) >>
    metis_tac []) >>
  rw []
  >- metis_tac [next_ips_same_func] >>
  Cases_on `path` >> fs []
QED

Theorem dominates_same_block:
  ∀ip1 ip2 ip3.
    loc_prog_ok prog ∧ dominates prog ip1 ip3 ∧ ip1.f = ip2.f ∧ ip1.b = ip2.b ∧
    ip3.b ≠ ip1.b ∧ bip_less ip1.i ip2.i ∧ (∃inst. get_instr prog ip2 instr)
    ⇒
    dominates prog ip2 ip3
Proof
  rw [dominates_def] >>
  first_x_assum drule >> rw [] >>
  drule MEM_FRONT >>
  REWRITE_TAC [Once MEM_SPLIT] >> strip_tac >>
  `good_path prog (ip1'::l2)`
  by (
    full_simp_tac std_ss [GSYM APPEND_ASSOC] >>
    full_simp_tac std_ss [Once good_path_append]) >>
  full_simp_tac std_ss [LAST_APPEND] >>
  drule good_path_find_ip >>
  disch_then (qspec_then `ip2` mp_tac) >>
  impl_tac >> rw []
  >- (fs [ip_equiv_def] >> metis_tac [])
  >- (
    `ip1'.f = (entry_ip ip3.f).f` suffices_by (fs [ip_equiv_def]) >>
    irule good_path_same_func >>
    qexists_tac `entry_ip ip3.f::path` >>
    qexists_tac `prog` >> rw_tac std_ss []
    >- rw []
    >- metis_tac [MEM])
  >- fs [ip_equiv_def]
  >- metis_tac [ip_equiv_less2]
  >- metis_tac []
  >- (
    qexists_tac `ip2` >> rw [ip_equiv_refl] >>
    rw [FRONT_APPEND])
QED

(* ----- Liveness ----- *)

Definition live_def:
  live prog ip =
    { r | ∃path.
            good_path prog (ip::path) ∧
            r ∈ uses prog (last (ip::path)) ∧
            ∀ip2. ip2 ∈ set (front (ip::path)) ⇒ r ∉ image fst (assigns prog ip2) }
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
    BIGUNION {live prog ip' | ip' | ip' ∈ next_ips prog ip} DIFF image fst (assigns prog ip) ∪ uses prog ip
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
    is_ssa prog ∧ ip1.f = ip2.f ∧ r ∈ image fst (assigns prog ip1) ∧ r ∈ live prog ip2 ⇒
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
        rw [] >> rw [] >>
        metis_tac [ip_equiv_next, ip_equiv_sym]))
    >- (Cases_on `path` >> fs [] >> metis_tac [ip_equiv_refl])) >>
  rw [] >>
  `ip1'.f = (last (entry_ip ip2.f::path')).f`
  by (
    irule good_path_same_func >>
    qexists_tac `entry_ip ip2.f::path'` >>
    qexists_tac `prog` >>
    conj_tac
    >- (
      Cases_on `path` >>
      full_simp_tac std_ss [GSYM APPEND, FRONT_APPEND, APPEND_NIL, LAST_CONS]
      >- metis_tac [MEM_FRONT] >>
      full_simp_tac std_ss [GSYM APPEND, FRONT_APPEND] >> fs [] >> rw [FRONT_DEF] >> fs [] >>
      metis_tac [ip_equiv_assigns])
    >- metis_tac [MEM_LAST]) >>
  `ip2'.f = ip1.f` by fs [ip_equiv_def] >>
  `ip_equiv ip2' ip1` by metis_tac [] >>
  rw [] >>
  Cases_on `path` >> fs [] >>
  full_simp_tac std_ss [GSYM APPEND, FRONT_APPEND] >> fs [] >> rw [FRONT_DEF] >> fs []
  >- (fs [FRONT_DEF] >> metis_tac [ip_equiv_sym, ip_equiv_trans])
  >- (fs [ip_equiv_def, entry_ip_def] >> metis_tac [pc_component_equality])
  >- (fs [FRONT_DEF] >> metis_tac [ip_equiv_sym, ip_equiv_trans])
  >- (
    `mem ip1' path' = mem ip1' (front path' ++ [last path'])` by metis_tac [APPEND_FRONT_LAST] >>
    fs [LAST_DEF] >>
    metis_tac [ip_equiv_trans, ip_equiv_assigns])
  >- metis_tac [ip_equiv_assigns]
  >- metis_tac [ip_equiv_assigns]
QED

Theorem ssa_dominates_live_range:
  ∀prog r ip.
    is_ssa prog ∧ r ∈ uses prog ip
    ⇒
    ∃ip1. ip1.f = ip.f ∧ r ∈ image fst (assigns prog ip1) ∧
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
  `ip1'.f = (last (entry_ip ip2.f::path)).f` suffices_by fs [ip_equiv_def] >>
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
  simp [Once good_path_cases, ip_equiv_refl] >>
  rw []
  >- (fs [next_ips_cases, IN_DEF] >> metis_tac [])
  >- metis_tac [ip_equiv_next, ip_equiv_sym]
QED

(* ----- A theory of *dominator ordered* programs ------ *)
(* A list of basic blocks is dominator ordered if each variable use occurs after
 * the assignment to that variable. We can also define a notion of variable
 * liveness that follows the list structure, rather than the CFG structure, and
 * show that for dominator ordered lists, the live set is empty at the entry
 * point *)

Definition instrs_live_def:
  (instrs_live [] = ({}, {})) ∧
  (instrs_live (i::is) =
    let (gen, kill) = instrs_live is in
      (instr_uses i ∪ (gen DIFF image fst (instr_assigns i)),
       (image fst (instr_assigns i) ∪ (kill DIFF instr_uses i))))
End

Definition header_uses_def:
  (header_uses (Head phis land) =
    bigunion { phi_uses from_l p | from_l,p | mem p phis }) ∧
  (header_uses Entry = {})
End

Definition header_assigns_def:
  (header_assigns (Head phis land) = set (map (fst o phi_assigns) phis)) ∧
  (header_assigns Entry = {})
End

Definition linear_live_def:
  (linear_live [] = {}) ∧
  (linear_live (b::bs) =
    let (gen,kill) = instrs_live b.body in
    header_uses b.h ∪ (gen ∪ (linear_live bs DIFF kill) DIFF header_assigns b.h))
End

Definition linear_pc_less_def:
  linear_pc_less = $< LEX bip_less
End

Inductive lpc_get_instr:
 (∀i idx bs.
  i < length bs ∧
  idx < length (el i bs).body
  ⇒
  lpc_get_instr bs (i, Offset idx) (Inl (el idx (el i bs).body))) ∧
 (∀i from_l phis bs landing.
  i < length bs ∧
  (el i bs).h = Head phis landing
  ⇒
  lpc_get_instr bs (i, Phi_ip from_l) (Inr (from_l, phis)))
End

Inductive lpc_assigns:
 (∀bs ip i r.
    lpc_get_instr bs ip (Inl i) ∧
    r ∈ instr_assigns i
    ⇒
    lpc_assigns bs ip r) ∧
 (∀bs ip from_l phis r.
    lpc_get_instr bs ip (Inr (from_l, phis)) ∧
    r ∈ set (map phi_assigns phis)
    ⇒
    lpc_assigns bs ip r)
End

Inductive lpc_uses:
 (∀bs ip i r.
    lpc_get_instr bs ip (Inl i) ∧
    r ∈ instr_uses i
    ⇒
    lpc_uses bs ip r) ∧
 (∀bs ip from_l phis r.
    lpc_get_instr bs ip (Inr (from_l, phis)) ∧
    r ∈ BIGUNION (set (map (phi_uses from_l) phis))
    ⇒
    lpc_uses bs ip r)
End

Definition dominator_ordered_def:
  dominator_ordered p ⇔
    ∀f d lip1 r.
      alookup p (Fn f) = Some d ∧
      r ∈ lpc_uses (map snd d.blocks) lip1
      ⇒
      ∃lip2. linear_pc_less lip2 lip1 ∧ r ∈ image fst (lpc_assigns (map snd d.blocks) lip2)
End

Theorem instrs_kill_subset_assigns:
  snd (instrs_live is) ⊆ bigunion (image (λi. image fst (instr_assigns i)) (set is))
Proof
  Induct_on `is` >> rw [instrs_live_def] >>
  pairarg_tac >> rw [] >>
  fs [SUBSET_DEF]
QED

Theorem instrs_gen_subset_uses:
  fst (instrs_live is) ⊆ bigunion (image instr_uses (set is))
Proof
  Induct_on `is` >> rw [instrs_live_def] >>
  pairarg_tac >> rw [] >>
  fs [SUBSET_DEF]
QED

Theorem instrs_subset_assigns_subset_kill_gen:
  bigunion (image (λi. image fst (instr_assigns i)) (set is)) ⊆
    snd (instrs_live is) ∪ fst (instrs_live is)
Proof
  Induct_on `is` >> rw [instrs_live_def] >>
  pairarg_tac >> rw [] >> fs [SUBSET_DEF] >> rw [] >>
  metis_tac []
QED

Theorem use_assign_in_gen_kill:
  ∀n is r.
    n < length is ∧ (r ∈ image fst (instr_assigns (el n is)) ∨ r ∈ instr_uses (el n is))
    ⇒
    r ∈ fst (instrs_live is) ∨ r ∈ snd (instrs_live is)
Proof
  Induct_on `n` >> rw [] >> Cases_on `is` >> rw [] >> fs [] >>
  rw [instrs_live_def] >>
  pairarg_tac >> rw [] >>
  metis_tac [FST, SND, pair_CASES]
QED

Theorem instrs_live_uses:
  ∀is r.
    r ∈ fst (instrs_live is)
    ⇒
    ∃i. i < length is ∧ r ∈ instr_uses (el i is) ∧
        ∀j. j < i ⇒ r ∉ instr_uses (el j is) ∧ r ∉ image fst (instr_assigns (el j is))
Proof
  Induct >> rw [instrs_live_def] >> pairarg_tac >> fs []
  >- (qexists_tac `0` >> rw []) >>
  rename1 `(i1::is)` >>
  Cases_on `r ∈ instr_uses i1`
  >- (qexists_tac `0` >> rw []) >>
  first_x_assum drule >> rw [] >>
  qexists_tac `Suc i` >> rw [] >>
  Cases_on `j` >> fs []
QED

Theorem lpc_get_instr_cons:
  ∀b bs i bip.
    lpc_get_instr (b::bs) (i + 1, bip) = lpc_get_instr bs (i, bip)
Proof
  rw [lpc_get_instr_cases, EXTENSION, IN_DEF, EL_CONS] >>
  `PRE (i + 1) = i` by decide_tac >>
  rw [ADD1]
QED

Theorem lpc_uses_cons:
  ∀b bs i bip.
    lpc_uses (b::bs) (i + 1, bip) = lpc_uses bs (i, bip)
Proof
  rw [lpc_uses_cases, EXTENSION, IN_DEF, lpc_get_instr_cons]
QED

Theorem lpc_uses_0_head:
  ∀b bs. header_uses b.h = bigunion { lpc_uses (b::bs) (0, Phi_ip from_l) | from_l | T}
Proof
  rw [EXTENSION, IN_DEF] >>
  rw [lpc_uses_cases, lpc_get_instr_cases, PULL_EXISTS] >>
  Cases_on `b.h` >> rw [header_uses_def, MEM_MAP, PULL_EXISTS]
  >- metis_tac [] >>
  eq_tac >> rw []
  >- (
    qexists_tac `(\x'. ∃y. x' ∈ phi_uses from_l y ∧ mem y l)` >>
    qexists_tac `from_l` >>
    rw [] >>
    metis_tac []) >>
  metis_tac []
QED

Theorem lpc_uses_0_body:
  ∀b bs. lpc_uses (b::bs) (0, Offset n) ⊆ fst (instrs_live b.body) ∪ snd (instrs_live b.body)
Proof
  rw [SUBSET_DEF, IN_DEF] >>
  fs [lpc_uses_cases, lpc_get_instr_cases, PULL_EXISTS] >>
  metis_tac [use_assign_in_gen_kill, IN_DEF]
QED

Theorem lpc_assigns_cons:
  ∀b bs i bip.
    lpc_assigns (b::bs) (i + 1, bip) = lpc_assigns bs (i, bip)
Proof
  rw [lpc_assigns_cases, EXTENSION, IN_DEF, lpc_get_instr_cons]
QED

Theorem lpc_assigns_0_head:
  ∀b bs from_l.
    image fst (lpc_assigns (b::bs) (0, Phi_ip from_l)) = header_assigns b.h
Proof
  rw [EXTENSION, Once IN_DEF] >>
  rw [lpc_assigns_cases, lpc_get_instr_cases, PULL_EXISTS] >>
  Cases_on `b.h` >> rw [header_assigns_def, MEM_MAP] >>
  metis_tac []
QED

Theorem lpc_assigns_0_body:
  ∀b bs. image fst (lpc_assigns (b::bs) (0, Offset n)) ⊆ fst (instrs_live b.body) ∪ snd (instrs_live b.body)
Proof
  rw [SUBSET_DEF, IN_DEF] >>
  fs [lpc_assigns_cases, lpc_get_instr_cases, PULL_EXISTS] >>
  drule use_assign_in_gen_kill >>
  rw [] >>
  metis_tac [IN_DEF]
QED

Theorem linear_live_uses:
  ∀bs r. r ∈ linear_live bs ⇒
    ∃lip. r ∈ lpc_uses bs lip ∧
        ∀lip2. linear_pc_less lip2 lip ⇒ r ∉ lpc_uses bs lip2 ∧ r ∉ image fst (lpc_assigns bs lip2)
Proof
  Induct >> rw [linear_live_def] >>
  rename1 `header_uses b.h` >>
  Cases_on `r ∈ header_uses b.h`
  >- (
    fs [header_uses_def] >> pairarg_tac >> fs [] >>
    Cases_on `b.h` >> fs [header_uses_def] >>
    qexists_tac `(0, Phi_ip from_l)` >> fs [header_uses_def] >>
    conj_tac
    >- (
      simp [IN_DEF] >>
      rw [lpc_uses_cases, lpc_get_instr_cases, PULL_EXISTS] >>
      rw [MEM_MAP] >> metis_tac [])
    >- (
      gen_tac >> simp [linear_pc_less_def, LEX_DEF] >>
      pairarg_tac >> simp [bip_less_def])) >>
  pairarg_tac >> Cases_on `r ∈ gen` >> fs []
  >- (
    `r ∈ fst (instrs_live b.body)` by metis_tac [FST] >>
    drule instrs_live_uses >> rw [] >>
    qexists_tac `(0, Offset i)` >>
    conj_tac
    >- (
      simp [IN_DEF] >>
      rw [lpc_uses_cases, lpc_get_instr_cases, PULL_EXISTS] >>
      rw [MEM_MAP] >> metis_tac [])
    >- (
      gen_tac >> strip_tac >>
      PairCases_on `lip2` >> fs [linear_pc_less_def, LEX_DEF_THM] >>
      Cases_on `lip21` >> fs [bip_less_def]
      >- (
        Cases_on `b.h` >> fs [header_assigns_def, header_uses_def] >>
        simp [IN_DEF] >>
        rw [lpc_uses_cases, lpc_assigns_cases, lpc_get_instr_cases, PULL_EXISTS] >>
        fs [MEM_MAP] >>
        metis_tac [FST])
      >- (
        first_x_assum drule >>
        simp [IN_DEF] >>
        rw [lpc_uses_cases, lpc_assigns_cases, lpc_get_instr_cases, PULL_EXISTS] >>
        rw [IN_DEF])))
  >- (
    first_x_assum drule >> rw [] >>
    PairCases_on `lip` >>
    qexists_tac `lip0+1,lip1` >> simp [IN_DEF] >>
    conj_tac
    >- fs [lpc_uses_cons, IN_DEF] >>
    gen_tac >> disch_tac >>
    PairCases_on `lip2` >>
    Cases_on `lip20` >> fs [ADD1]
    >- (
      Cases_on `lip21`
      >- (
        rename1 `Phi_ip from_l` >>
        `r ∉ bigunion {lpc_uses (b::bs) (0,Phi_ip from_l) | from_l | T} ∧
         r ∉ image fst (lpc_assigns (b::bs) (0,Phi_ip from_l))`
        by metis_tac [lpc_assigns_0_head, lpc_uses_0_head] >>
        fs [IN_DEF] >> metis_tac [])
      >- (
        `r ∉ image fst (lpc_assigns (b::bs) (0,Offset n)) ∧
         r ∉ lpc_uses (b::bs) (0,Offset n)`
        by metis_tac [IN_UNION, lpc_assigns_0_body, lpc_uses_0_body, FST, SND, SUBSET_DEF] >>
        fs [IN_DEF]))
    >- (
      `linear_pc_less (n, lip21) (lip0, lip1)` by fs [linear_pc_less_def, LEX_DEF] >>
      first_x_assum drule >>
      rw [lpc_uses_cons, lpc_assigns_cons] >> fs [IN_DEF]))
QED

Theorem dominator_ordered_linear_live:
  ∀p f d.
    dominator_ordered p ∧
    alookup p (Fn f) = Some d
    ⇒
    linear_live (map snd d.blocks) = {}
Proof
  rw [dominator_ordered_def] >> first_x_assum drule >> rw [EXTENSION] >>
  CCONTR_TAC >> fs [] >> drule linear_live_uses >> rw [] >>
  metis_tac []
QED

Definition block_assigns_def:
  block_assigns (l, b) =
    header_assigns b.h ∪ image fst (bigunion (image instr_assigns (set b.body)))
End

Definition block_uses_def:
  block_uses (l, b) =
    header_uses b.h ∪ bigunion (image instr_uses (set b.body))
End

Definition block_order_def:
  block_order bs =
    tc { (b1, b2) |
          fst b1 ≠ fst b2 ∧ mem b1 bs ∧ mem b2 bs ∧ (∃r. r ∈ block_assigns b1 ∧ r ∈ block_uses b2) }
End

Theorem prog_ok_distinct_lem:
  loc_prog_ok p ∧ alookup p f = Some d ⇒ all_distinct (map fst d.blocks)
Proof
  rw [loc_prog_ok_def, EVERY_MEM] >>
  drule ALOOKUP_MEM >> rw [] >>
  res_tac >> fs []
QED

Theorem block_order_dominates:
  ∀b1 b2.
    (b1,b2) ∈ block_order d.blocks ⇒
    loc_prog_ok prog ∧
    is_ssa prog ∧
    alookup prog f = Some d ⇒
    ∃ip1 ip2.
      dominates prog ip1 ip2 ∧
      ip1.f = f ∧
      ip2.f = f ∧
      ip1.b = fst b1 ∧
      ip2.b = fst b2 ∧
      fst b1 ≠ fst b2 ∧
      (∃i2. get_instr prog ip2 i2)
Proof
  simp [block_order_def] >>
  ho_match_mp_tac tc_ind >> rw []
  >- (
    fs [is_ssa_def] >>
    `∃ip1. r ∈ uses prog ip1 ∧ ip1.f = f ∧ ip1.b = fst b2`
    by (
      simp [Once IN_DEF, uses_cases] >> Cases_on `b2` >> fs [block_uses_def] >>
      fs [get_instr_cases, PULL_EXISTS]
      >- (
        rename1 `_ ∈ header_uses b.h` >>
        Cases_on `b.h` >> fs [header_uses_def] >> rw [] >>
        rename1 `mem (l1, _) _` >>
        qexists_tac `<| f := f; b := l1; i := Phi_ip from_l |>` >> rw [] >>
        qexists_tac `l` >> qexists_tac `phi_uses from_l p` >> qexists_tac `b` >>
        rw [MEM_MAP] >>
        metis_tac [ALOOKUP_ALL_DISTINCT_MEM, loc_prog_ok_def, prog_ok_distinct_lem])
      >- (
        fs [MEM_EL] >> rw [] >>
        rename [`n1 < length b.body`, `(l1, _) = el _ _`] >>
        qexists_tac `<| f := f; b := l1; i := Offset n1 |>` >> rw [] >>
        metis_tac [ALOOKUP_ALL_DISTINCT_MEM, loc_prog_ok_def, EL_MEM, prog_ok_distinct_lem])) >>
    first_x_assum drule >> rw [] >>
    qexists_tac `ip2` >> qexists_tac `ip1` >> rw []
    >- (
      rename1 `rt ∈ assigns _ _` >>
      `∃ip3 t. (fst rt, t) ∈ assigns prog ip3 ∧ ip3.f = ip1.f ∧ ip3.b = fst b1`
      by (
        simp [Once IN_DEF, assigns_cases] >> Cases_on `b1` >> fs [block_assigns_def] >>
        fs [get_instr_cases, PULL_EXISTS]
        >- (
          rename1 `_ ∈ header_assigns b.h` >>
          Cases_on `b.h` >> fs [header_assigns_def] >> rw [] >>
          rename1 `mem (l1, _) _` >>
          fs [MEM_MAP] >>
          qexists_tac `<| f := ip1.f; b := l1; i := Phi_ip from_l |>` >> rw [] >>
          qexists_tac `SND (phi_assigns y)` >> qexists_tac `l` >> qexists_tac `b` >>
          qexists_tac `o'` >> rw [] >>
          metis_tac [FST, ALOOKUP_ALL_DISTINCT_MEM, loc_prog_ok_def, SND, prog_ok_distinct_lem])
        >- (
          fs [MEM_EL] >> rw [] >>
          rename [`n1 < length b.body`, `(l1, _) = el _ _`] >>
          qexists_tac `<| f := ip1.f; b := l1; i := Offset n1 |>` >> rw [] >>
          qexists_tac `snd x` >>
          rw [] >>
          metis_tac [ALOOKUP_ALL_DISTINCT_MEM, loc_prog_ok_def, EL_MEM, FST, SND, prog_ok_distinct_lem])) >>
      metis_tac [ip_equiv_def, FST])
    >- (fs [uses_cases, IN_DEF] >> metis_tac []))
  >- (
    first_x_assum drule >> first_x_assum drule >> rw [] >>
    Cases_on `ip1.b = ip2.b ∨ ip1'.b = ip2'.b`
    >- metis_tac [] >>
    qexists_tac `ip1` >> qexists_tac `ip2'` >>
    Cases_on `bip_less ip2.i ip1'.i`
    >- (
      `dominates prog ip2 ip1'` by metis_tac [same_block_dominates, loc_prog_ok_def] >>
      rw []
      >- metis_tac [dominates_trans]
      >- (
        CCONTR_TAC >> fs [] >>
        Cases_on `bip_less ip2'.i ip1.i`
        >- (
          `dominates prog ip2' ip1` by metis_tac [same_block_dominates, loc_prog_ok_def] >>
          metis_tac [dominates_trans, dominates_antisym, is_ssa_def]) >>
        Cases_on `bip_less ip1.i ip2'.i`
        >- (
          `dominates prog ip2' ip2` by metis_tac [dominates_same_block] >>
          metis_tac [dominates_antisym, dominates_trans, is_ssa_def])
        >- (
          `dominates prog ip1' ip1` by metis_tac [ip_equiv_dominates, bip_less_tri] >>
          metis_tac [dominates_antisym, dominates_trans, is_ssa_def]))
      >- metis_tac []) >>
    Cases_on `bip_less ip1'.i ip2.i`
    >- (
      `dominates prog ip2 ip2'` by metis_tac [dominates_same_block] >>
      rw []
      >- metis_tac [dominates_trans]
      >- (
        CCONTR_TAC >> fs [] >>
        Cases_on `bip_less ip2'.i ip1.i`
        >- (
          `dominates prog ip2' ip1` by metis_tac [same_block_dominates, loc_prog_ok_def] >>
          metis_tac [dominates_trans, dominates_antisym, is_ssa_def]) >>
        Cases_on `bip_less ip1.i ip2'.i`
        >- (
          `dominates prog ip2' ip2` by metis_tac [dominates_same_block] >>
          metis_tac [dominates_antisym, dominates_trans, is_ssa_def])
        >- (
          `dominates prog ip2 ip1` by metis_tac [ip_equiv_dominates, bip_less_tri] >>
          metis_tac [dominates_antisym, dominates_trans, is_ssa_def]))
      >- metis_tac [])
    >- (
      `ip_equiv ip1' ip2` by metis_tac [bip_less_tri] >>
      `dominates prog ip1 ip2'` by metis_tac [dominates_trans, ip_equiv_sym, ip_equiv_dominates] >>
      rw []
      >- (
        CCONTR_TAC >> fs [] >>
        Cases_on `bip_less ip2'.i ip1.i`
        >- (
          `dominates prog ip2' ip1` by metis_tac [same_block_dominates, loc_prog_ok_def] >>
          metis_tac [dominates_trans, dominates_antisym, is_ssa_def]) >>
        Cases_on `bip_less ip1.i ip2'.i`
        >- (
          `dominates prog ip2' ip2` by metis_tac [dominates_same_block] >>
          `dominates prog ip2 ip2'` by metis_tac [ip_equiv_dominates2, ip_equiv_sym] >>
          metis_tac [dominates_antisym, is_ssa_def])
        >- (
          `dominates prog ip2' ip2` by metis_tac [ip_equiv_dominates2, bip_less_tri] >>
          `dominates prog ip2 ip2'` by metis_tac [ip_equiv_dominates2, bip_less_tri] >>
          metis_tac [dominates_antisym, is_ssa_def]))
      >- metis_tac []))
QED

Theorem block_order_po:
  ∀prog f d.
    loc_prog_ok prog ∧ is_ssa prog ∧
    alookup prog f = Some d
    ⇒
    partial_order (rc (block_order d.blocks) (set d.blocks)) (set d.blocks)
Proof
  rw [partial_order_def]
  >- (rw [block_order_def, domain_def, SUBSET_DEF, rc_def] >> drule tc_domain_range >> rw [domain_def])
  >- (rw [block_order_def, range_def, SUBSET_DEF, rc_def] >> rw [] >> drule tc_domain_range >> rw [range_def])
  >- metis_tac [block_order_def, transitive_rc, tc_transitive]
  >- metis_tac [rc_is_reflexive]
  >- (
    simp [antisym_rc] >> rw [antisym_def] >>
    pop_assum mp_tac >>
    drule block_order_dominates >>
    disch_then drule >> simp [] >> disch_then drule >> rw [] >>
    drule block_order_dominates >>
    disch_then drule >> simp [] >> disch_then drule >> rw [] >>
    CCONTR_TAC >>
    Cases_on `bip_less ip2.i ip1'.i`
    >- (
      `dominates prog ip2 ip1'` by metis_tac [same_block_dominates, loc_prog_ok_def] >>
      Cases_on `bip_less ip2'.i ip1.i`
      >- (
        `dominates prog ip2' ip1` by metis_tac [same_block_dominates, loc_prog_ok_def] >>
        metis_tac [dominates_trans, dominates_antisym, is_ssa_def]) >>
      Cases_on `bip_less ip1.i ip2'.i`
      >- (
        `dominates prog ip2' ip2` by metis_tac [dominates_same_block] >>
        metis_tac [dominates_antisym, dominates_trans, is_ssa_def])
      >- (
        `dominates prog ip1' ip1` by metis_tac [ip_equiv_dominates, bip_less_tri] >>
        metis_tac [dominates_antisym, dominates_trans, is_ssa_def])) >>
    Cases_on `bip_less ip1'.i ip2.i`
    >- (
      `dominates prog ip2 ip2'` by metis_tac [dominates_same_block] >>
      Cases_on `bip_less ip2'.i ip1.i`
      >- (
        `dominates prog ip2' ip1` by metis_tac [same_block_dominates, loc_prog_ok_def] >>
        metis_tac [dominates_trans, dominates_antisym, is_ssa_def]) >>
      Cases_on `bip_less ip1.i ip2'.i`
      >- (
        `dominates prog ip2' ip2` by metis_tac [dominates_same_block] >>
        metis_tac [dominates_antisym, dominates_trans, is_ssa_def])
      >- (
        `dominates prog ip2 ip1` by metis_tac [ip_equiv_dominates, bip_less_tri] >>
        metis_tac [dominates_antisym, dominates_trans, is_ssa_def]))
    >- (
      `ip_equiv ip1' ip2` by metis_tac [bip_less_tri] >>
      `dominates prog ip1 ip2'` by metis_tac [dominates_trans, ip_equiv_sym, ip_equiv_dominates] >>
      Cases_on `bip_less ip2'.i ip1.i`
      >- (
        `dominates prog ip2' ip1` by metis_tac [same_block_dominates, loc_prog_ok_def] >>
        metis_tac [dominates_trans, dominates_antisym, is_ssa_def]) >>
      Cases_on `bip_less ip1.i ip2'.i`
      >- (
        `dominates prog ip2' ip2` by metis_tac [dominates_same_block] >>
        `dominates prog ip2 ip2'` by metis_tac [ip_equiv_dominates2, ip_equiv_sym] >>
        metis_tac [dominates_antisym, is_ssa_def])
      >- (
        `dominates prog ip2' ip2` by metis_tac [ip_equiv_dominates2, bip_less_tri] >>
        `dominates prog ip2 ip2'` by metis_tac [ip_equiv_dominates2, bip_less_tri] >>
        metis_tac [dominates_antisym, is_ssa_def])))
QED

Theorem assigns_weak:
  ∀l d p ip r. ~mem l (map fst p) ∧ r ∈ assigns p ip ⇒ r ∈ assigns ((l,d)::p) ip
Proof
  rw [assigns_cases, IN_DEF, get_instr_cases, PULL_EXISTS] >>
  imp_res_tac ALOOKUP_MEM >>
  fs [LIST_TO_SET_MAP] >> metis_tac [FST]
QED

Theorem uses_weak:
  ∀l d p ip r. ip.f ≠ l ⇒ uses ((l,d)::p) ip = uses p ip
Proof
  rw [uses_cases, EXTENSION, IN_DEF, get_instr_cases, PULL_EXISTS]
QED

Theorem assigns_weak2:
  ∀l d p ip r. ip.f ≠ l ⇒ assigns ((l,d)::p) ip = assigns p ip
Proof
  rw [assigns_cases, EXTENSION, IN_DEF, get_instr_cases, PULL_EXISTS]
QED

Theorem good_path_weak:
  ∀l d p ip path. ip.f ≠ l ⇒ good_path ((l,d)::p) (ip::path) = good_path p (ip::path)
Proof
  Induct_on `path` >> rw [] >>
  ONCE_REWRITE_TAC [good_path_cases] >> rw []
  >- rw [EXTENSION, IN_DEF, get_instr_cases, PULL_EXISTS] >>
  rename1 `good_path _ (p1::_)` >>
  Cases_on `p1.f ≠ l`
  >- (
    first_x_assum drule >> rw [] >>
    eq_tac >> rw [] >>
    fs [next_ips_cases, EXTENSION, IN_DEF, get_instr_cases, PULL_EXISTS] >> rw [] >>
    metis_tac [])
  >- metis_tac [next_ips_same_func]
QED

Theorem dominates_weak:
  ∀l d p ip1 ip2. ip1.f ≠ l ⇒ dominates ((l,d)::p) ip2 ip1 = dominates p ip2 ip1
Proof
  rw [dominates_def, EXTENSION, IN_DEF] >>
  `(entry_ip ip1.f).f = ip1.f` by rw [entry_ip_def] >>
  metis_tac [good_path_weak]
QED

Theorem uses_good_ip:
  ∀r prog ip. r ∈ uses prog ip ⇒ mem ip.f (map fst prog)
Proof
  rw [uses_cases, IN_DEF, get_instr_cases] >>
  imp_res_tac ALOOKUP_MEM >>
  fs [LIST_TO_SET_MAP] >>
  metis_tac [FST]
QED

Theorem is_ssa_weak:
  ∀l d p. ~mem l (map fst p) ∧ is_ssa ((l,d)::p) ⇒ is_ssa p
Proof
  rw [is_ssa_def]
  >- (first_x_assum irule >> rw [PULL_EXISTS] >> metis_tac [assigns_weak])
  >- (
    drule uses_good_ip >> rw [] >>
    last_x_assum (qspec_then `ip1` mp_tac) >> rw [] >>
    `ip1.f ≠ l` by metis_tac [MEM_MAP] >>
    fs [uses_weak, assigns_weak2] >>
    first_x_assum drule >> rw [] >>
    qexists_tac `ip2` >> rw [] >> rfs [] >>
    metis_tac [assigns_weak2, dominates_weak])
  >- (
    `get_instr ((l,d)::p) ip i`
    by (fs [get_instr_cases, MEM_MAP] >> rw [] >> metis_tac [ALOOKUP_MEM, FST]) >>
    first_x_assum drule >> rw [reachable_def] >>
    qexists_tac `path` >> rw [] >>
    `(entry_ip ip.f).f ≠ l` suffices_by metis_tac [good_path_weak] >>
    rw [entry_ip_def] >> fs [get_instr_cases, MEM_MAP] >>
    CCONTR_TAC >> fs [] >> rw [] >>
    metis_tac [FST, ALOOKUP_MEM])
QED

Theorem loc_prog_ok_weak:
  ∀l d p. ~mem l (map fst p) ∧ loc_prog_ok ((l,d)::p) ⇒ loc_prog_ok p
Proof
  rw [loc_prog_ok_def, MEM_MAP] >>
  metis_tac [ALOOKUP_MEM, FST]
QED

Triviality in_uncurry:
  (x,y) ∈ UNCURRY R ⇔ R x y
Proof
  rw [IN_DEF, UNCURRY_DEF]
QED

Theorem sorted_all_distinct_idx:
  ∀R l. all_distinct l ∧ transitive R ∧ reflexive R ∧ antisym (rrestrict (UNCURRY R) (set l)) ⇒
    (SORTED R l ⇔ (∀i j. i < length l ∧ j < length l ⇒ (R (el i l) (el j l) ⇔ i ≤ j)))
Proof
  Induct_on `l` >> rw [sortingTheory.SORTED_EQ] >>
  `antisym (rrestrict (UNCURRY R) (set l))`
  by (fs [antisym_def] >> rw [in_rrestrict]) >>
  eq_tac >> rw []
  >- (
    Cases_on `i` >> Cases_on `j` >> rw [] >> fs []
    >- fs [relationTheory.reflexive_def]
    >- (first_x_assum irule >> rw [MEM_EL] >> metis_tac [])
    >- (
      fs [antisym_def] >>
      last_x_assum (qspecl_then [`h`, `el n l`] mp_tac) >>
      simp [in_rrestrict, in_uncurry] >> metis_tac [MEM_EL]))
  >- (first_x_assum (qspecl_then [`Suc i`, `Suc j`] mp_tac) >> rw [])
  >- (
    fs [MEM_EL] >> rw [] >>
    first_x_assum (qspecl_then [`0`, `Suc n`] mp_tac) >> rw [])
QED

Theorem lpc_uses_to_uses:
  ∀r bs lip prog f d.
    r ∈ lpc_uses (map snd bs) lip ∧
    alookup prog (Fn f) = Some d ∧
    PERM d.blocks bs ∧
    all_distinct (map fst (d.blocks))
    ⇒
    r ∈ uses prog <| f := Fn f; b := fst (el (fst lip) bs); i := snd lip |> ∧
    fst lip < length bs
Proof
  rw [IN_DEF, uses_cases, lpc_uses_cases, get_instr_cases, PULL_EXISTS,
      lpc_get_instr_cases] >>
  rw []
  >- (
    qexists_tac `el i' (map snd bs)` >> rw [] >>
    irule ALOOKUP_ALL_DISTINCT_MEM >> rw [] >>
    drule sortingTheory.MEM_PERM >> rw [MEM_EL, EL_MAP] >>
    metis_tac [pair_CASES, FST, SND])
  >- (
    qexists_tac `phis` >>
    qexists_tac `el i (map snd bs)` >> rw [] >>
    qexists_tac `s` >> rw [] >>
    irule ALOOKUP_ALL_DISTINCT_MEM >> rw [] >>
    drule sortingTheory.MEM_PERM >> rw [MEM_EL, EL_MAP] >>
    metis_tac [pair_CASES, FST, SND])
QED

Theorem assigns_to_block_assigns:
  ∀prog ip d b r l.
    r ∈ assigns prog ip ∧
    alookup prog ip.f = Some d ∧
    alookup d.blocks ip.b = Some b
    ⇒
    fst r ∈ block_assigns (l:label option, b)
Proof
  rw [] >>
  qpat_x_assum `_ ∈ assigns _ _` mp_tac >>
  simp [Once IN_DEF] >>
  rw [assigns_cases, get_instr_cases, PULL_EXISTS, block_assigns_def]
  >- (
    disj2_tac >>
    qexists_tac `r` >> HINT_EXISTS_TAC >>
    metis_tac [MEM_EL])
  >- (
    rw [header_assigns_def, MEM_MAP] >>
    disj1_tac >>
    fs [MEM_MAP] >> rw [] >>
    metis_tac [])
QED

Theorem assigns_to_lpc_assigns:
  ∀prog ip d r idx (bs : (label option # block) list).
    r ∈ assigns prog ip ∧
    alookup prog ip.f = Some d ∧
    alookup d.blocks ip.b = Some (snd (el idx bs)) ∧
    idx < length bs
    ⇒
    r ∈ lpc_assigns (map snd bs) (idx, ip.i)
Proof
  rw [assigns_cases, get_instr_cases, PULL_EXISTS, block_assigns_def,
      lpc_assigns_cases, IN_DEF, lpc_get_instr_cases] >>
  fs [] >> rw [] >> fs [] >> rw [EL_MAP]
QED

Theorem uses_to_block_uses:
  ∀prog ip d b r l.
    r ∈ uses prog ip ∧
    alookup prog ip.f = Some d ∧
    alookup d.blocks ip.b = Some b
    ⇒
    r ∈ block_uses (l:label option, b)
Proof
  rw [] >>
  qpat_x_assum `_ ∈ uses _ _` mp_tac >>
  simp [Once IN_DEF] >>
  rw [uses_cases, get_instr_cases, PULL_EXISTS, block_uses_def]
  >- (
    disj2_tac >>
    metis_tac [MEM_EL])
  >- (
    rw [header_uses_def, MEM_MAP] >>
    disj1_tac >>
    fs [MEM_MAP] >> rw [] >>
    metis_tac [])
QED

Theorem same_block_assigns_less_uses:
  reachable prog ip1 ∧
  dominates prog ip2 ip1 ∧
  ip1.b = ip2.b ∧
  (?i. get_instr prog ip1 i) ∧
  (∀fname dec.
       alookup prog fname = Some dec ⇒
       every (λb. fst b = None ⇔ (snd b).h = Entry) dec.blocks)
  ⇒
  bip_less ip2.i ip1.i
Proof
  rw [] >> CCONTR_TAC >> fs [] >>
  `ip1.f = ip2.f` by metis_tac [reachable_dominates_same_func] >>
  `ip_equiv ip1 ip2 ∨ bip_less ip1.i ip2.i` by metis_tac [bip_less_tri]
  >- metis_tac [ip_equiv_dominates2, dominates_irrefl, ip_equiv_sym] >>
  metis_tac [dominates_antisym, same_block_dominates]
QED

Theorem ssa_to_dominator_ordered_lem:
  ∀p1. loc_prog_ok p1 ∧ is_ssa p1 ∧ all_distinct (map fst p1) ∧ every (λ(l,d). all_distinct (map fst d.blocks)) p1 ⇒
    ∃p2. list_rel (\(l1,d1) (l2,d2). l1 = l2 ∧ PERM d1.blocks d2.blocks) p1 p2 ∧
         dominator_ordered p2
Proof
  Induct_on `p1` >> rw []
  >- rw [dominator_ordered_def] >>
  `is_ssa p1` by metis_tac [is_ssa_weak, pair_CASES, FST] >>
  `loc_prog_ok p1` by metis_tac [loc_prog_ok_weak, pair_CASES, FST] >>
  fs [PULL_EXISTS] >>
  rename1 `is_ssa (x::_)` >>
  `?l d. x = (l,d)` by metis_tac [pair_CASES] >>
  `partial_order (rc (block_order d.blocks) (set d.blocks)) (set d.blocks)`
  by metis_tac [block_order_po, ALOOKUP_def] >>
  rw [] >>
  `finite (set d.blocks)` by rw [] >>
  drule finite_linear_order_of_finite_po >>
  disch_then drule >> rw [] >>
  drule finite_linear_order_to_list >>
  disch_then drule >> rw [] >>
  rename1 `set _ = set bs` >>
  qexists_tac `(l, d with blocks := bs)` >>
  qexists_tac `p2` >>
  conj_asm1_tac >> rw [] >>
  fs []
  >- (irule sortingTheory.PERM_ALL_DISTINCT >> rw [] >> metis_tac [ALL_DISTINCT_MAP]) >>
  qmatch_assum_abbrev_tac `SORTED R _` >>
  `transitive R ∧ reflexive R ∧ antisym (rrestrict (UNCURRY R) (set bs))`
  by (
    fs [linear_order_def, transitive_def, reflexive_def, SUBSET_DEF,
        antisym_def, domain_def, range_def, in_rrestrict] >>
    rw [Abbr `R`, relationTheory.transitive_def, relationTheory.reflexive_def] >>
    fs [IN_DEF] >> metis_tac []) >>
  drule sorted_all_distinct_idx >>
  disch_then drule >> rw [] >>
  fs [dominator_ordered_def] >> rw [PULL_EXISTS] >> fs []
  >- (
    drule lpc_uses_to_uses >>
    disch_then (qspecl_then [`(Fn f,d)::p1`, `f`, `d`] mp_tac) >>
    rw [] >>
    qmatch_assum_abbrev_tac `_ ∈ uses prog ip1` >>
    `?ip2. ip2.f = Fn f ∧ r ∈ image fst (assigns prog ip2) ∧ dominates prog ip2 ip1`
    by (fs [is_ssa_def] >> last_x_assum drule >> rw [Abbr `ip1`]) >>
    `?idx. idx < length bs ∧ alookup d.blocks ip2.b = Some (snd (el idx bs)) ∧
      fst (el idx bs) = ip2.b`
    by (
      fs [assigns_cases, IN_DEF, get_instr_cases, Abbr `prog`] >>
      rfs [] >>
      drule ALOOKUP_MEM >>
      drule sortingTheory.MEM_PERM >>
      rw [MEM_EL] >>
      metis_tac [FST, SND]) >>
    `?t. (r, t) ∈ assigns prog ip2` by metis_tac [IN_IMAGE, pair_CASES, FST] >>
    drule assigns_to_block_assigns >> rw [Abbr `prog`] >>
    qexists_tac `(idx, ip2.i)` >>
    qexists_tac `(r, t)` >>
    rw []
    >- (
      `(el idx bs, el (fst lip1) bs) ∈ rc (block_order d.blocks) (set d.blocks)`
      by (
        simp [rc_def, block_order_def, EL_MEM] >>
        drule uses_to_block_uses >> simp [Abbr `ip1`] >>
        `alookup d.blocks (fst (el (fst lip1) bs)) = Some (snd (el (fst lip1) bs))`
        by (
          qmatch_goalsub_abbrev_tac `(fst b)` >>
          `mem b bs` by (rw [Abbr `b`, MEM_EL] >> metis_tac []) >>
          metis_tac [ALOOKUP_ALL_DISTINCT_MEM, PAIR]) >>
        rw [METIS_PROVE [] ``a ∨ b ⇔ ~b ⇒ a``] >>
        simp [Once tc_cases] >>
        disj1_tac >> rw [MEM_EL]
        >- (
          `all_distinct (map fst bs)`
          by (
            irule ALL_DISTINCT_MAP_INJ >> rw [] >>
            `mem x d.blocks ∧ mem y d.blocks` by metis_tac [sortingTheory.MEM_PERM] >>
            metis_tac [PAIR, ALOOKUP_ALL_DISTINCT_MEM, optionTheory.SOME_11]) >>
          metis_tac [PAIR, optionTheory.SOME_11]) >>
        metis_tac [PAIR, FST]) >>
      rw [] >>
      `idx ≤ fst lip1`
      by (
        `R (el idx bs) (el (fst lip1) bs)` suffices_by metis_tac [] >>
        fs [Abbr `R`, SUBSET_DEF, rc_def] >> metis_tac []) >>
      PairCases_on `lip1` >>
      rw [linear_pc_less_def, LEX_DEF_THM] >> fs [LESS_OR_EQ] >>
      rw [] >>
      `reachable ((Fn f,d)::p1) ip1`
      by (
        fs [is_ssa_def] >> first_x_assum irule >>
        fs [uses_cases, IN_DEF] >> metis_tac []) >>
      drule same_block_assigns_less_uses >>
      disch_then drule >> simp [Abbr `ip1`] >>
      disch_then irule >> rw [] >>
      fs [uses_cases, IN_DEF, loc_prog_ok_def] >>
      metis_tac [])
    >- (drule assigns_to_lpc_assigns >> simp []))
  >- metis_tac []
QED

Theorem prog_ok_to_loc_prog_ok:
  ∀p. prog_ok p ⇒ loc_prog_ok p
Proof
  rw [prog_ok_def, loc_prog_ok_def] >> metis_tac []
QED

Theorem alookup_perm_blocks:
  ∀p1 p2.
    list_rel (λ(l1,d1) (l2,d2). l1 = l2 ∧ PERM d1.blocks d2.blocks) p1 p2
    ⇒
    (∀l d. alookup p2 l = Some d ⇒ ?d'. alookup p1 l = Some d' ∧ PERM d'.blocks d.blocks) ∧
    (∀l d. alookup p1 l = Some d ⇒ ?d'. alookup p2 l = Some d' ∧ PERM d.blocks d'.blocks) ∧
    (map fst p1 = map fst p2)
Proof
  Induct_on `p1` >> rw [] >>
  pairarg_tac >> fs [] >> pairarg_tac >> fs [] >> rw [] >> rw [] >> fs [] >>
  metis_tac []
QED

Theorem alookup_perm:
  ∀b1 b2. PERM b1 b2 ∧ all_distinct (map fst b1) ⇒ alookup b1 = alookup b2
Proof
  rw [] >>
  irule ALOOKUP_ALL_DISTINCT_PERM_same >>
  rw [sortingTheory.PERM_MAP, sortingTheory.PERM_LIST_TO_SET]
QED

Theorem get_instr_perm_blocks:
  every (\(l,d). all_distinct (map fst d.blocks)) p1 ∧
  list_rel (λ(l1,d1) (l2,d2). l1 = l2 ∧ PERM d1.blocks d2.blocks) p1 p2
  ⇒
  get_instr p1 = get_instr p2
Proof
  rw [get_instr_cases, FUN_EQ_THM] >>
  eq_tac >> rw [] >>
  drule alookup_perm_blocks >> rw [] >>
  first_x_assum drule >> rw [] >> rw [] >>
  `all_distinct (map fst d.blocks)`
  by (
    fs [EVERY_MEM] >>
    imp_res_tac ALOOKUP_MEM >>
    res_tac >> fs [] >>
    metis_tac [sortingTheory.ALL_DISTINCT_PERM, sortingTheory.PERM_MAP]) >>
  metis_tac [alookup_perm, sortingTheory.PERM_SYM]
QED

Theorem next_ips_perm_blocks:
  every (\(l,d). all_distinct (map fst d.blocks)) p1 ∧
  list_rel (λ(l1,d1) (l2,d2). l1 = l2 ∧ PERM d1.blocks d2.blocks) p1 p2
  ⇒
  next_ips p1 = next_ips p2
Proof
  rw [FUN_EQ_THM, next_ips_cases] >> metis_tac [get_instr_perm_blocks]
QED

Theorem good_path_perm_blocks:
  ∀p1 p2.
    every (\(l,d). all_distinct (map fst d.blocks)) p1 ∧
    list_rel (λ(l1,d1) (l2,d2). l1 = l2 ∧ PERM d1.blocks d2.blocks) p1 p2
    ⇒
    good_path p1 = good_path p2
Proof
  rw [] >> simp [FUN_EQ_THM] >>
  Induct >> rw [] >>
  ONCE_REWRITE_TAC [good_path_cases] >> rw [] >>
  metis_tac [next_ips_perm_blocks, get_instr_perm_blocks]
QED

Theorem ssa_to_dominator_ordered:
  ∀p1.
    prog_ok p1 ∧ is_ssa p1
    ⇒
    ∃p2. list_rel (\(l1,d1) (l2,d2). l1 = l2 ∧ PERM d1.blocks d2.blocks) p1 p2 ∧
         dominator_ordered p2 ∧ prog_ok p2 ∧ is_ssa p2
Proof
  rw [] >>
  drule prog_ok_to_loc_prog_ok >> rw [] >>
  drule ssa_to_dominator_ordered_lem >> simp [] >>
  impl_tac
  >- (
    fs [prog_ok_def] >> rw [EVERY_MEM] >>
    pairarg_tac >> fs [] >> rw [] >> metis_tac [ALOOKUP_ALL_DISTINCT_MEM]) >>
  rw [] >>
  qexists_tac `p2` >> rw [] >>
  `every (λ(l,d). all_distinct (map fst d.blocks)) p1`
  by fs [prog_ok_def] >>
  drule get_instr_perm_blocks >> disch_then drule >> rw [] >>
  drule good_path_perm_blocks >> disch_then drule >> rw []
  >- (
    drule alookup_perm_blocks >> rw [] >>
    fs [prog_ok_def] >>
    conj_tac
    >- (
      rw [] >> rpt (last_x_assum (qspec_then `fname` mp_tac)) >> rw [] >>
      metis_tac [alookup_perm, sortingTheory.MEM_PERM, prog_ok_distinct_lem]) >>
    conj_tac >- metis_tac [alookup_perm, sortingTheory.MEM_PERM, prog_ok_distinct_lem] >>
    conj_tac >- (fs [EVERY_MEM] >> metis_tac [alookup_perm, sortingTheory.MEM_PERM]) >>
    conj_tac
    >- (
      rfs [LIST_REL_EL_EQN, EVERY_EL] >> rw [] >>
      pairarg_tac >> fs [] >>
      rpt (first_x_assum (qspec_then `n` mp_tac)) >> rw [] >>
      pairarg_tac >> fs [] >>
      metis_tac [sortingTheory.ALL_DISTINCT_PERM, sortingTheory.PERM_MAP]) >>
    conj_tac >- metis_tac [alookup_perm, sortingTheory.MEM_PERM]
    >- metis_tac [alookup_perm, sortingTheory.MEM_PERM])
  >- (
    fs [is_ssa_def, IN_DEF, uses_cases, assigns_cases, reachable_def, dominates_def] >>
    rw [] >>
    first_x_assum irule >> metis_tac [])
QED

export_theory ();
