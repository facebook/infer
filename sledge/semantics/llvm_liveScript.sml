(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Proofs about a shallowly embedded concept of live variables *)

open HolKernel boolLib bossLib Parse;
open pred_setTheory;
open settingsTheory miscTheory llvmTheory llvm_propTheory;

new_theory "llvm_live";

numLib.prefer_num ();

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
  (instr_next_ips (Sub _ _ _ _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Extractvalue _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Insertvalue _ _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Alloca _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Load _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Store _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Gep _ _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Ptrtoint _ _ _) ip = { inc_pc ip }) ∧
  (instr_next_ips (Inttoptr _ _ _) ip = { inc_pc ip }) ∧
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
 (∀prog ip i l.
    get_instr prog ip (Inl i) ∧
    l ∈ instr_next_ips i ip
    ⇒
    next_ips prog ip l) ∧
 (∀prog ip from_l phis.
    get_instr prog ip (Inr (from_l, phis))
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
  (instr_uses (Ptrtoint _ (_, a) _) = arg_to_regs a) ∧
  (instr_uses (Inttoptr _ (_, a) _) = arg_to_regs a) ∧
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
  (instr_assigns (Ptrtoint r _ _) = {r}) ∧
  (instr_assigns (Inttoptr r _ _) = {r}) ∧
  (instr_assigns (Icmp r _ _ _ _) = {r}) ∧
  (instr_assigns (Call r _ _ _) = {r}) ∧
  (instr_assigns (Cxa_allocate_exn r _) = {r}) ∧
  (instr_assigns (Cxa_begin_catch r _) = {r}) ∧
  (instr_assigns (Cxa_get_exception_ptr r _) = {r}) ∧
  (instr_assigns _ = {})
End

Definition phi_assigns_def:
  phi_assigns (Phi r _ _) = {r}
End

Inductive assigns:
 (∀prog ip i r.
    get_instr prog ip (Inl i) ∧
    r ∈ instr_assigns i
    ⇒
    assigns prog ip r) ∧
 (∀prog ip from_l phis r.
    get_instr prog ip (Inr (from_l, phis)) ∧
    r ∈ BIGUNION (set (map phi_assigns phis))
    ⇒
    assigns prog ip r)
End

Definition live_def:
  live prog ip =
    { r | ∃path.
            good_path prog (ip::path) ∧
            r ∈ uses prog (last (ip::path)) ∧
            ∀ip2. ip2 ∈ set (front (ip::path)) ⇒ r ∉ assigns prog ip2 }
End

(*
Theorem get_instr_live:
  ∀prog ip instr.
    get_instr prog ip instr
    ⇒
    uses instr ⊆ live prog ip
Proof
  rw [live_def, SUBSET_DEF] >>
  qexists_tac `[]` >> rw [Once good_path_cases] >>
  qexists_tac `instr` >> simp [] >> metis_tac [IN_DEF]
QED
*)

Triviality set_rw:
  !s P. (!x. x ∈ s ⇔ P x) ⇔ s = P
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

export_theory ();
