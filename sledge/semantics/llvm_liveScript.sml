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
  inc_pc ip = ip with i := ip.i + 1
End

(* The set of program counters the given instruction and starting point can
 * immediately reach, withing a function *)
Definition next_ips_def:
  (next_ips (Ret _) ip = {}) ∧
  (next_ips (Br _ l1 l2) ip =
    { <| f := ip.f; b := Some l; i := 0 |> | l | l ∈ {l1; l2} }) ∧
  (next_ips (Invoke _ _ _ _ l1 l2) ip =
    { <| f := ip.f; b := Some l; i := 0 |> | l | l ∈ {l1; l2} }) ∧
  (next_ips Unreachable ip = {}) ∧
  (next_ips (Sub _ _ _ _ _ _) ip = { inc_pc ip }) ∧
  (next_ips (Extractvalue _ _ _) ip = { inc_pc ip }) ∧
  (next_ips (Insertvalue _ _ _ _) ip = { inc_pc ip }) ∧
  (next_ips (Alloca _ _ _) ip = { inc_pc ip }) ∧
  (next_ips (Load _ _ _) ip = { inc_pc ip }) ∧
  (next_ips (Store _ _) ip = { inc_pc ip }) ∧
  (next_ips (Gep _ _ _ _) ip = { inc_pc ip }) ∧
  (next_ips (Ptrtoint _ _ _) ip = { inc_pc ip }) ∧
  (next_ips (Inttoptr _ _ _) ip = { inc_pc ip }) ∧
  (next_ips (Icmp _ _ _ _ _) ip = { inc_pc ip }) ∧
  (next_ips (Call _ _ _ _) ip = { inc_pc ip }) ∧
  (next_ips (Cxa_allocate_exn _ _) ip = { inc_pc ip }) ∧
  (* TODO: revisit throw when dealing with exceptions *)
  (next_ips (Cxa_throw _ _ _) ip = {  }) ∧
  (next_ips (Cxa_begin_catch _ _) ip = { inc_pc ip }) ∧
  (next_ips (Cxa_end_catch) ip = { inc_pc ip }) ∧
  (next_ips (Cxa_get_exception_ptr _ _) ip = { inc_pc ip })
End

(* The path is a list of program counters that represent a statically feasible
 * path through a function *)
Inductive good_path:
  (∀prog. good_path prog []) ∧

  (∀ip i.
    get_instr prog ip i
    ⇒
    good_path prog [ip]) ∧

  (∀prog path ip1 i1 ip2.
    get_instr prog ip1 i1 ∧
    ip2 ∈ next_ips i1 ip1 ∧
    good_path prog (ip2::path)
    ⇒
    good_path prog (ip1::ip2::path))
End

Definition arg_to_regs_def:
  (arg_to_regs (Constant _) = {}) ∧
  (arg_to_regs (Variable r) = {r})
End

(* The registers that an instruction uses *)
Definition uses_def:
  (uses (Ret (_, a)) = arg_to_regs a) ∧
  (uses (Br a _ _) = arg_to_regs a) ∧
  (uses (Invoke _ _ a targs _ _) =
    arg_to_regs a ∪ BIGUNION (set (map (arg_to_regs o snd) targs))) ∧
  (uses Unreachable = {}) ∧
  (uses (Sub _ _ _ _ a1 a2) =
    arg_to_regs a1 ∪ arg_to_regs a2) ∧
  (uses (Extractvalue _ (_, a) _) = arg_to_regs a) ∧
  (uses (Insertvalue _ (_, a1) (_, a2) _) =
    arg_to_regs a1 ∪ arg_to_regs a2) ∧
  (uses (Alloca _ _ (_, a)) = arg_to_regs a) ∧
  (uses (Load _ _ (_, a)) = arg_to_regs a) ∧
  (uses (Store (_, a1) (_, a2)) =
    arg_to_regs a1 ∪ arg_to_regs a2) ∧
  (uses (Gep _ _ (_, a) targs) =
    arg_to_regs a ∪ BIGUNION (set (map (arg_to_regs o snd) targs))) ∧
  (uses (Ptrtoint _ (_, a) _) = arg_to_regs a) ∧
  (uses (Inttoptr _ (_, a) _) = arg_to_regs a) ∧
  (uses (Icmp _ _ _ a1 a2) =
    arg_to_regs a1 ∪ arg_to_regs a2) ∧
  (uses (Call _ _ _ targs) =
    BIGUNION (set (map (arg_to_regs o snd) targs))) ∧
  (uses (Cxa_allocate_exn _ a) = arg_to_regs a) ∧
  (uses (Cxa_throw a1 a2 a3) =
    arg_to_regs a1 ∪ arg_to_regs a2 ∪ arg_to_regs a3) ∧
  (uses (Cxa_begin_catch _ a) = arg_to_regs a) ∧
  (uses (Cxa_end_catch) = {  }) ∧
  (uses (Cxa_get_exception_ptr _ a) = arg_to_regs a)
End

(* The registers that an instruction assigns *)
Definition assigns_def:
  (assigns (Invoke r _ _ _ _ _) = {r}) ∧
  (assigns (Sub r _ _ _ _ _) = {r}) ∧
  (assigns (Extractvalue r _ _) = {r}) ∧
  (assigns (Insertvalue r _ _ _) = {r}) ∧
  (assigns (Alloca r _ _) = {r}) ∧
  (assigns (Load r _ _) = {r}) ∧
  (assigns (Gep r _ _ _) = {r}) ∧
  (assigns (Ptrtoint r _ _) = {r}) ∧
  (assigns (Inttoptr r _ _) = {r}) ∧
  (assigns (Icmp r _ _ _ _) = {r}) ∧
  (assigns (Call r _ _ _) = {r}) ∧
  (assigns (Cxa_allocate_exn r _) = {r}) ∧
  (assigns (Cxa_begin_catch r _) = {r}) ∧
  (assigns (Cxa_get_exception_ptr r _) = {r}) ∧
  (assigns _ = {})
End

Definition live_def:
  live prog ip ⇔
    { r | ∃path instr.
            good_path prog (ip::path) ∧
            get_instr prog (last (ip::path)) instr ∧
            r ∈ uses instr ∧
            ∀ip2 instr2. ip2 ∈ set (front (ip::path)) ∧ get_instr prog ip2 instr2 ⇒ r ∉ assigns instr2 }
End

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

Triviality set_rw:
  !s P. (!x. x ∈ s ⇔ P x) ⇔ s = P
Proof
  rw [] >> eq_tac >> rw [IN_DEF] >> metis_tac []
QED

Theorem live_gen_kill:
  ∀prog ip instr ip'.
    get_instr prog ip instr
    ⇒
    live prog ip = BIGUNION {live prog ip' | ip' ∈ next_ips instr ip} DIFF assigns instr ∪ uses instr
Proof
  rw [live_def, EXTENSION] >> eq_tac >> rw []
  >- (
    Cases_on `path` >> fs []
    >- metis_tac [get_instr_func] >>
    rename1 `ip::ip2::path` >>
    qpat_x_assum `good_path _ _` mp_tac >> simp [Once good_path_cases] >> rw [] >>
    Cases_on `x ∈ uses instr` >> fs [] >> simp [set_rw, PULL_EXISTS] >>
    qexists_tac `ip2` >> qexists_tac `path` >> qexists_tac `instr'` >> rw [] >>
    metis_tac [get_instr_func])
  >- (
    fs [] >>
    qexists_tac `ip'::path` >> qexists_tac `instr'` >> rw []
    >- (simp [Once good_path_cases] >> metis_tac []) >>
    metis_tac [get_instr_func])
  >- (
    qexists_tac `[]` >> qexists_tac `instr` >> rw [] >>
    simp [Once good_path_cases] >>
    metis_tac [])
QED

export_theory ();
