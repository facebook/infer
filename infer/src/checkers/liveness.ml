(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format
module L = Logging

(** backward analysis for computing set of maybe-live variables at each program point *)

module Domain = AbstractDomain.FiniteSet(Var.Set)

(* compilers 101-style backward transfer functions for liveness analysis. gen a variable when it is
   read, kill the variable when it is assigned *)
module TransferFunctions = struct
  type astate = Domain.astate

  (* add all of the vars read in [exp] to the live set *)
  let exp_add_live exp astate =
    let (ids, pvars) = Sil.exp_get_vars exp in
    let astate' =
      IList.fold_left (fun astate_acc id -> Domain.add (LogicalVar id) astate_acc) astate ids in
    IList.fold_left (fun astate_acc pvar -> Domain.add (ProgramVar pvar) astate_acc) astate' pvars

  let exec_instr astate _ = function
    | Sil.Letderef (lhs_id, rhs_exp, _, _)
    | Sil.Set (Sil.Var lhs_id, _, rhs_exp, _) ->
        Domain.remove (LogicalVar lhs_id) astate
        |> exp_add_live rhs_exp
    | Sil.Set (Lvar lhs_pvar, _, rhs_exp, _) ->
        let astate' =
          if Pvar.is_global lhs_pvar
          then astate (* never kill globals *)
          else Domain.remove (ProgramVar lhs_pvar) astate in
        exp_add_live rhs_exp astate'
    | Sil.Set (lhs_exp, _, rhs_exp, _) ->
        exp_add_live lhs_exp astate
        |> exp_add_live rhs_exp
    | Sil.Prune (exp, _, _, _) | Sil.Goto_node (exp, _) ->
        exp_add_live exp astate
    | Sil.Call (ret_ids, call_exp, params, _, _) ->
        IList.fold_right
          (fun ret_id astate_acc -> Domain.remove (LogicalVar ret_id) astate_acc)
          ret_ids
          astate
        |> exp_add_live call_exp
        |> IList.fold_right exp_add_live (IList.map fst params)
    | Sil.Declare_locals _ | Stackop _ ->
        astate
    | Sil.Nullify _ | Remove_temps _ | Abstract _ ->
        failwith
          "Nullify, Remove_temps, and Abstract instructions should be added after running liveness"

end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Backward(ProcCfg.Forward))
    (Scheduler.ReversePostorder)
    (Domain)
    (TransferFunctions)

let checker { Callbacks.proc_desc; } = ignore(Analyzer.exec_pdesc proc_desc)
