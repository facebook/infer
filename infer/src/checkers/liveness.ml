(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format
module L = Logging

(** backward analysis for computing set of maybe-live variables at each program point *)

module Domain = AbstractDomain.FiniteSet(Var.Set)

(* compilers 101-style backward transfer functions for liveness analysis. gen a variable when it is
   read, kill the variable when it is assigned *)
module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain
  type extras = ProcData.no_extras

  (* add all of the vars read in [exp] to the live set *)
  let exp_add_live exp astate =
    let (ids, pvars) = Exp.get_vars exp in
    let astate' =
      IList.fold_left (fun astate_acc id -> Domain.add (Var.of_id id) astate_acc) astate ids in
    IList.fold_left (fun astate_acc pvar -> Domain.add (Var.of_pvar pvar) astate_acc) astate' pvars

  let exec_instr astate _ _ = function
    | Sil.Load (lhs_id, rhs_exp, _, _) ->
        Domain.remove (Var.of_id lhs_id) astate
        |> exp_add_live rhs_exp
    | Sil.Store (Lvar lhs_pvar, _, rhs_exp, _) ->
        let astate' =
          if Pvar.is_global lhs_pvar
          then astate (* never kill globals *)
          else Domain.remove (Var.of_pvar lhs_pvar) astate in
        exp_add_live rhs_exp astate'
    | Sil.Store (lhs_exp, _, rhs_exp, _) ->
        exp_add_live lhs_exp astate
        |> exp_add_live rhs_exp
    | Sil.Prune (exp, _, _, _) ->
        exp_add_live exp astate
    | Sil.Call (ret_id, call_exp, params, _, _) ->
        Option.value_map ~f:(fun (ret_id, _) -> Domain.remove (Var.of_id ret_id) astate)
          ~default:astate ret_id
        |> exp_add_live call_exp
        |> IList.fold_right exp_add_live (IList.map fst params)
    | Sil.Declare_locals _ | Remove_temps _ | Abstract _ | Nullify _ ->
        astate
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Backward(ProcCfg.Exceptional))
    (Scheduler.ReversePostorder)
    (TransferFunctions)
