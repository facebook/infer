(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format
module L = Logging

module Domain = struct
  type astate = Var.t Var.Map.t

  let initial = Var.Map.empty

  let is_bottom _ = false

  (* return true if the key-value bindings in [rhs] are a subset of the key-value bindings in
     [lhs] *)
  let (<=) ~lhs ~rhs =
    if lhs == rhs
    then true
    else
      Var.Map.for_all
        (fun k v ->
           try Var.var_equal v (Var.Map.find k lhs)
           with Not_found -> false)
        rhs

  let join astate1 astate2 =
    if astate1 == astate2
    then astate1
    else
      let keep_if_same _ v1_opt v2_opt = match v1_opt, v2_opt with
        | Some v1, Some v2 ->
            if Var.var_equal v1 v2 then Some v1 else None
        | _ -> None in
      Var.Map.merge keep_if_same astate1 astate2

  let widen ~prev ~next ~num_iters:_=
    join prev next

  let pp fmt astate =
    let pp_value = Var.pp_var in
    Var.Map.pp ~pp_value fmt astate

  let gen var1 var2 astate =
    (* don't add tautological copies *)
    if Var.var_equal var1 var2
    then astate
    else Var.Map.add var1 var2 astate

  let kill_copies_with_var var astate =
    let do_kill lhs_var rhs_var astate_acc =
      if Var.var_equal var lhs_var
      then astate_acc (* kill copies vith [var] on lhs *)
      else
      if Var.var_equal var rhs_var
      then (* delete [var] = [rhs_var] copy, but add [lhs_var] = M(rhs_var) copy*)
        try Var.Map.add lhs_var (Var.Map.find rhs_var astate) astate_acc
        with Not_found -> astate_acc
      else (* copy is unaffected by killing [var]; keep it *)
        Var.Map.add lhs_var rhs_var astate_acc in
    Var.Map.fold do_kill astate Var.Map.empty

  (* kill the previous binding for [lhs_var], and add a new [lhs_var] -> [rhs_var] binding *)
  let kill_then_gen lhs_var rhs_var astate =
    let already_has_binding lhs_var rhs_var astate =
      try Var.var_equal rhs_var (Var.Map.find lhs_var astate)
      with Not_found -> false in
    if Var.var_equal lhs_var rhs_var || already_has_binding lhs_var rhs_var astate
    then astate (* already have this binding; no need to kill or gen *)
    else
      kill_copies_with_var lhs_var astate
      |> gen lhs_var rhs_var
end

module TransferFunctions = struct

  type astate = Domain.astate

  let exec_instr astate _ = function
    | Sil.Letderef (lhs_id, Sil.Var rhs_id, _, _) ->
        (* note: logical vars are SSA, don't need to worry about overwriting existing bindings *)
        Domain.gen (LogicalVar lhs_id) (LogicalVar rhs_id) astate
    | Sil.Letderef (lhs_id, Sil.Lvar rhs_pvar, _, _) when not (Pvar.is_global rhs_pvar) ->
        Domain.gen (LogicalVar lhs_id) (ProgramVar rhs_pvar) astate
    | Sil.Set (Sil.Lvar lhs_pvar, _, Sil.Var rhs_id, _) when not (Pvar.is_global lhs_pvar) ->
        Domain.kill_then_gen (ProgramVar lhs_pvar) (LogicalVar rhs_id) astate
    | Sil.Set (Sil.Lvar lhs_pvar, _, Sil.Lvar rhs_pvar, _)
      when not (Pvar.is_global lhs_pvar || Pvar.is_global rhs_pvar)  ->
        Domain.kill_then_gen (ProgramVar lhs_pvar) (ProgramVar rhs_pvar) astate
    | Sil.Letderef (lhs_id, _, _, _) ->
        (* non-copy assignment (or assignment to global); can only kill *)
        Domain.kill_copies_with_var (LogicalVar lhs_id) astate
    | Sil.Set (Sil.Lvar lhs_pvar, _, _, _) ->
        (* non-copy assignment (or assignment to global); can only kill *)
        Domain.kill_copies_with_var (ProgramVar lhs_pvar) astate
    | Sil.Call (ret_ids, _, actuals, _, _) ->
        let kill_ret_ids astate_acc id =
          Domain.kill_copies_with_var (LogicalVar id) astate_acc in
        let kill_actuals_by_ref astate_acc = function
          | (Sil.Lvar pvar, Sil.Tptr _) -> Domain.kill_copies_with_var (ProgramVar pvar) astate_acc
          | _ -> astate_acc in
        let astate' = IList.fold_left kill_ret_ids astate ret_ids in
        if !Config.curr_language = Config.Java
        then astate' (* Java doesn't have pass-by-reference *)
        else IList.fold_left kill_actuals_by_ref astate' actuals
    | Sil.Set (Sil.Var _, _, _, _) ->
        (* this should never happen *)
        assert false
    | Sil.Set _ | Sil.Prune _ | Sil.Nullify _ | Sil.Abstract _ | Sil.Remove_temps _
    | Sil.Declare_locals _ | Sil.Stackop _ ->
        (* none of these can assign to program vars or logical vars *)
        astate
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Exceptional)
    (Scheduler.ReversePostorder)
    (Domain)
    (TransferFunctions)

let checker { Callbacks.proc_desc; tenv; } =
  ignore(Analyzer.exec_pdesc (ProcData.make proc_desc tenv))
