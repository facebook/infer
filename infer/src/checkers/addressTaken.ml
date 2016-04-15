(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module PvarSet = PrettyPrintable.MakePPSet(struct
    type t = Pvar.t
    let compare = Pvar.compare
    let pp_element = (Pvar.pp pe_text)
  end)

module Domain = AbstractDomain.FiniteSet(PvarSet)

module TransferFunctions = struct
  type astate = Domain.astate

  let rec add_address_taken_pvars exp astate = match exp with
    | Sil.Lvar pvar ->
        Domain.add pvar astate
    | Sil.Cast (_, e) | UnOp (_, e, _) | Lfield (e, _, _) ->
        add_address_taken_pvars e astate
    | Sil.BinOp (_, e1, e2) | Lindex (e1, e2) ->
        add_address_taken_pvars e1 astate
        |> add_address_taken_pvars e2
    | Sil.Const (Cclosure _ | Cint _ | Cfun _ | Cstr _ | Cfloat _ | Cattribute _ | Cexn _ | Cclass _
                | Cptr_to_fld _)
    | Var _ | Sizeof _ ->
        astate

  let exec_instr astate _ = function
    | Sil.Set (_, Tptr _, rhs_exp, _) ->
        add_address_taken_pvars rhs_exp astate
    | Sil.Call (_, _, actuals, _, _) ->
        let add_actual_by_ref astate_acc = function
          | actual_exp, Sil.Tptr _ -> add_address_taken_pvars actual_exp astate_acc
          | _ -> astate_acc in
        IList.fold_left add_actual_by_ref astate actuals
    | Sil.Set _ | Letderef _ | Prune _ | Nullify _ | Abstract _ | Remove_temps _ | Stackop _
    | Declare_locals _ ->
        astate

end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Exceptional) (Scheduler.ReversePostorder) (Domain) (TransferFunctions)
