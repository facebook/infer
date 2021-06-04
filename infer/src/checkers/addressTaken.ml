(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Domain = AbstractDomain.FiniteSet (struct
  include Pvar

  let pp = pp Pp.text
end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = unit

  let rec add_address_taken_pvars (exp : Exp.t) astate =
    match exp with
    | Lvar pvar ->
        Domain.add pvar astate
    | Cast (_, e) | UnOp (_, e, _) | Lfield (e, _, _) ->
        add_address_taken_pvars e astate
    | BinOp (_, e1, e2) | Lindex (e1, e2) ->
        add_address_taken_pvars e1 astate |> add_address_taken_pvars e2
    | Exn _ | Closure _ | Const (Cint _ | Cfun _ | Cstr _ | Cfloat _ | Cclass _) | Var _ | Sizeof _
      ->
        astate


  let exec_instr astate () _ _ (instr : Sil.instr) =
    match instr with
    | Store {typ= {desc= Tptr _}; e2= rhs_exp} ->
        add_address_taken_pvars rhs_exp astate
    | Call (_, _, actuals, _, _) ->
        let add_actual_by_ref astate_acc = function
          | actual_exp, {Typ.desc= Tptr _} ->
              add_address_taken_pvars actual_exp astate_acc
          | _ ->
              astate_acc
        in
        List.fold ~f:add_actual_by_ref ~init:astate actuals
    | Store _ | Load _ | Prune _ | Metadata _ ->
        astate


  let pp_session_name _node fmt = Format.pp_print_string fmt "address taken"
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (ProcCfg.Exceptional))
