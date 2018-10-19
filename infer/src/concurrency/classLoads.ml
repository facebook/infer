(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module Payload = SummaryPayload.Make (struct
  type t = ClassLoadsDomain.summary

  let update_payloads post (payloads : Payloads.t) = {payloads with class_loads= Some post}

  let of_payloads (payloads : Payloads.t) = payloads.class_loads
end)

module TransferFunctions = struct
  module CFG = ProcCfg.Normal
  module Domain = ClassLoadsDomain

  type extras = unit

  type instr = Sil.instr

  let exec_instr (astate : Domain.astate) {ProcData.pdesc} _ (instr : instr) =
    match instr with
    | Call (_, Const (Cfun callee), _, loc, _) ->
        Payload.read pdesc callee
        |> Option.fold ~init:astate ~f:(Domain.integrate_summary callee loc)
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "class loads"
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions)

let analyze_procedure {Callbacks.proc_desc; tenv; summary} =
  let initial = ClassLoadsDomain.empty in
  let proc_data = ProcData.make proc_desc tenv () in
  Analyzer.compute_post proc_data ~initial
  |> Option.value_map ~default:summary ~f:(fun astate -> Payload.update_summary astate summary)
