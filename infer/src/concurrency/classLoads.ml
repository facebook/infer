(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

module Payload = SummaryPayload.Make (struct
  type t = ClassLoadsDomain.summary

  let update_payloads post (payloads : Payloads.t) = {payloads with class_loads= Some post}

  let of_payloads (payloads : Payloads.t) = payloads.class_loads
end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ClassLoadsDomain

  type extras = unit

  let exec_instr (astate : Domain.astate) {ProcData.pdesc} _ (instr : HilInstr.t) =
    match instr with
    | Call (_, Direct callee, _, _, loc) ->
        Payload.read pdesc callee
        |> Option.value_map ~default:astate ~f:(Domain.integrate_summary astate callee loc)
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "class loads"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (ProcCfg.Normal) (TransferFunctions)

let die_if_not_java proc_desc =
  let is_java =
    Procdesc.get_proc_name proc_desc |> Typ.Procname.get_language |> Language.(equal Java)
  in
  if not is_java then L.(die InternalError "Not supposed to run on non-Java code yet.")


let analyze_procedure {Callbacks.proc_desc; tenv; summary} =
  die_if_not_java proc_desc ;
  let initial = ClassLoadsDomain.empty in
  let proc_data = ProcData.make proc_desc tenv () in
  Analyzer.compute_post proc_data ~initial
  |> Option.value_map ~default:summary ~f:(fun astate -> Payload.update_summary astate summary)
