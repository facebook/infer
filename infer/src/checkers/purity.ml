(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

let debug fmt = L.(debug Analysis Verbose fmt)

(* A simple purity checker *)

module Payload = SummaryPayload.Make (struct
  type t = PurityDomain.summary

  let update_payloads post (payloads : Payloads.t) = {payloads with purity= Some post}

  let of_payloads (payloads : Payloads.t) = payloads.purity
end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = PurityDomain.Pure

  type extras = ProcData.no_extras

  let rec is_heap_access ae =
    match (ae : AccessExpression.t) with
    | FieldOffset _ | ArrayOffset _ ->
        true
    | Dereference ae | AddressOf ae ->
        is_heap_access ae
    | Base _ ->
        false


  let exec_instr (astate : Domain.astate) {ProcData.pdesc; tenv} _ (instr : HilInstr.t) =
    if astate then
      match instr with
      | Assign (ae, _, _) ->
          not (is_heap_access ae)
      | Call (_, Direct called_pname, _, _, _) -> (
        match InvariantModels.Call.dispatch tenv called_pname [] with
        | Some inv ->
            InvariantModels.is_invariant inv
        | None ->
            Payload.read pdesc called_pname
            |> Option.value_map ~default:false ~f:(fun summary ->
                   debug "Reading from %a \n" Typ.Procname.pp called_pname ;
                   summary ) )
      | Call (_, Indirect _, _, _, _) ->
          (* This should never happen in Java. Fail if it does. *)
          L.(die InternalError) "Unexpected indirect call %a" HilInstr.pp instr
      | _ ->
          astate
    else astate


  let pp_session_name _node fmt = F.pp_print_string fmt "purity checker"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (ProcCfg.Normal) (TransferFunctions)

let should_report is_pure pdesc =
  match Procdesc.get_proc_name pdesc with
  | Typ.Procname.Java java_pname as proc_name ->
      is_pure
      && (not (Typ.Procname.is_constructor proc_name))
      && (not (Typ.Procname.Java.is_class_initializer java_pname))
      && not (Typ.Procname.Java.is_access_method java_pname)
  | _ ->
      L.(die InternalError "Not supposed to run on non-Java code.")


let checker {Callbacks.tenv; summary; proc_desc} : Summary.t =
  let initial = true in
  let proc_name = Procdesc.get_proc_name proc_desc in
  let proc_data = ProcData.make_default proc_desc tenv in
  let report_pure () =
    let loc = Procdesc.get_loc proc_desc in
    let exp_desc = F.asprintf "Side-effect free function %a" Typ.Procname.pp proc_name in
    let ltr = [Errlog.make_trace_element 0 loc exp_desc []] in
    Reporting.log_error summary ~loc ~ltr IssueType.pure_function exp_desc
  in
  match Analyzer.compute_post proc_data ~initial with
  | Some is_pure ->
      if should_report is_pure proc_desc then report_pure () ;
      Payload.update_summary is_pure summary
  | None ->
      L.internal_error "Analyzer failed to compute purity information for %a@." Typ.Procname.pp
        proc_name ;
      summary
