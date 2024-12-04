(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

module Mem = struct
  type kind = [`Call | `Dispatch_once] [@@deriving compare, equal]

  type trace_elem = {call_site: CallSite.t; kind: kind} [@@deriving compare, equal]

  type t = trace_elem list [@@deriving compare, equal]

  let pp_trace_elem fmt {call_site} = F.fprintf fmt "call to %a" CallSite.pp call_site

  let pp fmt trace = (Pp.comma_seq pp_trace_elem) fmt trace

  let add_call call trace_elems = call :: trace_elems
end

module Summary = struct
  include AbstractDomain.FiniteSet (Mem)

  let add_call t astate =
    if is_empty astate then add (Mem.add_call t []) astate else map (Mem.add_call t) astate
end

module TransferFunctions = struct
  module Domain = Summary
  module CFG = ProcCfg.Normal

  type analysis_data = Summary.t InterproceduralAnalysis.t

  let pp_session_name _node fmt = F.pp_print_string fmt "StaticConstructorStallChecker"

  let exec_instr (astate : Summary.t) {InterproceduralAnalysis.analyze_dependency} _cfg_node _
      (instr : Sil.instr) =
    match instr with
    | Call (_, Exp.Const (Const.Cfun procname), _, loc, _) ->
        (* Any time static constructor calls dispatch_async then thread execution changes and calling dispatch_once is not an issue. *)
        if String.equal "dispatch_async" (Procname.get_method procname) then astate
        else
          let call_site = CallSite.make procname loc in
          let astate =
            match analyze_dependency procname with
            | Ok summary ->
                L.d_printf "Applying summary of callee `%a`@\n" Procname.pp procname ;
                L.d_printf "Summary: %a @\n" Summary.pp summary ;
                if not (Summary.is_empty summary) then
                  let astate' = Summary.add_call {call_site; kind= `Call} summary in
                  Summary.join astate astate'
                else astate
            | Error _ ->
                astate
          in
          let calls_dispatch_once = String.equal "_dispatch_once" (Procname.get_method procname) in
          if calls_dispatch_once then Summary.add_call {call_site; kind= `Dispatch_once} astate
          else astate
    | _ ->
        astate
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let make_trace trace_elems {Mem.call_site} =
  let loc = CallSite.loc call_site in
  let procname = CallSite.pname call_site in
  let trace_elem =
    let s = F.asprintf "Call to %a" Procname.pp procname in
    Errlog.make_trace_element 0 loc s []
  in
  trace_elem :: trace_elems


let report_issue proc_desc err_log trace_elems =
  let ltr = List.fold ~f:make_trace ~init:[] trace_elems |> List.rev in
  let {Mem.call_site; kind} = List.hd_exn trace_elems in
  let loc = CallSite.loc call_site in
  let kind_to_string kind = match kind with `Call -> "an indirect" | `Dispatch_once -> "a" in
  let message =
    F.asprintf
      "There is %s call to `disptach_once` at line %a from a static constructor. This could cause \
       a deadlock."
      (kind_to_string kind) Location.pp loc
  in
  Reporting.log_issue proc_desc err_log ~ltr ~loc StaticConstructorStallChecker
    IssueType.static_constructor_stall message


let checker ({InterproceduralAnalysis.proc_desc; err_log} as analysis_data) =
  let attributes = Procdesc.get_attributes proc_desc in
  let initial = Summary.empty in
  let summary_opt = Analyzer.compute_post analysis_data ~initial proc_desc in
  if attributes.ProcAttributes.is_static_ctor then
    match summary_opt with
    | Some domain -> (
      match Summary.choose_opt domain with
      | Some mem ->
          report_issue proc_desc err_log mem ;
          None
      | None ->
          summary_opt )
    | None ->
        summary_opt
  else summary_opt
