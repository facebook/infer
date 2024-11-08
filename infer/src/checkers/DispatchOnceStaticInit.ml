(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module Mem = struct
  type t = {loc: Location.t} [@@deriving compare]

  let pp fmt {loc} = F.fprintf fmt "calls_dispatch_once at %a" Location.pp loc
end

module Domain = struct
  include AbstractDomain.FiniteSet (Mem)

  let add_calls_dispatch_once loc astate = add {Mem.loc} astate
end

module TransferFunctions = struct
  module Domain = Domain
  module CFG = ProcCfg.Normal

  type analysis_data = IntraproceduralAnalysis.t

  let pp_session_name _node fmt = F.pp_print_string fmt "DispatchOnceStaticInit"

  let exec_instr (astate : Domain.t) _ _cfg_node _ (instr : Sil.instr) =
    match instr with
    | Call (_, Exp.Const (Const.Cfun procname), _, loc, _) ->
        let calls_dispatch_once = String.equal "_dispatch_once" (Procname.get_method procname) in
        if calls_dispatch_once then Domain.add_calls_dispatch_once loc astate else astate
    | _ ->
        astate
end

module Analyzer = AbstractInterpreter.MakeWTO (TransferFunctions)

let make_trace loc =
  let trace_elem = Errlog.make_trace_element 0 loc "Call to `dispatch_once` here" [] in
  [trace_elem]


let report_issue proc_desc err_log {Mem.loc} =
  let ltr = make_trace loc in
  let message =
    F.asprintf
      "There is a call to `disptach_once` at line %a from a static constructor. This could cause a \
       deadlock."
      Location.pp loc
  in
  Reporting.log_issue proc_desc err_log ~ltr ~loc DispatchOnceStaticInit
    IssueType.dispatch_once_in_static_init message


let checker ({IntraproceduralAnalysis.proc_desc; err_log} as analysis_data) =
  let attributes = Procdesc.get_attributes proc_desc in
  if attributes.ProcAttributes.is_static_ctor then
    let initial = Domain.empty in
    match Analyzer.compute_post analysis_data ~initial proc_desc with
    | Some domain -> (
      match Domain.choose_opt domain with
      | Some mem ->
          report_issue proc_desc err_log mem
      | None ->
          () )
    | None ->
        ()
