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

module Summary = Summary.Make (struct
    type summary = SiofDomain.astate

    let update_payload astate payload =
      { payload with Specs.siof = Some astate }

    let read_from_payload payload =
      payload.Specs.siof
  end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = SiofDomain
  type extras = ProcData.no_extras

  let is_semantically_compile_constant tenv pdesc pv =
    match Pvar.get_initializer_pname pv with
    | Some pname -> (
        match Summary.read_summary tenv pdesc pname with
        | Some (Domain.NonBottom _) -> false
        | Some Domain.Bottom | None -> true
      )
    | None -> true

  let get_globals tenv astate pdesc loc e =
    let is_dangerous_global pv =
      Pvar.is_global pv
      && not (Pvar.is_compile_constant pv
              || is_semantically_compile_constant tenv pdesc pv) in
    let globals = Exp.get_vars e |> snd |> IList.filter is_dangerous_global in
    if globals = [] then
      Domain.Bottom
    else
      let sink_of_global global pname loc =
        let site = CallSite.make pname loc in
        SiofTrace.Sink.make global site in
      let pname = Procdesc.get_proc_name pdesc in
      let trace = match astate with
        | Domain.Bottom -> SiofTrace.initial
        | Domain.NonBottom t -> t in
      let globals_trace =
        IList.fold_left (fun trace_acc global ->
            SiofTrace.add_sink (sink_of_global global pname loc) trace_acc)
          trace
          globals in
      Domain.NonBottom globals_trace

  let add_params_globals astate tenv pdesc loc params =
    IList.map fst params
    |> IList.map (fun e -> get_globals tenv astate pdesc loc e)
    |> IList.fold_left Domain.join astate

  let at_least_bottom =
    Domain.join (Domain.NonBottom SiofTrace.initial)

  let exec_instr astate { ProcData.pdesc; tenv } _ (instr : Sil.instr) = match instr with
    | Load (_, exp, _, loc)
    | Store (_, _, exp, loc)
    | Prune (exp, loc, _, _) ->
        Domain.join astate (get_globals tenv astate pdesc loc exp)
    | Call (_, Const (Cfun callee_pname), params, loc, _) ->
        let callsite = CallSite.make callee_pname loc in
        let callee_globals =
          match Summary.read_summary tenv pdesc callee_pname with
          | Some (Domain.NonBottom trace) -> Domain.NonBottom (SiofTrace.to_callee trace callsite)
          | _ -> Domain.Bottom in
        add_params_globals astate tenv pdesc loc params
        |> Domain.join callee_globals
        |>
        (* make sure it's not Bottom: we made a function call so this needs initialization *)
        at_least_bottom
    | Call (_, _, params, loc, _) ->
        add_params_globals astate tenv pdesc loc params
        |>
        (* make sure it's not Bottom: we made a function call so this needs initialization *)
        at_least_bottom
    | Declare_locals _ | Remove_temps _ | Abstract _ | Nullify _ ->
        astate
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Normal)
    (Scheduler.ReversePostorder)
    (TransferFunctions)

module Interprocedural = Analyzer.Interprocedural (Summary)

let report_siof trace pdesc tenv loc =
  let trace_of_pname pname =
    match Summary.read_summary tenv pdesc pname with
    | Some (SiofDomain.NonBottom summary) -> summary
    | _ -> SiofTrace.initial in

  let pp_sink f sink =
    let pp_source f v = match Pvar.get_source_file v with
      | Some source_file when DB.source_file_equal DB.source_file_empty source_file ->
          F.fprintf f ""
      | None ->
          F.fprintf f ""
      | Some source_file ->
          F.fprintf f " from file %s" (DB.source_file_to_string source_file) in
    let v = SiofTrace.Sink.kind sink in
    F.fprintf f "%s%a" (Pvar.get_simplified_name v) pp_source v in

  let pp_path_part f path =
    let pp_path f path =
      let pp_sep f () = F.fprintf f " => " in
      let pp_elem f (sink, _) =
        CallSite.pp f (SiofTrace.Sink.call_site sink) in
      (F.pp_print_list ~pp_sep) pp_elem f path in
    if (IList.length path > 1)
    then F.fprintf f "Full path: %a" pp_path path in

  let report_one_path (_, path) =
    let final_sink = fst (IList.hd path) in
    let description =
      F.asprintf
        "This global variable initializer accesses the following globals in another translation \
         unit: %a. %a"
        pp_sink final_sink
        pp_path_part (IList.rev path) in
    let caller_pname = Procdesc.get_proc_name pdesc in
    let exn = Exceptions.Checkers
        ("STATIC_INITIALIZATION_ORDER_FIASCO", Localise.verbatim_desc description) in
    Reporting.log_error caller_pname ~loc exn in

  IList.iter report_one_path (SiofTrace.get_reportable_sink_paths trace ~trace_of_pname)

let siof_check pdesc tenv = function
  | Some (SiofDomain.NonBottom post) ->
      let attrs = Procdesc.get_attributes pdesc in
      let is_orig_file f = match attrs.ProcAttributes.translation_unit with
        | Some orig_file ->
            let orig_path = DB.source_file_to_abs_path orig_file in
            string_equal orig_path (DB.source_file_to_abs_path f)
        | None -> false in
      let is_foreign v = Option.map_default
          (fun f -> not (is_orig_file f)) false (Pvar.get_source_file v) in
      let foreign_global_sinks =
        SiofTrace.Sinks.filter
          (fun sink -> is_foreign (SiofTrace.Sink.kind sink))
          (SiofTrace.sinks post) in
      if not (SiofTrace.Sinks.is_empty foreign_global_sinks)
      then report_siof post pdesc tenv attrs.ProcAttributes.loc;
  | Some SiofDomain.Bottom | None ->
      ()

let checker ({ Callbacks.tenv; proc_desc } as callback) =
  let post = Interprocedural.checker callback ProcData.empty_extras in
  let pname = Procdesc.get_proc_name proc_desc in
  match pname with
  | Procname.C c when Procname.is_globals_initializer c ->
      siof_check proc_desc tenv post
  | _ ->
      ()
