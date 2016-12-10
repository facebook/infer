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

  let is_compile_time_constructed pdesc pv =
    let init_pname = Pvar.get_initializer_pname pv in
    match Option.bind init_pname (Summary.read_summary pdesc) with
    | Some Domain.Bottom ->
        (* we analyzed the initializer for this global and found that it doesn't require any runtime
           initialization so cannot participate in SIOF *)
        true
    | _ ->
        false

  let get_globals astate pdesc loc e =
    let is_dangerous_global pv =
      Pvar.is_global pv
      && not (Pvar.is_static_local pv)
      && not (Pvar.is_pod pv)
      && not (Pvar.is_compile_constant pv)
      && not (is_compile_time_constructed pdesc pv) in
    let globals = Exp.get_vars e |> snd |> IList.filter is_dangerous_global in
    if globals = [] then
      Domain.Bottom
    else
      let trace = match astate with
        | Domain.Bottom -> SiofTrace.initial
        | Domain.NonBottom t -> t in
      let globals_trace =
        IList.fold_left (fun trace_acc global ->
            SiofTrace.add_sink (SiofTrace.make_access global loc) trace_acc)
          trace
          globals in
      Domain.NonBottom globals_trace

  let add_params_globals astate pdesc loc params =
    IList.map fst params
    |> IList.map (fun e -> get_globals astate pdesc loc e)
    |> IList.fold_left Domain.join astate

  let at_least_bottom =
    Domain.join (Domain.NonBottom SiofTrace.initial)

  let exec_instr astate { ProcData.pdesc; } _ (instr : Sil.instr) = match instr with
    | Load (_, exp, _, loc)
    | Store (_, _, exp, loc)
    | Prune (exp, loc, _, _) ->
        Domain.join astate (get_globals astate pdesc loc exp)
    | Call (_, Const (Cfun callee_pname), _::params_without_self, loc, _)
      when Procname.is_c_method callee_pname && Procname.is_constructor callee_pname
           && Procname.is_constexpr callee_pname ->
        add_params_globals astate pdesc loc params_without_self
    | Call (_, Const (Cfun callee_pname), params, loc, _) ->
        let callsite = CallSite.make callee_pname loc in
        let callee_globals =
          match Summary.read_summary pdesc callee_pname with
          | Some (Domain.NonBottom trace) ->
              Domain.NonBottom (SiofTrace.with_callsite trace callsite)
          | _ ->
              Domain.Bottom in
        add_params_globals astate pdesc loc params
        |> Domain.join callee_globals
        |>
        (* make sure it's not Bottom: we made a function call so this needs initialization *)
        at_least_bottom
    | Call (_, _, params, loc, _) ->
        add_params_globals astate pdesc loc params
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


let is_foreign tu_opt v =
  let is_orig_file f = match tu_opt with
    | Some orig_file ->
        let orig_path = SourceFile.to_abs_path orig_file in
        String.equal orig_path (SourceFile.to_abs_path f)
    | None -> assert false in
  Option.value_map ~f:(fun f -> not (is_orig_file f)) ~default:false (Pvar.get_source_file v)

let report_siof trace pdesc gname loc =
  let tu_opt =
    let attrs = Procdesc.get_attributes pdesc in
    attrs.ProcAttributes.translation_unit in
  let trace_of_pname pname =
    match Summary.read_summary pdesc pname with
    | Some (SiofDomain.NonBottom summary) -> summary
    | _ -> SiofTrace.initial in

  let pp_sink f sink =
    let pp_source f v = match Pvar.get_source_file v with
      | Some source_file when not (SourceFile.equal SourceFile.empty source_file) ->
          F.fprintf f " from file %a" SourceFile.pp source_file
      | _ ->
          () in
    let v = SiofTrace.Sink.kind sink in
    F.fprintf f "%s%a" (Pvar.get_simplified_name v) pp_source v in

  let trace_of_error path =
    let desc_of_sink sink =
      let callsite = SiofTrace.Sink.call_site sink in
      if SiofTrace.is_intraprocedural_access sink then
        Format.asprintf "access to %s" (Pvar.get_simplified_name (SiofTrace.Sink.kind sink))
      else
        Format.asprintf "call to %a" Procname.pp (CallSite.pname callsite) in
    let sink_should_nest sink = not (SiofTrace.is_intraprocedural_access sink) in
    let trace_elem_of_global =
      Errlog.make_trace_element 0 loc
        (Format.asprintf "initialization of %s" gname)
        [] in
    trace_elem_of_global::(SiofTrace.to_sink_loc_trace ~desc_of_sink ~sink_should_nest path) in

  let report_one_path ((_, path) as sink_path) =
    let final_sink = fst (IList.hd path) in
    if is_foreign tu_opt (SiofTrace.Sink.kind final_sink) then (
      let description =
        F.asprintf
          "The initializer of %s accesses global variables in another translation unit: %a"
          gname
          pp_sink final_sink in
      let ltr = trace_of_error sink_path in
      let caller_pname = Procdesc.get_proc_name pdesc in
      let msg = Localise.to_string Localise.static_initialization_order_fiasco in
      let exn = Exceptions.Checkers (msg, Localise.verbatim_desc description) in
      Reporting.log_error caller_pname ~loc ~ltr exn
    ); in

  IList.iter report_one_path (SiofTrace.get_reportable_sink_paths trace ~trace_of_pname)

let siof_check pdesc gname = function
  | Some (SiofDomain.NonBottom post) ->
      let attrs = Procdesc.get_attributes pdesc in
      let foreign_global_sinks =
        SiofTrace.Sinks.filter
          (fun sink -> is_foreign attrs.ProcAttributes.translation_unit (SiofTrace.Sink.kind sink))
          (SiofTrace.sinks post) in
      if not (SiofTrace.Sinks.is_empty foreign_global_sinks)
      then report_siof post pdesc gname attrs.ProcAttributes.loc;
  | Some SiofDomain.Bottom | None ->
      ()

let checker ({ Callbacks.proc_desc; } as callback) =
  let post = Interprocedural.checker callback ProcData.empty_extras in
  let pname = Procdesc.get_proc_name proc_desc in
  match Procname.get_global_name_of_initializer pname with
  | Some gname ->
      siof_check proc_desc gname post
  | None ->
      ()
