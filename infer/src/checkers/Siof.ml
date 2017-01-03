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

module GlobalsAccesses = SiofTrace.GlobalsAccesses

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

  let get_globals pdesc loc e =
    let is_dangerous_global pv =
      Pvar.is_global pv
      && not (Pvar.is_static_local pv)
      && not (Pvar.is_pod pv)
      && not (Pvar.is_compile_constant pv)
      && not (is_compile_time_constructed pdesc pv) in
    let globals_accesses =
      Exp.get_vars e |> snd |> IList.filter is_dangerous_global
      |> IList.map (fun v -> (v, loc)) in
    GlobalsAccesses.of_list globals_accesses

  let add_globals astate outer_loc globals =
    if GlobalsAccesses.is_empty globals then
      astate
    else
      let trace = match astate with
        | Domain.Bottom -> SiofTrace.empty
        | Domain.NonBottom t -> t in
      let globals_trace =
        SiofTrace.add_sink (SiofTrace.make_access globals outer_loc) trace in
      Domain.NonBottom globals_trace

  let add_params_globals astate pdesc call_loc params =
    IList.map (fun (e, _) -> get_globals pdesc call_loc e) params
    |> IList.fold_left GlobalsAccesses.union GlobalsAccesses.empty
    |> add_globals astate (Procdesc.get_loc pdesc)

  let at_least_nonbottom =
    Domain.join (Domain.NonBottom SiofTrace.empty)

  let exec_instr astate { ProcData.pdesc; } _ (instr : Sil.instr) =
    match instr with
    | Load (_, exp, _, loc)
    | Store (_, _, exp, loc)
    | Prune (exp, loc, _, _) ->
        let proc_loc = Procdesc.get_loc pdesc in
        get_globals pdesc loc exp |> add_globals astate proc_loc
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
          | None | Some Domain.Bottom ->
              Domain.Bottom in
        add_params_globals astate pdesc loc params
        |> Domain.join callee_globals
        |>
        (* make sure it's not Bottom: we made a function call so this needs initialization *)
        at_least_nonbottom
    | Call (_, _, params, loc, _) ->
        add_params_globals astate pdesc loc params
        |>
        (* make sure it's not Bottom: we made a function call so this needs initialization *)
        at_least_nonbottom
    | Declare_locals _ | Remove_temps _ | Abstract _ | Nullify _ ->
        astate
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Normal)
    (Scheduler.ReversePostorder)
    (TransferFunctions)

module Interprocedural = AbstractInterpreter.Interprocedural (Summary)


let is_foreign tu_opt (v, _) =
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
    | _ -> SiofTrace.empty in

  let report_one_path (passthroughs, path) =
    let description, sink_path' = match path with
      | [] -> assert false
      | (final_sink, pt)::rest ->
          let foreign_globals =
            SiofTrace.Sink.kind final_sink |> GlobalsAccesses.filter (is_foreign tu_opt) in
          let final_sink' =
            let loc = CallSite.loc (SiofTrace.Sink.call_site final_sink) in
            SiofTrace.make_access foreign_globals loc in
          let description =
            F.asprintf
              "Initializer of %s accesses global variables from a different translation unit: %a"
              gname
              GlobalsAccesses.pp foreign_globals in
          description, (passthroughs, (final_sink', pt)::rest) in
    let ltr = SiofTrace.trace_of_error loc gname sink_path' in
    let caller_pname = Procdesc.get_proc_name pdesc in
    let msg = Localise.to_string Localise.static_initialization_order_fiasco in
    let exn = Exceptions.Checkers (msg, Localise.verbatim_desc description) in
    Reporting.log_error caller_pname ~loc ~ltr exn in

  let has_foreign_sink (_, path) =
    IList.exists
      (fun (sink, _) ->
         GlobalsAccesses.exists (is_foreign tu_opt)
           (SiofTrace.Sink.kind sink))
      path in

  SiofTrace.get_reportable_sink_paths trace ~trace_of_pname
  |> IList.filter has_foreign_sink
  |> IList.iter report_one_path

let siof_check pdesc gname = function
  | Some (SiofDomain.NonBottom post) ->
      let attrs = Procdesc.get_attributes pdesc in
      let all_globals = SiofTrace.Sinks.fold
          (fun sink -> GlobalsAccesses.union (SiofTrace.Sink.kind sink))
          (SiofTrace.sinks post) GlobalsAccesses.empty in
      let tu_opt =
        let attrs = Procdesc.get_attributes pdesc in
        attrs.ProcAttributes.translation_unit in
      if GlobalsAccesses.exists (is_foreign tu_opt) all_globals then
        report_siof post pdesc gname attrs.ProcAttributes.loc;
  | Some SiofDomain.Bottom | None ->
      ()

let compute_post proc_data =
  Analyzer.compute_post proc_data ~initial:SiofDomain.Bottom |> Option.map ~f:SiofDomain.normalize

let checker ({ Callbacks.proc_desc; } as callback) =
  let post =
    Interprocedural.compute_and_store_post
      ~compute_post
      ~make_extras:ProcData.make_empty_extras
      callback in
  let pname = Procdesc.get_proc_name proc_desc in
  match Procname.get_global_name_of_initializer pname with
  | Some gname ->
      siof_check proc_desc gname post
  | None ->
      ()
