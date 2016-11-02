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
      match payload.Specs.siof with
      | Some astate -> astate
      | None -> SiofDomain.initial
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
      let pname = Cfg.Procdesc.get_proc_name pdesc in
      let trace = match astate with
        | SiofDomain.Bottom -> SiofTrace.initial
        | SiofDomain.NonBottom t -> t in
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
        let callee_globals =
          Option.default Domain.initial
          @@ Summary.read_summary tenv pdesc callee_pname in
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

let report_siof pname loc bad_globals =
  let pp_desc fmt () =
    let pp_var f v =
      let pp_source f v = match Pvar.get_source_file v with
        | Some source_file when DB.source_file_equal DB.source_file_empty source_file ->
            Format.fprintf f ""
        | None ->
            Format.fprintf f ""
        | Some source_file ->
            Format.fprintf f " from file %s" (DB.source_file_to_string source_file) in
      Format.fprintf f "%s%a" (Pvar.get_simplified_name v) pp_source v in
    let pp_set f s = pp_seq pp_var f s in
    Format.fprintf fmt
      "This global variable initializer accesses the following globals in another translation \
       unit: %a"
      pp_set bad_globals in
  let description = pp_to_string pp_desc () in
  let exn = Exceptions.Checkers
      ("STATIC_INITIALIZATION_ORDER_FIASCO", Localise.verbatim_desc description) in
  Reporting.log_error pname ~loc exn

let siof_check pdesc = function
  | Some (SiofDomain.NonBottom post) ->
      let attrs = Cfg.Procdesc.get_attributes pdesc in
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
      then
        let foreign_globals =
          IList.map
            (fun sink -> (SiofTrace.Sink.kind sink))
            (SiofTrace.Sinks.elements foreign_global_sinks) in
        report_siof (Cfg.Procdesc.get_proc_name pdesc) attrs.ProcAttributes.loc foreign_globals;
  | Some SiofDomain.Bottom | None ->
      ()

let checker callback =
  let pdesc = callback.Callbacks.proc_desc in
  let post = Interprocedural.checker callback ProcData.empty_extras in
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  match pname with
  | Procname.C c when Procname.is_globals_initializer c ->
      siof_check pdesc post
  | _ ->
      ()
