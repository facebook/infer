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

let methods_whitelist = QualifiedCppName.quals_matcher_of_fuzzy_qual_names Config.siof_safe_methods

let is_whitelisted (pname : Procname.t) =
  Procname.get_qualifiers pname
  |> QualifiedCppName.match_qualifiers methods_whitelist

type siof_model = {
  qual_name : string; (** (fuzzy) name of the method, eg "std::ios_base::Init::Init" *)
  initialized_globals : string list; (** names of variables that are guaranteed to be initialized
                                         once the method is executed, eg ["std::cerr"] *)
}

let parse_siof_model (qual_name, initialized_globals) = { qual_name; initialized_globals; }

let models = List.map ~f:parse_siof_model [
    ("std::ios_base::Init::Init", [
        "std::cerr"; "std::wcerr";
        "std::cin"; "std::wcin";
        "std::clog"; "std::wclog";
        "std::cout"; "std::wcout";
      ]);
  ]

let is_modelled =
  let models_matcher =
    List.map models ~f:(fun {qual_name} -> qual_name)
    |> QualifiedCppName.quals_matcher_of_fuzzy_qual_names in
  fun pname ->
    Procname.get_qualifiers pname
    |> QualifiedCppName.match_qualifiers models_matcher

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
    | Some (Domain.BottomSiofTrace.Bottom, _)->
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
      Exp.get_vars e |> snd |> List.filter ~f:is_dangerous_global
      |> List.map ~f:(fun v -> (v, loc)) in
    GlobalsAccesses.of_list globals_accesses

  let filter_global_accesses initialized globals =
    let initialized_matcher =
      Domain.VarNames.elements initialized
      |> QualifiedCppName.quals_matcher_of_fuzzy_qual_names in
    (* gvar \notin initialized, up to some fuzzing *)
    let f (gvar, _) =
      QualifiedCppName.qualifiers_of_qual_name (Pvar.to_string gvar)
      |> Fn.non (QualifiedCppName.match_qualifiers initialized_matcher) in
    GlobalsAccesses.filter f globals

  let add_globals astate outer_loc globals =
    if GlobalsAccesses.is_empty globals then
      astate
    else
      let trace = match fst astate with
        | Domain.BottomSiofTrace.Bottom -> SiofTrace.empty
        | Domain.BottomSiofTrace.NonBottom t -> t in
      (* filter out variables that are known to be already initialized *)
      let non_init_globals =
        let initialized = snd astate in
        filter_global_accesses initialized globals in
      let globals_trace =
        SiofTrace.add_sink (SiofTrace.make_access non_init_globals outer_loc) trace in
      (Domain.BottomSiofTrace.NonBottom globals_trace, snd astate)

  let add_params_globals astate pdesc call_loc params =
    List.map ~f:(fun (e, _) -> get_globals pdesc call_loc e) params
    |> List.fold ~f:GlobalsAccesses.union ~init:GlobalsAccesses.empty
    |> add_globals astate (Procdesc.get_loc pdesc)

  let at_least_nonbottom =
    Domain.join ((Domain.BottomSiofTrace.NonBottom SiofTrace.empty), Domain.VarNames.empty)

  let exec_instr astate { ProcData.pdesc; } _ (instr : Sil.instr) =
    match instr with
    | Load (_, exp, _, loc)
    | Store (_, _, exp, loc)
    | Prune (exp, loc, _, _) ->
        let proc_loc = Procdesc.get_loc pdesc in
        get_globals pdesc loc exp |> add_globals astate proc_loc
    | Call (_, Const (Cfun callee_pname), _, _, _) when is_whitelisted callee_pname ->
        at_least_nonbottom astate
    | Call (_, Const (Cfun callee_pname), _, _, _) when is_modelled callee_pname ->
        let init = List.find_map_exn models
            ~f:(fun {qual_name; initialized_globals} ->
                if QualifiedCppName.quals_matcher_of_fuzzy_qual_names [qual_name]
                   |> Fn.flip QualifiedCppName.match_qualifiers
                     (Procname.get_qualifiers callee_pname) then
                  Some initialized_globals
                else
                  None) in
        Domain.join astate (Domain.BottomSiofTrace.NonBottom SiofTrace.empty,
                            Domain.VarNames.of_list init)
    | Call (_, Const (Cfun callee_pname), _::params_without_self, loc, _)
      when Procname.is_c_method callee_pname && Procname.is_constructor callee_pname
           && Procname.is_constexpr callee_pname ->
        add_params_globals astate pdesc loc params_without_self
    | Call (_, Const (Cfun callee_pname), params, loc, _) ->
        let callsite = CallSite.make callee_pname loc in
        let callee_astate = match Summary.read_summary pdesc callee_pname with
          | Some (Domain.BottomSiofTrace.NonBottom trace, initialized_globals) ->
              let trace_without_initialized_globals =
                let sinks_with_non_init_globals =
                  SiofTrace.Sinks.filter (fun sink ->
                      filter_global_accesses (snd astate) (SiofTrace.Sink.kind sink)
                      |> Fn.non GlobalsAccesses.is_empty) (SiofTrace.sinks trace) in
                SiofTrace.update_sinks trace sinks_with_non_init_globals in
              (Domain.BottomSiofTrace.NonBottom
                 (SiofTrace.with_callsite trace_without_initialized_globals callsite),
               initialized_globals)
          | Some ((Domain.BottomSiofTrace.Bottom, _) as astate) ->
              astate
          | None ->
              (Domain.BottomSiofTrace.Bottom, Domain.VarNames.empty) in
        add_params_globals astate pdesc loc params
        |> Domain.join callee_astate
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

module Analyzer = AbstractInterpreter.Make (ProcCfg.Normal) (TransferFunctions)

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
    | Some (SiofDomain.BottomSiofTrace.NonBottom summary, _) -> summary
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
    List.exists
      ~f:(fun (sink, _) ->
          GlobalsAccesses.exists (is_foreign tu_opt)
            (SiofTrace.Sink.kind sink))
      path in

  SiofTrace.get_reportable_sink_paths trace ~trace_of_pname
  |> List.filter ~f:has_foreign_sink
  |> List.iter ~f:report_one_path

let siof_check pdesc gname = function
  | Some ((SiofDomain.BottomSiofTrace.NonBottom post, _)) ->
      let attrs = Procdesc.get_attributes pdesc in
      let all_globals = SiofTrace.Sinks.fold
          (fun sink -> GlobalsAccesses.union (SiofTrace.Sink.kind sink))
          (SiofTrace.sinks post) GlobalsAccesses.empty in
      let tu_opt =
        let attrs = Procdesc.get_attributes pdesc in
        attrs.ProcAttributes.translation_unit in
      if GlobalsAccesses.exists (is_foreign tu_opt) all_globals then
        report_siof post pdesc gname attrs.ProcAttributes.loc;
  | Some (SiofDomain.BottomSiofTrace.Bottom, _) | None ->
      ()

let compute_post proc_data =
  Analyzer.compute_post proc_data
    ~initial:(SiofDomain.BottomSiofTrace.Bottom, SiofDomain.VarNames.empty)
  |> Option.map ~f:SiofDomain.normalize

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
