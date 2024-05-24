(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format
module L = Logging
module GlobalVar = SiofTrace.GlobalVar
module GlobalVarSet = SiofTrace.GlobalVarSet

let methods_allow_list = QualifiedCppName.Match.of_fuzzy_qual_names Config.siof_safe_methods

let is_allow_listed (pname : Procname.t) =
  Procname.get_qualifiers pname |> QualifiedCppName.Match.match_qualifiers methods_allow_list


type siof_model =
  { qual_name: string  (** (fuzzy) name of the method, eg "std::ios_base::Init::Init" *)
  ; initialized_globals: string list
        (** names of variables that are guaranteed to be initialized once the method is executed, eg
            ["std::cerr"] *) }

let parse_siof_model (qual_name, initialized_globals) = {qual_name; initialized_globals}

let standard_streams =
  [ "std::cerr"
  ; "std::wcerr"
  ; "std::cin"
  ; "std::wcin"
  ; "std::clog"
  ; "std::wclog"
  ; "std::cout"
  ; "std::wcout" ]


let always_initialized =
  (* We model "std::ios_base::Init::Init" as initializing [standard_streams], which works if either
     infer's cxx models are enabled ([Config.cxx_infer_headers]) or if the stdlib is a recent
     libstdc++ (which is kind enough to create an [std::ios_base::Init] object explicitely). In
     other cases, we assume they are always initialized so as not to be noisy. The issue to remove
     this assumption would be to detect when <iostream> is included in a file without swapping the
     C++ includes for our own. *)
  if Config.siof_check_iostreams then [] else standard_streams


let models = List.map ~f:parse_siof_model [("std::ios_base::Init::Init", standard_streams)]

let is_modelled =
  let models_matcher =
    List.map models ~f:(fun {qual_name} -> qual_name) |> QualifiedCppName.Match.of_fuzzy_qual_names
  in
  fun pname ->
    Procname.get_qualifiers pname |> QualifiedCppName.Match.match_qualifiers models_matcher


module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = SiofDomain

  type analysis_data = Domain.t InterproceduralAnalysis.t

  let is_compile_time_constructed {InterproceduralAnalysis.analyze_dependency} pv =
    let init_pname = Pvar.get_initializer_pname pv in
    match
      Option.bind init_pname ~f:(fun callee_pname ->
          analyze_dependency callee_pname |> AnalysisResult.to_option )
    with
    | Some (Bottom, _) ->
        (* we analyzed the initializer for this global and found that it doesn't require any runtime
           initialization so cannot participate in SIOF *)
        true
    | _ ->
        false


  let filter_global_accesses initialized =
    let initialized_matcher =
      (* Note: [QualifiedCppName.Match.of_fuzzy_qual_names] may be expensive since it includes
         [Str.regexp]. *)
      Domain.VarNames.elements initialized |> QualifiedCppName.Match.of_fuzzy_qual_names
    in
    Staged.stage (fun (* gvar \notin initialized, up to some fuzzing *)
                        gvar ->
        QualifiedCppName.of_qual_string (Pvar.to_string gvar)
        |> Fn.non (QualifiedCppName.Match.match_qualifiers initialized_matcher) )


  let is_not_always_initialized =
    Staged.unstage (filter_global_accesses (Domain.VarNames.of_list always_initialized))


  let get_globals analysis_data e =
    let is_dangerous_global pv =
      Pvar.is_global pv
      && (not (Pvar.is_static_local pv))
      && (not (Pvar.is_pod pv))
      && (not (Pvar.is_compile_constant pv))
      && (not (is_compile_time_constructed analysis_data pv))
      && is_not_always_initialized pv
    in
    Exp.program_vars e
    |> Sequence.fold ~init:GlobalVarSet.empty ~f:(fun gset g ->
           if is_dangerous_global g then GlobalVarSet.add g gset else gset )


  let add_globals astate loc globals =
    if GlobalVarSet.is_empty globals then astate
    else
      let trace = match fst astate with Bottom -> SiofTrace.bottom | NonBottom t -> t in
      let is_dangerous =
        (* filter out variables that are known to be already initialized *)
        let initialized = snd astate in
        filter_global_accesses initialized |> Staged.unstage
      in
      let trace_with_non_init_globals =
        GlobalVarSet.fold
          (fun global acc ->
            if is_dangerous global then SiofTrace.add_sink (SiofTrace.make_access global loc) acc
            else acc )
          globals trace
      in
      (NonBottom trace_with_non_init_globals, snd astate)


  let add_actuals_globals analysis_data astate0 call_loc actuals =
    List.fold_left actuals ~init:astate0 ~f:(fun astate (e, _) ->
        get_globals analysis_data e |> add_globals astate call_loc )


  let at_least_nonbottom = Domain.join (NonBottom SiofTrace.bottom, Domain.VarNames.empty)

  let init_f =
    List.map models ~f:(fun {qual_name; initialized_globals} ->
        let regexp = lazy (QualifiedCppName.Match.of_fuzzy_qual_names [qual_name]) in
        fun callee_pname ->
          if
            Lazy.force regexp
            |> Fn.flip QualifiedCppName.Match.match_qualifiers
                 (Procname.get_qualifiers callee_pname)
          then Some initialized_globals
          else None )


  let exec_instr astate
      ({InterproceduralAnalysis.proc_desc; analyze_dependency; _} as analysis_data) _ _
      (instr : Sil.instr) =
    match instr with
    | Store {e1= Lvar global; typ= Typ.{desc= Tptr _}; e2= Lvar _; loc}
      when (Option.equal Procname.equal)
             (Pvar.get_initializer_pname global)
             (Some (Procdesc.get_proc_name proc_desc)) ->
        (* if we are just taking the reference of another global then we are not really accessing
           it. This is a dumb heuristic as something also might take that result and then
           dereference it, thus requiring the target object to be initialized. Solving this would
           involve a more complicated domain and analysis.

           The heuristic is limited to the case where the access sets the global being initialized
           in the current variable initializer function. *)
        add_globals astate loc (GlobalVarSet.singleton global)
    | Load {e= exp; loc} (* dereference -> add all the dangerous variables *)
    | Store {e2= exp; loc} (* except in the case above, consider all reads as dangerous *)
    | Prune (exp, loc, _, _) ->
        get_globals analysis_data exp |> add_globals astate loc
    | Call (_, Const (Cfun callee_pname), _, _, _) when is_allow_listed callee_pname ->
        at_least_nonbottom astate
    | Call (_, Const (Cfun callee_pname), _, _, _) when is_modelled callee_pname ->
        let init = List.find_map_exn init_f ~f:(fun f -> f callee_pname) in
        Domain.join astate (NonBottom SiofTrace.bottom, Domain.VarNames.of_list init)
    | Call (_, Const (Cfun callee_pname), actuals, loc, _)
      when Attributes.load callee_pname
           |> Option.exists ~f:(fun attrs -> attrs.ProcAttributes.is_ret_constexpr) ->
        let actuals_without_this =
          if Procname.is_constructor callee_pname then List.tl actuals |> Option.value ~default:[]
          else actuals
        in
        add_actuals_globals analysis_data astate loc actuals_without_this
    | Call (_, Const (Cfun callee_pname), actuals, loc, _) ->
        let callee_astate =
          match analyze_dependency callee_pname with
          | Ok (NonBottom trace, initialized_by_callee) ->
              let already_initialized = snd astate in
              let dangerous_accesses =
                SiofTrace.sinks trace
                |> SiofTrace.Sinks.filter (fun sink ->
                       SiofTrace.Sink.kind sink
                       |> Staged.unstage (filter_global_accesses already_initialized) )
              in
              let callsite = CallSite.make callee_pname loc in
              let sinks =
                SiofTrace.Sinks.map
                  (fun access -> SiofTrace.Sink.with_callsite access callsite)
                  dangerous_accesses
              in
              (NonBottom (SiofTrace.update_sinks trace sinks), initialized_by_callee)
          | Ok ((Bottom, _) as callee_astate) ->
              callee_astate
          | Error no_summary ->
              L.d_printfln "No summary: %a" AnalysisResult.pp_no_summary no_summary ;
              (Bottom, Domain.VarNames.empty)
        in
        add_actuals_globals analysis_data astate loc actuals
        |> Domain.join callee_astate
        |> (* make sure it's not Bottom: we made a function call so this needs initialization *)
        at_least_nonbottom
    | Call (_, _, actuals, loc, _) ->
        add_actuals_globals analysis_data astate loc actuals
        |> (* make sure it's not Bottom: we made a function call so this needs initialization *)
        at_least_nonbottom
    | Metadata _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "siof"
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (ProcCfg.Normal))

let is_foreign current_tu v =
  match Pvar.get_translation_unit v with
  | Some v_tu ->
      not (SourceFile.equal current_tu v_tu)
  | None ->
      true


let report_siof {InterproceduralAnalysis.proc_desc; err_log; analyze_dependency; _} trace gname loc
    =
  let trace_of_pname pname =
    match analyze_dependency pname with
    | Ok (NonBottom summary, _) ->
        summary
    | _ ->
        SiofTrace.bottom
  in
  let report_one_path ((_, path) as trace) =
    let description =
      match path with
      | [] ->
          assert false
      | (final_sink, _) :: _ ->
          F.asprintf
            "Initializer of %s accesses global variable from a different translation unit: %a" gname
            GlobalVar.pp (SiofTrace.Sink.kind final_sink)
    in
    let ltr = SiofTrace.trace_of_error loc gname trace in
    Reporting.log_issue proc_desc err_log ~loc ~ltr SIOF
      IssueType.static_initialization_order_fiasco description
  in
  let reportable_paths = SiofTrace.get_reportable_sink_paths trace ~trace_of_pname in
  (* FIXME(T54950303) replace use of filtering with deduplicate *)
  if Config.filtering then List.hd reportable_paths |> Option.iter ~f:report_one_path
  else List.iter ~f:report_one_path reportable_paths


let siof_check ({InterproceduralAnalysis.proc_desc} as analysis_data) gname summary =
  match summary with
  | Some (NonBottom post, _) ->
      let attrs = Procdesc.get_attributes proc_desc in
      let tu = attrs.ProcAttributes.translation_unit in
      let foreign_sinks =
        SiofTrace.Sinks.filter
          (fun sink -> SiofTrace.Sink.kind sink |> is_foreign tu)
          (SiofTrace.sinks post)
      in
      if not (SiofTrace.Sinks.is_empty foreign_sinks) then
        report_siof analysis_data
          (SiofTrace.update_sinks post foreign_sinks)
          gname attrs.ProcAttributes.loc
  | Some (Bottom, _) | None ->
      ()


let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  let pname = Procdesc.get_proc_name proc_desc in
  let standard_streams_initialized_in_tu =
    let includes_iostream tu =
      let magic_iostream_marker =
        (* always [Some _] because we create a global variable with [mk_global] *)
        Option.value_exn
          ( Pvar.mk_global ~translation_unit:tu
              (Mangled.from_string
                 (* infer's C++ headers define this global variable in <iostream> *)
                 "__infer_translation_unit_init_streams" )
          |> Pvar.get_initializer_pname )
      in
      SourceFiles.get_procs_in_file pname |> List.exists ~f:(Procname.equal magic_iostream_marker)
    in
    includes_iostream (Procdesc.get_attributes proc_desc).ProcAttributes.translation_unit
  in
  let initial =
    ( Bottom
    , if standard_streams_initialized_in_tu then SiofDomain.VarNames.of_list standard_streams
      else SiofDomain.VarNames.empty )
  in
  let summary =
    (* If the function is constexpr then it doesn't participate in SIOF. The checker should be able
       to figure this out when analyzing the function, but we might as well use the user's
       specification if it's given to us. This also serves as an optimization as this skips the
       analysis of the function. *)
    if (Procdesc.get_attributes proc_desc).is_ret_constexpr then Some initial
    else Analyzer.compute_post analysis_data ~initial proc_desc
  in
  ( match Procname.get_global_name_of_initializer pname with
  | Some gname ->
      siof_check analysis_data gname summary
  | None ->
      () ) ;
  summary
