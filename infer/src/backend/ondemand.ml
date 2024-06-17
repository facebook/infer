(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for on-demand analysis. *)

module L = Logging
module F = Format
open Option.Monad_infix

(* always incremented before use *)
let nesting = ref (-1)

let () = AnalysisGlobalState.register_ref nesting ~init:(fun () -> -1)

let max_nesting_to_print = 8

(* keep track of the "call stack" of procedures we are currently analyzing; used to break mutual
   recursion cycles in a naive way: if we are already analyzing a procedure and another one (whose
   summary we transitively need to analyze the original procedure) asks for its summary, return no
   summary instead of triggering a recursive analysis of the original procedure. *)
let is_active, add_active, remove_active, clear_actives =
  let open Procname.HashSet in
  let currently_analyzed = create 0 in
  let is_active proc_name = mem currently_analyzed proc_name in
  let add_active proc_name = add proc_name currently_analyzed in
  let remove_active proc_name = remove proc_name currently_analyzed in
  let clear_actives () = clear currently_analyzed in
  (is_active, add_active, remove_active, clear_actives)


(** an alternative mean of "cutting" recursion cycles used when replaying a previous analysis: times
    where [is_active] caused ondemand to return an empty summary to avoid a recursive analysis in
    the previous analysis scheduling are recorded in this variable *)
let edges_to_ignore = ref None

(** use either [is_active] or [edges_to_ignore] to determine if we should return an empty summary to
    avoid mutual recursion cycles *)
let in_mutual_recursion_cycle ~caller_summary ~callee =
  match (!edges_to_ignore, caller_summary) with
  | Some edges_to_ignore, Some {Summary.proc_name} ->
      Procname.Map.find_opt proc_name edges_to_ignore
      |> Option.exists ~f:(fun recursive_callees -> Procname.Set.mem callee recursive_callees)
  | None, _ | _, None ->
      is_active callee


let procedure_is_defined proc_name =
  Attributes.load proc_name
  |> Option.exists ~f:(fun proc_attributes -> proc_attributes.ProcAttributes.is_defined)


(* Remember what the last status sent was so that we can update the status correctly when entering
   and exiting nested ondemand analyses. In particular we need to remember the original time.*)
let current_taskbar_status : (Mtime.t * string) option ref = ref None

let () =
  let open IOption.Let_syntax in
  AnalysisGlobalState.register
    ~save:(fun () ->
      let+ t0, status = !current_taskbar_status in
      (* the time elapsed doing [status] so far *)
      (Mtime.span t0 (Mtime_clock.now ()), status) )
    ~restore:(fun proc_analysis_time ->
      current_taskbar_status :=
        let+ suspended_span, status = proc_analysis_time in
        (* forget about the time spent doing a nested analysis and resend the status of the outer
           analysis with the updated "original" start time *)
        let new_t0 = Mtime.sub_span (Mtime_clock.now ()) suspended_span |> Option.value_exn in
        !ProcessPoolState.update_status new_t0 status ;
        (new_t0, status) )
    ~init:(fun _ -> current_taskbar_status := None)


let () =
  (* register [Logging] and [Timer]'s global states here because this requires knowing about [Procname.t] but
     they live in base/ which is too low in the dependency tree *)
  AnalysisGlobalState.register ~save:L.get_and_reset_delayed_prints ~restore:L.set_delayed_prints
    ~init:L.reset_delayed_prints ;
  AnalysisGlobalState.register ~save:Timer.suspend ~restore:Timer.resume ~init:(fun () -> ()) ;
  AnalysisGlobalState.register ~save:Ident.NameGenerator.get_current
    ~restore:Ident.NameGenerator.set_current ~init:Ident.NameGenerator.reset ;
  AnalysisGlobalState.register_ref_with_proc_desc_and_tenv Dependencies.currently_under_analysis
    ~init:(fun proc_desc _tenv -> Option.some (Procdesc.get_proc_name proc_desc) ) ;
  ()


(** reference to log errors only at the innermost recursive call *)
let logged_error = ref false

let update_taskbar proc_name_opt source_file_opt =
  let t0 = Mtime_clock.now () in
  let status =
    match (proc_name_opt, source_file_opt) with
    | Some pname, Some src_file ->
        let nesting =
          if !nesting <= max_nesting_to_print then String.make !nesting '>'
          else Printf.sprintf "%d>" !nesting
        in
        F.asprintf "%s%a: %a" nesting SourceFile.pp src_file Procname.pp pname
    | Some pname, None ->
        Procname.to_string pname
    | None, Some src_file ->
        SourceFile.to_string src_file
    | None, None ->
        "Unspecified task"
  in
  current_taskbar_status := Some (t0, status) ;
  !ProcessPoolState.update_status t0 status


let set_complete_result analysis_req summary =
  match (analysis_req : AnalysisRequest.t) with
  | All ->
      summary.Summary.is_complete_result <- true
  | One _ | CheckerWithoutPayload _ ->
      ()


let analyze exe_env analysis_req ?specialization callee_summary callee_pdesc =
  let summary =
    Callbacks.iterate_procedure_callbacks exe_env analysis_req ?specialization callee_summary
      callee_pdesc
  in
  set_complete_result analysis_req summary ;
  Stats.incr_ondemand_procs_analyzed () ;
  summary


let run_proc_analysis exe_env tenv analysis_req ?specialization ?caller_pname callee_pdesc =
  let callee_pname = Procdesc.get_proc_name callee_pdesc in
  let callee_attributes = Procdesc.get_attributes callee_pdesc in
  let log_elapsed_time =
    let start_time = Mtime_clock.counter () in
    fun () ->
      let elapsed = Mtime_clock.count start_time in
      let duration_us = IMtime.span_to_us_int elapsed in
      let file = SourceFile.to_rel_path callee_attributes.loc.file in
      let pname = Procname.to_string callee_pname in
      Stats.add_proc_duration_us file pname duration_us ;
      L.(debug Analysis Medium)
        "Elapsed analysis time: %a: %a@\n" Procname.pp callee_pname Mtime.Span.pp elapsed
  in
  if Config.trace_ondemand then
    L.progress "[%d] run_proc_analysis %a -> %a@." !nesting (Pp.option Procname.pp) caller_pname
      Procname.pp callee_pname ;
  let preprocess () =
    incr nesting ;
    let source_file = callee_attributes.ProcAttributes.translation_unit in
    update_taskbar (Some callee_pname) (Some source_file) ;
    Preanal.do_preanalysis tenv callee_pdesc ;
    let initial_callee_summary =
      match specialization with
      | None ->
          if Config.debug_mode then
            DotCfg.emit_proc_desc callee_attributes.translation_unit callee_pdesc |> ignore ;
          Summary.OnDisk.reset callee_pname analysis_req
      | Some (current_summary, _) ->
          current_summary
    in
    add_active callee_pname ;
    initial_callee_summary
  in
  let postprocess summary =
    decr nesting ;
    (* copy the previous recursion edges over to the new summary if doing a replay analysis so that
       subsequent replay analyses can pick them up too *)
    Option.iter !edges_to_ignore ~f:(fun edges_to_ignore ->
        Procname.Map.find_opt callee_pname edges_to_ignore
        |> Option.iter ~f:(fun recursive_callees ->
               Procname.Set.iter
                 (fun callee ->
                   Dependencies.record_pname_dep ~caller:callee_pname RecursionEdge callee )
                 recursive_callees ) ) ;
    let summary = Summary.OnDisk.store analysis_req summary in
    remove_active callee_pname ;
    Printer.write_proc_html callee_pdesc ;
    log_elapsed_time () ;
    summary
  in
  let log_error_and_continue exn ({Summary.err_log; payloads; stats} as summary) kind =
    BiabductionReporting.log_issue_using_state callee_pdesc err_log exn ;
    let stats = Summary.Stats.update stats ~failure_kind:kind in
    let payloads =
      let biabduction =
        Some
          (Lazy.from_val
             BiabductionSummary.
               {preposts= []; phase= payloads.biabduction |> ILazy.force_option |> opt_get_phase} )
      in
      {payloads with biabduction}
    in
    let new_summary = {summary with stats; payloads} in
    let new_summary = Summary.OnDisk.store analysis_req new_summary in
    remove_active callee_pname ;
    log_elapsed_time () ;
    new_summary
  in
  let initial_callee_summary = preprocess () in
  try
    let callee_summary =
      if callee_attributes.ProcAttributes.is_defined then
        let specialization = Option.map ~f:snd specialization in
        analyze exe_env analysis_req ?specialization initial_callee_summary callee_pdesc
      else initial_callee_summary
    in
    let final_callee_summary = postprocess callee_summary in
    (* don't forget to reset this so we output messages for future errors too *)
    logged_error := false ;
    final_callee_summary
  with exn -> (
    let backtrace = Printexc.get_backtrace () in
    IExn.reraise_if exn ~f:(fun () ->
        match exn with
        | RestartSchedulerException.ProcnameAlreadyLocked _
        | MissingDependencyException.MissingDependencyException ->
            clear_actives () ;
            true
        | exn ->
            if not !logged_error then (
              let source_file = callee_attributes.ProcAttributes.translation_unit in
              let location = callee_attributes.ProcAttributes.loc in
              L.internal_error "While analysing function %a:%a at %a, raised %s@\n" SourceFile.pp
                source_file Procname.pp callee_pname Location.pp_file_pos location
                (Exn.to_string exn) ;
              logged_error := true ) ;
            not Config.keep_going ) ;
    L.internal_error "@\nERROR RUNNING BACKEND: %a %s@\n@\nBACK TRACE@\n%s@?" Procname.pp
      callee_pname (Exn.to_string exn) backtrace ;
    match exn with
    | Exception.Analysis_failure_exe kind ->
        (* in production mode, log the timeout/crash and continue with the summary we had before
           the failure occurred *)
        log_error_and_continue exn initial_callee_summary kind
    | _ ->
        (* this happens with assert false or some other unrecognized exception *)
        log_error_and_continue exn initial_callee_summary (FKcrash (Exn.to_string exn)) )


(* shadowed for tracing *)
let run_proc_analysis exe_env tenv analysis_req ?specialization ?caller_pname callee_pdesc =
  PerfEvent.(
    log (fun logger ->
        let callee_pname = Procdesc.get_proc_name callee_pdesc in
        log_begin_event logger ~name:"ondemand" ~categories:["backend"]
          ~arguments:[("proc", `String (Procname.to_string callee_pname))]
          () ) ) ;
  let summary =
    run_proc_analysis exe_env tenv analysis_req ?specialization ?caller_pname callee_pdesc
  in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  summary


let dump_duplicate_procs source_file procs =
  let duplicate_procs =
    List.filter_map procs ~f:(fun pname ->
        match Attributes.load pname with
        | Some
            { is_defined=
                true
                (* likely not needed: if [pname] is part of [procs] then it *is* defined, so we
                   expect the attribute to be defined too *)
            ; translation_unit
            ; loc }
          when (* defined in another file *)
               (not (SourceFile.equal source_file translation_unit))
               && (* really defined in that file and not in an include *)
               SourceFile.equal translation_unit loc.file ->
            Some (pname, translation_unit)
        | _ ->
            None )
  in
  let output_to_file duplicate_procs =
    Out_channel.with_file (ResultsDir.get_path DuplicateFunctions) ~append:true ~perm:0o666
      ~f:(fun outc ->
        let fmt = F.formatter_of_out_channel outc in
        List.iter duplicate_procs ~f:(fun (pname, source_captured) ->
            F.fprintf fmt "DUPLICATE_SYMBOLS source:%a source_captured:%a pname:%a@\n" SourceFile.pp
              source_file SourceFile.pp source_captured Procname.pp pname ) ;
        F.pp_print_flush fmt () )
  in
  if not (List.is_empty duplicate_procs) then output_to_file duplicate_procs


let register_callee ~cycle_detected ?caller_summary callee =
  let dependency_kind : Dependencies.kind = if cycle_detected then RecursionEdge else SummaryLoad in
  Option.iter caller_summary ~f:(fun {Summary.proc_name= caller} ->
      Dependencies.record_pname_dep ~caller dependency_kind callee )


let error_if_ondemand_analysis_during_replay ~from_file_analysis caller_summary callee_pname =
  match (Config.replay_analysis_schedule, caller_summary, from_file_analysis) with
  | true, Some caller_summary, false ->
      if Config.replay_ondemand_should_error then
        L.internal_error
          "analyzing procedure %a via ondemand when computing the summary for %a; we may have \
           recorded analysis dependencies wrongly@\n"
          Procname.pp callee_pname Procname.pp caller_summary.Summary.proc_name
  | true, None, true ->
      if Config.replay_ondemand_should_error then
        L.internal_error
          "analyzing procedure %a for a file-level analysis; we may have recorded analysis \
           dependencies wrongly@\n"
          Procname.pp callee_pname
  | _, Some _, true ->
      (* the origin of the ondemand analysis cannot be both computing all procedure summaries for a
         file-level analysis *and* computing a summary for another procedure *)
      assert false
  | true, None, false | false, _, _ ->
      ()


let is_in_block_list =
  let matcher =
    QualifiedCppName.Match.of_fuzzy_qual_names ~prefix:true Config.qualified_cpp_name_block_list
  in
  fun pname ->
    Option.exists (Procname.get_class_type_name pname) ~f:(fun name ->
        QualifiedCppName.Match.match_qualifiers matcher (Typ.Name.qual_name name) )


let analyze_callee exe_env ~lazy_payloads (analysis_req : AnalysisRequest.t) ?specialization
    ?caller_summary ?(from_file_analysis = false) callee_pname : _ AnalysisResult.t =
  let cycle_detected = in_mutual_recursion_cycle ~caller_summary ~callee:callee_pname in
  let analysis_result_of_option opt = Result.of_option opt ~error:AnalysisResult.AnalysisFailed in
  register_callee ~cycle_detected ?caller_summary callee_pname ;
  if cycle_detected then Error MutualRecursionCycle
  else if is_in_block_list callee_pname then Error InBlockList
  else
    let analyze_callee_aux ~specialization =
      error_if_ondemand_analysis_during_replay ~from_file_analysis caller_summary callee_pname ;
      Procdesc.load callee_pname
      >>= fun callee_pdesc ->
      RestartScheduler.with_lock callee_pname ~f:(fun () ->
          let previous_global_state = AnalysisGlobalState.save () in
          protect
            ~f:(fun () ->
              (* preload tenv to avoid tainting preanalysis timing with IO *)
              let tenv = Exe_env.get_proc_tenv exe_env callee_pname in
              AnalysisGlobalState.initialize callee_pdesc tenv ;
              Timer.time Preanalysis
                ~f:(fun () ->
                  let caller_pname = caller_summary >>| fun summ -> summ.Summary.proc_name in
                  Some
                    (run_proc_analysis exe_env tenv analysis_req ?specialization ?caller_pname
                       callee_pdesc ) )
                ~on_timeout:(fun span ->
                  L.debug Analysis Quiet
                    "TIMEOUT after %fs of CPU time analyzing %a:%a, outside of any checkers \
                     (pre-analysis timeout?)@\n"
                    span SourceFile.pp (Procdesc.get_attributes callee_pdesc).translation_unit
                    Procname.pp callee_pname ;
                  None ) )
            ~finally:(fun () -> AnalysisGlobalState.restore previous_global_state) )
    in
    let analyze_specialization_none () =
      let res = analyze_callee_aux ~specialization:None in
      Option.iter res ~f:(set_complete_result analysis_req) ;
      analysis_result_of_option res
    in
    match (Summary.OnDisk.get ~lazy_payloads analysis_req callee_pname, specialization) with
    | Some ({is_complete_result= true} as summary), None ->
        Ok summary
    | Some ({payloads; is_complete_result= false} as summary), None -> (
      match analysis_req with
      | One payload_id when Payloads.has_payload payload_id payloads ->
          Ok summary
      | All | One _ | CheckerWithoutPayload _ ->
          analyze_specialization_none () )
    | None, Some _ ->
        L.die InternalError "specialization should always happend after regular analysis"
    | Some summary, Some specialization ->
        if Callbacks.is_specialized_for specialization summary then
          (* the current summary is specialized enough for this request *)
          Ok summary
        else if procedure_is_defined callee_pname then
          (* the current summary is not suitable for this specialization request *)
          analyze_callee_aux ~specialization:(Some (summary, specialization))
          |> analysis_result_of_option
        else (* can we even get there? *) Error UnknownProcedure
    | None, None ->
        if procedure_is_defined callee_pname then analyze_specialization_none ()
        else Error UnknownProcedure


let analyze_proc_name exe_env analysis_req ?specialization ~caller_summary callee_pname =
  analyze_callee ~lazy_payloads:false ?specialization exe_env analysis_req ~caller_summary
    callee_pname


let analyze_proc_name_for_file_analysis exe_env analysis_req callee_pname =
  (* load payloads lazily (and thus field by field as needed): we are either doing a file analysis
     and we don't want to load all payloads at once (to avoid high memory usage when only a few of
     the payloads are actually needed), or we are starting a procedure analysis in which case we're
     not interested in loading the summary if it has already been computed *)
  analyze_callee ~lazy_payloads:true exe_env analysis_req ~from_file_analysis:true callee_pname


let analyze_file_procedures exe_env analysis_req procs_to_analyze source_file_opt =
  let saved_language = !Language.curr_language in
  let analyze_proc_name_call pname =
    ignore
      (analyze_proc_name_for_file_analysis exe_env analysis_req pname : Summary.t AnalysisResult.t)
  in
  List.iter ~f:analyze_proc_name_call procs_to_analyze ;
  Option.iter source_file_opt ~f:(fun source_file ->
      if Config.dump_duplicate_symbols then dump_duplicate_procs source_file procs_to_analyze ;
      Callbacks.iterate_file_callbacks_and_store_issues procs_to_analyze exe_env source_file ) ;
  Language.curr_language := saved_language


(** Invoke all procedure-level and file-level callbacks on a given environment. *)
let analyze_file exe_env analysis_req source_file =
  update_taskbar None (Some source_file) ;
  let procs_to_analyze = SourceFiles.proc_names_of_source source_file in
  analyze_file_procedures exe_env analysis_req procs_to_analyze (Some source_file)


(** Invoke procedure callbacks on a given environment. *)
let analyze_proc_name_toplevel exe_env analysis_req proc_name =
  update_taskbar (Some proc_name) None ;
  analyze_callee ~lazy_payloads:true exe_env analysis_req proc_name |> ignore
