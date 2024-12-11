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

(** keep track of the "call stack" of procedures we are currently analyzing; used to break mutual
    recursion cycles in a naive way: if we are already analyzing a procedure and another one (whose
    summary we transitively need to analyze the original procedure) asks for its summary, return no
    summary instead of triggering a recursive analysis of the original procedure (unless it is a
    different specialization of the same procedure) *)
module ActiveProcedures : sig
  type active = SpecializedProcname.t

  val mem : active -> bool

  val add : active -> unit

  val remove : active -> unit

  val get_all : unit -> active list

  val get_cycle_start : active -> active * int * active
  (** given a target where we detected a recursive call, i.e. that already belongs to the queue,
      return a triple of where the cycle starts, the length of the cycle, and the first procedure we
      started to analyze *)
end = struct
  type active = SpecializedProcname.t

  module AnalysisTargets = Hash_queue.Make (SpecializedProcname)

  let currently_analyzed = DLS.new_key (fun () -> AnalysisTargets.create ())

  let pp_actives fmt =
    DLS.get currently_analyzed
    |> AnalysisTargets.iteri ~f:(fun ~key:target ~data:_ ->
           F.fprintf fmt "%a,@," SpecializedProcname.pp target )


  let mem analysis_target = AnalysisTargets.mem (DLS.get currently_analyzed) analysis_target

  let add analysis_target =
    if Config.trace_ondemand then L.progress "add %a@." SpecializedProcname.pp analysis_target ;
    AnalysisTargets.enqueue_back_exn (DLS.get currently_analyzed) analysis_target ()


  let remove analysis_target =
    if Config.trace_ondemand then L.progress "remove %a@." SpecializedProcname.pp analysis_target ;
    let popped_target, () =
      AnalysisTargets.dequeue_back_with_key_exn (DLS.get currently_analyzed)
    in
    if not (SpecializedProcname.equal popped_target analysis_target) then
      L.die InternalError
        "Queue structure for ondemand violated: expected to pop %a but got %a instead@\n\
         Active procedures: %t@\n"
        SpecializedProcname.pp analysis_target SpecializedProcname.pp popped_target pp_actives


  let get_all () = AnalysisTargets.keys @@ DLS.get currently_analyzed

  let get_cycle_start recursive =
    let all = get_all () in
    (* there is always one target in the queue since [recursive] is in the queue *)
    let first_active = List.hd_exn all in
    let cycle =
      List.drop_while all ~f:(fun target -> not (SpecializedProcname.equal target recursive))
    in
    let cycle_length = List.length cycle in
    (* there is always one target in the cycle which is the previous call to [recursive] *)
    let cycle_start = List.min_elt ~compare:SpecializedProcname.compare cycle |> Option.value_exn in
    (cycle_start, cycle_length, first_active)
end

(** an alternative mean of "cutting" recursion cycles used when replaying a previous analysis: times
    where [is_active] caused ondemand to return an empty summary to avoid a recursive analysis in
    the previous analysis scheduling are recorded in this variable

    TODO: [edges_to_ignore] should take specialization into account, like [is_active] *)
let edges_to_ignore = DLS.new_key (fun () -> None)

(** use either [is_active] or [edges_to_ignore] to determine if we should return an empty summary to
    avoid mutual recursion cycles *)
let detect_mutual_recursion_cycle ~caller_summary ~callee specialization =
  match (DLS.get edges_to_ignore, caller_summary) with
  | Some edges_to_ignore, Some {Summary.proc_name} ->
      let is_replay_recursive_callee =
        Procname.Map.find_opt proc_name edges_to_ignore
        |> Option.exists ~f:(fun recursive_callees -> Procname.Set.mem callee recursive_callees)
      in
      if is_replay_recursive_callee then `ReplayCycleCut else `NotInMutualRecursionCycle
  | None, _ | _, None ->
      if ActiveProcedures.mem {proc_name= callee; specialization} then `InMutualRecursionCycle
      else `NotInMutualRecursionCycle


let procedure_is_defined proc_name =
  Attributes.load proc_name
  |> Option.exists ~f:(fun proc_attributes -> proc_attributes.ProcAttributes.is_defined)


(* Remember what the last status sent was so that we can update the status correctly when entering
   and exiting nested ondemand analyses. In particular we need to remember the original time.*)
let current_taskbar_status : (Mtime.t * string) option DLS.key = DLS.new_key (fun () -> None)

let () =
  let open IOption.Let_syntax in
  AnalysisGlobalState.register
    ~save:(fun () ->
      let+ t0, status = DLS.get current_taskbar_status in
      (* the time elapsed doing [status] so far *)
      (Mtime.span t0 (Mtime_clock.now ()), status) )
    ~restore:(fun proc_analysis_time ->
      DLS.set current_taskbar_status
        (let+ suspended_span, status = proc_analysis_time in
         (* forget about the time spent doing a nested analysis and resend the status of the outer
            analysis with the updated "original" start time *)
         let new_t0 = Mtime.sub_span (Mtime_clock.now ()) suspended_span |> Option.value_exn in
         !ProcessPoolState.update_status (Some new_t0) status ;
         (new_t0, status) ) )
    ~init:(fun _ -> DLS.set current_taskbar_status None)


let () =
  (* register [Logging] and [Timer]'s global states here because this requires knowing about [Procname.t] but
     they live in base/ which is too low in the dependency tree *)
  AnalysisGlobalState.register ~save:L.get_and_reset_delayed_prints ~restore:L.set_delayed_prints
    ~init:L.reset_delayed_prints ;
  AnalysisGlobalState.register ~save:Timer.suspend ~restore:Timer.resume ~init:(fun () -> ()) ;
  AnalysisGlobalState.register ~save:Ident.NameGenerator.get_current
    ~restore:Ident.NameGenerator.set_current ~init:Ident.NameGenerator.reset ;
  AnalysisGlobalState.register_dls_with_proc_desc_and_tenv Dependencies.currently_under_analysis
    ~init:(fun proc_desc _tenv -> Option.some (Procdesc.get_proc_name proc_desc) ) ;
  ()


(** reference to log errors only at the innermost recursive call *)
let logged_error = DLS.new_key (fun () -> false)

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
  DLS.set current_taskbar_status (Some (t0, status)) ;
  !ProcessPoolState.update_status (Some t0) status


let set_complete_result analysis_req summary =
  match (analysis_req : AnalysisRequest.t) with
  | All ->
      summary.Summary.is_complete_result <- true
  | One _ | CheckerWithoutPayload _ ->
      ()


let analyze exe_env analysis_req specialization callee_summary callee_pdesc =
  let summary =
    Callbacks.iterate_procedure_callbacks exe_env analysis_req ?specialization callee_summary
      callee_pdesc
  in
  set_complete_result analysis_req summary ;
  Stats.incr_ondemand_procs_analyzed () ;
  summary


let run_proc_analysis exe_env tenv analysis_req specialization_context ?caller_pname callee_pdesc =
  let specialization = Option.map ~f:snd specialization_context in
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
      match specialization_context with
      | None ->
          if Config.debug_mode then
            DotCfg.emit_proc_desc callee_attributes.translation_unit callee_pdesc |> ignore ;
          Summary.OnDisk.reset callee_pname analysis_req
      | Some (current_summary, _) ->
          current_summary
    in
    ActiveProcedures.add {proc_name= callee_pname; specialization} ;
    initial_callee_summary
  in
  let postprocess summary =
    decr nesting ;
    (* copy the previous recursion edges over to the new summary if doing a replay analysis so that
       subsequent replay analyses can pick them up too *)
    DLS.get edges_to_ignore
    |> Option.iter ~f:(fun edges_to_ignore ->
           Procname.Map.find_opt callee_pname edges_to_ignore
           |> Option.iter ~f:(fun recursive_callees ->
                  Procname.Set.iter
                    (fun callee ->
                      Dependencies.record_pname_dep ~caller:callee_pname RecursionEdge callee )
                    recursive_callees ) ) ;
    let summary = Summary.OnDisk.store analysis_req summary in
    ActiveProcedures.remove {proc_name= callee_pname; specialization} ;
    if Option.is_some specialization then Stats.incr_summary_specializations () ;
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
    ActiveProcedures.remove {proc_name= callee_pname; specialization} ;
    log_elapsed_time () ;
    new_summary
  in
  let initial_callee_summary = preprocess () in
  try
    let callee_summary =
      if callee_attributes.ProcAttributes.is_defined then
        analyze exe_env analysis_req specialization initial_callee_summary callee_pdesc
      else initial_callee_summary
    in
    let final_callee_summary = postprocess callee_summary in
    (* don't forget to reset this so we output messages for future errors too *)
    DLS.set logged_error false ;
    final_callee_summary
  with exn -> (
    let backtrace = Printexc.get_backtrace () in
    IExn.reraise_if exn ~f:(fun () ->
        match exn with
        | RecursiveCycleException.RecursiveCycle _
        | RestartSchedulerException.ProcnameAlreadyLocked _
        | MissingDependencyException.MissingDependencyException ->
            decr nesting ;
            ActiveProcedures.remove {proc_name= callee_pname; specialization} ;
            true
        | exn ->
            if not (DLS.get logged_error) then (
              let source_file = callee_attributes.ProcAttributes.translation_unit in
              let location = callee_attributes.ProcAttributes.loc in
              L.internal_error "While analysing function %a:%a at %a, raised %s@\n" SourceFile.pp
                source_file Procname.pp callee_pname Location.pp_file_pos location
                (Exn.to_string exn) ;
              DLS.set logged_error true ) ;
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
let run_proc_analysis exe_env tenv analysis_req specialization ?caller_pname callee_pdesc =
  PerfEvent.(
    log (fun logger ->
        let callee_pname = Procdesc.get_proc_name callee_pdesc in
        log_begin_event logger ~name:"ondemand" ~categories:["backend"]
          ~arguments:[("proc", `String (Procname.to_string callee_pname))]
          () ) ) ;
  let summary =
    run_proc_analysis exe_env tenv analysis_req specialization ?caller_pname callee_pdesc
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


let is_summary_already_computed ~lazy_payloads (analysis_req : AnalysisRequest.t) callee_pname
    specialization =
  match (Summary.OnDisk.get ~lazy_payloads analysis_req callee_pname, specialization) with
  | Some ({is_complete_result= true} as summary), None ->
      `SummaryReady summary
  | Some ({payloads; is_complete_result= false} as summary), None -> (
    match analysis_req with
    | One payload_id when Payloads.has_payload payload_id payloads ->
        `SummaryReady summary
    | All | One _ | CheckerWithoutPayload _ ->
        `ComputeDefaultSummary )
  | None, Some specialization' ->
      `ComputeDefaultSummaryThenSpecialize specialization'
  | Some summary, Some specialization ->
      if Callbacks.is_specialized_for specialization summary then
        (* the current summary is specialized enough for this request *)
        `SummaryReady summary
      else if procedure_is_defined callee_pname then
        (* the current summary is not suitable for this specialization request *)
        `AddNewSpecialization (summary, specialization)
      else (* can we even get there? *) `UnknownProcedure
  | None, None ->
      if procedure_is_defined callee_pname then `ComputeDefaultSummary else `UnknownProcedure


let double_lock_for_restart ~lazy_payloads analysis_req callee_pname specialization =
  match Config.scheduler with
  | File | SyntacticCallGraph ->
      `NoSummary
  | Restart -> (
    match is_summary_already_computed ~lazy_payloads analysis_req callee_pname specialization with
    | `SummaryReady summary ->
        `YesSummary summary
    | `UnknownProcedure ->
        assert false
    | `ComputeDefaultSummary | `ComputeDefaultSummaryThenSpecialize _ | `AddNewSpecialization _ ->
        `NoSummary )


let analysis_result_of_option opt = Result.of_option opt ~error:AnalysisResult.AnalysisFailed

(** track how many times we restarted the analysis of the current dependency chain to make the
    analysis of mutual recursion cycles deterministic *)
let number_of_recursion_restarts = DLS.new_key (fun () -> 0)

let rec analyze_callee_can_raise_recursion exe_env ~lazy_payloads (analysis_req : AnalysisRequest.t)
    ~specialization ?caller_summary ?(from_file_analysis = false) callee_pname : _ AnalysisResult.t
    =
  match detect_mutual_recursion_cycle ~caller_summary ~callee:callee_pname specialization with
  | `InMutualRecursionCycle ->
      let target = {SpecializedProcname.proc_name= callee_pname; specialization} in
      let cycle_start, cycle_length, first_active = ActiveProcedures.get_cycle_start target in
      if
        DLS.get number_of_recursion_restarts >= Config.ondemand_recursion_restart_limit
        || SpecializedProcname.equal cycle_start target
      then (
        register_callee ~cycle_detected:true ?caller_summary callee_pname ;
        if Config.trace_ondemand then
          L.progress "Closed the cycle finishing in recursive call to %a@." Procname.pp callee_pname ;
        if
          DLS.get number_of_recursion_restarts >= Config.ondemand_recursion_restart_limit
          && not (SpecializedProcname.equal cycle_start target)
        then Stats.incr_ondemand_recursion_cycle_restart_limit_hit () ;
        Error MutualRecursionCycle )
      else (
        if Config.trace_ondemand then
          L.progress "Found cycle at %a, first_active= %a; restarting from %a@\nactives: %a@."
            Procname.pp callee_pname Procname.pp first_active.proc_name Procname.pp
            cycle_start.proc_name
            (Pp.seq ~sep:"," SpecializedProcname.pp)
            (ActiveProcedures.get_all ()) ;
        (* we want the exception to pop back up to the beginning of the cycle, so we set [ttl= cycle_length] *)
        Utils.with_dls number_of_recursion_restarts ~f:(( + ) 1) ;
        raise (RecursiveCycleException.RecursiveCycle {recursive= cycle_start; ttl= cycle_length}) )
  | `ReplayCycleCut ->
      register_callee ~cycle_detected:true ?caller_summary callee_pname ;
      if Config.trace_ondemand then
        L.progress "Closed the cycle finishing in recursive call to %a@." Procname.pp callee_pname ;
      Error MutualRecursionCycle
  | `NotInMutualRecursionCycle -> (
      register_callee ~cycle_detected:false ?caller_summary callee_pname ;
      if is_in_block_list callee_pname then Error InBlockList
      else
        let analyze_callee_aux specialization_context =
          error_if_ondemand_analysis_during_replay ~from_file_analysis caller_summary callee_pname ;
          RestartScheduler.with_lock ~get_actives:ActiveProcedures.get_all callee_pname
            ~f:(fun () ->
              (* the restart scheduler wants to avoid duplicated work, but between checking for a
                 summary and taking the lock on computing it someone else might have finished computing
                 the summary we want *)
              match
                double_lock_for_restart ~lazy_payloads analysis_req callee_pname specialization
              with
              | `YesSummary summary ->
                  Stats.incr_ondemand_double_analysis_prevented () ;
                  Some summary
              | `NoSummary ->
                  Procdesc.load callee_pname
                  >>= fun callee_pdesc ->
                  let previous_global_state = AnalysisGlobalState.save () in
                  protect
                    ~f:(fun () ->
                      (* preload tenv to avoid tainting preanalysis timing with IO *)
                      let tenv = Exe_env.get_proc_tenv exe_env callee_pname in
                      AnalysisGlobalState.initialize callee_pdesc tenv ;
                      Timer.time Preanalysis
                        ~f:(fun () ->
                          let caller_pname =
                            caller_summary >>| fun summ -> summ.Summary.proc_name
                          in
                          let summary =
                            run_proc_analysis exe_env tenv analysis_req specialization_context
                              ?caller_pname callee_pdesc
                          in
                          set_complete_result analysis_req summary ;
                          Some summary )
                        ~on_timeout:(fun span ->
                          L.debug Analysis Quiet
                            "TIMEOUT after %fs of CPU time analyzing %a:%a, outside of any \
                             checkers (pre-analysis timeout?)@\n"
                            span SourceFile.pp
                            (Procdesc.get_attributes callee_pdesc).translation_unit Procname.pp
                            callee_pname ;
                          None ) )
                    ~finally:(fun () -> AnalysisGlobalState.restore previous_global_state) )
        in
        match
          is_summary_already_computed ~lazy_payloads analysis_req callee_pname specialization
        with
        | `SummaryReady summary ->
            Ok summary
        | `ComputeDefaultSummary ->
            analyze_callee_aux None |> analysis_result_of_option
        | `ComputeDefaultSummaryThenSpecialize specialization ->
            (* recursive call so that we detect mutual recursion on the unspecialized summary *)
            analyze_callee exe_env ~lazy_payloads analysis_req ~specialization:None ?caller_summary
              ~from_file_analysis callee_pname
            |> Result.bind ~f:(fun summary ->
                   analyze_callee_aux (Some (summary, specialization)) |> analysis_result_of_option )
        | `AddNewSpecialization (summary, specialization) ->
            analyze_callee_aux (Some (summary, specialization)) |> analysis_result_of_option
        | `UnknownProcedure ->
            Error UnknownProcedure )


and on_recursive_cycle exe_env ~lazy_payloads analysis_req ?caller_summary:_ ?from_file_analysis
    ~ttl (cycle_start : SpecializedProcname.t) callee_pname =
  if ttl > 0 then
    raise (RecursiveCycleException.RecursiveCycle {recursive= cycle_start; ttl= ttl - 1}) ;
  analyze_callee exe_env ~lazy_payloads analysis_req ~specialization:cycle_start.specialization
    ?from_file_analysis cycle_start.proc_name
  |> ignore ;
  (* TODO: register caller -> callee relationship, possibly *)
  Summary.OnDisk.get ~lazy_payloads analysis_req callee_pname |> analysis_result_of_option


and analyze_callee exe_env ~lazy_payloads analysis_req ~specialization ?caller_summary
    ?from_file_analysis callee_pname =
  try
    analyze_callee_can_raise_recursion exe_env ~lazy_payloads analysis_req ~specialization
      ?caller_summary ?from_file_analysis callee_pname
  with RecursiveCycleException.RecursiveCycle {recursive; ttl} ->
    on_recursive_cycle ~lazy_payloads exe_env analysis_req recursive ~ttl callee_pname


let analyze_callee exe_env ~lazy_payloads analysis_req ~specialization ?caller_summary
    ?from_file_analysis callee_pname =
  (* If [caller_summary] is set then we are analyzing a dependency of another procedure, so we
     should keep counting restarts within that dependency chain (or cycle). If it's not set then
     this is a "toplevel" analysis of [callee_pname] so we start fresh. *)
  if Option.is_none caller_summary then DLS.set number_of_recursion_restarts 0 ;
  analyze_callee exe_env ~lazy_payloads analysis_req ~specialization ?caller_summary
    ?from_file_analysis callee_pname


let analyze_proc_name exe_env analysis_req ?specialization ~caller_summary callee_pname =
  analyze_callee ~lazy_payloads:false ~specialization exe_env analysis_req ~caller_summary
    callee_pname


let analyze_proc_name_for_file_analysis exe_env analysis_req callee_pname =
  (* load payloads lazily (and thus field by field as needed): we are either doing a file analysis
     and we don't want to load all payloads at once (to avoid high memory usage when only a few of
     the payloads are actually needed), or we are starting a procedure analysis in which case we're
     not interested in loading the summary if it has already been computed *)
  analyze_callee ~lazy_payloads:true ~specialization:None exe_env analysis_req
    ~from_file_analysis:true callee_pname


let analyze_file_procedures exe_env analysis_req procs_to_analyze source_file_opt =
  let saved_language = Language.get_language () in
  let analyze_proc_name_call pname =
    ignore
      (analyze_proc_name_for_file_analysis exe_env analysis_req pname : Summary.t AnalysisResult.t)
  in
  List.iter ~f:analyze_proc_name_call procs_to_analyze ;
  Option.iter source_file_opt ~f:(fun source_file ->
      if Config.dump_duplicate_symbols then dump_duplicate_procs source_file procs_to_analyze ;
      Callbacks.iterate_file_callbacks_and_store_issues procs_to_analyze exe_env source_file ) ;
  Language.set_language saved_language


(** Invoke all procedure-level and file-level callbacks on a given environment. *)
let analyze_file exe_env analysis_req source_file =
  update_taskbar None (Some source_file) ;
  let procs_to_analyze = SourceFiles.proc_names_of_source source_file in
  analyze_file_procedures exe_env analysis_req procs_to_analyze (Some source_file)


(** Invoke procedure callbacks on a given environment. *)
let analyze_proc_name_toplevel exe_env analysis_req ~specialization proc_name =
  update_taskbar (Some proc_name) None ;
  analyze_callee ~lazy_payloads:true ~specialization exe_env analysis_req proc_name |> ignore
