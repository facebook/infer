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

let is_active, add_active, remove_active, clear_actives =
  let open Procname.HashSet in
  let currently_analyzed = create 0 in
  let is_active proc_name = mem currently_analyzed proc_name in
  let add_active proc_name = add proc_name currently_analyzed in
  let remove_active proc_name = remove proc_name currently_analyzed in
  let clear_actives () = clear currently_analyzed in
  (is_active, add_active, remove_active, clear_actives)


let procedure_should_be_analyzed proc_name =
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
  AnalysisGlobalState.register ~save:Timer.suspend ~restore:Timer.resume ~init:(fun () -> ())


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


let analyze exe_env ?specialization callee_summary callee_pdesc =
  let summary =
    Callbacks.iterate_procedure_callbacks exe_env ?specialization callee_summary callee_pdesc
  in
  Stats.incr_ondemand_procs_analyzed () ;
  summary


let run_proc_analysis exe_env tenv ?specialization ?caller_pname callee_pdesc =
  let callee_pname = Procdesc.get_proc_name callee_pdesc in
  let callee_attributes = Procdesc.get_attributes callee_pdesc in
  let log_elapsed_time =
    let start_time = Mtime_clock.counter () in
    fun () ->
      let elapsed = Mtime_clock.count start_time in
      let duration_us = IMtime.span_to_us_int elapsed in
      Stats.add_proc_duration_us (Procname.to_string callee_pname) duration_us ;
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
          Summary.OnDisk.reset callee_pname
      | Some (current_summary, _) ->
          current_summary
    in
    add_active callee_pname ;
    initial_callee_summary
  in
  let postprocess summary =
    decr nesting ;
    Summary.OnDisk.store summary ;
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
        Lazy.from_val
          (Some
             BiabductionSummary.
               {preposts= []; phase= payloads.biabduction |> Lazy.force |> opt_get_phase} )
      in
      {payloads with biabduction}
    in
    let new_summary = {summary with stats; payloads} in
    Summary.OnDisk.store new_summary ;
    remove_active callee_pname ;
    log_elapsed_time () ;
    new_summary
  in
  let initial_callee_summary = preprocess () in
  try
    let callee_summary =
      if callee_attributes.ProcAttributes.is_defined then
        let specialization = Option.map ~f:snd specialization in
        analyze exe_env ?specialization initial_callee_summary callee_pdesc
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
let run_proc_analysis exe_env tenv ?specialization ?caller_pname callee_pdesc =
  PerfEvent.(
    log (fun logger ->
        let callee_pname = Procdesc.get_proc_name callee_pdesc in
        log_begin_event logger ~name:"ondemand" ~categories:["backend"]
          ~arguments:[("proc", `String (Procname.to_string callee_pname))]
          () ) ) ;
  let summary = run_proc_analysis exe_env tenv ?specialization ?caller_pname callee_pdesc in
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


let register_callee ?caller_summary callee =
  Option.iter caller_summary ~f:(fun {Summary.proc_name= caller} ->
      Dependencies.record_pname_dep ~caller callee )


let analyze_callee exe_env ~lazy_payloads ?specialization ?caller_summary callee_pname =
  register_callee ?caller_summary callee_pname ;
  if is_active callee_pname then None
  else
    let analyze_callee_aux ~specialization =
      Procdesc.load callee_pname
      >>= fun callee_pdesc ->
      RestartScheduler.with_lock callee_pname ~f:(fun () ->
          let previous_global_state = AnalysisGlobalState.save () in
          AnalysisGlobalState.initialize callee_pname ;
          (* preload tenv to avoid tainting preanalysis timing with IO *)
          let tenv = Exe_env.get_proc_tenv exe_env callee_pname in
          protect
            ~f:(fun () ->
              Timer.time Preanalysis
                ~f:(fun () ->
                  let caller_pname = caller_summary >>| fun summ -> summ.Summary.proc_name in
                  Some (run_proc_analysis exe_env tenv ?specialization ?caller_pname callee_pdesc)
                  )
                ~on_timeout:(fun span ->
                  L.debug Analysis Quiet
                    "TIMEOUT after %fs of CPU time analyzing %a:%a, outside of any checkers \
                     (pre-analysis timeout?)@\n"
                    span SourceFile.pp (Procdesc.get_attributes callee_pdesc).translation_unit
                    Procname.pp callee_pname ;
                  None ) )
            ~finally:(fun () -> AnalysisGlobalState.restore previous_global_state) )
    in
    match (Summary.OnDisk.get ~lazy_payloads callee_pname, specialization) with
    | (Some _ as summ_opt), None ->
        summ_opt
    | None, Some _ ->
        L.die InternalError "specialization should always happend after regular analysis"
    | (Some summary as summ_opt), Some specialization
      when Callbacks.is_specialized_for specialization summary ->
        (* the current summary is specialized enough for this request *)
        summ_opt
    | Some summary, Some specialization when procedure_should_be_analyzed callee_pname ->
        (* the current summary is not suitable for this specialization request *)
        analyze_callee_aux ~specialization:(Some (summary, specialization))
    | None, None when procedure_should_be_analyzed callee_pname ->
        analyze_callee_aux ~specialization:None
    | _, _ ->
        None


let analyze_proc_name exe_env ?specialization ~caller_summary callee_pname =
  analyze_callee ~lazy_payloads:false ?specialization exe_env ~caller_summary callee_pname


let analyze_proc_name_no_caller exe_env callee_pname =
  (* load payloads lazily (and thus field by field as needed): we are either doing a file analysis
     and we don't want to load all payloads at once (to avoid high memory usage when only a few of
     the payloads are actually needed), or we are starting a procedure analysis in which case we're
     not interested in loading the summary if it has already been computed *)
  analyze_callee ~lazy_payloads:true exe_env callee_pname


let analyze_procedures exe_env procs_to_analyze source_file_opt =
  let saved_language = !Language.curr_language in
  let analyze_proc_name_call pname =
    ignore (analyze_proc_name_no_caller exe_env pname : Summary.t option)
  in
  List.iter ~f:analyze_proc_name_call procs_to_analyze ;
  Option.iter source_file_opt ~f:(fun source_file ->
      if Config.dump_duplicate_symbols then dump_duplicate_procs source_file procs_to_analyze ;
      Callbacks.iterate_file_callbacks_and_store_issues procs_to_analyze exe_env source_file ) ;
  Language.curr_language := saved_language


(** Invoke all procedure-level and file-level callbacks on a given environment. *)
let analyze_file exe_env source_file =
  update_taskbar None (Some source_file) ;
  let procs_to_analyze = SourceFiles.proc_names_of_source source_file in
  analyze_procedures exe_env procs_to_analyze (Some source_file)


(** Invoke procedure callbacks on a given environment. *)
let analyze_proc_name_toplevel exe_env proc_name =
  update_taskbar (Some proc_name) None ;
  analyze_procedures exe_env [proc_name] None
