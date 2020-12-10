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

module LocalCache = struct
  let results =
    lazy
      (Procname.LRUHash.create ~initial_size:Config.summaries_caches_max_size
         ~max_size:Config.summaries_caches_max_size)


  let clear () = Procname.LRUHash.clear (Lazy.force results)

  let remove pname = Procname.LRUHash.remove (Lazy.force results) pname

  let get proc_name =
    let summ_opt_opt = Procname.LRUHash.find_opt (Lazy.force results) proc_name in
    if Option.is_some summ_opt_opt then BackendStats.incr_ondemand_local_cache_hits ()
    else BackendStats.incr_ondemand_local_cache_misses () ;
    summ_opt_opt


  let add proc_name summary_option =
    Procname.LRUHash.replace (Lazy.force results) proc_name summary_option
end

(* always incremented before use *)
let nesting = ref (-1)

let max_nesting_to_print = 8

(* Remember what the last status sent was so that we can update the status correctly when entering
   and exiting nested ondemand analyses. In particular we need to remember the original time.*)
let current_taskbar_status : (Mtime.t * string) option ref = ref None

let is_active, add_active, remove_active, clear_actives =
  let currently_analyzed = ref Procname.Set.empty in
  let is_active proc_name = Procname.Set.mem proc_name !currently_analyzed
  and add_active proc_name = currently_analyzed := Procname.Set.add proc_name !currently_analyzed
  and remove_active proc_name =
    currently_analyzed := Procname.Set.remove proc_name !currently_analyzed
  and clear_actives () = currently_analyzed := Procname.Set.empty in
  (is_active, add_active, remove_active, clear_actives)


let already_analyzed proc_name =
  match Summary.OnDisk.get proc_name with
  | Some summary ->
      Summary.(Status.is_analyzed (get_status summary))
  | None ->
      false


let should_be_analyzed proc_attributes =
  proc_attributes.ProcAttributes.is_defined
  &&
  let proc_name = proc_attributes.ProcAttributes.proc_name in
  (not (is_active proc_name)) (* avoid infinite loops *) && not (already_analyzed proc_name)


let get_proc_attr proc_name =
  IList.force_until_first_some
    [lazy (Summary.OnDisk.proc_resolve_attributes proc_name); lazy (Topl.get_proc_attr proc_name)]


let procedure_should_be_analyzed proc_name =
  match get_proc_attr proc_name with
  | Some proc_attributes ->
      should_be_analyzed proc_attributes
  | None ->
      false


type global_state =
  { abs_val: int
  ; abstraction_rules: Abs.rules
  ; delayed_prints: L.delayed_prints
  ; footprint_mode: bool
  ; html_formatter: F.formatter
  ; name_generator: Ident.NameGenerator.t
  ; proc_analysis_time: (Mtime.Span.t * string) option
        (** the time elapsed doing [status] so far *)
  ; pulse_address_generator: PulseAbstractValue.State.t
  ; absint_state: AnalysisState.t
  ; biabduction_state: State.t }

let save_global_state () =
  Timeout.suspend_existing_timeout ~keep_symop_total:false ;
  (* use a new global counter for the callee *)
  { abs_val= !BiabductionConfig.abs_val
  ; abstraction_rules= Abs.get_current_rules ()
  ; delayed_prints= L.get_and_reset_delayed_prints ()
  ; footprint_mode= !BiabductionConfig.footprint
  ; html_formatter= !Printer.curr_html_formatter
  ; name_generator= Ident.NameGenerator.get_current ()
  ; proc_analysis_time=
      Option.map !current_taskbar_status ~f:(fun (t0, status) ->
          (Mtime.span t0 (Mtime_clock.now ()), status) )
  ; pulse_address_generator= PulseAbstractValue.State.get ()
  ; absint_state= AnalysisState.save ()
  ; biabduction_state= State.save_state () }


let restore_global_state st =
  BiabductionConfig.abs_val := st.abs_val ;
  Abs.set_current_rules st.abstraction_rules ;
  L.set_delayed_prints st.delayed_prints ;
  BiabductionConfig.footprint := st.footprint_mode ;
  Printer.curr_html_formatter := st.html_formatter ;
  Ident.NameGenerator.set_current st.name_generator ;
  PulseAbstractValue.State.set st.pulse_address_generator ;
  AnalysisState.restore st.absint_state ;
  State.restore_state st.biabduction_state ;
  current_taskbar_status :=
    Option.map st.proc_analysis_time ~f:(fun (suspended_span, status) ->
        (* forget about the time spent doing a nested analysis and resend the status of the outer
           analysis with the updated "original" start time *)
        let new_t0 = Mtime.sub_span (Mtime_clock.now ()) suspended_span in
        let new_t0 = Option.value_exn new_t0 in
        !ProcessPoolState.update_status new_t0 status ;
        (new_t0, status) ) ;
  Timeout.resume_previous_timeout ()


(** reference to log errors only at the innermost recursive call *)
let logged_error = ref false

let update_taskbar callee_pdesc =
  let proc_name = Procdesc.get_proc_name callee_pdesc in
  let source_file = (Procdesc.get_attributes callee_pdesc).ProcAttributes.translation_unit in
  let t0 = Mtime_clock.now () in
  let status =
    let nesting =
      if !nesting <= max_nesting_to_print then String.make !nesting '>'
      else Printf.sprintf "%d>" !nesting
    in
    F.asprintf "%s%a: %a" nesting SourceFile.pp source_file Procname.pp proc_name
  in
  current_taskbar_status := Some (t0, status) ;
  !ProcessPoolState.update_status t0 status


let analyze exe_env callee_summary =
  let summary = Callbacks.iterate_procedure_callbacks exe_env callee_summary in
  BackendStats.incr_ondemand_procs_analyzed () ;
  summary


let run_proc_analysis exe_env ~caller_pdesc callee_pdesc =
  let callee_pname = Procdesc.get_proc_name callee_pdesc in
  let log_elapsed_time =
    let start_time = Mtime_clock.counter () in
    fun () ->
      L.(debug Analysis Medium)
        "Elapsed analysis time: %a: %a@\n" Procname.pp callee_pname Mtime.Span.pp
        (Mtime_clock.count start_time)
  in
  if Config.trace_ondemand then
    L.progress "[%d] run_proc_analysis %a -> %a@." !nesting (Pp.option Procname.pp)
      (Option.map caller_pdesc ~f:Procdesc.get_proc_name)
      Procname.pp callee_pname ;
  let preprocess () =
    incr nesting ;
    update_taskbar callee_pdesc ;
    Preanal.do_preanalysis exe_env callee_pdesc ;
    if Config.debug_mode then
      DotCfg.emit_proc_desc (Procdesc.get_attributes callee_pdesc).translation_unit callee_pdesc
      |> ignore ;
    let initial_callee_summary = Summary.OnDisk.reset callee_pdesc in
    add_active callee_pname ;
    initial_callee_summary
  in
  let postprocess summary =
    decr nesting ;
    Summary.OnDisk.store_analyzed summary ;
    remove_active callee_pname ;
    Printer.write_proc_html callee_pdesc ;
    log_elapsed_time () ;
    summary
  in
  let log_error_and_continue exn (summary : Summary.t) kind =
    BiabductionReporting.log_issue_using_state (Summary.get_proc_desc summary)
      (Summary.get_err_log summary) exn ;
    let stats = Summary.Stats.update summary.stats ~failure_kind:kind in
    let payloads =
      let biabduction =
        Some BiabductionSummary.{preposts= []; phase= summary.payloads.biabduction |> opt_get_phase}
      in
      {summary.payloads with biabduction}
    in
    let new_summary = {summary with stats; payloads} in
    Summary.OnDisk.store_analyzed new_summary ;
    remove_active callee_pname ;
    log_elapsed_time () ;
    new_summary
  in
  let old_state = save_global_state () in
  let initial_callee_summary = preprocess () in
  let attributes = Procdesc.get_attributes callee_pdesc in
  try
    let callee_summary =
      if attributes.ProcAttributes.is_defined then analyze exe_env initial_callee_summary
      else initial_callee_summary
    in
    let final_callee_summary = postprocess callee_summary in
    restore_global_state old_state ;
    (* don't forget to reset this so we output messages for future errors too *)
    logged_error := false ;
    final_callee_summary
  with exn -> (
    let backtrace = Printexc.get_backtrace () in
    IExn.reraise_if exn ~f:(fun () ->
        match exn with
        | TaskSchedulerTypes.ProcnameAlreadyLocked _ ->
            clear_actives () ;
            restore_global_state old_state ;
            true
        | _ ->
            if not !logged_error then (
              let source_file = attributes.ProcAttributes.translation_unit in
              let location = attributes.ProcAttributes.loc in
              L.internal_error "While analysing function %a:%a at %a@\n" SourceFile.pp source_file
                Procname.pp callee_pname Location.pp_file_pos location ;
              logged_error := true ) ;
            restore_global_state old_state ;
            not Config.keep_going ) ;
    L.internal_error "@\nERROR RUNNING BACKEND: %a %s@\n@\nBACK TRACE@\n%s@?" Procname.pp
      callee_pname (Exn.to_string exn) backtrace ;
    match exn with
    | SymOp.Analysis_failure_exe kind ->
        (* in production mode, log the timeout/crash and continue with the summary we had before
           the failure occurred *)
        log_error_and_continue exn initial_callee_summary kind
    | _ ->
        (* this happens with assert false or some other unrecognized exception *)
        log_error_and_continue exn initial_callee_summary (FKcrash (Exn.to_string exn)) )


(* shadowed for tracing *)
let run_proc_analysis exe_env ~caller_pdesc callee_pdesc =
  PerfEvent.(
    log (fun logger ->
        let callee_pname = Procdesc.get_proc_name callee_pdesc in
        log_begin_event logger ~name:"ondemand" ~categories:["backend"]
          ~arguments:[("proc", `String (Procname.to_string callee_pname))]
          () )) ;
  let summary = run_proc_analysis exe_env ~caller_pdesc callee_pdesc in
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


let register_callee ?caller_summary callee_pname =
  Option.iter
    ~f:(fun (summary : Summary.t) ->
      summary.callee_pnames <- Procname.Set.add callee_pname summary.callee_pnames )
    caller_summary


let get_proc_desc callee_pname =
  IList.force_until_first_some
    [ lazy (Procdesc.load callee_pname)
    ; lazy (Option.map ~f:Summary.get_proc_desc (Summary.OnDisk.get callee_pname))
    ; lazy (Topl.get_proc_desc callee_pname) ]


let analyze_callee exe_env ?caller_summary callee_pname =
  register_callee ?caller_summary callee_pname ;
  if is_active callee_pname then None
  else
    match LocalCache.get callee_pname with
    | Some callee_summary_option ->
        callee_summary_option
    | None ->
        let summ_opt =
          if procedure_should_be_analyzed callee_pname then
            match get_proc_desc callee_pname with
            | Some callee_pdesc ->
                RestartScheduler.lock_exn callee_pname ;
                let callee_summary =
                  run_proc_analysis exe_env
                    ~caller_pdesc:(Option.map ~f:Summary.get_proc_desc caller_summary)
                    callee_pdesc
                in
                RestartScheduler.unlock callee_pname ;
                Some callee_summary
            | None ->
                Summary.OnDisk.get callee_pname
          else Summary.OnDisk.get callee_pname
        in
        LocalCache.add callee_pname summ_opt ;
        summ_opt


let analyze_proc_name exe_env ~caller_summary callee_pname =
  analyze_callee exe_env ~caller_summary callee_pname


let analyze_proc_name_no_caller exe_env callee_pname =
  analyze_callee exe_env ?caller_summary:None callee_pname


let analyze_procedures exe_env procs_to_analyze source_file_opt =
  let saved_language = !Language.curr_language in
  let analyze_proc_name_call pname =
    ignore (analyze_proc_name_no_caller exe_env pname : Summary.t option)
  in
  List.iter ~f:analyze_proc_name_call procs_to_analyze ;
  Option.iter source_file_opt ~f:(fun source_file ->
      if Config.dump_duplicate_symbols then dump_duplicate_procs source_file procs_to_analyze ) ;
  Option.iter source_file_opt ~f:(fun source_file ->
      Callbacks.iterate_file_callbacks_and_store_issues procs_to_analyze exe_env source_file ) ;
  Language.curr_language := saved_language


(** Invoke all procedure-level and file-level callbacks on a given environment. *)
let analyze_file exe_env source_file =
  let procs_to_analyze = SourceFiles.proc_names_of_source source_file in
  analyze_procedures exe_env procs_to_analyze (Some source_file)


(** Invoke procedure callbacks on a given environment. *)
let analyze_proc_name_toplevel exe_env proc_name = analyze_procedures exe_env [proc_name] None
