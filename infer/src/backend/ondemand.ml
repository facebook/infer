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

let exe_env_ref = ref None

module LocalCache = struct
  let results = lazy (Typ.Procname.Hash.create 128)

  let clear () = Typ.Procname.Hash.clear (Lazy.force results)

  let remove pname = Typ.Procname.Hash.remove (Lazy.force results) pname

  let get proc_name =
    let summ_opt_opt = Typ.Procname.Hash.find_opt (Lazy.force results) proc_name in
    if Option.is_some summ_opt_opt then BackendStats.incr_ondemand_local_cache_hits ()
    else BackendStats.incr_ondemand_local_cache_misses () ;
    summ_opt_opt


  let add proc_name summary_option =
    Typ.Procname.Hash.add (Lazy.force results) proc_name summary_option
end

let set_exe_env (env : Exe_env.t) = exe_env_ref := Some env

let unset_exe_env () = exe_env_ref := None

(* always incremented before use *)
let nesting = ref (-1)

let max_nesting_to_print = 8

(* Remember what the last status sent was so that we can update the status correctly when entering
   and exiting nested ondemand analyses. In particular we need to remember the original time.*)
let current_taskbar_status : (Mtime.t * string) option ref = ref None

let is_active, add_active, remove_active =
  let currently_analyzed = ref Typ.Procname.Set.empty in
  let is_active proc_name = Typ.Procname.Set.mem proc_name !currently_analyzed
  and add_active proc_name =
    currently_analyzed := Typ.Procname.Set.add proc_name !currently_analyzed
  and remove_active proc_name =
    currently_analyzed := Typ.Procname.Set.remove proc_name !currently_analyzed
  in
  (is_active, add_active, remove_active)


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
  | Some proc_attributes when Config.reactive_capture && not proc_attributes.is_defined ->
      (* try to capture procedure first *)
      let defined_proc_attributes = OndemandCapture.try_capture proc_attributes in
      Option.value_map ~f:should_be_analyzed ~default:false defined_proc_attributes
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
  ; pulse_address_generator: PulseAbstractValue.state
  ; symexec_state: State.t }

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
  ; pulse_address_generator= PulseAbstractValue.get_state ()
  ; symexec_state= State.save_state () }


let restore_global_state st =
  BiabductionConfig.abs_val := st.abs_val ;
  Abs.set_current_rules st.abstraction_rules ;
  L.set_delayed_prints st.delayed_prints ;
  BiabductionConfig.footprint := st.footprint_mode ;
  Printer.curr_html_formatter := st.html_formatter ;
  Ident.NameGenerator.set_current st.name_generator ;
  PulseAbstractValue.set_state st.pulse_address_generator ;
  State.restore_state st.symexec_state ;
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

let analyze callee_summary =
  BackendStats.incr_ondemand_procs_analyzed () ;
  let callee_pdesc = Summary.get_proc_desc callee_summary in
  let exe_env = Option.value_exn !exe_env_ref in
  let proc_name = Procdesc.get_proc_name callee_pdesc in
  let source_file = (Procdesc.get_attributes callee_pdesc).ProcAttributes.translation_unit in
  let t0 = Mtime_clock.now () in
  let status =
    let nesting =
      if !nesting <= max_nesting_to_print then String.make !nesting '>'
      else Printf.sprintf "%d>" !nesting
    in
    F.asprintf "%s%a: %a" nesting SourceFile.pp source_file Typ.Procname.pp proc_name
  in
  current_taskbar_status := Some (t0, status) ;
  !ProcessPoolState.update_status t0 status ;
  let summary = Callbacks.iterate_procedure_callbacks exe_env callee_summary in
  if Topl.is_active () then Topl.add_errors exe_env summary ;
  summary


let run_proc_analysis ~caller_pdesc callee_pdesc =
  let callee_pname = Procdesc.get_proc_name callee_pdesc in
  let log_elapsed_time =
    let start_time = Mtime_clock.counter () in
    fun () ->
      L.(debug Analysis Medium)
        "Elapsed analysis time: %a: %a@\n" Typ.Procname.pp callee_pname Mtime.Span.pp
        (Mtime_clock.count start_time)
  in
  if Config.trace_ondemand then
    L.progress "[%d] run_proc_analysis %a -> %a@." !nesting (Pp.option Typ.Procname.pp)
      (Option.map caller_pdesc ~f:Procdesc.get_proc_name)
      Typ.Procname.pp callee_pname ;
  let preprocess () =
    incr nesting ;
    let initial_callee_summary = Summary.OnDisk.reset callee_pdesc in
    add_active callee_pname ; initial_callee_summary
  in
  let postprocess summary =
    decr nesting ;
    Summary.OnDisk.store summary ;
    remove_active callee_pname ;
    Printer.write_proc_html callee_pdesc ;
    log_elapsed_time () ;
    summary
  in
  let log_error_and_continue exn (summary : Summary.t) kind =
    Reporting.log_error_using_state summary exn ;
    let stats = Summary.Stats.update summary.stats ~failure_kind:kind in
    let payloads =
      let biabduction =
        Some BiabductionSummary.{preposts= []; phase= summary.payloads.biabduction |> opt_get_phase}
      in
      {summary.payloads with biabduction}
    in
    let new_summary = {summary with stats; payloads} in
    Summary.OnDisk.store new_summary ;
    remove_active callee_pname ;
    log_elapsed_time () ;
    new_summary
  in
  let old_state = save_global_state () in
  let initial_callee_summary = preprocess () in
  let attributes = Procdesc.get_attributes callee_pdesc in
  try
    let callee_summary =
      if attributes.ProcAttributes.is_defined then analyze initial_callee_summary
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
        if not !logged_error then (
          let source_file = attributes.ProcAttributes.translation_unit in
          let location = attributes.ProcAttributes.loc in
          L.internal_error "While analysing function %a:%a at %a@\n" SourceFile.pp source_file
            Typ.Procname.pp callee_pname Location.pp_file_pos location ;
          logged_error := true ) ;
        restore_global_state old_state ;
        not Config.keep_going ) ;
    L.internal_error "@\nERROR RUNNING BACKEND: %a %s@\n@\nBACK TRACE@\n%s@?" Typ.Procname.pp
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
let run_proc_analysis ~caller_pdesc callee_pdesc =
  PerfEvent.(
    log (fun logger ->
        let callee_pname = Procdesc.get_proc_name callee_pdesc in
        log_begin_event logger ~name:"ondemand" ~categories:["backend"]
          ~arguments:[("proc", `String (Typ.Procname.to_string callee_pname))]
          () )) ;
  let summary = run_proc_analysis ~caller_pdesc callee_pdesc in
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
    Out_channel.with_file (Config.results_dir ^/ Config.duplicates_filename)
      ~append:true ~perm:0o666 ~f:(fun outc ->
        let fmt = F.formatter_of_out_channel outc in
        List.iter duplicate_procs ~f:(fun (pname, source_captured) ->
            F.fprintf fmt "DUPLICATE_SYMBOLS source:%a source_captured:%a pname:%a@\n" SourceFile.pp
              source_file SourceFile.pp source_captured Typ.Procname.pp pname ) ;
        F.pp_print_flush fmt () )
  in
  if not (List.is_empty duplicate_procs) then output_to_file duplicate_procs


let create_perf_stats_report source_file =
  PerfStats.register_report PerfStats.TimeAndMemory (PerfStats.Backend source_file) ;
  PerfStats.get_reporter (PerfStats.Backend source_file) ()


let register_callee ?caller_summary callee_pname =
  Option.iter
    ~f:(fun (summary : Summary.t) ->
      summary.callee_pnames <- Typ.Procname.Set.add callee_pname summary.callee_pnames )
    caller_summary


let get_proc_desc callee_pname =
  IList.force_until_first_some
    [ lazy (Procdesc.load callee_pname)
    ; lazy (Option.map ~f:Summary.get_proc_desc (Summary.OnDisk.get callee_pname))
    ; lazy (Topl.get_proc_desc callee_pname) ]


type callee = ProcName of Typ.Procname.t | ProcDesc of Procdesc.t

let proc_name_of_callee = function
  | ProcName proc_name ->
      proc_name
  | ProcDesc proc_desc ->
      Procdesc.get_proc_name proc_desc


let callee_should_be_analyzed = function
  | ProcName proc_name ->
      procedure_should_be_analyzed proc_name
  | ProcDesc proc_desc ->
      should_be_analyzed (Procdesc.get_attributes proc_desc)


let get_callee_proc_desc = function
  | ProcDesc proc_desc ->
      Some proc_desc
  | ProcName proc_name ->
      get_proc_desc proc_name


let analyze_callee ?caller_summary callee =
  let callee_pname = proc_name_of_callee callee in
  register_callee ?caller_summary callee_pname ;
  if is_active callee_pname then None
  else
    match LocalCache.get callee_pname with
    | Some callee_summary_option ->
        callee_summary_option
    | None ->
        let summ_opt =
          if callee_should_be_analyzed callee then
            match get_callee_proc_desc callee with
            | Some callee_pdesc ->
                Some
                  (run_proc_analysis
                     ~caller_pdesc:(Option.map ~f:Summary.get_proc_desc caller_summary)
                     callee_pdesc)
            | None ->
                Summary.OnDisk.get callee_pname
          else (
            EventLogger.log_skipped_pname (F.asprintf "%a" Typ.Procname.pp callee_pname) ;
            Summary.OnDisk.get callee_pname )
        in
        LocalCache.add callee_pname summ_opt ;
        summ_opt


let analyze_proc_desc ~caller_summary callee_pdesc =
  analyze_callee ~caller_summary (ProcDesc callee_pdesc)


let analyze_proc_name ~caller_summary callee_pname =
  analyze_callee ~caller_summary (ProcName callee_pname)


let analyze_proc_name_no_caller callee_pname =
  analyze_callee ?caller_summary:None (ProcName callee_pname)


let analyze_procedures exe_env procs_to_analyze source_file_opt =
  let saved_language = !Language.curr_language in
  Option.iter source_file_opt ~f:(fun source_file ->
      if Config.dump_duplicate_symbols then dump_duplicate_procs source_file procs_to_analyze ) ;
  set_exe_env exe_env ;
  let analyze_proc_name_call pname =
    ignore (analyze_proc_name_no_caller pname : Summary.t option)
  in
  List.iter ~f:analyze_proc_name_call procs_to_analyze ;
  Option.iter source_file_opt ~f:(fun source_file ->
      Callbacks.iterate_cluster_callbacks procs_to_analyze exe_env source_file ;
      create_perf_stats_report source_file ) ;
  unset_exe_env () ;
  Language.curr_language := saved_language


(** Invoke all procedure and cluster callbacks on a given environment. *)
let analyze_file (exe_env : Exe_env.t) source_file =
  let procs_to_analyze = SourceFiles.proc_names_of_source source_file in
  analyze_procedures exe_env procs_to_analyze (Some source_file)


(** Invoke procedure callbacks on a given environment. *)
let analyze_proc_name_toplevel (exe_env : Exe_env.t) proc_name =
  analyze_procedures exe_env [proc_name] None
