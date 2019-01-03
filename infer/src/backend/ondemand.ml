(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for on-demand analysis. *)

module L = Logging
module F = Format

type analyze_ondemand = Summary.t -> Procdesc.t -> Summary.t

type callbacks = {exe_env: Exe_env.t; analyze_ondemand: analyze_ondemand}

let callbacks_ref = ref None

let cached_results = lazy (Typ.Procname.Hash.create 128)

let set_callbacks (callbacks : callbacks) = callbacks_ref := Some callbacks

let unset_callbacks () = callbacks_ref := None

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
  match Summary.get proc_name with
  | Some summary ->
      Summary.(Status.is_analyzed (get_status summary))
  | None ->
      false


let should_be_analyzed proc_attributes =
  proc_attributes.ProcAttributes.is_defined
  &&
  let proc_name = proc_attributes.ProcAttributes.proc_name in
  (not (is_active proc_name)) (* avoid infinite loops *) && not (already_analyzed proc_name)


let procedure_should_be_analyzed proc_name =
  match Summary.proc_resolve_attributes proc_name with
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
  ; symexec_state= State.save_state () }


let restore_global_state st =
  BiabductionConfig.abs_val := st.abs_val ;
  Abs.set_current_rules st.abstraction_rules ;
  L.set_delayed_prints st.delayed_prints ;
  BiabductionConfig.footprint := st.footprint_mode ;
  Printer.curr_html_formatter := st.html_formatter ;
  Ident.NameGenerator.set_current st.name_generator ;
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

let run_proc_analysis analyze_proc ~caller_pdesc callee_pdesc =
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
    let initial_summary = Summary.reset callee_pdesc in
    add_active callee_pname ; initial_summary
  in
  let postprocess summary =
    decr nesting ;
    Summary.store summary ;
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
        Some
          BiabductionSummary.{preposts= []; phase= summary.payloads.biabduction |> opt_get_phase}
      in
      {summary.payloads with biabduction}
    in
    let new_summary = {summary with stats; payloads} in
    Summary.store new_summary ; remove_active callee_pname ; log_elapsed_time () ; new_summary
  in
  let old_state = save_global_state () in
  let initial_summary = preprocess () in
  let attributes = Procdesc.get_attributes callee_pdesc in
  try
    let summary =
      if attributes.ProcAttributes.is_defined then analyze_proc initial_summary callee_pdesc
      else initial_summary
    in
    let final_summary = postprocess summary in
    restore_global_state old_state ;
    (* don't forget to reset this so we output messages for future errors too *)
    logged_error := false ;
    final_summary
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
        log_error_and_continue exn initial_summary kind
    | _ ->
        (* this happens with assert false or some other unrecognized exception *)
        log_error_and_continue exn initial_summary (FKcrash (Exn.to_string exn)) )


(* shadowed for tracing *)
let run_proc_analysis analyze_proc ~caller_pdesc callee_pdesc =
  PerfEvent.(
    log (fun logger ->
        let callee_pname = Procdesc.get_proc_name callee_pdesc in
        log_begin_event logger ~name:"ondemand" ~categories:["backend"]
          ~arguments:[("proc", `String (Typ.Procname.to_string callee_pname))]
          () )) ;
  let summary = run_proc_analysis analyze_proc ~caller_pdesc callee_pdesc in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  summary


let analyze_proc ?caller_pdesc callee_pdesc =
  let callbacks = Option.value_exn !callbacks_ref in
  (* wrap [callbacks.analyze_ondemand] to update the status bar *)
  let analyze_proc summary pdesc =
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
    callbacks.analyze_ondemand summary pdesc
  in
  Some (run_proc_analysis analyze_proc ~caller_pdesc callee_pdesc)


let hash_procname proc_name = Typ.Procname.to_unique_id proc_name |> Utils.string_crc_hex32

let memcache_get proc_name =
  if not Config.memcached then None
  else
    let key = hash_procname proc_name in
    Summary.SummaryServer.get ~key


let memcache_set proc_name summ =
  if Config.memcached then
    let key = hash_procname proc_name in
    Summary.SummaryServer.set ~key summ


let analyze_proc_desc ~caller_pdesc callee_pdesc =
  let callee_pname = Procdesc.get_proc_name callee_pdesc in
  if is_active callee_pname then None
  else
    let cache = Lazy.force cached_results in
    try Typ.Procname.Hash.find cache callee_pname with Caml.Not_found ->
      let summary_option, update_memcached =
        match memcache_get callee_pname with
        | Some summ_opt ->
            (summ_opt, false)
        | None ->
            let proc_attributes = Procdesc.get_attributes callee_pdesc in
            if should_be_analyzed proc_attributes then
              (analyze_proc ~caller_pdesc callee_pdesc, true)
            else (Summary.get callee_pname, true)
      in
      if update_memcached then memcache_set callee_pname summary_option ;
      Typ.Procname.Hash.add cache callee_pname summary_option ;
      summary_option


(** Find a proc desc for the procedure, perhaps loading it from disk. *)
let get_proc_desc callee_pname =
  match Procdesc.load callee_pname with
  | Some _ as pdesc_opt ->
      pdesc_opt
  | None ->
      Option.map ~f:Summary.get_proc_desc (Summary.get callee_pname)


(** analyze_proc_name ?caller_pdesc proc_name performs an on-demand analysis of proc_name triggered
    during the analysis of caller_pdesc *)
let analyze_proc_name ?caller_pdesc callee_pname =
  if is_active callee_pname then None
  else
    let cache = Lazy.force cached_results in
    try Typ.Procname.Hash.find cache callee_pname with Caml.Not_found ->
      let summary_option, update_memcached =
        match memcache_get callee_pname with
        | Some summ_opt ->
            (summ_opt, false)
        | None ->
            if procedure_should_be_analyzed callee_pname then
              match get_proc_desc callee_pname with
              | Some callee_pdesc ->
                  (analyze_proc ?caller_pdesc callee_pdesc, true)
              | None ->
                  (Summary.get callee_pname, true)
            else (
              EventLogger.log_skipped_pname (F.asprintf "%a" Typ.Procname.pp callee_pname) ;
              (Summary.get callee_pname, true) )
      in
      if update_memcached then memcache_set callee_pname summary_option ;
      Typ.Procname.Hash.add cache callee_pname summary_option ;
      summary_option


let clear_cache () = Typ.Procname.Hash.clear (Lazy.force cached_results)
