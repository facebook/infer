(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant

(** Module for on-demand analysis. *)

module L = Logging
module F = Format

type analyze_ondemand = Specs.summary -> Procdesc.t -> Specs.summary

type get_proc_desc = Typ.Procname.t -> Procdesc.t option

type callbacks = {analyze_ondemand: analyze_ondemand; get_proc_desc: get_proc_desc}

let callbacks_ref = ref None

let cached_results = lazy (Typ.Procname.Hash.create 128)

let set_callbacks (callbacks: callbacks) = callbacks_ref := Some callbacks

let unset_callbacks () = callbacks_ref := None

let nesting = ref 0

let is_active, add_active, remove_active =
  let currently_analyzed = ref Typ.Procname.Set.empty in
  let is_active proc_name = Typ.Procname.Set.mem proc_name !currently_analyzed
  and add_active proc_name =
    currently_analyzed := Typ.Procname.Set.add proc_name !currently_analyzed
  and remove_active proc_name =
    currently_analyzed := Typ.Procname.Set.remove proc_name !currently_analyzed
  in
  (is_active, add_active, remove_active)


let should_create_summary proc_name proc_attributes =
  match proc_name with
  | Typ.Procname.Java _ ->
      true
  | _ ->
      proc_attributes.ProcAttributes.is_defined


let should_be_analyzed proc_name proc_attributes =
  let already_analyzed () =
    match Specs.get_summary proc_name with
    | Some summary ->
        Specs.equal_status (Specs.get_status summary) Specs.Analyzed
    | None ->
        false
  in
  should_create_summary proc_name proc_attributes && not (is_active proc_name)
  && (* avoid infinite loops *)
     not (already_analyzed ())


let procedure_should_be_analyzed proc_name =
  match Specs.proc_resolve_attributes proc_name with
  | Some proc_attributes when Config.reactive_capture && not proc_attributes.is_defined ->
      (* try to capture procedure first *)
      let defined_proc_attributes = OndemandCapture.try_capture proc_attributes in
      Option.value_map ~f:(should_be_analyzed proc_name) ~default:false defined_proc_attributes
  | Some proc_attributes ->
      should_be_analyzed proc_name proc_attributes
  | None ->
      false


type global_state =
  { abs_val: int
  ; abstraction_rules: Abs.rules
  ; delayed_prints: L.print_action list
  ; footprint_mode: bool
  ; html_formatter: F.formatter
  ; name_generator: Ident.NameGenerator.t
  ; symexec_state: State.t }

let save_global_state () =
  Timeout.suspend_existing_timeout ~keep_symop_total:false ;
  (* use a new global counter for the callee *)
  { abs_val= !Config.abs_val
  ; abstraction_rules= Abs.get_current_rules ()
  ; delayed_prints= L.get_delayed_prints ()
  ; footprint_mode= !Config.footprint
  ; html_formatter= !Printer.curr_html_formatter
  ; name_generator= Ident.NameGenerator.get_current ()
  ; symexec_state= State.save_state () }


let restore_global_state st =
  Config.abs_val := st.abs_val ;
  Abs.set_current_rules st.abstraction_rules ;
  L.set_delayed_prints st.delayed_prints ;
  Config.footprint := st.footprint_mode ;
  Printer.curr_html_formatter := st.html_formatter ;
  Ident.NameGenerator.set_current st.name_generator ;
  State.restore_state st.symexec_state ;
  Timeout.resume_previous_timeout ()


let run_proc_analysis analyze_proc ~caller_pdesc callee_pdesc =
  let callee_pname = Procdesc.get_proc_name callee_pdesc in
  let log_elapsed_time =
    let start_time = Mtime_clock.counter () in
    fun () ->
      L.(debug Analysis Medium)
        "Elapsed analysis time: %a: %a@\n" Typ.Procname.pp callee_pname Mtime.Span.pp
        (Mtime_clock.count start_time)
  in
  L.progressbar_procedure () ;
  if Config.trace_ondemand then
    L.progress "[%d] run_proc_analysis %a -> %a@." !nesting (Pp.option Typ.Procname.pp)
      (Option.map caller_pdesc ~f:Procdesc.get_proc_name)
      Typ.Procname.pp callee_pname ;
  let preprocess () =
    incr nesting ;
    let initial_summary = Specs.reset_summary callee_pdesc in
    add_active callee_pname ; initial_summary
  in
  let postprocess summary =
    decr nesting ;
    Specs.store_summary summary ;
    remove_active callee_pname ;
    Printer.write_proc_html callee_pdesc ;
    log_elapsed_time () ;
    summary
  in
  let log_error_and_continue exn summary kind =
    Reporting.log_error summary exn ;
    let stats = {summary.Specs.stats with Specs.stats_failure= Some kind} in
    let payload = {summary.Specs.payload with Specs.preposts= Some []} in
    let new_summary = {summary with Specs.stats; payload} in
    Specs.store_summary new_summary ;
    remove_active callee_pname ;
    log_elapsed_time () ;
    new_summary
  in
  let old_state = save_global_state () in
  let initial_summary = preprocess () in
  try
    let attributes = Procdesc.get_attributes callee_pdesc in
    let summary =
      if attributes.ProcAttributes.is_defined then analyze_proc initial_summary callee_pdesc
      else initial_summary
    in
    let final_summary = postprocess summary in
    restore_global_state old_state ; final_summary
  with exn ->
    reraise_if exn ~f:(fun () -> restore_global_state old_state ; not Config.keep_going) ;
    L.internal_error "@\nERROR RUNNING BACKEND: %a %s@\n@\nBACK TRACE@\n%s@?" Typ.Procname.pp
      callee_pname (Exn.to_string exn) (Printexc.get_backtrace ()) ;
    match exn with
    | SymOp.Analysis_failure_exe kind ->
        (* in production mode, log the timeout/crash and continue with the summary we had before
            the failure occurred *)
        log_error_and_continue exn initial_summary kind
    | _ ->
        (* this happens with assert false or some other unrecognized exception *)
        log_error_and_continue exn initial_summary (FKcrash (Exn.to_string exn))


let analyze_proc_desc ?caller_pdesc callee_pdesc =
  let callee_pname = Procdesc.get_proc_name callee_pdesc in
  if is_active callee_pname then None
  else
    let cache = Lazy.force cached_results in
    try Typ.Procname.Hash.find cache callee_pname with Not_found ->
      let summary_option =
        let callbacks = Option.value_exn !callbacks_ref in
        let proc_attributes = Procdesc.get_attributes callee_pdesc in
        if should_be_analyzed callee_pname proc_attributes then
          Some (run_proc_analysis callbacks.analyze_ondemand ~caller_pdesc callee_pdesc)
        else Specs.get_summary callee_pname
      in
      Typ.Procname.Hash.add cache callee_pname summary_option ;
      summary_option


(** analyze_proc_name curr_pdesc proc_name performs an on-demand analysis of proc_name triggered
    during the analysis of curr_pname *)
let analyze_proc_name ?caller_pdesc callee_pname =
  if is_active callee_pname then None
  else
    let cache = Lazy.force cached_results in
    try Typ.Procname.Hash.find cache callee_pname with Not_found ->
      let summary_option =
        let callbacks = Option.value_exn !callbacks_ref in
        if procedure_should_be_analyzed callee_pname then
          match callbacks.get_proc_desc callee_pname with
          | Some callee_pdesc ->
              analyze_proc_desc ?caller_pdesc callee_pdesc
          | None ->
              Specs.get_summary callee_pname
        else Specs.get_summary callee_pname
      in
      Typ.Procname.Hash.add cache callee_pname summary_option ;
      summary_option


(** Find a proc desc for the procedure, perhaps loading it from disk. *)
let get_proc_desc callee_pname =
  match !callbacks_ref with Some callbacks -> callbacks.get_proc_desc callee_pname | None -> None
