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

(** Optional set of source dirs to analyze in on-demand mode. If None then all source dirs
    will be analyzed *)
let dirs_to_analyze =
  let process_changed_files changed_files =
    SourceFile.Set.fold
      (fun source_file source_dir_set ->
         let source_dir = DB.source_dir_from_source_file source_file in
         String.Set.add source_dir_set (DB.source_dir_to_string source_dir)
      )
      changed_files String.Set.empty in
  Option.map ~f:process_changed_files SourceFile.changed_files_set

type analyze_ondemand = SourceFile.t -> Procdesc.t -> Specs.summary

type get_proc_desc = Typ.Procname.t -> Procdesc.t option

type callbacks =
  {
    analyze_ondemand : analyze_ondemand;
    get_proc_desc : get_proc_desc;
  }

let callbacks_ref = ref None

let set_callbacks (callbacks : callbacks) =
  callbacks_ref := Some callbacks

let unset_callbacks () =
  callbacks_ref := None

let nesting = ref 0

let should_be_analyzed proc_name proc_attributes =
  let currently_analyzed () =
    match Specs.get_summary proc_name with
    | None -> false
    | Some summary -> Specs.is_active summary in
  let already_analyzed () =
    match Specs.get_summary proc_name with
    | Some summary ->
        Specs.equal_status (Specs.get_status summary) Specs.Analyzed
    | None ->
        false in
  proc_attributes.ProcAttributes.is_defined && (* we have the implementation *)
  not (currently_analyzed ()) && (* avoid infinite loops *)
  not (already_analyzed ()) (* avoid re-analysis of the same procedure *)

let procedure_should_be_analyzed proc_name =
  match AttributesTable.load_attributes ~cache:true proc_name with
  | Some proc_attributes when Config.reactive_capture && not proc_attributes.is_defined ->
      (* try to capture procedure first *)
      let defined_proc_attributes = OndemandCapture.try_capture proc_attributes in
      Option.value_map ~f:(should_be_analyzed proc_name) ~default:false defined_proc_attributes
  | Some proc_attributes ->
      should_be_analyzed proc_name proc_attributes
  | None ->
      false

type global_state =
  {
    abs_val : int;
    abstraction_rules : Abs.rules;
    delayed_prints : L.print_action list;
    footprint_mode : bool;
    html_formatter : F.formatter;
    name_generator : Ident.NameGenerator.t;
    symexec_state : State.t
  }

let save_global_state () =
  Timeout.suspend_existing_timeout
    ~keep_symop_total:false; (* use a new global counter for the callee *)
  {
    abs_val = !Config.abs_val;
    abstraction_rules = Abs.get_current_rules ();
    delayed_prints = L.get_delayed_prints ();
    footprint_mode = !Config.footprint;
    html_formatter = !Printer.curr_html_formatter;
    name_generator = Ident.NameGenerator.get_current ();
    symexec_state = State.save_state ();
  }

let restore_global_state st =
  Config.abs_val := st.abs_val;
  Abs.set_current_rules st.abstraction_rules;
  L.set_delayed_prints st.delayed_prints;
  Config.footprint := st.footprint_mode;
  Printer.curr_html_formatter := st.html_formatter;
  Ident.NameGenerator.set_current st.name_generator;
  State.restore_state st.symexec_state;
  Timeout.resume_previous_timeout ()


let run_proc_analysis ~propagate_exceptions analyze_proc curr_pdesc callee_pdesc =
  let curr_pname = Procdesc.get_proc_name curr_pdesc in
  let callee_pname = Procdesc.get_proc_name callee_pdesc in

  let log_elapsed_time =
    let start_time = Unix.gettimeofday () in
    fun () ->
      let elapsed_time = Unix.gettimeofday () -. start_time in
      L.out "Elapsed analysis time: %a: %f\n"
        Typ.Procname.pp callee_pname
        elapsed_time in

  (* Dot means start of a procedure *)
  L.log_progress_procedure ();
  if Config.trace_ondemand then L.stderr "[%d] run_proc_analysis %a -> %a@."
      !nesting
      Typ.Procname.pp curr_pname
      Typ.Procname.pp callee_pname;

  let preprocess () =
    incr nesting;
    let attributes_opt =
      Specs.proc_resolve_attributes callee_pname in
    let source =
      Option.value_map
        ~f:(fun (attributes : ProcAttributes.t) ->
            let attribute_pname = attributes.proc_name in
            if not (Typ.Procname.equal callee_pname attribute_pname) then
              failwith ("ERROR: "^(Typ.Procname.to_string callee_pname)
                        ^" not equal to "^(Typ.Procname.to_string attribute_pname));
            attributes.loc.file)
        ~default:SourceFile.empty
        attributes_opt in
    let callee_pdesc_option =
      if Config.dynamic_dispatch = `Lazy
      then Some callee_pdesc
      else None in
    ignore (Specs.reset_summary callee_pname attributes_opt callee_pdesc_option);
    Specs.set_status callee_pname Specs.Active;
    source in

  let postprocess source summary =
    decr nesting;
    Specs.store_summary summary;
    Printer.write_proc_html source callee_pdesc;
    log_elapsed_time ();
    summary in

  let log_error_and_continue exn kind =
    Reporting.log_error callee_pname exn;
    let prev_summary = Specs.get_summary_unsafe "Ondemand.do_analysis" callee_pname in
    let stats = { prev_summary.Specs.stats with Specs.stats_failure = Some kind } in
    let payload =
      { prev_summary.Specs.payload with Specs.preposts = Some []; } in
    let new_summary = { prev_summary with Specs.stats; payload } in
    Specs.store_summary new_summary;
    log_elapsed_time ();
    new_summary in

  let old_state = save_global_state () in
  let source = preprocess () in
  try
    let summary =
      analyze_proc source callee_pdesc |> postprocess source in
    restore_global_state old_state;
    summary
  with exn ->
    L.stderr "@.ONDEMAND EXCEPTION %a %s@.@.BACK TRACE@.%s@?"
      Typ.Procname.pp callee_pname
      (Exn.to_string exn)
      (Printexc.get_backtrace ());
    restore_global_state old_state;
    if propagate_exceptions
    then
      raise exn
    else
      match exn with
      | SymOp.Analysis_failure_exe kind ->
          (* in production mode, log the timeout/crash and continue with the summary we had before
             the failure occurred *)
          log_error_and_continue exn kind
      | _ ->
          (* this happens with assert false or some other unrecognized exception *)
          log_error_and_continue exn (FKcrash (Exn.to_string exn))


let analyze_proc_desc ~propagate_exceptions curr_pdesc callee_pdesc : Specs.summary option =
  let callee_pname = Procdesc.get_proc_name callee_pdesc in
  let proc_attributes = Procdesc.get_attributes callee_pdesc in
  match !callbacks_ref with
  | None ->
      failwithf
        "No callbacks registered to analyze proc desc %a when analyzing %a@."
        Typ.Procname.pp callee_pname
        Typ.Procname.pp (Procdesc.get_proc_name curr_pdesc)
  | Some callbacks ->
      if should_be_analyzed callee_pname proc_attributes then
        Some (run_proc_analysis
                ~propagate_exceptions callbacks.analyze_ondemand curr_pdesc callee_pdesc)
      else
        Specs.get_summary callee_pname


(** analyze_proc_name curr_pdesc proc_name
    performs an on-demand analysis of proc_name
    triggered during the analysis of curr_pname. *)
let analyze_proc_name ~propagate_exceptions curr_pdesc callee_pname : Specs.summary option =
  match !callbacks_ref with
  | None ->
      failwithf
        "No callbacks registered to analyze proc name %a when analyzing %a@."
        Typ.Procname.pp callee_pname
        Typ.Procname.pp (Procdesc.get_proc_name curr_pdesc)
  | Some callbacks ->
      if procedure_should_be_analyzed callee_pname then
        begin
          match callbacks.get_proc_desc callee_pname with
          | Some callee_pdesc ->
              analyze_proc_desc ~propagate_exceptions curr_pdesc callee_pdesc
          | None -> None
        end
      else
        Specs.get_summary callee_pname


(** Find a proc desc for the procedure, perhaps loading it from disk. *)
let get_proc_desc callee_pname =
  match !callbacks_ref with
  | Some callbacks ->
      callbacks.get_proc_desc callee_pname
  | None ->
      None
