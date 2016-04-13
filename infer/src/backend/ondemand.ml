(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Module for on-demand analysis. *)

module L = Logging
module F = Format

let trace () = Config.from_env_variable "INFER_TRACE_ONDEMAND"

(** Name of the ondemand file *)
let ondemand_file () = Config.get_env_variable "INFER_ONDEMAND_FILE"

(** Read the directories to analyze from the ondemand file. *)
let read_dirs_to_analyze () =
  let lines_opt = match ondemand_file () with
    | None ->
        None
    | Some fname ->
        read_file (DB.source_file_to_abs_path (DB.source_file_from_string fname)) in
  match lines_opt with
  | None ->
      None
  | Some lines ->
      let res = ref StringSet.empty in
      let do_line line =
        let rel_file = DB.source_file_to_rel_path (DB.source_file_from_string line) in
        let source_dir = DB.source_dir_from_source_file (DB.source_file_from_string rel_file) in
        res := StringSet.add (DB.source_dir_to_string source_dir) !res in
      IList.iter do_line lines;
      Some !res

(** Directories to analyze from the ondemand file. *)
let dirs_to_analyze =
  lazy (read_dirs_to_analyze ())

type analyze_ondemand = Cfg.Procdesc.t -> unit

type get_proc_desc = Procname.t -> Cfg.Procdesc.t option

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

let should_be_analyzed proc_attributes proc_name =
  let currently_analyzed () =
    Specs.summary_exists proc_name &&
    Specs.is_active proc_name in
  let already_analyzed () =
    match Specs.get_summary proc_name with
    | Some summary ->
        Specs.get_timestamp summary > 0
    | None ->
        false in
  proc_attributes.ProcAttributes.is_defined && (* we have the implementation *)
  not (currently_analyzed ()) && (* avoid infinite loops *)
  not (already_analyzed ()) (* avoid re-analysis of the same procedure *)

let procedure_should_be_analyzed proc_name =
  match AttributesTable.load_attributes proc_name with
  | Some proc_attributes ->
      should_be_analyzed proc_attributes proc_name
  | None ->
      false

type global_state =
  {
    abs_val : int;
    abstraction_rules : Abs.rules;
    current_source : DB.source_file;
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
    current_source = !DB.current_source;
    delayed_prints = L.get_delayed_prints ();
    footprint_mode = !Config.footprint;
    html_formatter = !Printer.curr_html_formatter;
    name_generator = Ident.NameGenerator.get_current ();
    symexec_state = State.save_state ();
  }

let restore_global_state st =
  Config.abs_val := st.abs_val;
  Abs.set_current_rules st.abstraction_rules;
  DB.current_source := st.current_source;
  L.set_delayed_prints st.delayed_prints;
  Config.footprint := st.footprint_mode;
  Printer.curr_html_formatter := st.html_formatter;
  Ident.NameGenerator.set_current st.name_generator;
  State.restore_state st.symexec_state;
  Timeout.resume_previous_timeout ()


let run_proc_analysis ~propagate_exceptions analyze_proc curr_pdesc callee_pdesc =
  let curr_pname = Cfg.Procdesc.get_proc_name curr_pdesc in
  let callee_pname = Cfg.Procdesc.get_proc_name callee_pdesc in

  (** Dot means start of a procedure *)
  L.log_progress_procedure ();
  if trace () then L.stderr "[%d] run_proc_analysis %a -> %a@."
      !nesting
      Procname.pp curr_pname
      Procname.pp callee_pname;

  let preprocess () =
    incr nesting;
    let attributes_opt =
      Specs.proc_resolve_attributes callee_pname in
    Option.may
      (fun attribute ->
         DB.current_source := attribute.ProcAttributes.loc.Location.file;
         let attribute_pname = attribute.ProcAttributes.proc_name in
         if not (Procname.equal callee_pname attribute_pname) then
           failwith ("ERROR: "^(Procname.to_string callee_pname)
                     ^" not equal to "^(Procname.to_string attribute_pname)))
      attributes_opt;
    let call_graph =
      let cg = Cg.create () in
      Cg.add_defined_node cg callee_pname;
      cg in
    Specs.reset_summary call_graph callee_pname attributes_opt;
    Specs.set_status callee_pname Specs.ACTIVE in

  let postprocess () =
    decr nesting;
    let summary = Specs.get_summary_unsafe "ondemand" callee_pname in
    let summary' =
      { summary with
        Specs.status = Specs.INACTIVE;
        timestamp = summary.Specs.timestamp + 1 } in
    Specs.add_summary callee_pname summary';
    Checkers.ST.store_summary callee_pname;
    Printer.write_proc_html false callee_pdesc in

  let log_error_and_continue exn kind =
    Reporting.log_error callee_pname exn;
    let prev_summary = Specs.get_summary_unsafe "Ondemand.do_analysis" callee_pname in
    let timestamp = max 1 (prev_summary.Specs.timestamp) in
    let stats = { prev_summary.Specs.stats with Specs.stats_failure = Some kind } in
    let payload =
      { prev_summary.Specs.payload with Specs.preposts = Some []; } in
    let new_summary =
      { prev_summary with Specs.stats; payload; timestamp; } in
    Specs.add_summary callee_pname new_summary in

  let old_state = save_global_state () in
  preprocess ();
  try
    analyze_proc callee_pdesc;
    postprocess ();
    restore_global_state old_state;
  with exn ->
    L.stderr "@.ONDEMAND EXCEPTION %a %s@.@.CALL STACK@.%s@.BACK TRACE@.%s@."
      Procname.pp callee_pname
      (Printexc.to_string exn)
      (Printexc.raw_backtrace_to_string (Printexc.get_callstack 1000))
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
          log_error_and_continue exn (FKcrash (Printexc.to_string exn))


let analyze_proc_desc ~propagate_exceptions curr_pdesc callee_pdesc =
  let callee_pname = Cfg.Procdesc.get_proc_name callee_pdesc in
  let proc_attributes = Cfg.Procdesc.get_attributes callee_pdesc in
  match !callbacks_ref with
  | Some callbacks
    when should_be_analyzed proc_attributes callee_pname ->
      run_proc_analysis
        ~propagate_exceptions callbacks.analyze_ondemand curr_pdesc callee_pdesc
  | _ -> ()



(** analyze_proc_name curr_pdesc proc_name
    performs an on-demand analysis of proc_name
    triggered during the analysis of curr_pname. *)
let analyze_proc_name ~propagate_exceptions curr_pdesc callee_pname =

  match !callbacks_ref with
  | Some callbacks
    when procedure_should_be_analyzed callee_pname ->
      begin
        match callbacks.get_proc_desc callee_pname with
        | Some callee_pdesc ->
            analyze_proc_desc ~propagate_exceptions curr_pdesc callee_pdesc
        | None ->
            ()
      end
  | _ ->
      () (* skipping *)

(** Find a proc desc for the procedure, perhaps loading it from disk. *)
let get_proc_desc callee_pname =
  match !callbacks_ref with
  | Some callbacks ->
      callbacks.get_proc_desc callee_pname
  | None ->
      None
