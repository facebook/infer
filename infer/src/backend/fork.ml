(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging
module F = Format
open Utils

module WeightedPnameSet =
  Set.Make(struct
    type t = (Procname.t * Cg.in_out_calls)
    let compare
        ((pn1: Procname.t), (calls1: Cg.in_out_calls))
        ((pn2: Procname.t), (calls2: Cg.in_out_calls)) =
      let n = int_compare calls1.Cg.in_calls calls2.Cg.in_calls in if n != 0 then n
      else Procname.compare pn1 pn2
  end)

let pp_weightedpnameset fmt set =
  let f (pname, weight) = F.fprintf fmt "%a@ " Procname.pp pname in
  WeightedPnameSet.iter f set

let compute_weighed_pnameset gr =
  let pnameset = ref WeightedPnameSet.empty in
  let add_pname_calls (pn, calls) =
    pnameset := WeightedPnameSet.add (pn, calls) !pnameset in
  IList.iter add_pname_calls (Cg.get_nodes_and_calls gr);
  !pnameset

(* Return true if there are no children of [pname] whose specs
   have changed since [pname] was last analyzed. *)
let proc_is_up_to_date gr pname =
  match Specs.get_summary pname with
  | None -> false
  | Some summary ->
      let filter dependent_proc =
        Specs.get_timestamp summary =
        Procname.Map.find dependent_proc summary.Specs.dependency_map in
      Procname.Set.for_all filter (Cg.get_defined_children gr pname)

(** Return the list of procedures which should perform a phase
    transition from [FOOTPRINT] to [RE_EXECUTION] *)
let should_perform_transition gr proc_name : Procname.t list =
  let recursive_dependents = Cg.get_recursive_dependents gr proc_name in
  let recursive_dependents_plus_self = Procname.Set.add proc_name recursive_dependents in
  let should_transition =
    Specs.get_phase proc_name == Specs.FOOTPRINT &&
    Procname.Set.for_all (proc_is_up_to_date gr) recursive_dependents in
  if should_transition then Procname.Set.elements recursive_dependents_plus_self
  else []

(** Perform the transition from [FOOTPRINT] to [RE_EXECUTION] in spec table *)
let transition_footprint_re_exe proc_name joined_pres =
  L.out "Transition %a from footprint to re-exe@." Procname.pp proc_name;
  let summary = Specs.get_summary_unsafe "transition_footprint_re_exe" proc_name in
  let summary' =
    if !Config.only_footprint then
      { summary with
        Specs.phase = Specs.RE_EXECUTION;
      }
    else
      let specs =
        IList.map
          (fun jp ->
             Specs.spec_normalize
               { Specs.pre = jp;
                 posts = [];
                 visited = Specs.Visitedset.empty })
          joined_pres in
      let payload =
        { summary.Specs.payload with
          Specs.preposts = Some specs; } in
      let dependency_map =
        Specs.re_initialize_dependency_map summary.Specs.dependency_map in
      { summary with
        Specs.timestamp = 0;
        phase = Specs.RE_EXECUTION;
        dependency_map;
        payload;
      } in
  Specs.add_summary proc_name summary'

module SpecMap = Map.Make (struct
    type t = Prop.normal Specs.Jprop.t
    let compare = Specs.Jprop.compare
  end)

(** Update the specs of the current proc after the execution of one phase *)
let update_specs proc_name (new_specs : Specs.NormSpec.t list) : Specs.NormSpec.t list * bool =
  let new_specs = Specs.normalized_specs_to_specs new_specs in
  let phase = Specs.get_phase proc_name in
  let old_specs = Specs.get_specs proc_name in
  let changed = ref false in
  let current_specs =
    ref
      (IList.fold_left
         (fun map spec ->
            SpecMap.add
              spec.Specs.pre
              (Paths.PathSet.from_renamed_list spec.Specs.posts, spec.Specs.visited) map)
         SpecMap.empty old_specs) in
  let re_exe_filter old_spec = (* filter out pres which failed re-exe *)
    if phase == Specs.RE_EXECUTION &&
       not (IList.exists
              (fun new_spec -> Specs.Jprop.equal new_spec.Specs.pre old_spec.Specs.pre)
              new_specs)
    then begin
      changed:= true;
      L.out "Specs changed: removing pre of spec@\n%a@." (Specs.pp_spec pe_text None) old_spec;
      current_specs := SpecMap.remove old_spec.Specs.pre !current_specs end
    else () in
  let add_spec spec = (* add a new spec by doing union of the posts *)
    try
      let old_post, old_visited = SpecMap.find spec.Specs.pre !current_specs in
      let new_post, new_visited =
        Paths.PathSet.union
          old_post
          (Paths.PathSet.from_renamed_list spec.Specs.posts),
        Specs.Visitedset.union old_visited spec.Specs.visited in
      if not (Paths.PathSet.equal old_post new_post) then begin
        changed := true;
        L.out "Specs changed: added new post@\n%a@."
          (Propset.pp pe_text (Specs.Jprop.to_prop spec.Specs.pre))
          (Paths.PathSet.to_propset new_post);
        current_specs :=
          SpecMap.add spec.Specs.pre (new_post, new_visited)
            (SpecMap.remove spec.Specs.pre !current_specs) end

    with Not_found ->
      changed := true;
      L.out "Specs changed: added new pre@\n%a@." (Specs.Jprop.pp_short pe_text) spec.Specs.pre;
      current_specs :=
        SpecMap.add
          spec.Specs.pre
          ((Paths.PathSet.from_renamed_list spec.Specs.posts), spec.Specs.visited)
          !current_specs in
  let res = ref [] in
  let convert pre (post_set, visited) =
    res :=
      Specs.spec_normalize
        { Specs.pre = pre;
          Specs.posts = Paths.PathSet.elements post_set;
          Specs.visited = visited }:: !res in
  IList.iter re_exe_filter old_specs; (* filter out pre's which failed re-exe *)
  IList.iter add_spec new_specs; (* add new specs *)
  SpecMap.iter convert !current_specs;
  !res,!changed

let tot_procs = ref 0 (** Total number of procedures to analyze *)
let num_procs_done = ref 0 (** Number of procedures done *)
let wpnames_todo = ref WeightedPnameSet.empty (** Weighted pnames (procedure names with weight) to do *)
let tot_files = ref 1 (** Total number of files in all the clusters *)
let tot_files_done = ref 0 (** Total number of files done so far *)
let this_cluster_files = ref 1 (** Number of files in the current cluster *)

(** Return true if [pname] is done and requires no further analysis *)
let proc_is_done gr pname =
  not (WeightedPnameSet.mem (pname, Cg.get_calls gr pname) !wpnames_todo)

(** flag to activate tracing of the algorithm *)
let trace = ref false

(** Return true if [pname] has just become done *)
let procs_become_done gr pname : Procname.t list =
  let recursive_dependents = Cg.get_recursive_dependents gr pname in
  let nonrecursive_dependents = Cg.get_nonrecursive_dependents gr pname in
  let summary = Specs.get_summary_unsafe "procs_become_done" pname in
  let is_done = Specs.get_timestamp summary <> 0 &&
                (!Config.only_footprint || Specs.get_phase pname == Specs.RE_EXECUTION) &&
                Procname.Set.for_all (proc_is_done gr) nonrecursive_dependents &&
                Procname.Set.for_all (proc_is_up_to_date gr) recursive_dependents in
  if !trace then L.err "proc is%s done@." (if is_done then "" else " not");
  if is_done
  then
    let procs_to_remove =
      (* the proc itself if not recursive, otherwise all the recursive circle *)
      Procname.Set.add pname recursive_dependents in
    Procname.Set.elements procs_to_remove
  else []

let post_process_procs exe_env procs_done =
  let check_no_specs pn =
    if Specs.get_specs pn = [] then begin
      Errdesc.warning_err
        (Specs.get_summary_unsafe "post_process_procs" pn).Specs.attributes.ProcAttributes.loc
        "No specs found for %a@." Procname.pp pn
    end in
  let cg = Exe_env.get_cg exe_env in
  IList.iter (fun pn ->
      let elem = (pn, Cg.get_calls cg pn) in
      if WeightedPnameSet.mem elem !wpnames_todo then
        begin
          incr num_procs_done;
          wpnames_todo := WeightedPnameSet.remove (pn, Cg.get_calls cg pn) !wpnames_todo;
          let whole_seconds = false in
          check_no_specs pn;
          Printer.proc_write_log whole_seconds (Exe_env.get_cfg exe_env pn) pn
        end
    ) procs_done

(** Find the max string in the [set] which satisfies [filter],and count the number of attempts.
    Precedence is given to strings in [priority_set] *)
let filter_max exe_env cg filter set priority_set =
  let rec find_max n filter set =
    let elem = WeightedPnameSet.max_elt set in
    if filter elem then
      begin
        let proc_name = fst elem in
        Config.footprint := Specs.get_phase proc_name = Specs.FOOTPRINT;
        let file_name = Exe_env.get_source exe_env proc_name in
        let action = if !Config.footprint then "Discovering" else "Verifying" in
        let pp_cluster_info fmt () =
          let files_done_previous_clusters = float_of_int !tot_files_done in
          let ratio_procs_this_cluster =
            (float_of_int !num_procs_done) /. (float_of_int !tot_procs) in
          let files_done_this_cluster =
            (float_of_int !this_cluster_files) *. ratio_procs_this_cluster in
          let files_done = files_done_previous_clusters +. files_done_this_cluster in
          let perc_total = 100. *. files_done /. (float_of_int !tot_files) in
          F.fprintf fmt " (%3.2f%% total)" perc_total in
        L.err "@\n**%s specs: %a file: %s@\n"
          action Procname.pp proc_name (DB.source_file_to_string file_name);
        L.err "  %d/%d procedures done%a@."
          !num_procs_done !tot_procs pp_cluster_info ();
        elem
      end
    else
      begin
        find_max (n + 1) filter (WeightedPnameSet.remove elem set)
      end in
  try
    (* try with priority elements first *)
    find_max 1 filter (WeightedPnameSet.inter set priority_set)
  with Not_found -> find_max 1 filter set

(** Handle timeout events *)
module Timeout : sig
  (** execute the function up to a given timeout given by the parameter *)
  val exe_timeout : int -> ('a -> unit) -> 'a -> failure_kind option
end = struct
  let set_alarm nsecs =
    match Config.os_type with
    | Config.Unix | Config.Cygwin ->
        ignore (Unix.setitimer Unix.ITIMER_REAL
                  { Unix.it_interval = 3.0; (* try again after 3 seconds if the signal is lost *)
                    Unix.it_value = float_of_int nsecs })
    | Config.Win32 ->
        SymOp.set_wallclock_alarm nsecs

  let unset_alarm () =
    match Config.os_type with
    | Config.Unix | Config.Cygwin -> set_alarm 0
    | Config.Win32 -> SymOp.unset_wallclock_alarm ()

  let timeout_action _ =
    unset_alarm ();
    raise (Analysis_failure_exe (FKtimeout))

  let () = begin
    match Config.os_type with
    | Config.Unix | Config.Cygwin ->
        Sys.set_signal Sys.sigvtalrm (Sys.Signal_handle timeout_action);
        Sys.set_signal Sys.sigalrm (Sys.Signal_handle timeout_action)
    | Config.Win32 ->
        SymOp.set_wallclock_timeout_handler timeout_action;
        (* use the Gc alarm for periodic timeout checks *)
        ignore (Gc.create_alarm SymOp.check_wallclock_alarm)
  end

  let current_timeouts = ref []

  let exe_timeout iterations f x =
    let restore_timeout () =
      match !current_timeouts with
      | prev_iterations :: _ ->
          set_alarm prev_iterations
      | [] ->
          () in
    let unwind () =
      let pop () = match !current_timeouts with
        | _ :: l -> current_timeouts := l
        | [] -> () in
      unset_alarm ();
      SymOp.unset_alarm ();
      pop () in
    let before () =
      current_timeouts := iterations :: !current_timeouts;
      set_iterations iterations;
      set_alarm (get_timeout_seconds ());
      SymOp.set_alarm () in
    let after () =
      unwind ();
      restore_timeout () in
    try
      before ();
      f x;
      after ();
      None
    with
    | Analysis_failure_exe kind ->
        after ();
        Errdesc.warning_err (State.get_loc ()) "TIMEOUT: %a@." pp_failure_kind kind;
        Some kind
    | exe ->
        after ();
        raise exe
end


(** Main algorithm responsible for driving the analysis of an Exe_env (set of procedures).
    The algorithm computes dependencies between procedures,
    propagates results, and handles fixpoints in the call graph. *)
let main_algorithm exe_env analyze_proc filter_out process_result : unit =
  let call_graph = Exe_env.get_cg exe_env in
  let filter_initial (pname, w) =
    let summary = Specs.get_summary_unsafe "main_algorithm" pname in
    Specs.get_timestamp summary = 0 in
  wpnames_todo := WeightedPnameSet.filter filter_initial (compute_weighed_pnameset call_graph);
  let wpnames_address_of = (* subset of the procedures whose address is taken *)
    let address_taken_of n =
      Procname.Set.mem n (Cfg.get_priority_procnames (Exe_env.get_cfg exe_env n)) in
    WeightedPnameSet.filter (fun (n, _) -> address_taken_of n) !wpnames_todo in
  tot_procs := WeightedPnameSet.cardinal !wpnames_todo;
  num_procs_done := 0;
  let max_timeout = ref 1 in
  let wpname_can_be_analyzed (pname, weight) : bool =
    (* Return true if [pname] is not up to date and it can be analyzed right now *)
    Procname.Set.for_all
      (proc_is_done call_graph) (Cg.get_nonrecursive_dependents call_graph pname) &&
    (Specs.get_timestamp (Specs.get_summary_unsafe "main_algorithm" pname) = 0
     || not (proc_is_up_to_date call_graph pname)) in
  let filter_out_ondemand pname =
    if !Config.ondemand_enabled then
      try
        let cfg = Exe_env.get_cfg exe_env pname in
        match Cfg.Procdesc.find_from_name cfg pname with
        | Some pdesc ->
            not (Ondemand.procedure_should_be_analyzed pdesc pname)
        | None ->
            false
      with Not_found -> false
    else
      false in
  let process_one_proc pname (calls: Cg.in_out_calls) =
    DB.current_source :=
      (Specs.get_summary_unsafe "main_algorithm" pname)
      .Specs.attributes.ProcAttributes.loc.Location.file;
    if !trace then
      begin
        let whole_seconds = false in
        L.err "@[<v 3>   *********** Summary of %a ***********@," Procname.pp pname;
        L.err "%a@]@\n"
          (Specs.pp_summary pe_text whole_seconds)
          (Specs.get_summary_unsafe "main_algorithm" pname)
      end;
    if filter_out call_graph pname ||
       filter_out_ondemand pname
    then
      post_process_procs exe_env [pname]
    else
      begin
        max_timeout := max (Specs.get_iterations pname) !max_timeout;
        Specs.update_dependency_map pname;
        process_result exe_env (pname, calls) (analyze_proc exe_env (pname, calls));
      end in
  while not (WeightedPnameSet.is_empty !wpnames_todo) do
    begin
      if !trace then begin
        let analyzable_pnames = WeightedPnameSet.filter wpname_can_be_analyzed !wpnames_todo in
        L.err "Nodes todo: %a@\n" pp_weightedpnameset !wpnames_todo;
        L.err "Analyzable procs: %a@\n" pp_weightedpnameset analyzable_pnames
      end;
      try
        let pname, calls =
          (** find max analyzable proc *)
          filter_max exe_env call_graph wpname_can_be_analyzed !wpnames_todo wpnames_address_of in
        process_one_proc pname calls
      with Not_found -> (* no analyzable procs *)
        L.err "Error: can't analyze any procs. Printing current spec table@\n@[<v>%a@]@."
          (Specs.pp_spec_table pe_text false) ();
        if !Config.ondemand_enabled then wpnames_todo := WeightedPnameSet.empty
        else raise (Failure "Stopping")
    end
  done

type analyze_proc = Exe_env.t -> Procname.t -> Specs.summary

type process_result = Exe_env.t -> (Procname.t * Cg.in_out_calls) -> Specs.summary -> unit

type filter_out = Cg.t -> Procname.t -> bool

(** Execute [analyze_proc] respecting dependencies between procedures,
    and apply [process_result] to the result of the analysis.
    If [filter_out] returns true, don't analyze the procedure. *)
let interprocedural_algorithm
    (exe_env: Exe_env.t)
    (_analyze_proc: analyze_proc)
    (_process_result: process_result)
    (filter_out: filter_out) : unit =
  let analyze_proc exe_env pname = (* wrap _analyze_proc and handle exceptions *)
    let log_error_and_continue exn kind =
      Reporting.log_error pname exn;
      let prev_summary = Specs.get_summary_unsafe "interprocedural_algorithm" pname in
      let timestamp = max 1 (prev_summary.Specs.timestamp) in
      let stats = { prev_summary.Specs.stats with Specs.stats_failure = Some kind } in
      let payload =
        { prev_summary.Specs.payload with Specs.preposts = Some []; } in
      { prev_summary with Specs.stats; payload; timestamp; } in

    try _analyze_proc exe_env pname with
    | Analysis_failure_exe kind as exn ->
        (* in production mode, log the timeout/crash and continue with the summary we had before
           the failure occurred *)
        log_error_and_continue exn kind
    | exn ->
        (* this happens due to exceptions from assert false or some other unrecognized exception *)
        log_error_and_continue exn (FKcrash (Printexc.to_string exn)) in

  let process_result exe_env (pname, calls) summary =
    (* wrap _process_result and handle exceptions *)
    try _process_result exe_env (pname, calls) summary with
    | exn ->
        let err_name, _, mloco, _, _, _, _ = Exceptions.recognize_exception exn in
        let err_str = "process_result raised " ^ (Localise.to_string err_name) in
        L.err "Error: %s@." err_str;
        let exn' = Exceptions.Internal_error (Localise.verbatim_desc err_str) in
        Reporting.log_error pname exn';
        post_process_procs exe_env [pname] in
  main_algorithm
    exe_env (fun exe_env (n, w) -> analyze_proc exe_env n) filter_out process_result
