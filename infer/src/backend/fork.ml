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

(* =============== START of module Process =============== *)

module type Process_signature =
sig
  type t
  type val_t = (Procname.t * Cg.in_out_calls) (** type of values sent to children *)
  val get_remaining_processes : unit -> t list (** return the list of remaining processes *)
  val kill_remaining_processes : unit -> unit (** kill the remaining processes *)
  val kill_process : t -> unit
  val get_node_calls : t -> val_t option
  val spawn_fun : (val_t -> Specs.summary) -> t
  val send_to_child : t -> val_t -> unit
  val receive_from_child : unit -> t * Specs.summary
  val get_last_input : t -> val_t
end

(** Implementation of the process interface for the simulator (processes are implemented just as functions) *)
module Process_simulate : Process_signature = struct
  type t = int
  type val_t = (Procname.t * Cg.in_out_calls)
  let count = ref 0
  let funs = Hashtbl.create 17

  let spawn_fun f =
    incr count;
    Hashtbl.add funs !count (f, None);
    !count

  let get_remaining_processes () =
    let ids = ref [] in
    Hashtbl.iter (fun id _ -> ids := id :: !ids) funs;
    list_rev !ids

  let kill_remaining_processes () =
    Hashtbl.clear funs

  let kill_process id =
    Hashtbl.remove funs id

  let get_node_calls p =
    try
      match Hashtbl.find funs p with
      | (f, Some (x, res)) -> Some x
      | _ -> None
    with Not_found -> None

  let send_to_child id (x : val_t) =
    let (f, _) = Hashtbl.find funs id in
    Hashtbl.replace funs id (f, Some (x, f x))

  let receive_from_child () : t * Specs.summary =
    let some_id = ref (- 1) in
    Hashtbl.iter (fun id _ -> some_id := id) funs;
    match Hashtbl.find funs !some_id with
    | (f, Some (x, res)) -> (!some_id, res)
    | _ -> assert false

  let get_last_input id =
    match Hashtbl.find funs id with
    | (f, Some (x, res)) -> x
    | _ -> assert false
end

(** Implementation of the process interface using fork and pipes *)
module Process_fork : Process_signature = struct

  let shared_in, shared_out = (* shared channel from children to parent *)
    let (read, write) = Unix.pipe ()
    in Unix.in_channel_of_descr read, Unix.out_channel_of_descr write

  type val_t = Procname.t * Cg.in_out_calls

  type pipe_str =
    { p2c_in : in_channel;
      p2c_out : out_channel;
      c2p_in : in_channel;
      c2p_out : out_channel;
      mutable input : val_t option;
      mutable pid : int }

  type t = pipe_str

  let processes = ref []

  let get_node_calls p_str =
    p_str.input

  let send_to_child p_str (v : val_t) =
    p_str.input <- Some v;
    Marshal.to_channel p_str.p2c_out v [];
    flush p_str.p2c_out

  let incr_process_count p_str =
    processes := p_str :: !processes
  (* ; F.printf "@.Number of processes: %d@." (list_length !processes) *)

  let decr_process_count pid =
    processes := list_filter (fun p_str' -> pid != p_str'.pid) !processes
  (* ; F.printf "@.Number of processes: %d@." (list_length !processes) *)

  let kill_process p_str =
    L.out "killing process %d@." p_str.pid;
    Unix.kill p_str.pid Sys.sigkill;
    Unix.close (Unix.descr_of_in_channel p_str.p2c_in);
    Unix.close (Unix.descr_of_out_channel p_str.p2c_out);
    Unix.close (Unix.descr_of_in_channel p_str.c2p_in);
    Unix.close (Unix.descr_of_out_channel p_str.c2p_out);
    ignore (Unix.waitpid [] p_str.pid);
    decr_process_count p_str.pid

  let get_remaining_processes () =
    !processes

  let kill_remaining_processes () =
    L.out "@.%d remaining processes@." (list_length !processes);
    list_iter kill_process !processes

  let rec receive_from_child () : t * Specs.summary =
    let sender_pid = input_binary_int shared_in in
    try
      let p_str = (* find which process sent its pid on the shared channel *)
        list_find (fun p_str -> p_str.pid = sender_pid) !processes in
      let (summ : Specs.summary) = Marshal.from_channel p_str.c2p_in in
      (p_str, summ)
    with Not_found ->
      L.err "@.ERROR: process %d was killed while trying to communicate with the parent@." sender_pid;
      receive_from_child () (* wait for communication from the next process *)

  let receive_from_parent p_str : val_t =
    Marshal.from_channel p_str.p2c_in

  let send_to_parent (p_str: t) (summ: Specs.summary) =
    output_binary_int shared_out p_str.pid; (* tell parent I'm sending the result *)
    flush shared_out;
    Marshal.to_channel p_str.c2p_out summ [];
    flush p_str.c2p_out

  let get_last_input p_str =
    match p_str.input with
    | None -> assert false
    | Some x -> x

  let spawn_fun (service_f : val_t -> Specs.summary) =
    let p_str =
      let (p2c_read, p2c_write) = Unix.pipe () in
      let (c2p_read, c2p_write) = Unix.pipe () in
      (* Unix.set_nonblock c2p_read; *)
      { p2c_in = Unix.in_channel_of_descr p2c_read;
        p2c_out = Unix.out_channel_of_descr p2c_write;
        c2p_in = Unix.in_channel_of_descr c2p_read;
        c2p_out = Unix.out_channel_of_descr c2p_write;
        input = None;
        pid = 0 } in
    let colour = L.next_colour () in
    match Unix.fork () with
    | 0 ->
        Config.in_child_process := true;
        p_str.pid <- Unix.getpid ();
        L.change_terminal_colour colour;
        L.out "@.STARTING PROCESS %d@." p_str.pid;
        let rec loop () =
          let n = receive_from_parent p_str in
          let res = service_f n in
          send_to_parent p_str res;
          loop () in
        loop ()
    | cid ->
        p_str.pid <- cid;
        incr_process_count p_str;
        p_str
end

(* =============== END of module Process =============== *)

let parallel_mode = ref false

(* =============== START of module Timing_log =============== *)
module Timing_log : sig
  val event_start : string -> unit
  val event_finish : string -> unit
  val print_timing : unit -> unit
end = struct
  type ev_kind = START | FINISH
  type event = { time : float; kind : ev_kind; name : string }

  let active = ref []
  let log = ref []
  let bogus_time = - 1000.0
  let bogus_event = { time = bogus_time; kind = START; name ="" }
  let last_event = ref bogus_event
  let initial_time = ref bogus_time
  let total_procs_time = ref 0.0
  let total_cores_time = ref 0.0

  let reset () =
    active := [];
    log := [];
    last_event := bogus_event;
    initial_time := bogus_time;
    total_procs_time := 0.0;
    total_cores_time := 0.0

  let expand_log event =
    let elapsed = event.time -. !last_event.time in
    let num_procs = list_length !active in
    let num_cores = min num_procs !Config.num_cores in
    match Pervasives.(=) !last_event bogus_event with
    | true ->
        last_event := event;
        initial_time := event.time;
    | false ->
        let label =
          list_fold_left (fun s name -> "\\n" ^ name ^s) "" (list_rev !active) in
        if !Config.write_dotty then log := (!last_event, label, event)::!log;
        total_procs_time := (float_of_int num_procs) *. elapsed +. !total_procs_time;
        total_cores_time := (float_of_int num_cores) *. elapsed +. !total_cores_time;
        last_event := event

  let event_start s =
    expand_log { time = (Unix.gettimeofday ()); kind = START; name = s };
    active := s :: !active;
    L.err "  %d cores active@." (list_length !active)

  let event_finish s =
    expand_log { time = (Unix.gettimeofday ()); kind = FINISH; name = s };
    active := list_filter (fun s' -> s' <> s) !active

  let print_timing () =
    let total_time = !last_event.time -. !initial_time in
    (*
    let avg_num_proc = !total_procs_time /. total_time in
    let avg_num_cores = !total_cores_time /. total_time in
    *)
    if !Config.write_dotty then begin
      let pp_event fmt event = match event.kind with
        | START -> F.fprintf fmt "\"%fs START %s\"" event.time event.name
        | FINISH -> F.fprintf fmt "\"%fs FINISH %s\"" event.time event.name in
      let pp_edge fmt (event1, label, event2) =
        let color = match event1.kind with
          | START -> "green"
          | FINISH -> "red" in
        F.fprintf fmt "%a -> %a [label=\"{%fs}%s\",color=\"%s\"]\n" pp_event event1 pp_event event2 (event2.time -. event1.time) label color in
      let outc = open_out (DB.filename_to_string (DB.Results_dir.path_to_filename DB.Results_dir.Abs_root ["timing.dot"])) in
      let fmt = F.formatter_of_out_channel outc in
      F.fprintf fmt "digraph {\n";
      list_iter (pp_edge fmt) !log;
      F.fprintf fmt "}@.";
      close_out outc;
    end;
    reset ();
    L.err "Analysis time: %fs@." total_time
end
(* =============== END of module Timing_log =============== *)

(** print the timing stats, and generate timing.dot if in dotty mode *)
let print_timing () =
  Timing_log.print_timing ()

module WeightedPnameSet =
  Set.Make(struct
    type t = (Procname.t * Cg.in_out_calls)
    let compare ((pn1: Procname.t), (calls1: Cg.in_out_calls)) ((pn2: Procname.t), (calls2: Cg.in_out_calls)) =
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
  list_iter add_pname_calls (Cg.get_nodes_and_calls gr);
  !pnameset

(* Return true if there are no children of [pname] whose specs
   have changed since [pname] was last analyzed. *)
let proc_is_up_to_date gr pname =
  match Specs.get_summary pname with
  | None -> false
  | Some summary ->
      let filter dependent_proc = Specs.get_timestamp summary =
                                  Procname.Map.find dependent_proc summary.Specs.dependency_map in
      let res =
        Specs.is_inactive pname &&
        Procname.Set.for_all filter (Cg.get_defined_children gr pname) in
      res

(** Return the list of procedures which should perform a phase
    transition from [FOOTPRINT] to [RE_EXECUTION] *)
let should_perform_transition gr proc_name : Procname.t list =
  let recursive_dependents = Cg.get_recursive_dependents gr proc_name in
  let recursive_dependents_plus_self = Procname.Set.add proc_name recursive_dependents in
  let should_transition =
    Specs.get_phase proc_name == Specs.FOOTPRINT &&
    Procname.Set.for_all (fun pn -> Specs.is_inactive pn) recursive_dependents &&
    Procname.Set.for_all (proc_is_up_to_date gr) recursive_dependents in
  if should_transition then Procname.Set.elements recursive_dependents_plus_self
  else []

(** Perform the transition from [FOOTPRINT] to [RE_EXECUTION] in spec table *)
let transition_footprint_re_exe proc_name joined_pres =
  L.out "Transition %a from footprint to re-exe@." Procname.pp proc_name;
  let summary = Specs.get_summary_unsafe proc_name in
  let summary' =
    if !Config.only_footprint then
      { summary with
        Specs.phase = Specs.RE_EXECUTION;
      }
    else
      { summary with
        Specs.timestamp = 0;
        Specs.phase = Specs.RE_EXECUTION;
        Specs.dependency_map = Specs.re_initialize_dependency_map summary.Specs.dependency_map;
        Specs.payload =
          let specs =
            list_map
              (fun jp ->
                 Specs.spec_normalize
                   { Specs.pre = jp;
                     Specs.posts = [];
                     Specs.visited = Specs.Visitedset.empty })
              joined_pres in
          Specs.PrePosts specs
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
      (list_fold_left
         (fun map spec ->
            SpecMap.add
              spec.Specs.pre
              (Paths.PathSet.from_renamed_list spec.Specs.posts, spec.Specs.visited) map)
         SpecMap.empty old_specs) in
  let re_exe_filter old_spec = (* filter out pres which failed re-exe *)
    if phase == Specs.RE_EXECUTION && not (list_exists (fun new_spec -> Specs.Jprop.equal new_spec.Specs.pre old_spec.Specs.pre) new_specs)
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
        L.out "Specs changed: added new post@\n%a@." (Propset.pp pe_text (Specs.Jprop.to_prop spec.Specs.pre)) (Paths.PathSet.to_propset new_post);
        current_specs := SpecMap.add spec.Specs.pre (new_post, new_visited) (SpecMap.remove spec.Specs.pre !current_specs) end

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
  list_iter re_exe_filter old_specs; (* filter out pre's which failed re-exe *)
  list_iter add_spec new_specs; (* add new specs *)
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

(** flag to activate tracing of the parallel algorithm *)
let trace = ref false

(** Return true if [pname] has just become done *)
let procs_become_done gr pname : Procname.t list =
  let recursive_dependents = Cg.get_recursive_dependents gr pname in
  let nonrecursive_dependents = Cg.get_nonrecursive_dependents gr pname in
  let summary = Specs.get_summary_unsafe pname in
  let is_done = Specs.get_timestamp summary <> 0 &&
                Specs.is_inactive pname &&
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
        (Specs.get_summary_unsafe pn).Specs.attributes.ProcAttributes.loc
        "No specs found for %a@." Procname.pp pn
    end in
  let cg = Exe_env.get_cg exe_env in
  list_iter (fun pn ->
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

(** Activate a check which ensures that multi-core mode gives the same result as one-core.
    If true, detect when a dependent proc is active (analyzed concurrently)
    and in that case wait for a process to terminate next *)
let one_core_compatibility_mode = ref true

(** Find the max string in the [set] which satisfies [filter], and count the number of attempts.
    Precedence is given to strings in [priority_set] *)
let filter_max exe_env cg filter set priority_set =
  let rec find_max n filter set =
    let elem = WeightedPnameSet.max_elt set in
    let check_one_core_compatibility () =
      if !one_core_compatibility_mode &&
         Procname.Set.exists (fun child -> Specs.is_active child) (Cg.get_dependents cg (fst elem))
      then raise Not_found in
    check_one_core_compatibility ();
    if filter elem then
      begin
        let proc_name = fst elem in
        Config.footprint := Specs.get_phase proc_name = Specs.FOOTPRINT;
        let file_name = Exe_env.get_source exe_env proc_name in
        let action = if !Config.footprint then "Discovering" else "Verifying" in
        let pp_cluster_info fmt () =
          let files_done_previous_clusters = float_of_int !tot_files_done in
          let ratio_procs_this_cluster = (float_of_int !num_procs_done) /. (float_of_int !tot_procs) in
          let files_done_this_cluster = (float_of_int !this_cluster_files) *. ratio_procs_this_cluster in
          let files_done = files_done_previous_clusters +. files_done_this_cluster in
          let perc_total = 100. *. files_done /. (float_of_int !tot_files) in
          F.fprintf fmt " (%3.2f%% total)" perc_total in
        L.err "@\n**%s specs: %a file: %s@\n" action Procname.pp proc_name (DB.source_file_to_string file_name);
        L.err "  %d/%d procedures done%a@." !num_procs_done !tot_procs pp_cluster_info ();
        elem
      end
    else
      begin
        find_max (n + 1) filter (WeightedPnameSet.remove elem set)
      end in
  try find_max 1 filter (WeightedPnameSet.inter set priority_set) (* try with priority elements first *)
  with Not_found -> find_max 1 filter set

(** Handle timeout events *)
module Timeout : sig
  val exe_timeout : int -> ('a -> 'b) -> 'a -> 'b option (* execute the function up to a given timeout given by the parameter *)
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

  let pp_kind f = function
    | TOtime ->
        F.fprintf f "time"
    | TOrecursion n ->
        F.fprintf f "recursion %d" n
    | TOsymops n ->
        F.fprintf f "SymOps %d" n

  let timeout_action _ =
    unset_alarm ();
    raise (Timeout_exe (TOtime))

  let () = begin
    match Config.os_type with
    | Config.Unix | Config.Cygwin ->
        Sys.set_signal Sys.sigvtalrm (Sys.Signal_handle timeout_action);
        Sys.set_signal Sys.sigalrm (Sys.Signal_handle timeout_action)
    | Config.Win32 ->
        SymOp.set_wallclock_timeout_handler timeout_action;
        ignore (Gc.create_alarm SymOp.check_wallclock_alarm) (* use the Gc alarm for periodic timeout checks *)
  end

  let exe_timeout iterations f x =
    try
      set_iterations iterations;
      set_alarm (get_timeout_seconds ());
      SymOp.set_alarm ();
      let res = f x in
      unset_alarm ();
      SymOp.unset_alarm ();
      Some res
    with
    | Timeout_exe kind ->
        unset_alarm ();
        SymOp.unset_alarm ();
        Errdesc.warning_err (State.get_loc ()) "TIMEOUT: %a@." pp_kind kind;
        None
    | exe ->
        unset_alarm ();
        SymOp.unset_alarm ();
        raise exe
end

module Process = Process_fork

(** Main algorithm responsible for driving the analysis of an Exe_env (set of procedures).
    The algorithm computes dependencies between procedures, spawns processes if required,
    propagates results, and handles fixpoints in the call graph. *)
let parallel_execution exe_env num_processes analyze_proc filter_out process_result : unit =
  parallel_mode := num_processes > 1 || !Config.max_num_proc > 0;
  let call_graph = Exe_env.get_cg exe_env in
  let filter_initial (pname, w) =
    let summary = Specs.get_summary_unsafe pname in
    Specs.get_timestamp summary = 0 in
  wpnames_todo := WeightedPnameSet.filter filter_initial (compute_weighed_pnameset call_graph);
  let wpnames_address_of = (* subset of the procedures whose address is taken *)
    let address_taken_of n =
      Procname.Set.mem n (Cfg.get_priority_procnames (Exe_env.get_cfg exe_env n)) in
    WeightedPnameSet.filter (fun (n, _) -> address_taken_of n) !wpnames_todo in
  tot_procs := WeightedPnameSet.cardinal !wpnames_todo;
  num_procs_done := 0;
  let avail_num = ref num_processes (* number of processors available *) in
  let max_timeout = ref 1 in
  let wpname_can_be_analyzed (pname, weight) : bool = (* Return true if [pname] is not up to date and it can be analyzed right now *)
    Specs.is_inactive pname &&
    Procname.Set.for_all
      (proc_is_done call_graph) (Cg.get_nonrecursive_dependents call_graph pname) &&
    Procname.Set.for_all
      (fun child -> Specs.is_inactive child) (Cg.get_defined_children call_graph pname) &&
    (Specs.get_timestamp (Specs.get_summary_unsafe pname) = 0
     || not (proc_is_up_to_date call_graph pname)) in
  let process_one_proc pname (calls: Cg.in_out_calls) =
    DB.current_source :=
      (Specs.get_summary_unsafe pname).Specs.attributes.ProcAttributes.loc.Location.file;
    if !trace then
      begin
        let whole_seconds = false in
        L.err "@[<v 3>   *********** Summary of %a ***********@," Procname.pp pname;
        L.err "%a@]@\n" (Specs.pp_summary pe_text whole_seconds) (Specs.get_summary_unsafe pname)
      end;
    if filter_out call_graph pname
    then
      post_process_procs exe_env [pname]
    else
      begin
        Specs.set_status pname Specs.ACTIVE;
        max_timeout := max (Specs.get_iterations pname) !max_timeout;
        Specs.update_dependency_map pname;
        if !parallel_mode then
          let p_str = Process.spawn_fun (analyze_proc exe_env) in
          Timing_log.event_start (Procname.to_string pname);
          Process.send_to_child p_str (pname, calls);
          decr avail_num
        else
          begin
            Timing_log.event_start (Procname.to_string pname);
            process_result exe_env (pname, calls) (analyze_proc exe_env (pname, calls));
            Timing_log.event_finish (Procname.to_string pname)
          end
      end in
  let wait_for_next_result () =
    try
      match Timeout.exe_timeout (2 * !max_timeout) Process.receive_from_child () with
      | None ->
          let remaining_procedures =
            let procs = list_map Process.get_node_calls (Process.get_remaining_processes ()) in
            list_map (function None -> assert false | Some x -> x) procs in
          L.err "No process is responding: killing %d pending processes@." (list_length remaining_procedures);
          Process.kill_remaining_processes ();
          let do_proc (pname, calls) =
            let prev_summary = Specs.get_summary_unsafe pname in
            let timestamp = max 1 (prev_summary.Specs.timestamp) in
            let stats = { prev_summary.Specs.stats with Specs.stats_timeout = true } in
            let summ =
              { prev_summary with
                Specs.stats = stats;
                Specs.payload = Specs.PrePosts [];
                Specs.timestamp = timestamp;
                Specs.status = Specs.INACTIVE } in
            process_result exe_env (pname, calls) summ;
            Timing_log.event_finish (Procname.to_string pname);
            incr avail_num in
          list_iter do_proc remaining_procedures
      | Some (p_str, summ) ->
          let (pname, weight) = Process.get_last_input p_str in
          (try
             DB.current_source :=
               (Specs.get_summary_unsafe pname).Specs.attributes.ProcAttributes.loc.Location.file;
             process_result exe_env (pname, weight) summ
           with exn -> assert false);
          Timing_log.event_finish (Procname.to_string pname);
          Process.kill_process p_str;
          incr avail_num
    with
    | Sys_blocked_io -> () in
  while not (WeightedPnameSet.is_empty !wpnames_todo) do
    if !avail_num > 0 then
      begin
        if !trace then begin
          let analyzable_pnames = WeightedPnameSet.filter wpname_can_be_analyzed !wpnames_todo in
          L.err "Nodes todo: %a@\n" pp_weightedpnameset !wpnames_todo;
          L.err "Analyzable procs: %a@\n" pp_weightedpnameset analyzable_pnames
        end;
        try
          let pname, calls = filter_max exe_env call_graph wpname_can_be_analyzed !wpnames_todo wpnames_address_of in (** find max analyzable proc *)
          process_one_proc pname calls
        with Not_found -> (* no analyzable procs *)
          if !avail_num < num_processes (* some other process is doing work *)
          then wait_for_next_result ()
          else
            (L.err "Error: can't analyze any procs. Printing current spec table@\n@[<v>%a@]@." (Specs.pp_spec_table pe_text false) ();
             raise (Failure "Stopping"))
      end
    else
      wait_for_next_result ()
  done

(** [parallel_iter_nodes cfg call_graph analyze_proc process_result filter_out]
    executes [analyze_proc] in parallel as much as possible as allowed
    by the call graph, and applies [process_result] to the result as
    soon as it is returned by a child process. If [filter_out] returns
    true, no execution. *)
let parallel_iter_nodes (exe_env: Exe_env.t) (_analyze_proc: Exe_env.t -> Procname.t -> 'a) (_process_result: Exe_env.t -> (Procname.t * Cg.in_out_calls) -> 'a -> unit) (filter_out: Cg.t -> Procname.t -> bool) : unit =
  let analyze_proc exe_env pname = (* wrap _analyze_proc and handle exceptions *)
    try _analyze_proc exe_env pname with
    | exn ->
        Reporting.log_error pname exn;
        let prev_summary = Specs.get_summary_unsafe pname in
        let timestamp = max 1 (prev_summary.Specs.timestamp) in
        let stats = { prev_summary.Specs.stats with Specs.stats_timeout = true } in
        let summ =
          { prev_summary with
            Specs.stats = stats;
            Specs.payload = Specs.PrePosts [];
            Specs.timestamp = timestamp } in
        summ in
  let process_result exe_env (pname, calls) summary = (* wrap _process_result and handle exceptions *)
    try _process_result exe_env (pname, calls) summary with
    | exn ->
        let err_name, _, mloco, _, _, _, _ = Exceptions.recognize_exception exn in
        let err_str = "process_result raised " ^ (Localise.to_string err_name) in
        L.err "Error: %s@." err_str;
        let exn' = Exceptions.Internal_error (Localise.verbatim_desc err_str) in
        Reporting.log_error pname exn';
        post_process_procs exe_env [pname] in
  let num_processes = if !Config.max_num_proc = 0 then !Config.num_cores else !Config.max_num_proc in
  Unix.handle_unix_error (parallel_execution exe_env num_processes (fun exe_env (n, w) -> analyze_proc exe_env n) filter_out) process_result
