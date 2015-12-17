(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Main module for the analysis after the capture phase *)

module L = Logging
module F = Format
open Utils
open Yojson.Basic.Util

(* This module, unused by default, generates random c files with procedure calls *)
module Codegen = struct
  let num_files = 5000
  let num_functions = 1000
  let calls_per_fun = 5
  let fun_nr = ref 0

  let gen () =
    for file_nr = 1 to num_files do
      let fname = Printf.sprintf "file%04d.c" file_nr in
      let fmt = open_out fname in
      for i = 1 to num_functions do
        incr fun_nr;
        let num_calls = if !fun_nr = 1 then 0 else Random.int calls_per_fun in
        Printf.fprintf fmt "void f%04d() {\n" !fun_nr;
        for call_nr = 1 to num_calls do
          let callee_nr = 1 + Random.int (max 1 (num_calls - 1)) in
          Printf.fprintf fmt "f%04d();\n" callee_nr
        done;
        Printf.fprintf fmt "}\n";
      done;
      close_out fmt
    done

  let dont () =
    gen ();
    exit 0
end

(** if true, save file dependency graph to disk *)
let save_file_dependency = ref false

type incremental =
  | ANALYZE_ALL (** analyze all the files in the results dir *)
  | ANALYZE_CHANGED_AND_DEPENDENCIES (** analyze the files that have changed, as well as those dependent on them *)
  | ANALYZE_CHANGED_ONLY (** only analyze the files that have changed *)

let incremental_mode = ref ANALYZE_ALL

(** command line option: if true, simulate the analysis only *)
let simulate = ref false

(** command line option: if true, run the analysis in checker mode *)
let checkers = ref false

(** command line option: name of the makefile to create with clusters and dependencies *)
let makefile_cmdline = ref ""

(** Command line option: only consider procedures whose name begins with the given string. *)
let select_proc = ref None

(** if not empty, only analyze the files in the list provided from the command-line *)
let only_files_cmdline = ref []

(** optional command-line name of the .cluster file *)
let cluster_cmdline = ref None

(** value of -out_file command-line *)
let out_file_cmdline = ref ""

(** value of -err_file command-line *)
let err_file_cmdline = ref ""

(** Files excluded from the analysis *)
let excluded_files : string list ref = ref []

(** Absolute path to the project source, used for relative paths in the exclude list *)
let source_path = ref ""

(** List of obj memory leak buckets to be checked in Objective-C/C++ *)
let ml_buckets_arg = ref "cf"

(** Whether specs can be cleaned up before starting analysis *)
let allow_specs_cleanup = ref false

(** Compute the exclude function from excluded_files and source_path.
    The exclude function builds an exclude list of file path prefixes, and checks if one
    of them is a prefix of the given source file.
    Prefixes are obtained by prepending source_path, if any, to relative paths in excluded_fies *)
let compute_exclude_fun () : DB.source_file -> bool =
  let prepend_source_path s =
    if Filename.is_relative s then Filename.concat !source_path s
    else s in
  let excluded_list = IList.map (fun file_path -> prepend_source_path file_path) !excluded_files in
  let exclude_fun (source_file : DB.source_file) =
    IList.exists (fun excluded_path -> string_is_prefix excluded_path (DB.source_file_to_string source_file)) excluded_list in
  exclude_fun

let version_string () =
  "Infer version " ^ Version.versionString ^ "\nCopyright 2009 - present Facebook. All Rights Reserved.\n"

let print_version () =
  F.fprintf F.std_formatter "%s@." (version_string ());
  exit 0

let print_version_json () =
  F.fprintf F.std_formatter "%s@." Version.versionJson;
  exit 0

let arg_desc =
  let base_arg =
    let exclude s = match read_file s with
      | None ->
          F.fprintf F.std_formatter "ERROR: cannot read the exclude file %s@." s;
          exit 1
      | Some files -> excluded_files := files in
    let source_path s =
      if Filename.is_relative s then
        begin
          F.fprintf F.std_formatter "ERROR: source_path must be an absolute path: %s @." s;
          exit 1
        end;
      source_path := s in
    let desc =
      base_arg_desc @
      [
        "-err_file", Arg.Set_string err_file_cmdline, Some "file", "use file for the err channel";
        "-exclude", Arg.String exclude, Some "file", "exclude from analysis the files and directories specified in file";
        "-incremental_changed_only", Arg.Unit (fun () -> incremental_mode := ANALYZE_CHANGED_ONLY), None, "only analyze files captured since the last analysis";
        "-incremental", Arg.Unit (fun () -> incremental_mode := ANALYZE_CHANGED_AND_DEPENDENCIES), None, "analyze files captured since the last analysis plus any dependencies";
        "-iterations", Arg.Set_int iterations_cmdline, Some "n", "set the max number of operations for each function, expressed as a multiple of symbolic operations (default n=1)";
        "-nonstop", Arg.Set Config.nonstop, None, "activate the nonstop mode: the analysis continues after finding errors. With this option the analysis can become less precise.";
        "-out_file", Arg.Set_string out_file_cmdline, Some "file", "use file for the out channel";
        "-print_builtins", Arg.Unit SymExec.print_builtins, None, "print the builtin functions and exit";
        "-source_path", Arg.String source_path, Some "path", "specify the absolute path to the root of the source files. Used to interpret relative paths when using option -exclude.";
        (* TODO: merge with the -project_root option *)
        "-java", Arg.Unit (fun () -> Config.curr_language := Config.Java), None, "Set language to Java";
        "-version", Arg.Unit print_version, None, "print version information and exit";
        "-version_json", Arg.Unit print_version_json, None, "print version json formatted";
        "-objcm", Arg.Set Config.objc_memory_model_on, None, "Use ObjC memory model";
        "-no_progress_bar", Arg.Unit (fun () -> Config.show_progress_bar := false), None, "Do not show a progress bar";
        "-ml_buckets", Arg.Set_string ml_buckets_arg, Some "ml_buckets",
        "memory leak buckets to be checked, separated by commas. The possible buckets are cf (Core Foundation), arc, narc (No arc), cpp, unknown_origin";
      ] in
    Arg.create_options_desc false "Analysis Options" desc in
  let reserved_arg =
    let desc =
      reserved_arg_desc @
      [
        "-analysis_stops", Arg.Set Config.analysis_stops, None, "issue a warning when the analysis stops";
        "-angelic_execution", Arg.Set Config.angelic_execution, None, "activate angelic execution: the analysis ignores errors caused by unknown procedure calls.";
        "-checkers", Arg.Set checkers, None, " run only the checkers instead of the full analysis";
        "-cluster", Arg.String (fun s -> cluster_cmdline := Some s), Some "fname", "specify a .cluster file to be analyzed";
        "-codequery", Arg.String (fun s -> CodeQuery.query := Some s), Some "query", " execute the code query";
        "-eradicate", Arg.Set Config.eradicate, None, " activate the eradicate checker for java annotations";
        "-file", Arg.String (fun s -> only_files_cmdline := s :: !only_files_cmdline), Some "fname", "specify one file to be analyzed (without path); the option can be repeated";
        "-intraprocedural", Arg.Set Config.intraprocedural, None, "perform an intraprocedural analysis only";
        "-makefile", Arg.Set_string makefile_cmdline, Some "file", "create a makefile to perform the analysis";
        "-max_cluster", Arg.Set_int Config.max_cluster_size, Some "n", "set the max number of procedures in each cluster (default n=2000)";
        "-only_nospecs", Arg.Set Config.only_nospecs, None, " only analyze procedures which were analyzed before but have no specs";
        "-only_skips", Arg.Set Config.only_skips, None, " only analyze procedures dependent on previous skips which now have a .specs file";
        "-seconds_per_iteration", Arg.Set_int seconds_per_iteration, Some "n", "set the number of seconds per iteration (default n=30)";
        "-simulate", Arg.Set simulate, None, " run a simulation of the analysis only";
        "-subtype_multirange", Arg.Set Config.subtype_multirange, None, "use the multirange subtyping domain";
        "-optimistic_cast", Arg.Set Config.optimistic_cast, None, "allow cast of undefined values";
        "-select_proc", Arg.String (fun s -> select_proc := Some s), Some "string", "only consider procedures whose name contains the given string";
        "-symops_per_iteration", Arg.Set_int symops_per_iteration, Some "n", "set the number of symbolic operations per iteration (default n="^(string_of_int !symops_per_iteration)^")";
        "-type_size", Arg.Set Config.type_size, None, "consider the size of types during analysis";
        "-tracing", Arg.Unit (fun () -> Config.report_runtime_exceptions := true), None,
        "Report error traces for runtime exceptions (Only for Java)";
        "-allow_specs_cleanup", Arg.Unit (fun () -> allow_specs_cleanup := true), None,
        "Allow to remove existing specs before running analysis when it's not incremental";
        "-print_buckets", Arg.Unit (fun() -> Config.show_buckets := true; Config.show_ml_buckets := true), None,
        "Add buckets to issue descriptions, useful when developing infer"
      ] in
    Arg.create_options_desc false
      "Reserved Options: Experimental features, use with caution!" desc in
  base_arg @ reserved_arg

let usage =
  (version_string ()) ^
  "\nUsage: InferAnalyze [options]\n" ^
  " Analyze the files captured in the project results directory, which can be specified with the -results_dir option."

let print_usage_exit () =
  Arg.usage arg_desc usage;
  exit(1)

let () = (* parse command-line arguments *)
  let f arg =
    () (* ignore anonymous arguments *) in
  Arg.parse arg_desc f usage;
  if not (Sys.file_exists !Config.results_dir) then
    begin
      L.err "ERROR: results directory %s does not exist@.@." !Config.results_dir;
      print_usage_exit ()
    end

module Simulator = struct (** Simulate the analysis only *)
  let reset_summaries cg =
    IList.iter
      (fun (pname, in_out_calls) -> Specs.reset_summary cg pname None)
      (Cg.get_nodes_and_calls cg)

  (** Perform phase transition from [FOOTPRINT] to [RE_EXECUTION] for
      the procedures enabled after the analysis of [proc_name] *)
  let perform_transition exe_env proc_name =
    let proc_names = Fork.should_perform_transition (Exe_env.get_cg exe_env) proc_name in
    let f proc_name =
      let joined_pres = [] in
      Fork.transition_footprint_re_exe proc_name joined_pres in
    IList.iter f proc_names

  let process_result (exe_env: Exe_env.t) ((proc_name: Procname.t), (calls: Cg.in_out_calls)) (_summ: Specs.summary) : unit =
    L.err "in process_result %a@." Procname.pp proc_name;
    let summ =
      { _summ with
        Specs.stats = { _summ.Specs.stats with Specs.stats_calls = calls }} in
    Specs.add_summary proc_name summ;
    perform_transition exe_env proc_name;
    let procs_done = Fork.procs_become_done (Exe_env.get_cg exe_env) proc_name in
    Fork.post_process_procs exe_env procs_done

  let analyze_proc exe_env tenv proc_name =
    L.err "in analyze_proc %a@." Procname.pp proc_name;
    (* for i = 1 to Random.int 1000000 do () done; *)
    let prev_summary = Specs.get_summary_unsafe "Simulator" proc_name in
    let timestamp = max 1 (prev_summary.Specs.timestamp) in
    { prev_summary with Specs.timestamp = timestamp }

  let filter_out cg proc_name = false
end

let analyze exe_env =
  let init_time = Unix.gettimeofday () in
  L.log_progress_simple ".";
  Specs.clear_spec_tbl ();
  Random.self_init ();
  let line_reader = Printer.LineReader.create () in
  if !checkers then (* run the checkers only *)
    begin
      let call_graph = Exe_env.get_cg exe_env in
      Callbacks.iterate_callbacks Checkers.ST.store_summary call_graph exe_env
    end
  else if !simulate then (* simulate the analysis *)
    begin
      Simulator.reset_summaries (Exe_env.get_cg exe_env);
      Fork.interprocedural_algorithm
        exe_env
        (Simulator.analyze_proc exe_env)
        Simulator.process_result
        Simulator.filter_out
    end
  else (* full analysis *)
    begin
      Interproc.do_analysis exe_env;
      Printer.c_files_write_html line_reader exe_env;
      Interproc.print_stats exe_env;
      let elapsed = Unix.gettimeofday () -. init_time in
      L.out "Interprocedural footprint analysis terminated in %f sec@." elapsed
    end

(** add [x] to list [l] at position [nth] *)
let list_add_nth x l nth =
  let rec add acc todo nth =
    if nth = 0 then IList.rev_append acc (x:: todo)
    else match todo with
      | [] -> raise Not_found
      | y:: todo' -> add (y:: acc) todo' (nth - 1) in
  add [] l nth

(** sort a list weakly w.r.t. a compare function which doest not have to be a total order
    the number returned by [compare x y] indicates 'how strongly' x should come before y *)
let weak_sort compare list =
  let weak_add l x =
    let length = IList.length l in
    let fitness = Array.make (length + 1) 0 in
    IList.iter (fun y -> fitness.(0) <- fitness.(0) + compare x y) l;
    let best_position = ref 0 in
    let best_value = ref (fitness.(0)) in
    let i = ref 0 in
    IList.iter (fun y ->
        incr i;
        let new_value = fitness.(!i - 1) - (compare x y) + (compare y x) in
        fitness.(!i) <- new_value;
        if new_value < !best_value then
          begin
            best_value := new_value;
            best_position := !i
          end)
      l;
    list_add_nth x l !best_position in
  IList.fold_left weak_add [] list

let pp_stringlist fmt slist =
  IList.iter (fun pname -> F.fprintf fmt "%s " pname) slist

let weak_sort_nodes cg =
  let nodes = Cg.get_defined_nodes cg in
  let grand_hash = Procname.Hash.create 1 in
  let get_grandparents x =
    try Procname.Hash.find grand_hash x with
    | Not_found ->
        let res = ref Procname.Set.empty in
        let do_parent p = res := Procname.Set.union (Cg.get_parents cg p) !res in
        Procname.Set.iter do_parent (Cg.get_parents cg x);
        Procname.Hash.replace grand_hash x !res;
        !res in
  let cmp x y =
    let res = ref 0 in
    if Procname.Set.mem y (Cg.get_parents cg x) then res := !res - 2;
    if Procname.Set.mem y (get_grandparents x) then res := !res - 1;
    if Procname.Set.mem x (Cg.get_parents cg y) then res := !res + 2;
    if Procname.Set.mem x (get_grandparents y) then res := !res + 1;
    !res in
  weak_sort cmp nodes

let file_pname_to_cg file_pname =
  let source_file = ClusterMakefile.source_file_from_pname file_pname in
  let source_dir = DB.source_dir_from_source_file source_file in
  let cg_fname = DB.source_dir_get_internal_file source_dir ".cg" in
  Cg.load_from_file cg_fname

let output_json_file_stats num_files num_procs num_lines =
  let file_stats =
    `Assoc [ ("files", `Int num_files);
             ("procedures", `Int num_procs);
             ("lines", `Int num_lines) ] in
  (* write stats file to disk, intentionally overwriting old file if it already exists *)
  let f = open_out (Filename.concat !Config.results_dir Config.proc_stats_filename) in
  Yojson.Basic.pretty_to_channel f file_stats

(** create clusters of minimal size in the dependence order,
    with recursive parts grouped together. *)
let create_minimal_clusters file_cg exe_env to_analyze_map : Cluster.t list =
  if !Cluster.trace_clusters then L.err "[create_minimal_clusters]@.";
  let sorted_files = weak_sort_nodes file_cg in
  let seen = ref Procname.Set.empty in
  let clusters = ref [] in
  let total_files = ref 0 in
  let total_procs = ref 0 in
  let total_LOC = ref 0 in
  let total = Procname.Map.cardinal to_analyze_map in
  let analyzed_map_done = ref 0 in
  let create_cluster_elem  (file_pname, changed_procs) = (* create a Cluster.elem for the file *)
    L.log_progress "Creating clusters..." analyzed_map_done total;
    let source_file = ClusterMakefile.source_file_from_pname file_pname in
    if !Cluster.trace_clusters then
      L.err "      [create_minimal_clusters] %s@." (DB.source_file_to_string source_file);
    DB.current_source := source_file;
    match file_pname_to_cg file_pname with
    | None ->
        Cluster.create_bottomup source_file 0 []
    | Some cg ->
        (* decide whether a proc is active using pname_to_fname, i.e. whether this is the file associated to it *)
        let proc_is_selected pname = match !select_proc with
          | None -> true
          | Some pattern_str -> string_is_prefix pattern_str (Procname.to_unique_id pname) in
        let proc_is_active pname =
          proc_is_selected pname &&
          DB.source_file_equal (Exe_env.get_source exe_env pname) source_file in
        let active_procs = IList.filter proc_is_active (Procname.Set.elements changed_procs) in
        let naprocs = IList.length active_procs in
        total_files := !total_files + 1;
        total_procs := !total_procs + naprocs;
        total_LOC := !total_LOC + (Cg.get_nLOC cg);
        Cluster.create_bottomup source_file naprocs active_procs in
  let choose_next_file list = (* choose next file from the weakly ordered list *)
    let file_has_no_unseen_dependents fname =
      Procname.Set.subset (Cg.get_dependents file_cg fname) !seen in
    match IList.partition file_has_no_unseen_dependents list with
    | (fname :: no_deps), deps -> (* if all the dependents of fname have been seen, bypass the order in the list *)
        if !Cluster.trace_clusters then
          L.err "  [choose_next_file] %s (NO dependents)@." (Procname.to_string fname);
        Some (fname, IList.rev_append no_deps deps)
    | [], _ ->
        begin
          match list with
          | fname :: list' ->
              if !Cluster.trace_clusters then
                L.err "  [choose_next_file] %s (HAS dependents)@." (Procname.to_string fname);
              Some(fname, list')
          | [] -> None
        end in
  let rec build_clusters list = match choose_next_file list with
    | None -> ()
    | Some (fname, list') ->
        if !Cluster.trace_clusters then
          L.err "    [build_clusters] %s@." (Procname.to_string fname);
        if Procname.Set.mem fname !seen then build_clusters list'
        else
          let cluster_set = Procname.Set.add fname (Cg.get_recursive_dependents file_cg fname) in
          let cluster, list'' = IList.partition (fun node -> Procname.Set.mem node cluster_set) list in
          seen := Procname.Set.union !seen cluster_set;
          let to_analyze =
            IList.fold_right
              (fun file_pname l ->
                 try (file_pname, Procname.Map.find file_pname to_analyze_map) :: l
                 with Not_found -> l)
              cluster
              [] in
          if to_analyze <> [] then
            begin
              let cluster = IList.map create_cluster_elem to_analyze in
              clusters := cluster :: !clusters;
            end;
          build_clusters list'' in
  build_clusters sorted_files;
  output_json_file_stats !total_files !total_procs !total_LOC;
  IList.rev !clusters

let proc_list_to_set proc_list =
  IList.fold_left (fun s p -> Procname.Set.add p s) Procname.Set.empty proc_list

(** compute the files to analyze map for incremental mode *)
let compute_to_analyze_map_incremental files_changed_map global_cg exe_env =
  let changed_fold_f files_changed_map f =
    let apply_f _ procs acc =
      Procname.Set.fold (fun proc acc -> Procname.Set.union acc (f proc)) procs acc in
    Procname.Map.fold apply_f files_changed_map Procname.Set.empty in
  (* all callers of changed procedures *)
  let callers_of_changed =
    changed_fold_f files_changed_map (fun p -> Cg.get_ancestors global_cg p) in
  (* all callees of changed procedures *)
  let callees_of_changed =
    changed_fold_f files_changed_map (fun p -> Cg.get_heirs global_cg p) in
  (* add a procedure to the file -> (procedures to analyze) map *)
  let add_proc_to_map proc map =
    let source_opt =
      try Some (Exe_env.get_source exe_env proc)
      with Not_found -> None in
    match source_opt with
    | Some source ->
        let proc_file_pname = ClusterMakefile.source_file_to_pname source in
        let procs_to_analyze =
          try Procname.Map.find proc_file_pname map
          with Not_found -> Procname.Set.empty in
        let procs_to_analyze' = Procname.Set.add proc procs_to_analyze in
        Procname.Map.add proc_file_pname procs_to_analyze' map
    | None -> map in
  let get_specs_filename pname =
    Specs.res_dir_specs_filename pname in
  let is_stale pname =
    let specs_file = get_specs_filename pname in
    match Specs.load_summary specs_file with
    | Some summary -> summary.Specs.status = Specs.STALE
    | None -> false in
  (* add stale callees to the map *)
  let add_stale_callee_to_map callee map =
    if is_stale callee then add_proc_to_map callee map
    else map in
  let files_changed_map' =
    Procname.Set.fold add_stale_callee_to_map callees_of_changed files_changed_map in
  if !incremental_mode = ANALYZE_CHANGED_ONLY then
    (* mark all caller specs stale. this ensures future runs of the analysis that need to use specs
       from callers will re-compute them first. we do this instead of deleting the specs because
       that would force the next analysis run to re-compute them even if it doesn't need them. *)
    let mark_spec_as_stale pname =
      let specs_file = get_specs_filename pname in
      match Specs.load_summary specs_file with
      | Some summary ->
          let summary' = { summary with Specs.status = Specs.STALE } in
          Specs.store_summary pname summary'
      | None -> ()  in
    Procname.Set.iter (fun caller -> mark_spec_as_stale caller) callers_of_changed;
    files_changed_map'
  else (* !incremental_mode = ANALYZE_CHANGED_AND_DEPENDENTS *)
    (* add callers of changed procedures to the map of stuff to analyze *)
    Procname.Set.fold add_proc_to_map callers_of_changed files_changed_map'

(** compute the clusters *)
let compute_clusters exe_env files_changed : Cluster.t list =
  if !Cluster.trace_clusters then
    L.err "[compute_clusters] %d changed files@." (Procname.Map.cardinal files_changed);
  let file_cg = Cg.create () in
  let global_cg = Exe_env.get_cg exe_env in
  let nodes, edges = Cg.get_nodes_and_edges global_cg in
  let defined_procs = Cg.get_defined_nodes global_cg in
  let total_nodes = IList.length nodes in
  let computed_nodes = ref 0 in
  let do_node (n, defined, restricted) =
    L.log_progress "Computing dependencies..." computed_nodes total_nodes;
    if defined then
      Cg.add_defined_node file_cg
        (ClusterMakefile.source_file_to_pname (Exe_env.get_source exe_env n)) in
  let do_edge (n1, n2) =
    if Cg.node_defined global_cg n1 && Cg.node_defined global_cg n2 then
      begin
        let src1 = Exe_env.get_source exe_env n1 in
        let src2 = Exe_env.get_source exe_env n2 in
        if not (DB.source_file_equal src1 src2) then begin
          if !Cluster.trace_clusters then
            L.err "file_cg %s -> %s [%a]@."
              (DB.source_file_to_string src1)
              (DB.source_file_to_string src2)
              Procname.pp n2;
          Cg.add_edge file_cg
            (ClusterMakefile.source_file_to_pname src1)
            (ClusterMakefile.source_file_to_pname src2)
        end
      end in
  IList.iter do_node nodes;
  if IList.length nodes > 0 then L.log_progress_simple "\n";
  if not !Config.intraprocedural then IList.iter do_edge edges;
  if !save_file_dependency then
    Cg.save_call_graph_dotty (Some (DB.filename_from_string "file_dependency.dot")) Specs.get_specs file_cg;
  let files = Cg.get_defined_nodes file_cg in
  let num_files = IList.length files in
  L.err "@.Found %d defined procedures in %d files.@." (IList.length defined_procs) num_files;
  let to_analyze_map =
    if !incremental_mode = ANALYZE_ALL then
      (* get all procedures defined in a file *)
      let get_defined_procs file_pname = match file_pname_to_cg file_pname with
        | None -> Procname.Set.empty
        | Some cg -> proc_list_to_set (Cg.get_defined_nodes cg) in
      IList.fold_left
        (fun m file_pname -> Procname.Map.add file_pname (get_defined_procs file_pname) m)
        Procname.Map.empty
        files
    else compute_to_analyze_map_incremental files_changed global_cg exe_env in
  L.err "Analyzing %d files.@.@." (Procname.Map.cardinal to_analyze_map);
  let clusters = create_minimal_clusters file_cg exe_env to_analyze_map in
  L.err "Minimal clusters:@.";
  Cluster.print_clusters_stats clusters;
  if !makefile_cmdline <> "" then
    begin
      let max_cluster_size = 50 in
      let desired_cluster_size = 1 in
      let clusters' =
        Cluster.combine_split_clusters clusters max_cluster_size desired_cluster_size in
      L.err "@.Combined clusters with max size %d@." max_cluster_size;
      Cluster.print_clusters_stats clusters';
      let number_of_clusters = IList.length clusters' in
      let plural_of_cluster = if number_of_clusters != 1 then "s" else "" in
      L.log_progress_simple
        (Printf.sprintf "Analyzing %d cluster%s" number_of_clusters plural_of_cluster);
      ClusterMakefile.create_cluster_makefile_and_exit clusters' file_cg !makefile_cmdline false
    end;
  let clusters' =
    Cluster.combine_split_clusters
      clusters !Config.max_cluster_size !Config.max_cluster_size in
  L.err "@.Combined clusters with max size %d@." !Config.max_cluster_size;
  Cluster.print_clusters_stats clusters';
  let number_of_clusters = IList.length clusters' in
  L.log_progress_simple ("\nAnalyzing "^(string_of_int number_of_clusters)^" clusters");
  clusters'

(** compute the set of procedures in [cg] changed since the last analysis *)
let cg_get_changed_procs exe_env source_dir cg =
  let cfg_fname = DB.source_dir_get_internal_file source_dir ".cfg" in
  let cfg_opt = Cfg.load_cfg_from_file cfg_fname in
  let pdesc_changed pname =
    match cfg_opt with
    | Some cfg -> Cfg.pdesc_is_changed cfg pname
    | None -> true in
  let spec_exists pname =
    let spec_fname = Specs.res_dir_specs_filename pname in
    Sys.file_exists (DB.filename_to_string spec_fname) in
  let cfg_modified_after_specs pname =
    let spec_fname = Specs.res_dir_specs_filename pname in
    DB.file_modified_time cfg_fname > DB.file_modified_time spec_fname in
  let is_changed pname =
    not (spec_exists pname) || (cfg_modified_after_specs pname && pdesc_changed pname) in
  let defined_nodes = Cg.get_defined_nodes cg in
  if !Config.incremental_procs then IList.filter is_changed defined_nodes
  else if IList.exists is_changed defined_nodes then defined_nodes
  else []

(** Load a .c or .cpp file into an execution environment *)
let load_cg_file (_exe_env: Exe_env.initial) (source_dir : DB.source_dir) exclude_fun =
  match Exe_env.add_cg_exclude_fun _exe_env source_dir exclude_fun with
  | None -> None
  | Some cg ->
      L.err "loaded %s@." (DB.source_dir_to_string source_dir);
      Some cg

(** Return a map of (changed file procname) -> (procs in that file that have changed) *)
let compute_files_changed_map _exe_env (source_dirs : DB.source_dir list) exclude_fun =
  let sorted_dirs = IList.sort DB.source_dir_compare source_dirs in
  let cg_list =
    IList.fold_left
      (fun cg_list source_dir ->
         match load_cg_file _exe_env source_dir exclude_fun with
         | None -> cg_list
         | Some cg -> (source_dir, cg) :: cg_list)
      []
      sorted_dirs in
  let exe_env_get_files_changed files_changed_map exe_env =
    let cg_get_files_changed files_changed_map (source_dir, cg) =
      let changed_procs =
        if !incremental_mode = ANALYZE_ALL then Cg.get_defined_nodes cg
        else cg_get_changed_procs exe_env source_dir cg in
      if changed_procs <> [] then
        let file_pname = ClusterMakefile.source_file_to_pname (Cg.get_source cg) in
        Procname.Map.add file_pname (proc_list_to_set changed_procs) files_changed_map
      else files_changed_map in
    IList.fold_left cg_get_files_changed files_changed_map cg_list in
  let exe_env = Exe_env.freeze _exe_env in
  let files_changed =
    if !incremental_mode = ANALYZE_ALL then Procname.Map.empty
    else exe_env_get_files_changed Procname.Map.empty exe_env in
  files_changed, exe_env

(** Create an exe_env from a cluster. *)
let exe_env_from_cluster cluster =
  let _exe_env =
    let active_procs_opt = Cluster.get_active_procs cluster in
    Exe_env.create active_procs_opt in
  let source_dirs =
    let fold_cluster_elem source_dirs ce =
      let source_dir =
        match Cluster.get_ondemand_info ce with
        | Some source_dir ->
            source_dir
        | None ->
            DB.source_dir_from_source_file ce.Cluster.ce_file in
      source_dir :: source_dirs in
    IList.fold_left fold_cluster_elem [] cluster in
  let sorted_dirs = IList.sort DB.source_dir_compare source_dirs in
  IList.iter (fun src_dir -> ignore (Exe_env.add_cg _exe_env src_dir)) sorted_dirs;
  let exe_env = Exe_env.freeze _exe_env in
  exe_env

(** Analyze a cluster of files *)
let analyze_cluster cluster_num tot_clusters (cluster : Cluster.t) =
  incr cluster_num;
  let exe_env = exe_env_from_cluster cluster in
  let num_files = IList.length cluster in
  let defined_procs = Cg.get_defined_nodes (Exe_env.get_cg exe_env) in
  let num_procs = IList.length defined_procs in
  L.err "@.Processing cluster #%d/%d with %d files and %d procedures@." !cluster_num tot_clusters num_files num_procs;
  Fork.this_cluster_files := num_files;
  analyze exe_env;
  Fork.tot_files_done := num_files + !Fork.tot_files_done

let process_cluster_cmdline_exit () =
  match !cluster_cmdline with
  | None -> ()
  | Some fname ->
      (match Cluster.load_from_file (DB.filename_from_string fname) with
       | None ->
           L.err "Cannot find cluster file %s@." fname;
           exit 0
       | Some (nr, tot_nr, cluster) ->
           Fork.tot_files_done := (nr - 1) * IList.length cluster;
           Fork.tot_files := tot_nr * IList.length cluster;
           analyze_cluster (ref (nr -1)) tot_nr cluster;
           exit 0)

let open_output_file f fname =
  try
    let cout = open_out fname in
    let fmt = Format.formatter_of_out_channel cout in
    f fmt;
    Some (fmt, cout)
  with Sys_error _ ->
    Format.fprintf Format.std_formatter "Error: cannot open output file %s@." fname;
    exit(-1)

let close_output_file = function
  | None -> ()
  | Some (fmt, cout) -> close_out cout

let setup_logging () =
  if !Config.developer_mode then
    let log_dir_name = "log" in
    let analyzer_out_name = "analyzer_out" in
    let analyzer_err_name = "analyzer_err" in
    let log_dir = DB.filename_to_string (DB.Results_dir.path_to_filename DB.Results_dir.Abs_root [log_dir_name]) in
    DB.create_dir log_dir;
    let analyzer_out_file =
      if !out_file_cmdline = "" then Filename.concat log_dir analyzer_out_name
      else !out_file_cmdline in
    let analyzer_err_file =
      if !err_file_cmdline = "" then Filename.concat log_dir analyzer_err_name
      else !err_file_cmdline in
    let analyzer_out_of = open_output_file Logging.set_out_formatter analyzer_out_file in
    let analyzer_err_of = open_output_file Logging.set_err_formatter analyzer_err_file in
    analyzer_out_of, analyzer_err_of
  else None, None

let teardown_logging analyzer_out_of analyzer_err_of =
  if !Config.developer_mode then
    begin
      L.flush_streams ();
      close_output_file analyzer_out_of;
      close_output_file analyzer_err_of;
    end

(** Compute clusters when on-demand is active.
    Each cluster will contain only the name of the directory for a file. *)
let compute_ondemand_clusters source_dirs =
  let mk_cluster source_dir =
    Cluster.create_ondemand source_dir in
  let clusters =
    let do_source_dir acc source_dir = mk_cluster source_dir @ acc in
    IList.fold_left do_source_dir [] source_dirs in
  Cluster.print_clusters_stats clusters;
  let num_files = IList.length clusters in
  let num_procs = 0 (* can't compute it at this stage *) in
  let num_lines = 0 in
  output_json_file_stats num_files num_procs num_lines;
  if !makefile_cmdline <> "" then
    begin
      let file_cg = Cg.create () in
      ClusterMakefile.create_cluster_makefile_and_exit clusters file_cg !makefile_cmdline false
    end;
  clusters

let () =
  let () =
    match !cluster_cmdline with
    | None ->
        L.stdout "Starting analysis (Infer version %s)@." Version.versionString;
    | Some clname -> L.stdout "Cluster %s@." clname in
  RegisterCheckers.register ();
  Facebook.register_checkers ();

  if !allow_specs_cleanup = true && !incremental_mode = ANALYZE_ALL && !cluster_cmdline = None then
    DB.Results_dir.clean_specs_dir ();

  let analyzer_out_of, analyzer_err_of = setup_logging () in
  if (!Config.curr_language = Config.C_CPP) then Mleak_buckets.init_buckets !ml_buckets_arg;

  process_cluster_cmdline_exit ();

  let source_dirs =
    if !only_files_cmdline = [] then DB.find_source_dirs ()
    else
      let filter source_dir =
        let source_dir_base = Filename.basename (DB.source_dir_to_string source_dir) in
        IList.exists (fun s -> Utils.string_is_prefix s source_dir_base) !only_files_cmdline in
      IList.filter filter (DB.find_source_dirs ()) in
  L.err "Found %d source files in %s@." (IList.length source_dirs) !Config.results_dir;

  let clusters =
    if !Config.ondemand_enabled
    then
      compute_ondemand_clusters source_dirs
    else
      begin
        let _exe_env = Exe_env.create None in
        let files_changed_map, exe_env =
          compute_files_changed_map _exe_env source_dirs (compute_exclude_fun ()) in
        L.err "Procedures defined in more than one file: %a@."
          Procname.pp_set (Exe_env.get_procs_defined_in_several_files exe_env);
        compute_clusters exe_env files_changed_map
      end in


  let tot_clusters = IList.length clusters in
  Fork.tot_files := IList.fold_left (fun n cluster -> n + IList.length cluster) 0 clusters;
  IList.iter (analyze_cluster (ref 0) tot_clusters) clusters;
  teardown_logging analyzer_out_of analyzer_err_of;
  if !cluster_cmdline = None then L.stdout "Analysis finished in %as@." pp_elapsed_time ()
