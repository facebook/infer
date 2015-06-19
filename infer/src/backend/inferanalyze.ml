(*
* Copyright (c) 2009 -2013 Monoidics ltd.
* Copyright (c) 2013 - Facebook. All rights reserved.
*)

(** Main module for the analysis after the capture phase *)

module L = Logging
module F = Format
open Utils

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

(** if true, print tracing information for functions that manipulate clusters *)
let trace_clusters = ref false

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

(** List of obj memory leak buckets to be checked in objc *)
let objc_ml_buckets_arg = ref "cf"

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
  let excluded_list = list_map (fun file_path -> prepend_source_path file_path) !excluded_files in
  let exclude_fun (source_file : DB.source_file) =
    list_exists (fun excluded_path -> string_is_prefix excluded_path (DB.source_file_to_string source_file)) excluded_list in
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
      "-incremental_ignore_dependencies", Arg.Unit (fun () -> incremental_mode := ANALYZE_CHANGED_ONLY), None, "only analyze files captured since the last analysis";
      "-incremental", Arg.Unit (fun () -> incremental_mode := ANALYZE_CHANGED_AND_DEPENDENCIES), None, "analyze files captured since the last analysis plus any dependencies";
      "-iterations", Arg.Set_int iterations_cmdline, Some "n", "set the max number of operations for each function, expressed as a multiple of symbolic operations (default n=1)";
      "-nonstop", Arg.Set Config.nonstop, None, "activate the nonstop mode: the analysis continues after finding errors. With this option the analysis can become less precise.";
      "-out_file", Arg.Set_string out_file_cmdline, Some "file", "use file for the out channel";
      "-print_builtins", Arg.Unit SymExec.print_builtins, None, "print the builtin functions and exit";
      "-source_path", Arg.String source_path, Some "path", "specify the absolute path to the root of the source files. Used to interpret relative paths when using option -exclude.";
      (* TODO: merge with the -project_root option *)
      "-java", Arg.Unit (fun () -> Sil.curr_language := Sil.Java), None, "Set language to Java";
      "-version", Arg.Unit print_version, None, "print version information and exit";
      "-version_json", Arg.Unit print_version_json, None, "print version json formatted";
      "-objcm", Arg.Set Config.objc_memory_model_on, None, "Use ObjC memory model";
      "-objc_ml_buckets", Arg.Set_string objc_ml_buckets_arg, Some "objc_ml_buckets",
      "memory leak buckets to be checked, separated by commas. The possible buckets are cf (Core Foundation), arc, narc (No arc)";
      ] in
    Arg2.create_options_desc false "Analysis Options" desc in
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
      "Allow to remove existing specs before running analysis when it's not incremental"
      ] in
    Arg2.create_options_desc false "Reserved Options: Experimental features, use with caution!" desc in
  base_arg @ reserved_arg

let usage =
  (version_string ()) ^
  "\nUsage: InferAnalyze [options]\n" ^
  " Analyze the files captured in the project results directory, which can be specified with the -results_dir option."

let print_usage_exit () =
  Arg2.usage arg_desc usage;
  exit(1)

let () = (* parse command-line arguments *)
  let f arg =
    () (* ignore anonymous arguments *) in
  Arg2.parse arg_desc f usage;
  if not (Sys.file_exists !Config.results_dir) then
    begin
      L.err "ERROR: results directory %s does not exist@.@." !Config.results_dir;
      print_usage_exit ()
    end

module Simulator = struct (** Simulate the analysis only *)
  let reset_summaries cg =
    list_iter
      (fun (pname, in_out_calls) -> Specs.reset_summary cg pname Sil.loc_none)
      (Cg.get_nodes_and_calls cg)

  (** Perform phase transition from [FOOTPRINT] to [RE_EXECUTION] for
  the procedures enabled after the analysis of [proc_name] *)
  let perform_transition exe_env proc_name =
    let proc_names = Fork.should_perform_transition (Exe_env.get_cg exe_env) proc_name in
    let f proc_name =
      let joined_pres = [] in
      Fork.transition_footprint_re_exe proc_name joined_pres in
    list_iter f proc_names

  let process_result (exe_env: Exe_env.t) ((proc_name: Procname.t), (calls: Cg.in_out_calls)) (_summ: Specs.summary) : unit =
    L.err "in process_result %a@." Procname.pp proc_name;
    let summ = { _summ with Specs.status = Specs.INACTIVE; Specs.stats = { _summ.Specs.stats with Specs.stats_calls = calls }} in
    Specs.add_summary proc_name summ;
    perform_transition exe_env proc_name;
    let procs_done = Fork.procs_become_done (Exe_env.get_cg exe_env) proc_name in
    Fork.post_process_procs exe_env procs_done

  let analyze_proc exe_env tenv proc_name =
    L.err "in analyze_proc %a@." Procname.pp proc_name;
    (* for i = 1 to Random.int 1000000 do () done; *)
    let prev_summary = Specs.get_summary_unsafe proc_name in
    let timestamp = max 1 (prev_summary.Specs.timestamp) in
    { prev_summary with Specs.timestamp = timestamp }

  let filter_out cg proc_name = false
end

let analyze exe_env =
  let init_time = Unix.gettimeofday () in
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
    Fork.parallel_iter_nodes exe_env (Simulator.analyze_proc exe_env) Simulator.process_result Simulator.filter_out
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
    if nth = 0 then list_rev_append acc (x:: todo)
    else match todo with
      | [] -> raise Not_found
      | y:: todo' -> add (y:: acc) todo' (nth - 1) in
  add [] l nth

(** sort a list weakly w.r.t. a compare function which doest not have to be a total order
the number returned by [compare x y] indicates 'how strongly' x should come before y *)
let weak_sort compare list =
  let weak_add l x =
    let length = list_length l in
    let fitness = Array.make (length + 1) 0 in
    list_iter (fun y -> fitness.(0) <- fitness.(0) + compare x y) l;
    let best_position = ref 0 in
    let best_value = ref (fitness.(0)) in
    let i = ref 0 in
    list_iter (fun y ->
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
  list_fold_left weak_add [] list

let pp_stringlist fmt slist =
  list_iter (fun pname -> F.fprintf fmt "%s " pname) slist

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

(** cluster element: the file name, the number of procedures defined in it, and the list of active procedures
A procedure is active if it is defined only in this file, or if it is defined in several files and this
is the representative file for it (see Exe_env.add_cg) *)
type cluster_elem =
  { ce_file : DB.source_file;
    ce_naprocs : int; (** number of active procedures defined in the file *)
    ce_active_procs : Procname.t list; (** list of active procedures *)
    ce_source_map : DB.source_file Procname.Map.t; (** map from undefined procedures to the file where they are defined *) }

(** cluster of files *)
type cluster =
  cluster_elem list

(** .cluster file: (n,m,cl) indicates cl is cluster n out of m *)
type cluster_serial = int * int * cluster

(** Serializer for clusters *)
let cluster_serializer : cluster_serial Serialization.serializer = Serialization.create_serializer Serialization.cluster_key

(** Load a cluster from a file *)
let load_cluster_from_file (filename : DB.filename) : cluster_serial option =
  Serialization.from_file cluster_serializer filename

(** Save a cluster into a file *)
let store_cluster_to_file (filename : DB.filename) (cluster_serial: cluster_serial) =
  Serialization.to_file cluster_serializer filename cluster_serial

(* this relies on the assumption that a source_file can be converted to a string, then pname, then back *)
let source_file_from_pname pname =
  DB.source_file_from_string (Procname.to_string pname)

let source_file_to_pname fname =
  Procname.from_string (DB.source_file_to_string fname)

(** create clusters of minimal size in the dependence order, with recursive parts grouped together *)
let create_minimal_clusters file_cg exe_env (only_analyze : Procname.Set.t option) : cluster list =
  if !trace_clusters then L.err "[create_minimal_clusters]@.";
  let sorted_files = weak_sort_nodes file_cg in
  let seen = ref Procname.Set.empty in
  let clusters = ref [] in
  let create_cluster_elem pname = (* create a cluster_elem for the file *)
    let source_file = source_file_from_pname pname in
    if !trace_clusters then L.err "      [create_cluster_elem] %s@." (DB.source_file_to_string source_file);
    DB.current_source := source_file;
    let source_dir = DB.source_dir_from_source_file source_file in
    let cg_fname = DB.source_dir_get_internal_file source_dir ".cg" in
    match Cg.load_from_file cg_fname with
    | None -> { ce_file = source_file; ce_naprocs = 0; ce_active_procs = []; ce_source_map = Procname.Map.empty }
    | Some cg ->
    (* decide whether a proc is active using pname_to_fname, i.e. whether this is the file associated to it *)
        let proc_is_selected pname = match !select_proc with
          | None -> true
          | Some pattern_str -> string_is_prefix pattern_str (Procname.to_unique_id pname) in
        let proc_is_active pname =
          proc_is_selected pname &&
          DB.source_file_equal (Exe_env.get_source exe_env pname) source_file in
        let defined_procs = Cg.get_defined_nodes cg in
        let active_procs = list_filter proc_is_active defined_procs in
        let naprocs = list_length active_procs in
        let source_map =
          let all_procs, _ = Cg.get_nodes_and_edges cg in
          let mapr = ref Procname.Map.empty in
          let do_proc (pn, isdefined) =
            if not isdefined then
              try
                let source = Exe_env.get_source exe_env pn in
                mapr := Procname.Map.add pn source !mapr;
              with Not_found -> () in
          list_iter do_proc all_procs;
          !mapr in
        { ce_file = source_file; ce_naprocs = naprocs; ce_active_procs = active_procs; ce_source_map = source_map } in
  let choose_next_file list = (* choose next file from the weakly ordered list *)
    let file_has_no_unseen_dependents fname =
      Procname.Set.subset (Cg.get_dependents file_cg fname) !seen in
    match list_partition file_has_no_unseen_dependents list with
    | (fname :: no_deps), deps -> (* if all the dependents of fname have been seen, bypass the order in the list *)
        if !trace_clusters then L.err "  [choose_next_file] %s (NO dependents)@." (Procname.to_string fname);
        Some (fname, list_rev_append no_deps deps)
    | [], _ ->
        begin
          match list with
          | fname :: list' ->
              if !trace_clusters then L.err "  [choose_next_file] %s (HAS dependents)@." (Procname.to_string fname);
              Some(fname, list')
          | [] -> None
        end in
  let rec build_clusters list = match choose_next_file list with
    | None -> ()
    | Some (fname, list') ->
        if !trace_clusters then L.err "    [build_clusters] %s@." (Procname.to_string fname);
        if Procname.Set.mem fname !seen then build_clusters list'
        else
          let cluster_set = Procname.Set.add fname (Cg.get_recursive_dependents file_cg fname) in
          let cluster, list'' = list_partition (fun node -> Procname.Set.mem node cluster_set) list in
          seen := Procname.Set.union !seen cluster_set;
          let files_to_analyze = list_filter (fun node ->
                    match only_analyze with
                    | None -> true
                    | Some files_to_analyze -> Procname.Set.mem node files_to_analyze) cluster in
          if files_to_analyze <> [] then
            begin
              let cluster = list_map create_cluster_elem files_to_analyze in
              clusters := cluster :: !clusters;
            end;
          build_clusters list'' in
  build_clusters sorted_files;
  list_rev !clusters

let cluster_nfiles cluster = list_length cluster

let cluster_naprocs cluster = list_fold_left (fun n ce -> ce.ce_naprocs + n) 0 cluster

let clusters_nfiles clusters = list_fold_left (fun n cluster -> cluster_nfiles cluster + n) 0 clusters

let clusters_naprocs clusters = list_fold_left (fun n cluster -> cluster_naprocs cluster + n) 0 clusters

let print_clusters_stats clusters =
  let i = ref 0 in
  list_iter (fun cluster -> incr i; L.err "cluster #%d files: %d active procedures: %d@." !i (cluster_nfiles cluster) (cluster_naprocs cluster)) clusters

let cluster_split_prefix (cluster : cluster) size =
  let rec split (cluster_seen : cluster) (cluster_todo : cluster) n =
    if n <= 0 then (list_rev cluster_seen, cluster_todo)
    else match cluster_todo with
      | [] -> raise Not_found
      | ce :: todo' -> split (ce :: cluster_seen) todo' (n - ce.ce_naprocs) in
  split [] cluster size

let combine_split_clusters (clusters : cluster list) max_size desired_size =
  if !trace_clusters then L.err "[combine_split_clusters]@.";
  let old_clusters = ref clusters in
  let old_size = clusters_naprocs !old_clusters in
  let new_clusters = ref [] in
  let current = ref [] in
  let current_size = ref 0 in
  while !old_clusters != [] do
    if old_size != clusters_naprocs !old_clusters + clusters_naprocs !new_clusters + !current_size then begin
      L.err "mismatch in invariant for cluster size@.";
      assert (cluster_naprocs !current = !current_size);
      L.err "old size: %d@." old_size;
      L.err "old clusters size: %d@." (clusters_naprocs !old_clusters);
      L.err "new clusters size: %d@." (clusters_naprocs !new_clusters);
      L.err "current size: %d@." !current_size;
      assert false
    end;
    let next_cluster = list_hd !old_clusters in
    let next_size = cluster_naprocs next_cluster in
    let new_size = !current_size + next_size in
    if (new_size > max_size || new_size > desired_size) && !current_size > 0 then
      begin
        new_clusters := !new_clusters @ [!current];
        current := [];
        current_size := 0
      end
    else if new_size > max_size then
      begin
        let next_cluster', next_cluster'' = cluster_split_prefix next_cluster max_size in
        current := [];
        current_size := 0;
        new_clusters := !new_clusters @ [next_cluster'];
        old_clusters := next_cluster'' :: (list_tl !old_clusters)
      end
    else
      begin
        current := !current @ next_cluster;
        current_size := !current_size + next_size;
        old_clusters := list_tl !old_clusters
      end
  done;
  if !current_size > 0 then new_clusters := !new_clusters @ [!current];
  !new_clusters

(** return the set of active procedures in a cluster *)
let cluster_to_active_procs cluster =
  let procset = ref Procname.Set.empty in
  let do_cluster_elem cluster_elem =
    let add proc = if not (Procname.Set.mem proc !procset) then procset := Procname.Set.add proc !procset in
    list_iter add cluster_elem.ce_active_procs in
  list_iter do_cluster_elem cluster;
  !procset

(** Module to create a makefile with dependencies between clusters *)
module ClusterMakefile = struct
  let cl_name n = "cl" ^ string_of_int n
  let cl_file n = "x" ^ (cl_name n) ^ ".cluster"

  let pp_cl fmt n = Format.fprintf fmt "%s" (cl_name n)

  let pp_prolog fmt num_clusters =
    F.fprintf fmt "INFERANALYZE= %s $(INFER_OPTIONS) -results_dir '%s'\n@."
      Sys.executable_name
      (Escape.escape_map (fun c -> if c = '#' then Some "\\#" else None) !Config.results_dir);
    F.fprintf fmt "OBJECTS=";
    for i = 1 to num_clusters do F.fprintf fmt "%a " pp_cl i done;
    F.fprintf fmt "@.@.default: test@.@.all: test@.@.";
    F.fprintf fmt "test: $(OBJECTS)@.\techo \"Analysis done\"@.@."

  let pp_epilog fmt () =
    F.fprintf fmt "@.clean:@.\trm -f $(OBJECTS)@."

  let pp_cluster_dependency nr tot_nr cluster print_files fmt dependents =
    let fname = cl_file nr in
    store_cluster_to_file (DB.filename_from_string fname) (nr, tot_nr, cluster);
    let pp_active_procs fmt cluster =
      let procnames = Procname.Set.elements (cluster_to_active_procs cluster) in
      let pp_pname fmt pname = Format.fprintf fmt "%s" (Procname.to_string pname) in
      F.fprintf fmt "procedures: %a" (pp_seq pp_pname) procnames in
    let pp_file fmt ce = F.fprintf fmt "%s" (DB.source_file_to_string ce.ce_file) in
    let pp_files fmt cluster = F.fprintf fmt "files: %a" (pp_seq pp_file) cluster in
    F.fprintf fmt "%a : %a@\n" pp_cl nr (pp_seq pp_cl) dependents;
    F.fprintf fmt "\t$(INFERANALYZE) -cluster %s >%a@\n" fname pp_cl nr;
    if print_files then F.fprintf fmt "# %a %a" pp_files cluster pp_active_procs cluster;
    F.fprintf fmt "@\n"

  let create_cluster_makefile_and_exit (clusters: cluster list) (file_cg: Cg.t) (fname: string) (print_files: bool) =
    let outc = open_out fname in
    let fmt = Format.formatter_of_out_channel outc in
    let file_to_cluster = ref DB.SourceFileMap.empty in
    let cluster_nr = ref 0 in
    let tot_clusters_nr = list_length clusters in
    let do_cluster cluster =
      incr cluster_nr;
      let dependent_clusters = ref IntSet.empty in
      let add_dependent file_as_pname =
        let source_file = source_file_from_pname file_as_pname in
        try
          let num = DB.SourceFileMap.find source_file !file_to_cluster in
          if num < !cluster_nr then
            dependent_clusters := IntSet.add num !dependent_clusters
        with Not_found -> F.fprintf fmt "#[%a] missing dependency to %s@." pp_cl !cluster_nr (DB.source_file_to_string source_file) in
      let do_file ce =
        let source_file = ce.ce_file in
        let children = Cg.get_defined_children file_cg (source_file_to_pname source_file) in
        Procname.Set.iter add_dependent children;
        () (* L.err "file %s has %d children@." file (StringSet.cardinal children) *) in
      list_iter (fun ce -> file_to_cluster := DB.SourceFileMap.add ce.ce_file !cluster_nr !file_to_cluster) cluster;
      list_iter do_file cluster;
      pp_cluster_dependency !cluster_nr tot_clusters_nr cluster print_files fmt (IntSet.elements !dependent_clusters);
    (* L.err "cluster %d has %d dependencies@." !cluster_nr (IntSet.cardinal !dependent_clusters) *) in
    pp_prolog fmt tot_clusters_nr;
    list_iter do_cluster clusters;
    pp_epilog fmt ();
    exit 0
end

(** compute the clusters *)
let compute_clusters exe_env (files_changed : Procname.Set.t) : cluster list =
  if !trace_clusters then L.err "[compute_clusters] %d changed files@." (Procname.Set.cardinal files_changed);
  let file_cg = Cg.create () in
  let global_cg = Exe_env.get_cg exe_env in
  let nodes, edges = Cg.get_nodes_and_edges global_cg in
  let defined_procs = Cg.get_defined_nodes global_cg in
  let do_node (n, defined) =
    if defined then Cg.add_node file_cg (source_file_to_pname (Exe_env.get_source exe_env n)) in
  let do_edge (n1, n2) =
    if Cg.node_defined global_cg n1 && Cg.node_defined global_cg n2 then
      begin
        let src1 = Exe_env.get_source exe_env n1 in
        let src2 = Exe_env.get_source exe_env n2 in
        if not (DB.source_file_equal src1 src2) then begin
          if !trace_clusters then L.err "file_cg %s -> %s [%a]@." (DB.source_file_to_string src1) (DB.source_file_to_string src2) Procname.pp n2;
          Cg.add_edge file_cg (source_file_to_pname src1) (source_file_to_pname src2)
        end
      end in
  list_iter do_node nodes;
  if !Config.intraprocedural = false then list_iter do_edge edges;
  if !save_file_dependency then
    Cg.save_call_graph_dotty (Some (DB.filename_from_string "file_dependency.dot")) Specs.get_specs file_cg;
  let files = Cg.get_defined_nodes file_cg in
  let num_files = list_length files in
  L.err "@.Found %d defined procedures in %d files.@." (list_length defined_procs) num_files;
  let files_changed_and_dependents = ref files_changed in
  if !incremental_mode != ANALYZE_ALL then
    begin
      Procname.Set.iter (fun c_file ->
              let ancestors =
                try Cg.get_ancestors file_cg c_file with
                | Not_found ->
                    L.err "Warning: modified file %s is ignored, all its functions might be already defined in another file@." (Procname.to_string c_file);
                    Procname.Set.empty in
              files_changed_and_dependents := Procname.Set.union ancestors !files_changed_and_dependents) files_changed;
      L.err "Number of files changed since the last analysis: %d.@." (Procname.Set.cardinal files_changed)
    end
  else L.err ".@.";
  let only_analyze = match !incremental_mode with
    | ANALYZE_ALL -> None
    | ANALYZE_CHANGED_AND_DEPENDENCIES -> Some !files_changed_and_dependents
    | ANALYZE_CHANGED_ONLY -> Some files_changed in
  let num_files_to_analyze = match only_analyze with
    | None -> num_files
    | Some set -> Procname.Set.cardinal set in
  L.err "Analyzing %d files.@.@." num_files_to_analyze;
  let clusters = create_minimal_clusters file_cg exe_env only_analyze in
  L.err "Minimal clusters:@.";
  print_clusters_stats clusters;
  if !makefile_cmdline <> "" then
    begin
      let max_cluster_size = 50 in
      let desired_cluster_size = 1 in
      let clusters' = combine_split_clusters clusters max_cluster_size desired_cluster_size in
      L.err "@.Combined clusters with max size %d@." max_cluster_size;
      print_clusters_stats clusters';
      ClusterMakefile.create_cluster_makefile_and_exit clusters' file_cg !makefile_cmdline false
    end;
  let clusters' = combine_split_clusters clusters !Config.max_cluster_size !Config.max_cluster_size in
  L.err "@.Combined clusters with max size %d@." !Config.max_cluster_size;
  print_clusters_stats clusters';
  clusters'

(** Check whether the cg file is changed. It is unchanged if for each defined procedure, the .specs
file exists and is more recent than the cg file. *)
let cg_check_changed exe_env source_dir cg =
  let cg_fname = DB.source_dir_get_internal_file source_dir ".cg" in
  let defined_nodes = Cg.get_defined_nodes cg in
  let changed = ref false in
  let cg_source_file = Cg.get_source cg in
  let proc_is_active pname = DB.source_file_equal (Exe_env.get_source exe_env pname) cg_source_file in
  let check_needs_update pname =
    let is_active = proc_is_active pname in
    let spec_fname = Specs.res_dir_specs_filename pname in
    if is_active then
      changed := (!changed || not (Sys.file_exists (DB.filename_to_string spec_fname)) ||
        DB.file_modified_time cg_fname > DB.file_modified_time spec_fname) in
  list_iter check_needs_update defined_nodes;
  !changed

(** Load a .c or .cpp file into an execution environment *)
let load_cg_file (_exe_env: Exe_env.initial) (source_dir : DB.source_dir) exclude_fun =
  match Exe_env.add_cg_exclude_fun _exe_env source_dir exclude_fun with
  | None -> None
  | Some cg ->
      L.err "loaded %s@." (DB.source_dir_to_string source_dir);
      Some cg

(** Load a list of cg files and return the set of changed ones if [check_changed] is true *)
let load_cg_files _exe_env check_changed (source_dirs : DB.source_dir list) exclude_fun =
  let sorted_dirs = list_sort DB.source_dir_compare source_dirs in
  let files_changed = ref Procname.Set.empty in
  let cg_list = ref [] in
  let check_cgs_changed exe_env =
    let check_cg_changed (source_dir, cg) =
      let is_changed = cg_check_changed exe_env source_dir cg in
      if is_changed then files_changed :=
        Procname.Set.add (source_file_to_pname (Cg.get_source cg)) !files_changed in
    list_iter check_cg_changed !cg_list in
  list_iter (fun source_dir ->
          match load_cg_file _exe_env source_dir exclude_fun with
          | None -> ()
          | Some cg ->
              if check_changed then cg_list := (source_dir, cg) :: !cg_list) sorted_dirs;
  let exe_env = Exe_env.freeze _exe_env in
  if check_changed then check_cgs_changed exe_env;
  !files_changed, exe_env

(** Create an exe_env from a cluster. *)
let exe_env_from_cluster cluster =
  let _exe_env = Exe_env.create (Some (cluster_to_active_procs cluster)) in
  let exclude_fun _ = false in
  let callees = ref [] in
  let source_files =
    let do_cluster_elem ce =
      let source_map = ce.ce_source_map in
      let do_callee pn file =
        callees := (pn, file) :: !callees in
      Procname.Map.iter do_callee source_map;
      DB.source_dir_from_source_file ce.ce_file in
    list_map do_cluster_elem cluster in
  let _, exe_env = load_cg_files _exe_env false source_files exclude_fun in
  let do_callee (pn, file) =
    Exe_env.add_callee exe_env file pn in
  list_iter do_callee !callees;
  exe_env

(** Analyze a cluster of files *)
let analyze_cluster cluster_num tot_clusters (cluster : cluster) =
  incr cluster_num;
  let exe_env = exe_env_from_cluster cluster in
  let num_files = list_length cluster in
  let defined_procs = Cg.get_defined_nodes (Exe_env.get_cg exe_env) in
  let num_procs = list_length defined_procs in
  L.err "@.Processing cluster #%d/%d with %d files and %d procedures@." !cluster_num tot_clusters num_files num_procs;
  Fork.this_cluster_files := num_files;
  analyze exe_env;
  Fork.tot_files_done := num_files + !Fork.tot_files_done;
  Fork.print_timing ()

let process_cluster_cmdline_exit () =
  match !cluster_cmdline with
  | None -> ()
  | Some fname ->
      (match load_cluster_from_file (DB.filename_from_string fname) with
        | None ->
            L.err "Cannot find cluster file %s@." fname;
            exit 0
        | Some (nr, tot_nr, cluster) ->
            Fork.tot_files_done := (nr - 1) * list_length cluster;
            Fork.tot_files := tot_nr * list_length cluster;
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

let log_dir_name = "log"
let analyzer_out_name = "analyzer_out"
let analyzer_err_name = "analyzer_err"

let () =
  let () =
    match !cluster_cmdline with
    | None -> L.stdout "Starting analysis (Infer version %s)@." Version.versionString;
    | Some clname -> L.stdout "Cluster %s@." clname in
  RegisterCheckers.register ();
  Facebook.register_checkers ();

  if !allow_specs_cleanup = true && !incremental_mode = ANALYZE_ALL  && !cluster_cmdline = None then
    DB.Results_dir.clean_specs_dir ();

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
  if (!Sil.curr_language = Sil.C_CPP) then Mleak_buckets.init_buckets !objc_ml_buckets_arg;

  process_cluster_cmdline_exit ();
  let source_dirs =
    if !only_files_cmdline = [] then DB.find_source_dirs ()
    else
      let filter source_dir =
        let source_dir_base = Filename.basename (DB.source_dir_to_string source_dir) in
        list_exists (fun s -> Utils.string_is_prefix s source_dir_base) !only_files_cmdline in
      list_filter filter (DB.find_source_dirs ()) in
  L.err "Found %d source files in %s@." (list_length source_dirs) !Config.results_dir;
  let _exe_env = Exe_env.create None in
  let check_changed = !incremental_mode != ANALYZE_ALL in
  let files_changed, exe_env = load_cg_files _exe_env check_changed source_dirs (compute_exclude_fun ()) in
  L.err "Procedures defined in more than one file: %a" Procname.pp_set (Exe_env.get_procs_defined_in_several_files exe_env);
  let clusters = compute_clusters exe_env files_changed in
  let tot_clusters = list_length clusters in
  Fork.tot_files := list_fold_left (fun n cluster -> n + list_length cluster) 0 clusters;
  list_iter (analyze_cluster (ref 0) tot_clusters) clusters;
  L.flush_streams ();
  close_output_file analyzer_out_of;
  close_output_file analyzer_err_of;
  if !cluster_cmdline = None then L.stdout "Analysis finished in %as@." pp_elapsed_time ()
