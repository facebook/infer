(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Main module for the analysis after the capture phase *)

module L = Logging
module F = Format

(** command line option: if true, run the analysis in checker mode *)
let checkers = ref false

(** command line option: name of the makefile to create with clusters and dependencies *)
let makefile_cmdline = ref ""

(** optional command-line name of the .cluster file *)
let cluster_cmdline = ref None

(** value of -out_file command-line *)
let out_file_cmdline = ref ""

(** value of -err_file command-line *)
let err_file_cmdline = ref ""

(** List of obj memory leak buckets to be checked in Objective-C/C++ *)
let ml_buckets_arg = ref "cf"

(** Whether specs can be cleaned up before starting analysis *)
let allow_specs_cleanup = ref false

let version_string () =
  "Infer version "
  ^ Version.versionString
  ^ "\nCopyright 2009 - present Facebook. All Rights Reserved.\n"

let print_version () =
  F.fprintf F.std_formatter "%s@." (version_string ());
  exit 0

let print_version_json () =
  F.fprintf F.std_formatter "%s@." Version.versionJson;
  exit 0

let arg_desc =
  let base_arg =
    let desc =
      base_arg_desc @
      [
        "-err_file",
        Arg.Set_string err_file_cmdline,
        Some "file",
        "use file for the err channel"
        ;
        "-iterations",
        Arg.Set_int Config.iterations,
        Some "n",
        "set the max number of operations for each function, \
         expressed as a multiple of symbolic operations (default n=1)"
        ;
        "-nonstop",
        Arg.Set Config.nonstop,
        None,
        "activate the nonstop mode: the analysis continues after finding errors. \
         With this option the analysis can become less precise."
        ;
        "-out_file",
        Arg.Set_string out_file_cmdline,
        Some "file",
        "use file for the out channel"
        ;
        "-print_builtins",
        Arg.Unit Builtin.print_and_exit,
        None,
        "print the builtin functions and exit"
        ;
        "-reactive",
        Arg.Unit (fun () -> Config.reactive_mode := true),
        None,
        "analyze in reactive propagation mode starting from changed files"
        ;
        "-continue",
        Arg.Set Config.continue_capture,
        None,
        "continue the capture for the reactive analysis,\
         increasing the changed files/procedures."
        ;
        (* TODO: merge with the -project_root option *)
        "-java",
        Arg.Unit (fun () -> Config.curr_language := Config.Java),
        None,
        "Set language to Java"
        ;
        "-version",
        Arg.Unit print_version,
        None,
        "print version information and exit"
        ;
        "-version_json",
        Arg.Unit print_version_json,
        None,
        "print version json formatted"
        ;
        "-objcm",
        Arg.Set Config.objc_memory_model_on,
        None,
        "Use ObjC memory model"
        ;
        "-no_progress_bar",
        Arg.Unit (fun () -> Config.show_progress_bar := false),
        None,
        "Do not show a progress bar"
        ;
        "-ml_buckets",
        Arg.Set_string ml_buckets_arg,
        Some "ml_buckets",
        "memory leak buckets to be checked, separated by commas. \
         The possible buckets are cf (Core Foundation), arc, narc (No arc), cpp, unknown_origin"
        ;
      ] in
    Arg.create_options_desc false "Analysis Options" desc in
  let reserved_arg =
    let desc =
      reserved_arg_desc @
      [
        "-allow_specs_cleanup",
        Arg.Unit (fun () -> allow_specs_cleanup := true),
        None,
        "Allow to remove existing specs before running analysis when it's not incremental"
        ;
        "-analysis_stops",
        Arg.Set Config.analysis_stops,
        None,
        "issue a warning when the analysis stops"
        ;
        "-angelic_execution",
        Arg.Set Config.angelic_execution,
        None,
        "activate angelic execution: \
         The analysis ignores errors caused by unknown procedure calls."
        ;
        "-checkers",
        Arg.Unit (fun () -> checkers := true),
        None,
        " run only the checkers instead of the full analysis"
        ;
        "-cluster",
        Arg.String (fun s -> cluster_cmdline := Some s),
        Some "fname",
        "specify a .cluster file to be analyzed"
        ;
        "-codequery",
        Arg.String (fun s -> CodeQuery.query := Some s),
        Some "query",
        " execute the code query"
        ;
        "-eradicate",
        Arg.Unit (fun () ->
            Config.eradicate := true;
            checkers := true),
        None,
        " activate the eradicate checker for java annotations"
        ;
        "-merge",
        Arg.Set Config.merge,
        None,
        "merge the captured results directories specified in the dependency file"
        ;
        "-makefile",
        Arg.Set_string makefile_cmdline,
        Some "file",
        "create a makefile to perform the analysis"
        ;
        "-modified_targets",
        Arg.String (fun file -> MergeCapture.modified_file file),
        Some "file",
        "read the file of buck targets modified since the last analysis"
        ;
        "-optimistic_cast",
        Arg.Set Config.optimistic_cast,
        None,
        "allow cast of undefined values"
        ;
        "-print_buckets",
        Arg.Unit (fun() -> Config.show_buckets := true; Config.show_ml_buckets := true),
        None,
        "Add buckets to issue descriptions, useful when developing infer"
        ;
        "-seconds_per_iteration",
        Arg.Set_float Config.seconds_per_iteration,
        Some "n",
        "set the number of seconds per iteration (default n=30)"
        ;
        "-subtype_multirange",
        Arg.Set Config.subtype_multirange,
        None,
        "use the multirange subtyping domain"
        ;
        "-symops_per_iteration",
        Arg.Set_int Config.symops_per_iteration,
        Some "n",
        "set the number of symbolic operations per iteration (default n="
        ^ (string_of_int !Config.symops_per_iteration) ^ ")"
        ;
        "-tracing",
        Arg.Unit (fun () -> Config.report_runtime_exceptions := true),
        None,
        "Report error traces for runtime exceptions (Only for Java)"
        ;
        "-type_size",
        Arg.Set Config.type_size,
        None,
        "consider the size of types during analysis"
        ;
      ] in
    Arg.create_options_desc false
      "Reserved Options: Experimental features, use with caution!" desc in
  base_arg @ reserved_arg

let usage =
  (version_string ()) ^ "\
  Usage: InferAnalyze [options]\n\
  Analyze the files captured in the project results directory, \
  which can be specified with the -results_dir option."

let print_usage_exit () =
  Arg.usage arg_desc usage;
  exit(1)

let () = (* parse command-line arguments *)
  let f _ =
    () (* ignore anonymous arguments *) in
  Arg.parse "INFER_ARGS" arg_desc f usage;
  if not (Sys.file_exists !Config.results_dir) then
    begin
      L.err "ERROR: results directory %s does not exist@.@." !Config.results_dir;
      print_usage_exit ()
    end

let analyze_exe_env exe_env =
  let init_time = Unix.gettimeofday () in
  L.log_progress_file ();
  Specs.clear_spec_tbl ();
  Random.self_init ();
  let line_reader = Printer.LineReader.create () in
  if !checkers then
    begin
      (** run the checkers only *)
      let call_graph = Exe_env.get_cg exe_env in
      Callbacks.iterate_callbacks Checkers.ST.store_summary call_graph exe_env
    end
  else
    begin
      (** run the full analysis *)
      Interproc.do_analysis exe_env;
      Printer.write_all_html_files line_reader exe_env;
      Interproc.print_stats exe_env;
      let elapsed = Unix.gettimeofday () -. init_time in
      L.out "Interprocedural footprint analysis terminated in %f sec@." elapsed
    end

(** Create an exe_env from a cluster. *)
let exe_env_from_cluster cluster =
  let _exe_env = Exe_env.create () in
  let source_dirs = [cluster] in
  let sorted_dirs = IList.sort DB.source_dir_compare source_dirs in
  IList.iter (fun src_dir -> ignore (Exe_env.add_cg _exe_env src_dir)) sorted_dirs;
  let exe_env = Exe_env.freeze _exe_env in
  exe_env

(** Analyze a cluster of files *)
let analyze_cluster cluster_num (cluster : Cluster.t) =
  let exe_env = exe_env_from_cluster cluster in
  let defined_procs = Cg.get_defined_nodes (Exe_env.get_cg exe_env) in
  let num_procs = IList.length defined_procs in
  L.err "@.Processing cluster #%d with %d procedures@."
    (cluster_num + 1) num_procs;
  analyze_exe_env exe_env

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
  | Some (_, cout) -> close_out cout

let setup_logging () =
  if !Config.developer_mode then
    let log_dir_name = "log" in
    let analyzer_out_name = "analyzer_out" in
    let analyzer_err_name = "analyzer_err" in
    let log_dir =
      DB.filename_to_string
        (DB.Results_dir.path_to_filename DB.Results_dir.Abs_root [log_dir_name]) in
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

let output_json_makefile_stats clusters =
  let clusters_to_analyze =
    IList.filter ClusterMakefile.cluster_should_be_analyzed clusters in
  let num_files = IList.length clusters_to_analyze in
  let num_procs = 0 (* can't compute it at this stage *) in
  let num_lines = 0 in
  let file_stats =
    `Assoc [ ("files", `Int num_files);
             ("procedures", `Int num_procs);
             ("lines", `Int num_lines) ] in
  (* write stats file to disk, intentionally overwriting old file if it already exists *)
  let f = open_out (Filename.concat !Config.results_dir Config.proc_stats_filename) in
  Yojson.Basic.pretty_to_channel f file_stats

let print_prolog () =
  match !cluster_cmdline with
  | None ->
      L.stdout "Starting analysis (Infer version %s)@." Version.versionString;
  | Some clname -> L.stdout "Cluster %s@." clname

let process_cluster_cmdline fname =
  match Cluster.load_from_file (DB.filename_from_string fname) with
  | None ->
      L.err "Cannot find cluster file %s@." fname
  | Some (nr, cluster) ->
      analyze_cluster (nr - 1)  cluster

let register_perf_stats_report () =
  let stats_dir = Filename.concat !Config.results_dir Config.backend_stats_dir_name in
  let cluster = match !cluster_cmdline with Some cl -> "_" ^ cl | None -> "" in
  let stats_file = Filename.concat stats_dir (Config.perf_stats_prefix ^ cluster ^ ".json") in
  DB.create_dir !Config.results_dir ;
  DB.create_dir stats_dir ;
  PerfStats.register_report_at_exit stats_file

let () =
  register_perf_stats_report () ;

  if !Config.developer_mode then
    Printexc.record_backtrace true;
  print_prolog ();
  RegisterCheckers.register ();

  if !allow_specs_cleanup = true && !cluster_cmdline = None then
    DB.Results_dir.clean_specs_dir ();

  let analyzer_out_of, analyzer_err_of = setup_logging () in

  if !Config.curr_language = Config.C_CPP
  then Mleak_buckets.init_buckets !ml_buckets_arg;

  let finish_logging () =
    teardown_logging analyzer_out_of analyzer_err_of in

  match !cluster_cmdline with
  | Some fname ->
      process_cluster_cmdline fname;
      finish_logging ()
  | None ->
      if !Config.merge then MergeCapture.merge_captured_targets ();
      let clusters = DB.find_source_dirs () in
      L.err "Found %d source files in %s@."
        (IList.length clusters) !Config.results_dir;

      if !makefile_cmdline <> ""
      then
        ClusterMakefile.create_cluster_makefile clusters !makefile_cmdline
      else
        begin
          IList.iteri
            (fun i cluster -> analyze_cluster i cluster)
            clusters;
          L.stdout "Analysis finished in %as@." pp_elapsed_time ()
        end;
      output_json_makefile_stats clusters;
      finish_logging ()
