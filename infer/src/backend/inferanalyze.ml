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

let () =
  if Config.print_builtins then Builtin.print_and_exit () ;
  (match Config.modified_targets with Some file -> MergeCapture.modified_file file | None -> ()) ;
  if not (Sys.file_exists Config.results_dir) then
    begin
      L.err "ERROR: results directory %s does not exist@.@." Config.results_dir;
      Config.print_usage_exit ()
    end

let analyze_exe_env exe_env =
  let init_time = Unix.gettimeofday () in
  L.log_progress_file ();
  Specs.clear_spec_tbl ();
  Random.self_init ();
  let line_reader = Printer.LineReader.create () in
  if Config.checkers then
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
  if Config.developer_mode then
    let log_dir_name = "log" in
    let analyzer_out_name = "analyzer_out" in
    let analyzer_err_name = "analyzer_err" in
    let log_dir =
      DB.filename_to_string
        (DB.Results_dir.path_to_filename DB.Results_dir.Abs_root [log_dir_name]) in
    DB.create_dir log_dir;
    let analyzer_out_file =
      if Config.out_file_cmdline = "" then Filename.concat log_dir analyzer_out_name
      else Config.out_file_cmdline in
    let analyzer_err_file =
      if Config.err_file_cmdline = "" then Filename.concat log_dir analyzer_err_name
      else Config.err_file_cmdline in
    let analyzer_out_of = open_output_file Logging.set_out_formatter analyzer_out_file in
    let analyzer_err_of = open_output_file Logging.set_err_formatter analyzer_err_file in
    analyzer_out_of, analyzer_err_of
  else None, None

let teardown_logging analyzer_out_of analyzer_err_of =
  if Config.developer_mode then
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
  let f = open_out (Filename.concat Config.results_dir Config.proc_stats_filename) in
  Yojson.Basic.pretty_to_channel f file_stats

let print_prolog () =
  match Config.cluster_cmdline with
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
  let stats_dir = Filename.concat Config.results_dir Config.backend_stats_dir_name in
  let cluster = match Config.cluster_cmdline with Some cl -> "_" ^ cl | None -> "" in
  let stats_base = Config.perf_stats_prefix ^ "_" ^ (Filename.basename cluster) ^ ".json" in
  let stats_file = Filename.concat stats_dir stats_base in
  DB.create_dir Config.results_dir ;
  DB.create_dir stats_dir ;
  PerfStats.register_report_at_exit stats_file

let () =
  register_perf_stats_report () ;

  if Config.developer_mode then
    Printexc.record_backtrace true;
  print_prolog ();
  RegisterCheckers.register ();

  if Config.allow_specs_cleanup = true && Config.cluster_cmdline = None then
    DB.Results_dir.clean_specs_dir ();

  let analyzer_out_of, analyzer_err_of = setup_logging () in

  let finish_logging () =
    teardown_logging analyzer_out_of analyzer_err_of in

  match Config.cluster_cmdline with
  | Some fname ->
      process_cluster_cmdline fname;
      finish_logging ()
  | None ->
      if Config.merge then MergeCapture.merge_captured_targets ();
      let clusters = DB.find_source_dirs () in
      L.err "Found %d source files in %s@."
        (IList.length clusters) Config.results_dir;

      if Config.makefile_cmdline <> ""
      then
        ClusterMakefile.create_cluster_makefile clusters Config.makefile_cmdline
      else
        begin
          IList.iteri
            (fun i cluster -> analyze_cluster i cluster)
            clusters;
          L.stdout "Analysis finished in %as@." pp_elapsed_time ()
        end;
      output_json_makefile_stats clusters;
      finish_logging ()
