/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;


/** Main module for the analysis after the capture phase */
let module L = Logging;

let module F = Format;

let analyze_exe_env exe_env => {
  let init_time = Unix.gettimeofday ();
  L.log_progress_file ();
  Specs.clear_spec_tbl ();
  Random.self_init ();
  if Config.checkers {
    /* run the checkers only */
    let call_graph = Exe_env.get_cg exe_env;
    Callbacks.iterate_callbacks Checkers.ST.store_summary call_graph exe_env
  } else {
    /* run the full analysis */
    Interproc.do_analysis exe_env;
    Printer.write_all_html_files exe_env;
    Interproc.print_stats exe_env;
    let elapsed = Unix.gettimeofday () -. init_time;
    L.out "Interprocedural footprint analysis terminated in %f sec@." elapsed
  }
};


/** Create an exe_env from a cluster. */
let exe_env_from_cluster cluster => {
  let _exe_env = Exe_env.create ();
  Exe_env.add_cg _exe_env cluster;
  Exe_env.freeze _exe_env
};


/** Analyze a cluster of files */
let analyze_cluster cluster_num (cluster: Cluster.t) => {
  let exe_env = exe_env_from_cluster cluster;
  let defined_procs = Cg.get_defined_nodes (Exe_env.get_cg exe_env);
  let num_procs = IList.length defined_procs;
  L.err "@.Processing cluster #%d with %d procedures@." (cluster_num + 1) num_procs;
  analyze_exe_env exe_env
};

let output_json_makefile_stats clusters => {
  let clusters_to_analyze = IList.filter ClusterMakefile.cluster_should_be_analyzed clusters;
  let num_files = IList.length clusters_to_analyze;
  let num_procs = 0;
  /* can't compute it at this stage */
  let num_lines = 0;
  let file_stats =
    `Assoc [("files", `Int num_files), ("procedures", `Int num_procs), ("lines", `Int num_lines)];
  /* write stats file to disk, intentionally overwriting old file if it already exists */
  let f = open_out (Filename.concat Config.results_dir Config.proc_stats_filename);
  Yojson.Basic.pretty_to_channel f file_stats
};

let process_cluster_cmdline fname =>
  switch (Cluster.load_from_file (DB.filename_from_string fname)) {
  | None => L.err "Cannot find cluster file %s@." fname
  | Some (nr, cluster) => analyze_cluster (nr - 1) cluster
  };

let print_stdout_legend () => {
  L.stdout "Starting analysis...@\n";
  L.stdout "@\n";
  L.stdout "legend:@\n";
  L.stdout "  \"%s\" analyzing a file@\n" Config.log_analysis_file;
  L.stdout "  \"%s\" analyzing a procedure@\n" Config.log_analysis_procedure;
  if Config.stats_mode {
    L.stdout "  \"%s\" analyzer crashed@\n" Config.log_analysis_crash;
    L.stdout
      "  \"%s\" timeout: procedure analysis took too much time@\n"
      Config.log_analysis_wallclock_timeout;
    L.stdout
      "  \"%s\" timeout: procedure analysis took too many symbolic execution steps@\n"
      Config.log_analysis_symops_timeout;
    L.stdout
      "  \"%s\" timeout: procedure analysis took too many recursive iterations@\n"
      Config.log_analysis_recursion_timeout
  };
  L.stdout "@\n@?"
};

let main makefile => {
  BuiltinDefn.init ();
  RegisterCheckers.register ();
  switch Config.modified_targets {
  | Some file => MergeCapture.modified_file file
  | None => ()
  };
  switch Config.cluster_cmdline {
  | Some fname => process_cluster_cmdline fname
  | None =>
    print_stdout_legend ();
    if Config.allow_specs_cleanup {
      DB.Results_dir.clean_specs_dir ()
    };
    if Config.merge {
      MergeCapture.merge_captured_targets ()
    };
    let clusters = DB.find_source_dirs ();
    L.stdout "Found %d source files in %s@." (IList.length clusters) Config.results_dir;
    if (makefile != "") {
      ClusterMakefile.create_cluster_makefile clusters makefile
    } else {
      IList.iteri (fun i cluster => analyze_cluster i cluster) clusters;
      L.stdout "@\nAnalysis finished in %as@." Pp.elapsed_time ()
    };
    output_json_makefile_stats clusters
  }
};
