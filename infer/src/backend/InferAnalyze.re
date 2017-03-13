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


/** Create tasks to analyze an execution environment */
let analyze_exe_env_tasks cluster exe_env :Tasks.t => {
  L.log_progress_file ();
  Specs.clear_spec_tbl ();
  Random.self_init ();
  if Config.checkers {
    /* run the checkers only */
    Tasks.create [
      fun () => {
        let call_graph = Exe_env.get_cg exe_env;
        Callbacks.iterate_callbacks call_graph exe_env;
        if Config.write_html {
          Printer.write_all_html_files cluster
        }
      }
    ]
  } else {
    /* run the full analysis */
    Tasks.create
      (Interproc.do_analysis_closures exe_env)
      continuation::(
        if (Config.write_html || Config.developer_mode) {
          Some (
            fun () => {
              if Config.write_html {
                Printer.write_all_html_files cluster
              };
              if Config.developer_mode {
                Interproc.print_stats cluster
              }
            }
          )
        } else {
          None
        }
      )
  }
};


/** Create tasks to analyze a cluster */
let analyze_cluster_tasks cluster_num (cluster: Cluster.t) :Tasks.t => {
  let exe_env = Exe_env.from_cluster cluster;
  let defined_procs = Cg.get_defined_nodes (Exe_env.get_cg exe_env);
  let num_procs = List.length defined_procs;
  L.err "@.Processing cluster #%d with %d procedures@." (cluster_num + 1) num_procs;
  analyze_exe_env_tasks cluster exe_env
};

let analyze_cluster cluster_num cluster => Tasks.run (analyze_cluster_tasks cluster_num cluster);

let output_json_makefile_stats clusters => {
  let num_files = List.length clusters;
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
  if (Config.stats_mode || Config.debug_mode) {
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

let cluster_should_be_analyzed cluster => {
  let fname = DB.source_dir_to_string cluster;
  let in_ondemand_config =
    Option.map f::(fun dirs => String.Set.mem dirs fname) Ondemand.dirs_to_analyze;
  let check_modified () => {
    let modified = DB.file_was_updated_after_start (DB.filename_from_string fname);
    if (modified && Config.developer_mode) {
      L.stdout "Modified: %s@." fname
    };
    modified
  };
  switch in_ondemand_config {
  | Some b =>
    /* ondemand config file is specified */
    b
  | None when Config.reactive_mode => check_modified ()
  | None => true
  }
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
    let all_clusters = DB.find_source_dirs ();
    let clusters_to_analyze = List.filter f::cluster_should_be_analyzed all_clusters;
    L.stdout
      "Found %d (out of %d) source files to be analyzed in %s@."
      (List.length clusters_to_analyze)
      (List.length all_clusters)
      Config.results_dir;
    if (makefile != "" || Config.per_procedure_parallelism) {
      let is_java () =>
        List.exists
          f::(
            fun cl => SourceFile.string_crc_has_extension ext::"java" (DB.source_dir_to_string cl)
          )
          all_clusters;
      if (not Config.per_procedure_parallelism) {
        ClusterMakefile.create_cluster_makefile clusters_to_analyze makefile
      } else {
        /* per-procedure parallelism */
        if (is_java ()) {
          /* Java uses ZipLib which is incompatible with forking */
          L.stderr "Error: option --per-procedure-parallelism not supported with Java@.";
          exit 1
        };
        L.stdout "per-procedure parallelism jobs:%d@." Config.jobs;
        if (makefile != "") {
          ClusterMakefile.create_cluster_makefile [] makefile
        };
        /* Prepare tasks one cluster at a time while executing in parallel */
        let runner = Tasks.Runner.create jobs::Config.jobs;
        let cluster_start_tasks i cluster => {
          let tasks = analyze_cluster_tasks i cluster;
          let aggregate_tasks = Tasks.aggregate size::1 tasks;
          Tasks.Runner.start runner tasks::aggregate_tasks
        };
        List.iteri f::cluster_start_tasks clusters_to_analyze;
        Tasks.Runner.complete runner
      }
    } else {
      /* This branch is reached when -j 1 is used */
      List.iteri f::analyze_cluster clusters_to_analyze;
      L.stdout "@\nAnalysis finished in %as@." Pp.elapsed_time ()
    };
    output_json_makefile_stats clusters_to_analyze
  }
};
