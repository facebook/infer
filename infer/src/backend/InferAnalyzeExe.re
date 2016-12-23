/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;


/** Main module for the analysis after the capture phase */
let module L = Logging;

let register_perf_stats_report () => {
  let stats_dir = Filename.concat Config.results_dir Config.backend_stats_dir_name;
  let cluster =
    switch Config.cluster_cmdline {
    | Some cl => "_" ^ cl
    | None => ""
    };
  let stats_base = Config.perf_stats_prefix ^ Filename.basename cluster ^ ".json";
  let stats_file = Filename.concat stats_dir stats_base;
  Utils.create_dir Config.results_dir;
  Utils.create_dir stats_dir;
  PerfStats.register_report_at_exit stats_file
};

let () = {
  Logging.set_log_file_identifier
    CommandLineOption.Analyze (Option.map f::Filename.basename Config.cluster_cmdline);
  if Config.print_builtins {
    Builtin.print_and_exit ()
  };
  if (Sys.file_exists Config.results_dir != `Yes) {
    L.err "ERROR: results directory %s does not exist@.@." Config.results_dir;
    Config.print_usage_exit ()
  };
  register_perf_stats_report ();
  InferAnalyze.main Config.makefile_cmdline
};
