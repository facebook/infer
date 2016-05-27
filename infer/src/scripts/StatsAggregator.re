/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 *
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open! Utils;

let filename = "aggregated_stats.json";

let dir_exists dir =>
  try (Sys.is_directory dir) {
  | Sys_error _ => false
  };

let find_json_files_in_dir dir => {
  let is_valid_json_file path => {
    let s = Unix.lstat path;
    let json_regex = Str.regexp_case_fold ".*\\.json$";
    not (Str.string_match (Str.regexp (".*" ^ Str.quote filename ^ "$")) path 0) &&
      Str.string_match json_regex path 0 && s.st_kind == Unix.S_REG
  };
  dir_exists dir ?
    {
      let content = Array.to_list (Sys.readdir dir);
      let content_with_path = IList.map (fun p => Filename.concat dir p) content;
      IList.filter is_valid_json_file content_with_path
    } :
    []
};

let find_stats_files_in_dir dir => {
  let frontend_stats_files = find_json_files_in_dir (
    Filename.concat dir Config.frontend_stats_dir_name
  );
  let backend_stats_files = find_json_files_in_dir (
    Filename.concat dir Config.backend_stats_dir_name
  );
  let reporting_stats_files = find_json_files_in_dir (
    Filename.concat dir Config.reporting_stats_dir_name
  );
  (frontend_stats_files, backend_stats_files, reporting_stats_files)
};

let load_data_from_infer_deps file => {
  let extract_path line =>
    switch (Str.split_delim (Str.regexp (Str.quote "\t")) line) {
    | [target, _, path, ..._] =>
      if (dir_exists path) {
        (target, path)
      } else {
        raise (Failure ("path '" ^ path ^ "' is not a valid directory"))
      }
    | _ => raise (Failure "malformed input")
    };
  let lines = Option.get (Utils.read_file file);
  try (Ok (IList.map extract_path lines)) {
  | Failure msg => Error msg
  }
};

let find_all_stats_files () => {
  let accumulate_paths acc paths => {
    let (f, b, r) = acc;
    let (f', b', r') = paths;
    (f @ f', b @ b', r @ r')
  };
  let concatenate_paths p1 p2 =>
    if (Filename.is_relative p2) {
      Filename.concat p1 p2
    } else {
      p2
    };
  let infer_out = Config.results_dir;
  let result =
    switch Config.buck_out {
    | Some p =>
      if (dir_exists p) {
        let data = load_data_from_infer_deps (
          Filename.concat infer_out Config.buck_infer_deps_file_name
        );
        switch data {
        | Ok r =>
          let paths = IList.map (fun (_, path) => path) r;
          Ok (Filename.concat p Filename.parent_dir_name, paths)
        | Error _ as e => e
        }
      } else {
        Error ("buck-out path '" ^ p ^ "' not found")
      }
    | None => Ok (infer_out, [infer_out])
    };
  switch result {
  | Ok (base_path, paths_to_explore) =>
    Ok (
      IList.fold_left
        (
          fun acc path =>
            accumulate_paths acc (find_stats_files_in_dir (concatenate_paths base_path path))
        )
        ([], [], [])
        paths_to_explore
    )
  | Error _ as e => e
  }
};

let open_json_file file => Yojson.Basic.from_file file;

let write_to_json_file destfile json => {
  let stats_oc = open_out destfile;
  Yojson.Basic.pretty_to_channel stats_oc json;
  close_out stats_oc
};

let aggregate_stats_to_file paths destfile => {
  let load_stats paths => IList.map (fun path => PerfStats.from_json (open_json_file path)) paths;
  let all_perf_stats = load_stats paths;
  switch all_perf_stats {
  | [] => Printf.eprintf "No stats to aggregate into %s\n" destfile
  | _ =>
    let aggr_stats = PerfStats.aggregate all_perf_stats;
    write_to_json_file destfile aggr_stats
  }
};

let () = {
  let infer_out = Config.results_dir;
  let result = find_all_stats_files ();
  switch result {
  | Ok (f, b, r) =>
    let aggregated_frontend_stats_dir = Filename.concat infer_out Config.frontend_stats_dir_name;
    let aggregated_backend_stats_dir = Filename.concat infer_out Config.backend_stats_dir_name;
    let aggregated_reporting_stats_dir = Filename.concat infer_out Config.reporting_stats_dir_name;
    DB.create_dir aggregated_frontend_stats_dir;
    DB.create_dir aggregated_backend_stats_dir;
    DB.create_dir aggregated_reporting_stats_dir;
    aggregate_stats_to_file f (Filename.concat aggregated_frontend_stats_dir filename);
    aggregate_stats_to_file b (Filename.concat aggregated_backend_stats_dir filename);
    aggregate_stats_to_file r (Filename.concat aggregated_reporting_stats_dir filename)
  | Error msg => failwith msg
  }
};
