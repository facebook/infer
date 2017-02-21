/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;

open! PVariant;

let aggregated_stats_filename = "aggregated_stats.json";

let aggregated_stats_by_target_filename = "aggregated_stats_by_target.json";

let json_files_to_ignore_regex = Str.regexp (
  ".*\\(" ^
  Str.quote aggregated_stats_filename ^
  "\\|" ^ Str.quote aggregated_stats_by_target_filename ^ "\\)$"
);

let dir_exists dir => Sys.is_directory dir == `Yes;

let find_json_files_in_dir dir => {
  let is_valid_json_file path => {
    let s = Unix.lstat path;
    let json_regex = Str.regexp_case_fold ".*\\.json$";
    not (Str.string_match json_files_to_ignore_regex path 0) &&
    Str.string_match json_regex path 0 && Polymorphic_compare.(==) s.st_kind Unix.S_REG
  };
  dir_exists dir ?
    {
      let content = Array.to_list (Sys.readdir dir);
      let content_with_path = IList.map (fun p => Filename.concat dir p) content;
      List.filter f::is_valid_json_file content_with_path
    } :
    []
};

type stats_paths = {
  frontend_paths: list string,
  backend_paths: list string,
  reporting_paths: list string
};

type origin =
  | Buck_out (list (string, stats_paths))
  | Infer_out stats_paths;

let find_stats_files_in_dir dir => {
  let frontend_paths = find_json_files_in_dir (Filename.concat dir Config.frontend_stats_dir_name);
  let backend_paths = find_json_files_in_dir (Filename.concat dir Config.backend_stats_dir_name);
  let reporting_paths = find_json_files_in_dir (
    Filename.concat dir Config.reporting_stats_dir_name
  );
  {frontend_paths, backend_paths, reporting_paths}
};

let load_data_from_infer_deps file => {
  let extract_target_and_path line =>
    switch (Str.split_delim (Str.regexp (Str.quote "\t")) line) {
    | [target, _, path, ..._] =>
      if (dir_exists path) {
        (target, path)
      } else {
        raise (Failure ("path '" ^ path ^ "' is not a valid directory"))
      }
    | _ => raise (Failure "malformed input")
    };
  let lines = Utils.read_file file;
  try (
    switch lines {
    | Some l => Ok (IList.map extract_target_and_path l)
    | None => raise (Failure ("Error reading '" ^ file ^ "'"))
    }
  ) {
  | Failure msg => Error msg
  }
};

let collect_all_stats_files () => {
  let infer_out = Config.results_dir;
  let concatenate_paths p1 p2 =>
    if (Filename.is_relative p2) {
      Filename.concat p1 p2
    } else {
      p2
    };
  switch Config.buck_out {
  | Some p =>
    if (dir_exists p) {
      let data = load_data_from_infer_deps (
        Filename.concat infer_out Config.buck_infer_deps_file_name
      );
      switch data {
      | Ok r =>
        let buck_out_parent = Filename.concat p Filename.parent_dir_name;
        let targets_files =
          IList.map
            (fun (t, p) => (t, find_stats_files_in_dir (concatenate_paths buck_out_parent p))) r;
        Ok (Buck_out targets_files)
      | Error _ as e => e
      }
    } else {
      Error ("buck-out path '" ^ p ^ "' not found")
    }
  | None => Ok (Infer_out (find_stats_files_in_dir infer_out))
  }
};

let aggregate_stats_files paths => {
  let open_json_file file => Yojson.Basic.from_file file;
  let load_stats paths => IList.map (fun path => PerfStats.from_json (open_json_file path)) paths;
  let all_perf_stats = load_stats paths;
  switch all_perf_stats {
  | [] => None
  | _ => Some (PerfStats.aggregate all_perf_stats)
  }
};

type json_aggregated_stats = {
  frontend_json_data: option Yojson.Basic.json,
  backend_json_data: option Yojson.Basic.json,
  reporting_json_data: option Yojson.Basic.json
};

let aggregate_all_stats origin => {
  let accumulate_paths acc paths => {
    frontend_paths: paths.frontend_paths @ acc.frontend_paths,
    backend_paths: paths.backend_paths @ acc.backend_paths,
    reporting_paths: paths.reporting_paths @ acc.reporting_paths
  };
  let empty_stats_paths = {frontend_paths: [], backend_paths: [], reporting_paths: []};
  let stats_paths =
    switch origin {
    | Buck_out tf =>
      List.fold f::(fun acc (_, paths) => accumulate_paths acc paths) init::empty_stats_paths tf
    | Infer_out paths => paths
    };
  {
    frontend_json_data: aggregate_stats_files stats_paths.frontend_paths,
    backend_json_data: aggregate_stats_files stats_paths.backend_paths,
    reporting_json_data: aggregate_stats_files stats_paths.reporting_paths
  }
};

let aggregate_stats_by_target tp => {
  let to_json f aggr_stats => {
    let collect_valid_stats acc t p =>
      switch p {
      | Some v => [(t, v), ...acc]
      | None => acc
      };
    let l = List.fold f::(fun acc (t, p) => collect_valid_stats acc t (f p)) init::[] aggr_stats;
    switch l {
    | [] => None
    | _ as v => Some (`Assoc v)
    }
  };
  let frontend_json_data = to_json (fun p => aggregate_stats_files p.frontend_paths) tp;
  let backend_json_data = to_json (fun p => aggregate_stats_files p.backend_paths) tp;
  let reporting_json_data = to_json (fun p => aggregate_stats_files p.reporting_paths) tp;
  {frontend_json_data, backend_json_data, reporting_json_data}
};

let generate_files () => {
  let infer_out = Config.results_dir;
  let stats_files = collect_all_stats_files ();
  let origin =
    switch stats_files {
    | Ok origin => origin
    | Error e => failwith e
    };
  let aggregated_frontend_stats_dir = Filename.concat infer_out Config.frontend_stats_dir_name;
  let aggregated_backend_stats_dir = Filename.concat infer_out Config.backend_stats_dir_name;
  let aggregated_reporting_stats_dir = Filename.concat infer_out Config.reporting_stats_dir_name;
  Utils.create_dir aggregated_frontend_stats_dir;
  Utils.create_dir aggregated_backend_stats_dir;
  Utils.create_dir aggregated_reporting_stats_dir;
  let write_to_json_file_opt destfile json =>
    switch json {
    | Some j => Utils.write_json_to_file destfile j
    | None => ()
    };
  switch origin {
  | Buck_out tp =>
    let j = aggregate_stats_by_target tp;
    write_to_json_file_opt
      (Filename.concat aggregated_frontend_stats_dir aggregated_stats_by_target_filename)
      j.frontend_json_data;
    write_to_json_file_opt
      (Filename.concat aggregated_backend_stats_dir aggregated_stats_by_target_filename)
      j.backend_json_data;
    write_to_json_file_opt
      (Filename.concat aggregated_reporting_stats_dir aggregated_stats_by_target_filename)
      j.reporting_json_data
  | Infer_out _ => ()
  };
  let j = aggregate_all_stats origin;
  write_to_json_file_opt
    (Filename.concat aggregated_frontend_stats_dir aggregated_stats_filename) j.frontend_json_data;
  write_to_json_file_opt
    (Filename.concat aggregated_backend_stats_dir aggregated_stats_filename) j.backend_json_data;
  write_to_json_file_opt
    (Filename.concat aggregated_reporting_stats_dir aggregated_stats_filename)
    j.reporting_json_data
};
