(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
open! PVariant

let aggregated_stats_filename = "aggregated_stats.json"

let aggregated_stats_by_target_filename = "aggregated_stats_by_target.json"

let json_files_to_ignore_regex =
  Str.regexp
    ( ".*\\(" ^ Str.quote aggregated_stats_filename ^ "\\|"
    ^ Str.quote aggregated_stats_by_target_filename ^ "\\)$" )

let dir_exists dir = Sys.is_directory dir = `Yes

let find_json_files_in_dir dir =
  let is_valid_json_file path =
    let s = Unix.lstat path in
    let json_regex = Str.regexp_case_fold ".*\\.json$" in
    not (Str.string_match json_files_to_ignore_regex path 0) && Str.string_match json_regex path 0
    && Polymorphic_compare.( = ) s.st_kind Unix.S_REG
  in
  match dir_exists dir with
  | true
   -> let content = Array.to_list (Sys.readdir dir) in
      let content_with_path = List.map ~f:(fun p -> Filename.concat dir p) content in
      List.filter ~f:is_valid_json_file content_with_path
  | false
   -> []

type stats_paths =
  {frontend_paths: string list; backend_paths: string list; reporting_paths: string list}

type origin = Buck_out of (string * stats_paths) list | Infer_out of stats_paths

let find_stats_files_in_dir dir =
  let frontend_paths =
    find_json_files_in_dir (Filename.concat dir Config.frontend_stats_dir_name)
  in
  let backend_paths = find_json_files_in_dir (Filename.concat dir Config.backend_stats_dir_name) in
  let reporting_paths =
    find_json_files_in_dir (Filename.concat dir Config.reporting_stats_dir_name)
  in
  {frontend_paths; backend_paths; reporting_paths}

let load_data_from_infer_deps file =
  let extract_target_and_path line =
    match Str.split_delim (Str.regexp (Str.quote "\t")) line with
    | target :: _ :: path :: _
     -> if dir_exists path then (target, path)
        else raise (Failure ("path '" ^ path ^ "' is not a valid directory"))
    | _
     -> raise (Failure "malformed input")
  in
  let lines = Utils.read_file file in
  try
    match lines with
    | Ok l
     -> Ok (List.map ~f:extract_target_and_path l)
    | Error error
     -> raise (Failure (Printf.sprintf "Error reading '%s': %s" file error))
  with Failure msg -> Error msg

let collect_all_stats_files () =
  let infer_out = Config.results_dir in
  let concatenate_paths p1 p2 = if Filename.is_relative p2 then Filename.concat p1 p2 else p2 in
  match Config.buck_out with
  | Some p
   -> if dir_exists p then
        let data =
          load_data_from_infer_deps (Filename.concat infer_out Config.buck_infer_deps_file_name)
        in
        match data with
        | Ok r
         -> let buck_out_parent = Filename.concat p Filename.parent_dir_name in
            let targets_files =
              List.map
                ~f:(fun (t, p) ->
                  (t, find_stats_files_in_dir (concatenate_paths buck_out_parent p)))
                r
            in
            Ok (Buck_out targets_files)
        | Error _ as e
         -> e
      else Error ("buck-out path '" ^ p ^ "' not found")
  | None
   -> Ok (Infer_out (find_stats_files_in_dir infer_out))

let aggregate_stats_files paths =
  let open_json_file file = Yojson.Basic.from_file file in
  let load_stats paths =
    List.map ~f:(fun path -> PerfStats.from_json (open_json_file path)) paths
  in
  let all_perf_stats = load_stats paths in
  match all_perf_stats with [] -> None | _ -> Some (PerfStats.aggregate all_perf_stats)

type json_aggregated_stats =
  { frontend_json_data: Yojson.Basic.json option
  ; backend_json_data: Yojson.Basic.json option
  ; reporting_json_data: Yojson.Basic.json option }

let aggregate_all_stats origin =
  let accumulate_paths acc paths =
    { frontend_paths= paths.frontend_paths @ acc.frontend_paths
    ; backend_paths= paths.backend_paths @ acc.backend_paths
    ; reporting_paths= paths.reporting_paths @ acc.reporting_paths }
  in
  let empty_stats_paths = {frontend_paths= []; backend_paths= []; reporting_paths= []} in
  let stats_paths =
    match origin with
    | Buck_out tf
     -> List.fold ~f:(fun acc (_, paths) -> accumulate_paths acc paths) ~init:empty_stats_paths tf
    | Infer_out paths
     -> paths
  in
  { frontend_json_data= aggregate_stats_files stats_paths.frontend_paths
  ; backend_json_data= aggregate_stats_files stats_paths.backend_paths
  ; reporting_json_data= aggregate_stats_files stats_paths.reporting_paths }

let aggregate_stats_by_target tp =
  let to_json f aggr_stats =
    let collect_valid_stats acc t p = match p with Some v -> (t, v) :: acc | None -> acc in
    let l = List.fold ~f:(fun acc (t, p) -> collect_valid_stats acc t (f p)) ~init:[] aggr_stats in
    match l with [] -> None | _ as v -> Some (`Assoc v)
  in
  let frontend_json_data = to_json (fun p -> aggregate_stats_files p.frontend_paths) tp in
  let backend_json_data = to_json (fun p -> aggregate_stats_files p.backend_paths) tp in
  let reporting_json_data = to_json (fun p -> aggregate_stats_files p.reporting_paths) tp in
  {frontend_json_data; backend_json_data; reporting_json_data}

let generate_files () =
  let infer_out = Config.results_dir in
  let stats_files = collect_all_stats_files () in
  let origin = match stats_files with Ok origin -> origin | Error e -> failwith e in
  let aggregated_frontend_stats_dir = Filename.concat infer_out Config.frontend_stats_dir_name in
  let aggregated_backend_stats_dir = Filename.concat infer_out Config.backend_stats_dir_name in
  let aggregated_reporting_stats_dir = Filename.concat infer_out Config.reporting_stats_dir_name in
  Utils.create_dir aggregated_frontend_stats_dir ;
  Utils.create_dir aggregated_backend_stats_dir ;
  Utils.create_dir aggregated_reporting_stats_dir ;
  let write_to_json_file_opt destfile json =
    match json with Some j -> Utils.write_json_to_file destfile j | None -> ()
  in
  ( match origin with
  | Buck_out tp
   -> let j = aggregate_stats_by_target tp in
      write_to_json_file_opt
        (Filename.concat aggregated_frontend_stats_dir aggregated_stats_by_target_filename)
        j.frontend_json_data ;
      write_to_json_file_opt
        (Filename.concat aggregated_backend_stats_dir aggregated_stats_by_target_filename)
        j.backend_json_data ;
      write_to_json_file_opt
        (Filename.concat aggregated_reporting_stats_dir aggregated_stats_by_target_filename)
        j.reporting_json_data
  | Infer_out _
   -> () ) ;
  let j = aggregate_all_stats origin in
  write_to_json_file_opt (Filename.concat aggregated_frontend_stats_dir aggregated_stats_filename)
    j.frontend_json_data ;
  write_to_json_file_opt (Filename.concat aggregated_backend_stats_dir aggregated_stats_filename)
    j.backend_json_data ;
  write_to_json_file_opt (Filename.concat aggregated_reporting_stats_dir aggregated_stats_filename)
    j.reporting_json_data
