(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module YB = Yojson.Basic
module YBU = Yojson.Basic.Util

let merge_changed_functions_json ~infer_out_src =
  let main_changed_fs_file = Config.results_dir ^/ Config.export_changed_functions_output in
  let changed_fs_file = infer_out_src ^/ Config.export_changed_functions_output in
  let main_json = try YB.from_file main_changed_fs_file |> YBU.to_list with Sys_error _ -> [] in
  let changed_json = try YB.from_file changed_fs_file |> YBU.to_list with Sys_error _ -> [] in
  let all_fs =
    `List
      (List.dedup_and_sort
         ~compare:(fun s1 s2 ->
           match (s1, s2) with `String s1, `String s2 -> String.compare s1 s2 | _ -> 0 )
         (List.append main_json changed_json))
  in
  YB.to_file main_changed_fs_file all_fs


let iter_infer_deps infer_deps_file ~f =
  let one_line line =
    match String.split ~on:'\t' line with
    | [_; _; target_results_dir] ->
        let infer_out_src =
          if Filename.is_relative target_results_dir then
            Filename.dirname (Config.project_root ^/ "buck-out") ^/ target_results_dir
          else target_results_dir
        in
        f ~infer_out_src
    | _ ->
        assert false
  in
  match Utils.read_file infer_deps_file with
  | Ok lines ->
      List.iter ~f:one_line lines
  | Error error ->
      L.internal_error "Couldn't read deps file '%s': %s" infer_deps_file error


let merge_buck_flavors_results infer_deps_file =
  iter_infer_deps infer_deps_file ~f:DBWriter.merge_dbs


let merge_buck_changed_functions infer_deps_file =
  iter_infer_deps infer_deps_file ~f:merge_changed_functions_json
