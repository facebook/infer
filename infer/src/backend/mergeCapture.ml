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

(** Module to merge the results of capture for different buck targets. *)

let merge_global_tenvs infer_deps_file =
  let time0 = Mtime_clock.counter () in
  let global_tenv = Tenv.create () in
  let merge infer_out_src =
    let global_tenv_path =
      infer_out_src ^/ Config.global_tenv_filename |> DB.filename_from_string
    in
    Tenv.read global_tenv_path |> Option.iter ~f:(fun tenv -> Tenv.merge ~src:tenv ~dst:global_tenv)
  in
  Utils.iter_infer_deps ~project_root:Config.project_root ~f:merge infer_deps_file ;
  Tenv.store_global global_tenv ;
  L.progress "Merging type environments took %a@." Mtime.Span.pp (Mtime_clock.count time0)


let merge_json_results infer_out_src json_file_name =
  let main_changed_fs_file = Config.results_dir ^/ json_file_name in
  let changed_fs_file = infer_out_src ^/ json_file_name in
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


let merge_all_json_results merge_results results_json_str =
  L.progress "Merging %s files...@." results_json_str ;
  let infer_deps_file = Config.(results_dir ^/ buck_infer_deps_file_name) in
  Utils.iter_infer_deps ~project_root:Config.project_root ~f:merge_results infer_deps_file ;
  L.progress "Done merging %s files@." results_json_str


let merge_changed_functions () =
  let merge_changed_functions_json infer_out_src =
    merge_json_results infer_out_src Config.export_changed_functions_output
  in
  merge_all_json_results merge_changed_functions_json "changed functions"


let merge_captured_targets () =
  let time0 = Mtime_clock.counter () in
  L.progress "Merging captured Buck targets...@\n%!" ;
  let infer_deps_file = Config.(results_dir ^/ buck_infer_deps_file_name) in
  DBWriter.merge ~infer_deps_file ;
  if Config.genrule_master_mode then
    ScubaLogging.execute_with_time_logging "merge_captured_tenvs" (fun () ->
        merge_global_tenvs infer_deps_file ) ;
  let targets_num =
    let counter = ref 0 in
    let incr_counter _line = incr counter in
    Utils.with_file_in infer_deps_file ~f:(In_channel.iter_lines ~f:incr_counter) ;
    !counter
  in
  ScubaLogging.log_count ~label:"merged_captured_targets" ~value:targets_num ;
  L.progress "Merging %d captured Buck targets took %a@\n%!" targets_num Mtime.Span.pp
    (Mtime_clock.count time0)


(* shadowed for tracing *)
let merge_captured_targets () =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"merge buck targets" ())) ;
  merge_captured_targets () ;
  PerfEvent.(log (fun logger -> log_end_event logger ()))


let merge_captured_targets () =
  ScubaLogging.execute_with_time_logging "merge_captured_targets" merge_captured_targets
