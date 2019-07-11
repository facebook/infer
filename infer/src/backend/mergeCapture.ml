(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** Module to merge the results of capture for different buck targets. *)

let merge_global_tenvs infer_deps_file =
  let time0 = Mtime_clock.counter () in
  let global_tenv = Tenv.create () in
  let merge infer_out_src =
    let global_tenv_path =
      infer_out_src ^/ Config.global_tenv_filename |> DB.filename_from_string
    in
    Tenv.read global_tenv_path
    |> Option.iter ~f:(fun tenv -> Tenv.merge ~src:tenv ~dst:global_tenv)
  in
  MergeResults.iter_infer_deps infer_deps_file ~f:merge ;
  Tenv.store_global global_tenv ;
  L.progress "Merging type environments took %a@." Mtime.Span.pp (Mtime_clock.count time0)


let merge_changed_functions () =
  L.progress "Merging changed functions files...@." ;
  let infer_deps_file = Config.(results_dir ^/ buck_infer_deps_file_name) in
  MergeResults.merge_buck_changed_functions infer_deps_file ;
  L.progress "Done merging changed functions files@."


let merge_captured_targets () =
  let time0 = Mtime_clock.counter () in
  L.progress "Merging captured Buck targets...@\n%!" ;
  let infer_deps_file = Config.(results_dir ^/ buck_infer_deps_file_name) in
  MergeResults.merge_buck_flavors_results infer_deps_file ;
  if Config.genrule_master_mode then merge_global_tenvs infer_deps_file ;
  L.progress "Merging captured Buck targets took %a@\n%!" Mtime.Span.pp (Mtime_clock.count time0)


(* shadowed for tracing *)
let merge_captured_targets () =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"merge buck targets" ())) ;
  merge_captured_targets () ;
  PerfEvent.(log (fun logger -> log_end_event logger ()))
