(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** Module to merge the results of capture for different buck targets. *)

module TenvMerger = struct
  let merge_global_tenvs infer_deps_file =
    let time0 = Mtime_clock.counter () in
    let global_tenv = Tenv.create () in
    let merge infer_out_src =
      let global_tenv_path =
        ResultsDirEntryName.get_path ~results_dir:infer_out_src JavaGlobalTypeEnvironment
        |> DB.filename_from_string
      in
      Tenv.read global_tenv_path
      |> Option.iter ~f:(fun tenv -> Tenv.merge ~src:tenv ~dst:global_tenv)
    in
    Utils.iter_infer_deps ~root:Config.project_root ~f:merge infer_deps_file ;
    let time1 = Mtime_clock.counter () in
    Tenv.store_global global_tenv ;
    L.progress "Merging type environments took %a, of which %a were spent storing the global tenv@."
      Mtime.Span.pp (Mtime_clock.count time0) Mtime.Span.pp (Mtime_clock.count time1)


  let merge_global_tenvs infer_deps_file =
    ScubaLogging.execute_with_time_logging "merge_captured_tenvs" (fun () ->
        GCStats.log_f ~name:"tenv_merge" MergeCapture (fun () -> merge_global_tenvs infer_deps_file) )


  let start infer_deps_file =
    match Unix.fork () with
    | `In_the_child ->
        ForkUtils.protect ~f:merge_global_tenvs infer_deps_file ;
        L.exit 0
    | `In_the_parent child_pid ->
        child_pid


  let wait child_pid =
    match Unix.waitpid child_pid with
    | Error _ as err ->
        L.die InternalError "Worker terminated abnormally: %s.@\n"
          (Unix.Exit_or_signal.to_string_hum err)
    | Ok () ->
        ()
end

let merge_changed_functions () =
  L.progress "Merging changed functions files...@." ;
  let tgt_dir = ResultsDir.get_path ChangedFunctionsTempResults in
  let infer_deps_file = ResultsDir.get_path CaptureDependencies in
  Utils.iter_infer_deps infer_deps_file ~root:Config.project_root ~f:(fun infer_out_src ->
      let src_dir =
        ResultsDirEntryName.get_path ~results_dir:infer_out_src ChangedFunctionsTempResults
      in
      match Sys.is_directory src_dir with
      | `Yes ->
          Utils.create_dir tgt_dir ;
          Utils.directory_iter
            (fun src ->
              let data = In_channel.read_all src in
              Out_channel.write_all (tgt_dir ^/ Filename.basename src) ~data )
            src_dir
      | `No | `Unknown ->
          () ) ;
  L.progress "Done merging changed functions files@."


let merge_captured_targets ~root =
  let time0 = Mtime_clock.counter () in
  L.progress "Merging captured Buck targets...@\n%!" ;
  let infer_deps_file = ResultsDir.get_path CaptureDependencies in
  let tenv_merger_child = TenvMerger.start infer_deps_file in
  DBWriter.merge_captures ~root ~infer_deps_file ;
  TenvMerger.wait tenv_merger_child ;
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
let merge_captured_targets ~root =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"merge buck targets" ())) ;
  merge_captured_targets ~root ;
  PerfEvent.(log (fun logger -> log_end_event logger ()))


let merge_captured_targets ~root =
  ScubaLogging.execute_with_time_logging "merge_captured_targets" (fun () ->
      merge_captured_targets ~root )
