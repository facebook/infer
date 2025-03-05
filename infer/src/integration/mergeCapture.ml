(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** Module to merge the results of capture for different buck/gradle targets. *)

module TenvMerger = struct
  let merge paths =
    let output =
      if Config.(continue_capture || reactive_capture) then (
        match Tenv.Global.read () with
        | None ->
            L.progress "Merge found no pre-existing global type environment@\n" ;
            Tenv.create ()
        | Some tenv ->
            L.progress "Merge found pre-existing global type environment with %d entries@\n"
              (Tenv.length tenv) ;
            tenv )
      else Tenv.create ()
    in
    let do_merge path =
      Tenv.read path |> Option.iter ~f:(fun tenv -> Tenv.merge ~src:tenv ~dst:output)
    in
    List.iter paths ~f:do_merge ;
    output


  let merge_into_global ~normalize paths =
    let time0 = Mtime_clock.counter () in
    let global_tenv = merge paths in
    let time1 = Mtime_clock.counter () in
    Tenv.Global.store ~normalize global_tenv ;
    L.progress "Merging type environments took %a, of which %a were spent storing the global tenv@."
      Mtime.Span.pp (Mtime_clock.count time0) Mtime.Span.pp (Mtime_clock.count time1)


  let merge_global_tenvs ~normalize infer_deps_file =
    let dep_tenv_path infer_out_src =
      ResultsDirEntryName.get_path ~results_dir:infer_out_src GlobalTypeEnvironment
      |> DB.filename_from_string
    in
    let fold_dep_paths =
      Utils.fold_infer_deps ~root:Config.project_root |> IContainer.map ~f:dep_tenv_path
    in
    let paths = Container.to_list ~fold:fold_dep_paths infer_deps_file in
    merge_into_global ~normalize paths


  let merge_global_tenvs ~normalize infer_deps_file =
    StatsLogging.execute_with_time_logging "merge_captured_tenvs" (fun () ->
        GCStats.log_f ~name:"tenv_merge" MergeCapture (fun () ->
            merge_global_tenvs ~normalize infer_deps_file ) )


  let start infer_deps_file =
    if Config.multicore then
      `DomainWorker
        (Domain.spawn (fun () ->
             Database.new_database_connections Primary ;
             merge_global_tenvs ~normalize:true infer_deps_file ) )
    else
      match IUnix.fork () with
      | `In_the_child ->
          ForkUtils.protect ~f:(merge_global_tenvs ~normalize:true) infer_deps_file ;
          L.exit 0
      | `In_the_parent child_pid ->
          `ForkWorker child_pid


  let wait = function
    | `DomainWorker domain ->
        Domain.join domain
    | `ForkWorker child_pid -> (
      match IUnix.waitpid child_pid with
      | Error _ as err ->
          L.die InternalError "Worker terminated abnormally: %s.@\n"
            (IUnix.Exit_or_signal.to_string_hum err)
      | Ok () ->
          Tenv.Global.force_load () |> ignore )
end

let merge_global_tenv = TenvMerger.merge_into_global

let merge_captured_targets ~root =
  let time0 = Mtime_clock.counter () in
  L.progress "Merging captured targets...@\n%!" ;
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
  StatsLogging.log_count ~label:"merged_captured_targets" ~value:targets_num ;
  L.progress "Merging %d captured targets took %a@\n%!" targets_num Mtime.Span.pp
    (Mtime_clock.count time0)


(* shadowed for tracing *)
let merge_captured_targets ~root =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"merge targets" ())) ;
  merge_captured_targets ~root ;
  PerfEvent.(log (fun logger -> log_end_event logger ()))


let merge_captured_targets ~root =
  StatsLogging.execute_with_time_logging "merge_captured_targets" (fun () ->
      merge_captured_targets ~root )
