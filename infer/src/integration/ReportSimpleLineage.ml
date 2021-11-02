(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let report {Summary.payloads= {simple_lineage}; proc_desc} =
  match simple_lineage with
  | None ->
      let procname = Procdesc.get_proc_name proc_desc in
      L.user_warning "No summary for %a@\n" Procname.pp procname
  | Some summary ->
      SimpleLineage.Summary.report summary proc_desc


let worker source_file =
  let t0 = Mtime_clock.now () in
  let status = Format.asprintf "%a" SourceFile.pp source_file in
  !ProcessPoolState.update_status t0 status ;
  let proc_names = SourceFiles.proc_names_of_source source_file in
  List.iter
    ~f:(fun proc_name ->
      let summary = Summary.OnDisk.get proc_name in
      Option.iter summary ~f:report )
    proc_names ;
  None


let report () =
  Tasks.Runner.create ~jobs:Config.jobs
    ~child_prologue:(fun () -> ())
    ~f:worker
    ~child_epilogue:(fun () -> ())
    ~tasks:(fun () ->
      ProcessPool.TaskGenerator.of_list (SourceFiles.get_all ~filter:(fun _ -> true) ()) )
  |> Tasks.Runner.run |> ignore
