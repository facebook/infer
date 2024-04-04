(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let report {Summary.payloads= {lineage}; proc_name} =
  match Lazy.force lineage with
  | None ->
      L.user_warning "No summary for %a@\n" Procname.pp proc_name
  | Some lineage_summary ->
      Procdesc.load_exn proc_name |> Lineage.Summary.report lineage_summary


let worker source_file =
  let t0 = Mtime_clock.now () in
  let status = Format.asprintf "%a" SourceFile.pp source_file in
  !ProcessPoolState.update_status t0 status ;
  let proc_names = SourceFiles.proc_names_of_source source_file in
  List.iter
    ~f:(fun proc_name ->
      let summary = Summary.OnDisk.get ~lazy_payloads:true proc_name in
      Option.iter summary ~f:report )
    proc_names ;
  None


let report () =
  let tasks () =
    ProcessPool.TaskGenerator.of_list (SourceFiles.get_all ~filter:(fun _ -> true) ())
  in
  Tasks.Runner.create ~jobs:Config.jobs ~child_prologue:ignore ~f:worker ~child_epilogue:ignore
    tasks
  |> Tasks.Runner.run |> ignore
