(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let dbwriter_command_mutex = IMutex.create ()

let perform cmd =
  match (cmd : DBWriterCommand.t) with
  | Terminate ->
      L.debug Analysis Quiet "Sqlite write daemon: terminating@." ;
      ExecutionDuration.log ~prefix:"dbwriter.store_sql" Analysis !DBWriterCommand.store_sql_time ;
      IMutex.critical_section dbwriter_command_mutex ~f:(fun () -> DBWriterCommand.perform cmd)
  | _ ->
      IMutex.critical_section dbwriter_command_mutex ~f:(fun () -> DBWriterCommand.perform cmd)
