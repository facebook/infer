(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let should_use_daemon = ref Config.dbwriter

let use_daemon () = !should_use_daemon

let override_use_daemon f = should_use_daemon := f

module LockfMutex = struct
  let lock_path = ResultsDirEntryName.get_path ~results_dir:Config.results_dir DBLock

  let lock_file_fd = ref None

  let start () =
    lock_file_fd :=
      Some (Unix.openfile lock_path ~mode:[O_KEEPEXEC; O_CREAT; O_RDWR; O_TRUNC] ~perm:0o644)


  let stop () =
    let fd = Option.value_exn !lock_file_fd in
    Unix.close fd ;
    Unix.unlink lock_path ;
    lock_file_fd := None


  let lock () =
    let fd = Option.value_exn !lock_file_fd in
    Unix.lockf fd ~mode:F_LOCK ~len:0


  let unlock () =
    let fd = Option.value_exn !lock_file_fd in
    Unix.lockf fd ~mode:F_ULOCK ~len:0


  let critical_section ~f =
    lock () ;
    Exn.protect ~f ~finally:unlock
end

let perform cmd =
  match (cmd : DBWriterCommand.t) with
  | Start ->
      LockfMutex.start ()
  | Terminate ->
      L.debug Analysis Quiet "Deleting dbwriter lockfile@." ;
      DBWriterCommand.perform cmd ;
      LockfMutex.stop ()
  | _ ->
      LockfMutex.critical_section ~f:(fun () -> DBWriterCommand.perform cmd)
