(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

exception UnlockNotLocked of Procname.t

let log_lock_systime = BackendStats.add_to_proc_locker_lock_sys_time

let log_unlock_systime = BackendStats.add_to_proc_locker_unlock_sys_time

let record_sys_time_of ~f ~log_f =
  let start_time = Unix.times () in
  let result = f () in
  let end_time = Unix.times () in
  let sys_time_spent = end_time.tms_stime -. start_time.tms_stime in
  log_f sys_time_spent ; result


let locks_dir = Config.procnames_locks_dir

let setup () = Utils.rmtree locks_dir ; Utils.create_dir locks_dir

let clean () = ()

let filename_from pname = locks_dir ^/ Procname.to_filename pname

let unlock pname =
  record_sys_time_of ~log_f:log_unlock_systime ~f:(fun () ->
      try Unix.unlink (filename_from pname)
      with Unix.Unix_error (Unix.ENOENT, _, _) -> raise (UnlockNotLocked pname) )


let try_lock pname =
  record_sys_time_of ~log_f:log_lock_systime ~f:(fun () ->
      try
        Unix.openfile ~mode:[O_CREAT; O_EXCL; O_RDONLY] (filename_from pname) |> Unix.close ;
        true
      with Unix.Unix_error (Unix.EEXIST, _, _) -> false )
