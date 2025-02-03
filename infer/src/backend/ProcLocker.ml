(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type worker_id = Pid of Pid.t | Domain of int

module ProcessLocks = struct
  let locks_dir = ResultsDir.get_path ProcnamesLocks

  let setup () =
    Utils.rmtree locks_dir ;
    Utils.create_dir locks_dir ;
    ()


  let lock_of_filename filename = locks_dir ^/ filename

  let lock_of_procname pname = lock_of_filename (Procname.to_filename pname)

  let unlock pname =
    try Unix.unlink (lock_of_procname pname)
    with Unix.Unix_error (ENOENT, _, _) ->
      L.die InternalError "Tried to unlock not-locked pname: %a@\n" Procname.pp pname


  let try_taking_lock pid filename =
    (* Writing the contents is not atomic but at least we prevent two processes from writing at the
       same time. This causes complications when reading out the contents as one could read an empty
       file. We assume it's not possible to read a partially-written PID given the small size. YOLO. *)
    Utils.with_file_out ~fail_if_exists:true filename ~f:(fun out_c ->
        Out_channel.output_binary_int out_c (Pid.to_int pid) )


  let read_lock_value filename = Utils.with_file_in filename ~f:In_channel.input_binary_int

  let rec do_try_lock pid filename =
    try
      try_taking_lock pid filename ;
      (* the lock file did not exist and we were the first to create it, i.e. lock it *)
      `LockAcquired
    with Unix.Unix_error ((EEXIST | EACCES), _, _) | Sys_error _ -> (
      (* the lock file already existed; now to figure which process locked it in case it was us *)
      match read_lock_value filename with
      | Some owner_pid ->
          if Int.equal owner_pid (Pid.to_int pid) then `AlreadyLockedByUs
          else `LockedByAnotherProcess
      | None | (exception (Sys_error _ | Unix.Unix_error ((ENOENT | EINVAL), _, _) | End_of_file))
        ->
          (* the lock file went away, opportunistically try taking the lock for ourselves again *)
          do_try_lock pid filename )


  let try_lock pname =
    let filename = lock_of_procname pname in
    do_try_lock (WorkerPoolState.get_pid ()) filename


  let unlock_all proc_filenames =
    List.iter proc_filenames ~f:(fun proc_filename -> Unix.unlink (lock_of_filename proc_filename))


  let lock_all pid proc_filenames =
    let lock_result =
      List.fold_result proc_filenames ~init:[] ~f:(fun locks proc_filename ->
          match do_try_lock pid (lock_of_filename proc_filename) with
          | `AlreadyLockedByUs ->
              Ok locks
          | `LockAcquired ->
              Ok (proc_filename :: locks)
          | `LockedByAnotherProcess ->
              Error locks )
    in
    match lock_result with
    | Ok locks ->
        `LocksAcquired locks
    | Error locks ->
        unlock_all locks ;
        `FailedToLockAll
end

let record_time_of ~f ~log_f =
  let ExecutionDuration.{result; execution_duration} = ExecutionDuration.timed_evaluate ~f in
  log_f execution_duration ;
  result


let setup () = if Config.multicore then assert false else ProcessLocks.setup ()

let try_lock pname =
  record_time_of ~log_f:Stats.add_to_proc_locker_lock_time ~f:(fun () ->
      if Config.multicore then assert false else ProcessLocks.try_lock pname )


let unlock pname =
  record_time_of ~log_f:Stats.add_to_proc_locker_unlock_time ~f:(fun () ->
      if Config.multicore then assert false else ProcessLocks.unlock pname )


let lock_all worker_id proc_filenames =
  match (worker_id, Config.multicore) with
  | Pid pid, false ->
      ProcessLocks.lock_all pid proc_filenames
  | Domain _, true ->
      assert false
  | _, _ ->
      Die.die InternalError "Tried to use incorrect worker type for current analysis mode.@\n"


let unlock_all proc_filenames =
  if Config.multicore then assert false else ProcessLocks.unlock_all proc_filenames
