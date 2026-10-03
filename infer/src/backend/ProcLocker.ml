(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

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


  let is_locked_by pid filename =
    match read_lock_value filename with
    | Some owner_pid ->
        Int.equal owner_pid (Pid.to_int pid)
    | None | (exception (Sys_error _ | Unix.Unix_error _ | End_of_file)) ->
        false


  (* the locks of [pid] are only released by [pid] itself, or by the scheduler once [pid] is idle,
     so they cannot go away between the check and the unlinking *)
  let unlock_if_locked_by pid filename = if is_locked_by pid filename then Unix.unlink filename

  let unlock_all_owned pid proc_filenames =
    List.iter proc_filenames ~f:(fun proc_filename ->
        unlock_if_locked_by pid (lock_of_filename proc_filename) )


  let is_locked_by_us pname = is_locked_by (WorkerPoolState.get_pid ()) (lock_of_procname pname)

  let unlock_all_locked_by_us ~except =
    let pid = WorkerPoolState.get_pid () in
    Stdlib.Sys.readdir locks_dir
    |> Array.iter ~f:(fun proc_filename ->
        if not (List.mem except proc_filename ~equal:String.equal) then
          unlock_if_locked_by pid (lock_of_filename proc_filename) )


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

module DomainLocks = struct
  module LockMap = IString.Hash

  let mutex = IMutex.create ()

  let lock_map = LockMap.create 1009

  let setup () = LockMap.clear lock_map

  let unsafe_unlock_key key =
    if LockMap.mem lock_map key then LockMap.remove lock_map key
    else L.die InternalError "Tried to unlock not-locked key: %s@\n" key


  (* the scheduler reserves procedures for jobs using the keys that the workers send it, which are
     [Procname.to_filename]s *)
  let key_of_procname = Procname.to_filename

  let unlock pname =
    let key = key_of_procname pname in
    IMutex.critical_section mutex ~f:(fun () -> unsafe_unlock_key key)


  let unlock_all proc_filenames =
    IMutex.critical_section mutex ~f:(fun () -> List.iter proc_filenames ~f:unsafe_unlock_key)


  let unsafe_try_lock_key domain_id key =
    match LockMap.find_opt lock_map key with
    | None ->
        LockMap.add lock_map key domain_id ;
        `LockAcquired
    | Some locker_id when Int.equal domain_id locker_id ->
        `AlreadyLockedByUs
    | Some _ ->
        `LockedByAnotherProcess


  let try_lock pname =
    let our_id = WorkerPoolState.get_in_child () |> Option.value_exn in
    let key = key_of_procname pname in
    IMutex.critical_section mutex ~f:(fun () -> unsafe_try_lock_key our_id key)


  let unsafe_is_locked_by domain_id key =
    LockMap.find_opt lock_map key |> Option.exists ~f:(Int.equal domain_id)


  let unlock_all_owned domain_id keys =
    IMutex.critical_section mutex ~f:(fun () ->
        List.iter keys ~f:(fun key ->
            if unsafe_is_locked_by domain_id key then LockMap.remove lock_map key ) )


  let is_locked_by_us pname =
    match WorkerPoolState.get_in_child () with
    | None ->
        (* eg in a whole-program analysis after the jobs; the main domain only locks procedures on
           behalf of the workers *)
        false
    | Some our_id ->
        let key = key_of_procname pname in
        IMutex.critical_section mutex ~f:(fun () -> unsafe_is_locked_by our_id key)


  let unlock_all_locked_by_us ~except =
    let our_id = WorkerPoolState.get_in_child () |> Option.value_exn in
    IMutex.critical_section mutex ~f:(fun () ->
        let to_unlock =
          LockMap.fold
            (fun key locker_id to_unlock ->
              if Int.equal our_id locker_id && not (List.mem except key ~equal:String.equal) then
                key :: to_unlock
              else to_unlock )
            lock_map []
        in
        List.iter to_unlock ~f:(LockMap.remove lock_map) )


  let lock_all domain_id keys =
    let lock_result =
      IMutex.critical_section mutex ~f:(fun () ->
          List.fold_result keys ~init:[] ~f:(fun locks key ->
              match unsafe_try_lock_key domain_id key with
              | `AlreadyLockedByUs ->
                  Ok locks
              | `LockAcquired ->
                  Ok (key :: locks)
              | `LockedByAnotherProcess ->
                  Error locks ) )
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


let setup () = if Config.multicore then DomainLocks.setup () else ProcessLocks.setup ()

let try_lock pname =
  record_time_of ~log_f:Stats.add_to_proc_locker_lock_time ~f:(fun () ->
      if Config.multicore then DomainLocks.try_lock pname else ProcessLocks.try_lock pname )


let unlock pname =
  record_time_of ~log_f:Stats.add_to_proc_locker_unlock_time ~f:(fun () ->
      if Config.multicore then DomainLocks.unlock pname else ProcessLocks.unlock pname )


let lock_all worker_id proc_filenames =
  match ((worker_id : WorkerPoolState.worker_id), Config.multicore) with
  | Pid pid, false ->
      ProcessLocks.lock_all pid proc_filenames
  | Domain domain_id, true ->
      DomainLocks.lock_all domain_id proc_filenames
  | _, _ ->
      Die.die InternalError "Tried to use incorrect worker type for current analysis mode.@\n"


let unlock_all proc_filenames =
  if Config.multicore then DomainLocks.unlock_all proc_filenames
  else ProcessLocks.unlock_all proc_filenames


let unlock_all_owned worker_id proc_filenames =
  match ((worker_id : WorkerPoolState.worker_id), Config.multicore) with
  | Pid pid, false ->
      ProcessLocks.unlock_all_owned pid proc_filenames
  | Domain domain_id, true ->
      DomainLocks.unlock_all_owned domain_id proc_filenames
  | _, _ ->
      Die.die InternalError "Tried to use incorrect worker type for current analysis mode.@\n"


let is_locked_by_us pname =
  if Config.multicore then DomainLocks.is_locked_by_us pname else ProcessLocks.is_locked_by_us pname


let unlock_all_locked_by_us ~except =
  if Config.multicore then DomainLocks.unlock_all_locked_by_us ~except
  else ProcessLocks.unlock_all_locked_by_us ~except
