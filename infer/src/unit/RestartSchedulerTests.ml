(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2

let a_pname = Procname.from_string_c_fun "a_c_fun_name"

(* Tests are organized like this instead of using one function per test because
   OUnit run tests in parallel and since all tests use the same output directory
   (inter-out-unit) the file locks would collide because they all live in a
   directory called procnames_locks inside the output dir. *)
let tests_wrapper _test_ctxt =
  ProcLocker.(
    setup () ;
    (* When tries to lock a Procname that was already locked it fails *)
    try_lock a_pname |> ignore ;
    assert_equal ~msg:"Should not be able to lock a Procname that's already locked."
      (try_lock a_pname) `AlreadyLockedByUs ;
    unlock a_pname ;
    (* When successives locks/unlocks are performed in the right order they succeed *)
    try_lock a_pname |> ignore ;
    unlock a_pname ;
    try_lock a_pname |> ignore ;
    unlock a_pname ;
    (* When an unlock is performed over a non-locked Procname it fails *)
    try_lock a_pname |> ignore ;
    unlock a_pname ;
    try
      unlock a_pname ;
      assert_failure "Should have raised an exception."
    with Die.InferInternalError _ -> () ) ;
  let worker n = WorkerPoolState.Pid (Pid.of_int n) in
  let is_lockable_by n proc_filename =
    match ProcLocker.lock_all (worker n) [proc_filename] with
    | `LocksAcquired locks ->
        ProcLocker.unlock_all locks ;
        true
    | `FailedToLockAll ->
        false
  in
  let job proc_name = TaskSchedulerTypes.Procname {proc_name; specialization= None} in
  let assert_next msg scheduler n proc_name =
    assert_equal ~msg
      ~cmp:(Option.equal TaskSchedulerTypes.equal_target)
      (Some (job proc_name))
      (scheduler.TaskGenerator.next
         {TaskGenerator.child_slot= n; child_id= worker (100 + n); is_first_update= false} )
  in
  let race_on scheduler proc_name dependency_filenames =
    scheduler.TaskGenerator.finished
      ~result:(Some (TaskSchedulerTypes.RaceOn {dependency_filenames}))
      (job proc_name)
  in
  let finish scheduler proc_name =
    scheduler.TaskGenerator.finished ~result:(Some TaskSchedulerTypes.Ok) (job proc_name)
  in
  let proc1 = Procname.from_string_c_fun "job1" in
  let proc2 = Procname.from_string_c_fun "job2" in
  (* Two jobs restarted because they needed a callee that was being analyzed. Once the callee is not
     locked anymore, both jobs can be retried at the same time. *)
  let scheduler = RestartScheduler.of_queue (Queue.of_list [job proc1; job proc2]) in
  assert_next "job1 is scheduled" scheduler 1 proc1 ;
  assert_next "job2 is scheduled" scheduler 2 proc2 ;
  race_on scheduler proc1 ["callee"; Procname.to_filename proc1] ;
  race_on scheduler proc2 ["callee"; Procname.to_filename proc2] ;
  assert_next "job1 is retried" scheduler 3 proc1 ;
  assert_next "job2 is retried while job1 is running" scheduler 4 proc2 ;
  (* The worker running job1 released its reservation of job1 after analyzing it, and another worker
     has locked it since. *)
  ProcLocker.unlock_all [Procname.to_filename proc1] ;
  ProcLocker.lock_all (worker 5) [Procname.to_filename proc1] |> ignore ;
  finish scheduler proc1 ;
  assert_bool "Finishing a job released a lock of another worker."
    (not (is_lockable_by 6 (Procname.to_filename proc1))) ;
  ProcLocker.unlock_all [Procname.to_filename proc1] ;
  finish scheduler proc2 ;
  assert_bool "All jobs are done." (scheduler.TaskGenerator.is_empty ()) ;
  (* job2 restarted in the middle of analyzing the callee, so the callee has no summary yet: it stays
     reserved for job1, which needs it. *)
  let scheduler = RestartScheduler.of_queue (Queue.of_list [job proc1; job proc2]) in
  assert_next "job1 is scheduled" scheduler 1 proc1 ;
  assert_next "job2 is scheduled" scheduler 2 proc2 ;
  ProcLocker.lock_all (worker 5) ["elsewhere"] |> ignore ;
  race_on scheduler proc1 ["callee"; Procname.to_filename proc1] ;
  race_on scheduler proc2 ["elsewhere"; Procname.to_filename proc2; "callee"] ;
  assert_next "job1 is retried" scheduler 3 proc1 ;
  assert_bool "Callee being analyzed by a blocked job not reserved."
    (not (is_lockable_by 6 "callee")) ;
  finish scheduler proc1 ;
  ProcLocker.unlock_all ["elsewhere"] ;
  assert_next "job2 is retried" scheduler 4 proc2 ;
  finish scheduler proc2 ;
  assert_bool "All jobs are done." (scheduler.TaskGenerator.is_empty ()) ;
  (* The scheduler reserved procedures for the current job when retrying it. A reserved procedure is
     released as soon as the job has analyzed it, but not if its analysis is interrupted. *)
  let reserve procs =
    ProcLocker.lock_all
      (WorkerPoolState.Pid (WorkerPoolState.get_pid ()))
      (List.map procs ~f:Procname.to_filename)
    |> ignore
  in
  let is_reserved = ProcLocker.is_locked_by_us in
  let with_lock = RestartScheduler.with_lock ~get_actives:(fun () -> []) in
  let p = Procname.from_string_c_fun "p" in
  let q = Procname.from_string_c_fun "q" in
  let r = Procname.from_string_c_fun "r" in
  reserve [p] ;
  with_lock p ~f:(fun () ->
      with_lock p ~f:(fun () -> ()) ;
      assert_bool "Reservation released by a nested analysis." (is_reserved p) ) ;
  assert_bool "Reservation kept after the analysis." (not (is_reserved p)) ;
  reserve [p] ;
  let exception Interrupted in
  (try with_lock p ~f:(fun () -> raise Interrupted) with Interrupted -> ()) ;
  assert_bool "Reservation released after an interrupted analysis." (is_reserved p) ;
  (* Once the job finds that another worker has analyzed one of its reserved procedures, it releases
     its reservations except for the procedures it is analyzing. *)
  reserve [q; r] ;
  with_lock p ~f:(fun () ->
      RestartScheduler.release_reservations q ;
      assert_bool "Reservations kept after finding an analyzed reserved procedure."
        (not (is_reserved q || is_reserved r)) ;
      assert_bool "Reservation of a procedure being analyzed released." (is_reserved p) ) ;
  assert_bool "Reservation kept after the analysis." (not (is_reserved p)) ;
  (* Outside of any procedure, eg at the start of a File job, the job also releases its reservations
     as soon as it finds one of them analyzed. *)
  RestartScheduler.forget_reservations () ;
  reserve [q; r] ;
  RestartScheduler.release_reservations q ;
  assert_bool "Reservations kept after finding an analyzed reserved procedure at the top level."
    (not (is_reserved q || is_reserved r)) ;
  (* Inside a procedure, a job only looks for reservations to release once it has entered a reserved
     procedure, and it forgets that at the end of the job. *)
  reserve [p] ;
  with_lock p ~f:(fun () -> ()) ;
  RestartScheduler.forget_reservations () ;
  reserve [q] ;
  with_lock r ~f:(fun () -> RestartScheduler.release_reservations q) ;
  assert_bool "Reservations looked up by a job that has not entered a reserved procedure."
    (is_reserved q) ;
  ProcLocker.unlock_all [Procname.to_filename q] ;
  (* With --multicore, the workers lock procedures under the names the scheduler reserves them by. *)
  let module DomainLocks = ProcLocker.DomainLocks in
  let as_domain id ~f =
    WorkerPoolState.set_in_child (Some id) ;
    f ()
  in
  DomainLocks.setup () ;
  DomainLocks.lock_all 1 [Procname.to_filename p; Procname.to_filename q] |> ignore ;
  as_domain 1 ~f:(fun () ->
      assert_equal ~msg:"Reserved procedure not locked by its domain." (DomainLocks.try_lock p)
        `AlreadyLockedByUs ) ;
  as_domain 2 ~f:(fun () ->
      assert_equal ~msg:"Domain locked a procedure reserved for another domain."
        (DomainLocks.try_lock p) `LockedByAnotherProcess ;
      DomainLocks.unlock_all_owned 2 [Procname.to_filename p] ;
      DomainLocks.unlock_all_locked_by_us ~except:[] ) ;
  as_domain 1 ~f:(fun () ->
      assert_bool "Domain released a lock of another domain." (DomainLocks.is_locked_by_us p) ;
      DomainLocks.unlock_all_locked_by_us ~except:[Procname.to_filename p] ;
      assert_bool "Lock of the domain not released." (not (DomainLocks.is_locked_by_us q)) ;
      assert_bool "Lock released despite [except]." (DomainLocks.is_locked_by_us p) ) ;
  DomainLocks.unlock_all_owned 1 [Procname.to_filename p] ;
  as_domain 2 ~f:(fun () ->
      assert_equal ~msg:"Lock not released by its owner." (DomainLocks.try_lock p) `LockAcquired ) ;
  WorkerPoolState.set_in_child None ;
  assert_bool "Lock owned outside of the workers." (not (DomainLocks.is_locked_by_us p))


let tests = "restart_scheduler_suite" >:: tests_wrapper
