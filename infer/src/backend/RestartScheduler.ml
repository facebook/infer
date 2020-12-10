(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

type work_with_dependency = {work: TaskSchedulerTypes.target; dependency_filename_opt: string option}

let of_queue content : ('a, string) ProcessPool.TaskGenerator.t =
  let remaining = ref (Queue.length content) in
  let remaining_tasks () = !remaining in
  let is_empty () = Int.equal !remaining 0 in
  let finished ~result work =
    match result with
    | None ->
        decr remaining
    | Some _ as dependency_filename_opt ->
        Queue.enqueue content {work; dependency_filename_opt}
  in
  let work_if_dependency_allows w =
    match w.dependency_filename_opt with
    | Some dependency_filename when ProcLocker.is_locked ~proc_filename:dependency_filename ->
        Queue.enqueue content w ;
        None
    | None | Some _ ->
        Some w.work
  in
  let next () = Option.bind (Queue.dequeue content) ~f:(fun w -> work_if_dependency_allows w) in
  {remaining_tasks; is_empty; finished; next}


let make sources =
  let target_count = ref 0 in
  let cons_proc_uid_work acc procname =
    incr target_count ;
    let proc_uid = Procname.to_unique_id procname in
    {work= TaskSchedulerTypes.ProcUID proc_uid; dependency_filename_opt= None} :: acc
  in
  let pname_targets =
    List.fold sources ~init:[] ~f:(fun init source ->
        SourceFiles.proc_names_of_source source |> List.fold ~init ~f:cons_proc_uid_work )
  in
  let make_file_work file =
    incr target_count ;
    {work= TaskSchedulerTypes.File file; dependency_filename_opt= None}
  in
  let file_targets = List.rev_map sources ~f:make_file_work in
  let queue = Queue.create ~capacity:!target_count () in
  let permute_and_enqueue targets =
    List.permute targets ~random_state:(Random.State.make (Array.create ~len:1 0))
    |> List.iter ~f:(Queue.enqueue queue)
  in
  permute_and_enqueue pname_targets ;
  permute_and_enqueue file_targets ;
  of_queue queue


let if_restart_scheduler f =
  if Int.equal Config.jobs 1 then ()
  else match Config.scheduler with File | SyntacticCallGraph -> () | Restart -> f ()


type locked_proc =
  {pname: Procname.t; start: ExecutionDuration.counter; mutable callees_useful: ExecutionDuration.t}

let locked_procs = Stack.create ()

let record_locked_proc (pname : Procname.t) =
  Stack.push locked_procs
    {pname; start= ExecutionDuration.counter (); callees_useful= ExecutionDuration.zero}


let add_to_useful_time from =
  BackendStats.add_to_restart_scheduler_useful_time (ExecutionDuration.since from)


let add_to_useful_exe_duration exe_duration =
  BackendStats.add_to_restart_scheduler_useful_time exe_duration


let add_to_total_time from =
  BackendStats.add_to_restart_scheduler_total_time (ExecutionDuration.since from)


let unlock_all () =
  Stack.until_empty locked_procs (fun {pname; start; callees_useful} ->
      ( match Stack.top locked_procs with
      | Some caller ->
          caller.callees_useful <- ExecutionDuration.add caller.callees_useful callees_useful
      | None ->
          add_to_useful_exe_duration callees_useful ;
          add_to_total_time start ) ;
      ProcLocker.unlock pname )


let lock_exn pname =
  if_restart_scheduler (fun () ->
      if ProcLocker.try_lock pname then record_locked_proc pname
      else (
        unlock_all () ;
        raise
          (TaskSchedulerTypes.ProcnameAlreadyLocked {dependency_filename= Procname.to_filename pname})
        ) )


let unlock pname =
  if_restart_scheduler (fun () ->
      match Stack.pop locked_procs with
      | None ->
          L.die InternalError "Trying to unlock %s but it does not appear to be locked.@."
            (Procname.to_string pname)
      | Some {pname= stack_pname} when not (Procname.equal pname stack_pname) ->
          L.die InternalError "Trying to unlock %s but top of stack is %s.@."
            (Procname.to_string pname) (Procname.to_string stack_pname)
      | Some {start} ->
          ( match Stack.top locked_procs with
          | Some caller ->
              caller.callees_useful <-
                ExecutionDuration.add_duration_since caller.callees_useful start
          | None ->
              add_to_useful_time start ;
              add_to_total_time start ) ;
          ProcLocker.unlock pname )


let setup () = if_restart_scheduler ProcLocker.setup
