(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

type work_with_dependency = {work: TaskSchedulerTypes.target; need_to_finish: Procname.t option}

let of_list (lst : work_with_dependency list) : ('a, Procname.t) ProcessPool.TaskGenerator.t =
  let content = Queue.of_list lst in
  let remaining = ref (Queue.length content) in
  let remaining_tasks () = !remaining in
  let is_empty () = Int.equal !remaining 0 in
  let finished ~result work =
    match result with
    | None ->
        decr remaining
    | Some _ as need_to_finish ->
        Queue.enqueue content {work; need_to_finish}
  in
  let work_if_dependency_allows w =
    match w.need_to_finish with
    | Some pname when ProcLocker.is_locked pname ->
        Queue.enqueue content w ;
        None
    | None | Some _ ->
        Some w.work
  in
  let next () = Option.bind (Queue.dequeue content) ~f:(fun w -> work_if_dependency_allows w) in
  {remaining_tasks; is_empty; finished; next}


let make sources =
  let pnames =
    List.map sources ~f:SourceFiles.proc_names_of_source
    |> List.concat
    |> List.rev_map ~f:(fun procname ->
           {work= TaskSchedulerTypes.Procname procname; need_to_finish= None} )
  in
  let files =
    List.map sources ~f:(fun file -> {work= TaskSchedulerTypes.File file; need_to_finish= None})
  in
  let permute = List.permute ~random_state:(Random.State.make (Array.create ~len:1 0)) in
  permute pnames @ permute files |> of_list


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
        raise (TaskSchedulerTypes.ProcnameAlreadyLocked pname) ) )


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

let clean () = if_restart_scheduler ProcLocker.clean
