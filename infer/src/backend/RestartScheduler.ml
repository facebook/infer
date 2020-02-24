(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

exception ProcnameAlreadyLocked of Procname.t

type work_with_dependency = {work: SchedulerTypes.target; need_to_finish: Procname.t option}

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
        Queue.enqueue content w ; None
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
           {work= SchedulerTypes.Procname procname; need_to_finish= None} )
  in
  let files =
    List.map sources ~f:(fun file -> {work= SchedulerTypes.File file; need_to_finish= None})
  in
  let permute = List.permute ~random_state:(Random.State.make (Array.create ~len:1 0)) in
  permute pnames @ permute files |> of_list


let if_restart_scheduler f =
  if Int.equal Config.jobs 1 then ()
  else match Config.scheduler with File | SyntacticCallGraph -> () | Restart -> f ()


let locked_procs = Stack.create ()

let record_locked_proc (pname : Procname.t) = Stack.push locked_procs pname

let unlock_all () = Stack.until_empty locked_procs ProcLocker.unlock

let lock_exn pname =
  if_restart_scheduler (fun () ->
      if ProcLocker.try_lock pname then record_locked_proc pname
      else (
        unlock_all () ;
        raise (ProcnameAlreadyLocked pname) ) )


let unlock pname =
  if_restart_scheduler (fun () ->
      match Stack.pop locked_procs with
      | None ->
          L.die InternalError "Trying to unlock %s but it does not appear to be locked.@."
            (Procname.to_string pname)
      | Some stack_pname when not (Procname.equal pname stack_pname) ->
          L.die InternalError "Trying to unlock %s but top of stack is %s.@."
            (Procname.to_string pname) (Procname.to_string stack_pname)
      | Some _ ->
          ProcLocker.unlock pname )


let setup () = if_restart_scheduler ProcLocker.setup

let clean () = if_restart_scheduler ProcLocker.clean
