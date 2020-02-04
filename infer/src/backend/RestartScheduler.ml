(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let of_list (lst : 'a list) : 'a ProcessPool.TaskGenerator.t =
  let content = Queue.of_list lst in
  let remaining = ref (Queue.length content) in
  let remaining_tasks () = !remaining in
  let is_empty () = Int.equal !remaining 0 in
  let finished ~completed work = if completed then decr remaining else Queue.enqueue content work in
  let next () = Queue.dequeue content in
  {remaining_tasks; is_empty; finished; next}


let make_with_procs_from sources =
  let gen =
    List.map sources ~f:SourceFiles.proc_names_of_source
    |> List.concat
    |> List.rev_map ~f:(fun procname -> SchedulerTypes.Procname procname)
    |> List.permute ~random_state:(Random.State.make (Array.create ~len:1 0))
    |> of_list
  in
  let next x =
    let res = gen.next x in
    (* see defn of gen above to see why res should never match Some (File _) *)
    match res with None -> None | Some (File _) -> assert false | Some (Procname _) as v -> v
  in
  {gen with next}


let make sources =
  ProcessPool.TaskGenerator.chain (make_with_procs_from sources) (FileScheduler.make sources)


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
        raise ProcessPool.ProcnameAlreadyLocked ) )


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
