(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

module ProcLocker : sig
  val setup : unit -> unit
  (** This should be called once before trying to lock Anything. *)

  val try_lock : Procname.t -> bool
  (** true = the lock belongs to the calling process. false = the lock belongs to a different worker *)

  val unlock : Procname.t -> unit
  (** This will work as a cleanup function because after calling unlock all the workers that need an
      unlocked Proc should find it's summary already Cached. Throws if the lock had not been taken. *)

  val clean : unit -> unit
  (** This should be called when locks will no longer be used to remove any files or state that's
      not necessary. *)
end = struct
  let setup () = ()

  let try_lock _pname = true

  let unlock _pname = ()

  let clean () = ()
end

let of_list (lst : 'a list) : 'a ProcessPool.TaskGenerator.t =
  let content = Queue.of_list lst in
  let remaining = ref (Queue.length content) in
  let remaining_tasks () = !remaining in
  let is_empty () = Queue.is_empty content in
  let finished ~completed work = if completed then decr remaining else Queue.enqueue content work in
  let next () = Queue.dequeue content in
  {remaining_tasks; is_empty; finished; next}


(** This behaves exactly the same as the FileScheduler so far. The goal is that it will use a work
    queue and proc locking to avoid repeating work and hopefully get some in process cache hits. *)
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


let locked_procs = Stack.create ()

let unlock_all () = Stack.until_empty locked_procs ProcLocker.unlock

let record_locked_proc (pname : Procname.t) = Stack.push locked_procs pname

let if_restart_scheduler f =
  match Config.scheduler with File | SyntacticCallGraph -> () | Restart -> f ()


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


let setup () = ProcLocker.setup ()

let clean () = ProcLocker.clean ()
