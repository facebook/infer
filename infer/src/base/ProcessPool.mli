(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module TaskGenerator : sig
  (** abstraction for generating jobs *)
  type 'a t =
    { remaining_tasks: unit -> int
          (** number of tasks remaining to complete -- only used for reporting, so imprecision is not a bug *)
    ; is_empty: unit -> bool
          (** when should the main loop of the task manager stop expecting new tasks *)
    ; finished: 'a -> unit
          (** Process pool calls [finished x] when a worker finishes item [x]. This is only called 
            if [next ()] has previously returned [Some x] and [x] was sent to a worker. *)
    ; next: unit -> 'a option
          (** [next ()] generates the next work item. If [is_empty ()] is true then [next ()] 
            must return [None].  However, it is OK to for [next ()] to return [None] when [is_empty] 
            is false.  This corresponds to the case where there is more work to be done, 
            but it is not schedulable until some already scheduled work is finished. *)
    }

  val chain : 'a t -> 'a t -> 'a t
  (** chain two generators in order *)

  val of_list : 'a list -> 'a t
  (** schedule tasks out of a concrete list *)
end

(** Pool of parallel workers that can both receive tasks from the master process and start doing
    tasks on their own. Unix pipes are used for communication, all while refreshing a task bar
    periodically.

    Due to ondemand analysis, workers may do tasks unprompted (eg, when analysing a procedure, a
    process will typically end up analysing all its callees). Thus, children need to update the main
    process (which is in charge of the task bar) whenever they start analysing a new procedure, and
    whenever they resume analysing a previous procedure. This is more complicated than what, eg,
    `ParMap` can handle because of the bidirectional flow between children and parents.

    The children send "Ready" or "I'm working on task <some string>" messages that are used to
    respectively send them more tasks ("Do x") or update the task bar with the description provided
    by the child.

    See also {!module-ProcessPoolState}. *)

(** A [('work, 'final) t] process pool accepts tasks of type ['work] and produces an array of
   results of type ['final]. ['work] and ['final] will be marshalled over a Unix pipe.*)
type (_, _) t

val create :
     jobs:int
  -> child_prelude:(unit -> unit)
  -> f:('work -> unit)
  -> child_epilogue:(unit -> 'final)
  -> tasks:'work TaskGenerator.t
  -> ('work, 'final) t
(** Create a new pool of processes running [jobs] jobs in parallel *)

val run : (_, 'final) t -> 'final option Array.t
(** use the processes in the given process pool to run all the given tasks in parallel and return
    the results of the epilogues *)
