(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

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

(** A ['a t] process pool accepts tasks of type ['a]. ['a] will be marshalled over a Unix pipe.*)
type _ t

(** abstraction for generating jobs *)
type 'a task_generator =
  { n_tasks: int
        (** total number of tasks -- only used for reporting, so imprecision is not a bug *)
  ; is_empty: unit -> bool
        (** when should the main loop of the task manager stop expecting new tasks *)
  ; next: 'a option -> 'a option
        (** [next (Some finished_item)] generates the next work item.
            The worker requesting more work has just finished processing [finished_item]. 
            [None] is passed when the worker was previously idle. 
            
            In particular, it is OK to for [next] to return [None] even when [is_empty] 
            is false.  This corresponds to the case where there is more work to be done, 
            but it is not schedulable until some already scheduled work is finished. *)
  }

val create :
  jobs:int -> child_prelude:(unit -> unit) -> f:('a -> unit) -> tasks:'a task_generator -> 'a t
(** Create a new pool of processes running [jobs] jobs in parallel *)

val run : 'a t -> unit
(** use the processes in the given process pool to run all the given tasks in parallel. *)
