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

val create : jobs:int -> child_prelude:(unit -> unit) -> f:('a -> unit) -> 'a t
(** Create a new pool of processes running [jobs] jobs in parallel *)

val run : 'a t -> 'a list -> unit
(** use the processes in the given process pool to run all the given tasks in parallel *)
