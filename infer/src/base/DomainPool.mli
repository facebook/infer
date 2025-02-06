(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Pool of domain workers that can both receive tasks from the orchestrator process and start doing
    tasks on their own. A shared concurrent queue is used for updates from workers and a concurrent
    queue per worker is used for sending commands to the worker.

    See also {!module-ProcessPool}. *)

(** A [('work, 'final) t] domain pool accepts tasks of type ['work] and produces an array of results
    of type ['final]. ['work] and ['final] will be marshalled over a Unix pipe.*)
type (_, _, _) t

module Worker : sig
  (** the number matches the index of the worker in the array of final results *)
  type id = private int [@@deriving show]
end

val create :
     jobs:int
  -> child_prologue:(Worker.id -> unit)
  -> f:('work -> 'result option)
  -> child_epilogue:(Worker.id -> 'final)
  -> tasks:(unit -> ('work, 'result, WorkerPoolState.worker_id) TaskGenerator.t)
  -> ('work, 'final, 'result) t
(** Create a new pool of domains running [jobs] jobs in parallel *)

val run : (_, 'final, _) t -> 'final option Array.t
(** use the domains in the given domain pool to run all the given tasks in parallel and return the
    results of the epilogues *)
