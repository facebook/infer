(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type ('a, 'b) doer = 'a -> 'b option

val run_sequentially : f:('a, 'b) doer -> 'a list -> unit
(** Run the tasks sequentially *)

(** A runner accepts new tasks repeatedly for parallel execution *)
module Runner : sig
  type ('work, 'final, 'result) t

  val create :
       ?with_primary_db:bool
    -> jobs:int
    -> child_prologue:(ProcessPool.Worker.id -> unit)
    -> f:('work, 'result) doer
    -> child_epilogue:(ProcessPool.Worker.id -> 'final)
    -> (unit -> ('work, 'result) ProcessPool.TaskGenerator.t)
    -> ('work, 'final, 'result) t
  (** Create a runner running [jobs] jobs in parallel *)

  val run : (_, 'final, _) t -> 'final option Array.t
  (** Start the given tasks with the runner and wait until completion *)
end
