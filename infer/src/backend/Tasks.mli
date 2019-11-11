(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type 'a doer = 'a -> unit

val run_sequentially : f:'a doer -> 'a list -> unit
(** Run the tasks sequentially *)

val fork_protect : f:('a -> 'b) -> 'a -> 'b
(** does the bookkeeping necessary to safely execute an infer function [f] after a call to fork(2) *)

(** A runner accepts new tasks repeatedly for parallel execution *)
module Runner : sig
  type ('work, 'final) t

  val create :
       jobs:int
    -> f:'work doer
    -> child_epilogue:(unit -> 'final)
    -> tasks:'work ProcessPool.TaskGenerator.t
    -> ('work, 'final) t
  (** Create a runner running [jobs] jobs in parallel *)

  val run : (_, 'final) t -> 'final option Array.t
  (** Start the given tasks with the runner and wait until completion *)
end
