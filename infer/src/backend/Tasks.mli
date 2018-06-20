(*
 * Copyright (c) 2017-present, Facebook, Inc.
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
  type 'a t

  val create : jobs:int -> f:'a doer -> 'a t
  (** Create a runner running [jobs] jobs in parallel *)

  val run : 'a t -> tasks:'a list -> unit
  (** Start the given tasks with the runner and wait until completion *)
end
