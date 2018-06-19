(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Each task executes a closure *)
type task = unit -> unit

(** A sequence of tasks that can be executed in parallel *)
type t = task list

val aggregate : size:int -> t -> t
(** Aggregate closures into groups of the given size *)

val run : t -> unit
(** Run the closures *)

val fork_protect : f:('a -> 'b) -> 'a -> 'b
(** does the bookkeeping necessary to safely execute an infer function [f] after a call to fork(2) *)

(** A runner accepts new tasks repeatedly for parallel execution *)
module Runner : sig
  type tasks = t

  type t

  val create : jobs:int -> t
  (** Create a runner running [jobs] jobs in parallel *)

  val start : t -> tasks:tasks -> unit
  (** Start the given tasks with the runner *)

  val complete : t -> unit
  (** Wait until all the outstanding tasks are completed *)
end
