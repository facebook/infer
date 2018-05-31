(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** A sequence of tasks that can be executed in parallel,
    with a continuation to be executed at the end *)
type t

(** Each task/continuation executes a closure *)
type closure = unit -> unit

(* Aggregate closures into groups of the given size *)

val aggregate : size:int -> t -> t

val create : ?continuation:closure option -> closure list -> t
(** Create tasks with a list of closures to be executed in parallel,
    and an optional continuation to be executed afterwards *)

val run : t -> unit
(** Run the closures and continuation *)

val fork_protect : f:('a -> 'b) -> 'a -> 'b
(** does the bookkeeping necessary to safely execute an infer function [f] after a call to fork(2) *)

module Runner : sig
  (** A runner accepts new tasks repeatedly for parallel execution *)
  type runner

  val create : jobs:int -> runner
  (** Create a runner *)

  val start : runner -> tasks:t -> unit
  (** Start the given tasks with the runner *)

  val complete : runner -> unit
  (** Complete all the outstanding tasks *)
end
