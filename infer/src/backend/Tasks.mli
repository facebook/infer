(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** A sequence of tasks that can be executed in parallel,
    with a continuation to be executed at the end *)
type t

type tasks = t

(** Each task/continuation executes a closure *)
type closure = unit -> unit

(* Aggregate closures into groups of the given size *)

val aggregate : size:int -> t -> t

val create : ?continuation:closure option -> closure list -> t
(** Create tasks with a list of closures to be executed in parallel,
    and an optional continuation to be executed afterwards *)

val empty : t
(** No-op tasks *)

val run : t -> unit
(** Run the closures and continuation *)

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
