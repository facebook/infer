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

(** Create tasks with a list of closures to be executed in parallel,
    and an optional continuation to be executed afterwards *)
val create : ?continuation:(closure option) -> closure list -> t

(** No-op tasks *)
val empty : t

(** Run the closures and continuation *)
val run : t -> unit

module Runner : sig
  (** A runner accepts new tasks repeatedly for parallel execution *)
  type runner

  (** Create a runner *)
  val create: jobs:int -> runner

  (** Start the given tasks with the runner *)
  val start : runner -> tasks:t -> unit

  (** Complete all the outstanding tasks *)
  val complete : runner -> unit
end
