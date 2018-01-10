(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Pool of processes to execute in parallel up to a number of jobs. *)
type t

exception Execution_error of string  (** Infer process execution failure *)

val create : jobs:int -> t
(** Create a new pool of processes *)

val start_child : f:('a -> unit) -> pool:t -> 'a -> unit
(** Start a new child process in the pool.
    If all the jobs are taken, wait until one is free. *)

val wait_all : t -> unit
(** Wait until all the currently executing processes terminate *)

