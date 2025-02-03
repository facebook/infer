(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type worker_id = Pid of Pid.t | Domain of int

val set_in_child : int option -> unit
(** Keep track of whether the current execution is in a child process/domain *)

val get_in_child : unit -> int option
(** Get whether the current execution is in a child process/domain *)

val update_status : (Mtime.t option -> string -> unit) ref
(** Ping the task bar whenever a new task is started with the start time (or just rename the current
    task if the start time is [None]) and a description for the task *)

val update_heap_words : (unit -> unit) ref

val get_pid : unit -> Pid.t

val reset_pid : unit -> unit
