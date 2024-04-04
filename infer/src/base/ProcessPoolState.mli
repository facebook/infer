(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val in_child : int option ref
(** Keep track of whether the current execution is in a child process *)

val update_status : (Mtime.t -> string -> unit) ref
(** Ping the task bar whenever a new task is started with the start time and a description for the
    task *)

val update_heap_words : (unit -> unit) ref

val get_pid : unit -> Pid.t

val reset_pid : unit -> unit

val has_running_children : bool ref
