(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t

val refresh : t -> unit
(** draw the taskbar *)

val create : jobs:int -> t
(** creates a task bar for running [jobs] jobs in parallel *)

val update_status : t -> slot:int -> Mtime.t -> string -> unit
(** [update_status task_bar ~slot t status] records an event described by [status] on slot [slot]
    started at time [t] *)

val set_tasks_total : t -> int -> unit
(** set the total number of tasks to do *)

val tasks_done_reset : t -> unit
(** record that 0 tasks have been completed so far *)

val set_remaining_tasks : t -> int -> unit
(** set the number of tasks remaining to complete *)

val finish : t -> unit
(** tear down the task bar and ready the terminal for more output *)
