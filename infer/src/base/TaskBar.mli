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

val update_status : t -> slot:int -> Mtime.t -> ?heap_words:int -> string -> unit
(** [update_status task_bar ~slot t ?heap_words status] records an event described by [status] on
    slot [slot] started at time [t] with total size of the major heap [heap_words] *)

val update_heap_words : t -> slot:int -> int -> unit
(** [update_heap_words task_bar ~slot heap_words] records [heap_words] on slot [slot] *)

val set_tasks_total : t -> int -> unit
(** set the total number of tasks to do *)

val tasks_done_reset : t -> unit
(** record that 0 tasks have been completed so far *)

val set_remaining_tasks : t -> int -> unit
(** set the number of tasks remaining to complete *)

val finish : t -> unit
(** tear down the task bar and ready the terminal for more output *)
