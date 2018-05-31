(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val register : f:(unit -> unit) -> string -> unit
(** Register a function to run when the program exits or is interrupted. Registered functions are
    run in the reverse order in which they were registered. *)

val register_late : (unit -> unit) -> unit

val late : unit -> unit
