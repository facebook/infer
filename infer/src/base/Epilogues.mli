(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val register : f:(unit -> unit) -> description:string -> unit
(** Register a function to run when the program exits or is interrupted. Registered functions are
    run in the reverse order in which they were registered. *)

val register_late : f:(unit -> unit) -> description:string -> unit
(** Register a function to run when the program exits or is interrupted. Registered functions are
   run in the reverse order in which they were registered, but *after* the ones registered with
   {!register}. *)

val run : unit -> unit

val reset : unit -> unit
