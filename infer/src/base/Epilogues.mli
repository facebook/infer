(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val register : f:(unit -> unit) -> string -> unit
(** Register a function to run when the program exits or is interrupted. Registered functions are
    run in the reverse order in which they were registered. *)
