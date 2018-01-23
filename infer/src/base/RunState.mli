(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val add_run_to_sequence : unit -> unit
(** add an entry with the current run date *)

val store : unit -> unit
(** save the current state to disk *)

val load_and_validate : unit -> (unit, string) Result.t
(** attempt to load state from disk *)

val reset : unit -> unit
(** reset the in-memory state to what it would be if this were a fresh run of infer *)
