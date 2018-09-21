(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val add_run_to_sequence : unit -> unit
(** add an entry with the current run date *)

val set_merge_capture : bool -> unit
(** update the 'merge after capture' smart option *)

val get_merge_capture : unit -> bool
(** fetch the value of the 'merge after capture' smart option *)

val store : unit -> unit
(** save the current state to disk *)

val load_and_validate : unit -> (unit, string) Result.t
(** attempt to load state from disk *)

val reset : unit -> unit
(** reset the in-memory state to what it would be if this were a fresh run of infer *)
