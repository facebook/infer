(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Handle timeout events *)

val exe_timeout : ('a -> unit) -> 'a -> SymOp.failure_kind option
(** Execute the function up to a given timeout. *)

val resume_previous_timeout : unit -> unit
(** Resume a previously suspended timeout. *)

val suspend_existing_timeout : keep_symop_total:bool -> unit
(** Suspend the current timeout. It must be resumed later. *)
