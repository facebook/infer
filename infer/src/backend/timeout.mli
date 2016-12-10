(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Handle timeout events *)

(** Execute the function up to a given timeout. *)
val exe_timeout : ('a -> unit) -> 'a -> SymOp.failure_kind option

(** Resume a previously suspended timeout. *)
val resume_previous_timeout : unit -> unit

(** Suspend the current timeout. It must be resumed later. *)
val suspend_existing_timeout : keep_symop_total:bool -> unit
