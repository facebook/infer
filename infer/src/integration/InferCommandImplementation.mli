(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** implementations of infer commands *)

val debug : unit -> unit

val explore : unit -> unit

val help : unit -> unit

val report : unit -> unit

val report_diff : unit -> unit
