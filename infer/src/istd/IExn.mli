(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core

val reraise_after : f:(unit -> unit) -> exn -> 'a
(** Reraise the exception after doing f. Always reraise immediately after catching the exception,
    otherwise the backtrace can be wrong. *)

val reraise_if : f:(unit -> bool) -> exn -> unit
(** Reraise the exception if f returns true. Always reraise immediately after catching the
    exception, otherwise the backtrace can be wrong. *)
