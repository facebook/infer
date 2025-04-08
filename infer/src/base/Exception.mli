(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The restart scheduler uses exceptions for control flow (restarts/timeouts respectively).
    Functions here abstract away the semantics of when an exception can be ignored. *)

open! IStd

val try_finally : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a
(** [try_finally ~f ~finally] executes [f] and then [finally] even if [f] raises an exception.
    Restart scheduler exceptions are handled as necessary. *)
