(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The restart scheduler and biabduction use exceptions for control flow (restarts/timeouts
    respectively). Functions here abstract away the semantics of when an exception can be ignored. *)

open! IStd

(** types of biabduction failure due to timeouts *)
type failure_kind =
  | FKtimeout  (** max time exceeded *)
  | FKsymops_timeout of int  (** max symop's exceeded *)
  | FKcrash of string  (** uncaught exception or failed assertion *)

val pp_failure_kind : Format.formatter -> failure_kind -> unit

(** Timeout exception *)
exception Analysis_failure_exe of failure_kind

val exn_not_failure : exn -> bool
(** check that the exception is not a biabduction timeout or restart scheduler exception *)

val try_finally : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a
(** [try_finally ~f ~finally] executes [f] and then [finally] even if [f] raises an exception.
    Biabduction timeouts and restart scheduler exceptions are handled as necessary. *)
