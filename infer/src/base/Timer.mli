(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Time long-running operations listed in {!Timeable} under a timeout *)

type state

exception Timeout of float

val check_timeout : unit -> unit
(** check if the timer has been running for more than {!Config.timeout} seconds *)

val time : Timeable.t -> on_timeout:(float -> 'a) -> f:(unit -> 'a) -> 'a

val suspend : unit -> state

val resume : state -> unit
