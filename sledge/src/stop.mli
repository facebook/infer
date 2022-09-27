(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Stop analysis when encountering issues *)

exception Stop
exception Reached_goal of {steps: int}
exception Unreachable_goal
exception Unimplemented of {feature: string}

val on_unknown_call : 'a -> unit
val on_alarm : Alarm.t -> unit
val on_reached_goal : int -> 'a -> unit
val on_unreachable_goal : dp_path:(Format.formatter -> unit) -> unit
val on_unimplemented : string -> 'a -> unit
