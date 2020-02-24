(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t

type 'a evaluation_result = {result: 'a; execution_duration: t}

val zero : t

val since : Unix.process_times -> t

val add_duration_since : t -> Unix.process_times -> t

val add : t -> t -> t

val user_time : t -> float

val sys_time : t -> float

val pp : field:string -> Format.formatter -> t -> unit

val timed_evaluate : f:(unit -> 'a) -> 'a evaluation_result
