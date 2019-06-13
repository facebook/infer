(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* FB-ONLY *)

(**
   Functionality for logging into "infer_events" Scuba table.
   The table is organized in form of key-value pairs.
   Two most important fields are "event" and "value".
   Other fields in the table correspond to things common for this particular
   run of Infer.
 *)

val log_count : name:string -> value:int -> unit
(** Log anything that can be counted. Events will be prefixed with "count." *)

val execute_with_time_logging : string -> (unit -> 'a) -> 'a
(**
    A helper to log execution time of a particular function.
    Use this to measure a performance of a given function.
    Example:
    {|
      let f a b = <some code>
      let f a b = ScubaEventLogging.execute_with_time_logging "f" (fun () -> f a b)
    |}
 *)
