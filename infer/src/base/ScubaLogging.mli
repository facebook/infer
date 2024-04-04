(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Functionality for logging into "infer_events" Scuba table. The table is organized in form of
    key-value pairs. Two most important fields are "event" and "value". Other fields in the table
    correspond to things common for this particular run of Infer. *)

val log_many : LogEntry.t list -> unit
(** Log several events in one go. Useful when you do custom aggregations and have a place to log all
    aggregated results at once. *)

val log_count : label:string -> value:int -> unit
(** Log anything that can be counted. Events will be prefixed with ["count."] *)

val log_duration : label:string -> duration_us:int -> unit
(** Log elapsed time. Events will be prefixed with ["time."] *)

val log_message : label:string -> message:string -> unit
(** Log a [string]. Event is prefixed with ["msg."] *)

val log_message_with_location : label:string -> loc:string -> message:string -> unit

val execute_with_time_logging : string -> (unit -> 'a) -> 'a
(** A helper to log execution time of a particular function. Use this to measure a performance of a
    given function. Example:

    {[
      let f a b = <some code>
      let f a b = ScubaLogging.execute_with_time_logging "f" (fun () -> f a b)
    ]} *)

val register_global_log_flushing_at_exit : unit -> unit
