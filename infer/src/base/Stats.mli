(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** collect statistics about backend/analysis operations *)

type t

val incr_summary_file_try_load : unit -> unit
(** a query to the filesystem attempting to load a summary file *)

val incr_summary_read_from_disk : unit -> unit
(** a summary file is deserialized from disk *)

val incr_summary_cache_hits : unit -> unit

val incr_summary_cache_misses : unit -> unit

val incr_ondemand_procs_analyzed : unit -> unit

val add_to_proc_locker_lock_time : ExecutionDuration.t -> unit

val add_to_proc_locker_unlock_time : ExecutionDuration.t -> unit

val add_to_restart_scheduler_total_time : ExecutionDuration.t -> unit

val add_to_restart_scheduler_useful_time : ExecutionDuration.t -> unit

val incr_pulse_aliasing_contradictions : unit -> unit

val incr_pulse_args_length_contradictions : unit -> unit

val incr_pulse_captured_vars_length_contradictions : unit -> unit

val add_pulse_disjuncts_dropped : int -> unit

val add_pulse_interrupted_loops : int -> unit

val incr_pulse_unknown_calls : unit -> unit

val incr_pulse_unknown_calls_on_hack_resource : unit -> unit

val incr_pulse_summaries_contradictions : unit -> unit

val incr_pulse_summaries_unsat_for_caller : unit -> unit

val incr_pulse_summaries_with_some_unreachable_nodes : unit -> unit

val incr_pulse_summaries_with_some_unreachable_returns : unit -> unit

val incr_pulse_summaries_count_0_continue_program : unit -> unit

val add_pulse_summaries_count : int -> unit

val add_proc_duration_us : string -> string -> int -> unit

val incr_topl_reachable_calls : unit -> unit

val incr_timeouts : unit -> unit

val add_timing : Timeable.t -> float -> unit

val set_process_times : ExecutionDuration.t -> unit

val set_useful_times : ExecutionDuration.t -> unit

val incr_spec_store_times : ExecutionDuration.counter -> unit

val reset : unit -> unit
(** reset all stats *)

val get : unit -> t
(** get the stats so far *)

val log_aggregate : t list -> unit
(** log aggregated stats to infer's log file and to Scuba *)
