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

val incr_ondemand_local_cache_hits : unit -> unit

val incr_ondemand_local_cache_misses : unit -> unit

val add_to_proc_locker_lock_time : ExecutionDuration.t -> unit

val add_to_proc_locker_unlock_time : ExecutionDuration.t -> unit

val add_to_restart_scheduler_total_time : ExecutionDuration.t -> unit

val add_to_restart_scheduler_useful_time : ExecutionDuration.t -> unit

val reset : unit -> unit
(** reset all stats *)

val get : unit -> t
(** get the stats so far *)

val log_aggregate : t list -> unit
(** log aggregated stats to infer's log file and to Scuba *)
