(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** collect statistics about backend/analysis operations *)

type t

val initial : t

val incr_summary_file_try_load : unit -> unit
(** a query to the filesystem attempting to load a summary file *)

val incr_summary_read_from_disk : unit -> unit
(** a summary file is deserialized from disk *)

val incr_summary_cache_hits : unit -> unit

val incr_summary_cache_misses : unit -> unit

val incr_summary_has_model_queries : unit -> unit
(** someone asked if a proc name has a biabduction model *)

val incr_ondemand_procs_analyzed : unit -> unit

val incr_ondemand_local_cache_hits : unit -> unit

val incr_ondemand_local_cache_misses : unit -> unit

val reset : unit -> unit
(** reset all stats *)

val get : unit -> t
(** get the stats so far *)

val pp : Format.formatter -> t -> unit

val merge : t -> t -> t
(** return a new value that adds up the stats in both arguments *)

val log_to_scuba : t -> unit
(** Log aggregated backend stats to Scuba. Use after the stats have been fully calculated *)
