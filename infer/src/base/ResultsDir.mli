(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module RunState : sig
  val add_run_to_sequence : unit -> unit
  (** add an entry with the current run date *)
end

val get_path : ResultsDirEntryName.id -> string
(** Wrapper around {!ResultsDirEntryName.get_path} that implicitly applies to the current results
    directory {!Config.results_dir}. If you need to specify another results directory use
    {!ResultsDirEntryName} directly. *)

val assert_results_dir : string -> unit
(** Check that the results dir exists and sets up logging, the database, etc. *)

val remove_results_dir : unit -> unit
(** Recursively delete the results directory. *)

val create_results_dir : unit -> unit
(** Create the results dir and sets up logging, the database, etc. *)

val scrub_for_incremental : unit -> unit
(** scrub capture data in preparation of an incremental capture + analysis *)

val scrub_for_caching : unit -> unit
(** Clean up the results dir to keep only what's relevant to go in a cache (e.g., the distributed
    Buck cache). In particular, get rid of non-deterministic outputs.*)
