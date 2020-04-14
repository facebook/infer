(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Entries in the results directory (infer-out/). Unless you want to specify a custom results
    directory you probably want to use {!ResultsDir.Entry} instead of this module. *)

type id =
  | Logs  (** log file *)
  | Specs  (** directory containing summaries as .specs files *)
  | Temporary  (** directory containing temp files *)

val get_path : results_dir:string -> id -> string
(** the absolute path for the given entry *)

val to_delete_before_incremental_capture_and_analysis : results_dir:string -> string list
(** utility for {!ResultsDir.scrub_for_incremental}, you probably want to use that instead *)

val to_delete_before_caching_capture : results_dir:string -> string list
(** utility for {!ResultsDir.scrub_for_caching}, you probably want to use that instead *)
