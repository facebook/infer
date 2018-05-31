(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val assert_results_dir : string -> unit
(** Check that the results dir exists and sets up logging, the database, etc. *)

val remove_results_dir : unit -> unit
(** Recursively delete the results directory. *)

val create_results_dir : unit -> unit
(** Create the results dir and sets up logging, the database, etc. *)

val delete_capture_and_analysis_data : unit -> unit
(** delete all results from the capture and the analysis *)
