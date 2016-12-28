(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module L = Logging
module F = Format

(** Module to process clusters of procedures. *)

(** a cluster is a file *)
type t = DB.source_dir

(** type stored in .cluster file: (n,cl) indicates cl is cluster n *)
type serializer_t = int * t

(** Serializer for clusters *)
let serializer : serializer_t Serialization.serializer =
  Serialization.create_serializer Serialization.cluster_key

(** Load a cluster from a file *)
let load_from_file (filename : DB.filename) : serializer_t option =
  Serialization.from_file serializer filename

(** Save a cluster into a file *)
let store_to_file (filename : DB.filename) (serializer_t: serializer_t) =
  Serialization.to_file serializer filename serializer_t

let cl_name n = "cl" ^ string_of_int n
let cl_file n = "x" ^ (cl_name n) ^ ".cluster"
let pp_cluster_name fmt n = Format.fprintf fmt "%s" (cl_name n)

let pp_cluster fmt (nr, cluster) =
  let fname = Config.results_dir ^/ Config.multicore_dir_name ^/ cl_file nr in
  let pp_cl fmt n = Format.fprintf fmt "%s" (cl_name n) in
  store_to_file (DB.filename_from_string fname) (nr, cluster);
  F.fprintf fmt "%a: @\n" pp_cl nr;
  F.fprintf fmt "\t$(INFERANALYZE) -cluster '%s'@\n" fname;
  (* touch the target of the rule to let `make` know that the job has been done *)
  F.fprintf fmt "\t@@touch $@@@\n";
  F.fprintf fmt "@\n"
