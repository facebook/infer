(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format

(** Module to process clusters of procedures. *)

(** a cluster is a file *)
type t = DB.source_dir

(** type stored in .cluster file: (n,cl) indicates cl is cluster n *)
type serializer_t = int * t

val load_from_file : DB.filename -> serializer_t option
(** Load a cluster from a file *)

val pp_cluster : F.formatter -> serializer_t -> unit
(** Print a cluster *)

val pp_cluster_name : F.formatter -> int -> unit
(** Print a cluster name *)
