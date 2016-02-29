(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging
module F = Format

(** Module to process clusters of procedures. *)

(** a cluster is a file *)
type t = DB.source_dir

(** type stored in .cluster file: (n,m,cl) indicates cl is cluster n out of m *)
type serializer_t = int * int * t

(** Load a cluster from a file *)
val load_from_file : DB.filename -> serializer_t option

(** Print a cluster *)
val pp_cluster : int -> int -> t -> F.formatter -> unit -> unit

(** Print a cluster name *)
val pp_cluster_name : F.formatter -> int -> unit

(** Print clusters *)
val print_clusters : t list -> unit
