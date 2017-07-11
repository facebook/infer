(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Serialization of data stuctures *)

module Key : sig
  (** Serialization key, used to distinguish versions of serializers and avoid assert faults *)
  type t

  val analysis_results : t
  (** current key for an analysis results value *)

  val attributes : t
  (** current key for proc attributes *)

  val cfg : t
  (** current key for a cfg *)

  val cg : t
  (** current key for a call graph *)

  val cluster : t
  (** current key for a cluster *)

  val lint_issues : t
  (** current key for lint issues *)

  val summary : t
  (** current key for a procedure summary *)

  val tenv : t
  (** current key for tenv *)

  val trace : t
  (** current key for an error trace *)
end

(** Generic serializer *)
type 'a serializer

val create_serializer : Key.t -> 'a serializer
(** create a serializer from a file name
    given an integer key used as double-check of the file type *)

val read_from_file : 'a serializer -> DB.filename -> 'a option
(** Deserialize a file and check the keys *)

val read_from_string : 'a serializer -> string -> 'a option
(** Deserialize a string and check the keys *)

val update_file : 'a serializer -> f:('a option -> 'a) -> DB.filename -> unit
(** Serialize into a file.
    The upd function takes the old value, if any, and returns the value to write *)

val write_to_file : 'a serializer -> data:'a -> DB.filename -> unit
(** Serialize into a file writing value *)
