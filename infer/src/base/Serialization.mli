(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Serialization of data stuctures *)

module Key : sig
  (** Serialization key, used to distinguish versions of serializers and avoid assert faults *)
  type t

  val tenv : t
  (** current key for tenv *)
end

(** Generic serializer *)
type 'a serializer

val create_serializer : Key.t -> 'a serializer
(** create a serializer from a file name given an integer key used as double-check of the file type *)

val read_from_file : 'a serializer -> DB.filename -> 'a option
(** Deserialize a file and check the keys *)

val write_to_file : 'a serializer -> data:'a -> DB.filename -> unit
(** Serialize into a file writing value *)

val generate_keys : unit -> int * int * int
[@@warning "-unused-value-declaration"]
(** Generate new (random) serialization keys, to be used in an ocaml toplevel *)
