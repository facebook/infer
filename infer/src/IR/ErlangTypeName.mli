(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type t =
  | Any
  | Atom
  | Integer
  | Cons
  | Nil
  | Tuple of int
  | Map
  | GenServerPid of {module_name: string option}
  | ModuleInfo
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

val pp : Format.formatter -> t -> unit

val to_string : t -> string

val from_string : string -> t option

val atom_name : string

val atom_hash : string

val atom_true : string

val atom_false : string

val module_info_field_name : string

val module_info_attributes_class_name : string

val calculate_hash : string -> int

val integer_value : string

val cons_head : string

val cons_tail : string

val tuple_elem : int -> string

val tuple_field_names : int -> string list

val erlang_namespace : string

val unsupported : string

val infer_erlang_namespace : string
