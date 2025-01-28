(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

val make_closure : string -> t

val make_filename : string -> t

val make_global : string -> t

val make_reserved_builtin : string -> t

val builtin_object : t

val builtin_dict : t

val builtin_int : t

val builtin_none : t

val builtin_bool : t

val builtin_tuple : t

val classname : t -> string

val components : t -> string list

val wildcard : t

val pp : F.formatter -> t -> unit

val to_string : t -> string

val is_final : t -> bool

val is_module : t -> bool

val is_module_attribute : t -> bool

val is_reserved_builtin : t -> bool

val get_module_attribute_infos : t -> (t * string) option
(** will return the pair (module_name, attribute) params of the type iff the type name is a module
    attribute type *)

val get_module_name : t -> string option
(** will return the string representation of the module iff type name is a module type *)

val get_reserved_builtin : t -> string option

val get_reserved_builtin_from_underlying_pyclass : t -> string option

val globals_prefix : string

val concatenate_package_name_and_file_name : t -> string -> t option
