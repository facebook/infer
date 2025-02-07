(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type builtin_type = PyBool | PyDict | PyInt | PyNone | PyObject | PyString | PyTuple
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

type builtin_closure = IntFun | StrFun | TypeFun
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

type t =
  | Builtin of builtin_type
  | Globals of string
  | Closure of string
  | BuiltinClosure of builtin_closure
  | ClassCompanion of {module_name: string; attr_name: string}
  | ModuleAttribute of {module_name: string; attr_name: string}
  | Filename of string
  | Package of string
  | Wildcard
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

val classname : t -> string

val components : t -> string list

val pp : F.formatter -> t -> unit

val to_string : t -> string

val is_final : t -> bool

val is_singleton : t -> bool

val split_module_attr : t -> (string * string) option
(** if the argument is a closure type or a module type, split the corresponding name into a pair
    (module_name, function_name) *)

val concatenate_package_name_and_file_name : t -> string -> t option

val get_builtin_closure_from_builtin_type : builtin_type -> builtin_closure option
