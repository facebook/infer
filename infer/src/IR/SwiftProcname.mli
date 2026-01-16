(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type builtin = DerivedEnumEquals | DynamicCall | InitTuple | NonDet | ObjcMsgSend
[@@deriving compare, equal, yojson_of, sexp, hash, normalize, enumerate]

type t =
  | ClassMethod of {class_name: Typ.Name.t; method_name: Mangled.t}
  | Function of {function_name: Mangled.t}
  | Builtin of builtin
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

val mk_function : Mangled.t -> t

val mk_class_method : Typ.Name.t -> Mangled.t -> t

val mk_builtin : builtin -> t

val get_function_name : t -> Mangled.t

val pp : PpDetailLevel.t -> F.formatter -> t -> unit

val builtin_from_string : string -> builtin option

val show_builtin : builtin -> string

val to_string : t -> string
