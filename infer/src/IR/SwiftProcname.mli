(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  | ClassMethod of {class_name: Typ.Name.t; method_name: Mangled.t}
  | Function of {function_name: Mangled.t}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

val mk_function : Mangled.t -> t

val mk_class_method : Typ.Name.t -> Mangled.t -> t

val get_function_name : t -> Mangled.t

val pp : PpDetailLevel.t -> F.formatter -> t -> unit
