(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
include PpDetailLevel

type t =
  | ClassMethod of {class_name: Typ.Name.t; method_name: Mangled.t}
  | Function of {function_name: Mangled.t}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let mk_function mangled = Function {function_name= mangled}

let mk_class_method class_name mangled = ClassMethod {class_name; method_name= mangled}

let get_function_name osig =
  match osig with
  | ClassMethod {method_name} ->
      method_name
  | Function {function_name} ->
      function_name


let pp verbosity fmt osig =
  let sep = "." in
  match osig with
  | ClassMethod osig -> (
    match verbosity with
    | Simple ->
        F.pp_print_string fmt (Mangled.to_string osig.method_name)
    | Non_verbose | NameOnly | FullNameOnly ->
        F.fprintf fmt "%s%s%s" (Typ.Name.name osig.class_name) sep
          (Mangled.to_string osig.method_name)
    | Verbose ->
        F.fprintf fmt "%s%s%a" (Typ.Name.name osig.class_name) sep Mangled.pp osig.method_name )
  | Function osig -> (
    match verbosity with
    | Simple | Non_verbose | NameOnly | FullNameOnly ->
        F.pp_print_string fmt (Mangled.to_string osig.function_name)
    | Verbose ->
        F.pp_print_string fmt (Mangled.to_string_full osig.function_name) )
