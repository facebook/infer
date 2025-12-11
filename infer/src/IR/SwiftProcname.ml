(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
include PpDetailLevel

type builtin = NonDet | InitTuple | DynamicCall
[@@deriving compare, equal, yojson_of, sexp, hash, normalize, enumerate]

type t =
  | ClassMethod of {class_name: Typ.Name.t; method_name: Mangled.t}
  | Function of {function_name: Mangled.t}
  | Builtin of builtin
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let mk_function mangled = Function {function_name= mangled}

let mk_class_method class_name mangled = ClassMethod {class_name; method_name= mangled}

let mk_builtin builtin = Builtin builtin

let show_builtin = function
  | NonDet ->
      "llvm_nondet"
  | InitTuple ->
      "llvm_init_tuple"
  | DynamicCall ->
      "llvm_dynamic_call"


let get_function_name osig =
  match osig with
  | ClassMethod {method_name} ->
      method_name
  | Function {function_name} ->
      function_name
  | Builtin builtin ->
      Mangled.from_string (show_builtin builtin)


let pp_plain_class_name fmt proc_name =
  match proc_name with
  | ClassMethod {class_name= Typ.SwiftClass name} ->
      SwiftClassName.pp_plain_name fmt name
  | _ ->
      ()


let pp verbosity fmt proc_name =
  let sep = "." in
  match proc_name with
  | ClassMethod osig -> (
    match verbosity with
    | Simple ->
        F.pp_print_string fmt (Mangled.to_string osig.method_name)
    | Non_verbose | NameOnly | FullNameOnly ->
        F.fprintf fmt "%a%s%s" pp_plain_class_name proc_name sep
          (Mangled.to_string osig.method_name)
    | Verbose ->
        F.fprintf fmt "%s%s%a" (Typ.Name.name osig.class_name) sep Mangled.pp_full osig.method_name
    )
  | Function osig -> (
    match verbosity with
    | Simple | Non_verbose | NameOnly | FullNameOnly ->
        F.pp_print_string fmt (Mangled.to_string osig.function_name)
    | Verbose ->
        F.pp_print_string fmt (Mangled.to_string_full osig.function_name) )
  | Builtin builtin ->
      F.pp_print_string fmt (show_builtin builtin)


let builtin_from_string =
  let tbl = IString.Hash.create 100 in
  List.iter all_of_builtin ~f:(fun builtin -> IString.Hash.add tbl (show_builtin builtin) builtin) ;
  fun str -> IString.Hash.find_opt tbl str


let to_string osig = Format.asprintf "%a" (pp Simple) osig
