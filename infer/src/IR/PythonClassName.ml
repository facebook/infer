(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(* TODO: add support for
   - module names
   - nested classes
*)
type builtin_type = PyBool | PyDict | PyInt | PyNone | PyObject | PyTuple
[@@deriving compare, equal, yojson_of, sexp, hash, normalize, show]

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

let builtin_type_to_string = show_builtin_type

let builtin_closure_to_string = function IntFun -> "int" | StrFun -> "str" | TypeFun -> "type"

let pp fmt = function
  | Builtin builtin ->
      F.pp_print_string fmt (builtin_type_to_string builtin)
  | Globals name ->
      F.fprintf fmt "Globals[%s]" name
  | Closure name ->
      F.fprintf fmt "Closure[%s]" name
  | BuiltinClosure builtin ->
      F.fprintf fmt "ClosureBuiltin[%s]" (builtin_closure_to_string builtin)
  | ClassCompanion {module_name; attr_name} ->
      F.fprintf fmt "ClassCompanion[%s,%s]" module_name attr_name
  | ModuleAttribute {module_name; attr_name} ->
      F.fprintf fmt "ModuleAttribute[%s,%s]" module_name attr_name
  | Filename name ->
      F.pp_print_string fmt name
  | Package name ->
      F.fprintf fmt "%s.__init__" name
  | Wildcard ->
      F.pp_print_char fmt '?'


let components tname = [F.asprintf "%a" pp tname]

let to_string = Pp.string_of_pp pp

let classname = to_string

let is_final = function Globals _ -> true | _ -> false

let is_singleton = function BuiltinClosure _ | Builtin PyNone | Closure _ -> true | _ -> false

let concatenate_package_name_and_file_name typename filename =
  match typename with
  | Globals module_name ->
      Some (Globals (Printf.sprintf "%s::%s" module_name filename))
  | _ ->
      None


let get_builtin_closure_from_builtin_type = function
  | PyObject | PyDict | PyTuple | PyNone | PyBool ->
      None
  | PyInt ->
      Some IntFun
