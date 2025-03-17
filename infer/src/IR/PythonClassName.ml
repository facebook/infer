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
type builtin_type = PyBool | PyDict | PyInt | PyNone | PyObject | PyString | PyTuple
[@@deriving compare, equal, yojson_of, sexp, hash, normalize, show]

type builtin_closure = DictFun | IntFun | StrFun | TypeFun
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

type t =
  | Builtin of builtin_type
  | Globals of string
  | Closure of string
  | BuiltinClosure of builtin_closure
  | ClassCompanion of {module_name: string; class_name: string}
  | ClassInstance of {module_name: string; class_name: string}
  | ModuleAttribute of {module_name: string; attr_name: string}
  | Filename of string
  | Package of string
  | Wildcard
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let builtin_type_to_string = show_builtin_type

let builtin_closure_to_string = function
  | DictFun ->
      "dict"
  | IntFun ->
      "int"
  | StrFun ->
      "str"
  | TypeFun ->
      "type"


let pp fmt = function
  | Builtin builtin ->
      F.pp_print_string fmt (builtin_type_to_string builtin)
  | Globals name ->
      F.fprintf fmt "Globals[%s]" name
  | Closure name ->
      F.fprintf fmt "Closure[%s]" name
  | BuiltinClosure builtin ->
      F.fprintf fmt "ClosureBuiltin[%s]" (builtin_closure_to_string builtin)
  | ClassCompanion {module_name; class_name} ->
      F.fprintf fmt "ClassCompanion[%s,%s]" module_name class_name
  | ClassInstance {module_name; class_name} ->
      F.fprintf fmt "ClassInstance[%s,%s]" module_name class_name
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

let split_module_attr = function
  (* TODO: we could simplify this function if we use [string list] instead of [string] in [t] *)
  | Closure name ->
      let open IOption.Let_syntax in
      let+ pos = String.index name '.' in
      let length = String.length name in
      let attribute_name = String.sub name ~pos:(pos + 1) ~len:(length - pos - 1) in
      let module_name = String.sub name ~pos:0 ~len:pos in
      (module_name, attribute_name)
  | Globals name ->
      let open IOption.Let_syntax in
      let+ last_pos = String.substr_index_all name ~may_overlap:false ~pattern:"::" |> List.last in
      let length = String.length name in
      let attribute_name = String.sub name ~pos:(last_pos + 2) ~len:(length - last_pos - 2) in
      let module_name = String.sub name ~pos:0 ~len:last_pos in
      (module_name, attribute_name)
  | _ ->
      None


let concatenate_package_name_and_file_name typename filename =
  match typename with
  | Globals module_name ->
      Some (Globals (Printf.sprintf "%s::%s" module_name filename))
  | _ ->
      None


let get_builtin_closure_from_builtin_type = function
  | PyObject | PyTuple | PyNone | PyBool ->
      None
  | PyDict ->
      Some DictFun
  | PyInt ->
      Some IntFun
  | PyString ->
      Some StrFun
