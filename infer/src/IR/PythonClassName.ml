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
type t = {classname: string} [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let make classname = {classname}

let classname {classname} = classname

let components {classname} = [classname]

let wildcard = make "?"

let pp fmt {classname} = F.fprintf fmt "%s" classname

let to_string = Pp.string_of_pp pp

let globals_prefix = "PyGlobals::"

let is_module {classname} = String.is_prefix classname ~prefix:globals_prefix

let get_module_name {classname} = String.chop_prefix classname ~prefix:globals_prefix

let is_final name = is_module name

let module_attribute_prefix = "PyModuleAttr::"

let is_module_attribute {classname} = String.is_prefix classname ~prefix:module_attribute_prefix

let get_module_attribute_infos {classname} =
  let open IOption.Let_syntax in
  let* last_pos = String.substr_index_all classname ~may_overlap:false ~pattern:"::" |> List.last in
  let length = String.length classname in
  let attribute_name = String.sub classname ~pos:(last_pos + 2) ~len:(length - last_pos - 2) in
  let+ module_name =
    String.sub classname ~pos:0 ~len:last_pos |> String.chop_prefix ~prefix:module_attribute_prefix
  in
  let classname = globals_prefix ^ module_name in
  ({classname}, attribute_name)
