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
