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

let static_suffix = "$static"

let len_static_suffix = String.length static_suffix

let static_companion {classname} = {classname= classname ^ static_suffix}

let is_static {classname} = StringLabels.ends_with ~suffix:static_suffix classname

let static_companion_origin ({classname} as name) =
  let len_classname = String.length classname in
  if len_classname > len_static_suffix then
    let len = len_classname - len_static_suffix in
    {classname= StringLabels.sub ~pos:0 ~len classname}
  else name
