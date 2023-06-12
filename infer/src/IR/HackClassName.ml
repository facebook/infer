(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = {namespace: string option; classname: string}
[@@deriving compare, equal, yojson_of, sexp, hash]

let make ?namespace classname = {namespace; classname}

let classname {classname} = classname

let components {namespace; classname} =
  match namespace with Some ns -> [ns; classname] | _ -> [classname]


let wildcard = make "?"

let pp fmt {namespace; classname} =
  match namespace with
  | Some ns ->
      F.fprintf fmt "%s\\%s" ns classname
  | _ ->
      F.fprintf fmt "%s" classname


let to_string = Pp.string_of_pp pp

let static_suffix = "$static"

let len_static_suffix = String.length static_suffix

let static_companion {namespace; classname} = {namespace; classname= classname ^ static_suffix}

let is_static {classname} = StringLabels.ends_with ~suffix:static_suffix classname

let static_companion_origin ({namespace; classname} as name) =
  let len_classname = String.length classname in
  if len_classname > len_static_suffix then
    let len = len_classname - len_static_suffix in
    {namespace; classname= StringLabels.sub ~pos:0 ~len classname}
  else name


let is_builtins {classname} = String.equal classname "$builtins"
