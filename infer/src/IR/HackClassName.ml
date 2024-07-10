(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = {namespace: string option; classname: string}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

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

let is_static_companion {classname} = StringLabels.ends_with ~suffix:static_suffix classname

let static_companion ({namespace; classname} as typ) =
  let classname = if is_static_companion typ then classname else classname ^ static_suffix in
  {namespace; classname}


let static_companion_origin ({namespace; classname} as name) =
  let len_classname = String.length classname in
  if len_classname > len_static_suffix then
    let len = len_classname - len_static_suffix in
    {namespace; classname= StringLabels.sub ~pos:0 ~len classname}
  else name


let is_builtins {classname} = String.equal classname "$builtins"

let is_generated_curry {classname} = StringLabels.ends_with ~suffix:"$curry" classname

let extract_curry_info {classname} =
  (* classname should be of the form 'TYPENAME$static_SHORTPROCNAME$curry'
     for example: 'FunctionReference::Main$static_foo$curry' *)
  let open IOption.Let_syntax in
  let sep1 = "$static_" in
  let* sep1_start = String.substr_index classname ~pattern:sep1 in
  let sep2 = "$curry" in
  let* sep2_start = String.substr_index classname ~pattern:sep2 in
  let typename = String.sub classname ~pos:0 ~len:sep1_start ^ "$static" |> make in
  let sep1_end = sep1_start + String.length sep1 in
  let shortprocname = String.sub classname ~pos:sep1_end ~len:(sep2_start - sep1_end) in
  Some (typename, shortprocname)
