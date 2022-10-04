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


let pp fmt {namespace; classname} =
  match namespace with
  | Some ns ->
      F.fprintf fmt "%s\\%s" ns classname
  | _ ->
      F.fprintf fmt "%s" classname


let to_string = Pp.string_of_pp pp
