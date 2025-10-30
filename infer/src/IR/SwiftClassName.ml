(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type t = {classname: string; plain_name: string option [@ignore]}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let pp fmt {plain_name; classname} =
  match plain_name with
  | Some plain_name ->
      F.fprintf fmt "%s" plain_name
  | None ->
      F.fprintf fmt "%s" classname


let classname {classname} = classname

let to_string = Pp.string_of_pp pp

let of_string ?plain_name classname = {classname; plain_name}
