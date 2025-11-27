(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

type t = {mangled: string; plain_name: string option [@ignore]}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let classname {mangled; plain_name} = Option.value plain_name ~default:mangled

let pp fmt t = F.pp_print_string fmt (classname t)

let pp_full fmt {plain_name; mangled} =
  match plain_name with
  | Some plain_name ->
      F.fprintf fmt "%s [%s]" plain_name mangled
  | None ->
      F.pp_print_string fmt mangled


let to_string = Pp.string_of_pp pp

let of_string ?plain_name mangled =
  match plain_name with
  | Some "" ->
      L.die InternalError "Swift classname was given empty plain name for %s" mangled
  | _ ->
      {mangled; plain_name}
