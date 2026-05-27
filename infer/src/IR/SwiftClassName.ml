(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

type t = {mangled: string; plain_name: string option [@ignore]; args: t list}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let mangled {mangled} = mangled

let args {args} = args

let rec pp_plain_name fmt {plain_name; mangled; args} =
  F.pp_print_string fmt (Option.value plain_name ~default:mangled) ;
  pp_args fmt args


and pp_args fmt = function
  | [] ->
      ()
  | args ->
      F.fprintf fmt "<%a>" (Pp.comma_seq pp_plain_name) args


let pp fmt t = pp_plain_name fmt t

let to_string = Pp.string_of_pp pp

let of_string ?plain_name ?(args = []) mangled =
  match plain_name with
  | Some "" ->
      L.die InternalError "Swift classname was given empty plain name for %s" mangled
  | _ ->
      {mangled; plain_name; args}


let swift_alloc_unknown_type = of_string "__infer_swift_alloc_unknown_type"
