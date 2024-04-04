(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Module for Mangled Names *)

open! IStd
module F = Format

type t = {plain: string; mangled: string option}
[@@deriving compare, yojson_of, equal, sexp, hash, normalize]

(** Convert a string to a mangled name *)
let from_string (s : string) = {plain= s; mangled= None}

(** Create a mangled name from a plain and mangled string *)
let mangled (plain : string) (mangled : string) =
  {plain; mangled= Some (plain ^ "{" ^ mangled ^ "}")}


(** Convert a mangled name to a string *)
let to_string (pn : t) = pn.plain

(** Convert a full mangled name to a string *)
let to_string_full (pn : t) =
  match pn.mangled with Some mangled -> pn.plain ^ "{" ^ mangled ^ "}" | None -> pn.plain


(** Pretty print a mangled name *)
let pp f pn = F.pp_print_string f (to_string pn)

let this = from_string "this"

let is_this = function {plain= "this" | "$this"} -> true | _ -> false

let self = from_string "self"

let is_self = function {plain= "self"} -> true | _ -> false

let is_artificial = function {plain= "__promise" | "__coro_frame"} -> true | _ -> false

let return_param = from_string "__return_param"

let is_return_param = function {plain= "__return_param"} -> true | _ -> false

let is_underscore = function {plain= "_"} -> true | _ -> false

module Set = PrettyPrintable.MakePPSet (struct
  type nonrec t = t [@@deriving compare]

  let pp = pp
end)

module Map = PrettyPrintable.MakePPMap (struct
  type nonrec t = t [@@deriving compare]

  let pp = pp
end)
