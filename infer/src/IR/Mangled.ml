(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Module for Mangled Names *)

open! IStd
module F = Format

type t = {plain: string; mangled: string option} [@@deriving compare]

let equal = [%compare.equal: t]

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

let is_this = function {plain= "this"} -> true | _ -> false

let self = from_string "self"

let is_self = function {plain= "self"} -> true | _ -> false

module Set = Caml.Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Map = Caml.Map.Make (struct
  type nonrec t = t

  let compare = compare
end)
