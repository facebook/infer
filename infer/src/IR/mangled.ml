(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for Mangled Names *)

module F = Format

type t =
  { plain: string;
    mangled: string option }

let mangled_compare so1 so2 = match so1, so2 with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some s1, Some s2 -> string_compare s1 s2

let compare pn1 pn2 =
  let n = string_compare pn1.plain pn2.plain in
  if n <> 0 then n else mangled_compare pn1.mangled pn2.mangled

let equal pn1 pn2 =
  compare pn1 pn2 = 0

(** Convert a string to a mangled name *)
let from_string (s: string) =
  { plain = s;
    mangled = None }

(** Create a mangled name from a plain and mangled string *)
let mangled (plain: string) (mangled: string) =
  { plain = plain;
    mangled = Some (plain ^ "{" ^ mangled ^ "}") }

(** Convert a mangled name to a string *)
let to_string (pn: t) =
  pn.plain

(** Convert a full mangled name to a string *)
let to_string_full (pn: t) =
  match pn.mangled with
  | Some mangled -> pn.plain ^ "{" ^ mangled ^ "}"
  | None -> pn.plain

(** Get mangled string if given *)
let get_mangled pn = match pn.mangled with
  | Some s -> s
  | None -> pn.plain

(** Create a mangled type name from a package name and a class name *)
let from_package_class package_name class_name =
  if package_name = "" then from_string class_name
  else from_string (package_name ^ "." ^ class_name)

(** Pretty print a mangled name *)
let pp f pn =
  F.fprintf f "%s" (to_string pn)


type mangled_t = t
module MangledSet = Set.Make
    (struct
      type t = mangled_t
      let compare = compare
    end)
