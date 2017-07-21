(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for Names and Identifiers *)

open! IStd
module Hashtbl = Caml.Hashtbl
module L = Logging
module F = Format

module Name = struct
  type t = Primed | Normal | Footprint | Spec | FromString of string [@@deriving compare]

  let primed = "t"

  let normal = "n"

  let footprint = "f"

  let spec = "val"

  let from_string s = FromString s

  let to_string = function
    | Primed
     -> primed
    | Normal
     -> normal
    | Footprint
     -> footprint
    | Spec
     -> spec
    | FromString s
     -> s
end

type name = Name.t [@@deriving compare]

let name_spec = Name.Spec

let name_primed = Name.Primed

let equal_name = [%compare.equal : name]

type kind =
  | KNone
      (** special kind of "null ident" (basically, a more compact way of implementing an ident option).
      useful for situations when an instruction requires an id, but no one should read the result. *)
  | KFootprint
  | KNormal
  | KPrimed
  [@@deriving compare]

let kfootprint = KFootprint

let knormal = KNormal

let kprimed = KPrimed

let equal_kind = [%compare.equal : kind]

(* timestamp for a path identifier *)
let path_ident_stamp = -3

type t = {kind: kind; name: Name.t; stamp: int} [@@deriving compare]

(* most unlikely first *)
let equal i1 i2 =
  Int.equal i1.stamp i2.stamp && equal_kind i1.kind i2.kind && equal_name i1.name i2.name

(** {2 Set for identifiers} *)
module IdentSet = Caml.Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

module IdentMap = Caml.Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

module IdentHash = Hashtbl.Make (struct
  type nonrec t = t

  let equal = equal

  let hash (id: t) = Hashtbl.hash id
end)

let idlist_to_idset ids = List.fold ~f:(fun set id -> IdentSet.add id set) ~init:IdentSet.empty ids

(** {2 Conversion between Names and Strings} *)
module NameHash = Hashtbl.Make (struct
  type t = name

  let equal = equal_name

  let hash = Hashtbl.hash
end)

(** Convert a string to a name *)
let string_to_name = Name.from_string

(** Convert a name to a string. *)
let name_to_string = Name.to_string

(** {2 Functions and Hash Tables for Managing Stamps} *)

(** Set the stamp of the identifier *)
let set_stamp i stamp = {i with stamp}

(** Get the stamp of the identifier *)
let get_stamp i = i.stamp

module NameGenerator = struct
  type t = int NameHash.t

  let create () : t = NameHash.create 17

  (** Map from names to stamps. *)
  let name_map = ref (create ())

  let get_current () = !name_map

  let set_current map = name_map := map

  (** Reset the name generator *)
  let reset () = name_map := create ()

  (** Create a fresh identifier with the given kind and name. *)
  let create_fresh_ident kind name =
    let stamp =
      try
        let stamp = NameHash.find !name_map name in
        NameHash.replace !name_map name (stamp + 1) ;
        stamp + 1
      with Not_found ->
        NameHash.add !name_map name 0 ;
        0
    in
    {kind; name; stamp}

  (** Make sure that fresh ids after whis one will be with different stamps *)
  let update_name_hash name stamp =
    try
      let curr_stamp = NameHash.find !name_map name in
      let new_stamp = max curr_stamp stamp in
      NameHash.replace !name_map name new_stamp
    with Not_found -> NameHash.add !name_map name stamp
end

(** Name used for the return variable *)
let name_return = Mangled.from_string "return"

(** Return the standard name for the given kind *)
let standard_name kind =
  if equal_kind kind KNormal || equal_kind kind KNone then Name.Normal
  else if equal_kind kind KFootprint then Name.Footprint
  else Name.Primed

(** Every identifier with a given stamp should unltimately be created using this function *)
let create_with_stamp kind name stamp =
  NameGenerator.update_name_hash name stamp ; {kind; name; stamp}

(** Create an identifier with default name for the given kind *)
let create kind stamp = create_with_stamp kind (standard_name kind) stamp

(** Generate a normal identifier with the given name and stamp *)
let create_normal name stamp = create_with_stamp KNormal name stamp

(** Create a fresh identifier with default name for the given kind. *)
let create_fresh kind = NameGenerator.create_fresh_ident kind (standard_name kind)

let create_none () = create_fresh KNone

(** Generate a primed identifier with the given name and stamp *)
let create_primed name stamp = create_with_stamp KPrimed name stamp

(** Generate a footprint identifier with the given name and stamp *)
let create_footprint name stamp = create_with_stamp KFootprint name stamp

(** {2 Functions for Identifiers} *)

(** Get a name of an identifier *)
let get_name id = id.name

let has_kind id kind = equal_kind id.kind kind

let is_primed (id: t) = has_kind id KPrimed

let is_normal (id: t) = has_kind id KNormal || has_kind id KNone

let is_footprint (id: t) = has_kind id KFootprint

let is_none (id: t) = has_kind id KNone

let is_path (id: t) = has_kind id KNormal && Int.equal id.stamp path_ident_stamp

let make_unprimed id =
  if not (has_kind id KPrimed) then assert false
  else if has_kind id KNone then {id with kind= KNone}
  else {id with kind= KNormal}

(** Update the name generator so that the given id's are not generated again *)
let update_name_generator ids =
  let upd id = ignore (create_with_stamp id.kind id.name id.stamp) in
  List.iter ~f:upd ids

(** Generate a normal identifier whose name encodes a path given as a string. *)
let create_path pathstring =
  create_normal (string_to_name ("%path%" ^ pathstring)) path_ident_stamp

(** {2 Pretty Printing} *)

(** Convert an identifier to a string. *)
let to_string id =
  if has_kind id KNone then "_"
  else
    let base_name = name_to_string id.name in
    let prefix = if has_kind id KFootprint then "@" else if has_kind id KNormal then "" else "_" in
    let suffix = "$" ^ string_of_int id.stamp in
    prefix ^ base_name ^ suffix

(** Pretty print a name. *)
let pp_name f name = F.fprintf f "%s" (name_to_string name)

(** Pretty print a name in latex. *)
let pp_name_latex style f (name: name) = Latex.pp_string style f (name_to_string name)

(** Pretty print an identifier. *)
let pp pe f id =
  match pe.Pp.kind with
  | TEXT | HTML
   -> F.fprintf f "%s" (to_string id)
  | LATEX
   -> let base_name = name_to_string id.name in
      let style =
        if has_kind id KFootprint then Latex.Boldface
        else if has_kind id KNormal then Latex.Roman
        else Latex.Roman
      in
      F.fprintf f "%a_{%s}" (Latex.pp_string style) base_name (string_of_int id.stamp)

(** pretty printer for lists of identifiers *)
let pp_list pe = Pp.comma_seq (pp pe)

(** pretty printer for lists of names *)
let pp_name_list = Pp.comma_seq pp_name
