(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Module for Names and Identifiers *)

module L = Logging
module F = Format

type name = string

type fieldname =
  { fpos : int;
    fname : Mangled.t }

type kind = int

let kprimed = - 1
let knormal = 0
let kfootprint = 1

type t =
  { kind: int;
    name: name;
    stamp: int }

type _ident = t

(** {2 Comparison Functions} *)

let name_compare = string_compare

let fieldname_compare fn1 fn2 =
  let n = int_compare fn1.fpos fn2.fpos in
  if n <> 0 then n else Mangled.compare fn1.fname fn2.fname

let name_equal = string_equal

let kind_equal k1 k2 = k1 == k2

let compare i1 i2 =
  let n = i2.kind - i1.kind
  in if n <> 0 then n
  else
    let n = name_compare i1.name i2.name
    in if n <> 0 then n
    else int_compare i1.stamp i2.stamp

let equal i1 i2 =
  i1.stamp == i2.stamp && i1.kind == i2.kind && name_equal i1.name i2.name (* most unlikely first *)

let fieldname_equal fn1 fn2 =
  fieldname_compare fn1 fn2 = 0

let rec ident_list_compare il1 il2 = match il1, il2 with
  | [],[] -> 0
  | [], _ -> - 1
  | _,[] -> 1
  | i1:: l1, i2:: l2 ->
      let n = compare i1 i2
      in if n <> 0 then n
      else ident_list_compare l1 l2

let ident_list_equal ids1 ids2 = (ident_list_compare ids1 ids2 = 0)

(** {2 Set for identifiers} *)

module IdentSet = Set.Make
    (struct
      type t = _ident
      let compare = compare
    end)

module IdentMap = Map.Make
    (struct
      type t = _ident
      let compare = compare
    end)

module IdentHash =
  Hashtbl.Make(struct
    type t = _ident
    let equal = equal
    let hash (id: t) = Hashtbl.hash id
  end)

module FieldSet = Set.Make(struct
    type t = fieldname
    let compare = fieldname_compare
  end)

module FieldMap = Map.Make(struct
    type t = fieldname
    let compare = fieldname_compare
  end)

let idlist_to_idset ids =
  IList.fold_left (fun set id -> IdentSet.add id set) IdentSet.empty ids

(** {2 Conversion between Names and Strings} *)

module StringHash =
  Hashtbl.Make(struct
    type t = string
    let equal (s1: string) (s2: string) = s1 = s2
    let hash = Hashtbl.hash
  end)

module NameHash =
  Hashtbl.Make(struct
    type t = name
    let equal = name_equal
    let hash = Hashtbl.hash
  end)

(** Convert a string to a name *)
let string_to_name (s: string) =
  s

(** Create a field name with the given position (field number in the CSU) *)
let create_fieldname (n: Mangled.t) (position: int) =
  { fpos = position;
    fname = n }

(** Convert a name to a string. *)
let name_to_string (name: name) =
  name

(** Convert a fieldname to a string. *)
let fieldname_to_string fn = Mangled.to_string fn.fname

(** Convert a fieldname to a simplified string with at most one-level path. *)
let fieldname_to_simplified_string fn =
  let s = Mangled.to_string fn.fname in
  match string_split_character s '.' with
  | Some s1, s2 ->
      (match string_split_character s1 '.' with
       | Some _, s4 -> s4 ^ "." ^ s2
       | _ -> s)
  | _ -> s

(** Convert a fieldname to a flat string without path. *)
let fieldname_to_flat_string fn =
  let s = Mangled.to_string fn.fname in
  match string_split_character s '.' with
  | Some _, s2 -> s2
  | _ -> s

(** Returns the class part of the fieldname *)
let java_fieldname_get_class fn =
  let fn = fieldname_to_string fn in
  let ri = String.rindex fn '.' in
  String.sub fn 0 ri

(** Returns the last component of the fieldname *)
let java_fieldname_get_field fn =
  let fn = fieldname_to_string fn in
  let ri = 1 + String.rindex fn '.' in
  String.sub fn ri (String.length fn - ri)

(** Check if the field is the synthetic this$n of a nested class, used to access the n-th outher instance. *)
let java_fieldname_is_outer_instance fn =
  let fn = fieldname_to_string fn in
  let fn_len = String.length fn in
  let this = ".this$" in
  let this_len = String.length this in
  let zero_to_nine s = s >= "0" && s <= "9" in
  fn_len > this_len &&
  String.sub fn (fn_len - this_len - 1) this_len = this &&
  zero_to_nine (String.sub fn (fn_len - 1) 1)

let fieldname_offset fn = fn.fpos

(** hidded fieldname constant *)
let fieldname_hidden = create_fieldname (Mangled.from_string ".hidden") 0

(** hidded fieldname constant *)
let fieldname_is_hidden fn =
  fieldname_equal fn fieldname_hidden

(** {2 Functions and Hash Tables for Managing Stamps} *)

(** Set the stamp of the identifier *)
let set_stamp i stamp =
  { i with stamp = stamp }

(** Get the stamp of the identifier *)
let get_stamp i =
  i.stamp

module NameGenerator = struct
  type t = int NameHash.t

  let create () : t = NameHash.create 17

  (** Map from names to stamps. *)
  let name_map = ref (create ())

  let get_current () =
    !name_map

  let set_current map =
    name_map := map

  (** Reset the name generator *)
  let reset () =
    name_map := create ()

  (** Create a fresh identifier with the given kind and name. *)
  let create_fresh_ident kind name =
    let stamp =
      try
        let stamp = NameHash.find !name_map name in
        NameHash.replace !name_map name (stamp + 1);
        stamp + 1
      with Not_found ->
        NameHash.add !name_map name 0;
        0 in
    { kind = kind; name = name; stamp = stamp }

  (** Make sure that fresh ids after whis one will be with different stamps *)
  let update_name_hash name stamp =
    try
      let curr_stamp = NameHash.find !name_map name in
      let new_stamp = max curr_stamp stamp in
      NameHash.replace !name_map name new_stamp
    with Not_found ->
      NameHash.add !name_map name stamp
end

(** Name used for primed tmp variables *)
let name_primed = string_to_name "t"

(** Name used for normal tmp variables *)
let name_normal = string_to_name "n"

(** Name used for footprint tmp variables *)
let name_footprint = string_to_name "f"

(** Name used for spec variables *)
let name_spec = string_to_name "val"

(** Name used for the return variable *)
let name_return = Mangled.from_string "return"

(** Return the standard name for the given kind *)
let standard_name kind =
  if kind == knormal then name_normal
  else if kind == kfootprint then name_footprint
  else name_primed

(** Every identifier with a given stamp should unltimately be created using this function *)
let create_with_stamp kind name stamp =
  NameGenerator.update_name_hash name stamp;
  { kind = kind; name = name; stamp = stamp; }

(** Create an identifier with default name for the given kind *)
let create kind stamp =
  create_with_stamp kind (standard_name kind) stamp

(** Generate a normal identifier with the given name and stamp *)
let create_normal name stamp =
  create_with_stamp knormal name stamp

(** Generate a primed identifier with the given name and stamp *)
let create_primed name stamp =
  create_with_stamp kprimed name stamp

(** Generate a footprint identifier with the given name and stamp *)
let create_footprint name stamp =
  create_with_stamp kfootprint name stamp

(** {2 Functions for Identifiers} *)

(** Get a name of an identifier *)
let get_name id =
  id.name

let get_kind id =
  id.kind

let is_primed (id: t) =
  id.kind == kprimed

let is_normal (id: t) =
  id.kind == knormal

let is_footprint (id: t) =
  id.kind == kfootprint

(* timestamp for a path identifier *)
let path_ident_stamp = - 3

let is_path (id: t) =
  id.kind == knormal && id.stamp = path_ident_stamp

let make_unprimed id =
  if id.kind <> kprimed then assert false
  else { id with kind = knormal }

(** Update the name generator so that the given id's are not generated again *)
let update_name_generator ids =
  let upd id = ignore (create_with_stamp id.kind id.name id.stamp) in
  IList.iter upd ids

(** Create a fresh identifier with default name for the given kind. *)
let create_fresh kind =
  NameGenerator.create_fresh_ident kind (standard_name kind)

(** Generate a normal identifier whose name encodes a path given as a string. *)
let create_path pathstring =
  create_normal (string_to_name ("%path%" ^ pathstring)) path_ident_stamp

(** {2 Pretty Printing} *)

(** Convert an identifier to a string. *)
let to_string id =
  let base_name = name_to_string id.name in
  let prefix =
    if id.kind == kfootprint then "@"
    else if id.kind == knormal then ""
    else "_" in
  let suffix = "$" ^ (string_of_int id.stamp)
  in prefix ^ base_name ^ suffix

(** Pretty print a name. *)
let pp_name f name =
  F.fprintf f "%s" (name_to_string name)

let pp_fieldname f fn =
  (* only use for debug F.fprintf f "%a#%d" pp_name fn.fname fn.fpos *)
  Mangled.pp f fn.fname

(** Pretty print a name in latex. *)
let pp_name_latex style f (name: name) =
  Latex.pp_string style f (name_to_string name)

let pp_fieldname_latex style f fn =
  Latex.pp_string style f (Mangled.to_string fn.fname)

(** Pretty print an identifier. *)
let pp pe f id = match pe.pe_kind with
  | PP_TEXT | PP_HTML ->
      F.fprintf f "%s" (to_string id)
  | PP_LATEX ->
      let base_name = name_to_string id.name in
      let style =
        if id.kind = kfootprint then Latex.Boldface
        else if id.kind = knormal then Latex.Roman
        else Latex.Roman in
      F.fprintf f "%a_{%s}" (Latex.pp_string style) base_name (string_of_int id.stamp)

(** pretty printer for lists of identifiers *)
let pp_list pe = pp_comma_seq (pp pe)

(** pretty printer for lists of names *)
let pp_name_list = pp_comma_seq pp_name

(*
let make_ident_primed id =
  if id.kind == kprimed then assert false
  else { id with kind = kprimed }
*)
