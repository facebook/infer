(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** The Smallfoot Intermediate Language: Annotations *)
open! IStd
module L = Logging
module F = Format

type parameters = string list [@@deriving compare]

(** Type to represent one @Annotation. *)
type t =
  { class_name: string  (** name of the annotation *)
  ; parameters: parameters  (** currently only one string parameter *) }
  [@@deriving compare]

let volatile = {class_name= "volatile"; parameters= []}

(** Pretty print an annotation. *)
let prefix = match Config.curr_language_is Config.Java with true -> "@" | false -> "_"

let pp fmt annotation = F.fprintf fmt "%s%s" prefix annotation.class_name

module Map = PrettyPrintable.MakePPMap (struct
  type nonrec t = t

  let compare = compare

  let pp = pp
end)

module Item = struct
  (* Don't use nonrec due to https://github.com/janestreet/ppx_compare/issues/2 *)
  (* type nonrec t = list (t, bool) [@@deriving compare]; *)
  (** Annotation for one item: a list of annotations with visibility. *)
  type _t = (t * bool) list [@@deriving compare]

  type t = _t [@@deriving compare]

  let equal = [%compare.equal : t]

  (** Pretty print an item annotation. *)
  let pp fmt ann =
    let pp fmt (a, _) = pp fmt a in
    F.fprintf fmt "<%a>" (Pp.seq pp) ann

  let to_string ann =
    let pp fmt = pp fmt ann in
    F.asprintf "%t" pp

  (** Empty item annotation. *)
  let empty = []

  (** Check if the item annodation is empty. *)
  let is_empty ia = List.is_empty ia
end

module Class = struct
  let objc_str = "ObjC-Class"

  let cpp_str = "Cpp-Class"

  let of_string class_string = [({class_name= class_string; parameters= []}, true)]

  let objc = of_string objc_str

  let cpp = of_string cpp_str
end

module Method = struct
  (** Annotation for a method: return value and list of parameters. *)
  type t = Item.t * Item.t list [@@deriving compare]

  (** Pretty print a method annotation. *)
  let pp s fmt (ia, ial) = F.fprintf fmt "%a %s(%a)" Item.pp ia s (Pp.seq Item.pp) ial

  (** Empty method annotation. *)
  let empty = ([], [])

  (** Check if the method annodation is empty. *)
  let is_empty (ia, ial) = List.for_all ~f:Item.is_empty (ia :: ial)
end
