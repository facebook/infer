(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Annotations *)
open! IStd

module F = Format

type parameters = string list [@@deriving compare]

(** Type to represent one @Annotation. *)
type t =
  { class_name: string  (** name of the annotation *)
  ; parameters: parameters  (** currently only one string parameter *) }
[@@deriving compare]

let volatile = {class_name= "volatile"; parameters= []}

let final = {class_name= "final"; parameters= []}

(** Pretty print an annotation. *)
let prefix = match Language.curr_language_is Java with true -> "@" | false -> "_"

let pp fmt annotation =
  F.fprintf fmt "%s%s%s" prefix annotation.class_name
    (String.concat ~sep:"," annotation.parameters)


module Item = struct
  (* Don't use nonrec due to https://github.com/janestreet/ppx_compare/issues/2 *)
  (* type nonrec t = list (t, bool) [@@deriving compare]; *)

  (** Annotation for one item: a list of annotations with visibility. *)
  type t_ = (t * bool) list [@@deriving compare]

  type t = t_ [@@deriving compare]

  (** Pretty print an item annotation. *)
  let pp fmt ann =
    let pp fmt (a, _) = pp fmt a in
    F.fprintf fmt "<%a>" (Pp.seq pp) ann


  (** Empty item annotation. *)
  let empty = []

  (** Check if the item annotation is empty. *)
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
  type t = {return: Item.t; params: Item.t list}

  (** Pretty print a method annotation. *)
  let pp s fmt {return; params} =
    F.fprintf fmt "%a %s(%a)" Item.pp return s (Pp.seq Item.pp) params


  (** Empty method annotation. *)
  let empty = {return= []; params= []}

  (** Check if the method annotation is empty. *)
  let is_empty {return; params} = Item.is_empty return && List.for_all ~f:Item.is_empty params
end
