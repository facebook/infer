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
module F = Format

type parameters = string list

(** Type to represent one @Annotation. *)
type t =
  { class_name: string  (** name of the annotation *)
  ; parameters: parameters  (** currently only one string parameter *) }
  [@@deriving compare]

val volatile : t
(** annotation for fields/methods marked with the "volatile" keyword *)

val pp : F.formatter -> t -> unit
(** Pretty print an annotation. *)

module Map : PrettyPrintable.PPMap with type key = t

module Item : sig
  (** Annotation for one item: a list of annotations with visibility. *)
  type nonrec t = (t * bool) list [@@deriving compare]

  val equal : t -> t -> bool

  val pp : F.formatter -> t -> unit
  (** Pretty print an item annotation. *)

  val to_string : t -> string

  val empty : t
  (** Empty item annotation. *)

  val is_empty : t -> bool
  (** Check if the item annodation is empty. *)
end

module Class : sig
  val objc : Item.t

  val cpp : Item.t
end

module Method : sig
  (** Annotation for a method: return value and list of parameters. *)
  type t = Item.t * Item.t list [@@deriving compare]

  val empty : t
  (** Empty method annotation. *)

  val is_empty : t -> bool
  (** Check if the method annodation is empty. *)

  val pp : string -> F.formatter -> t -> unit
  (** Pretty print a method annotation. *)
end
