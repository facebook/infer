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

(** annotation for fields/methods marked with the "volatile" keyword *)

val volatile : t

(** Pretty print an annotation. *)

val pp : F.formatter -> t -> unit

module Map : PrettyPrintable.PPMap with type key = t

module Item : sig
  (** Annotation for one item: a list of annotations with visibility. *)

  type nonrec t = (t * bool) list [@@deriving compare]

  val equal : t -> t -> bool

  (** Pretty print an item annotation. *)

  val pp : F.formatter -> t -> unit

  val to_string : t -> string

  (** Empty item annotation. *)

  val empty : t

  (** Check if the item annodation is empty. *)

  val is_empty : t -> bool
end

module Class : sig
  val objc : Item.t

  val cpp : Item.t
end

module Method : sig
  (** Annotation for a method: return value and list of parameters. *)

  type t = Item.t * Item.t list [@@deriving compare]

  (** Empty method annotation. *)

  val empty : t

  (** Check if the method annodation is empty. *)

  val is_empty : t -> bool

  (** Pretty print a method annotation. *)

  val pp : string -> F.formatter -> t -> unit
end
