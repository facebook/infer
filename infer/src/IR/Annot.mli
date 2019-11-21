(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The Smallfoot Intermediate Language: Annotations *)

open! IStd
module F = Format

type parameter = {name: string option; value: string}

type parameters = parameter list

(** Type to represent one @Annotation. *)
type t =
  { class_name: string  (** name of the annotation *)
  ; parameters: parameters  (** currently only one string parameter *) }
[@@deriving compare]

val volatile : t
(** annotation for fields marked with the "volatile" keyword *)

val final : t
(** annotation for fields marked with the "final" keyword *)

val pp : F.formatter -> t -> unit
(** Pretty print an annotation. *)

module Item : sig
  (** Annotation for one item: a list of annotations with visibility. *)
  type nonrec t = (t * bool) list [@@deriving compare]

  val pp : F.formatter -> t -> unit
  (** Pretty print an item annotation. *)

  val empty : t
  (** Empty item annotation. *)

  val is_final : t -> bool
  (** Check if final annotation is included in. *)
end

module Class : sig
  val objc : Item.t

  val cpp : Item.t
end

module Method : sig
  (** Annotation for a method: return value and list of parameters. *)
  type t = {return: Item.t; params: Item.t list}

  val empty : t
  (** Empty method annotation. *)

  val is_empty : t -> bool
  (** Check if the method annotation is empty. *)

  val pp : string -> F.formatter -> t -> unit
  (** Pretty print a method annotation. *)
end
