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

(** Type to represent an [@Annotation] with potentially complex parameter values such as arrays or
    other annotations. *)
type t = {class_name: string  (** name of the annotation *); parameters: parameter list}
[@@deriving compare, equal, normalize]

and parameter = {name: string option; value: value} [@@deriving compare]

(** Type to represent possible annotation parameter values. Note that support for numeric parameters
    is missing for now due to an issue with [MaximumSharing] and [int64]. *)
and value =
  | Str of string
  | Bool of bool
  | Enum of {class_typ: Typ.t; value: string}
  | Array of value list
  | Class of Typ.t
  | Annot of t

val abstract : t
(** annotation for fields marked with the "abstract" keyword *)

val volatile : t
(** annotation for fields marked with the "volatile" keyword *)

val final : t
(** annotation for fields marked with the "final" keyword *)

val notnull : t
(** annotation for fields/params marked as "never null" *)

val has_matching_str_value : pred:(string -> bool) -> value -> bool
(** Check if annotation parameter value contains a string satisfying a predicate. For convenience it
    works both with raw [Str], [Str] inside [Array] and [Enum] values. *)

val find_parameter : t -> name:string -> value option

val pp : F.formatter -> t -> unit
(** Pretty print an annotation. *)

module Item : sig
  (** Annotation for one item: a list of annotations with visibility. *)
  type nonrec t = t list [@@deriving compare, equal, hash, normalize]

  val pp : F.formatter -> t -> unit
  (** Pretty print an item annotation. *)

  val empty : t
  (** Empty item annotation. *)

  val is_abstract : t -> bool
  (** Check if abstract annotation is included in. *)

  val is_empty : t -> bool

  val is_final : t -> bool
  (** Check if final annotation is included in. *)

  val is_notnull : t -> bool
  (** Check if notnull annotation is included in. *)
end

module Class : sig
  val objc : Item.t

  val cpp : Item.t
end
