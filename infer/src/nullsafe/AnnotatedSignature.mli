(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Method signature with annotations. *)

open! IStd

type t =
  { ret: Annot.Item.t * Typ.t  (** Annotated return type. *)
  ; params: (Mangled.t * Annot.Item.t * Typ.t) list  (** Annotated parameters. *) }
[@@deriving compare]

type annotation = Nullable | Present [@@deriving compare]

val param_has_annot : (Annot.Item.t -> bool) -> Pvar.t -> t -> bool
(** Check if the given parameter has an annotation in the given signature *)

val mark : Typ.Procname.t -> annotation -> t -> bool * bool list -> t
(** Mark the annotated signature with the given annotation map. *)

val get : ProcAttributes.t -> t
(** Get a method signature with annotations from a proc_attributes. *)

val mk_ia : annotation -> Annot.Item.t -> Annot.Item.t
(** Add the annotation to the item_annotation. *)

val pp : Typ.Procname.t -> Format.formatter -> t -> unit
(** Pretty print a method signature with annotations. *)
