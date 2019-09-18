(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

val param_has_annot : (Annot.Item.t -> bool) -> Pvar.t -> t -> bool
(** Check if the given parameter has an annotation in the given signature *)

val mark_nullability : Typ.Procname.t -> t -> bool * bool list -> t
(** Mark the annotated signature with the given nullability of the ret value and given nullability of the params ). *)

val get : ProcAttributes.t -> t
(** Get a method signature with annotations from a proc_attributes. *)

val pp : Typ.Procname.t -> Format.formatter -> t -> unit
(** Pretty print a method signature with annotations. *)
