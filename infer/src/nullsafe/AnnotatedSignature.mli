(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Method signature with annotations. *)

open! IStd

type t = {is_strict_mode: bool; ret: ret_signature; params: param_signature list}
[@@deriving compare]

and ret_signature = {ret_annotation_deprecated: Annot.Item.t; ret_annotated_type: AnnotatedType.t}
[@@deriving compare]

and param_signature =
  { param_annotation_deprecated: Annot.Item.t
  ; mangled: Mangled.t
  ; param_annotated_type: AnnotatedType.t }
[@@deriving compare]

val param_has_annot : (Annot.Item.t -> bool) -> Pvar.t -> t -> bool
(** Check if the given parameter has an annotation in the given signature *)

val set_modelled_nullability : Typ.Procname.t -> t -> bool * bool list -> t
(** Override nullability for a function signature given its modelled nullability (for ret value and params) *)

val get : is_strict_mode:bool -> ProcAttributes.t -> t
(** Get a method signature with annotations from a proc_attributes. *)

val pp : Typ.Procname.t -> Format.formatter -> t -> unit
(** Pretty print a method signature with annotations. *)
