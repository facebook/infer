(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Annotations. *)

val suppressLint : string

type annotation =
  | Nullable
  | Present

(** Method signature with annotations. *)
type annotated_signature =
  { ret : Sil.item_annotation * Sil.typ; (** Annotated return type. *)
    params: (string * Sil.item_annotation * Sil.typ) list } (** Annotated parameters. *)

(** Check if the annotated signature is for a wrapper of an anonymous inner class method.
    These wrappers have the same name as the original method, every type is Object, and the parameters
    are called x0, x1, x2. *)
val annotated_signature_is_anonymous_inner_class_wrapper : annotated_signature -> Procname.t -> bool

(** Check if the given parameter has a Nullable annotation in the given signature *)
val param_is_nullable : Sil.pvar -> annotated_signature -> bool

(** Mark the annotated signature with the given annotation map. *)
val annotated_signature_mark :
  Procname.t -> annotation -> annotated_signature -> bool * bool list -> annotated_signature

(** Mark the return of the annotated signature with the given annotation. *)
val annotated_signature_mark_return :
  Procname.t -> annotation -> annotated_signature -> annotated_signature

(** Mark the return of the annotated signature @Strict. *)
val annotated_signature_mark_return_strict :
  Procname.t -> annotated_signature -> annotated_signature

val equal : annotated_signature -> annotated_signature -> bool

type get_method_annotation = Procname.t -> Cfg.Procdesc.t -> Sil.method_annotation

(** Get a method signature with annotations from a proc_attributes. *)
val get_annotated_signature : ProcAttributes.t -> annotated_signature

(** Return the type of the field [fn] and its annotation, None if [typ] has no field named [fn] *)
val get_field_type_and_annotation :
  Ident.fieldname -> Sil.typ -> (Sil.typ * Sil.item_annotation) option

val ia_contains : Sil.item_annotation -> string -> bool

val ia_has_annotation_with : Sil.item_annotation -> (Sil.annotation -> bool) -> bool

val ia_get_strict : Sil.item_annotation -> Sil.annotation option

val ia_is_initializer : Sil.item_annotation -> bool
val ia_is_inject : Sil.item_annotation -> bool
val ia_is_inject_view : Sil.item_annotation -> bool
val ia_is_mutable : Sil.item_annotation -> bool
val ia_is_nullable : Sil.item_annotation -> bool
val ia_is_nonnull : Sil.item_annotation -> bool
val ia_is_present : Sil.item_annotation -> bool
val ia_is_verify : Sil.item_annotation -> bool

val ia_iter : (Sil.annotation -> unit) -> Sil.item_annotation -> unit

val ma_contains : Sil.method_annotation -> string list -> bool

val ma_has_annotation_with : Sil.method_annotation -> (Sil.annotation -> bool) -> bool

val ma_iter : (Sil.annotation -> unit) -> Sil.method_annotation -> unit

(** Add the annotation to the item_annotation. *)
val mk_ia : annotation -> Sil.item_annotation -> Sil.item_annotation

val pp_annotated_signature : Procname.t -> Format.formatter -> annotated_signature -> unit

val visibleForTesting : string
