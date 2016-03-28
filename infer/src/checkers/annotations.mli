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

val expensive : string
val performance_critical : string
val no_allocation : string
val suppress_warnings : string

type annotation =
  | Nullable
  | Present

(** Method signature with annotations. *)
type annotated_signature =
  { ret : Sil.item_annotation * Sil.typ; (** Annotated return type. *)
    params: (Mangled.t * Sil.item_annotation * Sil.typ) list } (** Annotated parameters. *)

(** Check if the annotated signature is for a wrapper of an anonymous inner class method.
    These wrappers have the same name as the original method, every type is Object, and the parameters
    are called x0, x1, x2. *)
val annotated_signature_is_anonymous_inner_class_wrapper : annotated_signature -> Procname.t -> bool

(** Check if the given parameter has a Nullable annotation in the given signature *)
val param_is_nullable : Pvar.t -> annotated_signature -> bool

(** Mark the annotated signature with the given annotation map. *)
val annotated_signature_mark :
  Procname.t -> annotation -> annotated_signature -> bool * bool list -> annotated_signature

(** Mark the return of the annotated signature with the given annotation. *)
val annotated_signature_mark_return :
  annotation -> annotated_signature -> annotated_signature

(** Mark the return of the annotated signature @Strict. *)
val annotated_signature_mark_return_strict :
  annotated_signature -> annotated_signature

val equal : annotated_signature -> annotated_signature -> bool

(** Get a method signature with annotations from a proc_attributes. *)
val get_annotated_signature : ProcAttributes.t -> annotated_signature

(** Return the type of the field [fn] and its annotation, None if [typ] has no field named [fn] *)
val get_field_type_and_annotation :
  Ident.fieldname -> Sil.typ -> (Sil.typ * Sil.item_annotation) option

val nullable : string

val ia_contains : Sil.item_annotation -> string -> bool

val ia_has_annotation_with : Sil.item_annotation -> (Sil.annotation -> bool) -> bool

val ia_get_strict : Sil.item_annotation -> Sil.annotation option

val ia_is_false_on_null : Sil.item_annotation -> bool
val ia_is_initializer : Sil.item_annotation -> bool

(** Annotations for readonly injectors.
    The injector framework initializes the field but does not write null into it. *)
val ia_is_field_injector_readonly : Sil.item_annotation -> bool

(** Annotations for read-write injectors.
    The injector framework initializes the field and can write null into it. *)
val ia_is_field_injector_readwrite : Sil.item_annotation -> bool

val ia_is_mutable : Sil.item_annotation -> bool
val ia_is_nonnull : Sil.item_annotation -> bool
val ia_is_nullable : Sil.item_annotation -> bool
val ia_is_present : Sil.item_annotation -> bool
val ia_is_true_on_null : Sil.item_annotation -> bool
val ia_is_verify : Sil.item_annotation -> bool
val ia_is_expensive : Sil.item_annotation -> bool
val ia_is_performance_critical : Sil.item_annotation -> bool
val ia_is_no_allocation : Sil.item_annotation -> bool
val ia_is_ignore_allocations : Sil.item_annotation -> bool
val ia_is_suppress_warnings : Sil.item_annotation -> bool

val ia_iter : (Sil.annotation -> unit) -> Sil.item_annotation -> unit

val ma_contains : Sil.method_annotation -> string list -> bool

val ma_has_annotation_with : Sil.method_annotation -> (Sil.annotation -> bool) -> bool

val ma_iter : (Sil.annotation -> unit) -> Sil.method_annotation -> unit

(** Mark the return of the method_annotation with the given annotation. *)
val method_annotation_mark_return :
  annotation -> Sil.method_annotation -> Sil.method_annotation

(** Add the annotation to the item_annotation. *)
val mk_ia : annotation -> Sil.item_annotation -> Sil.item_annotation

val pp_annotated_signature : Procname.t -> Format.formatter -> annotated_signature -> unit

val visibleForTesting : string
