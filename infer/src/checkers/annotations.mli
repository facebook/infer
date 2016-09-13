(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Annotations. *)

val suppressLint : string

val expensive : string
val performance_critical : string
val no_allocation : string
val on_bind : string
val suppress_warnings : string

type annotation =
  | Nullable
  | Present

(** Method signature with annotations. *)
type annotated_signature = {
  ret : Typ.item_annotation * Typ.t; (** Annotated return type. *)
  params: (Mangled.t * Typ.item_annotation * Typ.t) list (** Annotated parameters. *)
}

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

(** Return the annotations on the declaring class of [java_pname]. *)
val get_declaring_class_annotations : Procname.java -> Tenv.t -> Typ.item_annotation option

val nullable : string

(** Return true if [annot] ends with [ann_name] *)
val annot_ends_with : Typ.annotation -> string -> bool

(** Check if there is an annotation in [ia] which ends with the given name *)
val ia_ends_with : Typ.item_annotation -> string -> bool

val ia_contains : Typ.item_annotation -> string -> bool

val ia_has_annotation_with : Typ.item_annotation -> (Typ.annotation -> bool) -> bool

val ia_get_strict : Typ.item_annotation -> Typ.annotation option

val ia_is_false_on_null : Typ.item_annotation -> bool
val ia_is_initializer : Typ.item_annotation -> bool

(** Annotations for readonly injectors.
    The injector framework initializes the field but does not write null into it. *)
val ia_is_field_injector_readonly : Typ.item_annotation -> bool

(** Annotations for read-write injectors.
    The injector framework initializes the field and can write null into it. *)
val ia_is_field_injector_readwrite : Typ.item_annotation -> bool

val ia_is_mutable : Typ.item_annotation -> bool
val ia_is_nonnull : Typ.item_annotation -> bool
val ia_is_nullable : Typ.item_annotation -> bool
val ia_is_present : Typ.item_annotation -> bool
val ia_is_true_on_null : Typ.item_annotation -> bool
val ia_is_verify : Typ.item_annotation -> bool
val ia_is_expensive : Typ.item_annotation -> bool
val ia_is_performance_critical : Typ.item_annotation -> bool
val ia_is_no_allocation : Typ.item_annotation -> bool
val ia_is_ignore_allocations : Typ.item_annotation -> bool
val ia_is_suppress_warnings : Typ.item_annotation -> bool
val ia_is_privacy_source : Typ.item_annotation -> bool
val ia_is_privacy_sink : Typ.item_annotation -> bool
val ia_is_integrity_source : Typ.item_annotation -> bool
val ia_is_integrity_sink : Typ.item_annotation -> bool
val ia_is_guarded_by : Typ.item_annotation -> bool

val ia_iter : (Typ.annotation -> unit) -> Typ.item_annotation -> unit

val ma_has_annotation_with : Typ.method_annotation -> (Typ.annotation -> bool) -> bool

val pdesc_has_annot : Cfg.Procdesc.t -> string -> bool

(** Mark the return of the method_annotation with the given annotation. *)
val method_annotation_mark_return :
  annotation -> Typ.method_annotation -> Typ.method_annotation

(** Add the annotation to the item_annotation. *)
val mk_ia : annotation -> Typ.item_annotation -> Typ.item_annotation

val pp_annotated_signature : Procname.t -> Format.formatter -> annotated_signature -> unit

val visibleForTesting : string
val guarded_by : string
