(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Annotations. *)

val suppressLint : string

val expensive : string
val performance_critical : string
val no_allocation : string
val on_bind : string

type annotation =
  | Nullable
  | Present
[@@deriving compare]

(** Method signature with annotations. *)
type annotated_signature = {
  ret : Annot.Item.t * Typ.t; (** Annotated return type. *)
  params: (Mangled.t * Annot.Item.t * Typ.t) list (** Annotated parameters. *)
} [@@deriving compare]

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

(** Get a method signature with annotations from a proc_attributes. *)
val get_annotated_signature : ProcAttributes.t -> annotated_signature

val nullable : string

(** [annot_ends_with annot ann_name] returns true if the class name of [annot], without the package,
    is equal to [ann_name] *)
val annot_ends_with : Annot.t -> string -> bool

(** Check if there is an annotation in [ia] which ends with the given name *)
val ia_ends_with : Annot.Item.t -> string -> bool

val ia_contains : Annot.Item.t -> string -> bool

val ia_has_annotation_with : Annot.Item.t -> (Annot.t -> bool) -> bool

val ia_get_strict : Annot.Item.t -> Annot.t option

val ia_is_false_on_null : Annot.Item.t -> bool
val ia_is_initializer : Annot.Item.t -> bool

(** Annotations for readonly injectors.
    The injector framework initializes the field but does not write null into it. *)
val ia_is_field_injector_readonly : Annot.Item.t -> bool

(** Annotations for read-write injectors.
    The injector framework initializes the field and can write null into it. *)
val ia_is_field_injector_readwrite : Annot.Item.t -> bool

val ia_is_mutable : Annot.Item.t -> bool
val ia_is_nonnull : Annot.Item.t -> bool
val ia_is_nullable : Annot.Item.t -> bool
val ia_is_present : Annot.Item.t -> bool
val ia_is_true_on_null : Annot.Item.t -> bool
val ia_is_verify : Annot.Item.t -> bool
val ia_is_expensive : Annot.Item.t -> bool
val ia_is_performance_critical : Annot.Item.t -> bool
val ia_is_no_allocation : Annot.Item.t -> bool
val ia_is_ignore_allocations : Annot.Item.t -> bool
val ia_is_suppress_lint : Annot.Item.t -> bool
val ia_is_privacy_source : Annot.Item.t -> bool
val ia_is_privacy_sink : Annot.Item.t -> bool
val ia_is_integrity_source : Annot.Item.t -> bool
val ia_is_integrity_sink : Annot.Item.t -> bool
val ia_is_guarded_by : Annot.Item.t -> bool
val ia_is_not_thread_safe : Annot.Item.t -> bool
val ia_is_thread_safe : Annot.Item.t -> bool

val ia_iter : (Annot.t -> unit) -> Annot.Item.t -> unit

val ma_has_annotation_with : Annot.Method.t -> (Annot.t -> bool) -> bool

val pdesc_has_annot : Procdesc.t -> string -> bool

(** Mark the return of the method_annotation with the given annotation. *)
val method_annotation_mark_return :
  annotation -> Annot.Method.t -> Annot.Method.t

(** Add the annotation to the item_annotation. *)
val mk_ia : annotation -> Annot.Item.t -> Annot.Item.t

val pp_annotated_signature : Procname.t -> Format.formatter -> annotated_signature -> unit

val visibleForTesting : string
val guarded_by : string
val suppress_lint : string
