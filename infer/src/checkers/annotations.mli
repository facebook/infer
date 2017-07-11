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

val any_thread : string

val expensive : string

val no_allocation : string

val nullable : string

val nonnull : string

val on_bind : string

val performance_critical : string

val present : string

val for_non_ui_thread : string

val for_ui_thread : string

val guarded_by : string

val strict : string

val suppress_lint : string

val thread_confined : string

val thread_safe : string

val ui_thread : string

val visibleForTesting : string

val annot_ends_with : Annot.t -> string -> bool
(** [annot_ends_with annot ann_name] returns true if the class name of [annot], without the package,
    is equal to [ann_name] *)

val ia_ends_with : Annot.Item.t -> string -> bool
(** Check if there is an annotation in [ia] which ends with the given name *)

val ia_contains : Annot.Item.t -> string -> bool

val ia_has_annotation_with : Annot.Item.t -> (Annot.t -> bool) -> bool

val ia_get_strict : Annot.Item.t -> Annot.t option

val ia_is_false_on_null : Annot.Item.t -> bool

val ia_is_initializer : Annot.Item.t -> bool

val ia_is_field_injector_readonly : Annot.Item.t -> bool
(** Annotations for readonly injectors.
    The injector framework initializes the field but does not write null into it. *)

val ia_is_field_injector_readwrite : Annot.Item.t -> bool
(** Annotations for read-write injectors.
    The injector framework initializes the field and can write null into it. *)

val ia_is_mutable : Annot.Item.t -> bool

val ia_is_nonnull : Annot.Item.t -> bool

val ia_is_nullable : Annot.Item.t -> bool

val ia_is_present : Annot.Item.t -> bool

val ia_is_true_on_null : Annot.Item.t -> bool

val ia_is_verify : Annot.Item.t -> bool

val ia_is_expensive : Annot.Item.t -> bool

val ia_is_functional : Annot.Item.t -> bool

val ia_is_performance_critical : Annot.Item.t -> bool

val ia_is_propagates_nullable : Annot.Item.t -> bool

val ia_is_no_allocation : Annot.Item.t -> bool

val ia_is_ignore_allocations : Annot.Item.t -> bool

val ia_is_inject : Annot.Item.t -> bool

val ia_is_suppress_lint : Annot.Item.t -> bool

val ia_is_on_event : Annot.Item.t -> bool

val ia_is_on_bind : Annot.Item.t -> bool

val ia_is_on_mount : Annot.Item.t -> bool

val ia_is_on_unbind : Annot.Item.t -> bool

val ia_is_on_unmount : Annot.Item.t -> bool

val ia_is_privacy_source : Annot.Item.t -> bool

val ia_is_privacy_sink : Annot.Item.t -> bool

val ia_is_integrity_source : Annot.Item.t -> bool

val ia_is_integrity_sink : Annot.Item.t -> bool

val ia_is_guarded_by : Annot.Item.t -> bool

val ia_is_not_thread_safe : Annot.Item.t -> bool

val ia_is_returns_ownership : Annot.Item.t -> bool

val ia_is_synchronized_collection : Annot.Item.t -> bool

val ia_is_thread_confined : Annot.Item.t -> bool

val ia_is_ui_thread : Annot.Item.t -> bool

val ia_is_volatile : Annot.Item.t -> bool

val pdesc_has_parameter_annot : Procdesc.t -> (Annot.Item.t -> bool) -> bool
(** return true if the given predicate evaluates to true on an annotation of one of [pdesc]'s
    parameters *)

val pdesc_get_return_annot : Procdesc.t -> Annot.Item.t
(** get the list of annotations on the return value of [pdesc] *)

val pdesc_has_return_annot : Procdesc.t -> (Annot.Item.t -> bool) -> bool
(** return true if the given predicate evaluates to true on the annotation of [pdesc]'s return
    value *)

val pname_has_return_annot :
  Typ.Procname.t -> attrs_of_pname:(Typ.Procname.t -> ProcAttributes.t option)
  -> (Annot.Item.t -> bool) -> bool
(** return true if the given predicate evaluates to true on the annotation of [pname]'s return
    value. the function [attrs_of_pname] should resolve the proc attributes of [pname].
    Specs.proc_resolve_attributes is a good choice for this resolution function. *)

val pdesc_return_annot_ends_with : Procdesc.t -> string -> bool
(** return true if [pdesc]'s return value is annotated with a value ending with the given string *)

val ma_has_annotation_with : Annot.Method.t -> (Annot.t -> bool) -> bool

val field_has_annot : Typ.Fieldname.t -> Typ.Struct.t -> (Annot.Item.t -> bool) -> bool

val struct_typ_has_annot : Typ.Struct.t -> (Annot.Item.t -> bool) -> bool
(** return true if the given predicate evaluates to true on some annotation of [struct_typ] *)
