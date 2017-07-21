(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

(** Annotations. *)

let any_thread = "AnyThread"

let bind = "Bind"

let bind_view = "BindView"

let bind_array = "BindArray"

let bind_bitmap = "BindBitmap"

let bind_drawable = "BindDrawable"

let bind_string = "BindString"

let camel_nonnull = "NonNull"

let expensive = "Expensive"

let false_on_null = "FalseOnNull"

let for_ui_thread = "ForUiThread"

let for_non_ui_thread = "ForNonUiThread"

let functional = "Functional"

let guarded_by = "GuardedBy"

let ignore_allocations = "IgnoreAllocations"

let initializer_ = "Initializer"

let inject = "Inject"

let inject_view = "InjectView"

let integrity_source = "IntegritySource"

let integrity_sink = "IntegritySink"

let mutable_ = "Mutable"

let nonnull = "Nonnull"

let no_allocation = "NoAllocation"

let nullable = "Nullable"

let on_bind = "OnBind"

let on_event = "OnEvent"

let on_mount = "OnMount"

let on_unbind = "OnUnbind"

let on_unmount = "OnUnmount"

let notnull = "NotNull"

let not_thread_safe = "NotThreadSafe"

let performance_critical = "PerformanceCritical"

let present = "Present"

let privacy_source = "PrivacySource"

let privacy_sink = "PrivacySink"

let propagates_nullable = "PropagatesNullable"

let returns_ownership = "ReturnsOwnership"

let synchronized_collection = "SynchronizedCollection"

let strict = "com.facebook.infer.annotation.Strict"

let suppress_lint = "SuppressLint"

let suppress_view_nullability = "SuppressViewNullability"

let thread_confined = "ThreadConfined"

let thread_safe = "ThreadSafe"

let true_on_null = "TrueOnNull"

let ui_thread = "UiThread"

let verify_annotation = "com.facebook.infer.annotation.Verify"

let visibleForTesting = "VisibleForTesting"

let volatile = "volatile"

let ia_has_annotation_with (ia: Annot.Item.t) (predicate: Annot.t -> bool) : bool =
  List.exists ~f:(fun (a, _) -> predicate a) ia

let ma_has_annotation_with ((ia, ial): Annot.Method.t) (predicate: Annot.t -> bool) : bool =
  let has_annot a = ia_has_annotation_with a predicate in
  has_annot ia || List.exists ~f:has_annot ial

(** [annot_ends_with annot ann_name] returns true if the class name of [annot], without the package,
    is equal to [ann_name] *)
let annot_ends_with annot ann_name =
  match String.rsplit2 annot.Annot.class_name ~on:'.' with
  | None
   -> String.equal annot.Annot.class_name ann_name
  | Some (_, annot_class_name)
   -> String.equal annot_class_name ann_name

let class_name_matches s ((annot: Annot.t), _) = String.equal s annot.class_name

let ia_ends_with ia ann_name = List.exists ~f:(fun (a, _) -> annot_ends_with a ann_name) ia

let ia_contains ia ann_name = List.exists ~f:(class_name_matches ann_name) ia

let ia_get ia ann_name = List.find ~f:(class_name_matches ann_name) ia |> Option.map ~f:fst

let pdesc_has_parameter_annot pdesc predicate =
  let _, param_annotations = (Procdesc.get_attributes pdesc).ProcAttributes.method_annotation in
  List.exists ~f:predicate param_annotations

let pdesc_get_return_annot pdesc =
  fst (Procdesc.get_attributes pdesc).ProcAttributes.method_annotation

let pdesc_has_return_annot pdesc predicate = predicate (pdesc_get_return_annot pdesc)

let pdesc_return_annot_ends_with pdesc annot =
  pdesc_has_return_annot pdesc (fun ia -> ia_ends_with ia annot)

(* note: we would use Specs.proc_resolve_attributes directly instead of requiring [attrs_of_pname],
   but doing so creates a circular dependency *)
let pname_has_return_annot pname ~attrs_of_pname predicate =
  match attrs_of_pname pname with
  | Some attributes
   -> predicate (fst attributes.ProcAttributes.method_annotation)
  | None
   -> false

let field_has_annot fieldname (struct_typ: Typ.Struct.t) predicate =
  let fld_has_taint_annot (fname, _, annot) =
    Typ.Fieldname.equal fieldname fname && predicate annot
  in
  List.exists ~f:fld_has_taint_annot struct_typ.fields
  || List.exists ~f:fld_has_taint_annot struct_typ.statics

let struct_typ_has_annot (struct_typ: Typ.Struct.t) predicate = predicate struct_typ.annots

let ia_is_not_thread_safe ia = ia_ends_with ia not_thread_safe

let ia_is_propagates_nullable ia = ia_ends_with ia propagates_nullable

let ia_is_nullable ia = ia_ends_with ia nullable || ia_is_propagates_nullable ia

let ia_is_present ia = ia_ends_with ia present

let ia_is_nonnull ia = List.exists ~f:(ia_ends_with ia) [nonnull; notnull; camel_nonnull]

let ia_is_false_on_null ia = ia_ends_with ia false_on_null

let ia_is_returns_ownership ia = ia_ends_with ia returns_ownership

let ia_is_synchronized_collection ia = ia_ends_with ia synchronized_collection

let ia_is_thread_safe ia = ia_ends_with ia thread_safe

let ia_is_true_on_null ia = ia_ends_with ia true_on_null

let ia_is_initializer ia = ia_ends_with ia initializer_

let ia_is_volatile ia = ia_contains ia volatile

let field_injector_readwrite_list =
  [ inject_view
  ; bind
  ; bind_view
  ; bind_array
  ; bind_bitmap
  ; bind_drawable
  ; bind_string
  ; suppress_view_nullability ]

let field_injector_readonly_list = inject :: field_injector_readwrite_list

(** Annotations for readonly injectors.
    The injector framework initializes the field but does not write null into it. *)
let ia_is_field_injector_readonly ia =
  List.exists ~f:(ia_ends_with ia) field_injector_readonly_list

(** Annotations for read-write injectors.
    The injector framework initializes the field and can write null into it. *)
let ia_is_field_injector_readwrite ia =
  List.exists ~f:(ia_ends_with ia) field_injector_readwrite_list

let ia_is_mutable ia = ia_ends_with ia mutable_

let ia_get_strict ia = ia_get ia strict

let ia_is_verify ia = ia_contains ia verify_annotation

let ia_is_expensive ia = ia_ends_with ia expensive

let ia_is_functional ia = ia_ends_with ia functional

let ia_is_performance_critical ia = ia_ends_with ia performance_critical

let ia_is_no_allocation ia = ia_ends_with ia no_allocation

let ia_is_ignore_allocations ia = ia_ends_with ia ignore_allocations

let ia_is_inject ia = ia_ends_with ia inject

let ia_is_suppress_lint ia = ia_ends_with ia suppress_lint

let ia_is_on_event ia = ia_ends_with ia on_event

let ia_is_on_bind ia = ia_ends_with ia on_bind

let ia_is_on_mount ia = ia_ends_with ia on_mount

let ia_is_on_unbind ia = ia_ends_with ia on_unbind

let ia_is_on_unmount ia = ia_ends_with ia on_unmount

let ia_is_privacy_source ia = ia_ends_with ia privacy_source

let ia_is_privacy_sink ia = ia_ends_with ia privacy_sink

let ia_is_integrity_source ia = ia_ends_with ia integrity_source

let ia_is_integrity_sink ia = ia_ends_with ia integrity_sink

let ia_is_guarded_by ia = ia_ends_with ia guarded_by

let ia_is_ui_thread ia = ia_ends_with ia ui_thread

let ia_is_thread_confined ia = ia_ends_with ia thread_confined
