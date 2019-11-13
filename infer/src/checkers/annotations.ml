(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Annotations. *)

let any_thread = "AnyThread"

let auto_cleanup = "AutoCleanup"

let bind = "Bind"

let bind_view = "BindView"

let bind_array = "BindArray"

let bind_bitmap = "BindBitmap"

let bind_drawable = "BindDrawable"

let bind_string = "BindString"

let camel_nonnull = "NonNull"

let cleanup = "Cleanup"

let expensive = "Expensive"

let false_on_null = "FalseOnNull"

let for_ui_thread = "ForUiThread"

let for_non_ui_thread = "ForNonUiThread"

let functional = "Functional"

let guarded_by = "GuardedBy"

let ignore_allocations = "IgnoreAllocations"

let initializer_ = "Initializer"

let inject = "Inject"

let inject_prop = "InjectProp"

let inject_view = "InjectView"

let lockless = "Lockless"

let nonnull = "Nonnull"

let no_allocation = "NoAllocation"

let nullable = "Nullable"

let nullsafe_strict = "NullsafeStrict"

let mainthread = "MainThread"

let nonblocking = "NonBlocking"

let notnull = "NotNull"

let not_thread_safe = "NotThreadSafe"

let on_bind = "OnBind"

let on_event = "OnEvent"

let on_mount = "OnMount"

let on_unbind = "OnUnbind"

let on_unmount = "OnUnmount"

let performance_critical = "PerformanceCritical"

let prop = "Prop"

let propagates_nullable = "PropagatesNullable"

let returns_ownership = "ReturnsOwnership"

let synchronized_collection = "SynchronizedCollection"

let generated_graphql = "GeneratedGraphQL"

let suppress_lint = "SuppressLint"

let suppress_view_nullability = "SuppressViewNullability"

let thread_confined = "ThreadConfined"

let thread_safe = "ThreadSafe"

let thrift_service = "ThriftService"

let true_on_null = "TrueOnNull"

let ui_thread = "UiThread"

let visibleForTesting = "VisibleForTesting"

let volatile = "volatile"

let worker_thread = "WorkerThread"

let ia_has_annotation_with (ia : Annot.Item.t) (predicate : Annot.t -> bool) : bool =
  List.exists ~f:(fun (a, _) -> predicate a) ia


let ma_has_annotation_with ({return; params} : Annot.Method.t) (predicate : Annot.t -> bool) : bool
    =
  let has_annot a = ia_has_annotation_with a predicate in
  has_annot return || List.exists ~f:has_annot params


(** [annot_ends_with annot ann_name] returns true if the class name of [annot], without the package,
    is equal to [ann_name] *)
let annot_ends_with ({class_name} : Annot.t) ann_name =
  String.is_suffix class_name ~suffix:ann_name
  &&
  (* here, [class_name] ends with [ann_name] but it could be that it's just a suffix
     of the last dot-component of [class_name], eg [class_name="x.y.z.a.bcd"] and
     [ann_name="cd"]; in that case we want to fail the check, so we check that the
     character just before the match is indeed a dot (or there is no dot at all). *)
  let dot_pos = String.(length class_name - length ann_name - 1) in
  Int.is_negative dot_pos || Char.equal '.' class_name.[dot_pos]


let class_name_matches s ((annot : Annot.t), _) = String.equal s annot.class_name

let ia_ends_with ia ann_name = List.exists ~f:(fun (a, _) -> annot_ends_with a ann_name) ia

let ia_contains ia ann_name = List.exists ~f:(class_name_matches ann_name) ia

let pdesc_get_return_annot pdesc =
  (Procdesc.get_attributes pdesc).ProcAttributes.method_annotation.return


let pdesc_has_return_annot pdesc predicate = predicate (pdesc_get_return_annot pdesc)

let pdesc_return_annot_ends_with pdesc annot =
  pdesc_has_return_annot pdesc (fun ia -> ia_ends_with ia annot)


(* note: we would use Summary.proc_resolve_attributes directly instead of requiring [attrs_of_pname],
   but doing so creates a circular dependency *)
let pname_has_return_annot pname ~attrs_of_pname predicate =
  match attrs_of_pname pname with
  | Some attributes ->
      predicate attributes.ProcAttributes.method_annotation.return
  | None ->
      false


let field_has_annot fieldname (struct_typ : Typ.Struct.t) predicate =
  let fld_has_taint_annot (fname, _, annot) =
    Typ.Fieldname.equal fieldname fname && predicate annot
  in
  List.exists ~f:fld_has_taint_annot struct_typ.fields
  || List.exists ~f:fld_has_taint_annot struct_typ.statics


let struct_typ_has_annot (struct_typ : Typ.Struct.t) predicate = predicate struct_typ.annots

let ia_is_not_thread_safe ia = ia_ends_with ia not_thread_safe

let ia_is_propagates_nullable ia = ia_ends_with ia propagates_nullable

let ia_is_nullable ia = ia_ends_with ia nullable || ia_is_propagates_nullable ia

let ia_is_nonnull ia = List.exists ~f:(ia_ends_with ia) [nonnull; notnull; camel_nonnull]

let ia_is_nullsafe_strict ia = ia_ends_with ia nullsafe_strict

let ia_is_false_on_null ia = ia_ends_with ia false_on_null

let ia_is_returns_ownership ia = ia_ends_with ia returns_ownership

let ia_is_synchronized_collection ia = ia_ends_with ia synchronized_collection

let ia_is_thread_safe ia = ia_ends_with ia thread_safe

let ia_is_thrift_service ia = ia_ends_with ia thrift_service

let ia_is_true_on_null ia = ia_ends_with ia true_on_null

let ia_is_nonblocking ia = ia_ends_with ia nonblocking

let ia_is_initializer ia = ia_ends_with ia initializer_

let ia_is_cleanup ia = ia_ends_with ia cleanup

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
let ia_is_field_injector_readonly ia = List.exists ~f:(ia_ends_with ia) field_injector_readonly_list

(** Annotations for read-write injectors.
    The injector framework initializes the field and can write null into it. *)
let ia_is_field_injector_readwrite ia =
  List.exists ~f:(ia_ends_with ia) field_injector_readwrite_list


let ia_is_expensive ia = ia_ends_with ia expensive

let ia_is_functional ia = ia_ends_with ia functional

let ia_is_ignore_allocations ia = ia_ends_with ia ignore_allocations

let ia_is_inject ia = ia_ends_with ia inject

let ia_is_suppress_lint ia = ia_ends_with ia suppress_lint

let ia_is_thread_confined ia = ia_ends_with ia thread_confined

let ia_is_worker_thread ia = ia_ends_with ia worker_thread

(* methods annotated with the annotations below always run on the UI thread. *)
let ia_is_uithread_equivalent =
  let annotations = [mainthread; ui_thread; on_bind; on_event; on_mount; on_unbind; on_unmount] in
  fun ia -> List.exists annotations ~f:(ia_ends_with ia)
