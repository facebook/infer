(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Annotations. *)

let auto_cleanup = "AutoCleanup"

let camel_nonnull = "NonNull"

let expensive = "Expensive"

let for_ui_thread = "ForUiThread"

let for_non_ui_thread = "ForNonUiThread"

let functional = "Functional"

let guarded_by = "GuardedBy"

let ignore_allocations = "IgnoreAllocations"

let immutable = "Immutable"

let initializer_ = "Initializer"

let inject = "Inject"

let inject_prop = "InjectProp"

let lockless = "Lockless"

let nonnull = "Nonnull"

let no_allocation = "NoAllocation"

let nullable = "Nullable"

let nullable_decl = "NullableDecl"

let nullsafe_strict = "NullsafeStrict"

let nullsafe = "Nullsafe"

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

let suppress_lint = "SuppressLint"

let recently_nonnull = "RecentlyNonNull"

let recently_nullable = "RecentlyNullable"

let thread_confined = "ThreadConfined"

let thread_safe = "ThreadSafe"

let ui_thread = "UiThread"

let visibleForTesting = "VisibleForTesting"

let volatile = "volatile"

let worker_thread = "WorkerThread"

let jetbrains_not_null = "org.jetbrains.annotations.NotNull"

let ia_has_annotation_with (ia : Annot.Item.t) (predicate : Annot.t -> bool) : bool =
  List.exists ~f:predicate ia


let method_has_annotation_with (ret_annot : Annot.Item.t) (params : Annot.Item.t list)
    (predicate : Annot.t -> bool) : bool =
  let has_annot a = ia_has_annotation_with a predicate in
  has_annot ret_annot || List.exists ~f:has_annot params


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


let class_name_matches s (annot : Annot.t) = String.equal s annot.class_name

let ia_ends_with ia ann_name = List.exists ~f:(fun a -> annot_ends_with a ann_name) ia

let ia_class_name_matches ia class_name =
  List.exists ~f:(fun a -> class_name_matches class_name a) ia


let find_ia_ends_with ia ann_name = List.find ~f:(fun a -> annot_ends_with a ann_name) ia

let ia_contains ia ann_name = List.exists ~f:(class_name_matches ann_name) ia

let pdesc_get_return_annot pdesc = (Procdesc.get_attributes pdesc).ProcAttributes.ret_annots

let pdesc_has_return_annot pdesc predicate = predicate (pdesc_get_return_annot pdesc)

let pname_has_return_annot pname predicate =
  Attributes.load pname
  |> Option.exists ~f:(fun {ProcAttributes.ret_annots} -> predicate ret_annots)


let attrs_return_annot_ends_with attrs annot = ia_ends_with attrs.ProcAttributes.ret_annots annot

let field_has_annot fieldname (struct_typ : Struct.t) predicate =
  let fld_has_taint_annot {Struct.name= fname; annot} =
    Fieldname.equal fieldname fname && predicate annot
  in
  List.exists ~f:fld_has_taint_annot struct_typ.fields
  || List.exists ~f:fld_has_taint_annot struct_typ.statics


let struct_typ_has_annot (struct_typ : Struct.t) predicate = predicate struct_typ.annots

let ia_is_not_thread_safe ia = ia_ends_with ia not_thread_safe

let ia_is_nullable ia =
  List.exists ~f:(ia_ends_with ia)
    [ nullable
    ; nullable_decl
      (* From org.checkerframework.checker.nullness.compatqual package. Extensively used in Guava library.
         Identical to {@code @Nullable}, but can only be written at declaration locations. *)
    ; propagates_nullable (* @PropagatesNullable is implicitly nullable *)
    ; recently_nullable
      (* @RecentlyNullable is a special annotation that was added to solve backward compatibility issues
         for Android SDK migration.
         See https://android-developers.googleblog.com/2018/08/android-pie-sdk-is-now-more-kotlin.html for details.
         From nullsafe point of view, such annotations should be treated exactly as normal @Nullable annotation.
         (Actually, it might even be shown as @Nullable in IDE/source code)
      *) ]


let ia_is_nonnull ia =
  List.exists ~f:(ia_ends_with ia)
    [ nonnull
    ; notnull
    ; camel_nonnull
    ; recently_nonnull
      (* @RecentlyNonNull is a special annotation that was added to solve backward compatibility issues
         for Android SDK migration.
         See https://android-developers.googleblog.com/2018/08/android-pie-sdk-is-now-more-kotlin.html for details.
         From nullsafe point of view, such annotations should be treated exactly as normal @NonNull annotation.
         (Actually, it might even be shown as @NonNull in IDE/source code)
      *) ]


let ia_is_jetbrains_notnull ia = ia_class_name_matches ia jetbrains_not_null

let ia_is_nullsafe_strict ia = ia_ends_with ia nullsafe_strict

let ia_find_nullsafe ia = find_ia_ends_with ia nullsafe

let ia_is_returns_ownership ia = ia_ends_with ia returns_ownership

let ia_is_synchronized_collection ia = ia_ends_with ia synchronized_collection

let ia_is_thread_safe ia = ia_ends_with ia thread_safe

let ia_is_nonblocking ia = ia_ends_with ia nonblocking

let ia_is_initializer ia = ia_ends_with ia initializer_

let ia_is_volatile ia = ia_contains ia volatile

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
