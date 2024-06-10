(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** effect of call plus Hil expressions being un/locked, if known *)
type lock_effect =
  | Lock of HilExp.t list  (** simultaneously acquire a list of locks *)
  | Unlock of HilExp.t list  (** simultaneously release a list of locks *)
  | LockedIfTrue of HilExp.t list  (** simultaneously attempt to acquire a list of locks *)
  | GuardConstruct of {guard: HilExp.t; lock: HilExp.t; acquire_now: bool}
      (** mutex guard construction - clang only *)
  | GuardLock of HilExp.t  (** lock underlying mutex via guard - clang only *)
  | GuardLockedIfTrue of HilExp.t  (** lock underlying mutex if true via guard - clang only *)
  | GuardUnlock of HilExp.t  (** unlock underlying mutex via guard - clang only *)
  | GuardDestroy of HilExp.t  (** destroy guard and unlock underlying mutex - clang only *)
  | NoEffect  (** function call has no lock-relevant effect *)

type thread = BackgroundThread | MainThread | MainThreadIfTrue | UnknownThread

val get_lock_effect : Procname.t -> HilExp.t list -> lock_effect
(** describe how this procedure behaves with respect to locking *)

val get_thread_assert_effect : Procname.t -> thread
(** In Java, certain methods can be used to assert execution on a specific kind of thread, or return
    a boolean equivalent to such a fact. *)

val get_current_class_and_annotated_superclasses :
  (Annot.Item.t -> bool) -> Tenv.t -> Procname.t -> (Typ.name * Typ.name list) option

val is_recursive_lock_type : Typ.name -> bool

(** Type documenting why a method is considered as annotated with a certain annotation *)
type annotation_trail =
  | DirectlyAnnotated  (** the method is directly annotated as such *)
  | Override of Procname.t  (** it overrides a method annotated in a super class *)
  | SuperClass of Typ.name  (** the method's class or a super class of that is annotated as such *)
[@@deriving compare]

val find_override_or_superclass_annotated :
  (Annot.Item.t -> bool) -> Tenv.t -> Procname.t -> annotation_trail option
(** check if a method's transitive annotations satisfy the given predicate *)

val annotated_as_worker_thread : Tenv.t -> Procname.t -> bool

val annotated_as_named_thread : Procname.t -> string option

val runs_on_ui_thread : Tenv.t -> Procname.t -> bool
(** is method not transitively annotated [@WorkerThread] and is modeled or annotated [@UIThread] or
    equivalent? *)

val is_android_lifecycle_method : Tenv.t -> Procname.t -> bool
(** is method a known Android UI thread callback (eg [Activity.onCreate]) *)
