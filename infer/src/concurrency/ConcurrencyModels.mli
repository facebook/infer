(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

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

val is_thread_utils_method : string -> Typ.Procname.t -> bool
(** return true if the given method name is a utility class for checking what thread we're on
    TODO: clean this up so it takes only a procname *)

val get_lock_effect : Typ.Procname.t -> HilExp.t list -> lock_effect
(** describe how this procedure behaves with respect to locking *)

val get_thread : Typ.Procname.t -> thread
(** describe how this procedure behaves with respect to thread access *)

val runs_on_ui_thread :
     attrs_of_pname:(Typ.Procname.t -> ProcAttributes.t option)
  -> Tenv.t
  -> Procdesc.t
  -> string option
(** We don't want to warn on methods that run on the UI thread because they should always be
    single-threaded. Assume that methods annotated with @UiThread, @OnEvent, @OnBind, @OnMount,
    @OnUnbind, @OnUnmount always run on the UI thread.  Also assume that any superclass
    marked @UiThread implies all methods are on UI thread. Return Some string explaining why
    this method is on the UI thread, else return None. *)

val get_current_class_and_annotated_superclasses :
  (Annot.Item.t -> bool) -> Tenv.t -> Typ.Procname.t -> (Typ.name * Typ.name list) option

val find_annotated_or_overriden_annotated_method :
     attrs_of_pname:(Typ.Procname.t -> ProcAttributes.t option)
  -> (Annot.Item.t -> bool)
  -> Typ.Procname.t
  -> Tenv.t
  -> Typ.Procname.t sexp_option

val cpp_lock_types_matcher : QualifiedCppName.Match.quals_matcher

val is_recursive_lock_type : Typ.name -> bool
