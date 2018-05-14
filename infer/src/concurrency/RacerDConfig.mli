(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format

module Models : sig
  type lock = Lock | Unlock | LockedIfTrue | NoEffect

  type thread = BackgroundThread | MainThread | MainThreadIfTrue | UnknownThread

  type container_access = ContainerRead | ContainerWrite

  (* TODO: clean this up so it takes only a procname *)

  val is_thread_utils_method : string -> Typ.Procname.t -> bool
  (** return true if the given method name is a utility class for checking what thread we're on *)

  val get_lock : Typ.Procname.t -> HilExp.t list -> lock
  (** describe how this procedure behaves with respect to locking *)

  val get_thread : Typ.Procname.t -> thread
  (** describe how this procedure behaves with respect to thread access *)

  val get_container_access : Typ.Procname.t -> Tenv.t -> container_access option
  (** return Some (access) if this procedure accesses the contents of a container (e.g., Map.get) *)

  val is_java_library : Typ.Procname.t -> bool
  (** return true if this function is library code from the JDK core libraries or Android *)

  val is_builder_function : Typ.Procname.t -> bool

  val has_return_annot : (Annot.Item.t -> bool) -> Typ.Procname.t -> bool

  val is_functional : Typ.Procname.t -> bool

  val acquires_ownership : Typ.Procname.t -> Tenv.t -> bool

  val is_threadsafe_collection : Typ.Procname.t -> Tenv.t -> bool

  val is_box : Typ.Procname.t -> bool
  (** return true if the given procname boxes a primitive type into a reference type *)

  val is_thread_confined_method : Tenv.t -> Procdesc.t -> bool
  (** Methods in @ThreadConfined classes and methods annotated with @ThreadConfined are assumed to all
     run on the same thread. For the moment we won't warn on accesses resulting from use of such
     methods at all. In future we should account for races between these methods and methods from
     completely different classes that don't necessarily run on the same thread as the confined
     object. *)

  val runs_on_ui_thread : Tenv.t -> Procdesc.t -> bool
  (** We don't want to warn on methods that run on the UI thread because they should always be
      single-threaded. Assume that methods annotated with @UiThread, @OnEvent, @OnBind, @OnMount,
      @OnUnbind, @OnUnmount always run on the UI thread.  Also assume that any superclass
      marked @UiThread implies all methods are on UI thread. *)

  val should_analyze_proc : Procdesc.t -> Tenv.t -> bool
  (** return true if we should compute a summary for the procedure. if this returns false, we won't
     analyze the procedure or report any warnings on it.
     note: in the future, we will want to analyze the procedures in all of these cases in order to
     find more bugs. this is just a temporary measure to avoid obvious false positives *)

  val get_current_class_and_threadsafe_superclasses :
    Tenv.t -> Typ.Procname.t -> (Typ.name * Typ.name list) option

  val is_thread_safe_method : Typ.Procname.t -> Tenv.t -> bool
  (** returns true if method or overriden method in superclass
      is @ThreadSafe, @ThreadSafe(enableChecks = true), or is defined
      as an alias of @ThreadSafe in a .inferconfig file. *)

  val is_marked_thread_safe : Procdesc.t -> Tenv.t -> bool

  val should_report_on_proc : Procdesc.t -> Tenv.t -> bool
  (** return true if procedure is at an abstraction boundary or reporting has been explicitly
     requested via @ThreadSafe *)

  val may_block : Tenv.t -> Typ.Procname.t -> HilExp.t list -> bool
  (** is the method call potentially blocking, given the actuals passed? *)

  val is_synchronized_library_call : Tenv.t -> Typ.Procname.t -> bool
  (** does the method call lock-then-unlock the underlying object?
      legacy Java containers like Vector do this, and can interact with explicit locking *)
end
