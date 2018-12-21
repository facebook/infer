(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type container_access = ContainerRead | ContainerWrite

val get_container_access : Typ.Procname.t -> Tenv.t -> container_access option
(** return Some (access) if this procedure accesses the contents of a container (e.g., Map.get) *)

val has_return_annot : (Annot.Item.t -> bool) -> Typ.Procname.t -> bool

val is_functional : Typ.Procname.t -> bool

val acquires_ownership : Typ.Procname.t -> Tenv.t -> bool

val is_box : Typ.Procname.t -> bool
(** return true if the given procname boxes a primitive type into a reference type *)

val is_thread_confined_method : Tenv.t -> Procdesc.t -> bool
(** Methods in @ThreadConfined classes and methods annotated with @ThreadConfined are assumed to all
   run on the same thread. For the moment we won't warn on accesses resulting from use of such
   methods at all. In future we should account for races between these methods and methods from
   completely different classes that don't necessarily run on the same thread as the confined
   object. *)

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

val is_safe_access : AccessPath.access -> AccessPath.t -> Tenv.t -> bool
(** check if an access to a field is thread-confined, or whether the field is volatile *)

val should_flag_interface_call : Tenv.t -> HilExp.t list -> CallFlags.t -> Typ.Procname.t -> bool
(** should an interface call be flagged as potentially non-thread safe? *)

val is_synchronized_container : Typ.Procname.t -> AccessPath.t -> Tenv.t -> bool
(** is a call on an access path to a method of a synchronized container? *)
