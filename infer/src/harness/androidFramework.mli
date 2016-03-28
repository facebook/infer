(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Android lifecycle types and their lifecycle methods that are called by the framework *)

(** return the complete list of (package, lifecycle_classname, lifecycle_methods) trios *)
val get_lifecycles : (string * string * string list) list

(** return true if [typ] is a subclass of [lifecycle_typ] *)
val typ_is_lifecycle_typ : Sil.typ -> Sil.typ -> Tenv.t -> bool

(** return true if [typ] is a known callback class, false otherwise *)
val is_callback_class : Sil.typ -> Tenv.t -> bool

(** return true if [typ] <: android.content.Context *)
val is_context : Sil.typ -> Tenv.t -> bool

(** return true if [typ] <: android.app.Application *)
val is_application : Sil.typ -> Tenv.t -> bool

(** return true if [typ] <: android.app.Activity *)
val is_activity : Sil.typ -> Tenv.t -> bool

(** return true if [typ] <: android.view.View *)
val is_view : Sil.typ -> Tenv.t -> bool

val is_fragment : Sil.typ -> Tenv.t -> bool

(** return true if [procname] is a special lifecycle cleanup method *)
val is_destroy_method : Procname.t -> bool

(** returns an option containing the var name and type of a callback registered by [procname],
    None if no callback is registered *)
val get_callback_registered_by :
  Procname.java -> (Sil.exp * Sil.typ) list -> Tenv.t -> (Sil.exp * Sil.typ) option

(** return a list of typ's corresponding to callback classes registered by [procdesc] *)
val get_callbacks_registered_by_proc : Cfg.Procdesc.t -> Tenv.t -> Sil.typ list

(** given an Android framework type mangled string [lifecycle_typ] (e.g., android.app.Activity)
    and a list of method names [lifecycle_procs_strs], get the appropriate typ and procnames *)
val get_lifecycle_for_framework_typ_opt :
  Mangled.t -> string list -> Tenv.t -> (Sil.typ * Procname.t list) option

(** return true if [class_name] is the name of a class that belong to the Android framework *)
val is_android_lib_class : Typename.t -> bool

(** Path to the android.jar file containing real code, not just the method stubs as in the SDK *)
val non_stub_android_jar : unit -> string

val is_subclass : Tenv.t -> Typename.t -> string -> bool

(** [is_exception tenv class_name] checks if class_name is of type java.lang.Exception *)
val is_exception : Tenv.t -> Typename.t -> bool

(** [is_throwable tenv class_name] checks if class_name is of type java.lang.Throwable *)
val is_throwable : Tenv.t -> Typename.t -> bool

(** [is_runtime_exception tenv class_name] checks if classname is
    of type java.lang.RuntimeException *)
val is_runtime_exception : Tenv.t -> Typename.t -> bool
