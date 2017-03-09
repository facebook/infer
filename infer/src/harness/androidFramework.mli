(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Android lifecycle types and their lifecycle methods that are called by the framework *)

(** return the complete list of (package, lifecycle_classname, lifecycle_methods) trios *)
val get_lifecycles : (string * string * string list) list

(** return true if [typename] <: android.content.Context *)
val is_context : Tenv.t -> Typename.t -> bool

(** return true if [typename] <: android.app.Application *)
val is_application : Tenv.t -> Typename.t -> bool

(** return true if [typename] <: android.app.Activity *)
val is_activity : Tenv.t -> Typename.t -> bool

(** return true if [typename] <: android.view.View *)
val is_view : Tenv.t -> Typename.t -> bool

val is_fragment : Tenv.t -> Typename.t -> bool

(** return true if [procname] is a special lifecycle cleanup method *)
val is_destroy_method : Typ.Procname.t -> bool

(** given an Android framework type mangled string [lifecycle_typ] (e.g., android.app.Activity)
    and a list of method names [lifecycle_procs_strs], get the appropriate typ and procnames *)
val get_lifecycle_for_framework_typ_opt : Tenv.t -> Typename.t -> string list -> Typ.Procname.t list

(** return true if [class_name] is the name of a class that belong to the Android framework *)
val is_android_lib_class : Typename.t -> bool
