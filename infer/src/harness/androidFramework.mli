(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Android lifecycle types and their lifecycle methods that are called by the framework *)

(** return the complete list of (package, lifecycle_classname, lifecycle_methods) trios *)
val get_lifecycles : (string * string * string list) list

(** return true if [typ] <: android.content.Context *)
val is_context : Tenv.t -> Typ.struct_typ -> bool

(** return true if [struct_typ] <: android.app.Application *)
val is_application : Tenv.t -> Typ.struct_typ -> bool

(** return true if [struct_typ] <: android.app.Activity *)
val is_activity : Tenv.t -> Typ.struct_typ -> bool

(** return true if [struct_typ] <: android.view.View *)
val is_view : Tenv.t -> Typ.struct_typ -> bool

val is_fragment : Tenv.t -> Typ.struct_typ -> bool

(** return true if [procname] is a special lifecycle cleanup method *)
val is_destroy_method : Procname.t -> bool

(** given an Android framework type mangled string [lifecycle_typ] (e.g., android.app.Activity)
    and a list of method names [lifecycle_procs_strs], get the appropriate typ and procnames *)
val get_lifecycle_for_framework_typ_opt :
  Tenv.t -> Mangled.t -> string list -> (Typ.struct_typ * Procname.t list) option

(** return true if [class_name] is the name of a class that belong to the Android framework *)
val is_android_lib_class : Typename.t -> bool

(** Path to the android.jar file containing real code, not just the method stubs as in the SDK *)
val non_stub_android_jar : unit -> string
