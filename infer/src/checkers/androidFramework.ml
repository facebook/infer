(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module L = Logging
module F = Format

(** Android lifecycle types and their lifecycle methods that are called by the framework *)

let on_destroy = "onDestroy"

let on_destroy_view = "onDestroyView"

let drawable_prefix = "R$drawable"

(** return true if [pname] is a special lifecycle cleanup method *)
let is_destroy_method pname =
  match pname with
  | Typ.Procname.Java pname_java ->
      let method_name = Typ.Procname.java_get_method pname_java in
      String.equal method_name on_destroy || String.equal method_name on_destroy_view
  | _ ->
      false


let is_subtype_package_class tenv tname package classname =
  PatternMatch.is_subtype tenv tname (Typ.Name.Java.from_package_class package classname)


let is_autocloseable tenv tname = is_subtype_package_class tenv tname "java.lang" "AutoCloseable"

let is_context tenv tname = is_subtype_package_class tenv tname "android.content" "Context"

let is_application tenv tname = is_subtype_package_class tenv tname "android.app" "Application"

let is_activity tenv tname = is_subtype_package_class tenv tname "android.app" "Activity"

let is_view tenv tname = is_subtype_package_class tenv tname "android.view" "View"

let is_fragment tenv tname =
  is_subtype_package_class tenv tname "android.app" "Fragment"
  || is_subtype_package_class tenv tname "android.support.v4.app" "Fragment"


(** return true if [class_name] is the name of a class that belong to the Android framework *)
let is_android_lib_class class_name =
  let class_str = Typ.Name.name class_name in
  String.is_prefix ~prefix:"android" class_str || String.is_prefix ~prefix:"com.android" class_str
