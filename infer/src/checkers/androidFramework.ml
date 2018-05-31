(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Android lifecycle types and their lifecycle methods that are called by the framework *)

let on_destroy = "onDestroy"

let on_destroy_view = "onDestroyView"

let drawable_prefix = "R$drawable"

(** return true if [pname] is a special lifecycle cleanup method *)
let is_destroy_method pname =
  match pname with
  | Typ.Procname.Java pname_java ->
      let method_name = Typ.Procname.Java.get_method pname_java in
      String.equal method_name on_destroy || String.equal method_name on_destroy_view
  | _ ->
      false


let is_subtype_package_class tenv tname package classname =
  PatternMatch.is_subtype tenv tname (Typ.Name.Java.from_package_class package classname)


let is_autocloseable tenv tname = is_subtype_package_class tenv tname "java.lang" "AutoCloseable"

let is_view tenv tname = is_subtype_package_class tenv tname "android.view" "View"

let is_fragment tenv tname =
  is_subtype_package_class tenv tname "android.app" "Fragment"
  || is_subtype_package_class tenv tname "android.support.v4.app" "Fragment"
