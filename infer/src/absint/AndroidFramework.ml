(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  | Procname.Java pname_java ->
      let method_name = Procname.Java.get_method pname_java in
      String.equal method_name on_destroy || String.equal method_name on_destroy_view
  | _ ->
      false


let is_autocloseable tenv tname =
  PatternMatch.is_subtype_of_str tenv tname "java.lang.AutoCloseable"


let is_view tenv tname = PatternMatch.is_subtype_of_str tenv tname "android.view.View"

let is_fragment =
  let fragments =
    ["androidx.fragment.app.Fragment"; "android.app.Fragment"; "android.support.v4.app.Fragment"]
  in
  fun tenv tname -> List.exists fragments ~f:(PatternMatch.is_subtype_of_str tenv tname)
