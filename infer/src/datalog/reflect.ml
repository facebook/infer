(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! Javalib_pack
open Javalib
open JCode
open JBasics

type refl_call = {refl_ms: string; caller_cl: string; caller_ms: string}

let java_object = make_cn "java.lang.Object"

let java_class = make_cn "java.lang.Class"

let java_classloader = make_cn "java.lang.ClassLoader"

let java_string = make_cn "java.lang.String"

(** List of (class_name, method_signature) tuples. The scanner detects usages of these methods. *)
let reflect_ms =
  [ (* java.lang.Class.forName(String className) *)
    ( java_class
    , make_ms "forName" [TObject (TClass java_string)] (Some (TObject (TClass java_class))) )
  ; (* java.lang.Class.forName(String name, boolean initialize, ClassLoader loader) *)
    ( java_class
    , make_ms "forName"
        [TObject (TClass java_string); TBasic `Bool; TObject (TClass java_classloader)]
        (Some (TObject (TClass java_class))) )
  ; (* java.lang.Object.getClass() *)
    (java_object, make_ms "getClass" [] (Some (TObject (TClass java_class))))
  ; (* java.class.newInstance() *)
    (java_class, (make_ms "newInstance") [] (Some (TObject (TClass java_object)))) ]


let cn_ms_equal (cn, ms) (cn_r, ms_r) =
  String.equal (cn_name cn) (cn_name cn_r) && String.equal (ms_name ms) (ms_name ms_r)


(** The accumulator can be used in conjunction with MethodMap.fold *)
let get_method_refl_calls_ m acc =
  match m.cm_implementation with
  | Java code ->
      Array.fold (Lazy.force code).c_code ~init:acc ~f:(fun calls_acc op ->
          match op with
          (* invokevirtual (e.g. getClass) and invokestatic (e.g. forName) *)
          | (OpInvoke (`Virtual (TClass cn), ms) | OpInvoke (`Static (_, cn), ms))
            when List.exists ~f:(fun x -> cn_ms_equal x (cn, ms)) reflect_ms ->
              let caller_cl, caller_ms = cms_split m.cm_class_method_signature in
              {refl_ms= ms_name ms; caller_cl= cn_name caller_cl; caller_ms= ms_name caller_ms}
              :: calls_acc
          | _ ->
              calls_acc )
  | Native ->
      acc


let get_method_refl_calls m = get_method_refl_calls_ m []
