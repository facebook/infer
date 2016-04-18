(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging
module F = Format
module TypSet = Sil.TypSet

(** Android lifecycle types and their lifecycle methods that are called by the framework *)

let on_destroy = "onDestroy"
let on_destroy_view = "onDestroyView"

(** return true if [pname] is a special lifecycle cleanup method *)
let is_destroy_method pname =
  match pname with
  | Procname.Java pname_java ->
      let method_name = Procname.java_get_method pname_java in
      string_equal method_name on_destroy || string_equal method_name on_destroy_view
  | _ ->
      false

let android_lifecycles =
  let android_content = "android.content" in
  let android_app = "android.app" in
  let fragment_lifecycle =
    ["onInflate"; "onAttach"; "onCreate"; "onCreateView"; "onViewCreated"; "onActivityCreated";
     "onViewStateRestored"; "onStart"; "onResume"; "onPause"; "onSaveInstanceState"; "onStop";
     on_destroy_view; on_destroy; "onDetach"] in
  [ (android_content,
     "ContentProvider",
     ["onCreate"]);
    (android_app,
     "Activity",
     ["onCreate"; "onStart"; "onRestoreInstanceState"; "onPostCreate"; "onResume"; "onPostResume";
      "onCreateDescription"; "onSaveInstanceState"; "onPause"; "onStop"; on_destroy]);
    (android_app,
     "Service",
     ["onCreate"; "onStart"; "onStartCommand"; "onBind"; "onUnbind"; on_destroy]);
    (android_content,
     "BroadcastReceiever",
     ["onReceive"]);
    (android_app,
     "Fragment",
     fragment_lifecycle);
    (* this is the pre-Android 3.0 Fragment type (can also be used post-3.0) *)
    ("android.support.v4.app",
     "Fragment",
     fragment_lifecycle);
  ]

(** return the complete set of superclasses of [typ *)
(* TODO (t4644852): factor out subtyping functions into some sort of JavaUtil module *)
let get_all_supertypes typ tenv =
  let get_direct_supers = function
    | Sil.Tstruct { Sil.csu = Csu.Class _; superclasses } ->
        superclasses
    | _ -> [] in
  let rec add_typ class_name typs =
    match Tenv.lookup tenv class_name with
    | Some struct_typ ->
        let typ' = Sil.Tstruct struct_typ in
        get_supers_rec typ' (TypSet.add typ' typs)
    | None -> typs
  and get_supers_rec typ all_supers =
    let direct_supers = get_direct_supers typ in
    IList.fold_left
      (fun typs class_name -> add_typ class_name typs)
      all_supers direct_supers in
  get_supers_rec typ (TypSet.add typ TypSet.empty)

(** return true if [typ0] <: [typ1] *)
let is_subtype (typ0 : Sil.typ) (typ1 : Sil.typ) tenv =
  TypSet.mem typ1 (get_all_supertypes typ0 tenv)

let is_subtype_package_class typ package classname tenv =
  let classname = Mangled.from_package_class package classname in
  match Tenv.lookup tenv (Typename.TN_csu (Csu.Class Csu.Java, classname)) with
  | Some found_struct_typ -> is_subtype typ (Sil.Tstruct found_struct_typ) tenv
  | _ -> false

let is_context typ tenv =
  is_subtype_package_class typ "android.content" "Context" tenv

let is_application typ tenv =
  is_subtype_package_class typ "android.app" "Application" tenv

let is_activity typ tenv =
  is_subtype_package_class typ "android.app" "Activity" tenv

let is_view typ tenv =
  is_subtype_package_class typ "android.view" "View" tenv

let is_fragment typ tenv =
  is_subtype_package_class typ "android.app" "Fragment" tenv ||
  is_subtype_package_class typ "android.support.v4.app" "Fragment" tenv

(** return true if [typ] is a subclass of [lifecycle_typ] *)
let typ_is_lifecycle_typ typ lifecycle_typ tenv =
  let supers = get_all_supertypes typ tenv in
  TypSet.mem lifecycle_typ supers

(** return true if [class_name] is the name of a class that belong to the Android framework *)
let is_android_lib_class class_name =
  let class_str = Typename.name class_name in
  string_is_prefix "android" class_str || string_is_prefix "com.android" class_str

(** given an Android framework type mangled string [lifecycle_typ] (e.g., android.app.Activity) and
    a list of method names [lifecycle_procs_strs], get the appropriate typ and procnames *)
let get_lifecycle_for_framework_typ_opt lifecycle_typ lifecycle_proc_strs tenv =
  match Tenv.lookup tenv (Typename.TN_csu (Csu.Class Csu.Java, lifecycle_typ)) with
  | Some ({ Sil.csu = Csu.Class _; struct_name = Some _; def_methods } as lifecycle_typ) ->
      (* TODO (t4645631): collect the procedures for which is_java is returning false *)
      let lookup_proc lifecycle_proc =
        IList.find (fun decl_proc ->
            match decl_proc with
            | Procname.Java decl_proc_java ->
                lifecycle_proc = Procname.java_get_method decl_proc_java
            | _ ->
                false
          ) def_methods in
      (* convert each of the framework lifecycle proc strings to a lifecycle method procname *)
      let lifecycle_procs =
        IList.fold_left (fun lifecycle_procs lifecycle_proc_str ->
            try (lookup_proc lifecycle_proc_str) :: lifecycle_procs
            with Not_found -> lifecycle_procs)
          [] lifecycle_proc_strs in
      Some (Sil.Tstruct lifecycle_typ, lifecycle_procs)
  | _ -> None

(** return the complete list of (package, lifecycle_classname, lifecycle_methods) trios *)
let get_lifecycles = android_lifecycles


let is_subclass tenv cn1 classname_str =
  let typename =
    Typename.Java.from_string classname_str in
  let lookup = Tenv.lookup tenv in
  match lookup cn1, lookup typename with
  | Some typ1, Some typ2 ->
      is_subtype (Sil.Tstruct typ1) (Sil.Tstruct typ2) tenv
  | _ -> false


(** Checks if the exception is an uncheched exception *)
let is_runtime_exception tenv typename =
  is_subclass tenv typename "java.lang.RuntimeException"


(** Checks if the class name is a Java exception *)
let is_exception tenv typename =
  is_subclass tenv typename "java.lang.Exception"


(** Checks if the class name is a Java exception *)
let is_throwable tenv typename =
  is_subclass tenv typename "java.lang.Throwable"


let non_stub_android_jar () =
  let root_dir = Filename.dirname (Filename.dirname Sys.executable_name) in
  IList.fold_left Filename.concat root_dir ["lib"; "java"; "android"; "android-19.jar"]
