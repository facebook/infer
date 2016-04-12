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

(** Automatically create a harness method to exercise code under test *)

(** if [typ] is a lifecycle type, generate a list of (method call, receiver) pairs constituting a
    lifecycle trace *)
let try_create_lifecycle_trace typ lifecycle_typ lifecycle_procs tenv = match typ with
  | Sil.Tstruct { Sil.csu = Csu.Class Csu.Java; struct_name = Some name } ->
      let class_name = Typename.TN_csu (Csu.Class Csu.Java, name) in
      if AndroidFramework.typ_is_lifecycle_typ typ lifecycle_typ tenv &&
         not (AndroidFramework.is_android_lib_class class_name) then
        let ptr_to_typ = Some (Sil.Tptr (typ, Sil.Pk_pointer)) in
        IList.fold_left
          (fun trace lifecycle_proc ->
             (* given a lifecycle subclass T, resolve the call T.lifecycle_proc() to the procname
              * that will actually be called at runtime *)
             let resolved_proc = SymExec.resolve_method tenv class_name lifecycle_proc in
             (resolved_proc, ptr_to_typ) :: trace)
          [] lifecycle_procs
      else []
  | _ -> []

(** generate a harness for a lifecycle type in an Android application *)
let create_harness cfg cg tenv =
  IList.iter (fun (pkg, clazz, lifecycle_methods) ->
      let typ_name = Mangled.from_package_class pkg clazz in
      match AndroidFramework.get_lifecycle_for_framework_typ_opt typ_name lifecycle_methods tenv with
      | Some (framework_typ, framework_procs) ->
          (* iterate through the type environment and generate a lifecycle harness for each
             subclass of [lifecycle_typ] *)
          (* TODO: instead of iterating through the type environment, interate through the types
             declared in [cfg] *)
          Tenv.iter (fun _ struct_typ ->
              let typ = Sil.Tstruct struct_typ in
              match try_create_lifecycle_trace typ framework_typ framework_procs tenv with
              | [] -> ()
              | lifecycle_trace ->
                  let harness_procname =
                    let harness_cls_name = PatternMatch.get_type_name typ in
                    let pname =
                      Procname.Java
                        (Procname.java
                           (None, harness_cls_name) None
                           "InferGeneratedHarness" [] Procname.Static) in
                    match pname with
                    | Procname.Java harness_procname -> harness_procname
                    | _ -> assert false in
                  Inhabit.inhabit_trace lifecycle_trace harness_procname cg cfg
            ) tenv
      | None -> ()
    ) AndroidFramework.get_lifecycles
