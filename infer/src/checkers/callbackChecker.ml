(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Make sure callbacks are always unregistered. drive the point home by reporting possible NPE's *)

module L = Logging
module F = Format
module P = Printf
module IdSet = Ident.IdentSet
module FldSet = Ident.FieldSet

(** set of instance fields belonging to the current file that are assigned to null literals *)
let fields_nullified = ref FldSet.empty

(** set of callbacks registered in the current file *)
let registered_callback_procs = ref Procname.Set.empty

let android_lifecycle_typs = ref []

(** resolve the list of android lifecycle type strings in [tenv] *)
let get_or_create_lifecycle_typs tenv = match !android_lifecycle_typs with
  | [] ->
      let lifecycle_typs = IList.fold_left (fun typs (pkg, clazz, methods) ->
          let qualified_name = Mangled.from_package_class pkg clazz in
          match AndroidFramework.get_lifecycle_for_framework_typ_opt
                  qualified_name methods tenv with
          | Some (framework_typ, _) -> framework_typ :: typs
          | None -> typs
        ) [] AndroidFramework.get_lifecycles in
      android_lifecycle_typs := lifecycle_typs;
      lifecycle_typs
  | typs -> typs


let num_methods_checked = ref 0

let done_checking num_methods =
  incr num_methods_checked;
  !num_methods_checked = num_methods

(** ask Eradicate to check each of the procs in [registered_callback_procs] (and their transitive
 * callees) in a context where each of the fields in [fields_nullifed] is marked as @Nullable *)
let do_eradicate_check ({ Callbacks.get_proc_desc } as callback_args) =
  (* tell Eradicate to treat each of the fields nullified in on_destroy as nullable *)
  FldSet.iter (fun fld -> Models.Inference.field_add_nullable_annotation fld) !fields_nullified;
  Procname.Set.iter
    (fun proc_name ->
       match get_proc_desc proc_name with
       | Some proc_desc ->
           Eradicate.callback_eradicate
             { callback_args with Callbacks.proc_name; proc_desc }
       | None -> ())
    !registered_callback_procs

(** if [procname] belongs to an Android lifecycle type, save the set of callbacks registered in
 * [procname]. in addition, if [procname] is a special "destroy" /"cleanup" method, save the set of
 * fields that are nullified *)
let callback_checker_main_java
    proc_name_java ({ Callbacks.proc_desc; tenv } as callback_args) =
  let typename =
    Typename.TN_csu
      (Csu.Class Csu.Java,
       Mangled.from_string (Procname.java_get_class_name proc_name_java)) in
  match Tenv.lookup tenv typename with
  | Some ({ struct_name = Some _; def_methods } as struct_typ) ->
      let typ = Sil.Tstruct struct_typ in
      let lifecycle_typs = get_or_create_lifecycle_typs tenv in
      let proc_belongs_to_lifecycle_typ = IList.exists
          (fun lifecycle_typ -> AndroidFramework.typ_is_lifecycle_typ typ lifecycle_typ tenv)
          lifecycle_typs in
      if proc_belongs_to_lifecycle_typ then
        (* TODO (tt4959422): get all of the callbacks registered by callees as well *)
        let registered_callback_typs =
          AndroidFramework.get_callbacks_registered_by_proc proc_desc tenv in
        (* find the callbacks registered by this procedure and update the list *)
        let registered_callback_procs' = IList.fold_left
            (fun callback_procs callback_typ ->
               match callback_typ with
               | Sil.Tptr (Sil.Tstruct
                             { Sil.struct_name = Some _; def_methods = def_methods'}, _) ->
                   IList.fold_left
                     (fun callback_procs callback_proc ->
                        if Procname.is_constructor callback_proc then callback_procs
                        else Procname.Set.add callback_proc callback_procs)
                     callback_procs
                     def_methods'
               | _ -> callback_procs)
            !registered_callback_procs
            registered_callback_typs in
        registered_callback_procs := registered_callback_procs';
        let _ = if AndroidFramework.is_destroy_method callback_args.Callbacks.proc_name then
            (* compute the set of fields nullified by this procedure *)
            (* TODO (t4959422): get fields that are nullified in callees of the destroy method *)
            fields_nullified :=
              FldSet.union (PatternMatch.get_fields_nullified proc_desc) !fields_nullified in
        if done_checking (IList.length def_methods) then
          do_eradicate_check callback_args
  | _ -> ()


let callback_checker_main
    ({ Callbacks.proc_name } as callback_args) =
  match proc_name with
  | Procname.Java pname_java ->
      callback_checker_main_java pname_java callback_args
  | _ ->
      ()
