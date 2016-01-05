(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging
module F = Format

open Utils

(** Automatically create a harness method to exercise code under test *)

(** given a list [lst] = fst @ (e :: rest), a test predicate [test], and a list [to_insert], returns
    the list fst @ (e :: to_insert) @ rest, where e is the first element such that test(e) evaluates
    to true. if test(e) does not evaluate to true for any element of the list, returns [lst]. *)
let insert_after lst test to_insert =
  let rec insert_rec to_process processed = match to_process with
    | instr :: to_process ->
        let processed' = instr :: processed in
        if test instr then
          IList.append (IList.rev processed') (IList.append to_insert to_process)
        else
          insert_rec to_process processed'
    | [] -> lst in
  insert_rec lst []

(* clear indicator of artificiality, as any real field must have a non-negative offset *)
let generated_field_offset = -1

(** Return true if [fieldname] was created by the harness generation *)
let is_generated_field fieldname =
  Ident.fieldname_offset fieldname = generated_field_offset

(** find callees that register callbacks and add instrumentation to extract the callback.
    return the set of new global static fields created to extract callbacks and their types *)
let extract_callbacks procdesc cfg_file cfg tenv harness_name harness_lvar callback_fields =
  (* try to turn a nasty callback name like MyActivity$1 into a nice callback name like
   * Button.OnClickListener[line 7]*)
  let create_descriptive_callback_name callback_typ loc =
    let typ_str = match PatternMatch.type_get_class_name callback_typ with
      | Some mangled -> Mangled.get_mangled mangled
      | None -> Sil.typ_to_string callback_typ in
    let pretty_typ_str =
      if Procname.is_anonymous_inner_class_name typ_str then
        match PatternMatch.type_get_direct_supertypes callback_typ with
        | [] ->
            (* this should never happen since an inner class always has a supertype *)
            assert false
        | l ->
            (* choose to describe this anonymous inner class with one of the interfaces that it
             * implements. translation always places interfaces at the end of the supertypes list *)
            Typename.name (IList.hd (IList.rev l))
      else typ_str in
    Mangled.from_string (pretty_typ_str ^ "[line " ^ Location.to_string loc ^ "]") in
  let create_instrumentation_fields created_flds node instr = match instr with
    | Sil.Call([], Sil.Const (Sil.Cfun callee), args, loc, _) ->
        begin
          match AndroidFramework.get_callback_registered_by callee args tenv with
          | Some (cb_obj, (Sil.Tptr (cb_typ, Sil.Pk_pointer) as ptr_to_cb_typ)) ->
              let callback_fld_name = create_descriptive_callback_name ptr_to_cb_typ loc in
              let created_fld = Ident.create_fieldname callback_fld_name generated_field_offset in
              (* create a function that takes the type of the harness class as an argument and modifies
               * the instruction set with callback extraction code. we do this because we need to know
               * the typ of the harness class before we can write to any of its fields, but we cannot
               * actually create this typ until we know how many fields we are going to create in order
               * to extract callbacks *)
              let mk_field_write harness_class_typ =
                (* create an instruction that writes the registered callback object to a global static
                 * field in the harness class *)
                let fld_write_lhs = Sil.Lfield (harness_lvar, created_fld, harness_class_typ) in
                let extract_cb_instr = Sil.Set (fld_write_lhs, cb_typ, cb_obj, loc) in
                let instrumented_instrs =
                  let node_instrs = Cfg.Node.get_instrs node in
                  insert_after node_instrs (fun e -> e == instr) [extract_cb_instr] in
                Cfg.Node.replace_instrs node instrumented_instrs;
                (cfg_file, cfg) in
              (created_fld, ptr_to_cb_typ, mk_field_write) :: created_flds
          | _ -> created_flds
        end
    | _ -> created_flds in
  Cfg.Procdesc.fold_instrs create_instrumentation_fields callback_fields procdesc

(** find all of the callbacks registered by methods in [lifecycle_trace *)
let find_registered_callbacks lifecycle_trace harness_name proc_file_map tenv =
  (* what would be ideal to do here is to go through every method (transitively) called by a
   * lifecycle method and look for registered callbacks, however, this would need to be a complex
   * iterative process, as detecting callbacks can lead to more methods being called, which in
   * turn can lead to more callbacks being registered. so what we do here is iterate through each
   * file that a lifecycle proc is defined in and collect all callbacks possibly registered in
   * methods in that file. this can err on the side of including too many callbacks (for example, if
   * a callback is registered in a superclass method that is overridden, this scheme would
   * wrongly include it). on the other hand, this will miss callbacks registered in
   * callees of lifecycle methods that aren't in our list of "lifecycle methods files" *)
  (* TODO (t4793988): do something more principled here *)
  let harness_lvar = Sil.Lvar (Sil.mk_pvar_global harness_name) in
  let lifecycle_cfg_files =
    IList.fold_left (fun lifecycle_files (lifecycle_proc, _) ->
        try
          let cfg_fname =
            let source_dir = Inhabit.source_dir_from_name lifecycle_proc proc_file_map in
            DB.source_dir_get_internal_file source_dir ".cfg" in
          DB.FilenameSet.add cfg_fname lifecycle_files
        with Not_found -> lifecycle_files
      ) DB.FilenameSet.empty lifecycle_trace in
  DB.FilenameSet.fold (fun cfg_file registered_callbacks ->
      match Cfg.load_cfg_from_file cfg_file with
      | Some cfg ->
          IList.fold_left (fun registered_callbacks procdesc ->
              extract_callbacks procdesc cfg_file cfg tenv harness_name harness_lvar registered_callbacks
            ) registered_callbacks (Cfg.get_all_procs cfg)
      | None -> registered_callbacks
    ) lifecycle_cfg_files []

(** if [typ] is a lifecycle type, generate a list of (method call, receiver) pairs constituting a
    lifecycle trace *)
let try_create_lifecycle_trace typ lifecycle_typ lifecycle_procs proc_file_map tenv = match typ with
  | Sil.Tstruct(_, _, Csu.Class, Some name, _, methods, _) ->
      let class_name = Typename.TN_csu (Csu.Class, name) in
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

(** get all the callbacks registered in [lifecycle_trace], transform the SIL to "extract" them into
    global static fields belong to the harness so that they are easily callable, and return a list
    of the (field, typ) pairs that we have created for this purpose *)
let extract_callbacks lifecycle_trace harness_procname proc_file_map tenv =
  let harness_name = Mangled.from_string (Procname.to_string harness_procname) in
  let registered_cbs =
    find_registered_callbacks lifecycle_trace harness_name proc_file_map tenv in
  let fields = IList.map (fun (fld, typ, _) -> (fld, typ, [])) registered_cbs in
  (* create a new typ for the harness containing all of the cb extraction vars as static fields *)
  let harness_typ =
    Sil.Tstruct (fields, [], Csu.Class, Some harness_name, [], [harness_procname], []) in
  (* update the tenv with our created harness typ. we don't have to save the tenv to disk here
   * because this is done immediately after harness generation runs in jMain.ml *)
  let harness_class = Typename.TN_csu (Csu.Class, harness_name) in
  Sil.tenv_add tenv harness_class harness_typ;
  let cfgs_to_save =
    IList.fold_left (fun cfgs_to_save (_, _, instrument_sil_f) ->
        (* instrument the cfg's with callback extraction code *)
        let (cfg_file, cfg) = instrument_sil_f harness_typ in
        DB.FilenameMap.add cfg_file cfg cfgs_to_save
      ) DB.FilenameMap.empty registered_cbs in
  (* re-save the cfgs that we've modified by extracting callbacks *)
  DB.FilenameMap.iter
    (fun cfg_file cfg -> Cfg.store_cfg_to_file cfg_file false cfg) cfgs_to_save;
  (* these are all the static fields holding callbacks that should be invoked by the harness *)
  let harness_global = Sil.Lvar (Sil.mk_pvar_global harness_name) in
  IList.map (fun (fld, typ, _) -> (Sil.Lfield (harness_global, fld, harness_typ), typ)) fields

(** generate a harness for each lifecycle type in an Android application *)
let create_android_harness proc_file_map tenv =
  IList.iter (fun (pkg, clazz, lifecycle_methods) ->
      let typ_name = Mangled.from_package_class pkg clazz in
      match AndroidFramework.get_lifecycle_for_framework_typ_opt typ_name lifecycle_methods tenv with
      | Some (framework_typ, framework_procs) ->
          (* iterate through the type environment and generate a lifecycle harness for each subclass of
           * [lifecycle_typ] *)
          Sil.tenv_iter (fun _ typ ->
              match try_create_lifecycle_trace typ framework_typ framework_procs proc_file_map tenv with
              | [] -> ()
              | lifecycle_trace ->
                  (* we have identified an application lifecycle type and created a trace for it. now,
                   * identify the callbacks registered by methods belonging to this type and get the
                   * inhabitation module to create a harness for us *)
                  let harness_procname =
                    let harness_cls_name = PatternMatch.get_type_name typ in
                    Procname.mangled_java (None, harness_cls_name) None "InferGeneratedHarness" [] Procname.Static in
                  let callback_fields =
                    extract_callbacks lifecycle_trace harness_procname proc_file_map tenv in
                  Inhabit.inhabit_trace lifecycle_trace callback_fields harness_procname proc_file_map tenv
            ) tenv
      | None -> ()
    ) AndroidFramework.get_lifecycles

let parse_trace trace = Stacktrace.parse_stack_trace trace

(** Generate a harness method for exe_env and add it to the execution environment *)
let create_harness proc_file_map tenv = create_android_harness proc_file_map tenv
