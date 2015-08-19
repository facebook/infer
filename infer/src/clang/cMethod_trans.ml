(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Methods for creating a procdesc from a method or function declaration *)
(**   and for resolving a method call and finding the right callee *)

open Utils
open CFrontend_utils

module L = Logging

exception Invalid_declaration

(** When the methoc call is MCStatic, means that it is a class method. *)
(** When it is MCVirtual, it means that it is an instance method and that *)
(** the method to be called will be determined at runtime. If it is MCNoVirtual *)
(** it means that it is an instance method but that the method to be called will *)
(** be determined at compile time *)
type method_call_type =
  | MCVirtual
  | MCNoVirtual
  | MCStatic

type function_method_decl_info =
  | Func_decl_info of Clang_ast_t.function_decl_info * string
  | Cpp_Meth_decl_info of Clang_ast_t.function_decl_info * string (* class_name *) * string (* ret_type *)
  | ObjC_Meth_decl_info of Clang_ast_t.obj_c_method_decl_info * string
  | Block_decl_info of Clang_ast_t.block_decl_info * string

let is_instance_method function_method_decl_info is_instance =
  match function_method_decl_info with
  | Func_decl_info (function_decl_info, _) -> false
  | Cpp_Meth_decl_info _ -> true
  | ObjC_Meth_decl_info (method_decl_info, _) ->
      method_decl_info.Clang_ast_t.omdi_is_instance_method
  | Block_decl_info (block_decl_info, _) -> is_instance

let get_class_param function_method_decl_info =
  if (is_instance_method function_method_decl_info false) then
    match function_method_decl_info with
    | Cpp_Meth_decl_info (_, class_name, _) -> [(CFrontend_config.this, class_name, None)]
    | ObjC_Meth_decl_info (_, class_name) -> [(CFrontend_config.self, class_name, None)]
    | _ -> []
  else []

let get_param_decls function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info (function_decl_info, _)
  | Cpp_Meth_decl_info (function_decl_info, _, _) -> function_decl_info.Clang_ast_t.fdi_parameters
  | ObjC_Meth_decl_info (method_decl_info, _) -> method_decl_info.Clang_ast_t.omdi_parameters
  | Block_decl_info (block_decl_info, _) -> block_decl_info.Clang_ast_t.bdi_parameters

let get_parameters function_method_decl_info =
  let par_to_ms_par par =
    match par with
    | Clang_ast_t.ParmVarDecl (decl_info, name_info, qtype, var_decl_info) ->
        let name = name_info.Clang_ast_t.ni_name in
        Printing.log_out "Adding  param '%s' " name;
        Printing.log_out "with pointer %s@." decl_info.Clang_ast_t.di_pointer;
        (name, CTypes.get_type qtype, var_decl_info.Clang_ast_t.vdi_init_expr)
    | _ -> assert false in

  let pars = list_map par_to_ms_par (get_param_decls function_method_decl_info) in
  get_class_param function_method_decl_info @ pars

let get_return_type function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info (_, typ)
  | Cpp_Meth_decl_info (_, _, typ)
  | Block_decl_info (_, typ) -> typ
  | ObjC_Meth_decl_info (method_decl_info, _) ->
      let qt = method_decl_info.Clang_ast_t.omdi_result_type in
      CTypes.get_type qt

let build_method_signature decl_info procname function_method_decl_info is_instance is_anonym_block is_generated =
  let source_range = decl_info.Clang_ast_t.di_source_range in
  let qt = get_return_type function_method_decl_info in
  let is_instance_method = is_instance_method function_method_decl_info is_instance in
  let parameters = get_parameters function_method_decl_info in
  let attributes = decl_info.Clang_ast_t.di_attributes in
  CMethod_signature.make_ms procname parameters qt attributes source_range is_instance_method is_generated

let method_signature_of_decl class_name_opt meth_decl block_data_opt =
  let open Clang_ast_t in
  match meth_decl, block_data_opt, class_name_opt with
  | FunctionDecl (decl_info, name_info, qt, fdi), _, _ ->
      let name = name_info.ni_name in
      let func_decl = Func_decl_info (fdi, CTypes.get_type qt) in
      let procname = General_utils.mk_procname_from_function name (CTypes.get_type qt) in
      let ms = build_method_signature decl_info procname func_decl false false false in
      ms, fdi.Clang_ast_t.fdi_body, fdi.Clang_ast_t.fdi_parameters
  | CXXMethodDecl (decl_info, name_info, qt, fdi), _, Some class_name ->
      let method_name = name_info.Clang_ast_t.ni_name in
      let typ = CTypes.get_type qt in
      let procname = General_utils.mk_procname_from_cpp_method class_name method_name typ in
      let method_decl = Cpp_Meth_decl_info (fdi, class_name, typ)  in
      let ms = build_method_signature decl_info procname method_decl false false false in
      ms, fdi.Clang_ast_t.fdi_body, fdi.Clang_ast_t.fdi_parameters
  | ObjCMethodDecl (decl_info, name_info, mdi), _, Some class_name ->
      let method_name = name_info.ni_name in
      let is_instance = mdi.omdi_is_instance_method in
      let method_kind = Procname.objc_method_kind_of_bool is_instance in
      let procname = General_utils.mk_procname_from_objc_method class_name method_name method_kind in
      let method_decl = ObjC_Meth_decl_info (mdi, class_name) in
      let is_generated = Ast_utils.is_generated name_info in
      let ms = build_method_signature decl_info procname method_decl false false is_generated in
      ms, mdi.omdi_body, mdi.omdi_parameters
  | BlockDecl (decl_info, decl_list, decl_context_info, bdi),
    Some (qt, is_instance, procname, _, _), _ ->
      let func_decl = Block_decl_info (bdi, CTypes.get_type qt) in
      let ms = build_method_signature decl_info procname func_decl is_instance true false in
      ms, bdi.bdi_body, bdi.bdi_parameters
  | _ -> raise Invalid_declaration

let method_signature_of_pointer class_name_opt pointer =
  try
    match Ast_utils.get_decl pointer with
    | Some meth_decl ->
        let ms, _, _ = method_signature_of_decl class_name_opt meth_decl None in
        Some ms
    | None -> None
  with Invalid_declaration -> None

let get_superclass_curr_class context =
  let retrive_super cname super_opt =
    let iname = Sil.TN_csu (Sil.Class, Mangled.from_string cname) in
    Printing.log_out "Checking for superclass = '%s'\n\n%!" (Sil.typename_to_string iname);
    match Sil.tenv_lookup (CContext.get_tenv context) iname with
    | Some Sil.Tstruct(_, _, _, _, (_, super_name):: _, _, _) ->
        Mangled.to_string super_name
    | _ ->
        Printing.log_err "NOT FOUND superclass = '%s'\n\n%!" (Sil.typename_to_string iname);
        (match super_opt with
         | Some super -> super
         | _ -> assert false) in
  match CContext.get_curr_class context with
  | CContext.ContextCls (cname, super_opt, _) ->
      retrive_super cname super_opt
  | CContext.ContextCategory (_, cls) ->
      retrive_super cls None
  | CContext.ContextNoCls
  | CContext.ContextProtocol _ -> assert false

let get_class_selector_instance context obj_c_message_expr_info act_params =
  let selector = obj_c_message_expr_info.Clang_ast_t.omei_selector in
  let pointer = obj_c_message_expr_info.Clang_ast_t.omei_decl_pointer in
  match obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind with
  | `Class qt -> (CTypes.get_type qt, selector, pointer, MCStatic)
  | `Instance ->
      (match act_params with
       | (instance_obj, Sil.Tptr(t, _)):: _
       | (instance_obj, t):: _ ->
           (CTypes.classname_of_type t, selector, pointer, MCVirtual)
       | _ -> assert false)
  | `SuperInstance ->
      let superclass = get_superclass_curr_class context in
      (superclass, selector, pointer, MCNoVirtual)
  | `SuperClass ->
      let superclass = get_superclass_curr_class context in
      (superclass, selector, pointer, MCStatic)

let get_formal_parameters tenv ms =
  let rec defined_parameters pl =
    match pl with
    | [] -> []
    | (name, raw_type, _):: pl' ->
        let qt =
          if (name = CFrontend_config.self && CMethod_signature.ms_is_instance ms) then
            (Ast_expressions.create_pointer_type raw_type)
          else Ast_expressions.create_qual_type raw_type in
        let typ = CTypes_decl.qual_type_to_sil_type tenv qt in
        (name, typ):: defined_parameters pl' in
  defined_parameters (CMethod_signature.ms_get_args ms)

(* Returns a list of captured variables. *)
(* In order to get the right mangled name we search among *)
(* the local variables of the defining function and it's formals.*)
let captured_vars_from_block_info context cvl =
  let formal2captured (s, t) =
    (Mangled.from_string s, t, false) in
  let find lv n =
    try
      list_find (fun (n', _, _) -> Mangled.to_string n' = n) lv
    with Not_found -> Printing.log_err "Trying to find variable %s@." n; assert false in
  let rec f cvl' =
    match cvl' with
    | [] -> []
    | cv:: cvl'' ->
        (match cv.Clang_ast_t.bcv_variable with
         | Some dr ->
             (match dr.Clang_ast_t.dr_name, dr.Clang_ast_t.dr_qual_type with
              | Some name_info, _ ->
                  let n = name_info.Clang_ast_t.ni_name in
                  if n = CFrontend_config.self && not context.CContext.is_instance then []
                  else
                    (let procdesc_formals = Cfg.Procdesc.get_formals context.CContext.procdesc in
                     (Printing.log_err "formals are %s@." (Utils.list_to_string (fun (x, _) -> x) procdesc_formals));
                     let formals = list_map formal2captured procdesc_formals in
                     [find (context.CContext.local_vars @ formals) n])
              | _ -> assert false)
         | None -> []) :: f cvl'' in
  list_flatten (f cvl)

let get_return_type tenv ms =
  let qt = CMethod_signature.ms_get_ret_type ms in
  CTypes_decl.qual_type_to_sil_type tenv
    (Ast_expressions.create_qual_type (CTypes.get_function_return_type qt))

let sil_func_attributes_of_attributes attrs =
  let rec do_translation acc al = match al with
    | [] -> list_rev acc
    | Clang_ast_t.SentinelAttr attribute_info:: tl ->
        let (sentinel, null_pos) = match attribute_info.Clang_ast_t.ai_parameters with
          | a:: b::[] -> (int_of_string a, int_of_string b)
          | _ -> assert false
        in
        do_translation (Sil.FA_sentinel(sentinel, null_pos):: acc) tl
    | _:: tl -> do_translation acc tl in
  do_translation [] attrs

let should_create_procdesc cfg procname defined generated =
  match Cfg.Procdesc.find_from_name cfg procname with
  | Some prevoius_procdesc ->
      let is_defined_previous = Cfg.Procdesc.is_defined prevoius_procdesc in
      let is_generated_previous =
        (Cfg.Procdesc.get_attributes prevoius_procdesc).Sil.is_generated in
      if defined &&
         ((not is_defined_previous) || (generated && is_generated_previous)) then
        (Cfg.Procdesc.remove cfg (Cfg.Procdesc.get_proc_name prevoius_procdesc) true;
         true)
      else false
  | None -> true

(** Creates a procedure description. *)
let create_local_procdesc cfg tenv ms fbody captured is_objc_inst_method =
  let defined = not ((list_length fbody) == 0) in
  let procname = CMethod_signature.ms_get_name ms in
  let pname = Procname.to_string procname in
  let attributes = sil_func_attributes_of_attributes (CMethod_signature.ms_get_attributes ms) in
  let is_generated = CMethod_signature.ms_is_generated ms in
  let create_new_procdesc () =
    let formals = get_formal_parameters tenv ms in
    let captured_str = list_map (fun (s, t, _) -> (Mangled.to_string s, t)) captured in
    (* Captured variables for blocks are treated as parameters *)
    let formals = captured_str @formals in
    let source_range = CMethod_signature.ms_get_loc ms in
    Printing.log_out "\nCreating a new procdesc for function: '%s'\n@." pname;
    let loc_start = CLocation.get_sil_location_from_range source_range true in
    let loc_exit = CLocation.get_sil_location_from_range source_range false in
    let ret_type = get_return_type tenv ms in
    let captured' = list_map (fun (s, t, _) -> (s, t)) captured in
    let procdesc =
      (* This part below is a boilerplate and the following list of        *)
      (* instructions should be moved in the Cfg.Procdesc module     *)
      let open Cfg.Procdesc in
      let proc_attributes =
        {
          Sil.access = Sil.Default;
          Sil.exceptions = [];
          Sil.is_abstract = false;
          Sil.is_bridge_method = false;
          Sil.is_objc_instance_method = is_objc_inst_method;
          Sil.is_synthetic_method = false;
          Sil.language = Sil.C_CPP;
          Sil.func_attributes = attributes;
          Sil.method_annotation = Sil.method_annotation_empty;
          Sil.is_generated = is_generated;
        } in
      create {
        cfg = cfg;
        name = procname;
        is_defined = defined;
        ret_type = ret_type;
        formals = formals;
        locals = [];
        captured = captured';
        loc = loc_start;
        proc_attributes = proc_attributes;
      } in
    if defined then
      (if !Config.arc_mode then
         Cfg.Procdesc.set_flag procdesc Mleak_buckets.objc_arc_flag "true";
       let start_kind = Cfg.Node.Start_node procdesc in
       let start_node = Cfg.Node.create cfg loc_start start_kind [] procdesc [] in
       let exit_kind = Cfg.Node.Exit_node procdesc in
       let exit_node = Cfg.Node.create cfg loc_exit exit_kind [] procdesc [] in
       Cfg.Procdesc.set_start_node procdesc start_node;
       Cfg.Procdesc.set_exit_node procdesc exit_node) in
  let generated = CMethod_signature.ms_is_generated ms in
  if should_create_procdesc cfg procname defined generated then
    (create_new_procdesc (); true)
  else false

(** Create a procdesc for objc methods whose signature cannot be found. *)
let create_external_procdesc cfg procname is_objc_inst_method type_opt =
  match Cfg.Procdesc.find_from_name cfg procname with
  | Some _ -> ()
  | None ->
      let ret_type, formals =
        (match type_opt with
         | Some (ret_type, arg_types) ->
             ret_type, list_map (fun typ -> ("x", typ)) arg_types
         | None -> Sil.Tvoid, []) in
      let loc = Sil.loc_none in
      let _ =
        let open Cfg.Procdesc in
        let proc_attributes =
          {
            Sil.access = Sil.Default;
            Sil.exceptions = [];
            Sil.is_abstract = false;
            Sil.is_bridge_method = false;
            Sil.is_objc_instance_method = is_objc_inst_method;
            Sil.is_synthetic_method = false;
            Sil.language = Sil.C_CPP;
            Sil.func_attributes = [];
            Sil.method_annotation = Sil.method_annotation_empty;
            Sil.is_generated = false;
          } in
        create {
          cfg = cfg;
          name = procname;
          is_defined = false;
          ret_type = ret_type;
          formals = formals;
          locals = [];
          captured = [];
          loc = loc;
          proc_attributes = proc_attributes;
        } in
      ()

let instance_to_method_call_type instance =
  if instance then MCVirtual
  else MCStatic
