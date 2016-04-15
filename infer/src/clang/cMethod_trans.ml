(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Methods for creating a procdesc from a method or function declaration *)
(**   and for resolving a method call and finding the right callee *)

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
  | Func_decl_info of Clang_ast_t.function_decl_info * Clang_ast_t.type_ptr * CFrontend_config.lang
  | Cpp_Meth_decl_info of Clang_ast_t.function_decl_info * Clang_ast_t.cxx_method_decl_info * string * Clang_ast_t.type_ptr
  | ObjC_Meth_decl_info of Clang_ast_t.obj_c_method_decl_info * string
  | Block_decl_info of Clang_ast_t.block_decl_info * Clang_ast_t.type_ptr * CContext.t

let is_instance_method function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info _  | Block_decl_info _ -> false
  | Cpp_Meth_decl_info (_, method_decl_info, _, _) ->
      not method_decl_info.Clang_ast_t.xmdi_is_static
  | ObjC_Meth_decl_info (method_decl_info, _) ->
      method_decl_info.Clang_ast_t.omdi_is_instance_method

let get_original_return_type function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info (_, typ, _)
  | Cpp_Meth_decl_info (_, _, _, typ)
  | Block_decl_info (_, typ, _) -> CTypes.return_type_of_function_type typ
  | ObjC_Meth_decl_info (method_decl_info, _) -> method_decl_info.Clang_ast_t.omdi_result_type

let get_class_param function_method_decl_info =
  if (is_instance_method function_method_decl_info) then
    match function_method_decl_info with
    | Cpp_Meth_decl_info (_, _, class_name, _) ->
        let class_type = Ast_expressions.create_class_type (class_name, `CPP) in
        [(CFrontend_config.this, class_type)]
    | ObjC_Meth_decl_info (_, class_name) ->
        let class_type = Ast_expressions.create_class_type (class_name, `OBJC) in
        [(CFrontend_config.self, class_type)]
    | _ -> []
  else []


let should_add_return_param return_type ~is_objc_method =
  match return_type with
  | Sil.Tstruct _ -> not is_objc_method
  | _ -> false

let is_objc_method function_method_decl_info =
  match function_method_decl_info with
  | ObjC_Meth_decl_info _ -> true
  | _ -> false

let get_return_param tenv function_method_decl_info =
  let is_objc_method = is_objc_method function_method_decl_info in
  let return_type_ptr = get_original_return_type function_method_decl_info in
  let return_typ = CTypes_decl.type_ptr_to_sil_type tenv return_type_ptr in
  if should_add_return_param return_typ ~is_objc_method then
    [(CFrontend_config.return_param, Ast_expressions.create_pointer_type return_type_ptr)]
  else
    []

let get_param_decls function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info (function_decl_info, _, _)
  | Cpp_Meth_decl_info (function_decl_info, _, _, _) ->
      function_decl_info.Clang_ast_t.fdi_parameters
  | ObjC_Meth_decl_info (method_decl_info, _) -> method_decl_info.Clang_ast_t.omdi_parameters
  | Block_decl_info (block_decl_info, _, _) -> block_decl_info.Clang_ast_t.bdi_parameters

let get_language function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info (_, _, language) -> language
  | Cpp_Meth_decl_info _ -> CFrontend_config.CPP
  | ObjC_Meth_decl_info _ -> CFrontend_config.OBJC
  | Block_decl_info _ -> CFrontend_config.OBJC

let is_cpp_virtual function_method_decl_info =
  match function_method_decl_info with
  | Cpp_Meth_decl_info (_, mdi, _, _) -> mdi.Clang_ast_t.xmdi_is_virtual
  | _ -> false

(** Returns parameters of a function/method. They will have following order:
    1. self/this parameter (optional, only for methods)
    2. normal parameters
    3. return parameter (optional) *)
let get_parameters tenv function_method_decl_info =
  let par_to_ms_par par =
    match par with
    | Clang_ast_t.ParmVarDecl (_, name_info, type_ptr, var_decl_info) ->
        let name = General_utils.get_var_name_string name_info var_decl_info in
        (name, type_ptr)
    | _ -> assert false in
  let pars = IList.map par_to_ms_par (get_param_decls function_method_decl_info) in
  get_class_param function_method_decl_info @ pars @ get_return_param tenv function_method_decl_info

(** get return type of the function and optionally type of function's return parameter *)
let get_return_type tenv function_method_decl_info =
  let return_type_ptr = get_original_return_type function_method_decl_info in
  let return_typ = CTypes_decl.type_ptr_to_sil_type tenv return_type_ptr in
  let is_objc_method = is_objc_method function_method_decl_info in
  if should_add_return_param return_typ ~is_objc_method then
    Ast_expressions.create_void_type, Some (Sil.Tptr (return_typ, Sil.Pk_pointer))
  else return_type_ptr, None

let build_method_signature tenv decl_info procname function_method_decl_info
    parent_pointer pointer_to_property_opt =
  let source_range = decl_info.Clang_ast_t.di_source_range in
  let tp, return_param_type_opt = get_return_type tenv function_method_decl_info in
  let is_instance_method = is_instance_method function_method_decl_info in
  let parameters = get_parameters tenv function_method_decl_info in
  let attributes = decl_info.Clang_ast_t.di_attributes in
  let lang = get_language function_method_decl_info in
  let is_cpp_virtual = is_cpp_virtual function_method_decl_info in
  CMethod_signature.make_ms
    procname parameters tp attributes source_range is_instance_method ~is_cpp_virtual:is_cpp_virtual
    lang parent_pointer pointer_to_property_opt return_param_type_opt

let get_assume_not_null_calls param_decls =
  let do_one_param decl = match decl with
    | Clang_ast_t.ParmVarDecl (decl_info, name, tp, _)
      when CFrontend_utils.Ast_utils.is_type_nonnull tp ->
        let assume_call = Ast_expressions.create_assume_not_null_call decl_info name tp in
        [(`ClangStmt assume_call)]
    | _ -> [] in
  IList.flatten (IList.map do_one_param param_decls)

let get_init_list_instrs method_decl_info =
  let create_custom_instr construct_instr = `CXXConstructorInit construct_instr in
  IList.map create_custom_instr method_decl_info.Clang_ast_t.xmdi_cxx_ctor_initializers

let method_signature_of_decl tenv meth_decl block_data_opt =
  let open Clang_ast_t in
  match meth_decl, block_data_opt with
  | FunctionDecl (decl_info, name_info, tp, fdi), _ ->
      let name = Ast_utils.get_qualified_name name_info in
      let language = !CFrontend_config.language in
      let func_decl = Func_decl_info (fdi, tp, language) in
      let function_info = Some (decl_info, fdi) in
      let procname = General_utils.mk_procname_from_function name function_info tp language in
      let ms = build_method_signature tenv decl_info procname func_decl None None in
      let extra_instrs = get_assume_not_null_calls fdi.Clang_ast_t.fdi_parameters in
      ms, fdi.Clang_ast_t.fdi_body, extra_instrs
  | CXXMethodDecl (decl_info, name_info, tp, fdi, mdi), _
  | CXXConstructorDecl (decl_info, name_info, tp, fdi, mdi), _
  | CXXConversionDecl (decl_info, name_info, tp, fdi, mdi), _
  | CXXDestructorDecl (decl_info, name_info, tp, fdi, mdi), _ ->
      let method_name = Ast_utils.get_unqualified_name name_info in
      let class_name = Ast_utils.get_class_name_from_member name_info in
      let procname = General_utils.mk_procname_from_cpp_method class_name method_name tp in
      let method_decl = Cpp_Meth_decl_info (fdi, mdi, class_name, tp)  in
      let parent_pointer = decl_info.Clang_ast_t.di_parent_pointer in
      let ms = build_method_signature tenv decl_info procname method_decl parent_pointer None in
      let non_null_instrs = get_assume_not_null_calls fdi.Clang_ast_t.fdi_parameters in
      let init_list_instrs = get_init_list_instrs mdi in (* it will be empty for methods *)
      ms, fdi.Clang_ast_t.fdi_body, (init_list_instrs @ non_null_instrs)
  | ObjCMethodDecl (decl_info, name_info, mdi), _ ->
      let method_name = name_info.ni_name in
      let class_name = Ast_utils.get_class_name_from_member name_info in
      let is_instance = mdi.omdi_is_instance_method in
      let method_kind = Procname.objc_method_kind_of_bool is_instance in
      let procname = General_utils.mk_procname_from_objc_method class_name method_name method_kind in
      let method_decl = ObjC_Meth_decl_info (mdi, class_name) in
      let parent_pointer = decl_info.Clang_ast_t.di_parent_pointer in
      let pointer_to_property_opt =
        match mdi.Clang_ast_t.omdi_property_decl with
        | Some decl_ref -> Some decl_ref.Clang_ast_t.dr_decl_pointer
        | None -> None in
      let ms = build_method_signature tenv decl_info procname method_decl
          parent_pointer pointer_to_property_opt in
      let extra_instrs = get_assume_not_null_calls mdi.omdi_parameters in
      ms, mdi.omdi_body, extra_instrs
  | BlockDecl (decl_info, bdi), Some (outer_context, tp, procname, _) ->
      let func_decl = Block_decl_info (bdi, tp, outer_context) in
      let ms = build_method_signature tenv decl_info procname func_decl None None in
      let extra_instrs = get_assume_not_null_calls bdi.bdi_parameters in
      ms, bdi.bdi_body, extra_instrs
  | _ -> raise Invalid_declaration

let method_signature_of_pointer tenv pointer =
  try
    match Ast_utils.get_decl pointer with
    | Some meth_decl ->
        let ms, _, _ = method_signature_of_decl tenv meth_decl None in
        Some ms
    | None -> None
  with Invalid_declaration -> None

let get_method_name_from_clang tenv ms_opt =
  match ms_opt with
  | Some ms ->
      (match Ast_utils.get_decl_opt (CMethod_signature.ms_get_pointer_to_parent ms) with
       | Some decl ->
           if ObjcProtocol_decl.is_protocol decl then None
           else
             (ignore (CTypes_decl.add_types_from_decl_to_tenv tenv decl);
              match ObjcCategory_decl.get_base_class_name_from_category decl with
              | Some class_name ->
                  let procname = CMethod_signature.ms_get_name ms in
                  let new_procname = Procname.replace_class procname class_name in
                  CMethod_signature.ms_set_name ms new_procname;
                  Some ms
              | None -> Some ms)
       | None -> Some ms)
  | None -> None

let get_superclass_curr_class_objc context =
  let retrive_super cname super_opt =
    let iname = Typename.TN_csu (Csu.Class Csu.Objc, Mangled.from_string cname) in
    Printing.log_out "Checking for superclass = '%s'\n\n%!" (Typename.to_string iname);
    match Tenv.lookup (CContext.get_tenv context) iname with
    | Some { Sil.superclasses =  super_name :: _ } ->
        Typename.name super_name
    | _ ->
        Printing.log_err "NOT FOUND superclass = '%s'\n\n%!" (Typename.to_string iname);
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

(* Gets the class name from a method signature found by clang, if search is successful *)
let get_class_name_method_call_from_clang tenv obj_c_message_expr_info =
  match obj_c_message_expr_info.Clang_ast_t.omei_decl_pointer with
  | Some pointer ->
      (match method_signature_of_pointer tenv pointer with
       | Some ms ->
           begin
             match CMethod_signature.ms_get_name ms with
             | Procname.ObjC_Cpp objc_cpp ->
                 Some (Procname.objc_cpp_get_class_name objc_cpp)
             | _ ->
                 None
           end
       | None -> None)
  | None -> None

(* Get class name from a method call accorsing to the info given by the receiver kind  *)
let get_class_name_method_call_from_receiver_kind context obj_c_message_expr_info act_params =
  match obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind with
  | `Class tp ->
      let sil_type = CTypes_decl.type_ptr_to_sil_type context.CContext.tenv tp in
      (CTypes.classname_of_type sil_type)
  | `Instance ->
      (match act_params with
       | (_, Sil.Tptr(t, _)):: _
       | (_, t):: _ -> CTypes.classname_of_type t
       | _ -> assert false)
  | `SuperInstance ->get_superclass_curr_class_objc context
  | `SuperClass -> get_superclass_curr_class_objc context

let get_objc_method_data obj_c_message_expr_info =
  let selector = obj_c_message_expr_info.Clang_ast_t.omei_selector in
  let pointer = obj_c_message_expr_info.Clang_ast_t.omei_decl_pointer in
  match obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind with
  | `Instance -> (selector, pointer, MCVirtual)
  | `SuperInstance -> (selector, pointer, MCNoVirtual)
  | `Class _
  | `SuperClass -> (selector, pointer, MCStatic)

let get_objc_property_accessor tenv ms =
  let open Clang_ast_t in
  let pointer_to_property_opt = CMethod_signature.ms_get_pointer_to_property_opt ms in
  match Ast_utils.get_decl_opt pointer_to_property_opt, CMethod_signature.ms_get_name ms with
  | Some (ObjCPropertyDecl _ as d), Procname.ObjC_Cpp objc_cpp ->
      let class_name = Procname.objc_cpp_get_class_name objc_cpp in
      let field_name = CField_decl.get_property_corresponding_ivar tenv
          CTypes_decl.type_ptr_to_sil_type class_name d in
      if CMethod_signature.ms_is_getter ms then
        Some (ProcAttributes.Objc_getter field_name)
      else if CMethod_signature.ms_is_setter ms then
        Some (ProcAttributes.Objc_setter field_name)
      else None
  | _ -> None

let get_formal_parameters tenv ms =
  let rec defined_parameters pl =
    match pl with
    | [] -> []
    | (name, raw_type):: pl' ->
        let should_add_pointer name ms =
          let is_objc_self = name = CFrontend_config.self &&
                             CMethod_signature.ms_get_lang ms = CFrontend_config.OBJC in
          let is_cxx_this = name = CFrontend_config.this &&
                            CMethod_signature.ms_get_lang ms = CFrontend_config.CPP in
          (is_objc_self && CMethod_signature.ms_is_instance ms) || is_cxx_this in
        let tp = if should_add_pointer name ms then
            (Ast_expressions.create_pointer_type raw_type)
          else raw_type in
        let typ = CTypes_decl.type_ptr_to_sil_type tenv tp in
        (Mangled.from_string name, typ):: defined_parameters pl' in
  defined_parameters (CMethod_signature.ms_get_args ms)

let get_return_type tenv ms =
  let return_type = CMethod_signature.ms_get_ret_type ms in
  CTypes_decl.type_ptr_to_sil_type tenv return_type

let sil_func_attributes_of_attributes attrs =
  let rec do_translation acc al = match al with
    | [] -> IList.rev acc
    | Clang_ast_t.SentinelAttr attribute_info:: tl ->
        let (sentinel, null_pos) = match attribute_info.Clang_ast_t.ai_parameters with
          | a:: b::[] -> (int_of_string a, int_of_string b)
          | _ -> assert false
        in
        do_translation (Sil.FA_sentinel(sentinel, null_pos):: acc) tl
    | _:: tl -> do_translation acc tl in
  do_translation [] attrs

let should_create_procdesc cfg procname defined =
  match Cfg.Procdesc.find_from_name cfg procname with
  | Some prevoius_procdesc ->
      let is_defined_previous = Cfg.Procdesc.is_defined prevoius_procdesc in
      if defined && (not is_defined_previous) then
        (Cfg.Procdesc.remove cfg (Cfg.Procdesc.get_proc_name prevoius_procdesc) true;
         true)
      else false
  | None -> true

let sil_method_annotation_of_args args : Sil.method_annotation =
  let default_visibility = true in
  let mk_annot param_name annot_name =
    let annot = { Sil.class_name = annot_name; Sil.parameters = [param_name]; } in
    annot, default_visibility in
  let arg_to_sil_annot acc (arg_name, type_ptr) =
    if CFrontend_utils.Ast_utils.is_type_nullable type_ptr then
      [mk_annot arg_name Annotations.nullable] :: acc
    else acc in
  let param_annots = IList.fold_left arg_to_sil_annot [] args in
  (* TODO: parse annotations on return value *)
  let retval_annot = [] in
  retval_annot, param_annots

(** Creates a procedure description. *)
let create_local_procdesc cfg tenv ms fbody captured is_objc_inst_method =
  let defined = not ((IList.length fbody) == 0) in
  let proc_name = CMethod_signature.ms_get_name ms in
  let pname = Procname.to_string proc_name in
  let attributes = sil_func_attributes_of_attributes (CMethod_signature.ms_get_attributes ms) in
  let method_annotation =
    sil_method_annotation_of_args (CMethod_signature.ms_get_args ms) in
  let is_cpp_inst_method = CMethod_signature.ms_is_instance ms
                           && CMethod_signature.ms_get_lang ms = CFrontend_config.CPP in
  let create_new_procdesc () =
    let formals = get_formal_parameters tenv ms in
    let captured_str =
      IList.map (fun (var, t) -> (Mangled.from_string (Pvar.to_string var), t)) captured in
    (* Captured variables for blocks are treated as parameters *)
    let formals = captured_str @ formals in
    let source_range = CMethod_signature.ms_get_loc ms in
    Printing.log_out "\nCreating a new procdesc for function: '%s'\n@." pname;
    let loc_start = CLocation.get_sil_location_from_range source_range true in
    let loc_exit = CLocation.get_sil_location_from_range source_range false in
    let ret_type = get_return_type tenv ms in
    let captured' = IList.map (fun (var, t) -> (Pvar.get_name var, t)) captured in
    let procdesc =
      let proc_attributes =
        { (ProcAttributes.default proc_name Config.C_CPP) with
          ProcAttributes.captured = captured';
          ProcAttributes.objc_accessor = get_objc_property_accessor tenv ms;
          formals;
          func_attributes = attributes;
          is_defined = defined;
          is_objc_instance_method = is_objc_inst_method;
          is_cpp_instance_method = is_cpp_inst_method;
          loc = loc_start;
          method_annotation;
          ret_type;
        } in
      Cfg.Procdesc.create cfg proc_attributes in
    if defined then
      (if !Config.arc_mode then
         Cfg.Procdesc.set_flag procdesc Mleak_buckets.objc_arc_flag "true";
       let start_kind = Cfg.Node.Start_node procdesc in
       let start_node = Cfg.Node.create cfg loc_start start_kind [] procdesc [] in
       let exit_kind = Cfg.Node.Exit_node procdesc in
       let exit_node = Cfg.Node.create cfg loc_exit exit_kind [] procdesc [] in
       Cfg.Procdesc.set_start_node procdesc start_node;
       Cfg.Procdesc.set_exit_node procdesc exit_node) in
  if should_create_procdesc cfg proc_name defined then
    (create_new_procdesc (); true)
  else false

(** Create a procdesc for objc methods whose signature cannot be found. *)
let create_external_procdesc cfg proc_name is_objc_inst_method type_opt =
  match Cfg.Procdesc.find_from_name cfg proc_name with
  | Some _ -> ()
  | None ->
      let ret_type, formals =
        (match type_opt with
         | Some (ret_type, arg_types) ->
             ret_type, IList.map (fun typ -> (Mangled.from_string "x", typ)) arg_types
         | None -> Sil.Tvoid, []) in
      let loc = Location.dummy in
      let proc_attributes =
        { (ProcAttributes.default proc_name Config.C_CPP) with
          ProcAttributes.formals;
          is_objc_instance_method = is_objc_inst_method;
          loc;
          ret_type;
        } in
      ignore (Cfg.Procdesc.create cfg proc_attributes)

let create_procdesc_with_pointer context pointer class_name_opt name tp =
  let open CContext in
  match method_signature_of_pointer context.tenv pointer with
  | Some callee_ms ->
      ignore (create_local_procdesc context.cfg context.tenv callee_ms [] [] false);
      CMethod_signature.ms_get_name callee_ms
  | None ->
      let callee_name =
        match class_name_opt with
        | Some class_name ->
            General_utils.mk_procname_from_cpp_method class_name name tp
        | None ->
            General_utils.mk_procname_from_function name None tp !CFrontend_config.language in
      create_external_procdesc context.cfg callee_name false None;
      callee_name

let get_method_for_frontend_checks cfg cg loc =
  let mangled = string_crc_hex32 (DB.source_file_to_string loc.Location.file) in
  let proc_name = Procname.from_string_c_fun ("frontend_checks_" ^ mangled) in
  match Cfg.Procdesc.find_from_name cfg proc_name with
  | Some pdesc -> pdesc
  | None ->
      let attrs = { (ProcAttributes.default proc_name Config.C_CPP) with
                    is_defined = true;
                    loc = loc;
                  } in
      let pdesc = Cfg.Procdesc.create cfg attrs in
      let start_node = Cfg.Node.create cfg loc (Cfg.Node.Start_node pdesc) [] pdesc [] in
      let exit_node = Cfg.Node.create cfg loc (Cfg.Node.Exit_node pdesc) [] pdesc [] in
      Cfg.Procdesc.set_start_node pdesc start_node;
      Cfg.Procdesc.set_exit_node pdesc exit_node;
      Cfg.Node.set_succs_exn cfg start_node [exit_node] [];
      Cg.add_defined_node cg proc_name;
      pdesc

let get_procname_from_cpp_lambda context dec =
  match dec with
  | Clang_ast_t.CXXRecordDecl (_, _, _, _, _, _, _, cxx_rdi) ->
      (match cxx_rdi.xrdi_lambda_call_operator with
       | Some dr ->
           let name_info, decl_ptr, type_ptr = Ast_utils.get_info_from_decl_ref dr in
           create_procdesc_with_pointer context decl_ptr None name_info.ni_name type_ptr
       | _ -> assert false (* We should not get here *))
  | _ -> assert false (* We should not get here *)

(*
let instance_to_method_call_type instance =
  if instance then MCVirtual
  else MCStatic
*)
