(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Translate declarations **)

module L = Logging

open CFrontend_utils

module CFrontend_decl_funct(T: CModule_type.CTranslation) : CModule_type.CFrontend =
struct
  let model_exists procname =
    Specs.summary_exists_in_models procname && not Config.models_mode

  (* Translates the method/function's body into nodes of the cfg. *)
  let add_method trans_unit_ctx tenv cg cfg class_decl_opt procname body has_return_param
      is_objc_method outer_context_opt extra_instrs =
    Logging.out_debug
      "@\n@\n>>---------- ADDING METHOD: '%s' ---------<<@\n@." (Procname.to_string procname);
    try
      (match Cfg.Procdesc.find_from_name cfg procname with
       | Some procdesc ->
           if (Cfg.Procdesc.is_defined procdesc && not (model_exists procname)) then
             (let context =
                CContext.create_context trans_unit_ctx tenv cg cfg procdesc class_decl_opt
                  has_return_param is_objc_method outer_context_opt in
              let start_node = Cfg.Procdesc.get_start_node procdesc in
              let exit_node = Cfg.Procdesc.get_exit_node procdesc in
              Logging.out_debug
                "\n\n>>---------- Start translating body of function: '%s' ---------<<\n@."
                (Procname.to_string procname);
              let meth_body_nodes = T.instructions_trans context body extra_instrs exit_node in
              Cfg.Node.add_locals_ret_declaration start_node (Cfg.Procdesc.get_locals procdesc);
              Cfg.Node.set_succs_exn cfg start_node meth_body_nodes [];
              Cg.add_defined_node (CContext.get_cg context) (Cfg.Procdesc.get_proc_name procdesc))
       | None -> ())
    with
    | Not_found -> ()
    | CTrans_utils.Self.SelfClassException _ ->
        (* this shouldn't happen, because self or [a class] should always be arguments of
           functions. This is to make sure I'm not wrong. *)
        assert false
    | Assert_failure (file, line, column) ->
        Logging.out "Fatal error: exception Assert_failure(%s, %d, %d)\n%!" file line column;
        Cfg.Procdesc.remove cfg procname true;
        CMethod_trans.create_external_procdesc cfg procname is_objc_method None;
        ()

  let function_decl trans_unit_ctx tenv cfg cg func_decl block_data_opt =
    let captured_vars, outer_context_opt =
      match block_data_opt with
      | Some (outer_context, _, _, captured_vars) -> captured_vars, Some outer_context
      | None -> [], None in
    let ms, body_opt, extra_instrs =
      CMethod_trans.method_signature_of_decl trans_unit_ctx tenv func_decl block_data_opt in
    match body_opt with
    | Some body ->
        (* Only in the case the function declaration has a defined body we create a procdesc *)
        let procname = CMethod_signature.ms_get_name ms in
        let return_param_typ_opt = CMethod_signature.ms_get_return_param_typ ms in
        if CMethod_trans.create_local_procdesc
            trans_unit_ctx cfg tenv ms [body] captured_vars false then
          add_method trans_unit_ctx tenv cg cfg CContext.ContextNoCls procname body
            return_param_typ_opt false outer_context_opt extra_instrs
    | None -> ()

  let process_method_decl trans_unit_ctx tenv cg cfg curr_class meth_decl ~is_objc =
    let ms, body_opt, extra_instrs =
      CMethod_trans.method_signature_of_decl trans_unit_ctx tenv meth_decl None in
    match body_opt with
    | Some body ->
        let is_instance = CMethod_signature.ms_is_instance ms in
        let procname = CMethod_signature.ms_get_name ms in
        let is_objc_inst_method = is_instance && is_objc in
        let return_param_typ_opt = CMethod_signature.ms_get_return_param_typ ms in
        if CMethod_trans.create_local_procdesc
            trans_unit_ctx cfg tenv ms [body] [] is_objc_inst_method then
          add_method trans_unit_ctx tenv cg cfg curr_class procname body return_param_typ_opt
            is_objc None extra_instrs
    | None -> ()

  let process_property_implementation cfg trans_unit_ctx obj_c_property_impl_decl_info =
    let property_decl_opt = obj_c_property_impl_decl_info.Clang_ast_t.opidi_property_decl in
    match Ast_utils.get_decl_opt_with_decl_ref property_decl_opt with
    | Some ObjCPropertyDecl (_, _, obj_c_property_decl_info) ->
        let ivar_decl_ref = obj_c_property_decl_info.Clang_ast_t.opdi_ivar_decl in
        (match Ast_utils.get_decl_opt_with_decl_ref ivar_decl_ref with
         | Some ObjCIvarDecl (_, named_decl_info, _, _, _) ->
             let field_name = General_utils.mk_class_field_name named_decl_info in
             let process_accessor pointer ~getter =
               (match Ast_utils.get_decl_opt_with_decl_ref pointer with
                | Some (ObjCMethodDecl (decl_info, _, _) as d) ->
                    let source_range = decl_info.Clang_ast_t.di_source_range in
                    let loc =
                      CLocation.get_sil_location_from_range trans_unit_ctx source_range true in
                    let property_accessor =
                      if getter then
                        Some (ProcAttributes.Objc_getter field_name)
                      else
                        Some (ProcAttributes.Objc_setter field_name) in
                    let procname = General_utils.procname_of_decl trans_unit_ctx d in
                    let attrs = { (ProcAttributes.default procname Config.Clang) with
                                  loc = loc;
                                  objc_accessor = property_accessor; } in
                    ignore (Cfg.Procdesc.create cfg attrs)
                | _ -> ()) in
             process_accessor obj_c_property_decl_info.Clang_ast_t.opdi_getter_method ~getter:true;
             process_accessor obj_c_property_decl_info.Clang_ast_t.opdi_setter_method ~getter:false
         | _ -> ())
    | _ -> ()

  let process_one_method_decl trans_unit_ctx tenv cg cfg curr_class dec =
    let open Clang_ast_t in
    match dec with
    | CXXMethodDecl _ | CXXConstructorDecl _ | CXXConversionDecl _ | CXXDestructorDecl _ ->
        process_method_decl trans_unit_ctx tenv cg cfg curr_class dec ~is_objc:false
    | ObjCMethodDecl _ ->
        process_method_decl trans_unit_ctx tenv cg cfg curr_class dec ~is_objc:true
    | ObjCPropertyImplDecl (_, obj_c_property_impl_decl_info) ->
        process_property_implementation cfg trans_unit_ctx obj_c_property_impl_decl_info
    | EmptyDecl _
    | ObjCIvarDecl _ | ObjCPropertyDecl _ -> ()
    | _ ->
        Logging.out
          "\nWARNING: found Method Declaration '%s' skipped. NEED TO BE FIXED\n\n" (Ast_utils.string_of_decl dec);
        ()

  let process_methods trans_unit_ctx tenv cg cfg curr_class decl_list =
    IList.iter (process_one_method_decl trans_unit_ctx tenv cg cfg curr_class) decl_list

  let should_translate_decl trans_unit_ctx dec decl_trans_context =
    let info = Clang_ast_proj.get_decl_tuple dec in
    CLocation.update_curr_file trans_unit_ctx info;
    let source_range = info.Clang_ast_t.di_source_range in
    let translate_when_used = match dec with
      | Clang_ast_t.FunctionDecl (_, name_info, _, _)
      | Clang_ast_t.CXXMethodDecl (_, name_info, _, _, _) ->
          (* named_decl_info.ni_name has name without template parameters.*)
          (* It makes it possible to capture whole family of function instantiations*)
          (* to be named the same *)
          let name = Ast_utils.get_qualified_name name_info in
          AttributesTable.is_whitelisted_cpp_method name
      | _ -> false in
    let translate_location =
      CLocation.should_translate_lib trans_unit_ctx source_range decl_trans_context
        ~translate_when_used in
    let never_translate_decl = match dec with
      | Clang_ast_t.FunctionDecl (_, name_info, _, _)
      | Clang_ast_t.CXXMethodDecl (_, name_info, _, _, _) ->
          let fun_name = name_info.Clang_ast_t.ni_name in
          Str.string_match (Str.regexp "__infer_skip__" ) fun_name 0
      | _ -> false in
    (not never_translate_decl) && translate_location

  (* Translate one global declaration *)
  let rec translate_one_declaration trans_unit_ctx tenv cg cfg decl_trans_context dec =
    let open Clang_ast_t in

    (* each procedure has different scope: start names from id 0 *)
    Ident.NameGenerator.reset ();

    (if should_translate_decl trans_unit_ctx dec decl_trans_context then
       match dec with
       | FunctionDecl(_, _, _, _) ->
           function_decl trans_unit_ctx tenv cfg cg dec None

       | ObjCInterfaceDecl(_, name_info, decl_list, _, oi_decl_info) ->
           let name = Ast_utils.get_qualified_name name_info in
           let curr_class = ObjcInterface_decl.get_curr_class name oi_decl_info in
           ignore
             (ObjcInterface_decl.interface_declaration CTypes_decl.type_ptr_to_sil_type tenv dec);
           process_methods trans_unit_ctx tenv cg cfg curr_class decl_list

       | ObjCProtocolDecl(_, name_info, decl_list, _, _) ->
           let name = Ast_utils.get_qualified_name name_info in
           let curr_class = CContext.ContextProtocol name in
           ignore (ObjcProtocol_decl.protocol_decl CTypes_decl.type_ptr_to_sil_type tenv dec);
           process_methods trans_unit_ctx tenv cg cfg curr_class decl_list

       | ObjCCategoryDecl(_, name_info, decl_list, _, ocdi) ->
           let name = Ast_utils.get_qualified_name name_info in
           let curr_class = ObjcCategory_decl.get_curr_class_from_category_decl name ocdi in
           ignore (ObjcCategory_decl.category_decl CTypes_decl.type_ptr_to_sil_type tenv dec);
           process_methods trans_unit_ctx tenv cg cfg curr_class decl_list

       | ObjCCategoryImplDecl(_, name_info, decl_list, _, ocidi) ->
           let name = Ast_utils.get_qualified_name name_info in
           let curr_class = ObjcCategory_decl.get_curr_class_from_category_impl name ocidi in
           ignore (ObjcCategory_decl.category_impl_decl CTypes_decl.type_ptr_to_sil_type tenv dec);
           process_methods trans_unit_ctx tenv cg cfg curr_class decl_list;

       | ObjCImplementationDecl(decl_info, _, decl_list, _, idi) ->
           let curr_class = ObjcInterface_decl.get_curr_class_impl idi in
           let class_name = CContext.get_curr_class_name curr_class in
           let type_ptr_to_sil_type = CTypes_decl.type_ptr_to_sil_type in
           ignore (ObjcInterface_decl.interface_impl_declaration type_ptr_to_sil_type tenv dec);
           CMethod_trans.add_default_method_for_class trans_unit_ctx class_name decl_info;
           process_methods trans_unit_ctx tenv cg cfg curr_class decl_list;

       | CXXMethodDecl (decl_info, _, _, _, _)
       | CXXConstructorDecl (decl_info, _, _, _, _)
       | CXXConversionDecl (decl_info, _, _, _, _)
       | CXXDestructorDecl (decl_info, _, _, _, _) ->
           (* di_parent_pointer has pointer to lexical context such as class.*)
           let parent_ptr = Option.get decl_info.Clang_ast_t.di_parent_pointer in
           let class_decl = Ast_utils.get_decl parent_ptr in
           (match class_decl with
            | Some (CXXRecordDecl _)
            | Some (ClassTemplateSpecializationDecl _) when Config.cxx_experimental ->
                let curr_class = CContext.ContextClsDeclPtr parent_ptr in
                process_methods trans_unit_ctx tenv cg cfg curr_class [dec]
            | Some dec ->
                Logging.out "Methods of %s skipped\n" (Ast_utils.string_of_decl dec)
            | None -> ())
       | VarDecl (decl_info, { ni_name }, _, { vdi_is_global; vdi_init_expr })
         when vdi_is_global && Option.is_some vdi_init_expr ->
           (* create a fake procedure that initializes the global variable so that the variable
              initializer can be analyzed by the backend (eg, the SIOF checker) *)
           let procname = Procname.from_string_c_fun (Config.clang_initializer_prefix ^ ni_name) in
           let ms = CMethod_signature.make_ms procname [] Ast_expressions.create_void_type
               [] decl_info.Clang_ast_t.di_source_range false trans_unit_ctx.CFrontend_config.lang
               None None None in
           let stmt_info = { si_pointer = Ast_utils.get_fresh_pointer ();
                             si_source_range = decl_info.di_source_range } in
           let body = Clang_ast_t.DeclStmt (stmt_info, [], [dec]) in
           ignore (CMethod_trans.create_local_procdesc trans_unit_ctx cfg tenv ms [body] [] false);
           add_method trans_unit_ctx tenv cg cfg CContext.ContextNoCls procname body None false
             None []

       | _ -> ());
    let translate = translate_one_declaration trans_unit_ctx tenv cg cfg decl_trans_context in
    match dec with
    (* Note that C and C++ records are treated the same way
       Skip translating implicit struct declarations, unless they have
       full definition (which happens with C++ lambdas) *)
    | ClassTemplateSpecializationDecl (di, _, _, _, decl_list, _, rdi, _, _)
    | CXXRecordDecl (di, _, _, _, decl_list, _, rdi, _)
    | RecordDecl (di, _, _, _, decl_list, _, rdi)
      when (not di.di_is_implicit) || rdi.rdi_is_complete_definition ->
        let is_method_decl decl = match decl with
          | CXXMethodDecl _ | CXXConstructorDecl _ | CXXConversionDecl _
          | CXXDestructorDecl _ | FunctionTemplateDecl _ ->
              true
          | _ -> false in
        let method_decls, no_method_decls = IList.partition is_method_decl decl_list in
        IList.iter translate no_method_decls;
        ignore (CTypes_decl.add_types_from_decl_to_tenv tenv dec);
        IList.iter translate method_decls
    | EnumDecl _ -> ignore (CEnum_decl.enum_decl dec)
    | LinkageSpecDecl (_, decl_list, _) ->
        Logging.out_debug "ADDING: LinkageSpecDecl decl list@\n";
        IList.iter translate decl_list
    | NamespaceDecl (_, _, decl_list, _, _) ->
        IList.iter translate decl_list
    | ClassTemplateDecl (_, _, template_decl_info)
    | FunctionTemplateDecl (_, _, template_decl_info) ->
        let decl_list = template_decl_info.Clang_ast_t.tdi_specializations in
        IList.iter translate decl_list
    | _ -> ()

end
