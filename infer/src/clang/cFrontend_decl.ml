(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Translate declarations **)

module L = Logging

module CFrontend_decl_funct(T: CModule_type.CTranslation) : CModule_type.CFrontend =
struct
  let model_exists procname =
    Specs.summary_exists_in_models procname && not Config.models_mode

  (* Translates the method/function's body into nodes of the cfg. *)
  let add_method trans_unit_ctx tenv cg cfg class_decl_opt procname body has_return_param
      is_objc_method outer_context_opt extra_instrs =
    let handle_translation_failure () =
      Cfg.remove_proc_desc cfg procname;
      CMethod_trans.create_external_procdesc cfg procname is_objc_method None
    in
    Logging.out_debug
      "@\n@\n>>---------- ADDING METHOD: '%s' ---------<<@\n@." (Typ.Procname.to_string procname);
    try
      (match Cfg.find_proc_desc_from_name cfg procname with
       | Some procdesc ->
           if (Procdesc.is_defined procdesc && not (model_exists procname)) then
             (let context =
                CContext.create_context trans_unit_ctx tenv cg cfg procdesc class_decl_opt
                  has_return_param is_objc_method outer_context_opt in
              let start_node = Procdesc.get_start_node procdesc in
              let exit_node = Procdesc.get_exit_node procdesc in
              Logging.out_debug
                "\n\n>>---------- Start translating body of function: '%s' ---------<<\n@."
                (Typ.Procname.to_string procname);
              let meth_body_nodes = T.instructions_trans context body extra_instrs exit_node in
              let proc_attributes = Procdesc.get_attributes procdesc in
              Procdesc.Node.add_locals_ret_declaration
                start_node proc_attributes (Procdesc.get_locals procdesc);
              Procdesc.node_set_succs_exn procdesc start_node meth_body_nodes [];
              Cg.add_defined_node (CContext.get_cg context) (Procdesc.get_proc_name procdesc))
       | None -> ())
    with
    | Not_found -> ()
    | CTrans_utils.Self.SelfClassException _ ->
        (* this shouldn't happen, because self or [a class] should always be arguments of
           functions. This is to make sure I'm not wrong. *)
        assert false
    | CTrans_utils.TemplatedCodeException _ ->
        Logging.out "Fatal error: frontend doesn't support translation of templated code\n";
        handle_translation_failure ()
    | Assert_failure (file, line, column) when Config.failures_allowed ->
        Logging.out "Fatal error: exception Assert_failure(%s, %d, %d)\n%!" file line column;
        handle_translation_failure ()

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

  let process_method_decl ?(set_objc_accessor_attr=false) trans_unit_ctx tenv cg cfg
      curr_class meth_decl ~is_objc =
    let ms, body_opt, extra_instrs =
      CMethod_trans.method_signature_of_decl trans_unit_ctx tenv meth_decl None in
    let is_instance = CMethod_signature.ms_is_instance ms in
    let is_objc_inst_method = is_instance && is_objc in
    match body_opt with
    | Some body ->
        let procname = CMethod_signature.ms_get_name ms in
        let return_param_typ_opt = CMethod_signature.ms_get_return_param_typ ms in
        if CMethod_trans.create_local_procdesc ~set_objc_accessor_attr
            trans_unit_ctx cfg tenv ms [body] [] is_objc_inst_method then
          add_method trans_unit_ctx tenv cg cfg curr_class procname body return_param_typ_opt
            is_objc None extra_instrs
    | None ->
        if set_objc_accessor_attr then
          ignore (CMethod_trans.create_local_procdesc ~set_objc_accessor_attr trans_unit_ctx
                    cfg tenv ms [] [] is_objc_inst_method)

  let process_property_implementation trans_unit_ctx tenv cg cfg curr_class
      obj_c_property_impl_decl_info =
    let property_decl_opt = obj_c_property_impl_decl_info.Clang_ast_t.opidi_property_decl in
    match CAst_utils.get_decl_opt_with_decl_ref property_decl_opt with
    | Some ObjCPropertyDecl (_, _, obj_c_property_decl_info) ->
        let process_accessor pointer =
          (match CAst_utils.get_decl_opt_with_decl_ref pointer with
           | Some (ObjCMethodDecl _ as dec) ->
               process_method_decl ~set_objc_accessor_attr:true trans_unit_ctx tenv cg cfg
                 curr_class dec ~is_objc:true
           | _ -> ()) in
        process_accessor obj_c_property_decl_info.Clang_ast_t.opdi_getter_method;
        process_accessor obj_c_property_decl_info.Clang_ast_t.opdi_setter_method
    | _ -> ()

  let process_one_method_decl trans_unit_ctx tenv cg cfg curr_class dec =
    let open Clang_ast_t in
    match dec with
    | CXXMethodDecl _ | CXXConstructorDecl _ | CXXConversionDecl _ | CXXDestructorDecl _ ->
        process_method_decl trans_unit_ctx tenv cg cfg curr_class dec ~is_objc:false
    | ObjCMethodDecl _ ->
        process_method_decl trans_unit_ctx tenv cg cfg curr_class dec ~is_objc:true
    | ObjCPropertyImplDecl (_, obj_c_property_impl_decl_info) ->
        process_property_implementation trans_unit_ctx tenv cg cfg curr_class
          obj_c_property_impl_decl_info
    | EmptyDecl _
    | ObjCIvarDecl _ | ObjCPropertyDecl _ -> ()
    | _ ->
        Logging.out
          "\nWARNING: found Method Declaration '%s' skipped. NEED TO BE FIXED\n\n"
          (Clang_ast_proj.get_decl_kind_string dec);
        ()

  let process_methods trans_unit_ctx tenv cg cfg curr_class decl_list =
    List.iter ~f:(process_one_method_decl trans_unit_ctx tenv cg cfg curr_class) decl_list

  (** Given REVERSED list of method qualifiers (method_name::class_name::rest_quals), return
      whether method should be translated based on method and class whitelists *)
  let is_whitelisted_cpp_method =
    let method_matcher =
      QualifiedCppName.Match.of_fuzzy_qual_names Config.whitelisted_cpp_methods in
    let class_matcher =
      QualifiedCppName.Match.of_fuzzy_qual_names Config.whitelisted_cpp_classes in
    fun qual_method_rev ->
      (* either the method is explictely whitelisted, or the whole class is whitelisted *)
      QualifiedCppName.Match.match_qualifiers method_matcher
        (List.rev qual_method_rev |> QualifiedCppName.of_list) ||
      match qual_method_rev with
      | _::(_::_ as class_name_rev) ->
          (* make sure the class name is not empty; in particular, it cannot be a C function *)
          QualifiedCppName.Match.match_qualifiers class_matcher
            (List.rev class_name_rev |> QualifiedCppName.of_list)
      | _ ->
          false

  let should_translate_decl trans_unit_ctx dec decl_trans_context =
    let info = Clang_ast_proj.get_decl_tuple dec in
    let source_range = info.Clang_ast_t.di_source_range in
    let translate_when_used = match dec with
      | Clang_ast_t.FunctionDecl (_, name_info, _, _)
      | Clang_ast_t.CXXMethodDecl (_, name_info, _, _, _) ->
          is_whitelisted_cpp_method name_info.Clang_ast_t.ni_qual_name
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
    let translate = translate_one_declaration trans_unit_ctx tenv cg cfg decl_trans_context in
    (if should_translate_decl trans_unit_ctx dec decl_trans_context then
       let dec_ptr = (Clang_ast_proj.get_decl_tuple dec).di_pointer in
       match dec with
       | FunctionDecl(_, _, _, _) ->
           function_decl trans_unit_ctx tenv cfg cg dec None

       | ObjCInterfaceDecl(_, _, decl_list, _, _) ->
           let curr_class = CContext.ContextClsDeclPtr dec_ptr in
           ignore
             (ObjcInterface_decl.interface_declaration CType_decl.type_ptr_to_sil_type tenv dec);
           process_methods trans_unit_ctx tenv cg cfg curr_class decl_list

       | ObjCProtocolDecl(_, _, decl_list, _, _) ->
           let curr_class = CContext.ContextClsDeclPtr dec_ptr in
           ignore (ObjcProtocol_decl.protocol_decl CType_decl.type_ptr_to_sil_type tenv dec);
           process_methods trans_unit_ctx tenv cg cfg curr_class decl_list

       | ObjCCategoryDecl(_, _, decl_list, _, _) ->
           let curr_class =  CContext.ContextClsDeclPtr dec_ptr in
           ignore (ObjcCategory_decl.category_decl CType_decl.type_ptr_to_sil_type tenv dec);
           process_methods trans_unit_ctx tenv cg cfg curr_class decl_list

       | ObjCCategoryImplDecl(_, _, decl_list, _, _) ->
           let curr_class = CContext.ContextClsDeclPtr dec_ptr in
           ignore (ObjcCategory_decl.category_impl_decl CType_decl.type_ptr_to_sil_type tenv dec);
           process_methods trans_unit_ctx tenv cg cfg curr_class decl_list;

       | ObjCImplementationDecl(decl_info, _, decl_list, _, _) ->
           let curr_class = CContext.ContextClsDeclPtr dec_ptr in
           let class_typename = CType_decl.get_record_typename ~tenv dec in
           let type_ptr_to_sil_type = CType_decl.type_ptr_to_sil_type in
           ignore (ObjcInterface_decl.interface_impl_declaration type_ptr_to_sil_type tenv dec);
           CMethod_trans.add_default_method_for_class trans_unit_ctx class_typename decl_info;
           process_methods trans_unit_ctx tenv cg cfg curr_class decl_list;

       | CXXMethodDecl (decl_info, _, _, _, _)
       | CXXConstructorDecl (decl_info, _, _, _, _)
       | CXXConversionDecl (decl_info, _, _, _, _)
       | CXXDestructorDecl (decl_info, _, _, _, _) ->
           (* di_parent_pointer has pointer to lexical context such as class.*)
           let parent_ptr = Option.value_exn decl_info.Clang_ast_t.di_parent_pointer in
           let class_decl = CAst_utils.get_decl parent_ptr in
           (match class_decl with
            | Some (CXXRecordDecl _)
            | Some (ClassTemplateSpecializationDecl _) when Config.cxx ->
                let curr_class = CContext.ContextClsDeclPtr parent_ptr in
                process_methods trans_unit_ctx tenv cg cfg curr_class [dec]
            | Some dec ->
                Logging.out "Methods of %s skipped\n" (Clang_ast_proj.get_decl_kind_string dec)
            | None -> ())
       | VarDecl (decl_info, named_decl_info, qt, ({ vdi_is_global; vdi_init_expr } as vdi))
         when vdi_is_global && Option.is_some vdi_init_expr ->
           (* create a fake procedure that initializes the global variable so that the variable
              initializer can be analyzed by the backend (eg, the SIOF checker) *)
           let procname =
             (* create the corresponding global variable to get the right pname for its
                initializer *)
             let global = CGeneral_utils.mk_sil_global_var trans_unit_ctx named_decl_info vdi qt in
             (* safe to Option.get because it's a global *)
             Option.value_exn (Pvar.get_initializer_pname global) in
           let ms = CMethod_signature.make_ms procname [] Ast_expressions.create_void_type
               [] decl_info.Clang_ast_t.di_source_range false trans_unit_ctx.CFrontend_config.lang
               None None None in
           let stmt_info = { si_pointer = CAst_utils.get_fresh_pointer ();
                             si_source_range = decl_info.di_source_range } in
           let body = Clang_ast_t.DeclStmt (stmt_info, [], [dec]) in
           ignore (CMethod_trans.create_local_procdesc trans_unit_ctx cfg tenv ms [body] [] false);
           add_method trans_unit_ctx tenv cg cfg CContext.ContextNoCls procname body None false
             None []
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
           let method_decls, no_method_decls = List.partition_tf ~f:is_method_decl decl_list in
           List.iter ~f:translate no_method_decls;
           ignore (CType_decl.add_types_from_decl_to_tenv tenv dec);
           List.iter ~f:translate method_decls
       | _ -> ());
    match dec with
    | EnumDecl _ -> ignore (CEnum_decl.enum_decl dec)
    | LinkageSpecDecl (_, decl_list, _) ->
        Logging.out_debug "ADDING: LinkageSpecDecl decl list@\n";
        List.iter ~f:translate decl_list
    | NamespaceDecl (_, _, decl_list, _, _) ->
        List.iter ~f:translate decl_list
    | ClassTemplateDecl (_, _, template_decl_info)
    | FunctionTemplateDecl (_, _, template_decl_info) ->
        let decl_list = template_decl_info.Clang_ast_t.tdi_specializations in
        List.iter ~f:translate decl_list
    | _ -> ()

end
