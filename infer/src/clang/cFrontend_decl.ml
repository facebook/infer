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

module type CFrontend_decl = sig
  val function_decl : Tenv.t -> Cfg.cfg -> Cg.t -> Clang_ast_t.decl ->
    CModule_type.block_data option -> unit

  val translate_one_declaration :
    Tenv.t -> Cg.t -> Cfg.cfg -> CModule_type.decl_trans_context -> Clang_ast_t.decl -> unit
end

module CFrontend_decl_funct(T: CModule_type.CTranslation) : CFrontend_decl =
struct

  let model_exists procname =
    Specs.summary_exists_in_models procname && not !CFrontend_config.models_mode

  (* Translates the method/function's body into nodes of the cfg. *)
  let add_method tenv cg cfg class_decl_opt procname body has_return_param is_objc_method
      outer_context_opt extra_instrs =
    Printing.log_out
      "\n\n>>---------- ADDING METHOD: '%s' ---------<<\n@." (Procname.to_string procname);
    try
      (match Cfg.Procdesc.find_from_name cfg procname with
       | Some procdesc ->
           if (Cfg.Procdesc.is_defined procdesc && not (model_exists procname)) then
             (let context =
                CContext.create_context tenv cg cfg procdesc class_decl_opt
                  has_return_param is_objc_method outer_context_opt in
              let start_node = Cfg.Procdesc.get_start_node procdesc in
              let exit_node = Cfg.Procdesc.get_exit_node procdesc in
              Printing.log_out
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
        assert false (* this shouldn't happen, because self or [a class] should always be arguments of functions. This is to make sure I'm not wrong. *)
    | Assert_failure (file, line, column) ->
        print_endline ("Fatal error: exception Assert_failure("^
                       file^", "^(string_of_int line)^", "^(string_of_int column)^")");
        Cfg.Procdesc.remove cfg procname true;
        CMethod_trans.create_external_procdesc cfg procname is_objc_method None;
        ()

  let function_decl tenv cfg cg func_decl block_data_opt =
    let captured_vars, outer_context_opt =
      match block_data_opt with
      | Some (outer_context, _, _, captured_vars) -> captured_vars, Some outer_context
      | None -> [], None in
    let ms, body_opt, extra_instrs =
      CMethod_trans.method_signature_of_decl tenv func_decl block_data_opt in
    match body_opt with
    | Some body -> (* Only in the case the function declaration has a defined body we create a procdesc *)
        let procname = CMethod_signature.ms_get_name ms in
        let return_param_typ_opt = CMethod_signature.ms_get_return_param_typ ms in
        if CMethod_trans.create_local_procdesc cfg tenv ms [body] captured_vars false then
          add_method tenv cg cfg CContext.ContextNoCls procname body return_param_typ_opt false
            outer_context_opt extra_instrs
    | None -> ()

  let process_method_decl tenv cg cfg curr_class meth_decl ~is_objc =
    let ms, body_opt, extra_instrs =
      CMethod_trans.method_signature_of_decl tenv meth_decl None in
    match body_opt with
    | Some body ->
        let is_instance = CMethod_signature.ms_is_instance ms in
        let procname = CMethod_signature.ms_get_name ms in
        let is_objc_inst_method = is_instance && is_objc in
        let return_param_typ_opt = CMethod_signature.ms_get_return_param_typ ms in
        if CMethod_trans.create_local_procdesc cfg tenv ms [body] [] is_objc_inst_method then
          add_method tenv cg cfg curr_class procname body return_param_typ_opt is_objc
            None extra_instrs
    | None -> ()

  let process_one_method_decl tenv cg cfg curr_class dec =
    let open Clang_ast_t in
    match dec with
    | CXXMethodDecl _ | CXXConstructorDecl _ | CXXConversionDecl _ | CXXDestructorDecl _ ->
        process_method_decl tenv cg cfg curr_class dec ~is_objc:false
    | ObjCMethodDecl _ ->
        process_method_decl tenv cg cfg curr_class dec ~is_objc:true
    | ObjCPropertyImplDecl _ | EmptyDecl _
    | ObjCIvarDecl _ | ObjCPropertyDecl _ -> ()
    | _ ->
        Printing.log_stats
          "\nWARNING: found Method Declaration '%s' skipped. NEED TO BE FIXED\n\n" (Ast_utils.string_of_decl dec);
        ()

  let process_methods tenv cg cfg curr_class decl_list =
    IList.iter (process_one_method_decl tenv cg cfg curr_class) decl_list

  let should_translate_decl dec decl_trans_context =
    let info = Clang_ast_proj.get_decl_tuple dec in
    CLocation.update_curr_file info;
    let source_range = info.Clang_ast_t.di_source_range in
    let translate_location = CLocation.should_translate_lib source_range decl_trans_context in
    let always_translate_decl = match dec with
      | Clang_ast_t.FunctionDecl (_, name_info, _, _) ->
          (* named_decl_info.ni_name has name without template parameters.*)
          (* It makes it possible to capture whole family of function instantiations*)
          (* to be named the same *)
          let fun_name = name_info.Clang_ast_t.ni_name in
          let top_qual = IList.hd (IList.rev name_info.Clang_ast_t.ni_qual_name) in
          (* Always translate std::move so that it can be analyzed *)
          top_qual="std" && fun_name = "move"
      | _ -> false in
    translate_location || always_translate_decl

  (* Translate one global declaration *)
  let rec translate_one_declaration tenv cg cfg decl_trans_context dec =
    let open Clang_ast_t in
    (* Run the frontend checkers on this declaration *)
    if decl_trans_context = `DeclTraversal then
      CFrontend_errors.run_frontend_checkers_on_decl cfg cg dec;

    (* each procedure has different scope: start names from id 0 *)
    Ident.NameGenerator.reset ();

    (if should_translate_decl dec decl_trans_context then
       match dec with
       | FunctionDecl(_, _, _, _) ->
           function_decl tenv cfg cg dec None

       | ObjCInterfaceDecl(_, name_info, decl_list, _, oi_decl_info) ->
           let name = Ast_utils.get_qualified_name name_info in
           let curr_class = ObjcInterface_decl.get_curr_class name oi_decl_info in
           ignore
             (ObjcInterface_decl.interface_declaration CTypes_decl.type_ptr_to_sil_type tenv dec);
           process_methods tenv cg cfg curr_class decl_list

       | ObjCProtocolDecl(_, name_info, decl_list, _, _) ->
           let name = Ast_utils.get_qualified_name name_info in
           let curr_class = CContext.ContextProtocol name in
           ignore (ObjcProtocol_decl.protocol_decl CTypes_decl.type_ptr_to_sil_type tenv dec);
           process_methods tenv cg cfg curr_class decl_list

       | ObjCCategoryDecl(_, name_info, decl_list, _, ocdi) ->
           let name = Ast_utils.get_qualified_name name_info in
           let curr_class = ObjcCategory_decl.get_curr_class_from_category_decl name ocdi in
           ignore (ObjcCategory_decl.category_decl CTypes_decl.type_ptr_to_sil_type tenv dec);
           process_methods tenv cg cfg curr_class decl_list

       | ObjCCategoryImplDecl(_, name_info, decl_list, _, ocidi) ->
           let name = Ast_utils.get_qualified_name name_info in
           let curr_class = ObjcCategory_decl.get_curr_class_from_category_impl name ocidi in
           ignore (ObjcCategory_decl.category_impl_decl CTypes_decl.type_ptr_to_sil_type tenv dec);
           process_methods tenv cg cfg curr_class decl_list;

       | ObjCImplementationDecl(_, _, decl_list, _, idi) ->
           let curr_class = ObjcInterface_decl.get_curr_class_impl idi in
           let type_ptr_to_sil_type = CTypes_decl.type_ptr_to_sil_type in
           ignore (ObjcInterface_decl.interface_impl_declaration type_ptr_to_sil_type tenv dec);
           process_methods tenv cg cfg curr_class decl_list;

       | CXXMethodDecl (decl_info, _, _, _, _)
       | CXXConstructorDecl (decl_info, _, _, _, _)
       | CXXConversionDecl (decl_info, _, _, _, _)
       | CXXDestructorDecl (decl_info, _, _, _, _) ->
           (* di_parent_pointer has pointer to lexical context such as class.*)
           let class_decl = match decl_info.Clang_ast_t.di_parent_pointer with
             | Some ptr ->
                 Ast_utils.get_decl ptr
             | None ->
                 assert false in
           (match class_decl with
            | Some (CXXRecordDecl _ as d)
            | Some (ClassTemplateSpecializationDecl _ as d) ->
                let class_name = CTypes_decl.get_record_name d in
                let curr_class = CContext.ContextCls(class_name, None, []) in
                if !CFrontend_config.cxx_experimental then
                  process_methods tenv cg cfg curr_class [dec]
            | Some dec ->
                Printing.log_stats "Methods of %s skipped\n" (Ast_utils.string_of_decl dec)
            | None -> ())
       | _ -> ());
    match dec with
    (* Currently C/C++ record decl treated in the same way *)
    | ClassTemplateSpecializationDecl (_, _, _, _, decl_list, _, _, _)
    | CXXRecordDecl (_, _, _, _, decl_list, _, _, _)
    | RecordDecl (_, _, _, _, decl_list, _, _) ->
        let is_method_decl decl = match decl with
          | CXXMethodDecl _ | CXXConstructorDecl _ | CXXConversionDecl _
          | CXXDestructorDecl _ | FunctionTemplateDecl _ ->
              true
          | _ -> false in
        let method_decls, no_method_decls = IList.partition is_method_decl decl_list in
        IList.iter (translate_one_declaration tenv cg cfg decl_trans_context) no_method_decls;
        ignore (CTypes_decl.add_types_from_decl_to_tenv tenv dec);
        IList.iter (translate_one_declaration tenv cg cfg decl_trans_context) method_decls
    | EnumDecl _ -> ignore (CEnum_decl.enum_decl dec)
    | LinkageSpecDecl (_, decl_list, _) ->
        Printing.log_out "ADDING: LinkageSpecDecl decl list\n";
        IList.iter (translate_one_declaration tenv cg cfg decl_trans_context) decl_list
    | NamespaceDecl (_, _, decl_list, _, _) ->
        IList.iter (translate_one_declaration tenv cg cfg decl_trans_context) decl_list
    | ClassTemplateDecl (_, _, template_decl_info)
    | FunctionTemplateDecl (_, _, template_decl_info) ->
        let decl_list = template_decl_info.Clang_ast_t.tdi_specializations in
        IList.iter (translate_one_declaration tenv cg cfg decl_trans_context) decl_list
    | _ -> ()

end
