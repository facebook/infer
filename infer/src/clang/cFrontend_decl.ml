(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Translate declarations **)

module L = Logging

let protect ~f ~recover ~pp_context (trans_unit_ctx : CFrontend_config.translation_unit_context) =
  let log_and_recover ~print fmt =
    recover () ;
    incr CFrontend_config.procedures_failed ;
    (if print then L.internal_error else L.(debug Capture Quiet)) ("%a@\n" ^^ fmt) pp_context ()
  in
  try f () with
  (* Always keep going in case of known limitations of the frontend, crash otherwise (by not
       catching the exception) unless `--keep-going` was passed. Print errors we should fix
       (t21762295) to the console. *)
  | CFrontend_config.Unimplemented e ->
      ClangLogging.log_caught_exception trans_unit_ctx "Unimplemented" e.position e.source_range
        e.ast_node ;
      log_and_recover ~print:false "Unimplemented feature:@\n  %s@\n" e.msg
  | CFrontend_config.IncorrectAssumption e ->
      (* FIXME(t21762295): we do not expect this to happen but it does *)
      ClangLogging.log_caught_exception trans_unit_ctx "IncorrectAssumption" e.position
        e.source_range e.ast_node ;
      log_and_recover ~print:true "Known incorrect assumption in the frontend: %s@\n" e.msg
  | exn ->
      let trace = Backtrace.get () in
      IExn.reraise_if exn ~f:(fun () ->
          L.internal_error "%a: %a@\n%!" pp_context () Exn.pp exn ;
          not Config.keep_going ) ;
      log_and_recover ~print:true "Frontend error: %a@\nBacktrace:@\n%s" Exn.pp exn
        (Backtrace.to_string trace)


module CFrontend_decl_funct (T : CModule_type.CTranslation) : CModule_type.CFrontend = struct
  let model_exists procname = (not Config.models_mode) && Summary.has_model procname

  (** Translates the method/function's body into nodes of the cfg. *)
  let add_method ?(is_destructor_wrapper = false) trans_unit_ctx tenv cfg class_decl_opt procname
      body ms has_return_param outer_context_opt extra_instrs =
    L.(debug Capture Verbose)
      "@\n@\n>>---------- ADDING METHOD: '%a' ---------<<@\n@\n" Typ.Procname.pp procname ;
    incr CFrontend_config.procedures_attempted ;
    let recover () =
      Typ.Procname.Hash.remove cfg procname ;
      let method_kind = ms.CMethodSignature.method_kind in
      CMethod_trans.create_external_procdesc trans_unit_ctx cfg procname method_kind None
    in
    let pp_context fmt () =
      F.fprintf fmt "Aborting translation of method '%a' in file '%a'" Typ.Procname.pp procname
        SourceFile.pp trans_unit_ctx.CFrontend_config.source_file
    in
    let f () =
      match Typ.Procname.Hash.find cfg procname with
      | procdesc when Procdesc.is_defined procdesc && not (model_exists procname) -> (
          let vars_to_destroy = CTrans_utils.Scope.compute_vars_to_destroy body in
          let context =
            CContext.create_context trans_unit_ctx tenv cfg procdesc class_decl_opt
              has_return_param outer_context_opt vars_to_destroy
          in
          let start_node = Procdesc.get_start_node procdesc in
          let exit_node = Procdesc.get_exit_node procdesc in
          L.(debug Capture Verbose)
            "@\n@\n>>---------- Start translating body of function: '%s' ---------<<@\n@."
            (Typ.Procname.to_string procname) ;
          let meth_body_nodes =
            T.instructions_trans context body extra_instrs exit_node ~is_destructor_wrapper
          in
          Procdesc.node_set_succs_exn procdesc start_node meth_body_nodes [] ;
          match Procdesc.is_connected procdesc with
          | Ok () ->
              ()
          | Error broken_node ->
              let lang =
                CFrontend_config.string_of_clang_lang trans_unit_ctx.CFrontend_config.lang
              in
              ClangLogging.log_broken_cfg ~broken_node procdesc __POS__ ~lang )
      | _ ->
          ()
      | exception Caml.Not_found ->
          ()
    in
    protect ~f ~recover ~pp_context trans_unit_ctx


  let function_decl trans_unit_ctx tenv cfg func_decl block_data_opt =
    try
      let captured_vars, outer_context_opt =
        match block_data_opt with
        | Some (outer_context, _, _, captured_vars) ->
            (captured_vars, Some outer_context)
        | None ->
            ([], None)
      in
      let procname, block_return_type =
        match block_data_opt with
        | Some (_, block_return_type, procname, _) ->
            (procname, Some block_return_type)
        | _ ->
            (CType_decl.CProcname.from_decl ~tenv func_decl, None)
      in
      let ms, body_opt, extra_instrs =
        CType_decl.method_signature_body_of_decl tenv func_decl ?block_return_type procname
      in
      match body_opt with
      | Some body ->
          (* Only in the case the function declaration has a defined body we create a procdesc *)
          let return_param_typ_opt = ms.CMethodSignature.return_param_typ in
          if CMethod_trans.create_local_procdesc trans_unit_ctx cfg tenv ms [body] captured_vars
          then
            add_method trans_unit_ctx tenv cfg CContext.ContextNoCls procname body ms
              return_param_typ_opt outer_context_opt extra_instrs
      | None ->
          ()
    with CFrontend_config.IncorrectAssumption e ->
      ClangLogging.log_caught_exception trans_unit_ctx "IncorrectAssumption" e.position
        e.source_range e.ast_node


  let process_method_decl ?(set_objc_accessor_attr = false) ?(is_destructor = false) trans_unit_ctx
      tenv cfg curr_class meth_decl =
    try
      let ms, body_opt, extra_instrs =
        let procname = CType_decl.CProcname.from_decl ~tenv meth_decl in
        CType_decl.method_signature_body_of_decl tenv meth_decl procname
      in
      match body_opt with
      | Some body ->
          let procname = ms.CMethodSignature.name in
          let return_param_typ_opt = ms.CMethodSignature.return_param_typ in
          let ms', procname' =
            if is_destructor then (
              (* For a destructor we create two procedures: a destructor wrapper and an inner destructor *)
              (* A destructor wrapper is called from the outside, i.e. for destructing local variables and fields *)
              (* The destructor wrapper calls the inner destructor which has the actual body *)
              if
                CMethod_trans.create_local_procdesc ~set_objc_accessor_attr trans_unit_ctx cfg tenv
                  ms [body] []
              then
                add_method trans_unit_ctx tenv cfg curr_class procname body ms return_param_typ_opt
                  None extra_instrs ~is_destructor_wrapper:true ;
              let new_method_name =
                Config.clang_inner_destructor_prefix ^ Typ.Procname.get_method procname
              in
              let ms' =
                {ms with name= Typ.Procname.objc_cpp_replace_method_name procname new_method_name}
              in
              let procname' = ms'.CMethodSignature.name in
              (ms', procname') )
            else (ms, procname)
          in
          if
            CMethod_trans.create_local_procdesc ~set_objc_accessor_attr trans_unit_ctx cfg tenv ms'
              [body] []
          then
            add_method trans_unit_ctx tenv cfg curr_class procname' body ms' return_param_typ_opt
              None extra_instrs ~is_destructor_wrapper:false
      | None ->
          if set_objc_accessor_attr then
            ignore
              (CMethod_trans.create_local_procdesc ~set_objc_accessor_attr trans_unit_ctx cfg tenv
                 ms [] [])
    with CFrontend_config.IncorrectAssumption e ->
      ClangLogging.log_caught_exception trans_unit_ctx "IncorrectAssumption" e.position
        e.source_range e.ast_node


  let process_property_implementation trans_unit_ctx tenv cfg curr_class
      obj_c_property_impl_decl_info =
    let property_decl_opt = obj_c_property_impl_decl_info.Clang_ast_t.opidi_property_decl in
    match CAst_utils.get_decl_opt_with_decl_ref property_decl_opt with
    | Some (ObjCPropertyDecl (_, _, obj_c_property_decl_info)) ->
        let process_accessor pointer =
          match CAst_utils.get_decl_opt_with_decl_ref pointer with
          | Some (ObjCMethodDecl _ as dec) ->
              process_method_decl ~set_objc_accessor_attr:true trans_unit_ctx tenv cfg curr_class
                dec
          | _ ->
              ()
        in
        process_accessor obj_c_property_decl_info.Clang_ast_t.opdi_getter_method ;
        process_accessor obj_c_property_decl_info.Clang_ast_t.opdi_setter_method
    | _ ->
        ()


  let process_one_method_decl trans_unit_ctx tenv cfg curr_class dec =
    let open Clang_ast_t in
    match dec with
    | CXXMethodDecl _ | CXXConstructorDecl _ | CXXConversionDecl _ ->
        process_method_decl trans_unit_ctx tenv cfg curr_class dec
    | CXXDestructorDecl _ ->
        process_method_decl trans_unit_ctx tenv cfg curr_class dec ~is_destructor:true
    | ObjCMethodDecl _ ->
        process_method_decl trans_unit_ctx tenv cfg curr_class dec
    | ObjCPropertyImplDecl (_, obj_c_property_impl_decl_info) ->
        process_property_implementation trans_unit_ctx tenv cfg curr_class
          obj_c_property_impl_decl_info
    | EmptyDecl _ | ObjCIvarDecl _ | ObjCPropertyDecl _ | ObjCInterfaceDecl _ ->
        ()
    | AccessSpecDecl _
    | BlockDecl _
    | CapturedDecl _
    | ClassScopeFunctionSpecializationDecl _
    | ExportDecl _
    | ExternCContextDecl _
    | FileScopeAsmDecl _
    | FriendDecl _
    | FriendTemplateDecl _
    | ImportDecl _
    | LinkageSpecDecl _
    | LabelDecl _
    | NamespaceDecl _
    | NamespaceAliasDecl _
    | ObjCCompatibleAliasDecl _
    | ObjCCategoryDecl _
    | ObjCCategoryImplDecl _
    | ObjCImplementationDecl _
    | ObjCProtocolDecl _
    | BuiltinTemplateDecl _
    | ClassTemplateDecl _
    | FunctionTemplateDecl _
    | TypeAliasTemplateDecl _
    | VarTemplateDecl _
    | TemplateTemplateParmDecl _
    | EnumDecl _
    | RecordDecl _
    | CXXRecordDecl _
    | ClassTemplateSpecializationDecl _
    | ClassTemplatePartialSpecializationDecl _
    | TemplateTypeParmDecl _
    | ObjCTypeParamDecl _
    | TypeAliasDecl _
    | TypedefDecl _
    | UnresolvedUsingTypenameDecl _
    | UsingDecl _
    | UsingDirectiveDecl _
    | UsingPackDecl _
    | UsingShadowDecl _
    | ConstructorUsingShadowDecl _
    | BindingDecl _
    | FieldDecl _
    | ObjCAtDefsFieldDecl _
    | FunctionDecl _
    | CXXDeductionGuideDecl _
    | MSPropertyDecl _
    | NonTypeTemplateParmDecl _
    | VarDecl _
    | DecompositionDecl _
    | ImplicitParamDecl _
    | OMPCapturedExprDecl _
    | ParmVarDecl _
    | VarTemplateSpecializationDecl _
    | VarTemplatePartialSpecializationDecl _
    | EnumConstantDecl _
    | IndirectFieldDecl _
    | OMPDeclareReductionDecl _
    | UnresolvedUsingValueDecl _
    | OMPThreadPrivateDecl _
    | PragmaCommentDecl _
    | PragmaDetectMismatchDecl _
    | StaticAssertDecl _
    | TranslationUnitDecl _ ->
        let decl_info = Clang_ast_proj.get_decl_tuple dec in
        ClangLogging.log_unexpected_decl trans_unit_ctx __POS__
          decl_info.Clang_ast_t.di_source_range
          (Some (Clang_ast_proj.get_decl_kind_string dec))


  let process_methods trans_unit_ctx tenv cfg curr_class decl_list =
    List.iter ~f:(process_one_method_decl trans_unit_ctx tenv cfg curr_class) decl_list


  (** Given REVERSED list of method qualifiers (method_name::class_name::rest_quals), return
      whether method should be translated based on method and class whitelists *)
  let is_whitelisted_cpp_method =
    let method_matcher =
      QualifiedCppName.Match.of_fuzzy_qual_names Config.whitelisted_cpp_methods
    in
    let class_matcher =
      QualifiedCppName.Match.of_fuzzy_qual_names Config.whitelisted_cpp_classes
    in
    fun qual_name ->
      (* either the method is explictely whitelisted, or the whole class is whitelisted *)
      QualifiedCppName.Match.match_qualifiers method_matcher qual_name
      ||
      match QualifiedCppName.extract_last qual_name with
      | Some (_, class_qual_name) ->
          (* make sure the class name is not empty; in particular, it cannot be a C function *)
          QualifiedCppName.Match.match_qualifiers class_matcher class_qual_name
      | None ->
          false


  let should_translate_decl trans_unit_ctx (dec : Clang_ast_t.decl) decl_trans_context =
    let info = Clang_ast_proj.get_decl_tuple dec in
    let source_range = info.Clang_ast_t.di_source_range in
    let translate_when_used =
      match dec with
      | FunctionDecl (_, name_info, _, _)
      | CXXMethodDecl (_, name_info, _, _, _)
      | CXXConstructorDecl (_, name_info, _, _, _)
      | CXXConversionDecl (_, name_info, _, _, _)
      | CXXDestructorDecl (_, name_info, _, _, _) ->
          is_whitelisted_cpp_method (CAst_utils.get_qualified_name name_info)
      | _ ->
          false
    in
    let always_translate =
      match dec with
      | VarDecl (_, {ni_name}, _, _) ->
          String.is_prefix ni_name ~prefix:"__infer_"
      | _ ->
          false
    in
    let translate_location =
      always_translate
      || CLocation.should_translate_lib trans_unit_ctx.CFrontend_config.source_file source_range
           decl_trans_context ~translate_when_used
    in
    let never_translate_decl =
      match dec with
      | FunctionDecl (_, name_info, _, _)
      | CXXMethodDecl (_, name_info, _, _, _)
      | CXXConstructorDecl (_, name_info, _, _, _)
      | CXXConversionDecl (_, name_info, _, _, _)
      | CXXDestructorDecl (_, name_info, _, _, _) ->
          let fun_name = name_info.Clang_ast_t.ni_name in
          String.is_prefix ~prefix:"__infer_skip__" fun_name
      | _ ->
          false
    in
    (not never_translate_decl) && translate_location


  (* Translate one global declaration *)
  let rec translate_one_declaration trans_unit_ctx tenv cfg decl_trans_context dec =
    let open Clang_ast_t in
    (* each procedure has different scope: start names from id 0 *)
    Ident.NameGenerator.reset () ;
    let translate dec = translate_one_declaration trans_unit_ctx tenv cfg decl_trans_context dec in
    ( if should_translate_decl trans_unit_ctx dec decl_trans_context then
      let dec_ptr = (Clang_ast_proj.get_decl_tuple dec).di_pointer in
      match dec with
      | FunctionDecl (_, _, _, _) ->
          function_decl trans_unit_ctx tenv cfg dec None
      | ObjCInterfaceDecl (_, _, decl_list, _, _) ->
          let curr_class = CContext.ContextClsDeclPtr dec_ptr in
          ignore
            (ObjcInterface_decl.interface_declaration CType_decl.qual_type_to_sil_type
               CType_decl.CProcname.from_decl tenv dec) ;
          process_methods trans_unit_ctx tenv cfg curr_class decl_list
      | ObjCProtocolDecl (_, _, decl_list, _, _) ->
          let curr_class = CContext.ContextClsDeclPtr dec_ptr in
          ignore (ObjcProtocol_decl.protocol_decl CType_decl.qual_type_to_sil_type tenv dec) ;
          process_methods trans_unit_ctx tenv cfg curr_class decl_list
      | ObjCCategoryDecl (_, _, decl_list, _, _) ->
          let curr_class = CContext.ContextClsDeclPtr dec_ptr in
          ignore
            (ObjcCategory_decl.category_decl CType_decl.qual_type_to_sil_type
               CType_decl.CProcname.from_decl tenv dec) ;
          process_methods trans_unit_ctx tenv cfg curr_class decl_list
      | ObjCCategoryImplDecl (_, _, decl_list, _, _) ->
          let curr_class = CContext.ContextClsDeclPtr dec_ptr in
          ignore
            (ObjcCategory_decl.category_impl_decl CType_decl.qual_type_to_sil_type
               CType_decl.CProcname.from_decl tenv dec) ;
          process_methods trans_unit_ctx tenv cfg curr_class decl_list
      | ObjCImplementationDecl (_, _, decl_list, _, _) ->
          let curr_class = CContext.ContextClsDeclPtr dec_ptr in
          let qual_type_to_sil_type = CType_decl.qual_type_to_sil_type in
          ignore
            (ObjcInterface_decl.interface_impl_declaration qual_type_to_sil_type
               CType_decl.CProcname.from_decl tenv dec) ;
          process_methods trans_unit_ctx tenv cfg curr_class decl_list
      | CXXMethodDecl (decl_info, _, _, _, _)
      | CXXConstructorDecl (decl_info, _, _, _, _)
      | CXXConversionDecl (decl_info, _, _, _, _)
      | CXXDestructorDecl (decl_info, _, _, _, _) -> (
          (* di_parent_pointer has pointer to lexical context such as class.*)
          let parent_ptr = Option.value_exn decl_info.Clang_ast_t.di_parent_pointer in
          let class_decl = CAst_utils.get_decl parent_ptr in
          match class_decl with
          | (Some (CXXRecordDecl _) | Some (ClassTemplateSpecializationDecl _)) when Config.cxx ->
              let curr_class = CContext.ContextClsDeclPtr parent_ptr in
              process_methods trans_unit_ctx tenv cfg curr_class [dec]
          | Some dec ->
              L.(debug Capture Verbose)
                "Methods of %s skipped@\n"
                (Clang_ast_proj.get_decl_kind_string dec)
          | None ->
              () )
      | VarDecl (decl_info, named_decl_info, qt, ({vdi_is_global; vdi_init_expr} as vdi))
        when String.is_prefix ~prefix:"__infer_" named_decl_info.ni_name
             || (vdi_is_global && Option.is_some vdi_init_expr) ->
          (* create a fake procedure that initializes the global variable so that the variable
              initializer can be analyzed by the backend (eg, the SIOF checker) *)
          let procname =
            (* create the corresponding global variable to get the right pname for its
                initializer *)
            let global =
              CGeneral_utils.mk_sil_global_var trans_unit_ctx decl_info named_decl_info vdi qt
            in
            (* safe to Option.get because it's a global *)
            Option.value_exn (Pvar.get_initializer_pname global)
          in
          let ms =
            CMethodSignature.mk procname None [] (Typ.void, Annot.Item.empty) []
              decl_info.Clang_ast_t.di_source_range ClangMethodKind.C_FUNCTION None None None `None
          in
          let stmt_info =
            { si_pointer= CAst_utils.get_fresh_pointer ()
            ; si_source_range= decl_info.di_source_range }
          in
          let body = Clang_ast_t.DeclStmt (stmt_info, [], [dec]) in
          ignore (CMethod_trans.create_local_procdesc trans_unit_ctx cfg tenv ms [body] []) ;
          add_method trans_unit_ctx tenv cfg CContext.ContextNoCls procname body ms None None []
      (* Note that C and C++ records are treated the same way
          Skip translating implicit struct declarations, unless they have
          full definition (which happens with C++ lambdas) *)
      | ClassTemplateSpecializationDecl (di, _, _, decl_list, _, _, rdi, _, _, _)
      | CXXRecordDecl (di, _, _, decl_list, _, _, rdi, _)
      | RecordDecl (di, _, _, decl_list, _, _, rdi)
        when (not di.di_is_implicit) || rdi.rdi_is_complete_definition ->
          let is_method_decl decl =
            match decl with
            | CXXMethodDecl _
            | CXXConstructorDecl _
            | CXXConversionDecl _
            | CXXDestructorDecl _
            | FunctionTemplateDecl _ ->
                true
            | _ ->
                false
          in
          let method_decls, no_method_decls = List.partition_tf ~f:is_method_decl decl_list in
          List.iter ~f:translate no_method_decls ;
          protect
            ~f:(fun () -> ignore (CType_decl.add_types_from_decl_to_tenv tenv dec))
            ~recover:Fn.id
            ~pp_context:(fun fmt () ->
              F.fprintf fmt "Error adding types from decl '%a'"
                (Pp.to_string ~f:Clang_ast_j.string_of_decl)
                dec )
            trans_unit_ctx ;
          List.iter ~f:translate method_decls
      | _ ->
          () ) ;
    match dec with
    | EnumDecl _ ->
        ignore (CEnum_decl.enum_decl dec)
    | LinkageSpecDecl (_, decl_list, _) ->
        L.(debug Capture Verbose) "ADDING: LinkageSpecDecl decl list@\n" ;
        List.iter ~f:translate decl_list
    | NamespaceDecl (_, _, decl_list, _, _) ->
        List.iter ~f:translate decl_list
    | ClassTemplateDecl (_, _, template_decl_info) | FunctionTemplateDecl (_, _, template_decl_info)
      ->
        let decl_list = template_decl_info.Clang_ast_t.tdi_specializations in
        List.iter ~f:translate decl_list
    | _ ->
        ()
end
