(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Translate declarations **)

module L = Logging

module CFrontend_decl_funct (T : CModule_type.CTranslation) : CModule_type.CFrontend = struct
  (** Translates the method/function's body into nodes of the cfg. *)
  let add_method ?(is_destructor_wrapper = false) trans_unit_ctx tenv cfg class_decl_opt procname
      body ms has_return_param outer_context_opt extra_instrs =
    L.(debug Capture Verbose)
      "@\n@\n>>---------- ADDING METHOD: '%a' ---------<<@\n@\n" Procname.pp procname ;
    incr CFrontend_config.procedures_attempted ;
    let recover () =
      incr CFrontend_config.procedures_failed ;
      Procname.Hash.remove cfg procname ;
      let method_kind = ms.CMethodSignature.method_kind in
      CMethod_trans.create_external_procdesc trans_unit_ctx cfg procname method_kind None
    in
    let pp_context fmt () =
      F.fprintf fmt "Aborting translation of method '%a' in file '%a'" Procname.pp procname
        SourceFile.pp trans_unit_ctx.CFrontend_config.source_file
    in
    let f () =
      match Procname.Hash.find_opt cfg procname with
      | Some procdesc when Procdesc.is_defined procdesc && not (BiabductionModels.mem procname) ->
          L.(debug Capture Verbose)
            "@\n@\n>>---------- Start translating body of function: '%s' ---------<<@\n@."
            (Procname.to_string procname) ;
          let context =
            CContext.create_context trans_unit_ctx tenv cfg procdesc class_decl_opt has_return_param
              outer_context_opt
          in
          let context =
            {context with vars_to_destroy= CScope.Variables.compute_vars_to_destroy_map context body}
          in
          let start_node = Procdesc.get_start_node procdesc in
          let exit_node = Procdesc.get_exit_node procdesc in
          let meth_body_nodes =
            T.instructions_trans context body extra_instrs exit_node ~is_destructor_wrapper
          in
          Procdesc.node_set_succs procdesc start_node ~normal:meth_body_nodes ~exn:[]
      | _ ->
          ()
    in
    CFrontend_errors.protect ~f ~recover ~pp_context


  let function_decl trans_unit_ctx tenv cfg func_decl block_data_opt =
    try
      let captured_vars, outer_context_opt =
        match block_data_opt with
        | Some {CModule_type.captured_vars; context= outer_context} ->
            (captured_vars, Some outer_context)
        | None ->
            ([], None)
      in
      let block_as_arg_attributes, procname, block_return_type =
        match block_data_opt with
        | Some {CModule_type.block_as_arg_attributes; procname; return_type} ->
            (block_as_arg_attributes, procname, Some return_type)
        | _ ->
            (None, CType_decl.CProcname.from_decl ~tenv func_decl, None)
      in
      let ms, body_opt, extra_instrs =
        CType_decl.method_signature_body_of_decl tenv func_decl ?block_return_type
          ~block_as_arg_attributes procname
      in
      match body_opt with
      | Some body ->
          (* Only in the case the function declaration has a defined body we create a procdesc *)
          let return_param_typ_opt = ms.CMethodSignature.return_param_typ in
          let loc_instantiated =
            CMethodProperties.get_point_of_instantiation func_decl
            |> Option.map
                 ~f:(CLocation.clang_to_sil_location trans_unit_ctx.CFrontend_config.source_file)
          in
          if
            CMethod_trans.create_local_procdesc ?loc_instantiated trans_unit_ctx cfg tenv ms [body]
              captured_vars
          then
            add_method trans_unit_ctx tenv cfg CContext.ContextNoCls procname body ms
              return_param_typ_opt outer_context_opt extra_instrs
      | None ->
          ()
    with CFrontend_errors.IncorrectAssumption _ -> ()


  let process_method_decl ?(inside_cpp_lambda_expr = false) ?loc_instantiated
      ?(set_objc_accessor_attr = false) ?(is_destructor = false) trans_unit_ctx tenv cfg curr_class
      meth_decl =
    try
      let ms, body_opt, extra_instrs =
        let procname = CType_decl.CProcname.from_decl ~tenv meth_decl in
        CType_decl.method_signature_body_of_decl tenv meth_decl procname
      in
      match body_opt with
      | Some body ->
          let procname = ms.CMethodSignature.name in
          let return_param_typ_opt = ms.CMethodSignature.return_param_typ in
          let is_cpp_lambda_call_operator =
            CMethodProperties.is_cpp_lambda_call_operator meth_decl
          in
          let add_method_if_create_procdesc ms procname ~is_destructor_wrapper =
            if
              (* Do not translate body for lambda operator() if it comes from call expr rather than lambda expr
                 as captured variables will not be translated yet *)
              let body_new =
                if is_cpp_lambda_call_operator && not inside_cpp_lambda_expr then [] else [body]
              in
              CMethod_trans.create_local_procdesc ?loc_instantiated ~set_objc_accessor_attr
                ~is_cpp_lambda_call_operator trans_unit_ctx cfg tenv ms body_new []
            then
              if (not is_cpp_lambda_call_operator) || inside_cpp_lambda_expr then
                add_method trans_unit_ctx tenv cfg curr_class procname body ms return_param_typ_opt
                  None extra_instrs ~is_destructor_wrapper
          in
          ignore (add_method_if_create_procdesc ms procname ~is_destructor_wrapper:is_destructor) ;
          if is_destructor then
            (* For a destructor we create two procedures: a destructor wrapper and an inner destructor *)
            (* A destructor wrapper is called from the outside, i.e. for destructing local variables and fields *)
            (* The destructor wrapper calls the inner destructor which has the actual body *)
            let new_method_name =
              Config.clang_inner_destructor_prefix ^ Procname.get_method procname
            in
            let new_procname = Procname.objc_cpp_replace_method_name procname new_method_name in
            let new_ms = {ms with name= new_procname} in
            add_method_if_create_procdesc new_ms new_procname ~is_destructor_wrapper:false
      | None ->
          if set_objc_accessor_attr then
            ignore
              (CMethod_trans.create_local_procdesc ~set_objc_accessor_attr trans_unit_ctx cfg tenv
                 ms [] [] )
          else
            let proc_attributes =
              CMethod_trans.create_attributes ~set_objc_accessor_attr trans_unit_ctx tenv ms [] []
            in
            Attributes.store ~proc_desc:None proc_attributes ~analysis:false
    with CFrontend_errors.IncorrectAssumption _ -> ()


  let process_property_implementation trans_unit_ctx tenv cfg curr_class
      obj_c_property_impl_decl_info =
    let property_decl_opt = obj_c_property_impl_decl_info.Clang_ast_t.opidi_property_decl in
    match CAst_utils.get_decl_opt_with_decl_ref_opt property_decl_opt with
    | Some (ObjCPropertyDecl (_, _, obj_c_property_decl_info)) ->
        let process_accessor pointer =
          match CAst_utils.get_decl_opt_with_decl_ref_opt pointer with
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


  let process_one_method_decl ~inside_cpp_lambda_expr ~loc_instantiated trans_unit_ctx tenv cfg
      curr_class dec =
    let open Clang_ast_t in
    match dec with
    | CXXMethodDecl _ | CXXConstructorDecl _ | CXXConversionDecl _ ->
        process_method_decl ~inside_cpp_lambda_expr ?loc_instantiated trans_unit_ctx tenv cfg
          curr_class dec
    | CXXDestructorDecl _ ->
        process_method_decl ?loc_instantiated trans_unit_ctx tenv cfg curr_class dec
          ~is_destructor:true
    | ObjCMethodDecl _ ->
        process_method_decl ?loc_instantiated trans_unit_ctx tenv cfg curr_class dec
    | ObjCPropertyImplDecl (_, obj_c_property_impl_decl_info) ->
        process_property_implementation trans_unit_ctx tenv cfg curr_class
          obj_c_property_impl_decl_info
    | EmptyDecl _ | ObjCIvarDecl _ | ObjCPropertyDecl _ | ObjCInterfaceDecl _ ->
        ()
    | AccessSpecDecl _
    | BindingDecl _
    | BlockDecl _
    | BuiltinTemplateDecl _
    | CapturedDecl _
    | ClassTemplateDecl _
    | ClassTemplatePartialSpecializationDecl _
    | ClassTemplateSpecializationDecl _
    | ConceptDecl _
    | ConstructorUsingShadowDecl _
    | CXXDeductionGuideDecl _
    | CXXRecordDecl _
    | DecompositionDecl _
    | EnumConstantDecl _
    | EnumDecl _
    | ExportDecl _
    | ExternCContextDecl _
    | FieldDecl _
    | FileScopeAsmDecl _
    | FriendDecl _
    | FriendTemplateDecl _
    | FunctionDecl _
    | FunctionTemplateDecl _
    | HLSLBufferDecl _
    | ImplicitConceptSpecializationDecl _
    | ImplicitParamDecl _
    | ImportDecl _
    | IndirectFieldDecl _
    | LabelDecl _
    | LifetimeExtendedTemporaryDecl _
    | LinkageSpecDecl _
    | MSGuidDecl _
    | MSPropertyDecl _
    | NamespaceAliasDecl _
    | NamespaceDecl _
    | NonTypeTemplateParmDecl _
    | ObjCAtDefsFieldDecl _
    | ObjCCategoryDecl _
    | ObjCCategoryImplDecl _
    | ObjCCompatibleAliasDecl _
    | ObjCImplementationDecl _
    | ObjCProtocolDecl _
    | ObjCTypeParamDecl _
    | OMPAllocateDecl _
    | OMPCapturedExprDecl _
    | OMPDeclareMapperDecl _
    | OMPDeclareReductionDecl _
    | OMPRequiresDecl _
    | OMPThreadPrivateDecl _
    | ParmVarDecl _
    | PragmaCommentDecl _
    | PragmaDetectMismatchDecl _
    | RecordDecl _
    | RequiresExprBodyDecl _
    | StaticAssertDecl _
    | TemplateParamObjectDecl _
    | TemplateTemplateParmDecl _
    | TemplateTypeParmDecl _
    | TopLevelStmtDecl _
    | TranslationUnitDecl _
    | TypeAliasDecl _
    | TypeAliasTemplateDecl _
    | TypedefDecl _
    | UnnamedGlobalConstantDecl _
    | UnresolvedUsingIfExistsDecl _
    | UnresolvedUsingTypenameDecl _
    | UnresolvedUsingValueDecl _
    | UsingDecl _
    | UsingDirectiveDecl _
    | UsingEnumDecl _
    | UsingPackDecl _
    | UsingShadowDecl _
    | VarDecl _
    | VarTemplateDecl _
    | VarTemplatePartialSpecializationDecl _
    | VarTemplateSpecializationDecl _ ->
        (* TODO: some form of logging *)
        ()


  let process_methods ?(inside_cpp_lambda_expr = false) ?loc_instantiated trans_unit_ctx tenv cfg
      curr_class decl_list =
    List.iter
      ~f:
        (process_one_method_decl ~inside_cpp_lambda_expr ~loc_instantiated trans_unit_ctx tenv cfg
           curr_class )
      decl_list


  (* Here we add an empty dealloc method to every ObjC class if it doesn't have one. Then the implicit
     implementation of such method will be added in CAddImplicitDeallocImpl.process *)
  let create_and_process_dealloc_objc_impl trans_unit_ctx tenv cfg curr_class objc_class_decl_info
      decl_list =
    let open Clang_ast_t in
    let found_dealloc =
      List.exists
        ~f:(fun decl ->
          match decl with
          | ObjCMethodDecl (_, name_info, mdi) ->
              String.equal name_info.ni_name "dealloc" && mdi.Clang_ast_t.omdi_is_instance_method
          | _ ->
              false )
        decl_list
    in
    if not found_dealloc then
      let name_info =
        {ni_name= CFrontend_config.dealloc; ni_qual_name= [CFrontend_config.dealloc]}
      in
      let decl_info =
        { di_pointer= CAst_utils.get_fresh_pointer ()
        ; di_parent_pointer= Some objc_class_decl_info.Clang_ast_t.di_pointer
        ; di_source_range= objc_class_decl_info.Clang_ast_t.di_source_range
        ; di_owning_module= objc_class_decl_info.Clang_ast_t.di_owning_module
        ; di_is_hidden= true
        ; di_is_implicit= true
        ; di_is_used= true
        ; di_is_this_declaration_referenced= false
        ; di_is_invalid_decl= false
        ; di_attributes= []
        ; di_full_comment= None
        ; di_access= `None }
      in
      let obj_c_method_decl_info =
        { omdi_is_instance_method= true
        ; omdi_result_type= Ast_expressions.create_void_type
        ; omdi_is_property_accessor= false
        ; omdi_property_decl= None
        ; omdi_parameters= []
        ; omdi_implicit_parameters= []
        ; omdi_is_variadic= false
        ; omdi_is_overriding= true
        ; omdi_is_optional= false
        ; omdi_body= Some (Clang_ast_t.CompoundStmt (CAst_utils.dummy_stmt_info (), []))
        ; omdi_mangled_name= CFrontend_config.dealloc }
      in
      let method_decl = ObjCMethodDecl (decl_info, name_info, obj_c_method_decl_info) in
      process_method_decl trans_unit_ctx tenv cfg curr_class method_decl


  (** Given REVERSED list of method qualifiers (method_name::class_name::rest_quals), return whether
      method should be translated based on method and class allow lists *)
  let is_allow_listed_cpp_method =
    let method_matcher =
      QualifiedCppName.Match.of_fuzzy_qual_names Config.allow_listed_cpp_methods
    in
    let class_matcher =
      QualifiedCppName.Match.of_fuzzy_qual_names Config.allow_listed_cpp_classes
    in
    fun qual_name ->
      (* either the method is explictely allow listed, or the whole class is allow listed *)
      QualifiedCppName.Match.match_qualifiers method_matcher qual_name
      ||
      match QualifiedCppName.extract_last qual_name with
      | Some (_, class_qual_name) ->
          (* make sure the class name is not empty; in particular, it cannot be a C function *)
          QualifiedCppName.Match.match_qualifiers class_matcher class_qual_name
      | None ->
          false


  let never_translate_decl (dec : Clang_ast_t.decl) =
    match dec with
    | FunctionDecl (_, name_info, _, _)
    | CXXMethodDecl (_, name_info, _, _, _)
    | CXXConstructorDecl (_, name_info, _, _, _)
    | CXXConversionDecl (_, name_info, _, _, _)
    | CXXDestructorDecl (_, name_info, _, _, _) ->
        let fun_name = name_info.Clang_ast_t.ni_name in
        String.is_prefix ~prefix:(Procname.to_string BuiltinDecl.__infer_skip) fun_name
    | _ ->
        false


  let should_store_attributes (dec : Clang_ast_t.decl) = not (never_translate_decl dec)

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
          is_allow_listed_cpp_method (CAst_utils.get_qualified_name name_info)
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
    (not (never_translate_decl dec)) && translate_location


  let is_method_decl (decl : Clang_ast_t.decl) =
    match decl with
    | CXXMethodDecl _
    | CXXConstructorDecl _
    | CXXConversionDecl _
    | CXXDestructorDecl _
    | FunctionTemplateDecl _ ->
        true
    | _ ->
        false


  let is_used_in_file (sl_file_class : Clang_ast_t.source_file option) ctx_source_file =
    Option.exists sl_file_class ~f:(String.equal ctx_source_file)


  let rec store_attributes tenv trans_unit_ctx dec =
    let open Clang_ast_t in
    let store dec = store_attributes tenv trans_unit_ctx dec in
    match dec with
    | CXXMethodDecl (decl_info, _, _, _, _)
    | CXXConstructorDecl (decl_info, _, _, _, _)
    | CXXConversionDecl (decl_info, _, _, _, _)
    | CXXDestructorDecl (decl_info, _, _, _, _) -> (
        let parent_ptr = Option.value_exn decl_info.Clang_ast_t.di_parent_pointer in
        let class_decl = CAst_utils.get_decl parent_ptr in
        let ctx_source_file =
          trans_unit_ctx.CFrontend_config.source_file |> SourceFile.to_abs_path
        in
        match class_decl with
        | Some (ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, _, _, {sl_file}, _))
          when Config.cxx && is_used_in_file sl_file ctx_source_file ->
            let ms =
              let procname = CType_decl.CProcname.from_decl ~tenv dec in
              CType_decl.method_signature_of_decl tenv dec procname
            in
            let proc_attributes =
              CMethod_trans.create_attributes ~set_objc_accessor_attr:false trans_unit_ctx tenv ms
                [] []
            in
            Attributes.store ~proc_desc:None proc_attributes ~analysis:false
        | _ ->
            () )
    | ClassTemplateSpecializationDecl (di, _, _, decl_list, _, _, rdi, _, _, _, _)
    | CXXRecordDecl (di, _, _, decl_list, _, _, rdi, _)
    | RecordDecl (di, _, _, decl_list, _, _, rdi)
      when (not di.di_is_implicit) || rdi.rdi_is_complete_definition ->
        let method_decls = List.filter ~f:is_method_decl decl_list in
        List.iter ~f:store method_decls
    | _ ->
        ()


  (* Translate one global declaration *)
  let rec translate_one_declaration trans_unit_ctx tenv cfg decl_trans_context dec =
    let open Clang_ast_t in
    (* each procedure has different scope: start names from id 0 *)
    Ident.NameGenerator.reset () ;
    let translate dec = translate_one_declaration trans_unit_ctx tenv cfg decl_trans_context dec in
    if should_translate_decl trans_unit_ctx dec decl_trans_context then
      let dec_ptr = (Clang_ast_proj.get_decl_tuple dec).di_pointer in
      match dec with
      | FunctionDecl (_, _, _, _) ->
          function_decl trans_unit_ctx tenv cfg dec None
      | ObjCInterfaceDecl (_, _, decl_list, _, _) ->
          let curr_class = CContext.ContextClsDeclPtr dec_ptr in
          ignore
            (ObjcInterface_decl.interface_declaration CType_decl.qual_type_to_sil_type
               CType_decl.CProcname.from_decl tenv dec ) ;
          process_methods trans_unit_ctx tenv cfg curr_class decl_list
      | ObjCProtocolDecl (_, _, decl_list, _, _) ->
          let curr_class = CContext.ContextClsDeclPtr dec_ptr in
          ignore (ObjcProtocol_decl.protocol_decl CType_decl.qual_type_to_sil_type tenv dec) ;
          process_methods trans_unit_ctx tenv cfg curr_class decl_list
      | ObjCCategoryDecl (_, _, decl_list, _, _) ->
          let curr_class = CContext.ContextClsDeclPtr dec_ptr in
          ignore
            (ObjcCategory_decl.category_decl CType_decl.qual_type_to_sil_type
               CType_decl.CProcname.from_decl tenv dec ) ;
          process_methods trans_unit_ctx tenv cfg curr_class decl_list
      | ObjCCategoryImplDecl (_, _, decl_list, _, _) ->
          let curr_class = CContext.ContextClsDeclPtr dec_ptr in
          ignore
            (ObjcCategory_decl.category_impl_decl CType_decl.qual_type_to_sil_type
               CType_decl.CProcname.from_decl tenv dec ) ;
          process_methods trans_unit_ctx tenv cfg curr_class decl_list
      | ObjCImplementationDecl (objc_class_decl_info, _, decl_list, _, _) ->
          let curr_class = CContext.ContextClsDeclPtr dec_ptr in
          let qual_type_to_sil_type = CType_decl.qual_type_to_sil_type in
          ignore
            (ObjcInterface_decl.interface_impl_declaration qual_type_to_sil_type
               CType_decl.CProcname.from_decl tenv dec ) ;
          process_methods trans_unit_ctx tenv cfg curr_class decl_list ;
          create_and_process_dealloc_objc_impl trans_unit_ctx tenv cfg curr_class
            objc_class_decl_info decl_list
      | CXXMethodDecl (decl_info, _, _, _, _)
      | CXXConstructorDecl (decl_info, _, _, _, _)
      | CXXConversionDecl (decl_info, _, _, _, _)
      | CXXDestructorDecl (decl_info, _, _, _, _) -> (
          let inside_cpp_lambda_expr =
            match decl_trans_context with `CppLambdaExprTranslation -> true | _ -> false
          in
          (* di_parent_pointer has pointer to lexical context such as class.*)
          let parent_ptr = Option.value_exn decl_info.Clang_ast_t.di_parent_pointer in
          let class_decl = CAst_utils.get_decl parent_ptr in
          let loc_instantiated =
            match class_decl with
            | Some
                ( ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, _, _, source_location, _)
                | ClassTemplatePartialSpecializationDecl
                    (_, _, _, _, _, _, _, _, _, source_location, _) ) ->
                Some
                  (CLocation.clang_to_sil_location trans_unit_ctx.CFrontend_config.source_file
                     source_location )
            | _ ->
                None
          in
          match class_decl with
          | (Some (CXXRecordDecl _) | Some (ClassTemplateSpecializationDecl _)) when Config.cxx ->
              let curr_class = CContext.ContextClsDeclPtr parent_ptr in
              process_methods ~inside_cpp_lambda_expr ?loc_instantiated trans_unit_ctx tenv cfg
                curr_class [dec]
          | Some dec ->
              L.(debug Capture Verbose)
                "Methods of %s skipped@\n"
                (Clang_ast_proj.get_decl_kind_string dec)
          | None ->
              () )
      | VarDecl
          (decl_info, named_decl_info, qt, ({vdi_is_global; vdi_init_expr; vdi_is_constexpr} as vdi))
      | VarTemplateSpecializationDecl
          ( _
          , decl_info
          , named_decl_info
          , qt
          , ({vdi_is_global; vdi_init_expr; vdi_is_constexpr} as vdi) )
        when String.is_prefix ~prefix:"__infer_" named_decl_info.ni_name
             || (vdi_is_global && Option.is_some vdi_init_expr) ->
          let template_args_opt =
            match[@warning "-partial-match"] dec with
            | VarDecl _ ->
                None
            | VarTemplateSpecializationDecl (template_args, _, _, _, _) ->
                Some template_args
          in
          (* create a fake procedure that initializes the global variable so that the variable
             initializer can be analyzed by the backend (eg, the SIOF checker) *)
          let procname =
            (* create the corresponding global variable to get the right pname for its
               initializer *)
            let global =
              CVar_decl.mk_sil_global_var tenv trans_unit_ctx decl_info named_decl_info vdi
                template_args_opt qt
            in
            (* safe use of [Option.value_exn] because it's a global *)
            Option.value_exn (Pvar.get_initializer_pname global)
          in
          if
            CMethod_trans.should_create_procdesc cfg procname ~defined:true
              ~set_objc_accessor_attr:false
          then (
            let ms =
              CMethodSignature.mk procname None [] (StdTyp.void, Annot.Item.empty)
                ~is_ret_constexpr:vdi_is_constexpr [] decl_info.Clang_ast_t.di_source_range
                ClangMethodKind.C_FUNCTION None None None `None
            in
            let stmt_info =
              { si_pointer= CAst_utils.get_fresh_pointer ()
              ; si_source_range= decl_info.di_source_range }
            in
            let body = Clang_ast_t.DeclStmt (stmt_info, [], [dec]) in
            ignore (CMethod_trans.create_local_procdesc trans_unit_ctx cfg tenv ms [body] []) ;
            add_method trans_unit_ctx tenv cfg CContext.ContextNoCls procname body ms None None [] )
      (* Note that C and C++ records are treated the same way
         Skip translating implicit struct declarations, unless they have
         full definition (which happens with C++ lambdas) *)
      | ClassTemplateSpecializationDecl (di, _, _, decl_list, _, _, rdi, _, _, _, _)
      | CXXRecordDecl (di, _, _, decl_list, _, _, rdi, _)
      | RecordDecl (di, _, _, decl_list, _, _, rdi)
        when (not di.di_is_implicit) || rdi.rdi_is_complete_definition ->
          let method_decls, no_method_decls = List.partition_tf ~f:is_method_decl decl_list in
          List.iter ~f:translate no_method_decls ;
          CFrontend_errors.protect
            ~f:(fun () -> ignore (CType_decl.add_types_from_decl_to_tenv tenv dec))
            ~recover:(fun () -> ())
            ~pp_context:(fun fmt () ->
              F.fprintf fmt "Error adding types from decl '%a'"
                (Pp.of_string ~f:Clang_ast_j.string_of_decl)
                dec ) ;
          List.iter ~f:translate method_decls
      | _ ->
          ()
    else if should_store_attributes dec then store_attributes tenv trans_unit_ctx dec ;
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
