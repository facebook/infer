(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module MF = MarkupFormatter

let get_source_range an =
  match an with
  | Ctl_parser_types.Decl decl ->
      let decl_info = Clang_ast_proj.get_decl_tuple decl in
      decl_info.Clang_ast_t.di_source_range
  | Ctl_parser_types.Stmt stmt ->
      let stmt_info, _ = Clang_ast_proj.get_stmt_tuple stmt in
      stmt_info.Clang_ast_t.si_source_range


let is_in_main_file translation_unit_context an =
  let file_opt = (fst (get_source_range an)).Clang_ast_t.sl_file in
  match file_opt with
  | None ->
      false
  | Some source_file ->
      SourceFile.equal
        (SourceFile.from_abs_path source_file)
        translation_unit_context.CFrontend_config.source_file


let is_ck_context (context : CLintersContext.context) an =
  context.is_ck_translation_unit
  && is_in_main_file context.translation_unit_context an
  && CGeneral_utils.is_objc_extension context.translation_unit_context


(** Recursively go up the inheritance hierarchy of a given ObjCInterfaceDecl. (Returns false on
    decls other than that one.) *)
let is_component_or_controller_if decl =
  let open CFrontend_config in
  CAst_utils.is_objc_if_descendant decl [ckcomponent_cl; ckcomponentcontroller_cl]


(** True if it's an objc class impl that extends from CKComponent or CKComponentController, false
    otherwise *)
let rec is_component_or_controller_descendant_impl decl =
  match decl with
  | Clang_ast_t.ObjCImplementationDecl _ ->
      is_component_or_controller_if (CAst_utils.get_super_if (Some decl))
  | Clang_ast_t.LinkageSpecDecl (_, decl_list, _) ->
      contains_ck_impl decl_list
  | _ ->
      false


(** Returns true if the passed-in list of decls contains an ObjCImplementationDecl of a descendant
    of CKComponent or CKComponentController.

    Does not recurse into hierarchy. *)
and contains_ck_impl decl_list = List.exists ~f:is_component_or_controller_descendant_impl decl_list

(** An easy way to fix the component kit best practice
    http://componentkit.org/docs/avoid-local-variables.html

    Local variables that are const or const pointers by definition cannot be assigned to after
    declaration, which means the entire class of bugs stemming from value mutation after assignment
    are gone.

    Note we want const pointers, not mutable pointers to const instances.

    OK:

    {v
    const int a;
    int *const b;
    NSString *const c;
    const int *const d;
    v}

    Not OK:

    {v
    const int *z;
    const NSString *y;
    v} *)
let mutable_local_vars_advice context an =
  try
    let rec get_referenced_type (qual_type : Clang_ast_t.qual_type) : Clang_ast_t.decl option =
      let typ_opt = CAst_utils.get_desugared_type qual_type.qt_type_ptr in
      match (typ_opt : Clang_ast_t.c_type option) with
      | Some (ObjCInterfaceType (_, decl_ptr)) | Some (RecordType (_, decl_ptr)) ->
          CAst_utils.get_decl decl_ptr
      | Some (PointerType (_, inner_qual_type))
      | Some (ObjCObjectPointerType (_, inner_qual_type))
      | Some (LValueReferenceType (_, inner_qual_type)) ->
          get_referenced_type inner_qual_type
      | _ ->
          None
    in
    let is_of_whitelisted_type qual_type =
      let cpp_whitelist =
        [ "CKComponentScope"
        ; "FBTrackingNodeScope"
        ; "FBTrackingCodeScope"
        ; "CKComponentContext"
        ; "CKComponentKey"
        ; "UIContext" ]
      in
      let objc_whitelist = ["NSError"] in
      match get_referenced_type qual_type with
      | Some (CXXRecordDecl (_, ndi, _, _, _, _, _, _)) ->
          List.mem ~equal:String.equal cpp_whitelist ndi.ni_name
      | Some (ObjCInterfaceDecl (_, ndi, _, _, _)) ->
          List.mem ~equal:String.equal objc_whitelist ndi.ni_name
      | _ ->
          false
    in
    if is_ck_context context an then
      match an with
      | Ctl_parser_types.Decl
          (Clang_ast_t.VarDecl (decl_info, named_decl_info, qual_type, _) as decl) ->
          let is_const_ref =
            match CAst_utils.get_type qual_type.qt_type_ptr with
            | Some (LValueReferenceType (_, {Clang_ast_t.qt_is_const})) ->
                qt_is_const
            | _ ->
                false
          in
          let is_const = qual_type.qt_is_const || is_const_ref in
          let name_is decl name =
            match decl with
            | Clang_ast_t.VarDecl (_, named_decl_info, _, _) ->
                String.equal name named_decl_info.Clang_ast_t.ni_name
            | _ ->
                false
          in
          let should_not_report_mutable_local =
            CAst_utils.is_syntactically_global_var decl
            || CAst_utils.is_static_local_var decl
            || is_const || is_of_whitelisted_type qual_type || decl_info.di_is_implicit
            || context.CLintersContext.in_for_loop_declaration
            || CAst_utils.is_std_vector qual_type
            || CAst_utils.has_block_attribute decl
            || name_is decl "weakSelf" || name_is decl "strongSelf"
          in
          if should_not_report_mutable_local then None
          else
            Some
              { CIssue.issue_type= IssueType.mutable_local_variable_in_component_file
              ; severity= Advice
              ; mode= On
              ; description=
                  "Local variable "
                  ^ MF.monospaced_to_string named_decl_info.ni_name
                  ^ " should be const to avoid reassignment"
              ; suggestion= Some "Add a const (after the asterisk for pointer types)."
              ; loc= ALUtils.location_from_dinfo context decl_info }
      | _ ->
          None
    else None
  with CFrontend_errors.IncorrectAssumption _ -> None


(** Components should only have one factory method.

    (They could technically have none if they re-use the parent class's factory method.)

    We care about ones that are declared in the interface. In other words, if additional factory
    methods are implementation-only, the rule doesn't catch it. While its existence is probably not
    good, I can't think of any reason there would be factory methods that aren't exposed outside of
    a class is not useful if there's only one public factory method.

    Given n factory methods, the rule should emit n-1 issues. Each issue's location should point to
    the method declaration. *)
let component_with_multiple_factory_methods_advice context an =
  let is_unavailable_attr attr = match attr with `UnavailableAttr _ -> true | _ -> false in
  let is_available_factory_method if_decl (decl : Clang_ast_t.decl) =
    match decl with
    | ObjCMethodDecl (decl_info, _, _) ->
        let unavailable_attrs =
          List.filter ~f:is_unavailable_attr decl_info.Clang_ast_t.di_attributes
        in
        let is_available = List.is_empty unavailable_attrs in
        CAst_utils.is_objc_factory_method ~class_decl:if_decl ~method_decl:(Some decl)
        && is_available
    | _ ->
        false
  in
  let check_interface if_decl =
    match if_decl with
    | Clang_ast_t.ObjCInterfaceDecl (_, _, decls, _, _) ->
        let factory_methods = List.filter ~f:(is_available_factory_method (Some if_decl)) decls in
        List.map
          ~f:(fun meth_decl ->
            { CIssue.issue_type= IssueType.component_with_multiple_factory_methods
            ; severity= Advice
            ; mode= On
            ; description= "Avoid Overrides"
            ; suggestion=
                Some
                  "Instead, always expose all parameters in a single designated initializer and \
                   document which are optional."
            ; loc= ALUtils.location_from_decl context meth_decl } )
          (IList.drop factory_methods 1)
    | _ ->
        assert false
  in
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCImplementationDecl (_, _, _, _, impl_decl_info)) -> (
      let if_decl_opt =
        CAst_utils.get_decl_opt_with_decl_ref_opt impl_decl_info.oidi_class_interface
      in
      match if_decl_opt with Some d when is_ck_context context an -> check_interface d | _ -> [] )
  | _ ->
      []
