(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Types_lexer
module L = Logging

let parsed_type_map : Ctl_parser_types.abs_ctype String.Map.t ref = ref String.Map.empty

let rec objc_class_of_pointer_type type_ptr =
  match CAst_utils.get_type type_ptr with
  | Some (ObjCInterfaceType (_, decl_ptr)) ->
      CAst_utils.get_decl decl_ptr
  | Some (ObjCObjectPointerType (_, inner_qual_type)) ->
      objc_class_of_pointer_type inner_qual_type.qt_type_ptr
  | Some (AttributedType (type_info, _)) -> (
    match type_info.ti_desugared_type with
    | Some type_ptr ->
        objc_class_of_pointer_type type_ptr
    | None ->
        None )
  | _ ->
      None


let receiver_class_method_call an =
  match an with
  | Ctl_parser_types.Stmt (ObjCMessageExpr (_, _, _, obj_c_message_expr_info)) -> (
    match obj_c_message_expr_info.omei_receiver_kind with
    | `Class qt ->
        CAst_utils.get_decl_from_typ_ptr qt.qt_type_ptr
    | _ ->
        None )
  | _ ->
      None


let receiver_instance_method_call an =
  match an with
  | Ctl_parser_types.Stmt (ObjCMessageExpr (_, args, _, obj_c_message_expr_info)) -> (
    match obj_c_message_expr_info.omei_receiver_kind with
    | `Instance -> (
      match args with
      | receiver :: _ -> (
        match Clang_ast_proj.get_expr_tuple receiver with
        | Some (_, _, expr_info) ->
            objc_class_of_pointer_type expr_info.ei_qual_type.qt_type_ptr
        | None ->
            None )
      | [] ->
          None )
    | _ ->
        None )
  | _ ->
      None


let receiver_method_call an =
  match receiver_class_method_call an with
  | Some decl ->
      Some decl
  | None ->
      receiver_instance_method_call an


let declaration_name decl =
  match Clang_ast_proj.get_named_decl_tuple decl with
  | Some (_, ndi) ->
      Some ndi.ni_name
  | None ->
      None


let get_available_attr_ios_sdk an =
  let open Clang_ast_t in
  let rec get_available_attr attrs =
    match attrs with
    | [] ->
        None
    | AvailabilityAttr attr_info :: rest -> (
      match attr_info.ai_parameters with
      | "ios" :: version :: _ ->
          Some
            (String.Search_pattern.replace_all
               (String.Search_pattern.create "_")
               ~in_:version ~with_:".")
      | _ ->
          get_available_attr rest )
    | _ :: rest ->
        get_available_attr rest
  in
  match an with
  | Ctl_parser_types.Decl decl ->
      let decl_info = Clang_ast_proj.get_decl_tuple decl in
      get_available_attr decl_info.di_attributes
  | _ ->
      None


let get_ivar_attributes ivar_decl =
  let open Clang_ast_t in
  match ivar_decl with
  | ObjCIvarDecl (ivar_decl_info, _, _, _, _) -> (
    match CAst_utils.get_property_of_ivar ivar_decl_info.Clang_ast_t.di_pointer with
    | Some (ObjCPropertyDecl (_, _, obj_c_property_decl_info)) ->
        obj_c_property_decl_info.Clang_ast_t.opdi_property_attributes
    | _ ->
        [] )
  | _ ->
      []


(* list of cxx references captured by decl *)
let captured_variables_cxx_ref an =
  let open Clang_ast_t in
  let capture_var_is_cxx_ref reference_captured_vars captured_var =
    let decl_ref_opt = captured_var.Clang_ast_t.bcv_variable in
    match CAst_utils.get_decl_opt_with_decl_ref decl_ref_opt with
    | Some (VarDecl (_, named_decl_info, qual_type, _))
    | Some (ParmVarDecl (_, named_decl_info, qual_type, _))
    | Some (ImplicitParamDecl (_, named_decl_info, qual_type, _)) -> (
      match CAst_utils.get_desugared_type qual_type.Clang_ast_t.qt_type_ptr with
      | Some (RValueReferenceType _) | Some (LValueReferenceType _) ->
          named_decl_info :: reference_captured_vars
      | _ ->
          reference_captured_vars )
    | _ ->
        reference_captured_vars
  in
  match an with
  | Ctl_parser_types.Decl (BlockDecl (_, bdi)) ->
      List.fold ~f:capture_var_is_cxx_ref ~init:[] bdi.bdi_captured_variables
  | _ ->
      []


let objc_block_is_capturing_values an =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.BlockDecl (_, bdi)) ->
      not (List.is_empty bdi.bdi_captured_variables)
  | _ ->
      false


type t = ALVar.formula_id * (* (name, [param1,...,paramK]) *) ALVar.alexp list [@@deriving compare]

let pp_predicate fmt (name_, arglist_) =
  let name = ALVar.formula_id_to_string name_ in
  let arglist = List.map ~f:ALVar.alexp_to_string arglist_ in
  Format.fprintf fmt "%s(%a)" name (Pp.comma_seq Format.pp_print_string) arglist


(* an is a declaration whose name contains a regexp defined by re *)
let declaration_has_name an name =
  match an with
  | Ctl_parser_types.Decl d -> (
    match declaration_name d with
    | Some decl_name ->
        ALVar.compare_str_with_alexp decl_name name
    | _ ->
        false )
  | _ ->
      false


let rec is_subclass_of decl name =
  match CAst_utils.get_superclass_curr_class_objc_from_decl decl with
  | Some super_ref -> (
      let ndi = match super_ref.Clang_ast_t.dr_name with Some ni -> ni | _ -> assert false in
      if ALVar.compare_str_with_alexp ndi.ni_name name then true
      else
        match CAst_utils.get_decl_opt_with_decl_ref (Some super_ref) with
        | Some decl ->
            is_subclass_of decl name
        | None ->
            false )
  | None ->
      false


(* is an objc interface with name expected_name *)
let is_objc_interface_named an expected_name =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCInterfaceDecl _) ->
      declaration_has_name an expected_name
  | _ ->
      false


(* is an objc implementation with name expected_name *)
let is_objc_implementation_named an expected_name =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCImplementationDecl _) ->
      declaration_has_name an expected_name
  | _ ->
      false


let is_objc_class_named an re = is_objc_interface_named an re || is_objc_implementation_named an re

(* is an objc category interface with class name expected_name *)
let is_objc_category_interface_on_class_named an expected_name =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCCategoryDecl (_, _, _, _, ocdi)) -> (
    match CAst_utils.get_decl_opt_with_decl_ref ocdi.odi_class_interface with
    | Some decl_ref ->
        is_objc_interface_named (Decl decl_ref) expected_name
    | _ ->
        false )
  | _ ->
      false


(* is an objc category implementation with class name expected_name *)
let is_objc_category_implementation_on_class_named an expected_name =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCCategoryImplDecl (_, _, _, _, ocidi)) -> (
    match CAst_utils.get_decl_opt_with_decl_ref ocidi.ocidi_class_interface with
    | Some decl_ref ->
        is_objc_interface_named (Decl decl_ref) expected_name
    | _ ->
        false )
  | _ ->
      false


let is_objc_category_on_class_named an re =
  is_objc_category_interface_on_class_named an re
  || is_objc_category_implementation_on_class_named an re


(* is an objc category interface with superclass name expected_name *)
let is_objc_category_interface_on_subclass_of an expected_name =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCCategoryDecl (_, _, _, _, ocdi)) -> (
    match CAst_utils.get_decl_opt_with_decl_ref ocdi.odi_class_interface with
    | Some decl_ref ->
        is_subclass_of decl_ref expected_name
    | _ ->
        false )
  | _ ->
      false


(* is an objc category implementation with superclass name expected_name *)
let is_objc_category_implementation_on_subclass_of an expected_name =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCCategoryImplDecl (_, _, _, _, ocidi)) -> (
    match CAst_utils.get_decl_opt_with_decl_ref ocidi.ocidi_class_interface with
    | Some decl_ref ->
        is_subclass_of decl_ref expected_name
    | _ ->
        false )
  | _ ->
      false


let is_objc_category_on_subclass_of an expected_name =
  is_objc_category_interface_on_subclass_of an expected_name
  || is_objc_category_implementation_on_subclass_of an expected_name


(* is an objc category interface with name expected_name *)
let is_objc_category_interface_named an expected_name =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCCategoryDecl _) ->
      declaration_has_name an expected_name
  | _ ->
      false


(* is an objc category implementation with name expected_name *)
let is_objc_category_implementation_named an expected_name =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCCategoryImplDecl _) ->
      declaration_has_name an expected_name
  | _ ->
      false


let is_objc_category_named an re =
  is_objc_category_interface_named an re || is_objc_category_implementation_named an re


let is_objc_protocol_named an re =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCProtocolDecl _) ->
      declaration_has_name an re
  | _ ->
      false


let is_objc_class_method_named an name =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCMethodDecl (_, _, omdi)) ->
      declaration_has_name an name && not omdi.omdi_is_instance_method
  | _ ->
      false


let is_objc_instance_method_named an name =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCMethodDecl (_, _, omdi)) ->
      declaration_has_name an name && omdi.omdi_is_instance_method
  | _ ->
      false


let is_objc_method_named an name =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCMethodDecl _) ->
      declaration_has_name an name
  | _ ->
      false


let is_objc_method_overriding an =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCMethodDecl (_, _, mdi)) ->
      mdi.omdi_is_overriding
  | _ ->
      false


let decl_list_has_objc_method decl_list method_name is_instance_method =
  List.exists
    ~f:(fun decl ->
      match decl with
      | Clang_ast_t.ObjCMethodDecl (_, ni, omdi) ->
          Bool.equal omdi.omdi_is_instance_method is_instance_method
          && String.equal ni.ni_name method_name
      | _ ->
          false )
    decl_list


let current_objc_container context =
  let open CLintersContext in
  let current_objc_class = context.current_objc_class in
  let current_objc_category = context.current_objc_category in
  let current_objc_protocol = context.current_objc_protocol in
  if not (Option.is_none current_objc_class) then current_objc_class
  else if not (Option.is_none current_objc_category) then current_objc_category
  else if not (Option.is_none current_objc_protocol) then current_objc_protocol
  else None


let is_objc_method_exposed context an =
  let open Clang_ast_t in
  if is_objc_method_overriding an then true
  else
    match an with
    | Ctl_parser_types.Decl (ObjCMethodDecl (_, ndi, mdi)) -> (
        let method_name = ndi.ni_name in
        let is_instance_method = mdi.omdi_is_instance_method in
        match current_objc_container context with
        | Some (ObjCImplementationDecl (_, _, _, _, oidi)) -> (
          match CAst_utils.get_decl_opt_with_decl_ref oidi.oidi_class_interface with
          | Some (ObjCInterfaceDecl (_, _, decl_list, _, otdi)) ->
              decl_list_has_objc_method decl_list method_name is_instance_method
              || List.exists
                   ~f:(fun decl_ref ->
                     match CAst_utils.get_decl decl_ref.dr_decl_pointer with
                     | Some (ObjCCategoryDecl (_, ni, decl_list, _, _)) ->
                         String.equal ni.ni_name ""
                         && decl_list_has_objc_method decl_list method_name is_instance_method
                     | _ ->
                         false )
                   otdi.otdi_known_categories
          | _ ->
              false )
        | Some (ObjCCategoryImplDecl (_, _, _, _, ocidi)) -> (
          match CAst_utils.get_decl_opt_with_decl_ref ocidi.ocidi_category_decl with
          | Some (ObjCCategoryDecl (_, _, decl_list, _, _)) ->
              decl_list_has_objc_method decl_list method_name is_instance_method
          | _ ->
              false )
        | _ ->
            false )
    | _ ->
        false


let get_selector an =
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.ObjCMessageExpr (_, _, _, omei)) ->
      Some omei.omei_selector
  | _ ->
      None


let receiver_objc_type_name an =
  match an with
  | Ctl_parser_types.Stmt (ObjCMessageExpr (_, receiver :: _, _, {omei_receiver_kind= `Instance}))
    ->
      Clang_ast_proj.get_expr_tuple receiver
      |> Option.bind ~f:(fun (_, _, expr_info) ->
             CAst_utils.name_opt_of_typedef_qual_type expr_info.Clang_ast_t.ei_qual_type )
      |> Option.map ~f:QualifiedCppName.to_qual_string
  | _ ->
      None


let is_receiver_objc_class_type an =
  match receiver_objc_type_name an with
  | Some type_name ->
      String.equal type_name "Class"
  | None ->
      false


let is_receiver_objc_id_type an =
  match receiver_objc_type_name an with
  | Some type_name ->
      String.equal type_name "id"
  | None ->
      false


let objc_message_receiver context an =
  match an with
  | Ctl_parser_types.Stmt (ObjCMessageExpr (_, args, _, omei)) -> (
    match omei.omei_receiver_kind with
    | `SuperClass | `SuperInstance -> (
      match current_objc_container context with
      | Some container ->
          let decl_ref_opt = CAst_utils.get_superclass_curr_class_objc_from_decl container in
          CAst_utils.get_decl_opt_with_decl_ref decl_ref_opt
      | _ ->
          None )
    | `Class qt ->
        CAst_utils.get_decl_from_typ_ptr qt.qt_type_ptr
    | `Instance -> (
      match args with
      | receiver :: _ -> (
        match receiver with
        | ObjCMessageExpr (_, _, _, sub_omei) -> (
          match CAst_utils.get_decl_opt sub_omei.omei_decl_pointer with
          | Some (ObjCMethodDecl (_, _, omdi)) ->
              CAst_utils.qual_type_to_objc_interface omdi.omdi_result_type
          | _ ->
              None )
        | PseudoObjectExpr (_, _, ei) | ImplicitCastExpr (_, _, ei, _) | ParenExpr (_, _, ei) ->
            CAst_utils.qual_type_to_objc_interface ei.ei_qual_type
        | _ ->
            None )
      | [] ->
          None ) )
  | _ ->
      None


(* an |= call_method(m) where the name must be exactly m *)
let call_method an m =
  match get_selector an with
  | Some selector ->
      ALVar.compare_str_with_alexp selector m
  | _ ->
      false


let call_class_method an mname =
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.ObjCMessageExpr (_, _, _, omei)) -> (
    match omei.omei_receiver_kind with
    | `SuperClass | `Class _ ->
        ALVar.compare_str_with_alexp omei.omei_selector mname
    | `Instance ->
        (* The ObjC class type, 'Class', is treated as an instance receiver kind.
            We need to check if the receiver is the class type to catch cases like
            [[self class] myClassMethod] *)
        ALVar.compare_str_with_alexp omei.omei_selector mname && is_receiver_objc_class_type an
    | _ ->
        false )
  | _ ->
      false


(* an is a node calling method whose name contains mname of a class. *)
let call_instance_method an mname =
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.ObjCMessageExpr (_, _, _, omei)) -> (
    match omei.omei_receiver_kind with
    | `SuperInstance ->
        ALVar.compare_str_with_alexp omei.omei_selector mname
    | `Instance ->
        (* The ObjC class type, 'Class', is treated as an instance receiver kind.
            We need to verify the receiver is not the class type to avoid cases like
            [[self class] myClassMethod] *)
        ALVar.compare_str_with_alexp omei.omei_selector mname
        && not (is_receiver_objc_class_type an)
    | _ ->
        false )
  | _ ->
      false


let adhere_to_protocol an =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCInterfaceDecl (_, _, _, _, idi)) ->
      not (List.is_empty idi.otdi_protocols)
  | _ ->
      false


let is_objc_extension lcxt =
  CGeneral_utils.is_objc_extension lcxt.CLintersContext.translation_unit_context


let is_global_var an =
  match an with Ctl_parser_types.Decl d -> CAst_utils.is_syntactically_global_var d | _ -> false


let is_static_local_var an =
  match an with Ctl_parser_types.Decl d -> CAst_utils.is_static_local_var d | _ -> false


let is_const_expr_var an =
  match an with Ctl_parser_types.Decl d -> CAst_utils.is_const_expr_var d | _ -> false


let is_qual_type_const an =
  match an with
  | Ctl_parser_types.Stmt s -> (
    match Clang_ast_proj.get_expr_tuple s with
    | Some (_, _, ei) ->
        ei.Clang_ast_t.ei_qual_type.qt_is_const
    | _ ->
        false )
  | _ ->
      false


let objc_class_has_only_one_constructor_method_named an re =
  let open Clang_ast_t in
  let is_class_method d =
    match d with
    | Clang_ast_t.ObjCMethodDecl (_, _, omdi) ->
        not omdi.omdi_is_instance_method
    | _ ->
        false
  in
  match an with
  | Ctl_parser_types.Decl (ObjCImplementationDecl (_, _, decls, _, _)) -> (
    match List.filter decls ~f:(fun d -> is_class_method d) with
    | [n] ->
        is_objc_class_method_named (Ctl_parser_types.Decl n) re
    | _ ->
        false )
  | _ ->
      false


let has_init_list_const_expr an =
  let rec fold lexp = List.fold ~f:(fun acc e -> is_const_expr' e && acc) ~init:true lexp
  and is_const_expr' exp =
    let open Clang_ast_t in
    let res =
      match exp with
      | IntegerLiteral _ | StringLiteral _ ->
          true
      | DeclRefExpr (_, _, _, drei) -> (
        match drei.drti_decl_ref with
        | Some dr -> (
          match dr.dr_kind with `EnumConstant -> true | _ -> false )
        | _ ->
            false )
      | CallExpr (_, _ :: params, _) ->
          fold params
      | _ -> (
        match Clang_ast_proj.get_expr_tuple exp with
        | Some (_, sub_exps, _) ->
            fold sub_exps
        | _ ->
            false )
    in
    L.(debug Analysis Verbose)
      "@\n\n[has_init_list_const_expr]  EVALUATE EXP '%a'  result = '%b'@\n"
      (Pp.to_string ~f:Clang_ast_proj.get_stmt_kind_string)
      exp res ;
    res
  in
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.InitListExpr (_, sub_exps, _)) ->
      fold sub_exps
  | _ ->
      false


let decl_ref_name ?kind name st =
  match st with
  | Clang_ast_t.DeclRefExpr (_, _, _, drti) -> (
    match drti.drti_decl_ref with
    | Some dr -> (
        let ndi, _, _ = CAst_utils.get_info_from_decl_ref dr in
        let has_right_name = ALVar.compare_str_with_alexp ndi.ni_name name in
        match kind with
        | Some decl_kind ->
            has_right_name && PolyVariantEqual.( = ) dr.Clang_ast_t.dr_kind decl_kind
        | None ->
            has_right_name )
    | _ ->
        false )
  | _ ->
      false


let declaration_ref_name ?kind an name =
  match an with Ctl_parser_types.Stmt st -> decl_ref_name ?kind name st | _ -> false


let call_function an name =
  match an with
  | Ctl_parser_types.Stmt st ->
      CAst_utils.exists_eventually_st (decl_ref_name ~kind:`Function) name st
  | _ ->
      false


let is_enum_constant an name =
  match an with
  | Ctl_parser_types.Stmt st ->
      decl_ref_name ~kind:`EnumConstant name st
  | _ ->
      false


let is_enum_constant_of_enum an name =
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.DeclRefExpr (_, _, _, drti)) -> (
    match drti.drti_decl_ref with
    | Some dr -> (
        let ndi, _, _ = CAst_utils.get_info_from_decl_ref dr in
        let qual_name = CAst_utils.get_qualified_name ndi in
        match QualifiedCppName.extract_last qual_name with
        | Some (_, stripped_qual_name) -> (
          match QualifiedCppName.extract_last stripped_qual_name with
          | Some (enum_name, _) ->
              PolyVariantEqual.( = ) dr.Clang_ast_t.dr_kind `EnumConstant
              && ALVar.compare_str_with_alexp enum_name name
          | _ ->
              false )
        | _ ->
            false )
    | _ ->
        false )
  | _ ->
      false


let is_strong_property an =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCPropertyDecl (_, _, pdi)) ->
      ObjcProperty_decl.is_strong_property pdi
  | _ ->
      false


let is_weak_property an =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCPropertyDecl (_, _, pdi)) ->
      ObjcProperty_decl.is_weak_property pdi
  | _ ->
      false


let is_assign_property an =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCPropertyDecl (_, _, pdi)) ->
      ObjcProperty_decl.is_assign_property pdi
  | _ ->
      false


let is_property_pointer_type an =
  let open Clang_ast_t in
  match an with
  | Ctl_parser_types.Decl (ObjCPropertyDecl (_, _, pdi)) -> (
    match CAst_utils.get_desugared_type pdi.opdi_qual_type.Clang_ast_t.qt_type_ptr with
    | Some (MemberPointerType _) | Some (ObjCObjectPointerType _) | Some (BlockPointerType _) ->
        true
    | Some (TypedefType (_, tti)) ->
        let typedef_str =
          CAst_utils.name_of_typedef_type_info tti |> QualifiedCppName.to_qual_string
        in
        String.equal typedef_str CFrontend_config.id_cl
    | exception Caml.Not_found ->
        false
    | _ ->
        false )
  | _ ->
      false


let context_in_synchronized_block context = context.CLintersContext.in_synchronized_block

(* checks if ivar is defined among a set of fields and if it is atomic *)
let is_ivar_atomic an =
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.ObjCIvarRefExpr (_, _, _, irei)) -> (
      let dr_ref = irei.Clang_ast_t.ovrei_decl_ref in
      let ivar_pointer = dr_ref.Clang_ast_t.dr_decl_pointer in
      match CAst_utils.get_decl ivar_pointer with
      | Some d ->
          let attributes = get_ivar_attributes d in
          List.exists ~f:(PolyVariantEqual.( = ) `Atomic) attributes
      | _ ->
          false )
  | _ ->
      false


let is_method_property_accessor_of_ivar an context =
  let open Clang_ast_t in
  match an with
  | Ctl_parser_types.Stmt (ObjCIvarRefExpr (_, _, _, irei)) -> (
      let dr_ref = irei.Clang_ast_t.ovrei_decl_ref in
      let ivar_pointer = dr_ref.Clang_ast_t.dr_decl_pointer in
      match context.CLintersContext.current_method with
      | Some (ObjCMethodDecl (_, _, mdi)) ->
          if mdi.omdi_is_property_accessor then
            let property_opt = mdi.omdi_property_decl in
            match CAst_utils.get_decl_opt_with_decl_ref property_opt with
            | Some (ObjCPropertyDecl (_, _, pdi)) -> (
              match pdi.opdi_ivar_decl with
              | Some decl_ref ->
                  Int.equal decl_ref.dr_decl_pointer ivar_pointer
              | None ->
                  false )
            | _ ->
                false
          else false
      | _ ->
          false )
  | _ ->
      false


let get_method_name_from_context context =
  match context.CLintersContext.current_method with
  | Some method_decl -> (
    match Clang_ast_proj.get_named_decl_tuple method_decl with
    | Some (_, mnd) ->
        mnd.Clang_ast_t.ni_name
    | _ ->
        "" )
  | _ ->
      ""


let is_objc_constructor context =
  Typ.Procname.ObjC_Cpp.is_objc_constructor (get_method_name_from_context context)


let is_objc_dealloc context =
  Typ.Procname.ObjC_Cpp.is_objc_dealloc (get_method_name_from_context context)


let is_in_method context name =
  let current_method_name = get_method_name_from_context context in
  ALVar.compare_str_with_alexp current_method_name name


let is_in_objc_class_method context name =
  match context.CLintersContext.current_method with
  | Some (ObjCMethodDecl (_, _, omdi) as objc_method) ->
      declaration_has_name (Decl objc_method) name && not omdi.omdi_is_instance_method
  | _ ->
      false


let is_in_objc_instance_method context name =
  match context.CLintersContext.current_method with
  | Some (ObjCMethodDecl (_, _, omdi) as objc_method) ->
      declaration_has_name (Decl objc_method) name && omdi.omdi_is_instance_method
  | _ ->
      false


let is_in_objc_method context name =
  is_in_objc_class_method context name || is_in_objc_instance_method context name


let is_in_function context name =
  match context.CLintersContext.current_method with
  | Some (FunctionDecl _) ->
      is_in_method context name
  | _ ->
      false


let is_in_cxx_method context name =
  match context.CLintersContext.current_method with
  | Some (CXXMethodDecl _) ->
      is_in_method context name
  | _ ->
      false


let is_in_cxx_constructor context name =
  match context.CLintersContext.current_method with
  | Some (CXXConstructorDecl _) ->
      is_in_method context name
  | _ ->
      false


let is_in_cxx_destructor context name =
  match context.CLintersContext.current_method with
  | Some (CXXDestructorDecl _) ->
      is_in_method context name
  | _ ->
      false


let cxx_construct_expr_has_no_parameters an =
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.CXXConstructExpr (_, [], _, _)) ->
      true
  | _ ->
      false


let cxx_construct_expr_has_name an name =
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.CXXConstructExpr (_, _, _, xcei)) -> (
    match xcei.xcei_decl_ref.dr_name with
    | Some ni ->
        ALVar.compare_str_with_alexp ni.ni_name name
    | _ ->
        false )
  | _ ->
      false


let is_in_block context =
  match context.CLintersContext.current_method with Some (BlockDecl _) -> true | _ -> false


let is_in_objc_subclass_of context name =
  match context.CLintersContext.current_objc_class with
  | Some cls ->
      is_subclass_of cls name
  | None ->
      false


let is_in_objc_interface_named context name =
  match context.CLintersContext.current_objc_class with
  | Some cls ->
      is_objc_interface_named (Decl cls) name
  | None ->
      false


let is_in_objc_implementation_named context name =
  match context.CLintersContext.current_objc_class with
  | Some cls ->
      is_objc_implementation_named (Decl cls) name
  | None ->
      false


let is_in_objc_class_named context name =
  match context.CLintersContext.current_objc_class with
  | Some cls ->
      is_objc_class_named (Decl cls) name
  | None ->
      false


let is_in_objc_category_interface_on_class_named context name =
  match context.CLintersContext.current_objc_category with
  | Some cat ->
      is_objc_category_interface_on_class_named (Decl cat) name
  | None ->
      false


let is_in_objc_category_implementation_on_class_named context name =
  match context.CLintersContext.current_objc_category with
  | Some cat ->
      is_objc_category_implementation_on_class_named (Decl cat) name
  | None ->
      false


let is_in_objc_category_on_class_named context name =
  match context.CLintersContext.current_objc_category with
  | Some cat ->
      is_objc_category_on_class_named (Decl cat) name
  | None ->
      false


let is_in_objc_category_interface_on_subclass_of context name =
  match context.CLintersContext.current_objc_category with
  | Some cat ->
      is_objc_category_interface_on_subclass_of (Decl cat) name
  | None ->
      false


let is_in_objc_category_implementation_on_subclass_of context name =
  match context.CLintersContext.current_objc_category with
  | Some cat ->
      is_objc_category_implementation_on_subclass_of (Decl cat) name
  | None ->
      false


let is_in_objc_category_on_subclass_of context name =
  match context.CLintersContext.current_objc_category with
  | Some cat ->
      is_objc_category_on_subclass_of (Decl cat) name
  | None ->
      false


let is_in_objc_category_interface_named context name =
  match context.CLintersContext.current_objc_category with
  | Some cat ->
      is_objc_category_interface_named (Decl cat) name
  | None ->
      false


let is_in_objc_category_implementation_named context name =
  match context.CLintersContext.current_objc_category with
  | Some cat ->
      is_objc_category_implementation_named (Decl cat) name
  | None ->
      false


let is_in_objc_category_named context name =
  match context.CLintersContext.current_objc_category with
  | Some cat ->
      is_objc_category_named (Decl cat) name
  | None ->
      false


let is_in_objc_protocol_named context name =
  match context.CLintersContext.current_objc_protocol with
  | Some protocol ->
      is_objc_protocol_named (Decl protocol) name
  | None ->
      false


let is_receiver_subclass_of context an cname =
  match objc_message_receiver context an with
  | Some receiver ->
      is_subclass_of receiver cname
  | _ ->
      false


let is_receiver_class_named context an cname =
  match objc_message_receiver context an with
  | Some receiver ->
      declaration_has_name (Decl receiver) cname
  | _ ->
      false


let is_receiver_super an =
  match an with
  | Ctl_parser_types.Stmt
      (ObjCMessageExpr (_, _, _, {omei_receiver_kind= `SuperClass | `SuperInstance})) ->
      true
  | _ ->
      false


let is_receiver_self an =
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.ObjCMessageExpr (_, fst_param :: _, _, _)) ->
      CAst_utils.exists_eventually_st
        (decl_ref_name ~kind:`ImplicitParam)
        (ALVar.Const CFrontend_config.self) fst_param
  | _ ->
      false


let captures_cxx_references an = List.length (captured_variables_cxx_ref an) > 0

let is_binop_with_kind an alexp_kind =
  let str_kind = ALVar.alexp_to_string alexp_kind in
  if not (Clang_ast_proj.is_valid_binop_kind_name str_kind) then
    L.(die ExternalError) "Binary operator kind '%s' is not valid" str_kind ;
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.BinaryOperator (_, _, _, boi)) ->
      ALVar.compare_str_with_alexp (Clang_ast_proj.string_of_binop_kind boi.boi_kind) alexp_kind
  | _ ->
      false


let is_unop_with_kind an alexp_kind =
  let str_kind = ALVar.alexp_to_string alexp_kind in
  if not (Clang_ast_proj.is_valid_unop_kind_name str_kind) then
    L.(die ExternalError) "Unary operator kind '%s' is not valid" str_kind ;
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.UnaryOperator (_, _, _, uoi)) ->
      ALVar.compare_str_with_alexp (Clang_ast_proj.string_of_unop_kind uoi.uoi_kind) alexp_kind
  | _ ->
      false


let has_cast_kind an alexp_kind =
  match an with
  | Ctl_parser_types.Decl _ ->
      false
  | Ctl_parser_types.Stmt stmt -> (
      let str_kind = ALVar.alexp_to_string alexp_kind in
      match Clang_ast_proj.get_cast_kind stmt with
      | Some cast_kind ->
          let cast_kind_str = Clang_ast_proj.string_of_cast_kind cast_kind in
          String.equal cast_kind_str str_kind
      | None ->
          false )


let is_node an nodename =
  let nodename_str = ALVar.alexp_to_string nodename in
  if not (Clang_ast_proj.is_valid_astnode_kind nodename_str) then
    L.(die ExternalError) "Node '%s' is not a valid AST node" nodename_str ;
  let an_str =
    match an with
    | Ctl_parser_types.Stmt s ->
        Clang_ast_proj.get_stmt_kind_string s
    | Ctl_parser_types.Decl d ->
        Clang_ast_proj.get_decl_kind_string d
  in
  ALVar.compare_str_with_alexp an_str nodename


let is_ptr_to_objc_class typ class_name =
  match typ with
  | Some (Clang_ast_t.ObjCObjectPointerType (_, {Clang_ast_t.qt_type_ptr})) -> (
    match CAst_utils.get_desugared_type qt_type_ptr with
    | Some (ObjCInterfaceType (_, ptr)) -> (
      match CAst_utils.get_decl ptr with
      | Some (ObjCInterfaceDecl (_, ndi, _, _, _)) ->
          ALVar.compare_str_with_alexp ndi.ni_name class_name
      | _ ->
          false )
    | _ ->
        false )
  | _ ->
      false


(*  node an is of class classname *)
let isa an classname =
  match an with
  | Ctl_parser_types.Stmt stmt -> (
    match Clang_ast_proj.get_expr_tuple stmt with
    | Some (_, _, expr_info) ->
        let typ = CAst_utils.get_desugared_type expr_info.ei_qual_type.qt_type_ptr in
        is_ptr_to_objc_class typ classname
    | _ ->
        false )
  | _ ->
      false


let is_class an re = is_objc_class_named an re

(* an is an expression @selector with whose name in the language of re *)
let is_at_selector_with_name an re =
  match an with
  | Ctl_parser_types.Stmt (ObjCSelectorExpr (_, _, _, s)) ->
      ALVar.compare_str_with_alexp s re
  | _ ->
      false


let iphoneos_target_sdk_version_by_path (cxt : CLintersContext.context) =
  let source_file = cxt.translation_unit_context.source_file in
  let regex_version_opt =
    List.find Config.iphoneos_target_sdk_version_path_regex
      ~f:(fun (version_path_regex : Config.iphoneos_target_sdk_version_path_regex) ->
        ALVar.str_match_forward (SourceFile.to_rel_path source_file) version_path_regex.path )
  in
  match regex_version_opt with
  | Some version_by_regex ->
      Some version_by_regex.version
  | None (* no version by path specified, use default version *) ->
      Config.iphoneos_target_sdk_version


let iphoneos_target_sdk_version_greater_or_equal (cxt : CLintersContext.context) version =
  match iphoneos_target_sdk_version_by_path cxt with
  | Some target_version ->
      Utils.compare_versions target_version version >= 0
  | None ->
      false


let decl_unavailable_in_supported_ios_sdk (cxt : CLintersContext.context) an =
  let config_iphoneos_target_sdk_version = iphoneos_target_sdk_version_by_path cxt in
  let allowed_os_versions =
    match
      (config_iphoneos_target_sdk_version, (cxt.if_context : CLintersContext.if_context option))
    with
    | Some iphoneos_target_sdk_version, Some if_context ->
        iphoneos_target_sdk_version :: if_context.ios_version_guard
    | Some iphoneos_target_sdk_version, None ->
        [iphoneos_target_sdk_version]
    | _ ->
        []
  in
  let max_allowed_version_opt = List.max_elt allowed_os_versions ~compare:Utils.compare_versions in
  let available_attr_ios_sdk = get_available_attr_ios_sdk an in
  match (available_attr_ios_sdk, max_allowed_version_opt) with
  | Some available_attr_ios_sdk, Some max_allowed_version ->
      Utils.compare_versions available_attr_ios_sdk max_allowed_version > 0
  | _ ->
      false


let class_unavailable_in_supported_ios_sdk (cxt : CLintersContext.context) an =
  match receiver_method_call an with
  | Some decl ->
      decl_unavailable_in_supported_ios_sdk cxt (Ctl_parser_types.Decl decl)
  | None ->
      false


(* Check whether a type_ptr and a string denote the same type *)
let type_ptr_equal_type type_ptr type_str =
  let parse_type_string str =
    L.(debug Linters Medium) "Starting parsing type string '%s'@\n" str ;
    let lexbuf = Lexing.from_string str in
    try Types_parser.abs_ctype token lexbuf with
    | CTLExceptions.ALParserInvariantViolationException s ->
        raise
          CTLExceptions.(
            ALFileException (create_exc_info ("Syntax Error when defining type " ^ s) lexbuf))
    | SyntaxError _ | Types_parser.Error ->
        raise CTLExceptions.(ALFileException (create_exc_info "SYNTAX ERROR" lexbuf))
  in
  let abs_ctype =
    match String.Map.find !parsed_type_map type_str with
    | Some abs_ctype' ->
        abs_ctype'
    | None ->
        let abs_ctype' = parse_type_string type_str in
        parsed_type_map := String.Map.set !parsed_type_map ~key:type_str ~data:abs_ctype' ;
        abs_ctype'
  in
  match CAst_utils.get_type type_ptr with
  | Some c_type' ->
      Ctl_parser_types.c_type_equal c_type' abs_ctype
  | _ ->
      L.(debug Linters Medium) "Couldn't find type....@\n" ;
      false


let get_ast_node_type_ptr an =
  match an with
  | Ctl_parser_types.Stmt stmt -> (
    match Clang_ast_proj.get_expr_tuple stmt with
    | Some (_, _, expr_info) ->
        Some expr_info.ei_qual_type.qt_type_ptr
    | _ ->
        None )
  | Ctl_parser_types.Decl decl ->
      CAst_utils.type_of_decl decl


let has_type an typ_ =
  match (get_ast_node_type_ptr an, typ_) with
  | Some pt, ALVar.Const typ ->
      type_ptr_equal_type pt typ
  | _ ->
      false


let has_type_const_ptr_to_objc_class node =
  let open Clang_ast_t in
  match get_ast_node_type_ptr node with
  | Some type_ptr -> (
    match CAst_utils.get_desugared_type type_ptr with
    | Some (ObjCObjectPointerType (_, qt)) ->
        qt.qt_is_const
    | _ ->
        false )
  | None ->
      false


(* Return the lifetime of the pointer of an expression if it is of type AttributedType *)
(* This is useful to check the lifetime of ivars *)
(* @returns objc_lifetime_attr *)
let get_ivar_lifetime an =
  match get_ast_node_type_ptr an with
  | Some pt -> (
    match CAst_utils.get_type pt with
    | Some c_type -> (
        L.(debug Linters Medium) "@\nChecking type: `%s`\n" (Clang_ast_j.string_of_c_type c_type) ;
        let open Clang_ast_t in
        match c_type with
        | AttributedType (_, attr_info) ->
            Some attr_info.Clang_ast_t.ati_lifetime
        | ObjCObjectPointerType _ ->
            Some `OCL_Strong
        | _ ->
            L.(debug Linters Medium) "Pointer is not of type AttributedType...@\n" ;
            None )
    | _ ->
        L.(debug Linters Medium) "Couldn't find type....\n" ;
        None )
  | _ ->
      L.(debug Linters Medium) "Couldn't find pointer...@\n" ;
      None


let is_strong_ivar an =
  match get_ivar_lifetime an with
  | Some lifetime -> (
    match lifetime with `OCL_Strong | `OCL_Autoreleasing | `OCL_None -> true | _ -> false )
  | _ ->
      false


let is_decl node =
  match node with Ctl_parser_types.Decl _ -> true | Ctl_parser_types.Stmt _ -> false


let method_return_type an typ_ =
  L.(debug Linters Verbose) "@\n Executing method_return_type..." ;
  match (an, typ_) with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCMethodDecl (_, _, mdi)), ALVar.Const typ ->
      L.(debug Linters Verbose) "@\n with parameter `%s`...." typ ;
      let qual_type = mdi.Clang_ast_t.omdi_result_type in
      type_ptr_equal_type qual_type.Clang_ast_t.qt_type_ptr typ
  | _ ->
      false


let rec check_protocol_hiearachy decls_ptr prot_name_ =
  let open Clang_ast_t in
  let is_this_protocol di_opt =
    match di_opt with Some di -> ALVar.compare_str_with_alexp di.ni_name prot_name_ | _ -> false
  in
  match decls_ptr with
  | [] ->
      false
  | pt :: decls' ->
      let di, protocols =
        match CAst_utils.get_decl pt with
        | Some (ObjCProtocolDecl (_, di, _, _, opcdi)) ->
            (Some di, opcdi.opcdi_protocols)
        | _ ->
            (None, [])
      in
      if is_this_protocol di || List.exists ~f:(fun dr -> is_this_protocol dr.dr_name) protocols
      then true
      else
        let super_prot = List.map ~f:(fun dr -> dr.dr_decl_pointer) protocols in
        check_protocol_hiearachy (super_prot @ decls') prot_name_


let has_type_subprotocol_of an prot_name_ =
  let open Clang_ast_t in
  let rec check_subprotocol t =
    match t with
    | Some (ObjCObjectPointerType (_, qt)) ->
        check_subprotocol (CAst_utils.get_type qt.qt_type_ptr)
    | Some (ObjCObjectType (_, ooti)) ->
        if List.length ooti.ooti_protocol_decls_ptr > 0 then
          check_protocol_hiearachy ooti.ooti_protocol_decls_ptr prot_name_
        else
          List.exists
            ~f:(fun qt -> check_subprotocol (CAst_utils.get_type qt.qt_type_ptr))
            ooti.ooti_type_args
    | Some (ObjCInterfaceType (_, pt)) ->
        check_protocol_hiearachy [pt] prot_name_
    | _ ->
        false
  in
  match get_ast_node_type_ptr an with
  | Some tp ->
      check_subprotocol (CAst_utils.get_type tp)
  | _ ->
      false


let within_responds_to_selector_block (cxt : CLintersContext.context) an =
  let open Clang_ast_t in
  match an with
  | Ctl_parser_types.Decl (ObjCMethodDecl (_, named_decl_info, _)) -> (
    match cxt.if_context with
    | Some if_context ->
        let in_selector_block = if_context.within_responds_to_selector_block in
        List.mem ~equal:String.equal in_selector_block named_decl_info.ni_name
    | None ->
        false )
  | _ ->
      false


let within_available_class_block (cxt : CLintersContext.context) an =
  match (receiver_method_call an, cxt.if_context) with
  | Some receiver, Some if_context -> (
      let in_available_class_block = if_context.within_available_class_block in
      match declaration_name receiver with
      | Some receiver_name ->
          List.mem ~equal:String.equal in_available_class_block receiver_name
      | None ->
          false )
  | _ ->
      false


let using_namespace an namespace =
  let open Clang_ast_t in
  match an with
  | Ctl_parser_types.Decl (UsingDirectiveDecl (_, _, uddi)) -> (
    match uddi.uddi_nominated_namespace with
    | Some dr -> (
      match (dr.dr_kind, dr.dr_name) with
      | `Namespace, Some ni ->
          ALVar.compare_str_with_alexp ni.ni_name namespace
      | _ ->
          false )
    | None ->
        false )
  | _ ->
      false


let rec get_decl_attributes_for_callexpr an =
  let open Clang_ast_t in
  let open Ctl_parser_types in
  match an with
  | Stmt (CallExpr (_, func :: _, _)) ->
      get_decl_attributes_for_callexpr (Stmt func)
  | Stmt (ImplicitCastExpr (_, [stmt], _, _)) ->
      get_decl_attributes_for_callexpr (Stmt stmt)
  | Stmt (DeclRefExpr (_, _, _, drti)) -> (
    match CAst_utils.get_decl_opt_with_decl_ref drti.drti_decl_ref with
    | Some decl ->
        let decl_info = Clang_ast_proj.get_decl_tuple decl in
        decl_info.di_attributes
    | None ->
        [] )
  | _ ->
      []


let has_visibility_attribute an visibility =
  let open Clang_ast_t in
  let rec has_visibility_attr attrs param =
    match attrs with
    | [] ->
        false
    | VisibilityAttr attr_info :: rest ->
        if List.exists ~f:(fun s -> String.equal param (String.strip s)) attr_info.ai_parameters
        then true
        else has_visibility_attr rest param
    | _ :: rest ->
        has_visibility_attr rest param
  in
  let attributes = get_decl_attributes_for_callexpr an in
  match visibility with ALVar.Const vis -> has_visibility_attr attributes vis | _ -> false


let has_used_attribute an =
  let open Clang_ast_t in
  let attributes = get_decl_attributes_for_callexpr an in
  List.exists ~f:(fun attr -> match attr with UsedAttr _ -> true | _ -> false) attributes


let has_value an al_exp =
  let open Clang_ast_t in
  let open Ctl_parser_types in
  match an with
  | Stmt (IntegerLiteral (_, _, _, integer_literal_info)) ->
      let value = integer_literal_info.Clang_ast_t.ili_value in
      ALVar.compare_str_with_alexp value al_exp
  | Stmt (StringLiteral (_, _, _, l)) ->
      ALVar.compare_str_with_alexp (String.concat ~sep:"" l) al_exp
  | _ ->
      false


(* check if method is called on superclass *)
let is_method_called_by_superclass an =
  let open Clang_ast_t in
  let open Ctl_parser_types in
  match an with
  | Stmt (ObjCMessageExpr (_, _, _, obj_c_message_expr_info)) -> (
    match obj_c_message_expr_info.omei_receiver_kind with `SuperInstance -> true | _ -> false )
  | _ ->
      false


let is_cxx_copy_constructor an =
  let open Clang_ast_t in
  match an with
  | Ctl_parser_types.Stmt (CXXConstructExpr (_, _, _, xcei)) ->
      xcei.xcei_is_copy_constructor
  | _ ->
      false


let has_cxx_fully_qualified_name an qual_name_re =
  match Ctl_parser_types.ast_node_cxx_fully_qualified_name an with
  | "" ->
      false
  | fully_qualified_name ->
      ALVar.compare_str_with_alexp fully_qualified_name qual_name_re


let is_cxx_method_overriding an qual_name_re =
  let rec overrides_named (decl_refs : Clang_ast_t.decl_ref list) (qnre : ALVar.alexp) =
    List.exists
      ~f:(fun (decl_ref : Clang_ast_t.decl_ref) ->
        match CAst_utils.get_decl decl_ref.dr_decl_pointer with
        | None ->
            false
        | Some decl -> (
          match decl with
          | Clang_ast_t.CXXMethodDecl (_, ndi, _, _, mdi) ->
              ALVar.compare_str_with_alexp
                (String.concat ~sep:"::" (List.rev ndi.ni_qual_name))
                qnre
              || overrides_named mdi.xmdi_overriden_methods qnre
          | _ ->
              false ) )
      decl_refs
  in
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.CXXMethodDecl (_, _, _, _, mdi)) -> (
    match qual_name_re with
    | None ->
        not (List.is_empty mdi.xmdi_overriden_methods)
    | Some qnre ->
        overrides_named mdi.xmdi_overriden_methods qnre )
  | _ ->
      false


let is_init_expr_cxx11_constant an =
  let open Clang_ast_t in
  match an with
  | Ctl_parser_types.Decl (VarDecl (_, _, _, vdi)) ->
      vdi.vdi_is_init_expr_cxx11_constant
  | _ ->
      false
