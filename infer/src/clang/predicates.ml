(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

open CFrontend_utils

let get_available_attr_ios_sdk decl =
  let open Clang_ast_t in
  let decl_info = Clang_ast_proj.get_decl_tuple decl in
  let rec get_available_attr attrs =
    match attrs with
    | [] -> None
    | AvailabilityAttr attr_info :: _ ->
        (match attr_info.Clang_ast_t.ai_parameters with
         | "ios" :: version :: _ ->
             Some (String.Search_pattern.replace_all
                     (String.Search_pattern.create "_") ~in_:version ~with_:".")
         | _ -> None)
    | _ :: rest -> get_available_attr rest in
  get_available_attr decl_info.Clang_ast_t.di_attributes

let get_ivar_attributes ivar_decl =
  let open Clang_ast_t in
  match ivar_decl with
  | ObjCIvarDecl (ivar_decl_info, _, _, _, _) ->
      (match Ast_utils.get_property_of_ivar ivar_decl_info.Clang_ast_t.di_pointer with
       | Some ObjCPropertyDecl (_, _, obj_c_property_decl_info) ->
           obj_c_property_decl_info.Clang_ast_t.opdi_property_attributes
       | _ -> [])
  | _ -> []

(* list of cxx references captured by decl *)
let captured_variables_cxx_ref dec =
  let capture_var_is_cxx_ref reference_captured_vars captured_var =
    let decl_ref_opt = captured_var.Clang_ast_t.bcv_variable in
    match Ast_utils.get_decl_opt_with_decl_ref decl_ref_opt with
    | Some VarDecl (_, named_decl_info, qual_type, _)
    | Some ParmVarDecl (_, named_decl_info, qual_type, _)
    | Some ImplicitParamDecl (_, named_decl_info, qual_type, _) ->
        (match Ast_utils.get_desugared_type qual_type.Clang_ast_t.qt_type_ptr with
         | Some RValueReferenceType _ | Some LValueReferenceType _ ->
             named_decl_info::reference_captured_vars
         | _ -> reference_captured_vars)
    | _ -> reference_captured_vars in
  let captured_vars = match dec with
    | Clang_ast_t.BlockDecl (_, bdi) ->
        bdi.Clang_ast_t.bdi_captured_variables
    | _ -> [] in
  IList.fold_left capture_var_is_cxx_ref [] captured_vars



type t = string * string list (* (name, [param1,...,paramK]) *)

let pp_predicate fmt (name, arglist) =
  Format.fprintf fmt "%s(%a)" name (Pp.comma_seq Format.pp_print_string) arglist

let is_declaration_kind decl s =
  Clang_ast_proj.get_decl_kind_string decl = s

(* st |= call_method(m) *)
let call_method m st =
  match st with
  | Clang_ast_t.ObjCMessageExpr (_, _, _, omei) -> omei.omei_selector = m
  | _ -> false

let property_name_contains_word word decl =
  match Clang_ast_proj.get_named_decl_tuple decl with
  | Some (_, n) -> let pname = n.Clang_ast_t.ni_name in
      let rexp = Str.regexp_string_case_fold word in
      (try
         Str.search_forward rexp pname 0 >= 0
       with Not_found -> false)
  | _ -> false

let is_objc_extension lcxt =
  General_utils.is_objc_extension lcxt.CLintersContext.translation_unit_context

let is_syntactically_global_var decl =
  Ast_utils.is_syntactically_global_var decl

let is_const_expr_var decl =
  Ast_utils.is_const_expr_var decl

let decl_ref_is_in names st =
  match st with
  | Clang_ast_t.DeclRefExpr (_, _, _, drti) ->
      (match drti.drti_decl_ref with
       | Some dr -> let ndi, _, _ = CFrontend_utils.Ast_utils.get_info_from_decl_ref dr in
           IList.exists (fun n -> n = ndi.ni_name) names
       | _ -> false)
  | _ -> false

let call_function_named names st =
  Ast_utils.exists_eventually_st decl_ref_is_in names st

let is_strong_property decl =
  match decl with
  | Clang_ast_t.ObjCPropertyDecl (_, _, pdi) ->
      ObjcProperty_decl.is_strong_property pdi
  | _ -> false

let is_assign_property decl =
  match decl with
  | Clang_ast_t.ObjCPropertyDecl (_, _, pdi) ->
      ObjcProperty_decl.is_assign_property pdi
  | _ -> false

let is_property_pointer_type decl =
  let open Clang_ast_t in
  match decl with
  | ObjCPropertyDecl (_, _, pdi) ->
      (match Ast_utils.get_desugared_type pdi.opdi_type_ptr with
       | Some MemberPointerType _
       | Some ObjCObjectPointerType _
       | Some BlockPointerType _ -> true
       | Some TypedefType (_, tti) ->
           (Ast_utils.name_of_typedef_type_info tti) = CFrontend_config.id_cl
       | exception Not_found -> false
       | _ -> false)
  | _ -> false

let context_in_synchronized_block context =
  context.CLintersContext.in_synchronized_block

(* checks if ivar is defined among a set of fields and if it is atomic *)
let is_ivar_atomic stmt =
  match stmt with
  | Clang_ast_t.ObjCIvarRefExpr (_, _, _, irei) ->
      let dr_ref = irei.Clang_ast_t.ovrei_decl_ref in
      let ivar_pointer = dr_ref.Clang_ast_t.dr_decl_pointer in
      (match Ast_utils.get_decl ivar_pointer with
       | Some d ->
           let attributes = get_ivar_attributes d in
           IList.exists (Ast_utils.equal_property_attribute `Atomic) attributes
       | _ -> false)
  | _ -> false

let is_method_property_accessor_of_ivar stmt context =
  let open Clang_ast_t in
  match stmt with
  | ObjCIvarRefExpr (_, _, _, irei) ->
      let dr_ref = irei.Clang_ast_t.ovrei_decl_ref in
      let ivar_pointer = dr_ref.Clang_ast_t.dr_decl_pointer in
      (match context.CLintersContext.current_method with
       | Some ObjCMethodDecl (_, _, mdi) ->
           if mdi.omdi_is_property_accessor then
             let property_opt = mdi.omdi_property_decl in
             match Ast_utils.get_decl_opt_with_decl_ref property_opt with
             | Some ObjCPropertyDecl (_, _, pdi) ->
                 (match pdi.opdi_ivar_decl with
                  | Some decl_ref -> decl_ref.dr_decl_pointer = ivar_pointer
                  | None -> false)
             | _ -> false
           else false
       | _ -> false)
  | _ -> false

let is_objc_constructor context =
  match context.CLintersContext.current_method with
  | Some method_decl ->
      let method_name = (match Clang_ast_proj.get_named_decl_tuple method_decl with
          | Some (_, mnd) -> mnd.Clang_ast_t.ni_name
          | _ -> "") in
      Procname.is_objc_constructor method_name
  | _ -> false


let is_objc_dealloc context =
  match context.CLintersContext.current_method with
  | Some method_decl ->
      let method_name = (match Clang_ast_proj.get_named_decl_tuple method_decl with
          | Some (_, mnd) -> mnd.Clang_ast_t.ni_name
          | _ -> "") in
      Procname.is_objc_dealloc method_name
  | _ -> false

let captures_cxx_references decl =
  IList.length (captured_variables_cxx_ref decl) > 0

let is_binop_with_kind str_kind stmt =
  if not (Clang_ast_proj.is_valid_binop_kind_name str_kind) then
    failwith ("Binary operator kind " ^ str_kind ^ " is not valid");
  match stmt with
  | Clang_ast_t.BinaryOperator (_, _, _, boi) ->
      Clang_ast_proj.string_of_binop_kind boi.boi_kind = str_kind
  | _ -> false

let is_unop_with_kind str_kind stmt =
  if not (Clang_ast_proj.is_valid_unop_kind_name str_kind) then
    failwith ("Unary operator kind " ^ str_kind ^ " is not valid");
  match stmt with
  | Clang_ast_t.UnaryOperator (_, _, _, uoi) ->
      Clang_ast_proj.string_of_unop_kind uoi.uoi_kind = str_kind
  | _ -> false

let is_stmt nodename stmt =
  if not (Clang_ast_proj.is_valid_astnode_kind nodename) then
    failwith ("Statement " ^ nodename ^ " is not a valid statement");
  nodename = Clang_ast_proj.get_stmt_kind_string stmt

let is_decl nodename decl =
  if not (Clang_ast_proj.is_valid_astnode_kind nodename) then
    failwith ("Declaration " ^ nodename ^ " is not a valid declaration");
  nodename = Clang_ast_proj.get_decl_kind_string decl

let isa classname stmt =
  match Clang_ast_proj.get_expr_tuple stmt with
  | Some (_, _, expr_info) ->
      let typ = CFrontend_utils.Ast_utils.get_desugared_type expr_info.ei_type_ptr in
      CFrontend_utils.Ast_utils.is_ptr_to_objc_class typ classname
  | _ -> false

let decl_unavailable_in_supported_ios_sdk decl =
  let available_attr_ios_sdk = get_available_attr_ios_sdk decl in
  match available_attr_ios_sdk, Config.iphoneos_target_sdk_version with
  | Some available_attr_ios_sdk, Some iphoneos_target_sdk_version ->
      Utils.compare_versions available_attr_ios_sdk iphoneos_target_sdk_version = 1
  | _ -> false
