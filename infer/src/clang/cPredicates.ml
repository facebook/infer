(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open Lexing
open Types_lexer

let get_available_attr_ios_sdk an =
  let open Clang_ast_t in
  let rec get_available_attr attrs =
    match attrs with
    | [] -> None
    | AvailabilityAttr attr_info :: _ ->
        (match attr_info.ai_parameters with
         | "ios" :: version :: _ ->
             Some (String.Search_pattern.replace_all
                     (String.Search_pattern.create "_") ~in_:version ~with_:".")
         | _ -> None)
    | _ :: rest -> get_available_attr rest in
  match an with
  | Ctl_parser_types.Decl decl ->
      let decl_info = Clang_ast_proj.get_decl_tuple decl in
      get_available_attr decl_info.di_attributes
  | _ -> None

let get_ivar_attributes ivar_decl =
  let open Clang_ast_t in
  match ivar_decl with
  | ObjCIvarDecl (ivar_decl_info, _, _, _, _) ->
      (match CAst_utils.get_property_of_ivar ivar_decl_info.Clang_ast_t.di_pointer with
       | Some ObjCPropertyDecl (_, _, obj_c_property_decl_info) ->
           obj_c_property_decl_info.Clang_ast_t.opdi_property_attributes
       | _ -> [])
  | _ -> []

(* list of cxx references captured by decl *)
let captured_variables_cxx_ref an =
  let open Clang_ast_t in
  let capture_var_is_cxx_ref reference_captured_vars captured_var =
    let decl_ref_opt = captured_var.Clang_ast_t.bcv_variable in
    match CAst_utils.get_decl_opt_with_decl_ref decl_ref_opt with
    | Some VarDecl (_, named_decl_info, qual_type, _)
    | Some ParmVarDecl (_, named_decl_info, qual_type, _)
    | Some ImplicitParamDecl (_, named_decl_info, qual_type, _) ->
        (match CAst_utils.get_desugared_type qual_type.Clang_ast_t.qt_type_ptr with
         | Some RValueReferenceType _ | Some LValueReferenceType _ ->
             named_decl_info::reference_captured_vars
         | _ -> reference_captured_vars)
    | _ -> reference_captured_vars in
  match an with
  | Ctl_parser_types.Decl (BlockDecl (_, bdi)) ->
      List.fold ~f:capture_var_is_cxx_ref ~init:[] bdi.bdi_captured_variables
  | _ -> []

type t = ALVar.formula_id * ALVar.alexp list(* (name, [param1,...,paramK]) *)

(* true if and only if a substring of container matches the regular
   expression defined by contained
*)
let str_match_regex container re =
  let rexp = Str.regexp re in
  try
    Str.search_forward rexp container 0 >= 0
  with Not_found -> false

let compare_str_with_alexp s ae =
  match ae with
  | ALVar.Const s' ->
      String.equal s s'
  | ALVar.Regexp re -> str_match_regex s re
  | _ ->
      Logging.out "[WARNING]: ALVAR expression '%s' is not a constant or regexp\n"
        (ALVar.alexp_to_string ae);
      false

let pp_predicate fmt (_name, _arglist) =
  let name = ALVar.formula_id_to_string _name in
  let arglist = List.map ~f:ALVar.alexp_to_string _arglist in
  Format.fprintf fmt "%s(%a)" name (Pp.comma_seq Format.pp_print_string) arglist

(* is an objc interface with name expected_name *)
let is_objc_interface_named an expected_name =
  match an with
  | Ctl_parser_types.Decl Clang_ast_t.ObjCInterfaceDecl(_, ni, _, _, _) ->
      compare_str_with_alexp ni.ni_name expected_name
  | _ -> false

(* checkes whether an object is of a certain class *)
let is_object_of_class_named receiver cname =
  let open Clang_ast_t in
  match receiver with
  | PseudoObjectExpr (_, _, ei)
  | ImplicitCastExpr (_, _, ei, _)
  | ParenExpr (_, _, ei) ->
      (match CAst_utils.qual_type_to_objc_interface ei.ei_qual_type with
       | Some interface ->
           is_objc_interface_named (Ctl_parser_types.Decl interface) cname
       | _ -> false)
  | _ -> false

(* an |= call_method(m) where the name must be exactly m *)
let call_method an m =
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.ObjCMessageExpr (_, _, _, omei)) ->
      compare_str_with_alexp omei.omei_selector m
  | _ -> false

let is_receiver_kind_class omei cname =
  let open Clang_ast_t in
  match omei.omei_receiver_kind  with
  | `Class ptr ->
      (match CAst_utils.get_desugared_type ptr.Clang_ast_t.qt_type_ptr with
       | Some ObjCInterfaceType (_, ptr) ->
           (match CAst_utils.get_decl ptr with
            | Some ObjCInterfaceDecl (_, ndi, _, _, _) ->
                compare_str_with_alexp ndi.ni_name cname
            | _ -> false)
       | _ -> false)
  | _ -> false

let call_class_method an cname mname =
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.ObjCMessageExpr (_, _, _, omei)) ->
      is_receiver_kind_class omei cname &&
      compare_str_with_alexp omei.omei_selector mname
  | _ -> false

(* an is a node calling method whose name contains mname of a
   class whose name contains cname.
*)
let call_instance_method an cname mname =
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.ObjCMessageExpr (_, receiver :: _, _, omei)) ->
      is_object_of_class_named receiver cname &&
      compare_str_with_alexp omei.omei_selector mname
  | _ -> false

let is_objc_extension lcxt =
  CGeneral_utils.is_objc_extension lcxt.CLintersContext.translation_unit_context

let is_syntactically_global_var an =
  match an with
  | Ctl_parser_types.Decl d -> CAst_utils.is_syntactically_global_var d
  | _ -> false

let is_const_expr_var an =
  match an with
  | Ctl_parser_types.Decl d -> CAst_utils.is_const_expr_var d
  | _ -> false

let decl_ref_name ?kind name st =
  match st with
  | Clang_ast_t.DeclRefExpr (_, _, _, drti) ->
      (match drti.drti_decl_ref with
       | Some dr -> let ndi, _, _ = CAst_utils.get_info_from_decl_ref dr in
           let has_right_name = compare_str_with_alexp ndi.ni_name name in
           (match kind with
            | Some decl_kind ->
                has_right_name && PVariant.(=) dr.Clang_ast_t.dr_kind decl_kind
            | None -> has_right_name)
       | _ -> false)
  | _ -> false

let declaration_ref_name ?kind an name =
  match an with
  | Ctl_parser_types.Stmt st ->
      decl_ref_name ?kind name st
  | _ -> false

let call_function an name =
  match an with
  | Ctl_parser_types.Stmt st ->
      CAst_utils.exists_eventually_st (decl_ref_name ~kind:`Function) name st
  | _ -> false

let is_strong_property an =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCPropertyDecl (_, _, pdi)) ->
      ObjcProperty_decl.is_strong_property pdi
  | _ -> false

let is_assign_property an =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCPropertyDecl (_, _, pdi)) ->
      ObjcProperty_decl.is_assign_property pdi
  | _ -> false

let is_property_pointer_type an =
  let open Clang_ast_t in
  match an with
  | Ctl_parser_types.Decl (ObjCPropertyDecl (_, _, pdi)) ->
      (match CAst_utils.get_desugared_type pdi.opdi_qual_type.Clang_ast_t.qt_type_ptr with
       | Some MemberPointerType _
       | Some ObjCObjectPointerType _
       | Some BlockPointerType _ -> true
       | Some TypedefType (_, tti) ->
           let typedef_str = CAst_utils.name_of_typedef_type_info tti
                             |> QualifiedCppName.to_qual_string in
           String.equal typedef_str CFrontend_config.id_cl
       | exception Not_found -> false
       | _ -> false)
  | _ -> false

let context_in_synchronized_block context =
  context.CLintersContext.in_synchronized_block

(* checks if ivar is defined among a set of fields and if it is atomic *)
let is_ivar_atomic an =
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.ObjCIvarRefExpr (_, _, _, irei)) ->
      let dr_ref = irei.Clang_ast_t.ovrei_decl_ref in
      let ivar_pointer = dr_ref.Clang_ast_t.dr_decl_pointer in
      (match CAst_utils.get_decl ivar_pointer with
       | Some d ->
           let attributes = get_ivar_attributes d in
           List.exists ~f:(PVariant.(=) `Atomic) attributes
       | _ -> false)
  | _ -> false

let is_method_property_accessor_of_ivar an context =
  let open Clang_ast_t in
  match an with
  | Ctl_parser_types.Stmt (ObjCIvarRefExpr (_, _, _, irei)) ->
      let dr_ref = irei.Clang_ast_t.ovrei_decl_ref in
      let ivar_pointer = dr_ref.Clang_ast_t.dr_decl_pointer in
      (match context.CLintersContext.current_method with
       | Some ObjCMethodDecl (_, _, mdi) ->
           if mdi.omdi_is_property_accessor then
             let property_opt = mdi.omdi_property_decl in
             match CAst_utils.get_decl_opt_with_decl_ref property_opt with
             | Some ObjCPropertyDecl (_, _, pdi) ->
                 (match pdi.opdi_ivar_decl with
                  | Some decl_ref -> Int.equal decl_ref.dr_decl_pointer ivar_pointer
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
      Typ.Procname.is_objc_constructor method_name
  | _ -> false


let is_objc_dealloc context =
  match context.CLintersContext.current_method with
  | Some method_decl ->
      let method_name = (match Clang_ast_proj.get_named_decl_tuple method_decl with
          | Some (_, mnd) -> mnd.Clang_ast_t.ni_name
          | _ -> "") in
      Typ.Procname.is_objc_dealloc method_name
  | _ -> false

let captures_cxx_references an =
  List.length (captured_variables_cxx_ref an) > 0

let is_binop_with_kind an alexp_kind =
  let str_kind = ALVar.alexp_to_string alexp_kind in
  if not (Clang_ast_proj.is_valid_binop_kind_name str_kind) then
    failwith ("Binary operator kind " ^ str_kind ^ " is not valid");
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.BinaryOperator (_, _, _, boi)) ->
      compare_str_with_alexp (Clang_ast_proj.string_of_binop_kind boi.boi_kind) alexp_kind
  | _ -> false

let is_unop_with_kind an alexp_kind =
  let str_kind = ALVar.alexp_to_string alexp_kind in
  if not (Clang_ast_proj.is_valid_unop_kind_name str_kind) then
    failwith ("Unary operator kind " ^ str_kind ^ " is not valid");
  match an with
  | Ctl_parser_types.Stmt (Clang_ast_t.UnaryOperator (_, _, _, uoi)) ->
      compare_str_with_alexp (Clang_ast_proj.string_of_unop_kind uoi.uoi_kind) alexp_kind
  | _ -> false

let is_node an nodename =
  let nodename_str = ALVar.alexp_to_string nodename in
  if not (Clang_ast_proj.is_valid_astnode_kind nodename_str) then
    failwith ("Node " ^ nodename_str ^ " is not a valid AST node");
  let an_str = match an with
    | Ctl_parser_types.Stmt s -> Clang_ast_proj.get_stmt_kind_string s
    | Ctl_parser_types.Decl d -> Clang_ast_proj.get_decl_kind_string d in
  compare_str_with_alexp an_str nodename

let is_ptr_to_objc_class typ class_name =
  match typ with
  | Some Clang_ast_t.ObjCObjectPointerType (_, {Clang_ast_t.qt_type_ptr}) ->
      (match CAst_utils.get_desugared_type qt_type_ptr with
       | Some ObjCInterfaceType (_, ptr) ->
           (match CAst_utils.get_decl ptr with
            | Some ObjCInterfaceDecl (_, ndi, _, _, _) ->
                compare_str_with_alexp ndi.ni_name class_name
            | _ -> false)
       | _ -> false)
  | _ -> false

(*  node an is of class classname *)
let isa an classname =
  match an with
  | Ctl_parser_types.Stmt stmt ->
      (match Clang_ast_proj.get_expr_tuple stmt with
       | Some (_, _, expr_info) ->
           let typ = CAst_utils.get_desugared_type expr_info.ei_qual_type.qt_type_ptr in
           is_ptr_to_objc_class typ classname
       | _ -> false)
  | _ -> false

(* an is a declaration whose name contains a regexp defined by re *)
let declaration_has_name an name =
  match an with
  | Ctl_parser_types.Decl d ->
      (match Clang_ast_proj.get_named_decl_tuple d with
       | Some (_, ndi) -> compare_str_with_alexp ndi.ni_name name
       | _ -> false)
  | _ -> false

let is_class an re =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCInterfaceDecl _)
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCImplementationDecl _) ->
      declaration_has_name an re
  | _ -> false

let decl_unavailable_in_supported_ios_sdk (cxt : CLintersContext.context) an =
  let allowed_os_versions =
    match Config.iphoneos_target_sdk_version,
          (cxt.if_context : CLintersContext.if_context option) with
    | Some iphoneos_target_sdk_version, Some if_context ->
        iphoneos_target_sdk_version :: if_context.ios_version_guard
    | Some iphoneos_target_sdk_version, None -> [iphoneos_target_sdk_version]
    | _ -> [] in
  let max_allowed_version_opt = List.max_elt allowed_os_versions ~cmp:Utils.compare_versions in
  let available_attr_ios_sdk = get_available_attr_ios_sdk an in
  match available_attr_ios_sdk, max_allowed_version_opt with
  | Some available_attr_ios_sdk, Some max_allowed_version ->
      (Utils.compare_versions available_attr_ios_sdk max_allowed_version) > 0
  | _ -> false

(* Check whether a type_ptr and a string denote the same type *)
let type_ptr_equal_type type_ptr type_str =
  let pos_str lexbuf =
    let pos = lexbuf.lex_curr_p in
    pos.pos_fname ^ ":" ^ (string_of_int pos.pos_lnum) ^ ":" ^
    (string_of_int  (pos.pos_cnum - pos.pos_bol + 1)) in
  let lexbuf = Lexing.from_string type_str in
  let abs_ctype = try
      (Types_parser.abs_ctype token lexbuf)
    with
    | Ctl_parser_types.ALParsingException s ->
        raise (Ctl_parser_types.ALParsingException
                 ("Syntax Error when defining type" ^ s ))
    | SyntaxError _
    | Types_parser.Error ->
        raise (Ctl_parser_types.ALParsingException
                 ("SYNTAX ERROR at " ^ (pos_str lexbuf)))  in
  match CAst_utils.get_type type_ptr with
  | Some c_type' ->
      Ctl_parser_types.tmp_c_type_equal c_type' abs_ctype
  | _ -> Logging.out "Couldn't find type....\n"; false

let has_type an _typ =
  match an, _typ with
  | Ctl_parser_types.Stmt stmt, ALVar.Const typ ->
      (match Clang_ast_proj.get_expr_tuple stmt with
       | Some (_, _, expr_info) ->
           type_ptr_equal_type expr_info.ei_qual_type.qt_type_ptr typ
       | _ -> false)
  | Ctl_parser_types.Decl decl, ALVar.Const typ ->
      (match CAst_utils.type_of_decl decl with
       | Some type_ptr ->
           type_ptr_equal_type type_ptr typ
       | _ -> false)
  | _ -> false

let method_return_type an _typ =
  Logging.out "\n Executing method_return_type...";
  match an, _typ with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCMethodDecl (_, _, mdi)), ALVar.Const typ ->
      Logging.out "\n with parameter `%s`...." typ;
      let qual_type = mdi.Clang_ast_t.omdi_result_type in
      type_ptr_equal_type qual_type.Clang_ast_t.qt_type_ptr typ
  | _ -> false

let within_responds_to_selector_block (cxt:CLintersContext.context) an =
  let open Clang_ast_t in
  match an with
  | Ctl_parser_types.Decl (ObjCMethodDecl (_, named_decl_info, _)) ->
      (match cxt.if_context with
       | Some if_context ->
           let in_selector_block = if_context.within_responds_to_selector_block in
           List.mem ~equal:String.equal in_selector_block named_decl_info.ni_name
       | None -> false)
  | _ -> false
