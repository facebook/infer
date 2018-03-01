(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

let is_modelled_static_function name =
  let modelled_functions = ["_dispatch_once"; "CFAutorelease"; "CFBridgingRelease"] in
  List.mem ~equal:String.equal modelled_functions name


let class_equal class_typename class_name = String.equal (Typ.Name.name class_typename) class_name

let is_builtin_expect pname =
  String.equal (Typ.Procname.to_string pname) CFrontend_config.builtin_expect


let is_builtin_object_size pname =
  String.equal (Typ.Procname.to_string pname) CFrontend_config.builtin_object_size


let is_std_addressof pname =
  (* since std_addressof is a template function, matching it requires QualifiedCppName *)
  QualifiedCppName.Match.match_qualifiers CFrontend_config.std_addressof
    (Typ.Procname.get_qualifiers pname)


let is_replace_with_deref_first_arg pname =
  String.equal (Typ.Procname.to_string pname) CFrontend_config.replace_with_deref_first_arg_attr


let is_modeled_builtin funct = String.equal funct CFrontend_config.builtin_memset_chk

let is_modeled_attribute attr_name =
  List.mem ~equal:String.equal CFrontend_config.modeled_function_attributes attr_name


let is_assert_log_s funct =
  String.equal funct CFrontend_config.assert_rtn || String.equal funct CFrontend_config.assert_fail
  || String.equal funct CFrontend_config.fbAssertWithSignalAndLogFunctionHelper
  || String.is_substring ~substring:CFrontend_config.google_MakeCheckOpString funct


let is_assert_log_method m = String.equal m CFrontend_config.google_LogMessageFatal

let is_handleFailureInMethod funct =
  String.equal funct CFrontend_config.handleFailureInMethod
  || String.equal funct CFrontend_config.handleFailureInFunction


(** If the function is a builtin model, return the model, otherwise return the function *)
let is_assert_log pname =
  match pname with
  | Typ.Procname.ObjC_Cpp _ ->
      is_assert_log_method (Typ.Procname.to_string pname)
  | Typ.Procname.C _ ->
      is_assert_log_s (Typ.Procname.to_string pname)
  | _ ->
      false


let get_predefined_ms_method condition class_name method_name method_kind mk_procname lang
    arguments return_type attributes builtin =
  if condition then
    let procname =
      match builtin with
      | Some procname ->
          procname
      | None ->
          mk_procname class_name method_name method_kind
    in
    let ms =
      CMethodSignature.mk procname arguments return_type attributes
        (CAst_utils.dummy_source_range ())
        ProcAttributes.C_FUNCTION lang None None None `None
    in
    Some ms
  else None


let get_predefined_ms_stringWithUTF8String class_name method_name mk_procname lang =
  let condition =
    class_equal class_name CFrontend_config.nsstring_cl
    && String.equal method_name CFrontend_config.string_with_utf8_m
  in
  let id_type = Ast_expressions.create_id_type in
  let char_star_type =
    Ast_expressions.create_char_star_type ~quals:(Typ.mk_type_quals ~is_const:true ()) ()
  in
  let args = [(Mangled.from_string "x", char_star_type)] in
  get_predefined_ms_method condition class_name method_name Typ.Procname.ObjC_Cpp.ObjCClassMethod
    mk_procname lang args id_type [] None


let get_predefined_ms_is_kind_of_class class_name method_name mk_procname lang =
  let condition = String.equal method_name CFrontend_config.is_kind_of_class in
  let class_type = Ast_expressions.create_class_qual_type class_name in
  let args = [(Mangled.from_string CFrontend_config.self, class_type)] in
  get_predefined_ms_method condition class_name method_name
    Typ.Procname.ObjC_Cpp.ObjCInstanceMethod mk_procname lang args Ast_expressions.create_BOOL_type
    [] (Some BuiltinDecl.__instanceof)


let get_predefined_model_method_signature class_name method_name mk_procname lang =
  let next_predefined f = function Some _ as x -> x | None -> f method_name mk_procname lang in
  None |> next_predefined (get_predefined_ms_stringWithUTF8String class_name)
  |> next_predefined (get_predefined_ms_is_kind_of_class class_name)
