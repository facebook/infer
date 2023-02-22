(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let is_modelled_static_function name =
  let modelled_functions = ["_dispatch_once"; "CFAutorelease"; "CFBridgingRelease"] in
  List.mem ~equal:String.equal modelled_functions name


let class_equal class_typename class_name = String.equal (Typ.Name.name class_typename) class_name

let is_builtin_expect pname =
  String.equal (Procname.to_string pname) CFrontend_config.builtin_expect


let is_builtin_object_size pname =
  String.equal (Procname.to_string pname) CFrontend_config.builtin_object_size


let is_std_addressof pname =
  (* since std_addressof is a template function, matching it requires QualifiedCppName *)
  QualifiedCppName.Match.match_qualifiers CFrontend_config.std_addressof
    (Procname.get_qualifiers pname)


let is_replace_with_deref_first_arg pname =
  String.equal (Procname.to_string pname) CFrontend_config.replace_with_deref_first_arg_attr


let is_modeled_builtin funct = String.equal funct CFrontend_config.builtin_memset_chk

let is_modeled_attribute attr_name =
  List.mem ~equal:String.equal CFrontend_config.modeled_function_attributes attr_name


let is_assert_log_s funct =
  String.equal funct CFrontend_config.assert_rtn
  || String.equal funct CFrontend_config.assert_fail
  || String.equal funct CFrontend_config.fbAssertWithSignalAndLogFunctionHelper
  || String.is_substring ~substring:CFrontend_config.google_MakeCheckOpString funct


let is_assert_log_method m = String.equal m CFrontend_config.google_LogMessageFatal

let is_handleFailureInMethod funct =
  String.equal funct CFrontend_config.handleFailureInMethod
  || String.equal funct CFrontend_config.handleFailureInFunction


(** If the function is a builtin model, return the model, otherwise return the function *)
let is_assert_log pname =
  match pname with
  | Procname.ObjC_Cpp _ ->
      is_assert_log_method (Procname.to_string pname)
  | Procname.C _ ->
      is_assert_log_s (Procname.to_string pname)
  | _ ->
      false


let get_predefined_ms_method condition class_name method_name method_kind clang_params mk_procname
    arguments return_type attributes builtin =
  if condition then
    let procname =
      match builtin with
      | Some procname ->
          procname
      | None ->
          mk_procname class_name method_name method_kind clang_params
    in
    let ms =
      CMethodSignature.mk procname None arguments return_type ~is_ret_constexpr:false attributes
        (CAst_utils.dummy_source_range ())
        ClangMethodKind.C_FUNCTION None None None `None
    in
    Some ms
  else None


let get_predefined_ms_stringWithUTF8String class_name method_name mk_procname =
  let condition =
    class_equal class_name CFrontend_config.nsstring_cl
    && String.equal method_name CFrontend_config.string_with_utf8_m
  in
  let id_type = CType_to_sil_type.type_of_builtin_type_kind `ObjCId in
  let char_star_type =
    let char_type = CType_to_sil_type.type_of_builtin_type_kind ~is_const:true `Char_S in
    Typ.Tptr (char_type, Typ.Pk_pointer) |> Typ.mk ~quals:(Typ.mk_type_quals ())
  in
  let param_name = Mangled.from_string "x" in
  let params = [CMethodSignature.mk_param_type param_name char_star_type] in
  get_predefined_ms_method condition class_name method_name Procname.ObjC_Cpp.ObjCClassMethod [None]
    mk_procname params (id_type, Annot.Item.empty) [] None


let get_predefined_ms_is_kind_of_class class_name method_name mk_procname =
  let condition = String.equal method_name CFrontend_config.is_kind_of_class in
  let class_type = CType_to_sil_type.type_of_builtin_type_kind `ObjCClass in
  let name = Mangled.from_string CFrontend_config.self in
  let params = [CMethodSignature.mk_param_type name class_type] in
  let bool_type = CType_to_sil_type.type_of_builtin_type_kind `Bool in
  get_predefined_ms_method condition class_name method_name Procname.ObjC_Cpp.ObjCInstanceMethod
    [None] mk_procname params (bool_type, Annot.Item.empty) [] (Some BuiltinDecl.__instanceof)


let get_predefined_model_method_signature class_name method_name mk_procname =
  let next_predefined f = function Some _ as x -> x | None -> f method_name mk_procname in
  None
  |> next_predefined (get_predefined_ms_stringWithUTF8String class_name)
  |> next_predefined (get_predefined_ms_is_kind_of_class class_name)
