(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

open Objc_models

let class_equal class_typename class_name = String.equal (Typ.Name.name class_typename) class_name

let is_cf_non_null_alloc pname =
  String.equal (Typ.Procname.to_string pname) CFrontend_config.cf_non_null_alloc

let is_alloc pname =
  String.equal (Typ.Procname.to_string pname) CFrontend_config.cf_alloc

let is_alloc_model typ pname =
  if Specs.summary_exists pname then false
  else
    let funct = Typ.Procname.to_string pname in
    (* if (Core_foundation_model.is_core_lib_create typ funct) then
       print_endline ("\nCore Foundation create not modelled "
       ^(Typ.to_string typ)^" "^(funct));*)
    Core_foundation_model.is_core_lib_create typ funct

let is_builtin_expect pname =
  String.equal (Typ.Procname.to_string pname) CFrontend_config.builtin_expect

let is_builtin_object_size pname =
  String.equal (Typ.Procname.to_string pname) CFrontend_config.builtin_object_size

let is_replace_with_deref_first_arg pname =
  String.equal (Typ.Procname.to_string pname) CFrontend_config.replace_with_deref_first_arg_attr

let is_retain_predefined_model typ pname =
  let funct = Typ.Procname.to_string pname in
  Core_foundation_model.is_core_lib_retain typ funct

let is_release_predefined_model typ pname =
  let funct = Typ.Procname.to_string pname in
  Core_foundation_model.is_core_lib_release typ funct ||
  Core_foundation_model.is_core_graphics_release typ funct

let is_retain_method funct =
  String.equal funct CFrontend_config.retain

let is_release_method funct =
  String.equal funct CFrontend_config.release

let is_autorelease_method funct =
  String.equal funct CFrontend_config.autorelease

let get_builtinname method_name =
  if is_retain_method method_name then
    Some BuiltinDecl.__objc_retain
  else if is_autorelease_method method_name then
    Some BuiltinDecl.__set_autorelease_attribute
  else if is_release_method method_name then
    Some BuiltinDecl.__objc_release
  else None

let is_modeled_builtin funct =
  String.equal funct CFrontend_config.builtin_memset_chk

let is_modeled_attribute attr_name =
  List.mem ~equal:String.equal CFrontend_config.modeled_function_attributes attr_name

let get_first_param_typedef_string_opt type_ptr =
  match CAst_utils.get_desugared_type type_ptr with
  | Some Clang_ast_t.FunctionProtoType (_, _, {pti_params_type = [param_ptr]}) ->
      CAst_utils.name_opt_of_typedef_type_ptr param_ptr
  | _ -> None

let is_release_builtin funct fun_type =
  let pn = Typ.Procname.from_string_c_fun funct in
  if Specs.summary_exists pn then false
  else match get_first_param_typedef_string_opt fun_type with
    | Some typ -> is_release_predefined_model typ pn
    | _ -> false

let is_retain_builtin funct fun_type =
  let pn = Typ.Procname.from_string_c_fun funct in
  if Specs.summary_exists pn then false
  else match get_first_param_typedef_string_opt fun_type with
    | Some typ -> is_retain_predefined_model typ pn
    | _ -> false

let is_assert_log_s funct =
  String.equal funct CFrontend_config.assert_rtn ||
  String.equal funct CFrontend_config.assert_fail ||
  String.equal funct CFrontend_config.fbAssertWithSignalAndLogFunctionHelper ||
  String.is_substring ~substring:CFrontend_config.google_MakeCheckOpString funct

let is_assert_log_method m =
  String.equal m CFrontend_config.google_LogMessageFatal

let is_handleFailureInMethod funct =
  String.equal funct CFrontend_config.handleFailureInMethod ||
  String.equal funct CFrontend_config.handleFailureInFunction

let is_retain_or_release funct =
  is_retain_method funct ||
  is_release_method funct ||
  is_autorelease_method funct

let is_toll_free_bridging pn =
  let funct = (Typ.Procname.to_string pn) in
  String.equal funct CFrontend_config.cf_bridging_release ||
  String.equal funct CFrontend_config.cf_bridging_retain ||
  String.equal funct CFrontend_config.cf_autorelease ||
  String.equal funct CFrontend_config.ns_make_collectable

let is_cf_retain_release pn =
  Typ.Procname.equal pn BuiltinDecl.__objc_retain_cf
  || Typ.Procname.equal pn BuiltinDecl.__objc_release_cf

(** If the function is a builtin model, return the model, otherwise return the function *)
let is_assert_log pname =
  match pname with
  | Typ.Procname.ObjC_Cpp _ ->
      is_assert_log_method (Typ.Procname.to_string pname)
  | Typ.Procname.C _ -> is_assert_log_s (Typ.Procname.to_string pname)
  | _ -> false


let is_objc_memory_model_controlled o =
  Core_foundation_model.is_objc_memory_model_controlled o

let get_predefined_ms_method condition class_name method_name method_kind mk_procname lang
    arguments return_type attributes builtin =
  if condition then
    let procname =
      match builtin with
      | Some procname -> procname
      | None -> mk_procname class_name method_name method_kind in
    let ms = CMethod_signature.make_ms procname arguments return_type attributes
        (Ast_expressions.dummy_source_range ()) false lang None None None in
    Some ms
  else None

let get_predefined_ms_stringWithUTF8String class_name method_name mk_procname lang =
  let condition =
    class_equal class_name CFrontend_config.nsstring_cl &&
    String.equal method_name CFrontend_config.string_with_utf8_m in
  let id_type = Ast_expressions.create_id_type in
  let args = [(Mangled.from_string "x",
               Ast_expressions.create_char_star_qual_type ~is_const:true)] in
  get_predefined_ms_method condition class_name method_name Typ.Procname.ObjCClassMethod
    mk_procname lang args id_type [] None

let get_predefined_ms_retain_release method_name mk_procname lang =
  let condition = is_retain_or_release method_name in
  let return_type =
    if is_retain_method method_name || is_autorelease_method method_name
    then Ast_expressions.create_id_type else Ast_expressions.create_void_type in
  let class_typename = Typ.Name.Objc.from_string CFrontend_config.nsobject_cl in
  let class_type = Ast_expressions.create_class_qual_type class_typename in
  let args = [(Mangled.from_string CFrontend_config.self, class_type)] in
  get_predefined_ms_method condition class_typename method_name Typ.Procname.ObjCInstanceMethod
    mk_procname lang args return_type [] (get_builtinname method_name)

let get_predefined_ms_autoreleasepool_init class_name method_name mk_procname lang =
  let condition =
    String.equal method_name CFrontend_config.init &&
    class_equal class_name CFrontend_config.nsautorelease_pool_cl in
  let class_type = Ast_expressions.create_class_qual_type class_name in
  get_predefined_ms_method condition class_name method_name Typ.Procname.ObjCInstanceMethod
    mk_procname lang [(Mangled.from_string CFrontend_config.self, class_type)]
    Ast_expressions.create_void_type [] None

let get_predefined_ms_nsautoreleasepool_release class_name method_name mk_procname lang =
  let condition =
    (String.equal method_name CFrontend_config.release ||
     String.equal method_name CFrontend_config.drain)
    &&
    class_equal class_name CFrontend_config.nsautorelease_pool_cl in
  let class_type = Ast_expressions.create_class_qual_type class_name in
  let args = [(Mangled.from_string CFrontend_config.self, class_type)] in
  get_predefined_ms_method condition class_name method_name Typ.Procname.ObjCInstanceMethod
    mk_procname lang args Ast_expressions.create_void_type
    [] (Some BuiltinDecl.__objc_release_autorelease_pool)

let get_predefined_ms_is_kind_of_class class_name method_name mk_procname lang =
  let condition = String.equal method_name CFrontend_config.is_kind_of_class in
  let class_type = Ast_expressions.create_class_qual_type class_name in
  let args = [(Mangled.from_string CFrontend_config.self, class_type)] in
  get_predefined_ms_method condition class_name method_name Typ.Procname.ObjCInstanceMethod
    mk_procname lang args Ast_expressions.create_BOOL_type
    [] (Some BuiltinDecl.__instanceof)

let get_predefined_model_method_signature class_name method_name mk_procname lang =
  let next_predefined f = function
    | Some _ as x -> x
    | None -> f method_name mk_procname lang in
  get_predefined_ms_nsautoreleasepool_release class_name method_name mk_procname lang
  |> next_predefined get_predefined_ms_retain_release
  |> next_predefined (get_predefined_ms_stringWithUTF8String class_name)
  |> next_predefined (get_predefined_ms_autoreleasepool_init class_name)
  |> next_predefined (get_predefined_ms_is_kind_of_class class_name)

let dispatch_functions = [
  ("_dispatch_once", 1);
  ("dispatch_async", 1);
  ("dispatch_sync", 1);
  ("dispatch_after", 2);
  ("dispatch_group_async", 2);
  ("dispatch_group_notify", 2);
  ("dispatch_group_wait", 2);
  ("dispatch_barrier_async", 1);
]

let is_dispatch_function_name function_name =
  let rec is_dispatch functions =
    match functions with
    | [] -> None
    | (el, block_arg_pos):: rest ->
        if (String.equal el function_name)
        then Some (el, block_arg_pos)
        else is_dispatch rest in
  is_dispatch dispatch_functions
