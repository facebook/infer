(*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

open Utils
open CFrontend_utils
open Clang_ast_t
open Objc_models
open CFrontend_config

let is_cf_non_null_alloc funct =
  match funct with
  | Some procname ->
      Procname.to_string procname = CFrontend_config.cf_non_null_alloc
  | None -> false

let is_alloc funct =
  match funct with
  | Some procname ->
      Procname.to_string procname = CFrontend_config.cf_alloc
  | None -> false

let is_alloc_model typ funct =
  match funct with
  | Some procname ->
      if Specs.summary_exists procname then false
      else
        let funct = Procname.to_string procname in
        (* if (Core_foundation_model.is_core_lib_create typ funct) then
        print_endline ("\nCore Foundation create not modelled "^(Sil.typ_to_string typ)^" "^(funct));*)
        Core_foundation_model.is_core_lib_create typ funct
  | None -> false

let rec get_func_type_from_stmt stmt =
  match stmt with
  | DeclRefExpr(stmt_info, stmt_list, expr_info, decl_ref_expr_info) ->
      Some expr_info.ei_qual_type
  | _ ->
      match CFrontend_utils.Ast_utils.get_stmts_from_stmt stmt with
      | stmt:: rest -> get_func_type_from_stmt stmt
      | [] -> None

let is_retain_predefined_model typ funct =
  Core_foundation_model.is_core_lib_retain typ funct

let is_release_predefined_model typ funct =
  Core_foundation_model.is_core_lib_release typ funct ||
  Core_foundation_model.is_core_graphics_release typ funct

let is_retain_method funct =
  funct = CFrontend_config.retain

let is_release_method funct =
  funct = CFrontend_config.release

let is_autorelease_method funct =
  funct = CFrontend_config.autorelease

let get_builtinname method_name =
  if is_retain_method method_name then
    Some SymExec.ModelBuiltins.__objc_retain
  else if is_autorelease_method method_name then
    Some SymExec.ModelBuiltins.__set_autorelease_attribute
  else if is_release_method method_name then
    Some SymExec.ModelBuiltins.__objc_release
  else None

let is_modeled_builtin funct =
  funct = CFrontend_config.builtin_expect ||
  funct = CFrontend_config.builtin_memset_chk

let is_assert_log_s funct =
  funct = CFrontend_config.assert_rtn ||
  funct = CFrontend_config.assert_fail ||
  funct = CFrontend_config.fbAssertWithSignalAndLogFunctionHelper

let is_handleFailureInMethod funct =
  funct = CFrontend_config.handleFailureInMethod ||
  funct = CFrontend_config.handleFailureInFunction

let is_retain_or_release funct =
  is_retain_method funct ||
  is_release_method funct ||
  is_autorelease_method funct

let is_toll_free_bridging pn_opt =
  match pn_opt with
  | Some pn ->
      let funct = (Procname.to_string pn) in
      funct = CFrontend_config.cf_bridging_release ||
      funct = CFrontend_config.cf_bridging_retain ||
      funct = CFrontend_config.cf_autorelease ||
      funct = CFrontend_config.ns_make_collectable
  | None -> false

(** If the function is a builtin model, return the model, otherwise return the function *)
let builtin_predefined_model fun_stmt sil_fe =
  match get_func_type_from_stmt fun_stmt with
  | Some exp ->
      let typ = CTypes.get_type exp in
      (match sil_fe with
        | Sil.Const (Sil.Cfun pn) when Specs.summary_exists pn -> sil_fe, false
        | Sil.Const (Sil.Cfun pn) when is_retain_predefined_model typ (Procname.to_string pn) ->
            Sil.Const (Sil.Cfun SymExec.ModelBuiltins.__objc_retain_cf) , true
        | Sil.Const (Sil.Cfun pn) when is_release_predefined_model typ (Procname.to_string pn) ->
            Sil.Const (Sil.Cfun SymExec.ModelBuiltins.__objc_release_cf), true
        | _ -> sil_fe, false)
  | _ -> sil_fe, false

(** If the function is a builtin model, return the model, otherwise return the function *)
let is_assert_log sil_fe =
  match sil_fe with
  | Sil.Const (Sil.Cfun pn) when is_assert_log_s (Procname.to_string pn) -> true
  | _ -> false

let is_objc_memory_model_controlled o =
  Core_foundation_model.is_objc_memory_model_controlled o

let get_predefined_ms_method condition class_name method_name mk_procname
    arguments return_type attributes builtin =
  if condition then
    let procname =
      match builtin with
      | Some procname -> procname
      | None -> mk_procname class_name method_name in
    let ms = CMethod_signature.make_ms procname arguments return_type attributes
        (Ast_expressions.dummy_source_range ()) false in
    Some ms
  else None

let get_predefined_ms_stringWithUTF8String class_name method_name mk_procname =
  let condition = class_name = nsstring_cl && method_name = string_with_utf8_m in
  get_predefined_ms_method condition class_name method_name mk_procname [("x", "char *", None)]
    id_cl [] None

let get_predefined_ms_retain_release class_name method_name mk_procname =
  let condition = is_retain_or_release method_name in
  let return_type =
    if is_retain_method method_name || is_autorelease_method method_name
    then id_cl else void in
  get_predefined_ms_method condition nsobject_cl method_name mk_procname
    [(self, class_name, None)] return_type [] (get_builtinname method_name)

let get_predefined_ms_autoreleasepool_init class_name method_name mk_procname =
  let condition = (method_name = init) && (class_name = nsautorelease_pool_cl) in
  get_predefined_ms_method condition class_name method_name mk_procname
    [(self, class_name, None)] void [] None

let get_predefined_ms_nsautoreleasepool_release class_name method_name mk_procname =
  let condition = (method_name = release || method_name = drain) &&
    (class_name = nsautorelease_pool_cl) in
  get_predefined_ms_method condition class_name method_name mk_procname [(self, class_name, None)]
    void [] (Some SymExec.ModelBuiltins.__objc_release_autorelease_pool)

let get_predefined_model_method_signature class_name method_name mk_procname =
  match get_predefined_ms_nsautoreleasepool_release class_name method_name mk_procname with
  | Some ms -> Some ms
  | None ->
      match get_predefined_ms_retain_release class_name method_name mk_procname with
      | Some ms -> Some ms
      | None ->
          match get_predefined_ms_stringWithUTF8String class_name method_name mk_procname with
          | Some ms -> Some ms
          | None -> get_predefined_ms_autoreleasepool_init class_name method_name mk_procname

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
    | (el, block_arg_pos):: rest -> if (el = function_name) then
          Some (el, block_arg_pos)
        else is_dispatch rest in
  is_dispatch dispatch_functions
