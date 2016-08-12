(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open CFrontend_utils
open !Utils

(** Recursively go up the inheritance hierarchy of a given ObjCInterfaceDecl.
    (Returns false on decls other than that one.) *)
let rec is_component_or_controller_if decl =
  match decl with
  | Clang_ast_t.ObjCInterfaceDecl (_, ndi, _, _, _)->
      let open CFrontend_config in
      let whitelist = [ckcomponent_cl; ckcomponentcontroller_cl] in
      let blacklist = [nsobject_cl; nsproxy_cl] in
      let in_list some_list = IList.mem string_equal ndi.Clang_ast_t.ni_name some_list in
      if in_list whitelist then
        true
      else if in_list blacklist then
        false
      else
        (match Ast_utils.get_super_if (Some decl) with
         | Some super_decl ->
             is_component_or_controller_if super_decl
         | None -> false)
  | _ -> false

(** True if it's an objc class impl that extends from CKComponent or
    CKComponentController, false otherwise *)
let rec is_component_or_controller_descendant_impl decl =
  match decl with
  | Clang_ast_t.ObjCImplementationDecl _ ->
      let super_if = Ast_utils.get_super_if (Some decl) in
      Option.map_default is_component_or_controller_if false super_if
  | Clang_ast_t.LinkageSpecDecl (_, decl_list, _) ->
      contains_ck_impl decl_list
  | _ -> false

(** Returns true if the passed-in list of decls contains an
    ObjCImplementationDecl of a descendant of CKComponent or
    CKComponentController.

    Does not recurse into hierarchy. *)
and contains_ck_impl decl_list =
  IList.exists is_component_or_controller_descendant_impl decl_list
