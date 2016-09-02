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

(** An easy way to fix the component kit best practice
    http://componentkit.org/docs/avoid-local-variables.html

    Local variables that are const or const pointers by definition cannot be
    assigned to after declaration, which means the entire class of bugs stemming
    from value mutation after assignment are gone.

    Note we want const pointers, not mutable pointers to const instances.

    OK:

    ```
    const int a;
    int *const b;
    NSString *const c;
    const int *const d;
    ```

    Not OK:

    ```
    const int *z;
    const NSString *y;
    ``` *)
let mutable_local_vars_advice context decl =
  match decl with
  | Clang_ast_t.VarDecl(decl_info, _, qual_type, _) ->
      let is_const_ref = match Ast_utils.get_type qual_type.qt_type_ptr with
        | Some LValueReferenceType (_, {Clang_ast_t.qt_is_const}) ->
            qt_is_const
        | _ -> false in
      let is_const = qual_type.qt_is_const || is_const_ref in
      let condition = context.CLintersContext.is_ck_translation_unit
                      && Ast_utils.is_in_main_file decl
                      && General_utils.is_objc_extension
                      && (not (Ast_utils.is_syntactically_global_var decl))
                      && (not is_const) in
      if condition then
        Some {
          CIssue.issue = CIssue.Mutable_local_variable_in_component_file;
          CIssue.description = "Local variables should be const to avoid reassignment";
          CIssue.suggestion = Some "Add a const (after the asterisk for pointer types). \
                                    http://componentkit.org/docs/avoid-local-variables.html";
          CIssue.loc = CFrontend_checkers.location_from_dinfo decl_info
        }
      else None
  | _ -> assert false (* Should only be called with a VarDecl *)
