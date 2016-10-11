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

let is_ck_context {CLintersContext.is_ck_translation_unit; translation_unit_context} decl =
  is_ck_translation_unit
  && Ast_utils.is_in_main_file translation_unit_context decl
  && General_utils.is_objc_extension translation_unit_context


(** Recursively go up the inheritance hierarchy of a given ObjCInterfaceDecl.
    (Returns false on decls other than that one.) *)
let is_component_or_controller_if decl =
  let open CFrontend_config in
  Ast_utils.is_objc_if_descendant decl [ckcomponent_cl; ckcomponentcontroller_cl]

(** True if it's an objc class impl that extends from CKComponent or
    CKComponentController, false otherwise *)
let rec is_component_or_controller_descendant_impl decl =
  match decl with
  | Clang_ast_t.ObjCImplementationDecl _ ->
      is_component_or_controller_if (Ast_utils.get_super_if (Some decl))
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
  let rec get_referenced_type (qual_type: Clang_ast_t.qual_type) : Clang_ast_t.decl option =
    let typ_opt = Ast_utils.get_desugared_type qual_type.qt_type_ptr in
    match (typ_opt : Clang_ast_t.c_type option) with
    | Some ObjCInterfaceType (_, decl_ptr)
    | Some RecordType (_, decl_ptr) -> Ast_utils.get_decl decl_ptr
    | Some PointerType (_, inner_qual_type)
    | Some ObjCObjectPointerType (_, inner_qual_type)
    | Some LValueReferenceType (_, inner_qual_type) -> get_referenced_type inner_qual_type
    | _ -> None in

  let is_of_whitelisted_type qual_type =
    let cpp_whitelist = ["CKComponentScope"; "FBTrackingNodeScope"; "FBTrackingCodeScope"] in
    let objc_whitelist = ["NSError"] in
    match get_referenced_type qual_type with
    | Some CXXRecordDecl (_, ndi, _, _, _, _, _, _) ->
        IList.mem string_equal ndi.ni_name cpp_whitelist
    | Some ObjCInterfaceDecl (_, ndi, _, _, _) ->
        IList.mem string_equal ndi.ni_name objc_whitelist
    | _ -> false in

  match decl with
  | Clang_ast_t.VarDecl(decl_info, _, qual_type, _) ->
      let is_const_ref = match Ast_utils.get_type qual_type.qt_type_ptr with
        | Some LValueReferenceType (_, {Clang_ast_t.qt_is_const}) ->
            qt_is_const
        | _ -> false in
      let is_const = qual_type.qt_is_const || is_const_ref in
      let condition = is_ck_context context decl
                      && (not (Ast_utils.is_syntactically_global_var decl))
                      && (not is_const)
                      && not (is_of_whitelisted_type qual_type) in
      if condition then
        Some {
          CIssue.issue = CIssue.Mutable_local_variable_in_component_file;
          CIssue.description = "Local variables should be const to avoid reassignment";
          CIssue.suggestion = Some "Add a const (after the asterisk for pointer types).";
          CIssue.loc = CFrontend_checkers.location_from_dinfo context decl_info
        }
      else None
  | _ -> assert false (* Should only be called with a VarDecl *)


(** Catches functions that should be composite components.
    http://componentkit.org/docs/break-out-composites.html

    Any static function that returns a subclass of CKComponent will be flagged. *)
let component_factory_function_advice context decl =
  let is_component_if decl =
    Ast_utils.is_objc_if_descendant decl [CFrontend_config.ckcomponent_cl] in

  match decl with
  | Clang_ast_t.FunctionDecl (decl_info, _, (qual_type: Clang_ast_t.qual_type), _) ->
      let objc_interface =
        Ast_utils.type_ptr_to_objc_interface qual_type.qt_type_ptr in
      let condition =
        is_ck_context context decl && is_component_if objc_interface in
      if condition then
        Some {
          CIssue.issue = CIssue.Component_factory_function;
          CIssue.description = "Break out composite components";
          CIssue.suggestion = Some (
              "Prefer subclassing CKCompositeComponent to static helper functions \
               that return a CKComponent subclass."
            );
          CIssue.loc = CFrontend_checkers.location_from_dinfo context decl_info
        }
      else None
  | _ -> assert false (* Should only be called with FunctionDecl *)
