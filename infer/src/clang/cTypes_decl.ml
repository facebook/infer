(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Processes types and record declarations by adding them to the tenv *)

open Utils
open CFrontend_utils

module L = Logging
exception Typename_not_found

let add_predefined_objc_types tenv =
  let objc_class_mangled = Mangled.from_string CFrontend_config.objc_class in
  let objc_class_name = Sil.TN_csu (Sil.Class, objc_class_mangled) in
  let objc_class_type_info =
    Sil.Tstruct ([], [], Sil.Struct,
                 Some (Mangled.from_string CFrontend_config.objc_class), [], [], []) in
  Sil.tenv_add tenv objc_class_name objc_class_type_info;
  let class_typename = CType_to_sil_type.get_builtin_objc_typename `ObjCClass in
  let class_typ = Sil.Tvar (Sil.TN_csu (Sil.Struct, objc_class_mangled)) in
  Sil.tenv_add tenv class_typename class_typ;
  let typename_objc_object =
    Sil.TN_csu (Sil.Struct, Mangled.from_string CFrontend_config.objc_object) in
  let id_typedef = Sil.Tvar (typename_objc_object) in
  let id_typename = CType_to_sil_type.get_builtin_objc_typename `ObjCId in
  Sil.tenv_add tenv id_typename id_typedef

(* Whenever new type are added manually to the translation in ast_expressions, *)
(* they should be added here too!! *)
let add_predefined_basic_types tenv =
  let open Ast_expressions in
  let open Clang_ast_t in
  let add_basic_type qt basic_type_kind =
    let sil_type = CType_to_sil_type.sil_type_of_builtin_type_kind basic_type_kind in
    Ast_utils.update_sil_types_map qt.Clang_ast_t.qt_type_ptr sil_type in
  let add_pointer_type qt sil_type =
    let pointer_type = CTypes.add_pointer_to_typ sil_type in
    Ast_utils.update_sil_types_map qt.Clang_ast_t.qt_type_ptr pointer_type in
  let add_function_type qt return_type =
    (* We translate function types as the return type of the function *)
    Ast_utils.update_sil_types_map qt.Clang_ast_t.qt_type_ptr return_type in
  let sil_void_type = CType_to_sil_type.sil_type_of_builtin_type_kind `Void in
  let sil_char_type = CType_to_sil_type.sil_type_of_builtin_type_kind `Char_S in
  let sil_nsarray_type = Sil.Tvar (CTypes.mk_classname CFrontend_config.nsarray_cl) in
  let sil_id_type = CType_to_sil_type.get_builtin_objc_type `ObjCId in
  add_basic_type create_int_type `Int;
  add_basic_type create_void_type `Void;
  add_basic_type create_char_star_type `Char_S;
  add_basic_type create_BOOL_type `SChar;
  add_basic_type create_unsigned_long_type `ULong;
  add_pointer_type create_void_star_type sil_void_type;
  add_pointer_type create_char_star_type sil_char_type;
  add_pointer_type create_char_star_type sil_char_type;
  add_pointer_type create_nsarray_star_type sil_nsarray_type;
  add_pointer_type create_id_type sil_id_type;
  add_function_type create_void_unsigned_long_type sil_void_type;
  add_function_type create_void_void_type sil_void_type

let add_predefined_types tenv =
  add_predefined_objc_types tenv;
  add_predefined_basic_types tenv

let create_csu opt_type =
  match opt_type with
  | `Type s ->
      (let buf = Str.split (Str.regexp "[ \t]+") s in
       match buf with
       | "struct":: l ->Sil.Struct, General_utils.string_from_list l
       | "class":: l -> Sil.Class, General_utils.string_from_list l
       | "union":: l -> Sil.Union, General_utils.string_from_list l
       | _ -> Sil.Struct, s)
  | _ -> assert false

(* We need to take the name out of the type as the struct can be anonymous*)
let get_record_name_csu decl =
  let open Clang_ast_t in
  let name_info, opt_type, should_be_class = match decl with
    | RecordDecl (_, name_info, opt_type, _, _, _, _) -> name_info, opt_type, false
    | CXXRecordDecl (_, name_info, opt_type, _, _, _, _, cxx_record_info) ->
        (* we use Sil.Class for C++ because we expect Sil.Class csu from *)
        (* types that have methods. And in C++ struct/class/union can have methods *)
        name_info, opt_type, not cxx_record_info.xrdi_is_c_like
    | _-> assert false in
  let name_str = name_info.ni_name in
  let csu, type_name = create_csu opt_type in
  let csu' = if should_be_class then Sil.Class else csu in
  let prefix = Ast_utils.get_qualifier_string name_info in
  let name =
    if (String.length name_str = 0) then prefix ^ type_name else prefix ^ name_str in
  csu', name

let get_record_name decl = snd (get_record_name_csu decl)

let get_method_decls parent decl_list =
  let open Clang_ast_t in
  let rec traverse_decl parent decl = match decl with
    | CXXMethodDecl _ -> [(parent, decl)]
    | CXXRecordDecl (_, _, _, _, decl_list', _, _, _)
    | RecordDecl (_, _, _, _, decl_list', _, _) -> traverse_decl_list decl decl_list'
    | _ -> []
  and traverse_decl_list parent decl_list = list_flatten (list_map (traverse_decl parent) decl_list)  in
  traverse_decl_list parent decl_list

let get_class_methods tenv class_name namespace decl_list =
  let process_method_decl = function
    | Clang_ast_t.CXXMethodDecl (decl_info, name_info, qt, function_decl_info) ->
        let method_name = name_info.Clang_ast_t.ni_name in
        Printing.log_out "  ...Declaring method '%s'.\n" method_name;
        let method_proc = General_utils.mk_procname_from_cpp_method class_name method_name qt in
        Some method_proc
    | _ -> None in
  (* poor mans list_filter_map *)
  list_flatten_options (list_map process_method_decl decl_list)

(** fetches list of superclasses for C++ classes *)
let get_superclass_list decl =
  match decl with
  | Clang_ast_t.CXXRecordDecl (_, _, _, _, _, _, _, cxx_rec_info) ->
      (* there is no concept of virtual inheritance in the backend right now *)
      let base_ptr = cxx_rec_info.Clang_ast_t.xrdi_bases @ cxx_rec_info.Clang_ast_t.xrdi_vbases in
      let base_decls = list_map Ast_utils.get_decl_from_typ_ptr base_ptr in
      let decl_to_mangled_name decl = Mangled.from_string (get_record_name decl) in
      let get_super_field super_decl = (Sil.Class, decl_to_mangled_name super_decl) in
      list_map get_super_field base_decls
  | _ -> []

let add_struct_to_tenv tenv typ =
  let csu = match typ with
    | Sil.Tstruct(_, _, csu, _, _, _, _) -> csu
    | _ -> assert false in
  let mangled = CTypes.get_name_from_struct typ in
  let typename = Sil.TN_csu(csu, mangled) in
  Sil.tenv_add tenv typename typ

let rec get_struct_fields tenv record_name namespace decl_list =
  let open Clang_ast_t in
  let do_one_decl decl = match decl with
    | FieldDecl (_, name_info, qual_type, _) ->
        let id = General_utils.mk_class_field_name name_info in
        let typ = qual_type_to_sil_type tenv qual_type in
        let annotation_items = [] in (* For the moment we don't use them*)
        [(id, typ, annotation_items)]
    | CXXRecordDecl (decl_info, _, _, _, _, _, _, _)
    | RecordDecl (decl_info, _, _, _, _, _, _) ->
        (* C++/C Records treated in the same way*)
        if not decl_info.Clang_ast_t.di_is_implicit then
          ignore (add_types_from_decl_to_tenv tenv namespace decl); []
    | _ -> [] in
  list_flatten (list_map do_one_decl decl_list)

(* For a record declaration it returns/constructs the type *)
and get_declaration_type tenv namespace decl =
  let open Clang_ast_t in
  match decl with
  | CXXRecordDecl (decl_info, name_info, opt_type, type_ptr, decl_list, _, record_decl_info, _)
  | RecordDecl (decl_info, name_info, opt_type, type_ptr, decl_list, _, record_decl_info) ->
      let csu, name = get_record_name_csu decl in
      let mangled_name = Mangled.from_string name in
      let sil_typename = Sil.Tvar (Sil.TN_csu (csu, mangled_name)) in
      (* temporarily saves the type name to avoid infinite loops in recursive types *)
      Ast_utils.update_sil_types_map type_ptr sil_typename;
      if not record_decl_info.Clang_ast_t.rdi_is_complete_definition then
        Printing.log_err
          "   ...Warning, definition incomplete. The full definition will probably be later \n@.";
      let non_static_fields = get_struct_fields tenv name namespace decl_list in
      let non_static_fields' = if CTrans_models.is_objc_memory_model_controlled name then
          General_utils.append_no_duplicates_fields [Sil.objc_ref_counter_field] non_static_fields
        else non_static_fields in
      let sorted_non_static_fields = CFrontend_utils.General_utils.sort_fields non_static_fields' in
      let static_fields = [] in (* Warning for the moment we do not treat static field. *)
      let methods = get_class_methods tenv name namespace decl_list in (* C++ methods only *)
      let superclasses = get_superclass_list decl in
      let item_annotation = Sil.item_annotation_empty in  (* No annotations for struts *)
      let sil_type = Sil.Tstruct (sorted_non_static_fields, static_fields, csu, Some mangled_name,
                                  superclasses, methods, item_annotation) in
      Ast_utils.update_sil_types_map type_ptr sil_type;
      sil_type
  | _ -> assert false

and add_types_from_decl_to_tenv tenv namespace decl =
  let typ = get_declaration_type tenv namespace decl in
  add_struct_to_tenv tenv typ;
  typ

and qual_type_to_sil_type tenv qt =
  CType_to_sil_type.qual_type_to_sil_type add_types_from_decl_to_tenv tenv qt


let type_name_to_sil_type tenv name =
  qual_type_to_sil_type tenv (Ast_expressions.create_class_type name)

let get_type_from_expr_info ei tenv =
  let qt = ei.Clang_ast_t.ei_qual_type in
  qual_type_to_sil_type tenv qt

let class_from_pointer_type tenv qual_type =
  match qual_type_to_sil_type tenv qual_type with
  | Sil.Tptr( Sil.Tvar (Sil.TN_typedef name), _) -> Mangled.to_string name
  | Sil.Tptr( Sil.Tvar (Sil.TN_csu (_, name)), _) -> Mangled.to_string name
  | _ -> assert false

let get_class_type_np tenv expr_info obj_c_message_expr_info =
  let qt =
    match obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind with
    | `Class qt -> qt
    | _ -> expr_info.Clang_ast_t.ei_qual_type in
  qual_type_to_sil_type tenv qt

let get_type_curr_class tenv curr_class_opt =
  let name = CContext.get_curr_class_name curr_class_opt in
  let typ = Sil.Tvar (Sil.TN_csu (Sil.Class, (Mangled.from_string name))) in
  CTypes.expand_structured_type tenv typ
