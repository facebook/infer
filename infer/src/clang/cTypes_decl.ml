(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Processes types and record declarations by adding them to the tenv *)

open CFrontend_utils

module L = Logging

let add_predefined_objc_types tenv =
  let class_typename = CType_to_sil_type.get_builtin_objc_typename `ObjCClass in
  let objc_class_type_info =
    {
      Sil.instance_fields = [];
      static_fields = [];
      csu = Csu.Struct;
      struct_name = Some (Mangled.from_string CFrontend_config.objc_class);
      superclasses = [];
      def_methods = [];
      struct_annotations = [];
    } in
  Tenv.add tenv class_typename objc_class_type_info;
  let id_typename = CType_to_sil_type.get_builtin_objc_typename `ObjCId in
  let objc_object_type_info =
    {
      Sil.instance_fields = [];
      static_fields = [];
      csu = Csu.Struct;
      struct_name = Some (Mangled.from_string CFrontend_config.objc_object);
      superclasses = [];
      def_methods = [];
      struct_annotations = [];
    } in
  Tenv.add tenv id_typename objc_object_type_info

(* Whenever new type are added manually to the translation in ast_expressions, *)
(* they should be added here too!! *)
let add_predefined_basic_types () =
  let open Ast_expressions in
  let add_basic_type tp basic_type_kind =
    let sil_type = CType_to_sil_type.sil_type_of_builtin_type_kind basic_type_kind in
    Ast_utils.update_sil_types_map tp sil_type in
  let add_pointer_type tp sil_type =
    let pointer_type = CTypes.add_pointer_to_typ sil_type in
    Ast_utils.update_sil_types_map tp pointer_type in
  let add_function_type tp return_type =
    (* We translate function types as the return type of the function *)
    Ast_utils.update_sil_types_map tp return_type in
  let sil_void_type = CType_to_sil_type.sil_type_of_builtin_type_kind `Void in
  let sil_char_type = CType_to_sil_type.sil_type_of_builtin_type_kind `Char_S in
  let sil_nsarray_type = Sil.Tvar (CTypes.mk_classname CFrontend_config.nsarray_cl Csu.Objc) in
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
  add_predefined_basic_types ()

let create_csu opt_type =
  match opt_type with
  | `Type s ->
      (let buf = Str.split (Str.regexp "[ \t]+") s in
       match buf with
       | "struct":: _ ->Csu.Struct
       | "class":: _ -> Csu.Class Csu.CPP
       | "union":: _ -> Csu.Union
       | _ -> Csu.Struct)
  | _ -> assert false

(* We need to take the name out of the type as the struct can be anonymous*)
let get_record_name_csu decl =
  let open Clang_ast_t in
  let name_info, csu = match decl with
    | RecordDecl (_, name_info, opt_type, _, _, _, _) ->
        name_info, create_csu opt_type
    | CXXRecordDecl (_, name_info, _, _, _, _, _, _)
    | ClassTemplateSpecializationDecl (_, name_info, _, _, _, _, _, _) ->
        (* we use Csu.Class for C++ because we expect Csu.Class csu from *)
        (* types that have methods. And in C++ struct/class/union can have methods *)
        name_info, Csu.Class Csu.CPP
    | _-> assert false in
  let name = Ast_utils.get_qualified_name name_info in
  csu, name

let get_record_name decl = snd (get_record_name_csu decl)

let get_class_methods class_name decl_list =
  let process_method_decl = function
    | Clang_ast_t.CXXMethodDecl (_, name_info, tp, _, _)
    | Clang_ast_t.CXXConstructorDecl (_, name_info, tp, _, _)
    | Clang_ast_t.CXXConversionDecl (_, name_info, tp, _, _)
    | Clang_ast_t.CXXDestructorDecl (_, name_info, tp, _, _) ->
        let method_name = name_info.Clang_ast_t.ni_name in
        Printing.log_out "  ...Declaring method '%s'.\n" method_name;
        let method_proc = General_utils.mk_procname_from_cpp_method class_name method_name tp in
        Some method_proc
    | _ -> None in
  (* poor mans list_filter_map *)
  IList.flatten_options (IList.map process_method_decl decl_list)

let get_superclass_decls decl =
  let open Clang_ast_t in
  match decl with
  | CXXRecordDecl (_, _, _, _, _, _, _, cxx_rec_info)
  | ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, cxx_rec_info) ->
      (* there is no concept of virtual inheritance in the backend right now *)
      let base_ptr = cxx_rec_info.Clang_ast_t.xrdi_bases @ cxx_rec_info.Clang_ast_t.xrdi_vbases in
      let get_decl_or_fail typ_ptr = match Ast_utils.get_decl_from_typ_ptr typ_ptr with
        | Some decl -> decl
        | None -> assert false in
      IList.map get_decl_or_fail base_ptr
  | _ -> []

(** fetches list of superclasses for C++ classes *)
let get_superclass_list_cpp decl =
  let base_decls = get_superclass_decls decl in
  let decl_to_mangled_name decl = Mangled.from_string (get_record_name decl) in
  let get_super_field super_decl =
    Typename.TN_csu (Csu.Class Csu.CPP, decl_to_mangled_name super_decl) in
  IList.map get_super_field base_decls

let add_struct_to_tenv tenv typ =
  let csu, struct_typ = match typ with
    | Sil.Tstruct ({ Sil.csu } as struct_typ) -> csu, struct_typ
    | _ -> assert false in
  let mangled = CTypes.get_name_from_struct typ in
  let typename = Typename.TN_csu(csu, mangled) in
  Tenv.add tenv typename struct_typ

let rec get_struct_fields tenv decl =
  let open Clang_ast_t in
  let decl_list = match decl with
    | ClassTemplateSpecializationDecl (_, _, _, _, decl_list, _, _, _)
    | CXXRecordDecl (_, _, _, _, decl_list, _, _, _)
    | RecordDecl (_, _, _, _, decl_list, _, _) -> decl_list
    | _ -> [] in
  let do_one_decl decl = match decl with
    | FieldDecl (_, name_info, type_ptr, _) ->
        let id = General_utils.mk_class_field_name name_info in
        let typ = type_ptr_to_sil_type tenv type_ptr in
        let annotation_items = [] in (* For the moment we don't use them*)
        [(id, typ, annotation_items)]
    | _ -> [] in
  let base_decls = get_superclass_decls decl in
  let base_class_fields = IList.map (get_struct_fields tenv) base_decls in
  IList.flatten (base_class_fields @ (IList.map do_one_decl decl_list))

(* For a record declaration it returns/constructs the type *)
and get_struct_cpp_class_declaration_type tenv decl =
  let open Clang_ast_t in
  match decl with
  | ClassTemplateSpecializationDecl (_, _, _, type_ptr, decl_list, _, record_decl_info, _)
  | CXXRecordDecl (_, _, _, type_ptr, decl_list, _, record_decl_info, _)
  | RecordDecl (_, _, _, type_ptr, decl_list, _, record_decl_info) ->
      let csu, name = get_record_name_csu decl in
      let mangled_name = Mangled.from_string name in
      let is_complete_definition = record_decl_info.Clang_ast_t.rdi_is_complete_definition in
      let sil_typename = Typename.TN_csu (csu, mangled_name) in
      let extra_fields = if CTrans_models.is_objc_memory_model_controlled name then
          [Sil.objc_ref_counter_field]
        else [] in
      let struct_annotations =
        if csu = Csu.Class Csu.CPP then Sil.cpp_class_annotation
        else Sil.item_annotation_empty in  (* No annotations for structs *)
      if is_complete_definition then (
        Ast_utils.update_sil_types_map type_ptr (Sil.Tvar sil_typename);
        let non_static_fields = get_struct_fields tenv decl in
        let non_static_fields =
          General_utils.append_no_duplicates_fields non_static_fields extra_fields in
        let static_fields = [] in (* Note: We treat static field same as global variables *)
        let def_methods = get_class_methods name decl_list in (* C++ methods only *)
        let superclasses = get_superclass_list_cpp decl in
        let sil_type = Sil.Tstruct {
            Sil.instance_fields = non_static_fields;
            static_fields;
            csu;
            struct_name = Some mangled_name;
            superclasses;
            def_methods;
            struct_annotations;
          } in
        Ast_utils.update_sil_types_map type_ptr sil_type;
        add_struct_to_tenv tenv sil_type;
        sil_type
      ) else (
        match Tenv.lookup tenv sil_typename with
        | Some struct_typ -> Sil.Tstruct struct_typ (* just reuse what is already in tenv *)
        | None ->
            (* This is first forward definition seen so far. Instead of adding *)
            (* empty Tstruct to sil_types_map add Tvar so that frontend doeasn't expand *)
            (* type too early. Since tenv doesn't allow to put Tvars, add empty Tstruct there *)
            (* Later, when we see definition, it will be updated with a new value. *)
            (* Note: we know that this type will be wrapped with pointer type because *)
            (* there was no full definition of that type yet. *)
            let tvar_type = Sil.Tvar sil_typename in
            let empty_struct_type = Sil.Tstruct {
                Sil.instance_fields = extra_fields;
                static_fields = [];
                csu;
                struct_name = Some mangled_name;
                superclasses = [];
                def_methods = [];
                struct_annotations;
              } in
            Ast_utils.update_sil_types_map type_ptr tvar_type;
            add_struct_to_tenv tenv empty_struct_type;
            tvar_type)
  | _ -> assert false

and add_types_from_decl_to_tenv tenv decl =
  let open Clang_ast_t in
  match decl with
  | ClassTemplateSpecializationDecl _ | CXXRecordDecl _ | RecordDecl _ ->
      get_struct_cpp_class_declaration_type tenv decl
  | ObjCInterfaceDecl _ -> ObjcInterface_decl.interface_declaration type_ptr_to_sil_type tenv decl
  | ObjCImplementationDecl _ ->
      ObjcInterface_decl.interface_impl_declaration type_ptr_to_sil_type tenv decl
  | ObjCProtocolDecl _ -> ObjcProtocol_decl.protocol_decl type_ptr_to_sil_type tenv decl
  | ObjCCategoryDecl _ -> ObjcCategory_decl.category_decl type_ptr_to_sil_type tenv decl
  | ObjCCategoryImplDecl _ -> ObjcCategory_decl.category_impl_decl type_ptr_to_sil_type tenv decl
  | EnumDecl _ -> CEnum_decl.enum_decl decl
  | _ -> assert false

and type_ptr_to_sil_type tenv tp =
  CType_to_sil_type.type_ptr_to_sil_type add_types_from_decl_to_tenv tenv tp

let objc_class_name_to_sil_type tenv name =
  type_ptr_to_sil_type tenv (Ast_expressions.create_class_type (name, `OBJC))

let get_type_from_expr_info ei tenv =
  let tp = ei.Clang_ast_t.ei_type_ptr in
  type_ptr_to_sil_type tenv tp

let class_from_pointer_type tenv type_ptr =
  match type_ptr_to_sil_type tenv type_ptr with
  | Sil.Tptr( Sil.Tvar (Typename.TN_typedef name), _) -> Mangled.to_string name
  | Sil.Tptr( Sil.Tvar (Typename.TN_csu (_, name)), _) -> Mangled.to_string name
  | _ -> assert false

let get_class_type_np tenv expr_info obj_c_message_expr_info =
  let tp =
    match obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind with
    | `Class tp -> tp
    | _ -> expr_info.Clang_ast_t.ei_type_ptr in
  type_ptr_to_sil_type tenv tp

let get_type_curr_class_objc tenv curr_class_opt =
  let name = CContext.get_curr_class_name curr_class_opt in
  let typ = Sil.Tvar (Typename.TN_csu (Csu.Class Csu.Objc, (Mangled.from_string name))) in
  CTypes.expand_structured_type tenv typ
