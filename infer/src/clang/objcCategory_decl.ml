(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module L = Logging

(** In this module an ObjC category declaration or implementation is processed. The category    *)
(** is saved in the tenv as a struct with the corresponding fields and methods , and the class it belongs to *)

(* Name used for category with no name, i.e., "" *)
let noname_category class_name =
  CFrontend_config.emtpy_name_category^class_name

let cat_class_decl dr =
  match dr.Clang_ast_t.dr_name with
  | Some n -> CAst_utils.get_qualified_name n
  | _ -> assert false

let get_curr_class_from_category name decl_ref_opt =
  match decl_ref_opt with
  | Some dr ->
      let class_name = cat_class_decl dr in
      CContext.ContextCategory (name, class_name)
  | _ -> assert false

let get_curr_class_from_category_decl name ocdi =
  get_curr_class_from_category name ocdi.Clang_ast_t.odi_class_interface

let get_curr_class_from_category_impl name ocidi =
  get_curr_class_from_category name ocidi.Clang_ast_t.ocidi_class_interface

let add_category_decl type_ptr_to_sil_type tenv category_impl_info =
  let decl_ref_opt = category_impl_info.Clang_ast_t.ocidi_category_decl in
  CAst_utils.add_type_from_decl_ref type_ptr_to_sil_type tenv decl_ref_opt true

let add_class_decl type_ptr_to_sil_type tenv category_decl_info =
  let decl_ref_opt = category_decl_info.Clang_ast_t.odi_class_interface in
  CAst_utils.add_type_from_decl_ref type_ptr_to_sil_type tenv decl_ref_opt true

let add_category_implementation type_ptr_to_sil_type tenv category_decl_info =
  let decl_ref_opt = category_decl_info.Clang_ast_t.odi_implementation in
  CAst_utils.add_type_from_decl_ref type_ptr_to_sil_type tenv decl_ref_opt false

let get_base_class_name_from_category decl =
  let open Clang_ast_t in
  let base_class_pointer_opt =
    match decl with
    | ObjCCategoryDecl (_, _, _, _, cdi) ->
        cdi.Clang_ast_t.odi_class_interface
    | ObjCCategoryImplDecl (_, _, _, _, cii) ->
        cii.Clang_ast_t.ocidi_class_interface
    | _ -> None in
  match base_class_pointer_opt with
  | Some decl_ref ->
      (match CAst_utils.get_decl decl_ref.Clang_ast_t.dr_decl_pointer with
       | Some ObjCInterfaceDecl (_, name_info, _, _, _) ->
           Some (CAst_utils.get_qualified_name name_info)
       | _ -> None)
  | None -> None

(* Add potential extra fields defined only in the category *)
(* to the corresponding class. Update the tenv accordingly.*)
let process_category type_ptr_to_sil_type tenv curr_class decl_info decl_list =
  let decl_fields = CField_decl.get_fields type_ptr_to_sil_type tenv curr_class decl_list in
  let decl_methods = ObjcProperty_decl.get_methods curr_class decl_list in
  let class_name = CContext.get_curr_class_name curr_class in
  let mang_name = Mangled.from_string class_name in
  let class_tn_name = Typename.TN_csu (Csu.Class Csu.Objc, mang_name) in
  let decl_key = `DeclPtr decl_info.Clang_ast_t.di_pointer in
  CAst_utils.update_sil_types_map decl_key (Typ.Tstruct class_tn_name);
  (match Tenv.lookup tenv class_tn_name with
   | Some ({ fields; methods } as struct_typ) ->
       let new_fields = CGeneral_utils.append_no_duplicates_fields decl_fields fields in
       let new_methods = CGeneral_utils.append_no_duplicates_methods decl_methods methods in
       ignore(
         Tenv.mk_struct tenv
           ~default:struct_typ ~fields:new_fields ~statics:[] ~methods:new_methods class_tn_name );
       Logging.out_debug " Updating info for class '%s' in tenv\n" class_name
   | _ -> ());
  Typ.Tstruct class_tn_name

let category_decl type_ptr_to_sil_type tenv decl =
  let open Clang_ast_t in
  match decl with
  | ObjCCategoryDecl (decl_info, name_info, decl_list, _, cdi) ->
      let name = CAst_utils.get_qualified_name name_info in
      let curr_class = get_curr_class_from_category_decl name cdi in
      Logging.out_debug "ADDING: ObjCCategoryDecl for '%s'\n" name;
      let _ = add_class_decl type_ptr_to_sil_type tenv cdi in
      let typ = process_category type_ptr_to_sil_type tenv curr_class decl_info decl_list in
      let _ = add_category_implementation type_ptr_to_sil_type tenv cdi in
      typ
  | _ -> assert false

let category_impl_decl type_ptr_to_sil_type tenv decl =
  let open Clang_ast_t in
  match decl with
  | ObjCCategoryImplDecl (decl_info, name_info, decl_list, _, cii) ->
      let name = CAst_utils.get_qualified_name name_info in
      let curr_class = get_curr_class_from_category_impl name cii in
      Logging.out_debug "ADDING: ObjCCategoryImplDecl for '%s'\n" name;
      let _ = add_category_decl type_ptr_to_sil_type tenv cii in
      let typ = process_category type_ptr_to_sil_type tenv curr_class decl_info decl_list in
      typ
  | _ -> assert false
