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
let noname_category class_name = CFrontend_config.emtpy_name_category ^ class_name

let cat_class_decl dr =
  match dr.Clang_ast_t.dr_name with Some n -> CAst_utils.get_qualified_name n | _ -> assert false

let get_classname decl_ref_opt =
  match decl_ref_opt with Some dr -> cat_class_decl dr | _ -> assert false

let get_classname_from_category_decl ocdi = get_classname ocdi.Clang_ast_t.odi_class_interface

let get_classname_from_category_impl ocidi = get_classname ocidi.Clang_ast_t.ocidi_class_interface

let add_category_decl qual_type_to_sil_type tenv category_impl_info =
  let decl_ref_opt = category_impl_info.Clang_ast_t.ocidi_category_decl in
  CAst_utils.add_type_from_decl_ref_opt qual_type_to_sil_type tenv decl_ref_opt true

let add_class_decl qual_type_to_sil_type tenv category_decl_info =
  let decl_ref_opt = category_decl_info.Clang_ast_t.odi_class_interface in
  CAst_utils.add_type_from_decl_ref_opt qual_type_to_sil_type tenv decl_ref_opt true

let add_category_implementation qual_type_to_sil_type tenv category_decl_info =
  let decl_ref_opt = category_decl_info.Clang_ast_t.odi_implementation in
  CAst_utils.add_type_from_decl_ref_opt qual_type_to_sil_type tenv decl_ref_opt false

let get_base_class_name_from_category decl =
  let open Clang_ast_t in
  let base_class_pointer_opt =
    match decl with
    | ObjCCategoryDecl (_, _, _, _, cdi)
     -> cdi.Clang_ast_t.odi_class_interface
    | ObjCCategoryImplDecl (_, _, _, _, cii)
     -> cii.Clang_ast_t.ocidi_class_interface
    | _
     -> None
  in
  match base_class_pointer_opt with
  | Some decl_ref -> (
    match CAst_utils.get_decl decl_ref.Clang_ast_t.dr_decl_pointer with
    | Some ObjCInterfaceDecl (_, name_info, _, _, _)
     -> Some (Typ.Name.Objc.from_qual_name (CAst_utils.get_qualified_name name_info))
    | _
     -> None )
  | None
   -> None

(* Add potential extra fields defined only in the category *)
(* to the corresponding class. Update the tenv accordingly.*)
let process_category qual_type_to_sil_type tenv class_name decl_info decl_list =
  let class_tn_name = Typ.Name.Objc.from_qual_name class_name in
  let decl_fields = CField_decl.get_fields qual_type_to_sil_type tenv class_tn_name decl_list in
  let class_tn_desc = Typ.Tstruct class_tn_name in
  let decl_key = Clang_ast_extend.DeclPtr decl_info.Clang_ast_t.di_pointer in
  CAst_utils.update_sil_types_map decl_key class_tn_desc ;
  ( match Tenv.lookup tenv class_tn_name with
  | Some ({fields} as struct_typ)
   -> let new_fields = CGeneral_utils.append_no_duplicates_fields decl_fields fields in
      ignore
        (Tenv.mk_struct tenv ~default:struct_typ ~fields:new_fields ~statics:[] ~methods:[]
           class_tn_name) ;
      L.(debug Capture Verbose)
        " Updating info for class '%a' in tenv@\n" QualifiedCppName.pp class_name
  | _
   -> () ) ;
  class_tn_desc

let category_decl qual_type_to_sil_type tenv decl =
  let open Clang_ast_t in
  match decl with
  | ObjCCategoryDecl (decl_info, name_info, decl_list, _, cdi)
   -> let name = CAst_utils.get_qualified_name name_info in
      let class_name = get_classname_from_category_decl cdi in
      L.(debug Capture Verbose) "ADDING: ObjCCategoryDecl for '%a'@\n" QualifiedCppName.pp name ;
      let _ = add_class_decl qual_type_to_sil_type tenv cdi in
      let typ = process_category qual_type_to_sil_type tenv class_name decl_info decl_list in
      let _ = add_category_implementation qual_type_to_sil_type tenv cdi in
      typ
  | _
   -> assert false

let category_impl_decl qual_type_to_sil_type tenv decl =
  let open Clang_ast_t in
  match decl with
  | ObjCCategoryImplDecl (decl_info, name_info, decl_list, _, cii)
   -> let name = CAst_utils.get_qualified_name name_info in
      let class_name = get_classname_from_category_impl cii in
      L.(debug Capture Verbose) "ADDING: ObjCCategoryImplDecl for '%a'@\n" QualifiedCppName.pp name ;
      let _ = add_category_decl qual_type_to_sil_type tenv cii in
      let typ = process_category qual_type_to_sil_type tenv class_name decl_info decl_list in
      typ
  | _
   -> assert false
