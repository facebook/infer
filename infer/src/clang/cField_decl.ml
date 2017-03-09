(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Utility module to retrieve fields of structs of classes *)

module L = Logging

type field_type = Ident.fieldname * Typ.t * (Annot.t * bool) list

let rec get_fields_super_classes tenv super_class =
  Logging.out_debug "   ... Getting fields of superclass '%s'\n" (Typename.to_string super_class);
  match Tenv.lookup tenv super_class with
  | None -> []
  | Some { fields; supers = super_class :: _ } ->
      let sc_fields = get_fields_super_classes tenv super_class in
      CGeneral_utils.append_no_duplicates_fields fields sc_fields
  | Some { fields } -> fields

let fields_superclass tenv interface_decl_info =
  match interface_decl_info.Clang_ast_t.otdi_super with
  | Some dr ->
      (match dr.Clang_ast_t.dr_name with
       | Some sc ->
           let classname = Typename.Objc.from_string (CAst_utils.get_qualified_name sc) in
           get_fields_super_classes tenv classname
       | _ -> [])
  | _ -> []

let build_sil_field type_ptr_to_sil_type tenv field_name type_ptr prop_attributes =
  let prop_atts = List.map ~f:Clang_ast_j.string_of_property_attribute prop_attributes in
  let annotation_from_type t =
    match t with
    | Typ.Tptr (_, Typ.Pk_objc_weak) -> [Config.weak]
    | Typ.Tptr (_, Typ.Pk_objc_unsafe_unretained) -> [Config.unsafe_unret]
    | _ -> [] in
  let fname = CGeneral_utils.mk_class_field_name field_name in
  let typ = type_ptr_to_sil_type tenv type_ptr in
  let item_annotations = match prop_atts with
    | [] ->
        ({ Annot.class_name = Config.ivar_attributes; parameters = annotation_from_type typ },
         true)
    | _ ->
        ({ Annot.class_name = Config.property_attributes; parameters = prop_atts },
         true) in
  let item_annotations = item_annotations :: (CAst_utils.sil_annot_of_type type_ptr) in
  fname, typ, item_annotations

(* Given a list of declarations in an interface returns a list of fields  *)
let rec get_fields type_ptr_to_sil_type tenv decl_list =
  let open Clang_ast_t in
  let add_field name_info qt attributes decl_list' =
    let fields = get_fields type_ptr_to_sil_type tenv decl_list' in
    let field_tuple = build_sil_field type_ptr_to_sil_type tenv
        name_info qt.Clang_ast_t.qt_type_ptr attributes in
    CGeneral_utils.append_no_duplicates_fields [field_tuple] fields in
  match decl_list with
  | [] -> []
  | ObjCPropertyDecl (_, _, obj_c_property_decl_info) :: decl_list' ->
      (let ivar_decl_ref = obj_c_property_decl_info.Clang_ast_t.opdi_ivar_decl in
       match CAst_utils.get_decl_opt_with_decl_ref ivar_decl_ref with
       | Some (ObjCIvarDecl (_, name_info, type_ptr, _, _)) ->
           let attributes = obj_c_property_decl_info.Clang_ast_t.opdi_property_attributes in
           add_field name_info type_ptr attributes decl_list'
       | _ -> get_fields type_ptr_to_sil_type tenv decl_list')
  | ObjCIvarDecl (_, name_info, type_ptr, _, _) :: decl_list' ->
      add_field name_info type_ptr [] decl_list'
  | _ :: decl_list' ->
      get_fields type_ptr_to_sil_type tenv decl_list'

(* Add potential extra fields defined only in the implementation of the class *)
(* to the info given in the interface. Update the tenv accordingly. *)
let add_missing_fields tenv class_name missing_fields =
  let class_tn_name = Typename.Objc.from_string class_name in
  match Tenv.lookup tenv class_tn_name with
  | Some ({ fields } as struct_typ) ->
      let new_fields = CGeneral_utils.append_no_duplicates_fields fields missing_fields in
      ignore (Tenv.mk_struct tenv ~default:struct_typ ~fields:new_fields ~statics:[] class_tn_name);
      Logging.out_debug " Updating info for class '%s' in tenv\n" class_name
  | _ -> ()

let modelled_fields_in_classes =
  [("NSData", "_bytes", Typ.Tptr (Typ.Tvoid, Typ.Pk_pointer));
   ("NSArray", "elementData", Typ.Tint Typ.IInt)]

let modelled_field class_name_info =
  let modelled_field_in_class res (class_name, field_name, typ) =
    if String.equal class_name class_name_info.Clang_ast_t.ni_name then
      let class_name_qualified = class_name_info.Clang_ast_t.ni_qual_name in
      let field_name_qualified = CAst_utils.make_qual_name_decl class_name_qualified field_name in
      let name = CGeneral_utils.mk_class_field_name field_name_qualified in
      (name, typ, Annot.Item.empty) :: res
    else res in
  List.fold ~f:modelled_field_in_class ~init:[] modelled_fields_in_classes
