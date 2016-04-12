(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Utility module to retrieve fields of structs of classes *)

open CFrontend_utils

module L = Logging

type field_type = Ident.fieldname * Sil.typ * (Sil.annotation * bool) list

let rec get_fields_super_classes tenv super_class =
  Printing.log_out "   ... Getting fields of superclass '%s'\n" (Typename.to_string super_class);
  match Tenv.lookup tenv super_class with
  | None -> []
  | Some { Sil.instance_fields; superclasses = super_class :: _ } ->
      let sc_fields = get_fields_super_classes tenv super_class in
      General_utils.append_no_duplicates_fields instance_fields sc_fields
  | Some { Sil.instance_fields } -> instance_fields

let fields_superclass tenv interface_decl_info ck =
  match interface_decl_info.Clang_ast_t.otdi_super with
  | Some dr ->
      (match dr.Clang_ast_t.dr_name with
       | Some sc ->
           let classname = CTypes.mk_classname (Ast_utils.get_qualified_name sc) ck in
           get_fields_super_classes tenv classname
       | _ -> [])
  | _ -> []

let build_sil_field type_ptr_to_sil_type tenv field_name type_ptr prop_attributes =
  let prop_atts = IList.map Clang_ast_j.string_of_property_attribute prop_attributes in
  let annotation_from_type t =
    match t with
    | Sil.Tptr (_, Sil.Pk_objc_weak) -> [Config.weak]
    | Sil.Tptr (_, Sil.Pk_objc_unsafe_unretained) -> [Config.unsafe_unret]
    | _ -> [] in
  let fname = General_utils.mk_class_field_name field_name in
  let typ = type_ptr_to_sil_type tenv type_ptr in
  let item_annotations = match prop_atts with
    | [] ->
        [({ Sil.class_name = Config.ivar_attributes; Sil.parameters = annotation_from_type typ }, true)]
    | _ ->
        [({ Sil.class_name = Config.property_attributes; Sil.parameters = prop_atts }, true)] in
  fname, typ, item_annotations

(* Given a list of declarations in an interface returns a list of fields  *)
let rec get_fields type_ptr_to_sil_type tenv curr_class decl_list =
  let open Clang_ast_t in
  let add_field name_info type_ptr attributes decl_list' =
    let fields = get_fields type_ptr_to_sil_type tenv curr_class decl_list' in
    let field_tuple = build_sil_field type_ptr_to_sil_type tenv name_info type_ptr attributes in
    General_utils.append_no_duplicates_fields [field_tuple] fields in
  match decl_list with
  | [] -> []
  | ObjCPropertyDecl (_, _, obj_c_property_decl_info) :: decl_list' ->
      (let ivar_decl_ref = obj_c_property_decl_info.Clang_ast_t.opdi_ivar_decl in
       match Ast_utils.get_decl_opt_with_decl_ref ivar_decl_ref with
       | Some (ObjCIvarDecl (_, name_info, type_ptr, _, _)) ->
           let attributes = obj_c_property_decl_info.Clang_ast_t.opdi_property_attributes in
           add_field name_info type_ptr attributes decl_list'
       | _ -> get_fields type_ptr_to_sil_type tenv curr_class decl_list')
  | ObjCIvarDecl (_, name_info, type_ptr, _, _) :: decl_list' ->
      add_field name_info type_ptr [] decl_list'
  | _ :: decl_list' ->
      get_fields type_ptr_to_sil_type tenv curr_class decl_list'

(* Add potential extra fields defined only in the implementation of the class *)
(* to the info given in the interface. Update the tenv accordingly. *)
let add_missing_fields tenv class_name ck fields =
  let mang_name = Mangled.from_string class_name in
  let class_tn_name = Typename.TN_csu (Csu.Class ck, mang_name) in
  match Tenv.lookup tenv class_tn_name with
  | Some ({ Sil.instance_fields } as struct_typ) ->
      let new_fields = General_utils.append_no_duplicates_fields instance_fields fields in
      let class_type_info =
        {
          struct_typ with
          Sil.instance_fields = new_fields;
          static_fields = [];
          csu = Csu.Class ck;
          struct_name = Some mang_name;
        } in
      Printing.log_out " Updating info for class '%s' in tenv\n" class_name;
      Tenv.add tenv class_tn_name class_type_info
  | _ -> ()

(* checks if ivar is defined among a set of fields and if it is atomic *)
let is_ivar_atomic ivar fields =
  let do_one_annot a =
    (a.Sil.class_name = Config.property_attributes) &&
    IList.exists (fun p -> p = CFrontend_config.atomic_att) a.Sil.parameters in
  let has_atomic_annot ans =
    IList.exists (fun (a, _) -> do_one_annot a) ans in
  try
    let _, _, annot = IList.find (fun (fn, _, _) -> Ident.fieldname_equal ivar fn) fields in
    has_atomic_annot annot
  with Not_found -> (
      Printing.log_out "NOT Found field ivar = '%s' " (Ident.fieldname_to_string ivar);
      false)

let get_property_corresponding_ivar tenv type_ptr_to_sil_type class_name property_decl =
  let open Clang_ast_t in
  match property_decl with
  | ObjCPropertyDecl (_, named_decl_info, obj_c_property_decl_info) ->
      (let ivar_decl_ref = obj_c_property_decl_info.Clang_ast_t.opdi_ivar_decl in
       match Ast_utils.get_decl_opt_with_decl_ref ivar_decl_ref with
       | Some ObjCIvarDecl (_, named_decl_info, _, _, _) ->
           General_utils.mk_class_field_name named_decl_info
       | _ -> (* Ivar is not known, so add a default one to the tenv *)
           let type_ptr = obj_c_property_decl_info.Clang_ast_t.opdi_type_ptr in
           let field_name_str = Ast_utils.generated_ivar_name named_decl_info in
           let prop_attributes = obj_c_property_decl_info.Clang_ast_t.opdi_property_attributes in
           let field_name, typ, attr = build_sil_field type_ptr_to_sil_type tenv
               field_name_str type_ptr prop_attributes in
           ignore (add_missing_fields tenv class_name Csu.Objc [(field_name, typ, attr)]);
           field_name)
  | _ -> assert false
