(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Utility module to retrieve fields of structs of classes *)

module L = Logging

type field_type = Typ.Fieldname.t * Typ.t * (Annot.t * bool) list

let rec get_fields_super_classes tenv super_class =
  L.(debug Capture Verbose)
    "   ... Getting fields of superclass '%s'@\n" (Typ.Name.to_string super_class) ;
  match Tenv.lookup tenv super_class with
  | None ->
      []
  | Some {fields; supers= super_class :: _} ->
      let sc_fields = get_fields_super_classes tenv super_class in
      CGeneral_utils.append_no_duplicates_fields fields sc_fields
  | Some {fields} ->
      fields


let fields_superclass tenv interface_decl_info =
  match interface_decl_info.Clang_ast_t.otdi_super with
  | Some dr -> (
    match dr.Clang_ast_t.dr_name with
    | Some sc ->
        let classname = Typ.Name.Objc.from_qual_name (CAst_utils.get_qualified_name sc) in
        get_fields_super_classes tenv classname
    | _ ->
        [] )
  | _ ->
      []


let build_sil_field qual_type_to_sil_type tenv class_tname field_name qual_type prop_attributes =
  let prop_atts = List.map ~f:Clang_ast_j.string_of_property_attribute prop_attributes in
  let annotation_from_type t =
    match t.Typ.desc with
    | Typ.Tptr (_, Typ.Pk_objc_weak) ->
        [Config.weak]
    | Typ.Tptr (_, Typ.Pk_objc_unsafe_unretained) ->
        [Config.unsafe_unret]
    | _ ->
        []
  in
  let fname = CGeneral_utils.mk_class_field_name class_tname field_name.Clang_ast_t.ni_name in
  let typ = qual_type_to_sil_type tenv qual_type in
  let item_annotations =
    match prop_atts with
    | [] ->
        ({Annot.class_name= Config.ivar_attributes; parameters= annotation_from_type typ}, true)
    | _ ->
        ({Annot.class_name= Config.property_attributes; parameters= prop_atts}, true)
  in
  let item_annotations = item_annotations :: CAst_utils.sil_annot_of_type qual_type in
  (fname, typ, item_annotations)


(* Given a list of declarations in an interface returns a list of fields  *)
let get_fields qual_type_to_sil_type tenv class_tname decl_list =
  let open Clang_ast_t in
  let get_sil_field name_info (qt : qual_type) property_attributes =
    build_sil_field qual_type_to_sil_type tenv class_tname name_info qt property_attributes
  in
  let rec get_field fields decl =
    match decl with
    | ObjCPropertyDecl (_, _, obj_c_property_decl_info) -> (
        let ivar_decl_ref = obj_c_property_decl_info.Clang_ast_t.opdi_ivar_decl in
        let property_attributes = obj_c_property_decl_info.Clang_ast_t.opdi_property_attributes in
        match CAst_utils.get_decl_opt_with_decl_ref ivar_decl_ref with
        | Some (ObjCIvarDecl (_, name_info, qual_type, _, _)) ->
            let field = get_sil_field name_info qual_type property_attributes in
            CGeneral_utils.add_no_duplicates_fields field fields
        | _ ->
            fields )
    | ObjCPropertyImplDecl (_, obj_c_property_impl_decl_info) -> (
        let property_decl_opt = obj_c_property_impl_decl_info.Clang_ast_t.opidi_property_decl in
        match CAst_utils.get_decl_opt_with_decl_ref property_decl_opt with
        | Some decl ->
            get_field fields decl
        | None ->
            fields )
    | ObjCIvarDecl (_, name_info, qual_type, _, _) ->
        let field = get_sil_field name_info qual_type [] in
        CGeneral_utils.add_no_duplicates_fields field fields
    | _ ->
        fields
  in
  List.fold ~f:get_field ~init:[] decl_list


(* Add potential extra fields defined only in the implementation of the class *)
(* to the info given in the interface. Update the tenv accordingly. *)
let add_missing_fields tenv class_name missing_fields =
  let class_tn_name = Typ.Name.Objc.from_qual_name class_name in
  match Tenv.lookup tenv class_tn_name with
  | Some ({fields} as struct_typ) ->
      let new_fields = CGeneral_utils.append_no_duplicates_fields fields missing_fields in
      ignore (Tenv.mk_struct tenv ~default:struct_typ ~fields:new_fields ~statics:[] class_tn_name) ;
      L.(debug Capture Verbose)
        " Updating info for class '%a' in tenv@\n" QualifiedCppName.pp class_name
  | _ ->
      ()


let modelled_fields_in_classes =
  [ ("NSData", "_bytes", Typ.mk (Tptr (Typ.mk Tvoid, Typ.Pk_pointer)))
  ; ("NSArray", "elementData", Typ.mk (Tint Typ.IInt)) ]


let modelled_field class_name_info =
  let modelled_field_in_class res (class_name, field_name, typ) =
    if String.equal class_name class_name_info.Clang_ast_t.ni_name then
      let class_tname = Typ.Name.Objc.from_string class_name in
      let name = Typ.Fieldname.Clang.from_class_name class_tname field_name in
      (name, typ, Annot.Item.empty) :: res
    else res
  in
  List.fold ~f:modelled_field_in_class ~init:[] modelled_fields_in_classes
