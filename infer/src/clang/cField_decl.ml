(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Utility module to retrieve fields of structs of classes *)

module L = Logging

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


let fields_superclass tenv otdi_super =
  match otdi_super with
  | Some dr -> (
    match dr.Clang_ast_t.dr_name with
    | Some sc ->
        let classname = Typ.Name.Objc.from_qual_name (CAst_utils.get_qualified_name sc) in
        get_fields_super_classes tenv classname
    | _ ->
        [] )
  | _ ->
      []


let get_objc_property_attribute att =
  match att with
  | `Readonly ->
      None
  | `Assign ->
      Some Struct.Weak
  | `Readwrite ->
      None
  | `Retain ->
      Some Struct.Strong
  | `Copy ->
      Some Struct.Copy
  | `Nonatomic | `Atomic ->
      None
  | `Weak ->
      Some Struct.Weak
  | `Strong ->
      Some Struct.Strong
  | `Unsafe_unretained ->
      Some Struct.Weak
  | `ExplicitGetter | `ExplicitSetter ->
      None


let build_sil_field qual_type_to_sil_type tenv class_tname ni_name qual_type prop_attributes =
  let prop_atts = List.filter_map ~f:(fun att -> get_objc_property_attribute att) prop_attributes in
  let attribute_from_type t =
    match t.Typ.desc with
    | Typ.Tptr (_, Typ.Pk_objc_weak) ->
        Some Struct.Weak
    | Typ.Tptr (_, Typ.Pk_objc_unsafe_unretained) ->
        Some Struct.Weak
    | _ ->
        None
  in
  let typ = qual_type_to_sil_type tenv qual_type in
  let objc_property_attributes =
    match attribute_from_type typ with
    | Some attr ->
        if List.mem prop_atts ~equal:Struct.equal_objc_property_attribute attr then prop_atts
        else attr :: prop_atts
    | None ->
        prop_atts
  in
  let item_annotations = CAst_utils.sil_annot_of_type qual_type in
  let is_weak =
    List.exists objc_property_attributes ~f:(fun attr ->
        Struct.equal_objc_property_attribute attr Weak )
  in
  let fname = Fieldname.make class_tname ni_name ~is_weak in
  Struct.mk_field fname typ ~annot:item_annotations ~objc_property_attributes


(* Given a list of declarations in an interface returns a list of fields  *)
let get_fields qual_type_to_sil_type tenv class_tname decl_list =
  let open Clang_ast_t in
  let get_sil_field ni_name (qt : qual_type) property_attributes =
    build_sil_field qual_type_to_sil_type tenv class_tname ni_name qt property_attributes
  in
  let rec get_field fields decl =
    match decl with
    | ObjCPropertyDecl (_, {ni_name}, {opdi_qual_type; opdi_ivar_decl; opdi_property_attributes}) ->
        let ni_name, qual_type =
          match CAst_utils.get_decl_opt_with_decl_ref_opt opdi_ivar_decl with
          | Some (ObjCIvarDecl (_, {ni_name}, qual_type, _, _)) ->
              (ni_name, qual_type)
          | _ ->
              let ni_name = "_" ^ ni_name in
              (ni_name, opdi_qual_type)
        in
        let field = get_sil_field ni_name qual_type opdi_property_attributes in
        CGeneral_utils.add_no_duplicates_fields field fields
    | ObjCPropertyImplDecl (_, obj_c_property_impl_decl_info) -> (
        let property_decl_opt = obj_c_property_impl_decl_info.Clang_ast_t.opidi_property_decl in
        match CAst_utils.get_decl_opt_with_decl_ref_opt property_decl_opt with
        | Some decl ->
            get_field fields decl
        | None ->
            fields )
    | ObjCIvarDecl (_, {ni_name}, qual_type, _, _) ->
        let field = get_sil_field ni_name qual_type [] in
        CGeneral_utils.add_no_duplicates_fields field fields
    | _ ->
        fields
  in
  List.fold ~f:get_field ~init:[] decl_list


let modelled_fields_in_classes = [("NSArray", "elementData", Typ.mk (Tint Typ.IInt))]

let modelled_field class_name_info =
  let modelled_field_in_class res (class_name, field_name, typ) =
    if String.equal class_name class_name_info.Clang_ast_t.ni_name then
      let class_tname = Typ.Name.Objc.from_string class_name in
      let is_weak = Typ.is_weak_pointer typ in
      let name = Fieldname.make class_tname field_name ~is_weak in
      let field = Struct.mk_field name typ in
      field :: res
    else res
  in
  List.fold ~f:modelled_field_in_class ~init:[] modelled_fields_in_classes
