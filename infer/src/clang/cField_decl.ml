(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Utility module to retrieve fields of structs of classes *)

open Utils
open CFrontend_utils
open CFrontend_utils.General_utils
open Clang_ast_t

module L = Logging

let mk_class_field_name class_name field_name =
  Ident.create_fieldname (Mangled.mangled field_name (class_name^"_"^field_name)) 0

let rec get_fields_super_classes tenv super_class =
  Printing.log_out "   ... Getting fields of superclass '%s'\n" (Sil.typename_to_string super_class);
  match Sil.tenv_lookup tenv super_class with
  | None -> []
  | Some Sil.Tstruct (fields, _, _, _, (Sil.Class, sc):: _, _, _) ->
      let sc_fields = get_fields_super_classes tenv (Sil.TN_csu (Sil.Class, sc)) in
      append_no_duplicates_fields fields sc_fields
  | Some Sil.Tstruct (fields, _, _, _, _, _, _) -> fields
  | Some _ -> []

let fields_superclass tenv interface_decl_info =
  match interface_decl_info.Clang_ast_t.otdi_super with
  | Some dr ->
      (match dr.Clang_ast_t.dr_name with
        | Some sc -> get_fields_super_classes tenv (CTypes.mk_classname sc)
        | _ -> [])
  | _ -> []

let get_field_www name_field fl =
  let rec scan_fields nn ll =
    match ll with
    | [] -> []
    | (n, t, _):: ll' -> Printing.log_out ">>>>>Searching for field '%s'." (Ident.fieldname_to_string n);
        Printing.log_out " Seen '%s'.\n" nn;
        if (Ident.fieldname_to_string n) = nn then
          [(n, t)]
        else scan_fields nn ll' in
  CTrans_utils.extract_item_from_singleton (scan_fields name_field fl)
    "WARNING: In MemberExpr there must be only one type defininf for the struct. Returning (NO_FIELD_NAME, Tvoid)\n"
    (Ident.create_fieldname (Mangled.from_string "NO_FIELD_NAME") 0, Sil.Tvoid)

let rec build_sil_field tenv class_name field_name qual_type prop_atts =
  let annotation_from_type t =
    match t with
    | Sil.Tptr(_,Sil.Pk_objc_weak) -> [Config.weak]
    | Sil.Tptr(_,Sil.Pk_objc_unsafe_unretained) -> [Config.unsafe_unret]
    | _ -> [] in
  let fname = mk_class_field_name class_name field_name in
  let typ = CTypes_decl.qual_type_to_sil_type tenv qual_type in
  let item_annotations = match prop_atts with
    | None -> [({ Sil.class_name = Config.ivar_attributes; Sil.parameters = annotation_from_type typ }, true)]
    | Some atts -> [({ Sil.class_name = Config.property_attributes; Sil.parameters = atts }, true)] in
  fname, typ, item_annotations

(* From an ivar look for its property and if it finds it returns its attributes *)
let ivar_property curr_class ivar =
  Printing.log_out "Checking if a property is defined for the ivar: '%s'@." ivar;
  match ObjcProperty_decl.Property.find_property_name_from_ivar curr_class ivar with
  | Some pname' ->
      (Printing.log_out "Found property name from ivar: '%s'" pname';
        try
          let _, atts, _, _, _, _ = ObjcProperty_decl.Property.find_property curr_class pname' in
          let atts_str = list_map Clang_ast_j.string_of_property_attribute atts in
          Some atts_str
        with Not_found ->
            Printing.log_out "Didn't find property for pname '%s'" pname';
            None)
  | None -> Printing.log_out "No property found for ivar '%s'@." ivar;
      None

(* Given a list of declarations in an interface returns a list of fields  *)
let rec get_fields tenv curr_class decl_list =
  let class_name = CContext.get_curr_class_name curr_class in
  match decl_list with
  | [] -> []
  | ObjCIvarDecl(decl_info, field_name, qual_type, field_decl_info, obj_c_ivar_decl_info) :: decl_list' ->
      let fields = get_fields tenv curr_class decl_list' in
      (* Doing a post visit here. Adding Ivar after all the declaration have been visited so that *)
      (* ivar names will be added in the property list. *)
      Printing.log_out "  ...Adding Instance Variable '%s' @." field_name;
      let prop_attributes = ivar_property curr_class field_name in
      let (fname, typ, ia) = build_sil_field tenv class_name field_name qual_type prop_attributes in
      Printing.log_out "  ...Resulting sil field: (%s) with attributes:@." ((Ident.fieldname_to_string fname) ^":"^(Sil.typ_to_string typ));
      list_iter (fun (ia', _) ->
              list_iter (fun a -> Printing.log_out "         '%s'@." a) ia'.Sil.parameters) ia;
      (fname, typ, ia):: fields

  | ObjCPropertyImplDecl(decl_info, property_impl_decl_info):: decl_list' ->
      let property_fields_decl =
        ObjcProperty_decl.prepare_dynamic_property curr_class decl_info property_impl_decl_info in
      get_fields tenv curr_class (property_fields_decl @ decl_list')

  | (d : Clang_ast_t.decl):: decl_list' ->
      get_fields tenv curr_class decl_list'
