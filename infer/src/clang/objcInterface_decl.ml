(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** In this module an ObjC interface declaration or implementation is processed. The class  *)

(** is saved in the tenv as a struct with the corresponding fields, potential superclass and *)

(** list of defined methods *)

(* ObjectiveC doesn't have a notion of static or class fields. *)
(* So, in this module we translate a class into a sil srtuct with an empty list of static fields.*)

module L = Logging

let is_pointer_to_objc_class typ =
  match typ.Typ.desc with Tptr (typ, _) when Typ.is_objc_class typ -> true | _ -> false

let get_super_interface_decl otdi_super =
  match otdi_super with
  | Some dr
   -> Option.map ~f:CAst_utils.get_qualified_name dr.Clang_ast_t.dr_name
  | _
   -> None

let get_protocols protocols =
  let protocol_names =
    List.map
      ~f:(fun decl ->
        match decl.Clang_ast_t.dr_name with
        | Some name_info
         -> CAst_utils.get_qualified_name name_info
        | None
         -> assert false)
      protocols
  in
  protocol_names

let add_class_decl qual_type_to_sil_type tenv idi =
  let decl_ref_opt = idi.Clang_ast_t.oidi_class_interface in
  CAst_utils.add_type_from_decl_ref_opt qual_type_to_sil_type tenv decl_ref_opt true

let add_super_class_decl qual_type_to_sil_type tenv ocdi =
  let decl_ref_opt = ocdi.Clang_ast_t.otdi_super in
  CAst_utils.add_type_from_decl_ref_opt qual_type_to_sil_type tenv decl_ref_opt false

let add_protocols_decl qual_type_to_sil_type tenv protocols =
  CAst_utils.add_type_from_decl_ref_list qual_type_to_sil_type tenv protocols

let add_categories_decl qual_type_to_sil_type tenv categories =
  CAst_utils.add_type_from_decl_ref_list qual_type_to_sil_type tenv categories

let add_class_implementation qual_type_to_sil_type tenv idi =
  let decl_ref_opt = idi.Clang_ast_t.otdi_implementation in
  CAst_utils.add_type_from_decl_ref_opt qual_type_to_sil_type tenv decl_ref_opt false

(*The superclass is the first element in the list of super classes of structs in the tenv, *)
(* then come the protocols and categories. *)
let get_interface_supers super_opt protocols =
  let super_class =
    match super_opt with None -> [] | Some super -> [Typ.Name.Objc.from_qual_name super]
  in
  let protocol_names = List.map ~f:Typ.Name.Objc.protocol_from_qual_name protocols in
  let super_classes = super_class @ protocol_names in
  super_classes

let create_supers_fields qual_type_to_sil_type tenv class_tname decl_list otdi_super otdi_protocols =
  let super = get_super_interface_decl otdi_super in
  let protocols = get_protocols otdi_protocols in
  let supers = get_interface_supers super protocols in
  let fields = CField_decl.get_fields qual_type_to_sil_type tenv class_tname decl_list in
  (supers, fields)

(* Adds pairs (interface name, interface_type_info) to the global environment. *)
let add_class_to_tenv qual_type_to_sil_type tenv decl_info name_info decl_list ocidi =
  let class_name = CAst_utils.get_qualified_name name_info in
  L.(debug Capture Verbose) "ADDING: ObjCInterfaceDecl for '%a'@\n" QualifiedCppName.pp class_name ;
  let interface_name = Typ.Name.Objc.from_qual_name class_name in
  let interface_desc = Typ.Tstruct interface_name in
  let decl_key = Clang_ast_extend.DeclPtr decl_info.Clang_ast_t.di_pointer in
  CAst_utils.update_sil_types_map decl_key interface_desc ;
  let decl_supers, decl_fields =
    create_supers_fields qual_type_to_sil_type tenv interface_name decl_list
      ocidi.Clang_ast_t.otdi_super ocidi.Clang_ast_t.otdi_protocols
  in
  let fields_sc = CField_decl.fields_superclass tenv ocidi in
  List.iter
    ~f:(fun (fn, ft, _) ->
      L.(debug Capture Verbose) "----->SuperClass field: '%s' " (Typ.Fieldname.to_string fn) ;
      L.(debug Capture Verbose) "type: '%s'@\n" (Typ.to_string ft))
    fields_sc ;
  (*In case we found categories, or partial definition of this class earlier and they are already in the tenv *)
  let fields, (supers: Typ.Name.t list) =
    match Tenv.lookup tenv interface_name with
    | Some {fields; supers}
     -> ( CGeneral_utils.append_no_duplicates_fields decl_fields fields
        , CGeneral_utils.append_no_duplicates_csu decl_supers supers )
    | _
     -> (decl_fields, decl_supers)
  in
  let fields = CGeneral_utils.append_no_duplicates_fields fields fields_sc in
  (* We add the special hidden counter_field for implementing reference counting *)
  let modelled_fields =
    Typ.Struct.objc_ref_counter_field :: CField_decl.modelled_field name_info
  in
  let all_fields = CGeneral_utils.append_no_duplicates_fields modelled_fields fields in
  L.(debug Capture Verbose) "Class %a field:@\n" QualifiedCppName.pp class_name ;
  List.iter
    ~f:(fun (fn, _, _) ->
      L.(debug Capture Verbose) "-----> field: '%s'@\n" (Typ.Fieldname.to_string fn))
    all_fields ;
  ignore
    (Tenv.mk_struct tenv ~fields:all_fields ~supers ~methods:[] ~annots:Annot.Class.objc
       interface_name) ;
  L.(debug Capture Verbose)
    "  >>>Verifying that Typename '%s' is in tenv@\n" (Typ.Name.to_string interface_name) ;
  ( match Tenv.lookup tenv interface_name with
  | Some st
   -> L.(debug Capture Verbose)
        "  >>>OK. Found typ='%a'@\n" (Typ.Struct.pp Pp.text interface_name) st
  | None
   -> L.(debug Capture Verbose) "  >>>NOT Found!!@\n" ) ;
  interface_desc

(* Interface_type_info has the name of instance variables and the name of methods. *)
let interface_declaration qual_type_to_sil_type tenv decl =
  let open Clang_ast_t in
  match decl with
  | ObjCInterfaceDecl (decl_info, name_info, decl_list, _, ocidi)
   -> let typ = add_class_to_tenv qual_type_to_sil_type tenv decl_info name_info decl_list ocidi in
      let _ = add_class_implementation qual_type_to_sil_type tenv ocidi in
      let _ = add_super_class_decl qual_type_to_sil_type tenv ocidi in
      let _ = add_protocols_decl qual_type_to_sil_type tenv ocidi.Clang_ast_t.otdi_protocols in
      let known_categories = ocidi.Clang_ast_t.otdi_known_categories in
      let _ = add_categories_decl qual_type_to_sil_type tenv known_categories in
      typ
  | _
   -> assert false

(* Translate the methods defined in the implementation.*)
let interface_impl_declaration qual_type_to_sil_type tenv decl =
  let open Clang_ast_t in
  match decl with
  | ObjCImplementationDecl (decl_info, name_info, decl_list, _, idi)
   -> let class_name = CAst_utils.get_qualified_name name_info in
      L.(debug Capture Verbose)
        "ADDING: ObjCImplementationDecl for class '%a'@\n" QualifiedCppName.pp class_name ;
      let _ = add_class_decl qual_type_to_sil_type tenv idi in
      let class_tn_name = Typ.Name.Objc.from_qual_name class_name in
      let fields = CField_decl.get_fields qual_type_to_sil_type tenv class_tn_name decl_list in
      CField_decl.add_missing_fields tenv class_name fields ;
      let decl_key = Clang_ast_extend.DeclPtr decl_info.Clang_ast_t.di_pointer in
      let class_desc = Typ.Tstruct class_tn_name in
      CAst_utils.update_sil_types_map decl_key class_desc ; class_desc
  | _
   -> assert false
