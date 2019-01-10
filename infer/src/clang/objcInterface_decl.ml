(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** In this module an ObjC interface declaration or implementation is processed. The class  *)

(** is saved in the tenv as a struct with the corresponding fields, potential superclass and *)

(** list of defined methods *)

(* ObjectiveC doesn't have a notion of static or class fields. *)
(* So, in this module we translate a class into a sil srtuct with an empty list of static fields.*)

module L = Logging

let get_super_interface_decl otdi_super =
  match otdi_super with
  | Some dr ->
      Option.map ~f:CAst_utils.get_qualified_name dr.Clang_ast_t.dr_name
  | _ ->
      None


let get_protocols protocols =
  let protocol_names =
    List.map
      ~f:(fun decl ->
        match decl.Clang_ast_t.dr_name with
        | Some name_info ->
            CAst_utils.get_qualified_name name_info
        | None ->
            assert false )
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


let create_supers_fields qual_type_to_sil_type tenv class_tname decl_list otdi_super otdi_protocols
    =
  let super = get_super_interface_decl otdi_super in
  let protocols = get_protocols otdi_protocols in
  let supers = get_interface_supers super protocols in
  let fields = CField_decl.get_fields qual_type_to_sil_type tenv class_tname decl_list in
  (supers, fields)


let append_no_duplicates_typ_name =
  Staged.unstage (IList.append_no_duplicates ~cmp:Typ.Name.compare)


(* Adds pairs (interface name, interface_type_info) to the global environment. *)
let add_class_to_tenv qual_type_to_sil_type procname_from_decl tenv decl_info name_info decl_list
    ocidi =
  let class_name = CAst_utils.get_qualified_name name_info in
  let interface_name = Typ.Name.Objc.from_qual_name class_name in
  let interface_desc = Typ.Tstruct interface_name in
  let decl_key = Clang_ast_extend.DeclPtr decl_info.Clang_ast_t.di_pointer in
  CAst_utils.update_sil_types_map decl_key interface_desc ;
  (* We don't need to add the methods of the superclass *)
  let decl_supers, decl_fields =
    create_supers_fields qual_type_to_sil_type tenv interface_name decl_list
      ocidi.Clang_ast_t.otdi_super ocidi.Clang_ast_t.otdi_protocols
  in
  let fields_sc = CField_decl.fields_superclass tenv ocidi in
  (*In case we found categories, or partial definition of this class earlier and they are already in the tenv *)
  let fields, (supers : Typ.Name.t list), methods =
    match Tenv.lookup tenv interface_name with
    | Some {fields; supers; methods} ->
        ( CGeneral_utils.append_no_duplicates_fields decl_fields fields
        , append_no_duplicates_typ_name decl_supers supers
        , methods )
    | _ ->
        (decl_fields, decl_supers, [])
  in
  let fields = CGeneral_utils.append_no_duplicates_fields fields fields_sc in
  let modelled_fields = CField_decl.modelled_field name_info in
  let all_fields = CGeneral_utils.append_no_duplicates_fields modelled_fields fields in
  let methods =
    CGeneral_utils.append_no_duplicates_methods
      (ObjcMethod_decl.get_methods procname_from_decl tenv decl_list)
      methods
  in
  ignore
    (Tenv.mk_struct tenv ~fields:all_fields ~supers ~methods ~annots:Annot.Class.objc
       ~exported_objc_methods:methods interface_name) ;
  interface_desc


(* Interface_type_info has the name of instance variables and the name of methods. *)
let interface_declaration qual_type_to_sil_type procname_from_decl tenv decl =
  let open Clang_ast_t in
  match decl with
  | ObjCInterfaceDecl (decl_info, name_info, decl_list, _, ocidi) ->
      let typ =
        add_class_to_tenv qual_type_to_sil_type procname_from_decl tenv decl_info name_info
          decl_list ocidi
      in
      add_class_implementation qual_type_to_sil_type tenv ocidi ;
      add_super_class_decl qual_type_to_sil_type tenv ocidi ;
      add_protocols_decl qual_type_to_sil_type tenv ocidi.Clang_ast_t.otdi_protocols ;
      let known_categories = ocidi.Clang_ast_t.otdi_known_categories in
      add_categories_decl qual_type_to_sil_type tenv known_categories ;
      typ
  | _ ->
      assert false


(* Translate the methods defined in the implementation.*)
let interface_impl_declaration qual_type_to_sil_type procname_from_decl tenv decl =
  let open Clang_ast_t in
  match decl with
  | ObjCImplementationDecl (decl_info, name_info, decl_list, _, idi) ->
      let class_name = CAst_utils.get_qualified_name name_info in
      L.(debug Capture Verbose)
        "ADDING: ObjCImplementationDecl for class '%a'@\n" QualifiedCppName.pp class_name ;
      add_class_decl qual_type_to_sil_type tenv idi ;
      let class_tn_name = Typ.Name.Objc.from_qual_name class_name in
      let fields = CField_decl.get_fields qual_type_to_sil_type tenv class_tn_name decl_list in
      CField_decl.add_missing_fields tenv class_name fields ;
      let methods = ObjcMethod_decl.get_methods procname_from_decl tenv decl_list in
      ObjcMethod_decl.add_missing_methods tenv class_tn_name methods ;
      let decl_key = Clang_ast_extend.DeclPtr decl_info.Clang_ast_t.di_pointer in
      let class_desc = Typ.Tstruct class_tn_name in
      CAst_utils.update_sil_types_map decl_key class_desc ;
      class_desc
  | _ ->
      assert false
