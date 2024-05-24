(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** In this module an ObjC interface declaration or implementation is processed. The class *)

(** is saved in the tenv as a struct with the corresponding fields, potential superclass and *)

(** list of defined methods *)

(* ObjectiveC doesn't have a notion of static or class fields. *)
(* So, in this module we translate a class into a sil srtuct with an empty list of static fields.*)

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


let get_supers otdi_super =
  let get_super_interface_decl otdi_super =
    match otdi_super with
    | Some dr ->
        Option.map ~f:CAst_utils.get_qualified_name dr.Clang_ast_t.dr_name
    | _ ->
        None
  in
  let super_opt = get_super_interface_decl otdi_super in
  match super_opt with None -> [] | Some super -> [Typ.Name.Objc.from_qual_name super]


let append_no_duplicates_typ_name =
  Staged.unstage (IList.append_no_duplicates ~cmp:Typ.Name.compare)


(* Adds pairs (interface name, interface_type_info) to the global environment. *)
let add_class_to_tenv qual_type_to_sil_type procname_from_decl tenv decl_info name_info decl_list
    ~super ~protocols ~is_impl =
  let class_name = CAst_utils.get_qualified_name name_info in
  let interface_name = Typ.Name.Objc.from_qual_name class_name in
  let interface_desc = Typ.Tstruct interface_name in
  let decl_key = Clang_ast_extend.DeclPtr decl_info.Clang_ast_t.di_pointer in
  let source_file =
    let source_loc = fst decl_info.Clang_ast_t.di_source_range in
    Option.map ~f:(fun file -> SourceFile.from_abs_path file) source_loc.Clang_ast_t.sl_file
  in
  CAst_utils.update_sil_types_map decl_key interface_desc ;
  let new_objc_protocols =
    List.filter_map
      ~f:(fun dr ->
        Option.map dr.Clang_ast_t.dr_name ~f:(fun x ->
            CAst_utils.get_qualified_name x |> Typ.Name.Objc.from_qual_name ) )
      protocols
  in
  (* We don't need to add the methods of the superclass *)
  let new_supers = get_supers super in
  let decl_fields = CField_decl.get_fields qual_type_to_sil_type tenv interface_name decl_list in
  let sc_fields = CField_decl.fields_superclass tenv super in
  let modelled_fields = CField_decl.modelled_field name_info in
  let new_fields =
    CGeneral_utils.append_no_duplicates_fields decl_fields modelled_fields
    |> CGeneral_utils.append_no_duplicates_fields sc_fields
  in
  let new_methods = ObjcMethod_decl.get_methods procname_from_decl tenv decl_list in
  let new_exported_objc_methods = if is_impl then [] else new_methods in
  let _ =
    (*In case we found categories, or partial definition of this class earlier and they are already in the tenv *)
    match Tenv.lookup tenv interface_name with
    | Some struct_typ ->
        let fields = CGeneral_utils.append_no_duplicates_fields struct_typ.fields new_fields in
        let methods = CGeneral_utils.append_no_duplicates_methods struct_typ.methods new_methods in
        let exported_objc_methods =
          CGeneral_utils.append_no_duplicates_methods struct_typ.exported_objc_methods
            new_exported_objc_methods
        in
        let supers = append_no_duplicates_typ_name new_supers struct_typ.supers in
        let objc_protocols =
          append_no_duplicates_typ_name new_objc_protocols struct_typ.objc_protocols
        in
        let source_file =
          if Option.is_some struct_typ.Struct.source_file then struct_typ.Struct.source_file
          else source_file
        in
        ignore
          (Tenv.mk_struct tenv ~default:struct_typ ~fields ~supers ~objc_protocols ~methods
             ~exported_objc_methods interface_name ?source_file )
    | None ->
        ignore
          (Tenv.mk_struct tenv ~fields:new_fields ~supers:new_supers
             ~objc_protocols:new_objc_protocols ~methods:new_methods ~annots:Annot.Class.objc
             ~exported_objc_methods:new_exported_objc_methods interface_name ?source_file )
  in
  interface_desc


(* Interface_type_info has the name of instance variables and the name of methods. *)
let interface_declaration qual_type_to_sil_type procname_from_decl tenv decl =
  let open Clang_ast_t in
  match decl with
  | ObjCInterfaceDecl (decl_info, name_info, decl_list, _, ocidi) ->
      let typ =
        add_class_to_tenv qual_type_to_sil_type procname_from_decl tenv decl_info name_info
          decl_list ~super:ocidi.Clang_ast_t.otdi_super ~protocols:ocidi.Clang_ast_t.otdi_protocols
          ~is_impl:false
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
  | ObjCImplementationDecl (decl_info, name_info, decl_list, _, _) ->
      add_class_to_tenv qual_type_to_sil_type procname_from_decl tenv decl_info name_info decl_list
        ~super:None ~protocols:[] ~is_impl:true
  | _ ->
      assert false
