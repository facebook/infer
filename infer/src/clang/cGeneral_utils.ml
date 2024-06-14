(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** General utility functions such as functions on lists *)

let rec swap_elements_list l =
  match l with
  | el1 :: el2 :: rest ->
      el2 :: el1 :: swap_elements_list rest
  | [] ->
      []
  | _ ->
      assert false


let append_no_duplicates_annotations =
  let cmp annot1 annot2 = String.compare annot1.Annot.class_name annot2.Annot.class_name in
  Staged.unstage (IList.append_no_duplicates ~cmp)


let append_no_duplicates_attr =
  Staged.unstage (IList.append_no_duplicates ~cmp:Struct.compare_objc_property_attribute)


let append_no_duplicates_methods = Staged.unstage (IList.append_no_duplicates ~cmp:Procname.compare)

let add_no_duplicates_fields field_tuple l =
  let rec replace_field field_tuple l found =
    match (field_tuple, l) with
    | ( {Struct.name= field; typ; annot; objc_property_attributes}
      , ( { Struct.name= old_field
          ; typ= old_typ
          ; annot= old_annot
          ; objc_property_attributes= old_objc_property_attributes } as old_field_tuple )
        :: rest ) ->
        let ret_list, ret_found = replace_field field_tuple rest found in
        if Fieldname.equal field old_field && Typ.equal typ old_typ then
          let annotations = append_no_duplicates_annotations annot old_annot in
          let objc_property_attributes =
            append_no_duplicates_attr objc_property_attributes old_objc_property_attributes
          in
          let field = Struct.mk_field field typ ~annot:annotations ~objc_property_attributes in
          (field :: ret_list, true)
        else (old_field_tuple :: ret_list, ret_found)
    | _, [] ->
        ([], found)
  in
  let new_list, found = replace_field field_tuple l false in
  if found then new_list else field_tuple :: l


let rec append_no_duplicates_fields list1 list2 =
  match list1 with
  | field_tuple :: rest ->
      let updated_list2 = append_no_duplicates_fields rest list2 in
      add_no_duplicates_fields field_tuple updated_list2
  | [] ->
      list2


let list_range i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []


let is_cpp_translation translation_unit_context =
  let lang = translation_unit_context.CFrontend_config.lang in
  CFrontend_config.equal_clang_lang lang CFrontend_config.CPP
  || CFrontend_config.equal_clang_lang lang CFrontend_config.ObjCPP


let is_objc_extension translation_unit_context =
  let lang = translation_unit_context.CFrontend_config.lang in
  CFrontend_config.equal_clang_lang lang CFrontend_config.ObjC
  || CFrontend_config.equal_clang_lang lang CFrontend_config.ObjCPP


let get_var_name_mangled decl_info name_info var_decl_info =
  let clang_name = CAst_utils.get_qualified_name name_info |> QualifiedCppName.to_qual_string in
  let param_idx_opt = var_decl_info.Clang_ast_t.vdi_parm_index_in_function in
  let name_string =
    match (clang_name, param_idx_opt) with
    | "", Some index ->
        Pvar.unnamed_param_prefix ^ string_of_int index
    | "", None ->
        CFrontend_errors.incorrect_assumption __POS__ decl_info.Clang_ast_t.di_source_range
          "Got both empty clang_name and None for param_idx in get_var_name_mangled (%a) (%a)"
          (Pp.of_string ~f:Clang_ast_j.string_of_named_decl_info)
          name_info
          (Pp.of_string ~f:Clang_ast_j.string_of_var_decl_info)
          var_decl_info
    | _ ->
        clang_name
  in
  let mangled =
    match param_idx_opt with
    | Some index ->
        Mangled.mangled name_string (string_of_int index)
    | None ->
        Mangled.from_string name_string
  in
  (name_string, mangled)


let is_type_pod qt =
  let desugared_type = CAst_utils.get_desugared_type qt.Clang_ast_t.qt_type_ptr in
  let is_reference =
    Option.exists desugared_type ~f:(function
      | Clang_ast_t.LValueReferenceType _ | Clang_ast_t.RValueReferenceType _ ->
          true
      | _ ->
          false )
  in
  if is_reference then false
  else
    Option.bind desugared_type ~f:(function
      | Clang_ast_t.RecordType (_, decl_ptr) ->
          CAst_utils.get_decl decl_ptr
      | _ ->
          None )
    |> Option.value_map ~default:true ~f:(function
         | Clang_ast_t.(
             ( CXXRecordDecl (_, _, _, _, _, _, _, {xrdi_is_pod})
             | ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, {xrdi_is_pod}, _, _, _)
             | ClassTemplatePartialSpecializationDecl (_, _, _, _, _, _, _, {xrdi_is_pod}, _, _, _)
               )) ->
             xrdi_is_pod
         | _ ->
             true )
