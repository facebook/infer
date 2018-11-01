(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** General utility functions such as functions on lists *)

type var_info = Clang_ast_t.decl_info * Clang_ast_t.qual_type * Clang_ast_t.var_decl_info * bool

let rec swap_elements_list l =
  match l with
  | el1 :: el2 :: rest ->
      el2 :: el1 :: swap_elements_list rest
  | [] ->
      []
  | _ ->
      assert false


let append_no_duplicates_annotations =
  let cmp (annot1, _) (annot2, _) =
    String.compare annot1.Annot.class_name annot2.Annot.class_name
  in
  Staged.unstage (IList.append_no_duplicates ~cmp)


let append_no_duplicates_methods =
  Staged.unstage (IList.append_no_duplicates ~cmp:Typ.Procname.compare)


let add_no_duplicates_fields field_tuple l =
  let rec replace_field field_tuple l found =
    match (field_tuple, l) with
    | (field, typ, annot), ((old_field, old_typ, old_annot) as old_field_tuple) :: rest ->
        let ret_list, ret_found = replace_field field_tuple rest found in
        if Typ.Fieldname.equal field old_field && Typ.equal typ old_typ then
          let annotations = append_no_duplicates_annotations annot old_annot in
          ((field, typ, annotations) :: ret_list, true)
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


let mk_class_field_name class_tname field_name =
  Typ.Fieldname.Clang.from_class_name class_tname field_name


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
        "__param_" ^ string_of_int index
    | "", None ->
        CFrontend_config.incorrect_assumption __POS__ decl_info.Clang_ast_t.di_source_range
          "Got both empty clang_name and None for param_idx in get_var_name_mangled (%a) (%a)"
          (Pp.to_string ~f:Clang_ast_j.string_of_named_decl_info)
          name_info
          (Pp.to_string ~f:Clang_ast_j.string_of_var_decl_info)
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


let mk_sil_global_var {CFrontend_config.source_file} ?(mk_name = fun _ x -> x) decl_info
    named_decl_info var_decl_info qt =
  let name_string, simple_name = get_var_name_mangled decl_info named_decl_info var_decl_info in
  let translation_unit =
    match Clang_ast_t.(var_decl_info.vdi_is_extern, var_decl_info.vdi_init_expr) with
    | true, None ->
        None
    | _, None when var_decl_info.Clang_ast_t.vdi_is_static_data_member ->
        (* non-const static data member get extern scope unless they are defined out of line here (in which case vdi_init_expr will not be None) *)
        None
    | true, Some _
    (* "extern" variables with initialisation code are not extern at all, but compilers accept this *)
    | false, _ ->
        Some source_file
  in
  let is_constexpr = var_decl_info.Clang_ast_t.vdi_is_const_expr in
  let is_ice = var_decl_info.Clang_ast_t.vdi_is_init_ice in
  let is_pod =
    CAst_utils.get_desugared_type qt.Clang_ast_t.qt_type_ptr
    |> Option.bind ~f:(function
         | Clang_ast_t.RecordType (_, decl_ptr) ->
             CAst_utils.get_decl decl_ptr
         | _ ->
             None )
    |> Option.value_map ~default:true ~f:(function
         | Clang_ast_t.CXXRecordDecl (_, _, _, _, _, _, _, {xrdi_is_pod})
         | Clang_ast_t.ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, {xrdi_is_pod}, _, _)
           ->
             xrdi_is_pod
         | _ ->
             true )
  in
  let is_static_global =
    var_decl_info.Clang_ast_t.vdi_is_global
    (* only top level declarations are really have file scope, static field members have a global scope *)
    && (not var_decl_info.Clang_ast_t.vdi_is_static_data_member)
    && match var_decl_info.Clang_ast_t.vdi_storage_class with Some "static" -> true | _ -> false
  in
  Pvar.mk_global ~is_constexpr ~is_ice ~is_pod
    ~is_static_local:var_decl_info.Clang_ast_t.vdi_is_static_local ~is_static_global
    ?translation_unit (mk_name name_string simple_name)


let mk_sil_var trans_unit_ctx named_decl_info decl_info_qual_type_opt procname outer_procname =
  match decl_info_qual_type_opt with
  | Some (decl_info, qt, var_decl_info, should_be_mangled) ->
      let name_string, simple_name =
        get_var_name_mangled decl_info named_decl_info var_decl_info
      in
      if var_decl_info.Clang_ast_t.vdi_is_global then
        let mk_name =
          if var_decl_info.Clang_ast_t.vdi_is_static_local then
            Some
              (fun name_string _ ->
                Mangled.from_string (Typ.Procname.to_string outer_procname ^ "." ^ name_string) )
          else None
        in
        mk_sil_global_var trans_unit_ctx ?mk_name decl_info named_decl_info var_decl_info qt
      else if not should_be_mangled then Pvar.mk simple_name procname
      else
        let start_location = fst decl_info.Clang_ast_t.di_source_range in
        let line_opt = start_location.Clang_ast_t.sl_line in
        let line_str = match line_opt with Some line -> string_of_int line | None -> "" in
        let mangled = Utils.string_crc_hex32 line_str in
        let mangled_name = Mangled.mangled name_string mangled in
        Pvar.mk mangled_name procname
  | None ->
      let name_string =
        CAst_utils.get_qualified_name named_decl_info |> QualifiedCppName.to_qual_string
      in
      Pvar.mk (Mangled.from_string name_string) procname
