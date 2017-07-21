(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** General utility functions such as functions on lists *)

module L = Logging
module F = Format

type var_info = Clang_ast_t.decl_info * Clang_ast_t.qual_type * Clang_ast_t.var_decl_info * bool

let rec swap_elements_list l =
  match l with
  | el1 :: el2 :: rest
   -> el2 :: el1 :: swap_elements_list rest
  | []
   -> []
  | _
   -> assert false

let rec string_from_list l =
  match l with [] -> "" | [item] -> item | item :: l' -> item ^ " " ^ string_from_list l'

let rec append_no_duplicates eq list1 list2 =
  match list2 with
  | el :: rest2
   -> if List.mem ~equal:eq list1 el then append_no_duplicates eq list1 rest2
      else append_no_duplicates eq list1 rest2 @ [el]
  | []
   -> list1

let append_no_duplicates_csu list1 list2 = append_no_duplicates Typ.Name.equal list1 list2

let append_no_duplicates_annotations list1 list2 =
  let eq (annot1, _) (annot2, _) = String.equal annot1.Annot.class_name annot2.Annot.class_name in
  append_no_duplicates eq list1 list2

let add_no_duplicates_fields field_tuple l =
  let rec replace_field field_tuple l found =
    match (field_tuple, l) with
    | (field, typ, annot), (old_field, old_typ, old_annot as old_field_tuple) :: rest
     -> let ret_list, ret_found = replace_field field_tuple rest found in
        if Typ.Fieldname.equal field old_field && Typ.equal typ old_typ then
          let annotations = append_no_duplicates_annotations annot old_annot in
          ((field, typ, annotations) :: ret_list, true)
        else (old_field_tuple :: ret_list, ret_found)
    | _, []
     -> ([], found)
  in
  let new_list, found = replace_field field_tuple l false in
  if found then new_list else field_tuple :: l

let rec append_no_duplicates_fields list1 list2 =
  match list1 with
  | field_tuple :: rest
   -> let updated_list2 = append_no_duplicates_fields rest list2 in
      add_no_duplicates_fields field_tuple updated_list2
  | []
   -> list2

let sort_fields fields =
  let compare (name1, _, _) (name2, _, _) = Typ.Fieldname.compare name1 name2 in
  List.sort ~cmp:compare fields

let sort_fields_tenv tenv =
  let sort_fields_struct name ({Typ.Struct.fields} as st) =
    ignore (Tenv.mk_struct tenv ~default:st ~fields:(sort_fields fields) name)
  in
  Tenv.iter sort_fields_struct tenv

let rec collect_list_tuples l (a, a1, b, c, d) =
  match l with
  | []
   -> (a, a1, b, c, d)
  | (a', a1', b', c', d') :: l'
   -> collect_list_tuples l' (a @ a', a1 @ a1', b @ b', c @ c', d @ d')

let is_static_var var_decl_info =
  match var_decl_info.Clang_ast_t.vdi_storage_class with
  | Some sc
   -> String.equal sc CFrontend_config.static
  | _
   -> false

let rec zip xs ys =
  match (xs, ys) with [], _ | _, [] -> [] | x :: xs, y :: ys -> (x, y) :: zip xs ys

let list_range i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let replicate n el = List.map ~f:(fun _ -> el) (list_range 0 (n - 1))

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

let get_var_name_mangled name_info var_decl_info =
  let clang_name = CAst_utils.get_qualified_name name_info |> QualifiedCppName.to_qual_string in
  let param_idx_opt = var_decl_info.Clang_ast_t.vdi_parm_index_in_function in
  let name_string =
    match (clang_name, param_idx_opt) with
    | "", Some index
     -> "__param_" ^ string_of_int index
    | "", None
     -> assert false
    | _
     -> clang_name
  in
  let mangled =
    match param_idx_opt with
    | Some index
     -> Mangled.mangled name_string (string_of_int index)
    | None
     -> Mangled.from_string name_string
  in
  (name_string, mangled)

let mk_sil_global_var {CFrontend_config.source_file} ?(mk_name= fun _ x -> x) named_decl_info
    var_decl_info qt =
  let name_string, simple_name = get_var_name_mangled named_decl_info var_decl_info in
  let translation_unit =
    match
      (var_decl_info.Clang_ast_t.vdi_storage_class, var_decl_info.Clang_ast_t.vdi_init_expr)
    with
    | Some "extern", None
     -> (* some compilers simply disregard "extern" when the global is given some initialisation
           code, which is why we make sure that [vdi_init_expr] is None here... *)
        Pvar.TUExtern
    | _
     -> Pvar.TUFile source_file
  in
  let is_constexpr = var_decl_info.Clang_ast_t.vdi_is_const_expr in
  let is_pod =
    CAst_utils.get_desugared_type qt.Clang_ast_t.qt_type_ptr
    |> Option.bind ~f:(function
         | Clang_ast_t.RecordType (_, decl_ptr)
          -> CAst_utils.get_decl decl_ptr
         | _
          -> None )
    |> Option.value_map ~default:true ~f:(function
         | Clang_ast_t.CXXRecordDecl (_, _, _, _, _, _, _, {xrdi_is_pod})
         | Clang_ast_t.ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, {xrdi_is_pod}, _)
          -> xrdi_is_pod
         | _
          -> true )
  in
  Pvar.mk_global ~is_constexpr ~is_pod
    ~is_static_local:var_decl_info.Clang_ast_t.vdi_is_static_local
    (mk_name name_string simple_name) translation_unit

let mk_sil_var trans_unit_ctx named_decl_info decl_info_qual_type_opt procname outer_procname =
  match decl_info_qual_type_opt with
  | Some (decl_info, qt, var_decl_info, should_be_mangled)
   -> let name_string, simple_name = get_var_name_mangled named_decl_info var_decl_info in
      if var_decl_info.Clang_ast_t.vdi_is_global then
        let mk_name =
          if var_decl_info.Clang_ast_t.vdi_is_static_local then
            Some
              (fun name_string _ ->
                Mangled.from_string (Typ.Procname.to_string outer_procname ^ "_" ^ name_string))
          else None
        in
        mk_sil_global_var trans_unit_ctx ?mk_name named_decl_info var_decl_info qt
      else if not should_be_mangled then Pvar.mk simple_name procname
      else
        let start_location = fst decl_info.Clang_ast_t.di_source_range in
        let line_opt = start_location.Clang_ast_t.sl_line in
        let line_str = match line_opt with Some line -> string_of_int line | None -> "" in
        let mangled = Utils.string_crc_hex32 line_str in
        let mangled_name = Mangled.mangled name_string mangled in
        Pvar.mk mangled_name procname
  | None
   -> let name_string =
        CAst_utils.get_qualified_name named_decl_info |> QualifiedCppName.to_qual_string
      in
      Pvar.mk (Mangled.from_string name_string) procname
