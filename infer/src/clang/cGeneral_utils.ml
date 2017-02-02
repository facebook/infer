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
  | el1:: el2:: rest ->
      el2:: el1:: (swap_elements_list rest)
  | [] -> []
  | _ -> assert false

let rec string_from_list l =
  match l with
  | [] -> ""
  | [item] -> item
  | item:: l' -> item^" "^(string_from_list l')

let rec append_no_duplicates eq list1 list2 =
  match list2 with
  | el:: rest2 ->
      if (List.mem ~equal:eq list1 el) then
        (append_no_duplicates eq list1 rest2)
      else (append_no_duplicates eq list1 rest2)@[el]
  | [] -> list1

let append_no_duplicates_csu list1 list2 =
  append_no_duplicates Typename.equal list1 list2

let append_no_duplicates_methods list1 list2 =
  append_no_duplicates Procname.equal list1 list2

let append_no_duplicates_annotations list1 list2 =
  let eq (annot1, _) (annot2, _) = String.equal annot1.Annot.class_name annot2.Annot.class_name in
  append_no_duplicates eq list1 list2

let add_no_duplicates_fields field_tuple l =
  let rec replace_field field_tuple l found =
    match field_tuple, l with
    | (field, typ, annot), ((old_field, old_typ, old_annot) as old_field_tuple :: rest) ->
        let ret_list, ret_found = replace_field field_tuple rest found in
        if Ident.equal_fieldname field old_field && Typ.equal typ old_typ then
          let annotations = append_no_duplicates_annotations annot old_annot in
          (field, typ, annotations) :: ret_list, true
        else old_field_tuple :: ret_list, ret_found
    | _, [] -> [], found in
  let new_list, found = replace_field field_tuple l false in
  if found then new_list
  else field_tuple :: l

let rec append_no_duplicates_fields list1 list2 =
  match list1 with
  | field_tuple :: rest ->
      let updated_list2 = append_no_duplicates_fields rest list2 in
      add_no_duplicates_fields field_tuple updated_list2
  | [] -> list2

let sort_fields fields =
  let compare (name1, _, _) (name2, _, _) =
    Ident.compare_fieldname name1 name2 in
  IList.sort compare fields


let sort_fields_tenv tenv =
  let sort_fields_struct name ({StructTyp.fields} as st) =
    ignore (Tenv.mk_struct tenv ~default:st ~fields:(sort_fields fields) name) in
  Tenv.iter sort_fields_struct tenv

let rec collect_list_tuples l (a, a1, b, c, d) =
  match l with
  | [] -> (a, a1, b, c, d)
  | (a', a1', b', c', d'):: l' -> collect_list_tuples l' (a@a', a1@a1', b@b', c@c', d@d')

let is_static_var var_decl_info =
  match var_decl_info.Clang_ast_t.vdi_storage_class with
  | Some sc -> String.equal sc CFrontend_config.static
  | _ -> false

let block_procname_with_index defining_proc i =
  Config.anonymous_block_prefix ^
  (Procname.to_string defining_proc) ^
  Config.anonymous_block_num_sep ^
  (string_of_int i)

(* Global counter for anonymous block*)
let block_counter = ref 0

(* Returns a fresh index for a new anonymous block *)
let get_fresh_block_index () =
  block_counter := !block_counter +1;
  !block_counter

(* Makes a fresh name for a block defined inside the defining procedure.*)
(* It updates the global block_counter *)
let mk_fresh_block_procname defining_proc =
  let name = block_procname_with_index defining_proc (get_fresh_block_index ()) in
  Procname.mangled_objc_block name

(* Returns the next fresh name for a block defined inside the defining procedure *)
(* It does not update the global block_counter *)
let get_next_block_pvar defining_proc =
  let name = block_procname_with_index defining_proc (!block_counter +1) in
  Pvar.mk_tmp name defining_proc

(* Reset  block counter *)
let reset_block_counter () =
  block_counter := 0

let rec zip xs ys =
  match xs, ys with
  | [], _
  | _, [] -> []
  | x :: xs, y :: ys -> (x, y) :: zip xs ys

let list_range i j =
  let rec aux n acc =
    if n < i then acc else aux (n -1) (n :: acc)
  in aux j [] ;;

let replicate n el = IList.map (fun _ -> el) (list_range 0 (n -1))

let mk_class_field_name field_qual_name =
  let field_name = field_qual_name.Clang_ast_t.ni_name in
  let class_name = CAst_utils.get_class_name_from_member field_qual_name in
  Ident.create_fieldname (Mangled.mangled field_name class_name) 0

let is_cpp_translation translation_unit_context =
  let lang = translation_unit_context.CFrontend_config.lang in
  CFrontend_config.equal_clang_lang lang CFrontend_config.CPP ||
  CFrontend_config.equal_clang_lang lang CFrontend_config.ObjCPP

let is_objc_extension translation_unit_context =
  let lang = translation_unit_context.CFrontend_config.lang in
  CFrontend_config.equal_clang_lang lang CFrontend_config.ObjC ||
  CFrontend_config.equal_clang_lang lang CFrontend_config.ObjCPP

let rec get_mangled_method_name function_decl_info method_decl_info =
  (* For virtual methods return mangled name of the method from most base class
     Go recursively until there is no method in any parent class. All names
     of the same method need to be the same, otherwise dynamic dispatch won't
     work. *)
  let open Clang_ast_t in
  match method_decl_info.xmdi_overriden_methods with
  | [] -> function_decl_info.fdi_mangled_name
  | base1_dr :: _ ->
      (let base1 = match CAst_utils.get_decl base1_dr.dr_decl_pointer with
          | Some b -> b
          | _ -> assert false in
       match base1 with
       | CXXMethodDecl (_, _, _, fdi, mdi)
       | CXXConstructorDecl (_, _, _, fdi, mdi)
       | CXXConversionDecl (_, _, _, fdi, mdi)
       | CXXDestructorDecl (_, _, _, fdi, mdi) ->
           get_mangled_method_name fdi mdi
       | _ -> assert false)

let mk_procname_from_function translation_unit_context name function_decl_info_opt =
  let file =
    match function_decl_info_opt with
    | Some (decl_info, function_decl_info) ->
        (match function_decl_info.Clang_ast_t.fdi_storage_class with
         | Some "static" ->
             let file_opt = (fst decl_info.Clang_ast_t.di_source_range).Clang_ast_t.sl_file in
             Option.value_map ~f:SourceFile.to_string ~default:"" file_opt
         | _ -> "")
    | None -> "" in
  let mangled_opt = match function_decl_info_opt with
    | Some (_, function_decl_info) -> function_decl_info.Clang_ast_t.fdi_mangled_name
    | _ -> None in
  let mangled_name =
    match mangled_opt with
    | Some m when is_cpp_translation translation_unit_context -> m
    | _ -> "" in
  let mangled = (Utils.string_crc_hex32 file) ^ mangled_name in
  if String.is_empty file && String.is_empty mangled_name then
    Procname.from_string_c_fun name
  else
    Procname.C (Procname.c name mangled)

let mk_procname_from_objc_method class_name method_name method_kind =
  Procname.ObjC_Cpp
    (Procname.objc_cpp class_name method_name method_kind)

let mk_procname_from_cpp_method class_name method_name ?meth_decl mangled =
  let method_kind = match meth_decl with
    | Some (Clang_ast_t.CXXConstructorDecl (_, _, _, _, {xmdi_is_constexpr})) ->
        Procname.CPPConstructor (mangled, xmdi_is_constexpr)
    | _ ->
        Procname.CPPMethod mangled in
  Procname.ObjC_Cpp
    (Procname.objc_cpp class_name method_name method_kind)

let get_objc_method_name name_info mdi class_name =
  let method_name = name_info.Clang_ast_t.ni_name in
  let is_instance = mdi.Clang_ast_t.omdi_is_instance_method in
  let method_kind = Procname.objc_method_kind_of_bool is_instance in
  mk_procname_from_objc_method class_name method_name method_kind

let procname_of_decl translation_unit_context meth_decl =
  let open Clang_ast_t in
  match meth_decl with
  | FunctionDecl (decl_info, name_info, _, fdi) ->
      let name = CAst_utils.get_qualified_name name_info in
      let function_info = Some (decl_info, fdi) in
      mk_procname_from_function translation_unit_context name function_info
  | CXXMethodDecl (_, name_info, _, fdi, mdi)
  | CXXConstructorDecl (_, name_info, _, fdi, mdi)
  | CXXConversionDecl (_, name_info, _, fdi, mdi)
  | CXXDestructorDecl (_, name_info, _, fdi, mdi) ->
      let mangled = get_mangled_method_name fdi mdi in
      let method_name = CAst_utils.get_unqualified_name name_info in
      let class_name = CAst_utils.get_class_name_from_member name_info in
      mk_procname_from_cpp_method class_name method_name ~meth_decl mangled
  | ObjCMethodDecl (_, name_info, mdi) ->
      let class_name = CAst_utils.get_class_name_from_member name_info in
      get_objc_method_name name_info mdi class_name
  | BlockDecl _ ->
      let name = Config.anonymous_block_prefix ^ Config.anonymous_block_num_sep ^
                 (string_of_int (get_fresh_block_index ())) in
      Procname.mangled_objc_block name
  | _ -> assert false


let get_var_name_mangled name_info var_decl_info =
  let clang_name = CAst_utils.get_qualified_name name_info in
  let param_idx_opt = var_decl_info.Clang_ast_t.vdi_parm_index_in_function in
  let name_string =
    match clang_name, param_idx_opt with
    | "", Some index -> "__param_" ^ string_of_int index
    | "", None -> assert false
    | _ -> clang_name in
  let mangled = match param_idx_opt with
    | Some index -> Mangled.mangled name_string (string_of_int index)
    | None -> Mangled.from_string name_string in
  name_string, mangled

let mk_sil_global_var {CFrontend_config.source_file} ?(mk_name=fun _ x -> x)
    named_decl_info var_decl_info qt =
  let name_string, simple_name = get_var_name_mangled named_decl_info var_decl_info in
  let translation_unit =
    match (var_decl_info.Clang_ast_t.vdi_storage_class,
           var_decl_info.Clang_ast_t.vdi_init_expr) with
    | Some "extern", None ->
        (* some compilers simply disregard "extern" when the global is given some initialisation
           code, which is why we make sure that [vdi_init_expr] is None here... *)
        SourceFile.empty
    | _ ->
        source_file in
  let is_constexpr = var_decl_info.Clang_ast_t.vdi_is_const_expr in
  let is_pod =
    CAst_utils.get_desugared_type qt.Clang_ast_t.qt_type_ptr
    |> Fn.flip Option.bind (function
        | Clang_ast_t.RecordType(_, decl_ptr) -> CAst_utils.get_decl decl_ptr
        | _ -> None)
    |> Option.value_map ~default:true ~f:(function
        | Clang_ast_t.CXXRecordDecl(_, _, _, _, _, _, _, {xrdi_is_pod})
        | Clang_ast_t.ClassTemplateSpecializationDecl(_, _, _, _, _, _, _, {xrdi_is_pod}, _) ->
            xrdi_is_pod
        | _ -> true) in
  Pvar.mk_global ~is_constexpr ~is_pod
    ~is_static_local:(var_decl_info.Clang_ast_t.vdi_is_static_local)
    (mk_name name_string simple_name) translation_unit

let mk_sil_var trans_unit_ctx named_decl_info decl_info_type_ptr_opt procname outer_procname =
  match decl_info_type_ptr_opt with
  | Some (decl_info, qt, var_decl_info, should_be_mangled) ->
      let name_string, simple_name = get_var_name_mangled named_decl_info var_decl_info in
      if var_decl_info.Clang_ast_t.vdi_is_global then
        let mk_name =
          if var_decl_info.Clang_ast_t.vdi_is_static_local then
            Some (fun name_string _ ->
                Mangled.from_string ((Procname.to_string outer_procname) ^ "_" ^ name_string))
          else None in
        mk_sil_global_var trans_unit_ctx ?mk_name named_decl_info var_decl_info qt
      else if not should_be_mangled then Pvar.mk simple_name procname
      else
        let start_location = fst decl_info.Clang_ast_t.di_source_range in
        let line_opt = start_location.Clang_ast_t.sl_line in
        let line_str = match line_opt with | Some line -> string_of_int line | None -> "" in
        let mangled = Utils.string_crc_hex32 line_str in
        let mangled_name = Mangled.mangled name_string mangled in
        Pvar.mk mangled_name procname
  | None ->
      let name_string = CAst_utils.get_qualified_name named_decl_info in
      Pvar.mk (Mangled.from_string name_string) procname
