(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Functions for transformations of ast nodes *)

module L = Logging
module F = Format

type qual_type_to_sil_type = Tenv.t -> Clang_ast_t.qual_type -> Typ.t

type procname_from_decl =
     ?tenv:Tenv.t
  -> ?block_return_type:Clang_ast_t.qual_type
  -> ?outer_proc:Procname.t
  -> Clang_ast_t.decl
  -> Procname.t

let sanitize_name = String.tr_multi ~target:"/ " ~replacement:"_" |> Staged.unstage

(** Here we simplify the name "lambda_at_path" to keep just the file name and line and column, these
    are sufficient as identifiers and the full path to the file causes too long names. *)
let reduce_lambda_anon_name name =
  let path = String.chop_prefix_if_exists ~prefix:"lambda_" name in
  let path_items = String.split ~on:'/' path in
  match List.hd (List.rev path_items) with
  | Some last -> (
    match String.split ~on:':' last with
    | [file; line; _] ->
        let hash = String.sub (Utils.string_crc_hex32 name) ~pos:0 ~len:8 in
        F.asprintf "lambda_%s:%s_%s" file line hash
    | _ ->
        name )
  | _ ->
      name


let create_objc_block_name decl_info block_decl_info =
  let source_location, _ = decl_info.Clang_ast_t.di_source_range in
  let path = Option.value source_location.Clang_ast_t.sl_file ~default:"" in
  let line = Option.value source_location.Clang_ast_t.sl_line ~default:(-1) in
  let path_items = String.split ~on:'/' path in
  let file = List.hd_exn (List.rev path_items) in
  let hash =
    String.sub (Utils.string_crc_hex32 block_decl_info.Clang_ast_t.bdi_mangled_name) ~pos:0 ~len:8
  in
  (F.asprintf "%s%s:%d" Config.anonymous_block_prefix file line, hash)


let is_lambda_qual name =
  let is_lambda = String.is_substring name ~substring:"lambda" in
  match String.index name ':' with
  | Some colon_index -> (
    match String.index name '<' with
    | None ->
        is_lambda
    | Some template_index ->
        is_lambda && template_index > colon_index
        (* only transform lambda when it is not a template argument *) )
  | None ->
      false


let get_qual_name qual_name_list =
  (* The name given to the lambda has two qualifiers, "lambda_at_path" and "method_name". The first one is enough
     to identify the lambda, so we remove the second. *)
  let names =
    match qual_name_list with
    | name :: _ when is_lambda_qual name ->
        [reduce_lambda_anon_name name]
    | _ ->
        qual_name_list
  in
  let names = List.map ~f:sanitize_name names in
  QualifiedCppName.of_rev_list names


let get_qualified_name ?(linters_mode = false) name_info =
  if not linters_mode then get_qual_name name_info.Clang_ast_t.ni_qual_name
  else
    (* Because we are in linters mode, we can't get precise info about templates,
       so we strip the template characters to not upset invariants in the system. *)
    let replace_template_chars qual_name =
      String.tr ~target:'<' ~replacement:'_' qual_name |> String.tr ~target:'>' ~replacement:'_'
    in
    let qual_names = List.map ~f:replace_template_chars name_info.Clang_ast_t.ni_qual_name in
    get_qual_name qual_names


let get_unqualified_name name_info =
  let name =
    match name_info.Clang_ast_t.ni_qual_name with
    | name :: _ ->
        if is_lambda_qual name then reduce_lambda_anon_name name else name
    | [] ->
        name_info.Clang_ast_t.ni_name
  in
  sanitize_name name


let get_class_name_from_member member_name_info =
  match member_name_info.Clang_ast_t.ni_qual_name with
  | _ :: class_qual_list ->
      get_qual_name class_qual_list
  | [] ->
      assert false


let pointer_counter = ref 0

let get_fresh_pointer () =
  pointer_counter := !pointer_counter + 1 ;
  let internal_pointer = - !pointer_counter in
  internal_pointer


let dummy_source_range () =
  let dummy_source_loc =
    { Clang_ast_t.sl_file= None
    ; sl_line= None
    ; sl_column= None
    ; sl_macro_file= None
    ; sl_macro_line= None
    ; sl_is_macro= false }
  in
  (dummy_source_loc, dummy_source_loc)


let dummy_stmt_info () =
  {Clang_ast_t.si_pointer= get_fresh_pointer (); si_source_range= dummy_source_range ()}


let named_decl_info_equal ndi1 ndi2 =
  match
    List.for_all2 ndi1.Clang_ast_t.ni_qual_name ndi2.Clang_ast_t.ni_qual_name ~f:String.equal
  with
  | List.Or_unequal_lengths.Ok b ->
      b
  | List.Or_unequal_lengths.Unequal_lengths ->
      false


let get_decl decl_ptr =
  let decl = Int.Table.find ClangPointers.pointer_decl_table decl_ptr in
  match decl with
  | Some (VarDecl ({di_parent_pointer= Some parent_pointer}, ndi, _, _)) -> (
    match Int.Table.find ClangPointers.pointer_decl_table parent_pointer with
    | Some (CXXRecordDecl (_, _, _, decls, _, _, _, _)) -> (
        let has_same_ndi = function
          | Clang_ast_t.VarDecl (_, ndi', _, _) ->
              named_decl_info_equal ndi ndi'
          | _ ->
              false
        in
        match List.find decls ~f:has_same_ndi with Some _ as decl' -> decl' | None -> decl )
    | _ ->
        decl )
  | _ ->
      decl


let get_decl_opt decl_ptr_opt =
  match decl_ptr_opt with Some decl_ptr -> get_decl decl_ptr | None -> None


let get_stmt stmt_ptr source_range =
  let stmt = Int.Table.find ClangPointers.pointer_stmt_table stmt_ptr in
  if Option.is_none stmt then
    CFrontend_errors.incorrect_assumption __POS__ source_range "stmt with pointer %d not found@\n"
      stmt_ptr ;
  stmt


let get_stmt_exn stmt_ptr source_range =
  match get_stmt stmt_ptr source_range with
  | Some stmt ->
      stmt
  | None ->
      L.die InternalError "statement clang pointer %d not found" stmt_ptr


let get_stmt_opt stmt_ptr_opt source_range =
  match stmt_ptr_opt with Some stmt_ptr -> get_stmt stmt_ptr source_range | None -> None


let get_decl_opt_with_decl_ref decl_ref =
  L.debug Capture Verbose "#####POINTER LOOK UP: '%i'@\n" decl_ref.Clang_ast_t.dr_decl_pointer ;
  get_decl decl_ref.Clang_ast_t.dr_decl_pointer


let get_decl_opt_with_decl_ref_opt decl_ref_opt =
  Option.bind decl_ref_opt ~f:get_decl_opt_with_decl_ref


let update_sil_types_map type_ptr sil_type =
  CFrontend_config.sil_types_map :=
    Clang_ast_extend.TypePointerMap.add type_ptr sil_type !CFrontend_config.sil_types_map


let update_enum_map_exn enum_constant_pointer sil_exp =
  let predecessor_pointer_opt, _ =
    ClangPointers.Map.find_exn !CFrontend_config.enum_map enum_constant_pointer
  in
  let enum_map_value = (predecessor_pointer_opt, Some sil_exp) in
  CFrontend_config.enum_map :=
    ClangPointers.Map.set !CFrontend_config.enum_map ~key:enum_constant_pointer ~data:enum_map_value


let add_enum_constant enum_constant_pointer predecessor_pointer_opt =
  let enum_map_value = (predecessor_pointer_opt, None) in
  CFrontend_config.enum_map :=
    ClangPointers.Map.set !CFrontend_config.enum_map ~key:enum_constant_pointer ~data:enum_map_value


let get_enum_constant_exp_exn enum_constant_pointer =
  ClangPointers.Map.find_exn !CFrontend_config.enum_map enum_constant_pointer


let get_type type_ptr =
  match type_ptr with
  (* There is chance for success only if type_ptr is in fact clang pointer *)
  | Clang_ast_types.TypePtr.Ptr raw_ptr ->
      Int.Table.find ClangPointers.pointer_type_table raw_ptr
  | _ ->
      (* TODO(T30739447): investigate why this happens *)
      (* otherwise, function fails *)
      let type_str = Clang_ast_extend.type_ptr_to_string type_ptr in
      L.(debug Capture Medium) "type %s is not clang pointer@\n" type_str ;
      None


let get_desugared_type type_ptr =
  let typ_opt = get_type type_ptr in
  match typ_opt with
  | Some typ -> (
      let type_info = Clang_ast_proj.get_type_tuple typ in
      match type_info.Clang_ast_t.ti_desugared_type with Some ptr -> get_type ptr | _ -> typ_opt )
  | _ ->
      typ_opt


let get_decl_from_typ_ptr typ_ptr =
  let typ_opt = get_desugared_type typ_ptr in
  let typ = match typ_opt with Some t -> t | None -> assert false in
  match (typ : Clang_ast_t.c_type) with
  | RecordType (_, decl_ptr) | ObjCInterfaceType (_, decl_ptr) ->
      get_decl decl_ptr
  | _ ->
      None


let sil_annot_of_type {Clang_ast_t.qt_type_ptr} =
  let mk_annot annot_name_opt =
    match annot_name_opt with
    | Some annot_name ->
        [{Annot.class_name= annot_name; parameters= []}]
    | None ->
        Annot.Item.empty
  in
  let annot_name_opt =
    match get_type qt_type_ptr with
    | Some (AttributedType (_, {ati_attr_kind= TypeNullableAttrKind})) ->
        Some Annotations.nullable
    | Some (AttributedType (_, {ati_attr_kind= TypeNonNullAttrKind})) ->
        Some Annotations.nonnull
    | _ ->
        None
  in
  mk_annot annot_name_opt


let qual_type_of_decl_ptr decl_ptr =
  { (* This function needs to be in this module - CAst_utils can't depend on
       Ast_expressions *)
    Clang_ast_t.qt_type_ptr= Clang_ast_extend.DeclPtr decl_ptr
  ; qt_is_const= false
  ; qt_is_volatile= false
  ; qt_is_restrict= false }


let add_type_from_decl_ref qual_type_to_sil_type tenv dr =
  let qual_type = qual_type_of_decl_ptr dr.Clang_ast_t.dr_decl_pointer in
  ignore (qual_type_to_sil_type tenv qual_type)


let add_type_from_decl_ref_opt qual_type_to_sil_type tenv decl_ref_opt fail_if_not_found =
  match decl_ref_opt with
  (* translate interface first if found *)
  | Some dr ->
      add_type_from_decl_ref qual_type_to_sil_type tenv dr
  | _ ->
      if fail_if_not_found then assert false


let add_type_from_decl_ref_list qual_type_to_sil_type tenv decl_ref_list =
  List.iter ~f:(add_type_from_decl_ref qual_type_to_sil_type tenv) decl_ref_list


let get_function_decl_with_body decl_ptr =
  let open Clang_ast_t in
  let decl_opt = get_decl decl_ptr in
  let decl_ptr' =
    match decl_opt with
    | Some (FunctionDecl (_, _, _, fdecl_info))
    | Some (CXXMethodDecl (_, _, _, fdecl_info, _))
    | Some (CXXConstructorDecl (_, _, _, fdecl_info, _))
    | Some (CXXConversionDecl (_, _, _, fdecl_info, _))
    | Some (CXXDestructorDecl (_, _, _, fdecl_info, _)) ->
        fdecl_info.Clang_ast_t.fdi_decl_ptr_with_body
    | _ ->
        Some decl_ptr
  in
  if [%equal: int option] decl_ptr' (Some decl_ptr) then decl_opt else get_decl_opt decl_ptr'


let get_info_from_decl_ref decl_ref =
  let name_info = match decl_ref.Clang_ast_t.dr_name with Some ni -> ni | _ -> assert false in
  let decl_ptr = decl_ref.Clang_ast_t.dr_decl_pointer in
  let qual_type =
    match decl_ref.Clang_ast_t.dr_qual_type with Some tp -> tp | _ -> assert false
  in
  (name_info, decl_ptr, qual_type)


let type_of_decl decl =
  let open Clang_ast_t in
  match decl with
  | ObjCMethodDecl (_, _, obj_c_method_decl_info) ->
      Some obj_c_method_decl_info.omdi_result_type.qt_type_ptr
  | ObjCPropertyDecl (_, _, obj_c_property_decl_info) ->
      Some obj_c_property_decl_info.opdi_qual_type.qt_type_ptr
  | EnumDecl (_, _, type_ptr, _, _, _, _)
  | RecordDecl (_, _, type_ptr, _, _, _, _)
  | CXXRecordDecl (_, _, type_ptr, _, _, _, _, _)
  | ClassTemplateSpecializationDecl (_, _, type_ptr, _, _, _, _, _, _, _, _)
  | ClassTemplatePartialSpecializationDecl (_, _, type_ptr, _, _, _, _, _, _, _, _)
  | TemplateTypeParmDecl (_, _, type_ptr)
  | ObjCTypeParamDecl (_, _, type_ptr)
  | TypeAliasDecl (_, _, type_ptr)
  | TypedefDecl (_, _, type_ptr, _)
  | UnresolvedUsingTypenameDecl (_, _, type_ptr) ->
      Some type_ptr
  | BindingDecl (_, _, qual_type, _)
  | FieldDecl (_, _, qual_type, _)
  | ObjCAtDefsFieldDecl (_, _, qual_type, _)
  | ObjCIvarDecl (_, _, qual_type, _, _)
  | FunctionDecl (_, _, qual_type, _)
  | CXXMethodDecl (_, _, qual_type, _, _)
  | CXXConstructorDecl (_, _, qual_type, _, _)
  | CXXConversionDecl (_, _, qual_type, _, _)
  | CXXDestructorDecl (_, _, qual_type, _, _)
  | MSPropertyDecl (_, _, qual_type)
  | NonTypeTemplateParmDecl (_, _, qual_type)
  | VarDecl (_, _, qual_type, _)
  | DecompositionDecl (_, _, qual_type, _, _)
  | ImplicitParamDecl (_, _, qual_type, _)
  | OMPCapturedExprDecl (_, _, qual_type, _)
  | ParmVarDecl (_, _, qual_type, _)
  | VarTemplateSpecializationDecl (_, _, _, qual_type, _)
  | VarTemplatePartialSpecializationDecl (_, _, _, qual_type, _)
  | EnumConstantDecl (_, _, qual_type, _)
  | IndirectFieldDecl (_, _, qual_type, _)
  | OMPDeclareReductionDecl (_, _, qual_type)
  | UnresolvedUsingValueDecl (_, _, qual_type) ->
      Some qual_type.qt_type_ptr
  | _ ->
      None


let get_record_fields decl =
  let open Clang_ast_t in
  match decl with
  | ClassTemplateSpecializationDecl (_, _, _, decl_list, _, _, _, _, _, _, _)
  | CXXRecordDecl (_, _, _, decl_list, _, _, _, _)
  | RecordDecl (_, _, _, decl_list, _, _, _) ->
      List.filter ~f:(function FieldDecl _ -> true | _ -> false) decl_list
  | _ ->
      []


let get_cxx_base_classes decl =
  let open Clang_ast_t in
  match decl with
  | CXXRecordDecl (_, _, _, _, _, _, _, cxx_record_info)
  | ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, cxx_record_info, _, _, _) ->
      cxx_record_info.xrdi_bases
  | _ ->
      []


let get_cxx_virtual_base_classes decl =
  let open Clang_ast_t in
  match decl with
  | CXXRecordDecl (_, _, _, _, _, _, _, cxx_record_info)
  | ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, cxx_record_info, _, _, _) ->
      cxx_record_info.xrdi_transitive_vbases
  | _ ->
      []


(* true if a decl has a NS_NOESCAPE attribute *)
let is_no_escape_block_arg decl =
  let has_noescape_attr attr = match attr with `NoEscapeAttr _ -> true | _ -> false in
  let attrs = (Clang_ast_proj.get_decl_tuple decl).di_attributes in
  List.exists attrs ~f:has_noescape_attr


let is_cpp_implicit_decl decl =
  let decl_info = Clang_ast_proj.get_decl_tuple decl in
  decl_info.Clang_ast_t.di_is_implicit


let get_superclass_curr_class_objc_from_decl (decl : Clang_ast_t.decl) =
  match decl with
  | ObjCInterfaceDecl (_, _, _, _, otdi) ->
      otdi.otdi_super
  | ObjCImplementationDecl (_, ni, _, _, oi) -> (
    match
      oi.Clang_ast_t.oidi_class_interface
      |> Option.map ~f:(fun dr -> dr.Clang_ast_t.dr_decl_pointer)
      |> Option.value_map ~f:get_decl ~default:None
    with
    | Some (ObjCInterfaceDecl (_, _, _, _, otdi)) ->
        otdi.otdi_super
    | _ ->
        Logging.die InternalError
          "Expected that ObjCImplementationDecl always has a pointer to it's interface, but wasn't \
           the case with %s"
          ni.Clang_ast_t.ni_name )
  | ObjCCategoryDecl (_, _, _, _, ocdi) ->
      ocdi.odi_class_interface
  | ObjCCategoryImplDecl (_, _, _, _, ocidi) ->
      ocidi.ocidi_class_interface
  | decl ->
      Logging.die InternalError
        "Expected to be called only with ObjCInterfaceDecl, ObjCImplementationDecl, \
         ObjCCategoryDecl or ObjCCategoryImplDecl, but got %s"
        (Clang_ast_proj.get_decl_kind_string decl)


let get_captured_mode ~lci_capture_this ~lci_capture_kind =
  (* see http://en.cppreference.com/w/cpp/language/lambda *)
  let is_by_ref =
    match lci_capture_kind with
    | `LCK_ByRef (* explicit with [&x] or implicit with [&] *)
    | `LCK_This (* explicit with [this] or implicit with [&] *)
    | `LCK_VLAType
      (* capture a variable-length array by reference. we probably don't handle
         this correctly elsewhere, but it's definitely not captured by value! *) ->
        true
    | `LCK_ByCopy (* explicit with [x] or implicit with [=] *) ->
        (* [=] captures this by reference and everything else by value *)
        lci_capture_this
    | `LCK_StarThis (* [*this] is special syntax for capturing current object by value *) ->
        false
  in
  if is_by_ref then CapturedVar.ByReference else CapturedVar.ByValue
