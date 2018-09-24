(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PolyVariantEqual

(** Functions for transformations of ast nodes *)

module L = Logging

type qual_type_to_sil_type = Tenv.t -> Clang_ast_t.qual_type -> Typ.t

type procname_from_decl =
     ?tenv:Tenv.t
  -> ?block_return_type:Clang_ast_t.qual_type
  -> ?outer_proc:Typ.Procname.t
  -> Clang_ast_t.decl
  -> Typ.Procname.t

let sanitize_name s = Str.global_replace (Str.regexp "[/ ]") "_" s

let get_qual_name qual_name_list =
  List.map ~f:sanitize_name qual_name_list |> QualifiedCppName.of_rev_list


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
        name
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
  let dummy_source_loc = {Clang_ast_t.sl_file= None; sl_line= None; sl_column= None} in
  (dummy_source_loc, dummy_source_loc)


let dummy_stmt_info () =
  {Clang_ast_t.si_pointer= get_fresh_pointer (); si_source_range= dummy_source_range ()}


let get_decl decl_ptr = Int.Table.find ClangPointers.pointer_decl_table decl_ptr

let get_decl_opt decl_ptr_opt =
  match decl_ptr_opt with Some decl_ptr -> get_decl decl_ptr | None -> None


let get_stmt stmt_ptr source_range =
  let stmt = Int.Table.find ClangPointers.pointer_stmt_table stmt_ptr in
  if Option.is_none stmt then
    CFrontend_config.incorrect_assumption __POS__ source_range "stmt with pointer %d not found@\n"
      stmt_ptr ;
  stmt


let get_stmt_opt stmt_ptr_opt source_range =
  match stmt_ptr_opt with Some stmt_ptr -> get_stmt stmt_ptr source_range | None -> None


let get_decl_opt_with_decl_ref decl_ref_opt =
  match decl_ref_opt with
  | Some decl_ref ->
      get_decl decl_ref.Clang_ast_t.dr_decl_pointer
  | None ->
      None


let get_property_of_ivar decl_ptr = Int.Table.find ClangPointers.ivar_to_property_table decl_ptr

let update_sil_types_map type_ptr sil_type =
  CFrontend_config.sil_types_map :=
    Clang_ast_extend.TypePointerMap.add type_ptr sil_type !CFrontend_config.sil_types_map


let update_enum_map enum_constant_pointer sil_exp =
  let predecessor_pointer_opt, _ =
    ClangPointers.Map.find_exn !CFrontend_config.enum_map enum_constant_pointer
  in
  let enum_map_value = (predecessor_pointer_opt, Some sil_exp) in
  CFrontend_config.enum_map :=
    ClangPointers.Map.set !CFrontend_config.enum_map ~key:enum_constant_pointer
      ~data:enum_map_value


let add_enum_constant enum_constant_pointer predecessor_pointer_opt =
  let enum_map_value = (predecessor_pointer_opt, None) in
  CFrontend_config.enum_map :=
    ClangPointers.Map.set !CFrontend_config.enum_map ~key:enum_constant_pointer
      ~data:enum_map_value


let get_enum_constant_exp enum_constant_pointer =
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
  let default_visibility = true in
  let mk_annot annot_name_opt =
    match annot_name_opt with
    | Some annot_name ->
        [({Annot.class_name= annot_name; parameters= []}, default_visibility)]
    | None ->
        Annot.Item.empty
  in
  let annot_name_opt =
    match get_type qt_type_ptr with
    | Some (AttributedType (_, attr_info)) ->
        if attr_info.ati_attr_kind = `Nullable then Some Annotations.nullable
        else if attr_info.ati_attr_kind = `Nonnull then Some Annotations.nonnull
          (* other annotations go here *)
        else None
    | _ ->
        None
  in
  mk_annot annot_name_opt


let name_of_typedef_type_info {Clang_ast_t.tti_decl_ptr} =
  match get_decl tti_decl_ptr with
  | Some (TypedefDecl (_, name_decl_info, _, _)) ->
      get_qualified_name name_decl_info
  | _ ->
      QualifiedCppName.empty


let name_opt_of_typedef_qual_type qual_type =
  match get_type qual_type.Clang_ast_t.qt_type_ptr with
  | Some (Clang_ast_t.TypedefType (_, typedef_type_info)) ->
      Some (name_of_typedef_type_info typedef_type_info)
  | _ ->
      None


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
      if fail_if_not_found then assert false else ()


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
  if [%compare.equal: int option] decl_ptr' (Some decl_ptr) then decl_opt
  else get_decl_opt decl_ptr'


let get_info_from_decl_ref decl_ref =
  let name_info = match decl_ref.Clang_ast_t.dr_name with Some ni -> ni | _ -> assert false in
  let decl_ptr = decl_ref.Clang_ast_t.dr_decl_pointer in
  let qual_type =
    match decl_ref.Clang_ast_t.dr_qual_type with Some tp -> tp | _ -> assert false
  in
  (name_info, decl_ptr, qual_type)


(* st |= EF (atomic_pred param) *)
let rec exists_eventually_st atomic_pred param st =
  if atomic_pred param st then true
  else
    let _, st_list = Clang_ast_proj.get_stmt_tuple st in
    List.exists ~f:(exists_eventually_st atomic_pred param) st_list


let is_syntactically_global_var decl =
  match decl with
  | Clang_ast_t.VarDecl (_, _, _, vdi) ->
      vdi.vdi_is_global && not vdi.vdi_is_static_local
  | _ ->
      false


let is_static_local_var decl =
  match decl with Clang_ast_t.VarDecl (_, _, _, vdi) -> vdi.vdi_is_static_local | _ -> false


let is_const_expr_var decl =
  match decl with Clang_ast_t.VarDecl (_, _, _, vdi) -> vdi.vdi_is_const_expr | _ -> false


let full_name_of_decl_opt decl_opt =
  match decl_opt with
  | Some decl -> (
    match Clang_ast_proj.get_named_decl_tuple decl with
    | Some (_, name_info) ->
        get_qualified_name name_info
    | None ->
        QualifiedCppName.empty )
  | None ->
      QualifiedCppName.empty


(* Generates a unique number for each variant of a type. *)
let get_tag ast_item =
  let item_rep = Obj.repr ast_item in
  if Obj.is_block item_rep then Obj.tag item_rep else -Obj.obj item_rep


(* Generates a key for a statement based on its sub-statements and the statement tag. *)
let generate_key_stmt stmt =
  let buffer = Buffer.create 16 in
  let rec add_stmt stmt =
    Buffer.add_string buffer (string_of_int (get_tag stmt)) ;
    let _, stmts = Clang_ast_proj.get_stmt_tuple stmt in
    List.iter ~f:add_stmt stmts
  in
  add_stmt stmt ; Buffer.contents buffer


(* Generates a key for a declaration based on its name and the declaration tag. *)
let generate_key_decl decl =
  let name = full_name_of_decl_opt (Some decl) in
  Format.sprintf "%d%s" (get_tag decl) (QualifiedCppName.to_qual_string name)


let rec get_super_if decl =
  match decl with
  | Some (Clang_ast_t.ObjCImplementationDecl (_, _, _, _, impl_decl_info)) ->
      (* Try getting the super ref through the impl info, and fall back to
         getting the if decl first and getting the super ref through it. *)
      let super_ref = get_decl_opt_with_decl_ref impl_decl_info.oidi_super in
      if Option.is_some super_ref then super_ref
      else get_super_if (get_decl_opt_with_decl_ref impl_decl_info.oidi_class_interface)
  | Some (Clang_ast_t.ObjCInterfaceDecl (_, _, _, _, interface_decl_info)) ->
      get_decl_opt_with_decl_ref interface_decl_info.otdi_super
  | _ ->
      None


let get_super_ObjCImplementationDecl impl_decl_info =
  let objc_interface_decl_current =
    get_decl_opt_with_decl_ref impl_decl_info.Clang_ast_t.oidi_class_interface
  in
  let objc_interface_decl_super = get_super_if objc_interface_decl_current in
  let objc_implementation_decl_super =
    match objc_interface_decl_super with
    | Some (ObjCInterfaceDecl (_, _, _, _, interface_decl_info)) ->
        get_decl_opt_with_decl_ref interface_decl_info.otdi_implementation
    | _ ->
        None
  in
  objc_implementation_decl_super


let get_impl_decl_info dec =
  match dec with Clang_ast_t.ObjCImplementationDecl (_, _, _, _, idi) -> Some idi | _ -> None


let default_blacklist = CFrontend_config.[nsobject_cl; nsproxy_cl]

let rec is_objc_if_descendant ?(blacklist = default_blacklist) if_decl ancestors =
  (* List of ancestors to check for and list of classes to short-circuit to
     false can't intersect *)
  if not String.Set.(is_empty (inter (of_list blacklist) (of_list ancestors))) then
    L.(die InternalError) "Blacklist and ancestors must be mutually exclusive."
  else
    match if_decl with
    | Some (Clang_ast_t.ObjCInterfaceDecl (_, ndi, _, _, _)) ->
        let in_list some_list = List.mem ~equal:String.equal some_list ndi.Clang_ast_t.ni_name in
        (not (in_list blacklist))
        && (in_list ancestors || is_objc_if_descendant ~blacklist (get_super_if if_decl) ancestors)
    | _ ->
        false


let rec qual_type_to_objc_interface qual_type =
  let typ_opt = get_desugared_type qual_type.Clang_ast_t.qt_type_ptr in
  ctype_to_objc_interface typ_opt


and ctype_to_objc_interface typ_opt =
  match (typ_opt : Clang_ast_t.c_type option) with
  | Some (ObjCInterfaceType (_, decl_ptr)) ->
      get_decl decl_ptr
  | Some (ObjCObjectPointerType (_, (inner_qual_type : Clang_ast_t.qual_type))) ->
      qual_type_to_objc_interface inner_qual_type
  | Some (FunctionProtoType (_, function_type_info, _))
  | Some (FunctionNoProtoType (_, function_type_info)) ->
      qual_type_to_objc_interface function_type_info.Clang_ast_t.fti_return_type
  | _ ->
      None


let if_decl_to_di_pointer_opt if_decl =
  match if_decl with
  | Some (Clang_ast_t.ObjCInterfaceDecl (if_decl_info, _, _, _, _)) ->
      Some if_decl_info.di_pointer
  | _ ->
      None


let is_instance_type qual_type =
  match name_opt_of_typedef_qual_type qual_type with
  | Some name ->
      String.equal (QualifiedCppName.to_qual_string name) "instancetype"
  | None ->
      false


let return_type_matches_class_type result_type interface_decl =
  if is_instance_type result_type then true
  else
    let return_type_decl_opt = qual_type_to_objc_interface result_type in
    [%compare.equal: int option]
      (if_decl_to_di_pointer_opt interface_decl)
      (if_decl_to_di_pointer_opt return_type_decl_opt)


let is_objc_factory_method ~class_decl:interface_decl ~method_decl:meth_decl_opt =
  let open Clang_ast_t in
  match meth_decl_opt with
  | Some (ObjCMethodDecl (_, _, omdi)) ->
      (not omdi.omdi_is_instance_method)
      && return_type_matches_class_type omdi.omdi_result_type interface_decl
  | _ ->
      false


let name_of_decl_ref_opt (decl_ref_opt : Clang_ast_t.decl_ref option) =
  match decl_ref_opt with
  | Some decl_ref -> (
    match decl_ref.dr_name with Some named_decl_info -> Some named_decl_info.ni_name | _ -> None )
  | _ ->
      None


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
  | ClassTemplateSpecializationDecl (_, _, type_ptr, _, _, _, _, _, _, _)
  | ClassTemplatePartialSpecializationDecl (_, _, type_ptr, _, _, _, _, _, _, _)
  | TemplateTypeParmDecl (_, _, type_ptr)
  | ObjCTypeParamDecl (_, _, type_ptr)
  | TypeAliasDecl (_, _, type_ptr)
  | TypedefDecl (_, _, type_ptr, _)
  | UnresolvedUsingTypenameDecl (_, _, type_ptr) ->
      Some type_ptr
  | BindingDecl (_, _, qual_type)
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
  | DecompositionDecl (_, _, qual_type, _)
  | ImplicitParamDecl (_, _, qual_type, _)
  | OMPCapturedExprDecl (_, _, qual_type, _)
  | ParmVarDecl (_, _, qual_type, _)
  | VarTemplateSpecializationDecl (_, _, qual_type, _)
  | VarTemplatePartialSpecializationDecl (_, _, qual_type, _)
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
  | ClassTemplateSpecializationDecl (_, _, _, decl_list, _, _, _, _, _, _)
  | CXXRecordDecl (_, _, _, decl_list, _, _, _, _)
  | RecordDecl (_, _, _, decl_list, _, _, _) ->
      List.filter ~f:(function FieldDecl _ -> true | _ -> false) decl_list
  | _ ->
      []


let get_cxx_base_classes decl =
  let open Clang_ast_t in
  match decl with
  | CXXRecordDecl (_, _, _, _, _, _, _, cxx_record_info)
  | ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, cxx_record_info, _, _) ->
      cxx_record_info.xrdi_bases
  | _ ->
      []


let get_cxx_virtual_base_classes decl =
  let open Clang_ast_t in
  match decl with
  | CXXRecordDecl (_, _, _, _, _, _, _, cxx_record_info)
  | ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, cxx_record_info, _, _) ->
      cxx_record_info.xrdi_transitive_vbases
  | _ ->
      []


let is_std_vector qt =
  match get_decl_from_typ_ptr qt.Clang_ast_t.qt_type_ptr with
  | Some decl -> (
    match Clang_ast_proj.get_named_decl_tuple decl with
    | Some (_, qual_name) ->
        String.equal qual_name.ni_name "vector"
        && List.mem ~equal:String.equal qual_name.ni_qual_name "std"
    | None ->
        false )
  | None ->
      false


let has_block_attribute decl =
  let open Clang_ast_t in
  match decl with
  | VarDecl (decl_info, _, _, _) ->
      let attributes = decl_info.di_attributes in
      List.exists ~f:(fun attr -> match attr with BlocksAttr _ -> true | _ -> false) attributes
  | _ ->
      false


let is_implicit_decl decl =
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
          "Expected that ObjCImplementationDecl always has a pointer to it's interface, but \
           wasn't the case with %s"
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
