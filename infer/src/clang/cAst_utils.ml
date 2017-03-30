(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant

(** Functions for transformations of ast nodes *)

module L = Logging
module F = Format

type type_ptr_to_sil_type = Tenv.t -> Clang_ast_t.type_ptr -> Typ.t

let sanitize_name = Str.global_replace (Str.regexp "[/ ]") "_"
let get_qual_name qual_name_list =
  List.map ~f:sanitize_name qual_name_list |> QualifiedCppName.of_rev_list

let get_qualified_name name_info =
  get_qual_name name_info.Clang_ast_t.ni_qual_name

let get_unqualified_name name_info =
  let name = match name_info.Clang_ast_t.ni_qual_name with
    | name :: _ -> name
    | [] -> name_info.Clang_ast_t.ni_name in
  sanitize_name name

let get_class_name_from_member member_name_info =
  match member_name_info.Clang_ast_t.ni_qual_name with
  | _ :: class_qual_list -> get_qual_name class_qual_list
  | [] -> assert false

let make_name_decl name = {
  Clang_ast_t.ni_name = name;
  ni_qual_name = [name];
}

let make_qual_name_decl class_name_quals name = {
  Clang_ast_t.ni_name = name;
  ni_qual_name = name :: class_name_quals;
}

let pointer_counter = ref 0

let get_fresh_pointer () =
  pointer_counter := !pointer_counter + 1;
  let internal_pointer = -(!pointer_counter) in
  internal_pointer

let get_invalid_pointer () =
  CFrontend_config.invalid_pointer

let type_from_unary_expr_or_type_trait_expr_info info =
  match info.Clang_ast_t.uttei_type_ptr with
  | Some tp -> Some tp
  | None -> None

let get_decl decl_ptr =
  try
    Some (Clang_ast_main.PointerMap.find decl_ptr !CFrontend_config.pointer_decl_index)
  with Not_found -> Logging.out "decl with pointer %d not found\n" decl_ptr; None

let get_decl_opt decl_ptr_opt =
  match decl_ptr_opt with
  | Some decl_ptr -> get_decl decl_ptr
  | None -> None

let get_stmt stmt_ptr =
  try
    Some (Clang_ast_main.PointerMap.find stmt_ptr !CFrontend_config.pointer_stmt_index)
  with Not_found -> Logging.out "stmt with pointer %d not found\n" stmt_ptr; None

let get_stmt_opt stmt_ptr_opt =
  match stmt_ptr_opt with
  | Some stmt_ptr -> get_stmt stmt_ptr
  | None -> None

let get_decl_opt_with_decl_ref decl_ref_opt =
  match decl_ref_opt with
  | Some decl_ref -> get_decl decl_ref.Clang_ast_t.dr_decl_pointer
  | None -> None

let get_property_of_ivar decl_ptr =
  try
    Some (Clang_ast_main.PointerMap.find decl_ptr !CFrontend_config.ivar_to_property_index)
  with Not_found -> Logging.out "property with pointer %d not found\n" decl_ptr; None

let update_sil_types_map type_ptr sil_type =
  CFrontend_config.sil_types_map :=
    Clang_ast_types.TypePointerMap.add type_ptr sil_type !CFrontend_config.sil_types_map

let update_enum_map enum_constant_pointer sil_exp =
  let open Clang_ast_main in
  let (predecessor_pointer_opt, _) =
    try PointerMap.find enum_constant_pointer !CFrontend_config.enum_map
    with Not_found -> assert false in
  let enum_map_value = (predecessor_pointer_opt, Some sil_exp) in
  CFrontend_config.enum_map :=
    PointerMap.add enum_constant_pointer enum_map_value !CFrontend_config.enum_map

let add_enum_constant enum_constant_pointer predecessor_pointer_opt =
  let enum_map_value = (predecessor_pointer_opt, None) in
  CFrontend_config.enum_map :=
    Clang_ast_main.PointerMap.add enum_constant_pointer enum_map_value !CFrontend_config.enum_map

let get_enum_constant_exp enum_constant_pointer =
  Clang_ast_main.PointerMap.find enum_constant_pointer !CFrontend_config.enum_map

let get_type type_ptr =
  try
    (* There is chance for success only if type_ptr is in fact clang pointer *)
    (let raw_ptr = Clang_ast_types.type_ptr_to_clang_pointer type_ptr in
     try
       Some (Clang_ast_main.PointerMap.find raw_ptr !CFrontend_config.pointer_type_index)
     with Not_found -> Logging.out "type with pointer %d not found\n" raw_ptr; None)
  with Clang_ast_types.Not_Clang_Pointer ->
    (* otherwise, function fails *)
    let type_str = Clang_ast_types.type_ptr_to_string type_ptr in
    Logging.out "type %s is not clang pointer\n" type_str;
    None

let get_desugared_type type_ptr =
  let typ_opt = get_type type_ptr in
  match typ_opt with
  | Some typ ->
      let type_info = Clang_ast_proj.get_type_tuple typ in
      (match type_info.Clang_ast_t.ti_desugared_type with
       | Some ptr -> get_type ptr
       | _ -> typ_opt)
  | _ -> typ_opt

let get_decl_from_typ_ptr typ_ptr =
  let typ_opt = get_desugared_type typ_ptr in
  let typ = match typ_opt with Some t -> t | _ -> assert false in
  match typ with
  | Clang_ast_t.RecordType (_, decl_ptr)
  | Clang_ast_t.ObjCInterfaceType (_, decl_ptr) -> get_decl decl_ptr
  | _ -> None

(* TODO take the attributes into account too. To be done after we get the attribute's arguments. *)
let is_type_nonnull type_ptr =
  let open Clang_ast_t in
  match get_type type_ptr with
  | Some AttributedType (_, attr_info) ->
      attr_info.ati_attr_kind = `Nonnull
  | _ ->
      false

let sil_annot_of_type type_ptr =
  let default_visibility = true in
  let mk_annot annot_name_opt =
    match annot_name_opt with
    | Some annot_name ->
        [{ Annot.class_name = annot_name; parameters = []; }, default_visibility]
    | None -> Annot.Item.empty in
  let annot_name_opt =
    match get_type type_ptr with
    | Some AttributedType (_, attr_info) ->
        if attr_info.ati_attr_kind = `Nullable then Some Annotations.nullable
        else if attr_info.ati_attr_kind = `Nonnull then Some Annotations.nonnull
        (* other annotations go here *)
        else None
    | _ -> None in
  mk_annot annot_name_opt

let string_of_type_ptr type_ptr = Clang_ast_j.string_of_type_ptr type_ptr

let name_of_typedef_type_info {Clang_ast_t.tti_decl_ptr} =
  match get_decl tti_decl_ptr with
  | Some TypedefDecl (_, name_decl_info, _, _, _) ->
      get_qualified_name name_decl_info
  | _ -> QualifiedCppName.empty

let name_opt_of_typedef_type_ptr type_ptr =
  match get_type type_ptr with
  | Some Clang_ast_t.TypedefType (_, typedef_type_info) ->
      Some (name_of_typedef_type_info typedef_type_info)
  | _ -> None

let string_of_qual_type {Clang_ast_t.qt_type_ptr; qt_is_const} =
  Printf.sprintf "%s%s" (if qt_is_const then "is_const " else "") (string_of_type_ptr qt_type_ptr)

let add_type_from_decl_ref type_ptr_to_sil_type tenv decl_ref_opt fail_if_not_found =
  match decl_ref_opt with (* translate interface first if found *)
  | Some dr ->
      ignore (type_ptr_to_sil_type tenv (`DeclPtr dr.Clang_ast_t.dr_decl_pointer));
  | _ -> if fail_if_not_found then assert false else ()

let add_type_from_decl_ref_list type_ptr_to_sil_type tenv decl_ref_list =
  let add_elem dr =
    ignore (type_ptr_to_sil_type tenv (`DeclPtr dr.Clang_ast_t.dr_decl_pointer)) in
  List.iter ~f:add_elem decl_ref_list

let get_function_decl_with_body decl_ptr =
  let open Clang_ast_t in
  let decl_opt = get_decl decl_ptr in
  let decl_ptr' = match decl_opt with
    | Some (FunctionDecl (_, _, _, fdecl_info))
    | Some (CXXMethodDecl (_, _, _, fdecl_info, _))
    | Some (CXXConstructorDecl (_, _, _, fdecl_info, _))
    | Some (CXXConversionDecl (_, _, _, fdecl_info, _))
    | Some (CXXDestructorDecl (_, _, _, fdecl_info, _)) ->
        fdecl_info.Clang_ast_t.fdi_decl_ptr_with_body
    | _ -> Some decl_ptr in
  if [%compare.equal : int option] decl_ptr' (Some decl_ptr) then decl_opt
  else get_decl_opt decl_ptr'

let get_info_from_decl_ref decl_ref =
  let name_info = match decl_ref.Clang_ast_t.dr_name with Some ni -> ni | _ -> assert false in
  let decl_ptr = decl_ref.Clang_ast_t.dr_decl_pointer in
  let type_ptr = match decl_ref.Clang_ast_t.dr_type_ptr with Some tp -> tp | _ -> assert false in
  name_info, decl_ptr, type_ptr

(* st |= EF (atomic_pred param) *)
let rec exists_eventually_st atomic_pred param  st =
  if atomic_pred param st then true
  else
    let _, st_list = Clang_ast_proj.get_stmt_tuple st in
    List.exists ~f:(exists_eventually_st atomic_pred param) st_list

let is_syntactically_global_var decl =
  match decl with
  | Clang_ast_t.VarDecl (_, _ ,_, vdi) ->
      vdi.vdi_is_global && not vdi.vdi_is_static_local
  | _ -> false

let is_const_expr_var decl =
  match decl with
  | Clang_ast_t.VarDecl (_, _ ,_, vdi) -> vdi.vdi_is_const_expr
  | _ -> false

let is_ptr_to_objc_class typ class_name =
  match typ with
  | Some Clang_ast_t.ObjCObjectPointerType (_, {Clang_ast_t.qt_type_ptr}) ->
      (match get_desugared_type qt_type_ptr with
       | Some ObjCInterfaceType (_, ptr) ->
           (match get_decl ptr with
            | Some ObjCInterfaceDecl (_, ndi, _, _, _) ->
                String.equal ndi.ni_name class_name
            | _ -> false)
       | _ -> false)
  | _ -> false

let full_name_of_decl_opt decl_opt =
  match decl_opt with
  | Some decl ->
      (match Clang_ast_proj.get_named_decl_tuple decl with
       | Some (_, name_info) -> get_qualified_name name_info
       | None -> QualifiedCppName.empty)
  | None -> QualifiedCppName.empty

(* Generates a unique number for each variant of a type. *)
let get_tag ast_item =
  let item_rep = Obj.repr ast_item in
  if Obj.is_block item_rep then
    Obj.tag item_rep
  else -(Obj.obj item_rep)

(* Generates a key for a statement based on its sub-statements and the statement tag. *)
let rec generate_key_stmt stmt =
  let tag_str = string_of_int (get_tag stmt) in
  let _, stmts = Clang_ast_proj.get_stmt_tuple stmt in
  let tags = List.map ~f:generate_key_stmt stmts in
  let buffer = Buffer.create 16 in
  let tags = tag_str :: tags in
  List.iter ~f:(fun tag -> Buffer.add_string buffer tag) tags;
  Buffer.contents buffer

(* Generates a key for a declaration based on its name and the declaration tag. *)
let generate_key_decl decl =
  let buffer = Buffer.create 16 in
  let name = full_name_of_decl_opt (Some decl) in
  Buffer.add_string buffer (string_of_int (get_tag decl));
  Buffer.add_string buffer (QualifiedCppName.to_qual_string name);
  Buffer.contents buffer

let rec get_super_if decl =
  match decl with
  | Some Clang_ast_t.ObjCImplementationDecl(_, _, _, _, impl_decl_info) ->
      (* Try getting the super ref through the impl info, and fall back to
         getting the if decl first and getting the super ref through it. *)
      let super_ref = get_decl_opt_with_decl_ref impl_decl_info.oidi_super in
      if Option.is_some super_ref then
        super_ref
      else
        get_super_if (get_decl_opt_with_decl_ref impl_decl_info.oidi_class_interface)
  | Some Clang_ast_t.ObjCInterfaceDecl(_, _, _, _, interface_decl_info) ->
      get_decl_opt_with_decl_ref interface_decl_info.otdi_super
  | _ -> None

let get_super_impl impl_decl_info =
  let objc_interface_decl_current =
    get_decl_opt_with_decl_ref
      impl_decl_info.Clang_ast_t.oidi_class_interface in
  let objc_interface_decl_super = get_super_if objc_interface_decl_current in
  let objc_implementation_decl_super =
    match objc_interface_decl_super with
    | Some ObjCInterfaceDecl(_, _, _, _, interface_decl_info) ->
        get_decl_opt_with_decl_ref
          interface_decl_info.otdi_implementation
    | _ -> None in
  match objc_implementation_decl_super with
  | Some ObjCImplementationDecl(_, _, decl_list, _, impl_decl_info) ->
      Some (decl_list, impl_decl_info)
  | _ -> None

let get_super_ObjCImplementationDecl impl_decl_info =
  let objc_interface_decl_current =
    get_decl_opt_with_decl_ref
      impl_decl_info.Clang_ast_t.oidi_class_interface in
  let objc_interface_decl_super = get_super_if objc_interface_decl_current in
  let objc_implementation_decl_super =
    match objc_interface_decl_super with
    | Some ObjCInterfaceDecl(_, _, _, _, interface_decl_info) ->
        get_decl_opt_with_decl_ref
          interface_decl_info.otdi_implementation
    | _ -> None in
  objc_implementation_decl_super

let get_impl_decl_info dec =
  match dec with
  | Clang_ast_t.ObjCImplementationDecl (_, _, _, _, idi) -> Some idi
  | _ -> None

let default_blacklist =
  let open CFrontend_config in
  [nsobject_cl; nsproxy_cl]

let rec is_objc_if_descendant ?(blacklist = default_blacklist) if_decl ancestors =
  (* List of ancestors to check for and list of classes to short-circuit to
     false can't intersect *)
  if not String.Set.(is_empty (inter (of_list blacklist) (of_list ancestors))) then
    failwith "Blacklist and ancestors must be mutually exclusive."
  else
    match if_decl with
    | Some Clang_ast_t.ObjCInterfaceDecl (_, ndi, _, _, _) ->
        let in_list some_list =
          List.mem ~equal:String.equal some_list ndi.Clang_ast_t.ni_name in
        not (in_list blacklist)
        && (in_list ancestors
            || is_objc_if_descendant ~blacklist:blacklist (get_super_if if_decl) ancestors)
    | _ -> false

let rec type_ptr_to_objc_interface type_ptr =
  let typ_opt = get_desugared_type type_ptr in
  ctype_to_objc_interface typ_opt
and ctype_to_objc_interface typ_opt =
  match (typ_opt : Clang_ast_t.c_type option) with
  | Some ObjCInterfaceType (_, decl_ptr) -> get_decl decl_ptr
  | Some ObjCObjectPointerType (_, (inner_qual_type: Clang_ast_t.qual_type)) ->
      type_ptr_to_objc_interface inner_qual_type.qt_type_ptr
  | Some FunctionProtoType (_, function_type_info, _)
  | Some FunctionNoProtoType (_, function_type_info) ->
      type_ptr_to_objc_interface function_type_info.Clang_ast_t.fti_return_type
  | _ -> None

let type_ptr_is_typedef_named type_ptr (type_name: string): bool =
  let is_decl_name_match decl_opt =
    let tuple_opt = match decl_opt with
      | Some decl -> Clang_ast_proj.get_named_decl_tuple decl
      | _ -> None in
    match tuple_opt with
    | Some (_, ni) ->
        String.equal type_name ni.ni_name
    | _ -> false in
  match get_type type_ptr with
  | Some TypedefType (_, tti) ->
      let decl_opt = get_decl tti.tti_decl_ptr in
      is_decl_name_match decl_opt
  | _ -> false

let if_decl_to_di_pointer_opt if_decl =
  match if_decl with
  | Clang_ast_t.ObjCInterfaceDecl (if_decl_info, _, _, _, _) ->
      Some if_decl_info.di_pointer
  | _ -> None

let is_instance_type type_ptr =
  match name_opt_of_typedef_type_ptr type_ptr with
  | Some name -> String.equal (QualifiedCppName.to_qual_string name) "instancetype"
  | None -> false

let return_type_matches_class_type rtp type_decl_pointer =
  if is_instance_type rtp then
    true
  else
    let return_type_decl_opt = type_ptr_to_objc_interface rtp in
    let return_type_decl_pointer_opt =
      Option.map ~f:if_decl_to_di_pointer_opt return_type_decl_opt in
    [%compare.equal : int option option] (Some type_decl_pointer) return_type_decl_pointer_opt

let is_objc_factory_method if_decl meth_decl =
  let if_type_decl_pointer = if_decl_to_di_pointer_opt if_decl in
  match meth_decl with
  | Clang_ast_t.ObjCMethodDecl (_, _, omdi) ->
      (not omdi.omdi_is_instance_method) &&
      (return_type_matches_class_type omdi.omdi_result_type if_type_decl_pointer)
  | _ -> false

let name_of_decl_ref_opt (decl_ref_opt: Clang_ast_t.decl_ref option) =
  match decl_ref_opt with
  | Some decl_ref ->
      (match decl_ref.dr_name with
       | Some named_decl_info -> Some named_decl_info.ni_name
       | _ -> None)
  | _ -> None
