(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Translate an enumeration declaration by adding it to the tenv and *)
(** translating the code and adding it to a fake procdesc *)

(*Check if the constant is in the map, in which case that means that all the *)
(* contants of this enum are in the map, by invariant. Otherwise, add the constant *)
(* to the map. *)
let add_enum_constant_to_map_if_needed decl_pointer pred_decl_opt =
  try
    ignore (CAst_utils.get_enum_constant_exp decl_pointer);
    true
  with Not_found ->
    CAst_utils.add_enum_constant decl_pointer pred_decl_opt;
    false

(* Add the constants of this enum to the map if they are not in the map yet *)
let enum_decl decl =
  let open Clang_ast_t in
  let get_constant_decl_ptr decl =
    match decl with
    |  EnumConstantDecl (decl_info, _, _, _) -> decl_info.di_pointer
    | _ -> assert false in
  let rec add_enum_constants_to_map decl_list =
    match decl_list with
    | decl :: pred_decl :: rest ->
        let decl_pointer = get_constant_decl_ptr decl in
        let pred_decl_pointer = get_constant_decl_ptr pred_decl in
        if not (add_enum_constant_to_map_if_needed decl_pointer (Some pred_decl_pointer)) then
          add_enum_constants_to_map (pred_decl::rest)
    | [decl] ->
        let decl_pointer = get_constant_decl_ptr decl in
        ignore (add_enum_constant_to_map_if_needed decl_pointer None)
    | _ -> () in
  match decl with
  | EnumDecl (_, _, _, type_ptr, decl_list, _, _) ->
      add_enum_constants_to_map (List.rev decl_list);
      let sil_type = Typ.Tint Typ.IInt in
      CAst_utils.update_sil_types_map type_ptr sil_type;
      sil_type

  | _ -> assert false
