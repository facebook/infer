(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module L = Logging

let add_protocol_super type_ptr_to_sil_type tenv obj_c_protocol_decl_info =
  let protocols = obj_c_protocol_decl_info.Clang_ast_t.opcdi_protocols in
  CAst_utils.add_type_from_decl_ref_list type_ptr_to_sil_type tenv protocols

let protocol_decl type_ptr_to_sil_type tenv decl =
  let open Clang_ast_t in
  match decl with
  | ObjCProtocolDecl(decl_info, name_info, _, _, obj_c_protocol_decl_info) ->
      let name = CAst_utils.get_qualified_name name_info in
      (* Adds pairs (protocol name, protocol_type_info) to the global environment. *)
      (* Protocol_type_info contains the methods composing the protocol. *)
      (* Here we are giving a similar treatment as interfaces (see above)*)
      (* It may turn out that we need a more specific treatment for protocols*)
      Logging.out_debug "ADDING: ObjCProtocolDecl for '%a'\n" QualifiedCppName.pp name;
      let protocol_name = Typ.Name.Objc.protocol_from_qual_name name in
      let decl_key = `DeclPtr decl_info.Clang_ast_t.di_pointer in
      CAst_utils.update_sil_types_map decl_key (Typ.Tstruct protocol_name);
      ignore( Tenv.mk_struct tenv ~methods:[] protocol_name );
      add_protocol_super type_ptr_to_sil_type tenv obj_c_protocol_decl_info;
      Typ.Tstruct protocol_name
  | _ -> assert false

let is_protocol decl =
  let open Clang_ast_t in
  match decl with
  | ObjCProtocolDecl _ -> true
  | _ -> false
