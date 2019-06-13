(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let add_protocol_super qual_type_to_sil_type tenv obj_c_protocol_decl_info =
  let protocols = obj_c_protocol_decl_info.Clang_ast_t.opcdi_protocols in
  CAst_utils.add_type_from_decl_ref_list qual_type_to_sil_type tenv protocols


let protocol_decl qual_type_to_sil_type tenv decl =
  let open Clang_ast_t in
  match decl with
  | ObjCProtocolDecl (decl_info, name_info, _, _, obj_c_protocol_decl_info) ->
      let name = CAst_utils.get_qualified_name name_info in
      (* Adds pairs (protocol name, protocol_type_info) to the global environment. *)
      (* Protocol_type_info contains the methods composing the protocol. *)
      (* Here we are giving a similar treatment as interfaces (see above)*)
      (* It may turn out that we need a more specific treatment for protocols*)
      L.(debug Capture Verbose) "ADDING: ObjCProtocolDecl for '%a'@\n" QualifiedCppName.pp name ;
      let protocol_name = Typ.Name.Objc.protocol_from_qual_name name in
      let protocol_desc = Typ.Tstruct protocol_name in
      let decl_key = Clang_ast_extend.DeclPtr decl_info.Clang_ast_t.di_pointer in
      CAst_utils.update_sil_types_map decl_key protocol_desc ;
      ignore (Tenv.mk_struct tenv ~methods:[] protocol_name) ;
      add_protocol_super qual_type_to_sil_type tenv obj_c_protocol_decl_info ;
      protocol_desc
  | _ ->
      assert false
