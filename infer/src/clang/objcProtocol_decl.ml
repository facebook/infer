(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

open Utils
open CFrontend_utils

module L = Logging

let protocol_decl tenv name decl_list =
  (* Adds pairs (protocol name, protocol_type_info) to the global environment. *)
  (* Protocol_type_info contains the methods composing the protocol. *)
  (* Here we are giving a similar treatment as interfaces (see above)*)
  (* It may turn out that we need a more specific treatment for protocols*)
  Printing.log_out "ADDING: ObjCProtocolDecl for '%s'\n" name;
  let mang_name = Mangled.from_string name in
  let curr_class = CContext.ContextProtocol name in
  let protocol_name = Sil.TN_csu(Sil.Protocol, mang_name) in
  let methods = ObjcProperty_decl.get_methods curr_class decl_list in
  let protocol_type_info = Sil.Tstruct([], [], Sil.Protocol, Some mang_name, [], methods, []) in
  Sil.tenv_add tenv protocol_name protocol_type_info;
  curr_class
