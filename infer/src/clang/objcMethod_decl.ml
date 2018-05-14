(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
(* Given a list of declarations in an interface returns list of methods *)

open! IStd

let get_methods class_typename decl_list =
  let open Clang_ast_t in
  let get_method list_methods decl =
    match decl with
    | Clang_ast_t.ObjCMethodDecl (_, ndi, mdi) ->
        let method_kind =
          Typ.Procname.ObjC_Cpp.objc_method_kind_of_bool mdi.omdi_is_instance_method
        in
        let method_name =
          Typ.Procname.ObjC_Cpp
            (Typ.Procname.ObjC_Cpp.make class_typename ndi.ni_name method_kind Typ.NoTemplate)
        in
        method_name :: list_methods
    | _ ->
        list_methods
  in
  List.fold_left ~f:get_method decl_list ~init:[]


let add_missing_methods tenv class_tn_name missing_methods =
  match Tenv.lookup tenv class_tn_name with
  | Some ({methods} as struct_typ) ->
      let new_methods = CGeneral_utils.append_no_duplicates_methods methods missing_methods in
      ignore (Tenv.mk_struct tenv ~default:struct_typ ~methods:new_methods class_tn_name)
  | _ ->
      ()
