(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Process properties by creating their getters and setters in the case that they need to be syntethized *)
(** or in the case of dynamic. *)
(* How it works: *)
(* - First, the property is defined in the interface. Then, we add the method declarations of the getter *)
(* and setter to the map property_table. *)
(* - Second, in the class implementation, if synthetize is available, create the getters and setters, *)
(* unless some of these methods has already been created before. *)

let is_strong_property obj_c_property_decl_info =
  let attrs = obj_c_property_decl_info.Clang_ast_t.opdi_property_attributes in
  List.exists ~f:(fun a -> match a with
      | `Strong -> true
      | _ -> false) attrs

let is_assign_property obj_c_property_decl_info =
  let attrs = obj_c_property_decl_info.Clang_ast_t.opdi_property_attributes in
  List.exists ~f:(fun a -> match a with
      | `Assign -> true
      | _ -> false) attrs

(* Given a list of declarations in an interface returns list of methods *)
let get_methods curr_class decl_list =
  let class_name = CContext.get_curr_class_name curr_class in
  let get_method decl list_methods =
    match decl with
    | Clang_ast_t.ObjCMethodDecl (_, name_info, method_decl_info) ->
        let is_instance = method_decl_info.Clang_ast_t.omdi_is_instance_method in
        let method_kind = Procname.objc_method_kind_of_bool is_instance in
        let method_name = name_info.Clang_ast_t.ni_name in
        Logging.out_debug "  ...Adding Method '%s' \n" (class_name^"_"^method_name);
        let meth_name =
          CGeneral_utils.mk_procname_from_objc_method class_name method_name method_kind in
        meth_name:: list_methods
    | _ -> list_methods in
  List.fold_right ~f:get_method decl_list ~init:[]
