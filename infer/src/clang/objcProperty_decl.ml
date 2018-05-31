(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
  List.exists ~f:(fun a -> match a with `Strong -> true | _ -> false) attrs


let is_weak_property obj_c_property_decl_info =
  let attrs = obj_c_property_decl_info.Clang_ast_t.opdi_property_attributes in
  List.exists ~f:(fun a -> match a with `Weak -> true | _ -> false) attrs


let is_assign_property obj_c_property_decl_info =
  let attrs = obj_c_property_decl_info.Clang_ast_t.opdi_property_attributes in
  List.exists ~f:(fun a -> match a with `Assign -> true | _ -> false) attrs
