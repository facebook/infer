(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Process properties by creating their getters and setters in the case that they need to be synthesized *)

(** or in the case of dynamic. *)

(* Given a property type returns whether the property is strong *)

val is_strong_property : Clang_ast_t.obj_c_property_decl_info -> bool

(* Given a property type returns whether the property is weak *)

val is_weak_property : Clang_ast_t.obj_c_property_decl_info -> bool

(* Returns true if a property has the `assign` attribute *)

val is_assign_property : Clang_ast_t.obj_c_property_decl_info -> bool
