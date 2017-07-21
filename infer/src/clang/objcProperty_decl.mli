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

(* Given a property type returns whether the property is strong *)

val is_strong_property : Clang_ast_t.obj_c_property_decl_info -> bool

(* Returns true if a property has the `assign` attribute *)

val is_assign_property : Clang_ast_t.obj_c_property_decl_info -> bool
