(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)


(* === Warnings on properties === *)

(* Strong Delegate Warning: a property with name delegate should not be declared strong *)
val checker_strong_delegate_warning : Clang_ast_t.named_decl_info ->
  ObjcProperty_decl.property_type ->
  (bool * CFrontend_utils.General_utils.warning_desc)
