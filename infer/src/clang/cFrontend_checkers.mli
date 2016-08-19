(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(* === Warnings on properties === *)

(* Strong Delegate Warning: a property with name delegate should not be declared strong *)
val strong_delegate_warning :
  CLintersContext.context -> Clang_ast_t.decl_info -> Clang_ast_t.named_decl_info ->
  Clang_ast_t.obj_c_property_decl_info -> CIssue.issue_desc option

(* Assing Pointer Warning: a property with a pointer type should not be declared `assign` *)
val assign_pointer_warning :
  CLintersContext.context -> Clang_ast_t.decl_info -> Clang_ast_t.named_decl_info ->
  Clang_ast_t.obj_c_property_decl_info -> CIssue.issue_desc option

(* Direct Atomic Property access:
   a property declared atomic should not be accesses directly via its iva *)
val direct_atomic_property_access_warning :
  CLintersContext.context -> Clang_ast_t.stmt_info -> Clang_ast_t.decl_ref ->
  CIssue.issue_desc option

(* CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK: C++ references
   should not be captured in blocks.  *)
val captured_cxx_ref_in_objc_block_warning :
  CLintersContext.context -> Clang_ast_t.stmt_info ->
  Clang_ast_t.block_captured_variable list -> CIssue.issue_desc option

val bad_pointer_comparison_warning :
  CLintersContext.context ->
  Clang_ast_t.stmt_info -> Clang_ast_t.stmt list -> CIssue.issue_desc option

(* REGISTERED_OBSERVER_BEING_DEALLOCATED: an object is registered in a notification center
   but not removed before deallocation *)
val checker_NSNotificationCenter :
  CLintersContext.context ->
  Clang_ast_t.decl_info -> Clang_ast_t.obj_c_implementation_decl_info option ->
  Clang_ast_t.decl list -> CIssue.issue_desc option

(* GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL warning: a global variable initialization should not *)
(* contain calls to functions or methods as these can be expensive an delay the starting time *)
(* of a program *)
val global_var_init_with_calls_warning :
  CLintersContext.context -> Clang_ast_t.decl -> CIssue.issue_desc option

val location_from_dinfo :
  Clang_ast_t.decl_info -> Location.t
