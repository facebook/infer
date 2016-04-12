(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

type warning_desc = {
  name : string; (* name for the checker, this will be a kind of bug *)
  description : string; (* Description in the error message *)
  suggestion : string; (* an optional suggestion or correction *)
  loc : Location.t; (* location in the code *)
}

(* === Warnings on properties === *)

(* Strong Delegate Warning: a property with name delegate should not be declared strong *)
val strong_delegate_warning : Clang_ast_t.decl_info -> Clang_ast_t.named_decl_info ->
  Clang_ast_t.obj_c_property_decl_info -> warning_desc option

(* Assing Pointer Warning: a property with a pointer type should not be declared `assign` *)
val assign_pointer_warning : Clang_ast_t.decl_info -> Clang_ast_t.named_decl_info ->
  Clang_ast_t.obj_c_property_decl_info -> warning_desc option

(* Direct Atomic Property access:
   a property declared atomic should not be accesses directly via its iva *)
val direct_atomic_property_access_warning :
  CContext.t -> Clang_ast_t.stmt_info -> Clang_ast_t.named_decl_info option ->
  warning_desc option

(* CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK: C++ references
   should not be captured in blocks.  *)
val captured_cxx_ref_in_objc_block_warning : Clang_ast_t.stmt_info -> (Pvar.t * Sil.typ) list ->
  warning_desc option

(* REGISTERED_OBSERVER_BEING_DEALLOCATED: an object is registered in a notification center
   but not removed before deallocation *)
val checker_NSNotificationCenter : Clang_ast_t.decl_info -> Clang_ast_t.decl list -> warning_desc option
