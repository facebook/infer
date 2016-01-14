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
val checker_strong_delegate_warning : Clang_ast_t.decl_info -> Clang_ast_t.named_decl_info ->
  Clang_ast_t.obj_c_property_decl_info -> (bool * CFrontend_utils.General_utils.warning_desc)

(* Direct Atomic Property access:
   a property declared atomic should not be accesses directly via its iva *)
val direct_atomic_property_access :
  CContext.t -> Clang_ast_t.stmt_info -> Clang_ast_t.named_decl_info option ->
  (bool * CFrontend_utils.General_utils.warning_desc option)

(* CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK: C++ references
   should not be captured in blocks.  *)
val captured_cxx_ref_in_objc_block : Clang_ast_t.stmt_info -> (Sil.pvar * Sil.typ) list ->
  (bool * CFrontend_utils.General_utils.warning_desc option)
