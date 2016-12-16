(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(* === Warnings on properties === *)

(* Strong Delegate Warning: a property with name delegate should not be declared strong *)
val ctl_strong_delegate_warning :
  CLintersContext.context -> CTL.ast_node -> CTL.t * CIssue.issue_desc option

(* Assing Pointer Warning: a property with a pointer type should not be declared `assign` *)
val ctl_assign_pointer_warning :
  CLintersContext.context -> CTL.ast_node -> CTL.t * CIssue.issue_desc option

(* Direct Atomic Property access:
   a property declared atomic should not be accesses directly via its iva *)
val ctl_direct_atomic_property_access_warning :
  CLintersContext.context -> CTL.ast_node -> CTL.t * CIssue.issue_desc option

(* CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK: C++ references
   should not be captured in blocks.  *)
val ctl_captured_cxx_ref_in_objc_block_warning :
  CLintersContext.context -> CTL.ast_node -> CTL.t * CIssue.issue_desc option

(** Unavailable_api_in_supported_os_error :
    If the declaration has avilability attributes, check that it's compatible with
    the iphoneos_target_sdk_version *)
val ctl_unavailable_api_in_supported_ios_sdk_error :
  CLintersContext.context -> CTL.ast_node -> CTL.t * CIssue.issue_desc option

val ctl_bad_pointer_comparison_warning :
  CLintersContext.context -> CTL.ast_node -> CTL.t * CIssue.issue_desc option

(* REGISTERED_OBSERVER_BEING_DEALLOCATED: an object is registered in a notification center
   but not removed before deallocation *)
val ctl_ns_notification_warning :
  CLintersContext.context -> CTL.ast_node -> CTL.t * CIssue.issue_desc option

(* GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL warning: a global variable initialization should not *)
(* contain calls to functions or methods as these can be expensive an delay the starting time *)
(* of a program *)
val ctl_global_var_init_with_calls_warning :
  CLintersContext.context -> CTL.ast_node -> CTL.t * CIssue.issue_desc option

val location_from_stmt :
  CLintersContext.context -> Clang_ast_t.stmt -> Location.t

val location_from_dinfo :
  CLintersContext.context -> Clang_ast_t.decl_info -> Location.t

val location_from_decl :
  CLintersContext.context -> Clang_ast_t.decl -> Location.t

val decl_name : CTL.ast_node -> string

val ivar_name : CTL.ast_node -> string

val var_name : CTL.ast_node -> string

val decl_ref_or_selector_name : CTL.ast_node -> string

val iphoneos_target_sdk_version : CTL.ast_node -> string

val available_ios_sdk : CTL.ast_node -> string
