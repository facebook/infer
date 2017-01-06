(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val ctl_unavailable_api_in_supported_ios_sdk_error :
  CLintersContext.context -> CTL.ast_node -> CTL.t * CIssue.issue_desc option

val location_from_stmt :
  CLintersContext.context -> Clang_ast_t.stmt -> Location.t

val location_from_dinfo :
  CLintersContext.context -> Clang_ast_t.decl_info -> Location.t

val location_from_an :
  CLintersContext.context -> CTL.ast_node -> Location.t

val location_from_decl :
  CLintersContext.context -> Clang_ast_t.decl -> Location.t

val decl_name : CTL.ast_node -> string

val ivar_name : CTL.ast_node -> string

val cxx_ref_captured_in_block : CTL.ast_node -> string

val decl_ref_or_selector_name : CTL.ast_node -> string

val iphoneos_target_sdk_version : CTL.ast_node -> string

val available_ios_sdk : CTL.ast_node -> string

val tag_name_of_node : CTL.ast_node -> string
