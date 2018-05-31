(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val location_from_stmt : CLintersContext.context -> Clang_ast_t.stmt -> Location.t

val location_from_dinfo : CLintersContext.context -> Clang_ast_t.decl_info -> Location.t

val location_from_an : CLintersContext.context -> Ctl_parser_types.ast_node -> Location.t

val location_from_decl : CLintersContext.context -> Clang_ast_t.decl -> Location.t

val ivar_name : Ctl_parser_types.ast_node -> string

val cxx_ref_captured_in_block : Ctl_parser_types.ast_node -> string

val decl_ref_or_selector_name : Ctl_parser_types.ast_node -> string

val iphoneos_target_sdk_version : CLintersContext.context -> Ctl_parser_types.ast_node -> string

val available_ios_sdk : Ctl_parser_types.ast_node -> string

val class_available_ios_sdk : Ctl_parser_types.ast_node -> string

val receiver_method_call : Ctl_parser_types.ast_node -> string

val class_name : Ctl_parser_types.ast_node -> string
