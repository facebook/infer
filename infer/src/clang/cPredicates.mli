(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t = ALVar.formula_id * ALVar.alexp list (* (name, [param1,...,paramK]) *)

val captured_variables_cxx_ref : Ctl_parser_types.ast_node -> Clang_ast_t.named_decl_info list

val call_method_strict : Ctl_parser_types.ast_node -> string -> bool

val call_method : Ctl_parser_types.ast_node -> string -> bool

val call_class_method : Ctl_parser_types.ast_node  -> string -> string -> bool

val call_class_method_strict : Ctl_parser_types.ast_node  -> string -> string -> bool

val is_objc_interface_named_strict : Ctl_parser_types.ast_node -> string -> bool

val is_objc_interface_named : Ctl_parser_types.ast_node -> string -> bool

val property_name_contains_word : string -> Ctl_parser_types.ast_node -> bool

val is_objc_extension : CLintersContext.context -> bool

val is_syntactically_global_var : Ctl_parser_types.ast_node -> bool

val is_const_expr_var : Ctl_parser_types.ast_node -> bool

val call_function_named : string list -> Ctl_parser_types.ast_node -> bool

val is_strong_property : Ctl_parser_types.ast_node -> bool

val is_assign_property : Ctl_parser_types.ast_node -> bool

val is_property_pointer_type : Ctl_parser_types.ast_node -> bool

val context_in_synchronized_block : CLintersContext.context -> bool

val is_ivar_atomic : Ctl_parser_types.ast_node -> bool

val is_method_property_accessor_of_ivar : Ctl_parser_types.ast_node -> CLintersContext.context -> bool

val is_objc_constructor : CLintersContext.context -> bool

val is_objc_dealloc : CLintersContext.context -> bool

val captures_cxx_references : Ctl_parser_types.ast_node -> bool

val is_binop_with_kind : string -> Ctl_parser_types.ast_node -> bool

val is_unop_with_kind : string -> Ctl_parser_types.ast_node -> bool

val isa : string -> Ctl_parser_types.ast_node -> bool

val is_node : string -> Ctl_parser_types.ast_node -> bool

val declaration_has_name : Ctl_parser_types.ast_node -> string -> bool

val declaration_has_name_strict : Ctl_parser_types.ast_node -> string -> bool

val is_class : Ctl_parser_types.ast_node -> string -> bool

val is_class_strict : Ctl_parser_types.ast_node -> string -> bool

val pp_predicate : Format.formatter -> t -> unit

val decl_unavailable_in_supported_ios_sdk : CLintersContext.context -> Ctl_parser_types.ast_node
  -> bool

val get_available_attr_ios_sdk : Ctl_parser_types.ast_node -> string option

val within_responds_to_selector_block : CLintersContext.context -> Ctl_parser_types.ast_node -> bool
