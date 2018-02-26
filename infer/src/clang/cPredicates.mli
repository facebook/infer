(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t = ALVar.formula_id * ALVar.alexp list [@@deriving compare]

(* (name, [param1,...,paramK]) *)

val captured_variables_cxx_ref : Ctl_parser_types.ast_node -> Clang_ast_t.named_decl_info list

val call_method : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val call_class_method : Ctl_parser_types.ast_node -> ALVar.alexp -> ALVar.alexp -> bool

val call_instance_method : Ctl_parser_types.ast_node -> ALVar.alexp -> ALVar.alexp -> bool

val declaration_name : Clang_ast_t.decl -> string option

val is_enum_constant : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val is_enum_constant_of_enum : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val is_objc_interface_named : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val is_objc_extension : CLintersContext.context -> bool

val is_syntactically_global_var : Ctl_parser_types.ast_node -> bool

val is_const_expr_var : Ctl_parser_types.ast_node -> bool

val call_function : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val is_strong_property : Ctl_parser_types.ast_node -> bool

val is_strong_ivar : Ctl_parser_types.ast_node -> bool

val is_weak_property : Ctl_parser_types.ast_node -> bool

val is_assign_property : Ctl_parser_types.ast_node -> bool

val is_property_pointer_type : Ctl_parser_types.ast_node -> bool

val context_in_synchronized_block : CLintersContext.context -> bool

val is_ivar_atomic : Ctl_parser_types.ast_node -> bool

val is_method_property_accessor_of_ivar :
  Ctl_parser_types.ast_node -> CLintersContext.context -> bool

val is_in_block : CLintersContext.context -> bool

val is_in_cxx_constructor : CLintersContext.context -> ALVar.alexp -> bool

val is_in_cxx_destructor : CLintersContext.context -> ALVar.alexp -> bool

val is_in_cxx_method : CLintersContext.context -> ALVar.alexp -> bool

val is_in_function : CLintersContext.context -> ALVar.alexp -> bool

val is_in_objc_method : CLintersContext.context -> ALVar.alexp -> bool

val is_objc_constructor : CLintersContext.context -> bool

val is_objc_dealloc : CLintersContext.context -> bool

val is_in_objc_subclass_of : CLintersContext.context -> ALVar.alexp -> bool

val is_in_objc_class_named : CLintersContext.context -> ALVar.alexp -> bool

val captures_cxx_references : Ctl_parser_types.ast_node -> bool

val is_binop_with_kind : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val is_unop_with_kind : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val has_cast_kind : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val isa : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val is_node : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val declaration_has_name : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val declaration_ref_name :
  ?kind:Clang_ast_t.decl_kind -> Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val is_class : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val pp_predicate : Format.formatter -> t -> unit

val decl_unavailable_in_supported_ios_sdk :
  CLintersContext.context -> Ctl_parser_types.ast_node -> bool

val class_unavailable_in_supported_ios_sdk :
  CLintersContext.context -> Ctl_parser_types.ast_node -> bool

val has_type : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val has_value : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val method_return_type : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val has_type_subprotocol_of : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val get_available_attr_ios_sdk : Ctl_parser_types.ast_node -> string option

val get_selector : Ctl_parser_types.ast_node -> string option

val within_responds_to_selector_block :
  CLintersContext.context -> Ctl_parser_types.ast_node -> bool

val using_namespace : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val receiver_class_method_call : Ctl_parser_types.ast_node -> Clang_ast_t.decl option

val receiver_method_call : Ctl_parser_types.ast_node -> Clang_ast_t.decl option

val is_at_selector_with_name : Ctl_parser_types.ast_node -> ALVar.alexp -> bool
(** an is an expression @selector with whose name in the language of re *)

val has_visibility_attribute : Ctl_parser_types.ast_node -> ALVar.alexp -> bool

val has_used_attribute : Ctl_parser_types.ast_node -> bool

val iphoneos_target_sdk_version_by_path : CLintersContext.context -> string option

val iphoneos_target_sdk_version_greater_or_equal : CLintersContext.context -> string -> bool

val within_available_class_block : CLintersContext.context -> Ctl_parser_types.ast_node -> bool

val has_type_const_ptr_to_objc_class : Ctl_parser_types.ast_node -> bool

val is_decl : Ctl_parser_types.ast_node -> bool

val get_ast_node_type_ptr : Ctl_parser_types.ast_node -> Clang_ast_t.type_ptr option

val is_method_called_by_superclass : Ctl_parser_types.ast_node -> bool
