(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val is_cf_non_null_alloc : Procname.t -> bool

val is_alloc : Procname.t -> bool

val is_alloc_model : Typ.t -> Procname.t -> bool

val is_builtin_expect : Procname.t -> bool

val is_builtin_object_size : Procname.t -> bool

val is_replace_with_deref_first_arg : Procname.t -> bool

val is_objc_memory_model_controlled : string -> bool

val is_assert_log : Procname.t -> bool

val is_handleFailureInMethod : string -> bool

val is_modeled_builtin : string -> bool

val is_release_builtin : string -> Clang_ast_t.type_ptr -> bool

val is_retain_builtin : string -> Clang_ast_t.type_ptr -> bool

val is_modeled_attribute : string -> bool

val is_toll_free_bridging : Procname.t -> bool

val is_cf_retain_release : Procname.t -> bool

val get_predefined_model_method_signature : string -> string ->
  (string -> string -> Procname.objc_cpp_method_kind -> Procname.t) ->
  CFrontend_config.clang_lang -> CMethod_signature.method_signature option

val is_dispatch_function_name : string -> (string * int) option
