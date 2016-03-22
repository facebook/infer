(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module that contains constants and variables used in the frontend *)

val global_translation_unit_decls : Clang_ast_t.decl list ref

(** arguments of InferClang *)

val debug_mode : bool ref

val stats_mode : bool ref

val models_mode : bool ref

val source_file : string option ref

type lang =
  | C
  | CPP
  | OBJC
  | OBJCPP

val lang_from_string : string -> unit

val lang_to_string : lang -> string

val language : lang ref

val ast_file : string option ref

val no_translate_libs : bool ref

val testing_mode : bool ref

val cxx_experimental : bool ref

(** constants *)

val json : string ref

val pointer_decl_index : Clang_ast_t.decl Clang_ast_main.PointerMap.t ref

val pointer_stmt_index : Clang_ast_t.stmt Clang_ast_main.PointerMap.t ref

val objc_object : string

val id_cl : string

val self : string

val this : string

val return_param : string

val nsstring_cl : string

val nsobject_cl : string

val next_object : string

val nsautorelease_pool_cl : string

val string_with_utf8_m : string

val is_kind_of_class : string

val alloc : string

val malloc : string

val free : string

val static : string

val array_with_objects_count_m : string

val object_at_indexed_subscript_m : string

val emtpy_name_category : string

val objc_class : string

val class_type : string

val retain : string

val release : string

val drain : string

val autorelease : string

val copy : string

val mutableCopy : string

val new_str : string

val init : string

val temp_var : string

val invalid_pointer : int

val void : string

val class_method : string

val cf_non_null_alloc : string

val cf_alloc : string

val cf_bridging_release : string

val cf_bridging_retain : string

val cf_autorelease : string

val ns_make_collectable : string

val builtin_expect : string

val builtin_memset_chk : string

val builtin_object_size : string

val fbAssertWithSignalAndLogFunctionHelper : string

val assert_fail : string

val assert_rtn : string

val handleFailureInMethod : string

val handleFailureInFunction : string

val pseudo_object_type : string

val count : string

val objects : string

val enumerateObjectsUsingBlock : string

(** Map from clang pointers to types produced by ast exporter.
    Populated once on InferClang startup *)
val pointer_type_index : Clang_ast_t.c_type Clang_ast_main.PointerMap.t ref

(** Map from type pointers (clang pointers and types created later by frontend) to sil types
    Populated during frontend execution when new type is found *)
val sil_types_map : (Sil.typ Clang_ast_types.TypePointerMap.t) ref

(** Map from enum constants pointers to their predecesor and their sil value *)
val enum_map : (Clang_ast_t.pointer option * Sil.exp option) Clang_ast_main.PointerMap.t ref

val nsarray_cl : string

val infer : string

val block : string

val atomic_att : string

val infer_skip_gcc_ast_stmt : string

val infer_skip_fun : string
