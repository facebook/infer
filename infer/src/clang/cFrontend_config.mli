(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
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

val language : lang ref

val ast_file : string option ref

val no_translate_libs : bool ref

val testing_mode : bool ref

(** constants *)

val json : string ref

val objc_object : string

val id_cl : string

val self : string

val nsstring_cl : string

val nsobject_cl : string

val next_object : string

val nsautorelease_pool_cl : string

val string_with_utf8_m : string

val alloc : string

val malloc : string

val static : string

val array_with_objects_count_m : string

val dict_with_objects_and_keys_m : string

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

val pointer_prefix : string

val void : string

val class_method : string

val cf_non_null_alloc : string

val cf_alloc : string

val cf_bridging_release : string

val cf_bridging_retain : string

val cf_autorelease : string

val builtin_expect : string

val fbAssertWithSignalAndLogFunctionHelper : string

val assert_fail : string

val assert_rtn : string

val handleFailureInMethod : string

val handleFailureInFunction : string
