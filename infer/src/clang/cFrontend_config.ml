(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module that contains constants and variables used in the frontend *)

let no_translate_libs = ref true

let testing_mode = ref false

let cxx_experimental = ref false

let array_with_objects_count_m = "arrayWithObjects:count:"

let object_at_indexed_subscript_m = "objectAtIndexedSubscript:"

let string_with_utf8_m = "stringWithUTF8String:"

let is_kind_of_class = "isKindOfClass:"

let nsstring_cl = "NSString"

let nsobject_cl = "NSObject"

let next_object = "nextObject"

let nsautorelease_pool_cl = "NSAutoreleasePool"

let id_cl = "id"

let self = "self"

let this = "this"

let return_param = "__return_param"

let alloc = "alloc"

let malloc = "malloc"

let free = "free"

let static = "static"

let source_file : string option ref = ref None

let ast_file : string option ref = ref None

let json = ref ""

let pointer_decl_index = ref Clang_ast_main.PointerMap.empty

let pointer_stmt_index = ref Clang_ast_main.PointerMap.empty

let debug_mode = ref false

let stats_mode = ref false

let models_mode = ref false

type lang =
  | C
  | CPP
  | OBJC
  | OBJCPP

let language = ref OBJC (* Default is objc, since it's the default for clang (at least in Mac OS) *)

let lang_from_string lang_string =
  let lang =
    if lang_string = "c" then C
    else if lang_string = "objective-c" then OBJC
    else if lang_string = "c++" then CPP
    else if lang_string = "objective-c++" then OBJCPP
    else assert false in
  language := lang

let lang_to_string lang =
  match lang with
  | C -> "c"
  | OBJC -> "objective-c"
  | CPP -> "c++"
  | OBJCPP -> "objective-c++"

let emtpy_name_category ="EMPTY_NAME_CATEGORY_FOR_"

let objc_object = "objc_object"

let objc_class = "objc_class"

let class_type = "Class"

let global_translation_unit_decls : Clang_ast_t.decl list ref = ref []

let retain = "retain"

let release = "release"

let drain = "drain"

let autorelease = "autorelease"

let copy = "copy"

let mutableCopy = "mutableCopy"

let new_str = "new"

let init = "init"

let temp_var = "infer"

let invalid_pointer = 0

let void = "void"

let class_method = "class"

let cf_non_null_alloc ="__cf_non_null_alloc"

let cf_alloc ="__cf_alloc"

let cf_bridging_release = "CFBridgingRelease"

let cf_bridging_retain = "CFBridgingRetain"

let cf_autorelease = "CFAutorelease"

let ns_make_collectable = "NSMakeCollectable"

let builtin_expect = "__builtin_expect"

let builtin_memset_chk = "__builtin___memset_chk"

let builtin_object_size = "__builtin_object_size"

let assert_fail = "__assert_fail"

let assert_rtn = "__assert_rtn"

let handleFailureInMethod = "handleFailureInMethod:object:file:lineNumber:description:"

let handleFailureInFunction = "handleFailureInFunction:file:lineNumber:description:"

let fbAssertWithSignalAndLogFunctionHelper = "FBAssertWithSignalAndLogFunctionHelper"

let pseudo_object_type = "<pseudo-object type>"

let count = "count"

let objects = "objects"

let enumerateObjectsUsingBlock = "enumerateObjectsUsingBlock:"

let pointer_type_index = ref Clang_ast_main.PointerMap.empty

(* Map from type pointers or declaration pointers to sil types *)
let sil_types_map = ref Clang_ast_types.TypePointerMap.empty

(* Map from enum constants pointers to their predecesor and their sil value *)
let enum_map = ref Clang_ast_main.PointerMap.empty

let nsarray_cl = "NSArray"

let infer = "infer"

let block = "block"

let atomic_att = "<\"Atomic\">"

let infer_skip_gcc_ast_stmt = "__infer_skip_gcc_ast_stmt"

let infer_skip_fun = "__infer_skip_function"
