(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module that contains constants and global state used in the frontend *)

type clang_lang = C | CPP | ObjC | ObjCPP [@@deriving compare]

let equal_clang_lang = [%compare.equal : clang_lang]

type translation_unit_context = {lang: clang_lang; source_file: SourceFile.t}

(** Constants *)

let alloc = "alloc"

let array_with_objects_count_m = "arrayWithObjects:count:"

let assert_fail = "__assert_fail"

let assert_rtn = "__assert_rtn"

let atomic_att = "<\"Atomic\">"

let autorelease = "autorelease"

let block = "block"

let builtin_expect = "__builtin_expect"

let builtin_memset_chk = "__builtin___memset_chk"

let builtin_object_size = "__builtin_object_size"

let cf_alloc = "__cf_alloc"

let cf_autorelease = "CFAutorelease"

let cf_bridging_release = "CFBridgingRelease"

let cf_bridging_retain = "CFBridgingRetain"

let cf_non_null_alloc = "__cf_non_null_alloc"

let ckcomponent_cl = "CKComponent"

let ckcomponentcontroller_cl = "CKComponentController"

(** script to run our own clang *)
let clang_bin xx =
  Config.bin_dir ^/ Filename.parent_dir_name ^/ Filename.parent_dir_name
  ^/ "facebook-clang-plugins" ^/ "clang" ^/ "install" ^/ "bin" ^/ "clang" ^ xx

let class_method = "class"

let class_type = "Class"

let copy = "copy"

let count = "count"

let drain = "drain"

let emtpy_name_category = "EMPTY_NAME_CATEGORY_FOR_"

let enumerateObjectsUsingBlock = "enumerateObjectsUsingBlock:"

let fbAssertWithSignalAndLogFunctionHelper = "FBAssertWithSignalAndLogFunctionHelper"

let free = "free"

let google_LogMessageFatal = "google::LogMessageFatal_LogMessageFatal"

let google_MakeCheckOpString = "google::MakeCheckOpString"

let handleFailureInFunction = "handleFailureInFunction:file:lineNumber:description:"

let handleFailureInMethod = "handleFailureInMethod:object:file:lineNumber:description:"

let id_cl = "id"

let infer = "infer"

let infer_skip_fun = "__infer_skip_function"

let infer_skip_gcc_asm_stmt = "__infer_skip_gcc_asm_stmt"

let init = "init"

let invalid_pointer = 0

let is_kind_of_class = "isKindOfClass:"

let malloc = "malloc"

let mutableCopy = "mutableCopy"

let new_str = "new"

let next_object = "nextObject"

let ns_make_collectable = "NSMakeCollectable"

let nsarray_cl = "NSArray"

let nsautorelease_pool_cl = "NSAutoreleasePool"

let nsproxy_cl = "NSProxy"

let nsobject_cl = "NSObject"

let nsstring_cl = "NSString"

let objc_class = "objc_class"

let objc_object = "objc_object"

let object_at_indexed_subscript_m = "objectAtIndexedSubscript:"

let objects = "objects"

let pseudo_object_type = "<pseudo-object type>"

let release = "release"

let retain = "retain"

let return_param = "__return_param"

let self = "self"

let static = "static"

let string_with_utf8_m = "stringWithUTF8String:"

let this = "this"

let void = "void"

let replace_with_deref_first_arg_attr = "__infer_replace_with_deref_first_arg"

let modeled_function_attributes = [replace_with_deref_first_arg_attr]

(** Global state *)

let enum_map = ref ClangPointers.Map.empty

let global_translation_unit_decls : Clang_ast_t.decl list ref = ref []

let log_out = ref Format.std_formatter

let sil_types_map = ref Clang_ast_extend.TypePointerMap.empty

let reset_global_state () =
  enum_map := ClangPointers.Map.empty ;
  global_translation_unit_decls := [] ;
  log_out := Format.std_formatter ;
  sil_types_map := Clang_ast_extend.TypePointerMap.empty
