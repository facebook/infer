(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module that contains constants and global state used in the frontend *)

type clang_lang = C | CPP | ObjC | ObjCPP [@@deriving compare, equal]

type translation_unit_context =
  { lang: clang_lang
  ; source_file: SourceFile.t
  ; integer_type_widths: IntegerWidths.t
  ; is_objc_arc_on: bool }

type decl_trans_context = [`DeclTraversal | `Translation | `CppLambdaExprTranslation]

type instr_type =
  | ClangStmt of Procdesc.Node.stmt_nodekind * Clang_ast_t.stmt
  | CXXConstructorInit of Clang_ast_t.cxx_ctor_initializer

(** Constants *)

let alloc = "alloc"

let allocWithZone = "allocWithZone:"

let arrayWithObjects_count = "arrayWithObjects:count:"

let dictionaryWithObjects_forKeys_count = "dictionaryWithObjects:forKeys:count:"

let dealloc = "dealloc"

let assert_fail = "__assert_fail"

let assert_rtn = "__assert_rtn"

let biniou_buffer_size =
  (* the C++ standard suggests that implementation should support string literals up to this length *)
  65535


let builtin_expect = "__builtin_expect"

let builtin_memset_chk = "__builtin___memset_chk"

let builtin_object_size = "__builtin_object_size"

(** script to run our own clang *)
let clang_bin xx =
  Config.bin_dir ^/ Filename.parent_dir_name ^/ Filename.parent_dir_name ^/ "facebook-clang-plugins"
  ^/ "clang" ^/ "install" ^/ "bin" ^/ "clang" ^ xx


let class_method = "class"

let cxx_constructor = "__cxx_constructor"

let fbAssertWithSignalAndLogFunctionHelper = "FBAssertWithSignalAndLogFunctionHelper"

let google_LogMessageFatal = "google::LogMessageFatal_LogMessageFatal"

let google_MakeCheckOpString = "google::MakeCheckOpString"

let handleFailureInFunction = "handleFailureInFunction:file:lineNumber:description:"

let handleFailureInMethod = "handleFailureInMethod:object:file:lineNumber:description:"

let infer = "infer"

let init = "init"

let is_kind_of_class = "isKindOfClass:"

let malloc = "malloc"

let new_str = "new"

let next_object = "nextObject"

let nsenumerator_cl = "NSEnumerator"

let nsstring_cl = "NSString"

let objc_class = "objc_class"

let objc_object = "objc_object"

let object_enumerator = "objectEnumerator"

let return_param = "__return_param"

let self = "self"

let std_addressof = QualifiedCppName.Match.of_fuzzy_qual_names ["std::addressof"]

let string_with_utf8_m = "stringWithUTF8String:"

let this = "this"

let replace_with_deref_first_arg_attr = "__infer_replace_with_deref_first_arg"

let modeled_function_attributes = [replace_with_deref_first_arg_attr]

(** Global state *)

let enum_map = ref ClangPointers.Map.empty

let global_translation_unit_decls : Clang_ast_t.decl list ref = ref []

let log_out = ref Format.std_formatter

let sil_types_map = ref Clang_ast_extend.TypePointerMap.empty

let procedures_attempted = ref 0

let procedures_failed = ref 0

let reset_global_state () =
  enum_map := ClangPointers.Map.empty ;
  global_translation_unit_decls := [] ;
  log_out := Format.std_formatter ;
  sil_types_map := Clang_ast_extend.TypePointerMap.empty ;
  procedures_attempted := 0 ;
  procedures_failed := 0
