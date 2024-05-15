(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module that contains constants and global state used in the frontend *)

type clang_lang = C | CPP | ObjC | ObjCPP [@@deriving compare]

val equal_clang_lang : clang_lang -> clang_lang -> bool

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

val alloc : string

val allocWithZone : string

val arrayWithObjects_count : string

val dictionaryWithObjects_forKeys_count : string

val dealloc : string

val assert_fail : string

val assert_rtn : string

val biniou_buffer_size : int

val builtin_expect : string

val builtin_memset_chk : string

val builtin_object_size : string

val clang_bin : string -> string
(** Script to run our own clang. The argument is expected to be either "" or "++". *)

val class_method : string

val cxx_constructor : string

val fbAssertWithSignalAndLogFunctionHelper : string

val google_LogMessageFatal : string

val google_MakeCheckOpString : string

val handleFailureInFunction : string

val handleFailureInMethod : string

val infer : string

val init : string

val is_kind_of_class : string

val malloc : string

val new_str : string

val next_object : string

val nsenumerator_cl : string

val nsstring_cl : string

val objc_class : string

val objc_object : string

val object_enumerator : string

val return_param : string

val self : string

val std_addressof : QualifiedCppName.Match.quals_matcher

val string_with_utf8_m : string

val this : string

val replace_with_deref_first_arg_attr : string

val modeled_function_attributes : string list

(** Global state *)

val enum_map : (Clang_ast_t.pointer option * Exp.t option) ClangPointers.Map.t ref
(** Map from enum constants pointers to their predecesor and their sil value *)

val global_translation_unit_decls : Clang_ast_t.decl list ref

val sil_types_map : Typ.desc Clang_ast_extend.TypePointerMap.t ref
(** Map from type pointers (clang pointers and types created later by frontend) to sil types
    Populated during frontend execution when new type is found *)

val procedures_attempted : int ref

val procedures_failed : int ref

val reset_global_state : unit -> unit
