(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module that contains constants and global state used in the frontend *)

type clang_lang = C | CPP | ObjC | ObjCPP [@@deriving compare]

val string_of_clang_lang : clang_lang -> string

val equal_clang_lang : clang_lang -> clang_lang -> bool

type exception_details =
  { msg: string
  ; position: Logging.ocaml_pos
  ; source_range: Clang_ast_t.source_range
  ; ast_node: string option }

exception Unimplemented of exception_details

val unimplemented :
     Logging.ocaml_pos
  -> Clang_ast_t.source_range
  -> ?ast_node:string
  -> ('a, Format.formatter, unit, _) format4
  -> 'a
(** Raise Unimplemented. This is caught at the level of translating a method and makes the frontend
    give up on that method. *)

exception IncorrectAssumption of exception_details

val incorrect_assumption :
     Logging.ocaml_pos
  -> Clang_ast_t.source_range
  -> ?ast_node:string
  -> ('a, Format.formatter, unit, _) format4
  -> 'a
(** Used to mark places in the frontend that incorrectly assume something to be
    impossible. TODO(t21762295) get rid of all instances of this. *)

type translation_unit_context =
  {lang: clang_lang; source_file: SourceFile.t; integer_type_widths: Typ.IntegerWidths.t}

exception Invalid_declaration

(** Constants *)

val alloc : string

val assert_fail : string

val assert_rtn : string

val biniou_buffer_size : int

val builtin_expect : string

val builtin_memset_chk : string

val builtin_object_size : string

val ckcomponent_cl : string

val ckcomponentcontroller_cl : string

val clang_bin : string -> string
(** Script to run our own clang. The argument is expected to be either "" or "++". *)

val class_method : string

val fbAssertWithSignalAndLogFunctionHelper : string

val google_LogMessageFatal : string

val google_MakeCheckOpString : string

val handleFailureInFunction : string

val handleFailureInMethod : string

val id_cl : string

val infer : string

val infer_skip_fun : string

val infer_skip_gcc_asm_stmt : string

val infer_generic_selection_expr : string

val init : string

val is_kind_of_class : string

val malloc : string

val new_str : string

val next_object : string

val nsproxy_cl : string

val nsobject_cl : string

val nsstring_cl : string

val objc_class : string

val objc_object : string

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

val get_fresh_block_index : unit -> int

val reset_block_counter : unit -> unit

val reset_global_state : unit -> unit

val tableaux_evaluation : bool
