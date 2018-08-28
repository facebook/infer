(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for function to retrieve the location (file, line, etc) of instructions *)

val clang_to_sil_location : SourceFile.t -> Clang_ast_t.source_location -> Location.t

val should_translate_lib :
     SourceFile.t
  -> Clang_ast_t.source_range
  -> CModule_type.decl_trans_context
  -> translate_when_used:bool
  -> bool

val should_do_frontend_check : SourceFile.t -> Clang_ast_t.source_range -> bool

val is_file_blacklisted : string -> bool

val location_of_source_range :
  ?pick_location:[`Start | `End] -> SourceFile.t -> Clang_ast_t.source_range -> Location.t
(** picks the start of the source range by default *)

val location_of_stmt_info : SourceFile.t -> Clang_ast_t.stmt_info -> Location.t
