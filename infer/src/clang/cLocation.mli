(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for function to retrieve the location (file, line, etc) of instructions *)

val should_translate_lib :
     SourceFile.t
  -> Clang_ast_t.source_range
  -> CFrontend_config.decl_trans_context
  -> translate_when_used:bool
  -> bool

val is_file_block_listed : string -> bool

val clang_to_sil_location : SourceFile.t -> Clang_ast_t.source_location -> Location.t

val location_of_source_range :
  ?pick_location:[`Start | `End] -> SourceFile.t -> Clang_ast_t.source_range -> Location.t
(** picks the start of the source range by default *)

val location_of_stmt_info : SourceFile.t -> Clang_ast_t.stmt_info -> Location.t
