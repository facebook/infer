(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Module for function to retrieve the location (file, line, etc) of instructions *)

(** Inside the AST there may be code or type definitions from other files than the one passed as an
    argument. That current file in the translation is saved in this variable. *)
val curr_file : DB.source_file ref

val clang_to_sil_location : CFrontend_config.translation_unit_context ->
  Clang_ast_t.source_location -> Cfg.Procdesc.t option ->
  Location.t

val get_sil_location : Clang_ast_t.stmt_info -> CContext.t -> Location.t

val should_translate_lib : CFrontend_config.translation_unit_context -> Clang_ast_t.source_range ->
  CModule_type.decl_trans_context -> translate_when_used:bool -> bool

val should_do_frontend_check : CFrontend_config.translation_unit_context ->
  Clang_ast_t.source_range -> bool

val update_curr_file : CFrontend_config.translation_unit_context -> Clang_ast_t.decl_info -> unit

val is_file_blacklisted : string -> bool

val source_file_from_path : string -> DB.source_file

val get_sil_location_from_range : CFrontend_config.translation_unit_context ->
  Clang_ast_t.source_range -> bool -> Location.t
