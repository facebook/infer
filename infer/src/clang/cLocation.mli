(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Module for function to retrieve the location (file, line, etc) of instructions *)

val clang_to_sil_location : Clang_ast_t.source_location -> int -> Cfg.Procdesc.t option ->
Sil.location

val get_sil_location : Clang_ast_t.stmt_info -> int -> CContext.t -> Sil.location

val get_line : Clang_ast_t.stmt_info -> int -> int

val should_translate_lib : Clang_ast_t.source_range -> bool

val should_translate_enum : Clang_ast_t.source_range -> bool

val update_curr_file : Clang_ast_t.decl_info -> unit

val init_curr_source_file : DB.source_file -> unit

val check_source_file : string -> unit

val source_file_from_path : string -> DB.source_file

val get_sil_location_from_range : Clang_ast_t.source_range -> bool -> Sil.location
