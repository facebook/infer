(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Process methods or functions declarations by adding them to the cfg. *)

module CMethod_decl_funct(T: CModule_type.CTranslation) : sig
  val process_methods : Sil.tenv -> Cg.t -> Cfg.cfg -> CContext.curr_class -> string option ->
  Clang_ast_t.decl list -> unit

  val function_decl : Sil.tenv -> Cfg.cfg -> Cg.t -> string option -> bool -> Clang_ast_t.decl_info ->
  string -> Clang_ast_t.qual_type -> Clang_ast_t.function_decl_info -> (Mangled.t * Sil.typ * bool) list -> Procname.t option -> CContext.curr_class -> unit

  val create_function_signature : Clang_ast_t.decl_info -> Clang_ast_t.function_decl_info -> string ->
  Clang_ast_t.qual_type -> bool -> Procname.t option  -> Clang_ast_t.stmt option * CMethod_signature.method_signature
end

module type CMethod_decl = sig
  val process_methods : Sil.tenv -> Cg.t -> Cfg.cfg -> CContext.curr_class -> string option ->
  Clang_ast_t.decl list -> unit

  val function_decl : Sil.tenv -> Cfg.cfg -> Cg.t -> string option -> bool -> Clang_ast_t.decl_info ->
  string -> Clang_ast_t.qual_type -> Clang_ast_t.function_decl_info -> (Mangled.t * Sil.typ * bool) list -> Procname.t option -> CContext.curr_class -> unit

  val create_function_signature : Clang_ast_t.decl_info -> Clang_ast_t.function_decl_info -> string ->
  Clang_ast_t.qual_type -> bool -> Procname.t option -> Clang_ast_t.stmt option * CMethod_signature.method_signature
end
