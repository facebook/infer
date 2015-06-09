(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

module type CTranslation =
sig
  val instructions_trans : CContext.t -> Clang_ast_t.stmt list -> Cfg.Node.t -> Cfg.Node.t list
end

module type CMethod_declaration =
sig
  val function_decl : Sil.tenv -> Cfg.cfg -> Cg.t -> string option -> bool -> Clang_ast_t.decl_info ->
  string -> Clang_ast_t.qual_type -> Clang_ast_t.function_decl_info -> (Mangled.t * Sil.typ * bool) list -> Procname.t option -> CContext.curr_class -> unit
end
