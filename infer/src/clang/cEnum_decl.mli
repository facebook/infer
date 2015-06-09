(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Translate an enumeration declaration by adding it to the tenv and *)
(** translating the code and adding it to a fake procdesc *)

val enum_decl : string -> Sil.tenv -> Cfg.cfg -> Cg.t -> string option ->
Clang_ast_t.decl list -> Clang_ast_t.opt_type -> unit
