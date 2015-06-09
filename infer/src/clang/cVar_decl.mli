(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Process variable declarations by saving them as local or global variables.  *)
(** Computes the local variables of a function or method to be added to the procdesc *)

val get_fun_locals : CContext.t -> Clang_ast_t.stmt list -> unit

val global_var_decl : Sil.tenv -> string option -> Clang_ast_t.decl_info -> string ->
Clang_ast_t.qual_type -> unit

