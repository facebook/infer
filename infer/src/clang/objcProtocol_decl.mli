(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** In this module an ObjC protocol declaration or implementation is processed. The protocol    *)
(** is saved in the tenv as a struct with the corresponding methods  *)

val protocol_decl : Sil.tenv -> string -> Clang_ast_t.decl list -> CContext.curr_class
