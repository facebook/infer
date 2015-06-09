(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** In this module an ObjC category declaration or implementation is processed. The category    *)
(** is saved in the tenv as a struct with the corresponding fields and methods , and the class it belongs to *)

val category_decl : Sil.tenv -> string -> Clang_ast_t.obj_c_category_decl_info -> Clang_ast_t.decl list
-> CContext.curr_class

val category_impl_decl : Sil.tenv -> string -> Clang_ast_t.decl_info ->
Clang_ast_t.obj_c_category_impl_decl_info -> Clang_ast_t.decl list -> CContext.curr_class
