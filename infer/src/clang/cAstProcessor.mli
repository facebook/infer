(*
* Copyright (c) 2015 - Facebook.
* All rights reserved.
*)

(** Module to preprocess location information in the AST.
The original location information is incremental, each location is a delta
w.r.t. the previous one. This module processes the AST and makes locations explicit. *)

(** Pretty print an AST. *)
val pp_ast_decl : Format.formatter -> Clang_ast_j.decl -> unit

(** Preprocess the AST to make locations explicit. *)
val preprocess_ast_decl : Clang_ast_j.decl -> Clang_ast_j.decl
