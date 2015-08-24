(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module to preprocess location information in the AST.
    The original location information is incremental, each location is a delta
    w.r.t. the previous one. This module processes the AST and makes locations explicit. *)

(** Pretty print an AST. *)
val pp_ast_decl : Format.formatter -> Clang_ast_t.decl -> unit

(** Preprocess the AST to make locations explicit. *)
val preprocess_ast_decl : Clang_ast_t.decl -> Clang_ast_t.decl
