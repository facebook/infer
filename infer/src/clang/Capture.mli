(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val run_clang : ClangCommand.t -> (In_channel.t -> 'a) -> 'a

val capture : ClangCommand.t -> unit
(** If the command is detected to correspond to a source file, translate it.

    + Given a compilation command, attach our [ASTExporter] clang plugin to the command and run it.
    + Our clang plugin emits the AST (Abstract Syntax Tree) as Biniou data that we deserialize. The
      AST format is described in {!module-ATDGenerated.Clang_ast_t} (and its Biniou API is in
      {!module-ATDGenerated.Clang_ast_b}).
    + If enabled, invoke translation to {!module-IR.Sil} via {!module-CFrontend}. *)
