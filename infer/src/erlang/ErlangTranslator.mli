(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val translate_module : (_, _) ErlangEnvironment.t -> ErlangAst.module_ -> string option -> unit
(** Translate a module. Assumes an initialized environment and an AST annotated with scope
    information (see [ErlangScopes.annotate_scopes]). *)
