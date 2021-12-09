(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val annotate_scopes : (_, _) ErlangEnvironment.t -> ErlangAst.module_ -> unit
(** This is a preprocessing pass for the translation. Annotates the AST with scoping information:
    (1) Scope (procname) for each variable (2) Set of captured variables for each lambda. (3) Fresh
    procnames for lambdas, which are used to identify them. (This is required by point 1.) *)
