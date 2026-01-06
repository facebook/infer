(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module CC = CongruenceClosureSolver
module Rewrite = CongruenceClosureRewrite

val check_equivalence : PythonSourceAst.Node.t -> PythonSourceAst.Node.t -> bool

module TestOnly : sig
  val store_ast : ?debug:bool -> PythonSourceAst.Node.t -> unit

  val are_ast_equivalent :
    CC.t -> PythonSourceAst.Node.t -> PythonSourceAst.Node.t -> Rewrite.Rule.t list -> bool
end
