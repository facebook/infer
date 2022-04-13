(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val type_condition :
     (Procdesc.t ErlangEnvironment.present, 'a) ErlangEnvironment.t
  -> (string, ErlangAst.type_, 'b) Map_intf.Map.t
  -> Ident.t * ErlangAst.type_
  -> ErlangBlock.t * Exp.t
(** Given an argument, its type (and a list of constraints), returns a condition that is true if the
    arguments has the specified type. Also returns a block for intermediate computations. *)

val prune_spec :
     (Procdesc.t ErlangEnvironment.present, _) ErlangEnvironment.t
  -> Ident.t list
  -> ErlangAst.spec
  -> ErlangBlock.t
(** Given a function spec and a list if identifiers corresponding to function arguments, returns a
    block that prunes based on the types of the arguments. The assumptions lead to [exit_success]
    while [exit_failure] is an unreachable no-op node. *)
