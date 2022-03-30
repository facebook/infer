(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val assume_type :
     (Procdesc.t ErlangEnvironment.present, 'a) ErlangEnvironment.t
  -> (string, ErlangAst.type_, 'b) Map_intf.Map.t
  -> Ident.t * ErlangAst.type_
  -> ErlangBlock.t * Exp.t

val assume_spec :
     (Procdesc.t ErlangEnvironment.present, _) ErlangEnvironment.t
  -> Ident.t list
  -> ErlangAst.spec
  -> ErlangBlock.t
(** Given a function spec and a list if identifiers corresponding to function arguments, returns a
    block that assumes the types of the arguments. The assumptions lead to [exit_success] while
    [exit_failure] is an unreachable no-op node. *)
