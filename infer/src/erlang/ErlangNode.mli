(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Env = ErlangEnvironment

val make_stmt :
     (Procdesc.t Env.present, _) Env.t
  -> ?kind:Procdesc.Node.stmt_nodekind
  -> Sil.instr list
  -> Procdesc.Node.t

val make_load : (Procdesc.t Env.present, _) Env.t -> Ident.t -> Exp.t -> Typ.t -> Procdesc.Node.t

val make_nop : (Procdesc.t Env.present, _) Env.t -> Procdesc.Node.t

val make_join : (Procdesc.t Env.present, _) Env.t -> Procdesc.Node.t

val make_if : (Procdesc.t Env.present, _) Env.t -> bool -> Exp.t -> Procdesc.Node.t

val make_fail : (Procdesc.t Env.present, _) Env.t -> Procname.t -> Procdesc.Node.t
