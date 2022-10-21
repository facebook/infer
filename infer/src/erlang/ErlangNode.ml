(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Env = ErlangEnvironment

let make (env : (Procdesc.t Env.present, _) Env.t) kind instructions =
  let (Present procdesc) = env.procdesc in
  Procdesc.create_node procdesc env.location kind instructions


let make_stmt env ?(kind = Procdesc.Node.Erlang) instructions =
  make env (Stmt_node kind) instructions


let make_load (env : (_, _) Env.t) id e typ =
  let (Env.Present procdesc) = env.procdesc in
  let procname = Procdesc.get_proc_name procdesc in
  let temp_pvar = Pvar.mk_tmp "LoadBlock" procname in
  let instructions =
    [ Sil.Store {e1= Lvar temp_pvar; e2= e; typ; loc= env.location}
    ; Sil.Load {id; e= Lvar temp_pvar; typ; loc= env.location} ]
  in
  make_stmt env ~kind:ErlangExpression instructions


let make_nop env = make_stmt env []

let make_join env = make env Join_node []

let make_throw env one_instruction = make env Procdesc.Node.throw_kind [one_instruction]

let make_if (env : (_, _) Env.t) branch expr =
  let prune_kind : Procdesc.Node.prune_node_kind =
    if branch then PruneNodeKind_TrueBranch else PruneNodeKind_FalseBranch
  in
  let condition : Exp.t = if branch then expr else UnOp (LNot, expr, Some (Typ.mk (Tint IBool))) in
  let kind : Procdesc.Node.nodekind = Prune_node (branch, Ik_if {terminated= false}, prune_kind) in
  let prune : Sil.instr = Prune (condition, env.location, branch, Ik_if {terminated= false}) in
  make env kind [prune]


let make_fail (env : (_, _) Env.t) fail_function =
  let any = Env.typ_of_name Any in
  let crash_instruction =
    let ret_var = Ident.create_fresh Ident.knormal (* not used: nothing returned *) in
    let pattern_fail_fun = Exp.Const (Cfun fail_function) in
    Sil.Call ((ret_var, any), pattern_fail_fun, [], env.location, CallFlags.default)
  in
  make_throw env crash_instruction
