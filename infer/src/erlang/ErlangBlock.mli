(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Env = ErlangEnvironment

type t = {start: Procdesc.Node.t; exit_success: Procdesc.Node.t; exit_failure: Procdesc.Node.t}

val ( |~~> ) : Procdesc.Node.t -> Procdesc.Node.t list -> unit

val make_success : (Procdesc.t Env.present, _) Env.t -> t

val make_failure : (Procdesc.t Env.present, _) Env.t -> t

val all : (Procdesc.t Env.present, _) Env.t -> t list -> t
(** Chain a list of blocks together in a conjunctive style: a failure in any block leads to a global
    failure, and successes lead to the next block. *)

val any : (Procdesc.t Env.present, _) Env.t -> t list -> t
(** Chain a list of blocks together in a disjunctive style: a success in any block leads to a global
    success, and failures lead to the next block. *)

val make_instruction : (Procdesc.t Env.present, _) Env.t -> Sil.instr list -> t

val make_load : (Procdesc.t Env.present, _) Env.t -> Ident.t -> Exp.t -> Typ.t -> t

val make_branch : (Procdesc.t Env.present, _) Env.t -> Exp.t -> t
(** Make a branch based on the condition: go to success if true, go to failure if false *)
