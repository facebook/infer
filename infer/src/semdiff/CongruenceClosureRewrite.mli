(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module CC = CongruenceClosureSolver

module Var : sig
  type t = private string [@@deriving compare, equal]

  val of_string : string -> t
end

type subst

val pp_subst : CC.t -> F.formatter -> subst -> unit

val mk_subst : (Var.t * CC.Atom.t) list -> subst

module Pattern : sig
  type t = Var of Var.t | Term of {header: CC.header; args: t list}

  val pp : F.formatter -> t -> unit
end

module Rule : sig
  type t = {lhs: Pattern.t; rhs: Pattern.t}

  val pp : F.formatter -> t -> unit

  val rewrite_once : ?debug:bool -> CC.t -> t list -> unit
end

module TestOnly : sig
  val e_match_pattern_at : ?debug:bool -> CC.t -> Pattern.t -> CC.Atom.t -> subst list

  val e_match_pattern : ?debug:bool -> CC.t -> Pattern.t -> f:(CC.Atom.t -> subst -> unit) -> unit

  val pattern_to_term : CC.t -> subst -> Pattern.t -> CC.Atom.t

  val apply_rule_at : ?debug:bool -> CC.t -> Rule.t -> CC.Atom.t -> int
  (** return the number of compatible substitution used during rewriting *)
end
