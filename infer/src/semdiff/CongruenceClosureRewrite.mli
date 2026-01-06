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

  val pp : F.formatter -> t -> unit

  val of_string : string -> t
end

type subst

val pp_subst : CC.t -> F.formatter -> subst -> unit

val mk_subst : (Var.t * CC.Atom.t) list -> subst

module Pattern : sig
  type t = Var of Var.t | Term of {header: CC.header; args: t list}

  type ellipsis = {header: CC.header; arg: t}

  val pp : F.formatter -> t -> unit

  val pp_ellipsis : F.formatter -> ellipsis -> unit

  val vars : t -> Var.t list
end

module Rule : sig
  type t = Regular of {lhs: Pattern.t; rhs: Pattern.t} | Ellipsis of Pattern.ellipsis

  val pp : F.formatter -> t -> unit

  exception FuelExhausted of {round_count: int}

  val full_rewrite : ?fuel:int -> CC.t -> t list -> int
  (** iterate rewriting until saturation.
      @raise FuelExhausted if fuel exhausted before saturation *)
end

val parse_pattern : CC.t -> string -> Pattern.t

val parse_rule : CC.t -> string -> Rule.t

module TestOnly : sig
  val e_match_pattern_at : ?debug:bool -> CC.t -> Pattern.t -> CC.Atom.t -> subst list

  val e_match_pattern : ?debug:bool -> CC.t -> Pattern.t -> f:(CC.Atom.t -> subst -> unit) -> unit

  val e_match_ellipsis_at : CC.t -> Pattern.ellipsis -> CC.Atom.t -> CC.Atom.t list

  val pattern_to_term : CC.t -> subst -> Pattern.t -> CC.Atom.t

  val apply_rule_at : ?debug:bool -> CC.t -> Rule.t -> CC.Atom.t -> int
  (** return the number of compatible substitution used during rewriting *)

  val rewrite_rules_once : ?debug:bool -> CC.t -> Rule.t list -> int
end
