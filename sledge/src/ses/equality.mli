(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Constraints representing equivalence relations over uninterpreted
    functions and linear rational arithmetic.

    Functions that return relations that might be stronger than their
    argument relations accept and return a set of variables. The input set
    is the variables with which any generated variables must be chosen
    fresh, and the output set is the variables that have been generated. *)

type t [@@deriving compare, equal, sexp]
type classes = Term.t list Term.Map.t

val classes : t -> classes
(** [classes r] maps each equivalence class representative to the other
    terms [r] proves equal to it. *)

val pp : t pp
val pp_classes : t pp
val ppx_classes : Var.strength -> classes pp

include Invariant.S with type t := t

val true_ : t
(** The diagonal relation, which only equates each term with itself. *)

val and_eq : Var.Set.t -> Term.t -> Term.t -> t -> Var.Set.t * t
(** Conjoin an equation to a relation. *)

val and_term : Var.Set.t -> Term.t -> t -> Var.Set.t * t
(** Conjoin a (Boolean) term to a relation. *)

val and_ : Var.Set.t -> t -> t -> Var.Set.t * t
(** Conjunction. *)

val or_ : Var.Set.t -> t -> t -> Var.Set.t * t
(** Disjunction. *)

val orN : Var.Set.t -> t list -> Var.Set.t * t
(** Nary disjunction. *)

val rename : t -> (Var.t -> Var.t) -> t
(** Apply a renaming substitution to the relation. *)

val is_true : t -> bool
(** Test if the relation is diagonal. *)

val is_false : t -> bool
(** Test if the relation is empty / inconsistent. *)

val implies : t -> Term.t -> bool
(** Test if a term is implied by a relation. *)

val class_of : t -> Term.t -> Term.t list
(** Equivalence class of [e]: all the terms [f] in the relation such that
    [e = f] is implied by the relation. *)

val normalize : t -> Term.t -> Term.t
(** Normalize a term [e] to [e'] such that [e = e'] is implied by the
    relation, where [e'] and its subterms are expressed in terms of the
    relation's canonical representatives of each equivalence class. *)

val fold_vars : t -> 's -> f:(Var.t -> 's -> 's) -> 's

(** Solution Substitutions *)
module Subst : sig
  type t [@@deriving compare, equal, sexp]

  val pp : t pp
  val is_empty : t -> bool
  val fold : t -> 's -> f:(key:Term.t -> data:Term.t -> 's -> 's) -> 's

  val subst : t -> Term.t -> Term.t
  (** Apply a substitution recursively to subterms. *)

  val partition_valid : Var.Set.t -> t -> t * Var.Set.t * t
  (** Partition ∃xs. σ into equivalent ∃xs. τ ∧ ∃ks. ν where ks
      and ν are maximal where ∃ks. ν is universally valid, xs ⊇ ks and
      ks ∩ fv(τ) = ∅. *)
end

val apply_subst : Var.Set.t -> Subst.t -> t -> Var.Set.t * t
(** Relation induced by applying a substitution to a set of equations
    generating the argument relation. *)

val solve_for_vars : Var.Set.t list -> t -> Subst.t
(** [solve_for_vars vss r] is a solution substitution that is entailed by
    [r] and consists of oriented equalities [x ↦ e] that map terms [x]
    with free variables contained in (the union of) a prefix [uss] of [vss]
    to terms [e] with free variables contained in as short a prefix of [uss]
    as possible. *)

val elim : Var.Set.t -> t -> t
(** Weaken relation by removing oriented equations [k ↦ _] for [k] in
    [ks]. *)
