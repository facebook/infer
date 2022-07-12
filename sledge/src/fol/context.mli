(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Sets of assumptions, interpreted as conjunction, plus reasoning about
    their logical consequences.

    Functions that return contexts that might be stronger than their
    argument contexts accept and return a set of variables. The input set is
    the variables with which any generated variables must be chosen fresh,
    and the output set is the variables that have been generated. If the
    empty set is given, then no fresh variables are generated and equations
    that cannot be solved without generating fresh variables are dropped. *)

open Exp

type t [@@deriving sexp]

val pp : t pp

val ppx_diff :
  Var.strength -> Format.formatter -> t -> Formula.t -> t -> bool

include Invariant.S with type t := t

val empty : t
(** The empty context of assumptions. *)

val unsat : t
(** An unsatisfiable context of assumptions. *)

val add : Formula.t -> t -> t Var.Fresh.m
(** Add (that is, conjoin) an assumption to a context. *)

val union : t -> t -> t Var.Fresh.m
(** Union (that is, conjoin) two contexts of assumptions. *)

val inter : t -> t -> t Var.Fresh.m
(** Intersect two contexts of assumptions. Possibly weaker than logical
    disjunction. *)

val dnf : Formula.t -> ((Formula.t * t) * Var.Context.t) iter Var.Fresh.m
(** Disjunctive-normal form expansion. *)

val rename : t -> Var.Subst.t -> t
(** Apply a renaming substitution to the context. *)

val is_empty : t -> bool
(** Test if the context of assumptions is empty. *)

val is_unsat : t -> bool
(** Test if the context of assumptions is inconsistent. *)

val implies : t -> Formula.t -> bool
(** Holds only if a formula is a logical consequence of a context of
    assumptions. This only checks if the formula is valid in the current
    state of the context, without doing any further logical reasoning or
    propagation. *)

val refutes : t -> Formula.t -> bool
(** Holds only if a formula is inconsistent with a context of assumptions,
    that is, conjoining the formula to the assumptions is unsatisfiable. *)

val normalize : t -> Term.t -> Term.t
(** Normalize a term [e] to [e'] such that [e = e'] is implied by the
    assumptions, where [e'] and its subterms are expressed in terms of the
    canonical representatives of each equivalence class. *)

val fold_eqs : t -> 's -> f:(Formula.t -> 's -> 's) -> 's

val class_of : t -> Term.t -> Term.t list
(** Equivalence class of [e]: all the terms [f] in the context such that
    [e = f] is implied by the assumptions. *)

val vars : t -> Var.t iter
(** Enumerate the variables occurring in the terms of the context. *)

val fv : t -> Var.Set.t
(** The variables occurring in the terms of the context. *)

(** Solution Substitutions *)
module Subst : sig
  type t

  val pp : t pp
  val empty : t
  val compose : t -> t -> t
  val is_empty : t -> bool
  val fv : t -> Var.Set.t
  val fold_eqs : t -> 's -> f:(Formula.t -> 's -> 's) -> 's

  val subst : t -> Term.t -> Term.t
  (** Apply a substitution recursively to subterms. *)

  val partition_valid : Var.Set.t -> t -> t * Var.Set.t * t
  (** Partition ∃xs. σ into equivalent ∃xs. τ ∧ ∃ks. ν where ks and ν are
      maximal where ∃ks. ν is universally valid, xs ⊇ ks and ks ∩ fv(τ) = ∅. *)
end

val solve_for : Var.Set.t -> ?not_ito:Var.Set.t -> t -> Subst.t Var.Fresh.m
(** [solve_for vs ~not_ito:ws x] is a solution substitution that is implied
    by [x] and consists of oriented equalities [v ↦ e] that map terms [v]
    with free variables contained in [vs] to terms [e] with free variables
    disjoint from [ws]. *)

val apply_subst : Subst.t -> t -> t Var.Fresh.m
(** Context induced by applying a solution substitution to a set of
    equations generating the argument context. *)

val apply_and_elim :
  Var.Set.t -> Subst.t -> t -> (t * Var.Set.t) Var.Fresh.m
(** Apply a solution substitution to eliminate the solved variables. That
    is, [apply_and_elim vs s x] is [(x', ks)] where [x' ∧ ∃ks. s] is
    equivalent to [∃vs. x] where [ks ⊆ vs] and is maximal. *)

(**/**)

val pp_raw : t pp

val replay : string -> unit
(** Replay debugging *)
