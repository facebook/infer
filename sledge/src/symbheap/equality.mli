(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Constraints representing equivalence relations over uninterpreted
    functions and linear rational arithmetic *)

type t [@@deriving compare, equal, sexp]

val pp : t pp
val pp_classes : t pp

include Invariant.S with type t := t

val true_ : t
(** The diagonal relation, which only equates each exp with itself. *)

val and_eq : Exp.t -> Exp.t -> t -> t
(** Conjoin an equation to a relation. *)

val and_ : t -> t -> t
(** Conjunction. *)

val or_ : t -> t -> t
(** Disjunction. *)

val rename : t -> Var.Subst.t -> t
(** Apply a renaming substitution to the relation. *)

val fv : t -> Var.Set.t
(** The variables occurring in the exps of the relation. *)

val is_true : t -> bool
(** Test if the relation is diagonal. *)

val is_false : t -> bool
(** Test if the relation is empty / inconsistent. *)

val entails_eq : t -> Exp.t -> Exp.t -> bool
(** Test if an equation is entailed by a relation. *)

val entails : t -> t -> bool
(** Test if one relation entails another. *)

val normalize : t -> Exp.t -> Exp.t
(** Normalize an exp [e] to [e'] such that [e = e'] is implied by the
    relation, where [e'] and its sub-exps are expressed in terms of the
    relation's canonical representatives of each equivalence-modulo-offset
    class. *)

val difference : t -> Exp.t -> Exp.t -> Z.t option
(** The difference as an offset. [difference r a b = Some k] if [r] implies
    [a = b+k], or [None] if [a] and [b] are not equal up to an integer
    offset. *)

val fold_exps : t -> init:'a -> f:('a -> Exp.t -> 'a) -> 'a
