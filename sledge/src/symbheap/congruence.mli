(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Constraints representing congruence relations *)

type t [@@deriving compare, sexp]

val pp : t pp
val pp_classes : t pp

include Invariant.S with type t := t

(** If [prefer a ~over:b] is positive, then [b] will not be used as the
    representative of a class containing [a] and [b]. Similarly, if [prefer
    a ~over:b] is negative, then [a] will not be used as the representative
    of a class containing [a] and [b]. Otherwise the choice of
    representative is unspecified, and made to be most efficient. *)
type prefer = Exp.t -> over:Exp.t -> int

val true_ : t
(** The diagonal relation, which only equates each exp with itself. *)

val extend : t -> Exp.t -> t
(** Extend the carrier of the relation. *)

val merge : ?prefer:prefer -> t -> Exp.t -> Exp.t -> t
(** Merge the equivalence classes of exps together. If [prefer a ~over:b] is
    positive, then [b] will not be used as the representative of a class
    containing [a] and [b]. *)

val and_eq : ?prefer:prefer -> t -> Exp.t -> Exp.t -> t

val and_ : ?prefer:prefer -> t -> t -> t
(** Conjunction. *)

val or_ : ?prefer:prefer -> t -> t -> t
(** Disjunction. *)

val rename : t -> Var.Subst.t -> t
(** Apply a renaming substitution to the relation. *)

val fv : t -> Var.Set.t
(** The variables occurring in the exps of the relation. *)

val is_true : t -> bool
(** Test if the relation is diagonal. *)

val is_false : t -> bool
(** Test if the relation is empty / inconsistent. *)

val classes : t -> (Exp.t, Exp.t list, Exp.comparator_witness) Map.t
val entails : t -> t -> bool

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
