(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Heap Formulas *)

(** Segment of memory starting at [loc] containing a byte-array [arr] of
    size [siz], contained in an enclosing allocation-block starting at [bas]
    of length [len]. Byte-array expressions are either [Var]-iables or
    [Splat] vectors. *)
type seg = {loc: Exp.t; bas: Exp.t; len: Exp.t; siz: Exp.t; arr: Exp.t}

type starjunction = private
  { us: Var.Set.t  (** vocabulary / variable context of formula *)
  ; xs: Var.Set.t  (** existentially-bound variables *)
  ; cong: Congruence.t  (** congruence induced by rest of formula *)
  ; pure: Exp.t list  (** conjunction of pure boolean constraints *)
  ; heap: seg list  (** star-conjunction of segment atomic formulas *)
  ; djns: disjunction list  (** star-conjunction of disjunctions *) }

and disjunction = starjunction list

type t = starjunction

val pp_seg : seg pp
val pp_us : ?pre:('a, 'a) fmt -> Var.Set.t pp
val pp : t pp

include Invariant.S with type t := t

(** Construct *)

val emp : t
(** Empty heap formula. *)

val false_ : Var.Set.t -> t
(** Inconsistent formula with given vocabulary. *)

val seg : seg -> t
(** Atomic segment formula. *)

val star : t -> t -> t
(** Star-conjoin formulas, extending to a common vocabulary, and avoiding
    capturing existentials. *)

val or_ : t -> t -> t
(** Disjoin formulas, extending to a common vocabulary, and avoiding
    capturing existentials. *)

val pure : Exp.t -> t
(** Atomic pure boolean constraint formula. *)

val and_ : Exp.t -> t -> t
(** Conjoin a boolean constraint to a formula. *)

val and_cong : Congruence.t -> t -> t
(** Conjoin constraints of a congruence to a formula, extending to a common
    vocabulary, and avoiding capturing existentials. *)

(** Update *)

val with_pure : Exp.t list -> t -> t
(** [with_pure pure q] is [{q with pure}], which assumes that [q.pure] and
    [pure] are defined in the same vocabulary, induce the same congruence,
    etc. It can essentially only be used when [pure] is logically equivalent
    to [q.pure], but perhaps syntactically simpler. *)

val rem_seg : seg -> t -> t
(** [star (seg s) (rem_seg s q)] is equivalent to [q], assuming that [s] is
    (physically equal to) one of the elements of [q.heap]. Raises if [s] is
    not an element of [q.heap]. *)

(** Quantification and Vocabulary *)

val exists : Var.Set.t -> t -> t
(** Existential quantification, binding variables thereby removing them from
    vocabulary. *)

val bind_exists : t -> wrt:Var.Set.t -> Var.Set.t * t
(** Bind existentials, freshened with respect to [wrt], extends vocabulary. *)

val rename : Var.Subst.t -> t -> t
(** Apply a substitution, remove its domain from vocabulary and add its
    range. *)

val freshen : wrt:Var.Set.t -> t -> t * Var.Subst.t
(** Freshen free variables with respect to [wrt], and extend vocabulary with
    [wrt], renaming bound variables as needed. *)

val extend_us : Var.Set.t -> t -> t
(** Extend vocabulary, renaming existentials as needed. *)

(** Query *)

val is_emp : t -> bool
(** Holds of [emp]. *)

val is_false : t -> bool
(** Holds only of inconsistent formulas, does not hold of all inconsistent
    formulas. *)

val fv : t -> Var.Set.t
(** Free variables, a subset of vocabulary. *)

val pure_approx : t -> t
(** [pure_approx q] is inconsistent only if [q] is inconsistent. *)

val fold_dnf :
     conj:(starjunction -> 'conjuncts -> 'conjuncts)
  -> disj:('conjuncts -> 'disjuncts -> 'disjuncts)
  -> t
  -> 'conjuncts
  -> 'disjuncts
  -> 'disjuncts
(** Enumerate the cubes and clauses of a disjunctive-normal form expansion. *)

val dnf : t -> disjunction
(** Convert to disjunctive-normal form. *)
