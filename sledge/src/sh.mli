(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Heap Formulas *)

(** Segment of memory starting at [loc] containing [seq] (a byte-array) of
    size [siz], contained in an enclosing allocation-block starting at [bas]
    of length [len]. *)
type seg = {loc: Term.t; bas: Term.t; len: Term.t; siz: Term.t; seq: Term.t}

type starjunction = private
  { us: Var.Set.t  (** vocabulary / variable context of formula *)
  ; xs: Var.Set.t  (** existentially-bound variables *)
  ; cong: Equality.t  (** congruence induced by rest of formula *)
  ; pure: Term.t  (** pure boolean constraints *)
  ; heap: seg list  (** star-conjunction of segment atomic formulas *)
  ; djns: disjunction list  (** star-conjunction of disjunctions *) }

and disjunction = starjunction list

type t = starjunction [@@deriving compare, equal, sexp]

val pp_seg_norm : Equality.t -> seg pp
val pp_us : ?pre:('a, 'a) fmt -> ?vs:Var.Set.t -> unit -> Var.Set.t pp
val pp : t pp
val pp_raw : t pp
val pp_diff_eq : ?us:Var.Set.t -> ?xs:Var.Set.t -> Equality.t -> t pp
val pp_djn : disjunction pp
val simplify : t -> t

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

val pure : Term.t -> t
(** Atomic pure boolean constraint formula. *)

val and_ : Term.t -> t -> t
(** Conjoin a boolean constraint to a formula. *)

val and_cong : Equality.t -> t -> t
(** Conjoin constraints of a congruence to a formula, extending to a common
    vocabulary, and avoiding capturing existentials. *)

val and_subst : Equality.Subst.t -> t -> t
(** Conjoin constraints of a solution substitution to a formula, extending
    to a common vocabulary, and avoiding capturing existentials. *)

(** Update *)

val with_pure : Term.t -> t -> t
(** [with_pure pure q] is [{q with pure}], which assumes that [q.pure] and
    [pure] are defined in the same vocabulary. Note that [cong] is not
    weakened, so if [pure] and [q.pure] do not induce the same congruence,
    then the result will have a stronger [cong] than induced by its [pure]. *)

val rem_seg : seg -> t -> t
(** [star (seg s) (rem_seg s q)] is equivalent to [q], assuming that [s] is
    (physically equal to) one of the elements of [q.heap]. Raises if [s] is
    not an element of [q.heap]. *)

val filter_heap : f:(seg -> bool) -> t -> t
(** [filter_heap q f] Remove all segments in [q] for which [f] returns false *)

val norm : Equality.Subst.t -> t -> t
(** [norm s q] is [q] where subterms have been normalized with a
    substitution. *)

(** Quantification and Vocabulary *)

val exists : Var.Set.t -> t -> t
(** Existential quantification, binding variables thereby removing them from
    vocabulary. *)

val bind_exists : t -> wrt:Var.Set.t -> Var.Set.t * t
(** Bind existentials, freshened with respect to [wrt], extends vocabulary. *)

val rename : Var.Subst.t -> t -> t
(** Apply a substitution, remove its domain from vocabulary and add its
    range. *)

val subst : Var.Subst.t -> t -> t
(** Apply a substitution, remove its domain from vocabulary and add its
    range. *)

val freshen : t -> wrt:Var.Set.t -> t * Var.Subst.t
(** Freshen free variables with respect to [wrt], and extend vocabulary with
    [wrt], renaming bound variables as needed. *)

val extend_us : Var.Set.t -> t -> t
(** Extend vocabulary, renaming existentials as needed. *)

(** Query *)

val is_emp : t -> bool
(** Holds of [emp], with any vocabulary, existentials, and congruence. *)

val is_false : t -> bool
(** Holds only of inconsistent formulas, does not hold of all inconsistent
    formulas. *)

val fv : ?ignore_cong:unit -> t -> Var.Set.t
(** Free variables, a subset of vocabulary. *)

val pure_approx : t -> t
(** [pure_approx q] is inconsistent only if [q] is inconsistent. *)

val fold_dnf :
     conj:(starjunction -> 'conjuncts -> 'conjuncts)
  -> disj:(Var.Set.t * 'conjuncts -> 'disjuncts -> 'disjuncts)
  -> t
  -> Var.Set.t * 'conjuncts
  -> 'disjuncts
  -> 'disjuncts
(** Enumerate the cubes and clauses of a disjunctive-normal form expansion. *)

val dnf : t -> disjunction
(** Convert to disjunctive-normal form. *)
