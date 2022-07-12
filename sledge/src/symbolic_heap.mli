(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Heap Formulas *)

open Fol

module Sh : sig
  (** Segment of memory. *)
  type seg =
    { loc: Term.t  (** location (address) where segment starts *)
    ; bas: Term.t  (** base address of enclosing allocation-block *)
    ; len: Term.t  (** length of enclosing allocation-block *)
    ; siz: Term.t  (** size of segment / length of the contents *)
    ; cnt: Term.t  (** contents of segment, a sequence / byte array *) }
end

(** (Existentially-Quantified) Symbolic Heap Formulas *)
module Xsh : sig
  type t [@@deriving compare, equal, sexp]
  type disjunction

  module Set : Set.S with type elt := t and type t = disjunction

  (** Pretty-print *)

  val pp_seg_norm : Context.t -> Sh.seg pp
  val pp_us : Var.Set.t pp
  val pp : t pp
  val pp_raw : t pp
  val pp_diff_eq : ?us:Var.Set.t -> ?xs:Var.Set.t -> Context.t -> t pp
  val pp_djn : disjunction pp

  (** Access *)

  val us : t -> Var.Set.t
  (** Vocabulary / variable context of formula *)

  val xs : t -> Var.Set.t
  (** Existentially-bound variables of formula *)

  val ctx : t -> Context.t
  (** First-order logical context induced by rest of formula *)

  val heap : t -> Sh.seg iter
  (** Star-conjunction of segment atomic formulas *)

  val djns : t -> disjunction list
  (** Star-conjunction of disjunctions *)

  val fv : ?ignore_ctx:unit -> ?ignore_pure:unit -> t -> Var.Set.t
  (** Free variables, a subset of vocabulary. *)

  (** Construct *)

  val emp : t
  (** Empty heap formula. *)

  val false_ : Var.Set.t -> t
  (** Inconsistent formula with given vocabulary. *)

  val seg : Sh.seg -> t
  (** Atomic segment formula. *)

  val star : t -> t -> t
  (** Star-conjoin formulas, extending to a common vocabulary, and avoiding
      capturing existentials. *)

  val or_ : t -> t -> t
  (** Disjoin formulas, extending to a common vocabulary, and avoiding
      capturing existentials. *)

  val orN : disjunction -> t
  (** Disjoin formulas, extending to a common vocabulary, and avoiding
      capturing existentials. *)

  val pure : Formula.t -> t
  (** Atomic pure boolean constraint formula. *)

  val and_ : Formula.t -> t -> t
  (** Conjoin a boolean constraint to a formula. *)

  val andN : Formula.t list -> t -> t
  (** Conjoin boolean constraints to a formula. *)

  val and_ctx : Context.t -> t -> t
  (** Conjoin a context to that of a formula, extending to a common
      vocabulary, and avoiding capturing existentials. *)

  val and_subst : Context.Subst.t -> t -> t
  (** Conjoin constraints of a solution substitution to a formula, extending
      to a common vocabulary, and avoiding capturing existentials. *)

  (** Update *)

  val rem_seg : Sh.seg -> t -> t
  (** [star (seg s) (rem_seg s q)] is equivalent to [q], assuming that [s]
      is (physically equal to) one of the elements of [q.heap]. Raises if
      [s] is not an element of [q.heap]. *)

  val filter_heap : f:(Sh.seg -> bool) -> t -> t
  (** [filter_heap q f] Remove all segments in [q] for which [f] returns
      false *)

  (** Quantification and Vocabulary *)

  val extend_us : Var.Set.t -> t -> t
  (** Extend vocabulary, renaming existentials as needed. *)

  val exists : Var.Set.t -> t -> t
  (** Existential quantification, binding variables thereby removing them
      from vocabulary. *)

  val bind_exists : t -> wrt:Var.Set.t -> Var.Set.t * t
  (** Bind existentials, freshened with respect to [wrt], extends
      vocabulary. *)

  val freshen : t -> wrt:Var.Set.t -> t * Var.Subst.t
  (** Freshen free variables with respect to [wrt], and extend vocabulary
      with [wrt], renaming bound variables as needed. *)

  val rename : Var.Subst.t -> t -> t
  (** Apply a substitution, remove its domain from vocabulary and add its
      range. *)

  (** Simplify *)

  val norm : Context.Subst.t -> t -> t
  (** [norm s q] is [q] where subterms have been normalized with a
      substitution. *)

  val simplify : t -> t
  (** Transform a formula to an equivalent but simpler form. *)

  (** Query *)

  val is_unsat : t -> bool
  (** Holds only of inconsistent formulas, does not hold of all inconsistent
      formulas. *)

  val is_unsat_dnf : t -> bool
  (** Holds only of inconsistent formulas, does not hold of all inconsistent
      formulas. Like [is_unsat] but more complete and expensive. *)

  val is_empty : t -> bool
  (** Holds only if all satisfying states have empty heap. *)

  val pure_approx : t -> Formula.t
  (** [pure_approx q] is inconsistent only if [q] is inconsistent. If
      [is_empty q], then [pure_approx q] is equivalent to
      [pure (pure_approx q)]. *)

  val fold_dnf :
       conj:(t -> 'conjuncts -> 'conjuncts)
    -> disj:(Var.Set.t * 'conjuncts -> 'disjuncts -> 'disjuncts)
    -> t
    -> Var.Set.t * 'conjuncts
    -> 'disjuncts
    -> 'disjuncts
  (** Enumerate the cubes and clauses of a disjunctive-normal form
      expansion. *)

  val dnf : t -> disjunction
  (** Convert to disjunctive-normal form. *)

  val iter_dnf : t -> t iter

  (**/**)

  val dump_simplify : int ref
  val replay : string -> unit
end

val do_normalize : bool ref
