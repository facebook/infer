(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Heap Formulas *)

open Fol

(** Quantifier-Free Symbolic Heap Formulas *)
module Sh : sig
  (** Segment of memory *)
  type seg =
    { loc: Term.t  (** location (address) where segment starts *)
    ; bas: Term.t  (** base address of enclosing allocation-block *)
    ; len: Term.t  (** length of enclosing allocation-block *)
    ; siz: Term.t  (** size of segment / length of the contents *)
    ; cnt: Term.t  (** contents of segment, a sequence / byte array *) }

  (** Quantifier-free symbolic heap formula *)
  type t [@@deriving sexp]

  (** Quantifier-free symbolic heap formula with fresh variables that are
      not yet named, which can be quantified by [Xsh.exists_fresh] and named
      by [Xsh.name_exists] *)
  type u = t Var.Fresh.m

  module Set : Set.S with type elt := t

  (** Pretty-print *)

  val pp_seg_norm : Context.t -> seg pp
  val pp : t pp
  val pp_raw : t pp
  val pp_diff_eq : Context.t -> t pp

  (** Access *)

  val ctx : t -> Context.t
  (** First-order logical context induced by rest of formula. *)

  val heap : t -> seg iter
  (** Star-conjunction of segment atomic formulas. *)

  val fv : ?ignore_ctx:unit -> ?ignore_pure:unit -> t -> Var.Set.t
  (** Free variables. *)

  (** Construct *)

  val emp : t
  (** Empty heap formula. *)

  val false_ : t
  (** Inconsistent formula. *)

  val seg : seg -> t
  (** Atomic segment formula. *)

  val star : t -> t -> u
  (** Star-conjunction of formulas. *)

  val starN : t list -> u
  (** Iterated star-conjunction of formulas. *)

  val or_ : t -> t -> t
  (** Disjunction of formulas. *)

  val orN : Set.t -> t
  (** Iterated disjunction of formulas. *)

  val pure : Formula.t -> u
  (** Atomic pure boolean constraint formula. *)

  val and_ : Formula.t -> t -> u
  (** Conjunction of a pure constraint to a symbolic heap formula. *)

  val andN : Formula.t list -> t -> u
  (** Iterated pure formula conjunction to a symbolic heap formula. *)

  val and_ctx : Context.t -> t -> u
  (** Conjunction of a context to that of a formula. *)

  val and_subst : Context.Subst.t -> t -> u
  (** Conjunction of a solution substitution to a formula. *)

  val rename : Var.Subst.t -> t -> u
  (** Apply a renaming substitution. *)

  (** Update *)

  val rem_seg : seg -> t -> t
  (** [star (seg s) (rem_seg s q)] is equivalent to [q], assuming that [s]
      is (physically equal to) one of the elements of [q.heap]. Raises if
      [s] is not an element of [q.heap]. *)

  val filter_heap : f:(seg -> bool) -> t -> t
  (** [filter_heap q f] is [q] without any segment for which [f] returns
      false. *)

  (** Simplify *)

  val norm : Context.Subst.t -> t -> u
  (** [norm s q] is [q] where subterms have been normalized with a
      substitution. *)

  (** Query *)

  val is_unsat : t -> bool Var.Fresh.m
  (** Holds only of inconsistent formulas, does not hold of all inconsistent
      formulas. *)

  val is_unsat_dnf : t -> bool Var.Fresh.m
  (** Holds only of inconsistent formulas, does not hold of all inconsistent
      formulas. Like [is_unsat] but more complete and expensive. *)

  val is_empty : t -> bool
  (** Holds only if all satisfying states have empty heap. Does not
      necessarily hold of all such formulas. *)

  val pure_approx : t -> Formula.t
  (** [pure_approx q] is inconsistent only if [q] is inconsistent. If
      [is_empty q], then [pure_approx q] is equivalent to
      [pure (pure_approx q)]. *)

  val dnf : t -> Set.t Var.Fresh.m
  (** Convert to disjunctive-normal form. *)
end

(** (Existentially-Quantified) Symbolic Heap Formulas *)
module Xsh : sig
  type t [@@deriving compare, equal, sexp]

  module Set : sig
    include Set.S with type elt := t

    val pp : t pp
    val pp_raw : t pp
  end

  (** Pretty-print *)

  val pp : t pp
  val pp_raw : t pp

  (** Access *)

  val vx : t -> Var.Context.t
  (** Variable context of formula *)

  val us : t -> Var.Set.t
  (** Vocabulary / variable context of formula *)

  val xs : t -> Var.Set.t
  (** Existentially-bound variables of formula *)

  val ctx : t -> Context.t
  (** First-order logical context induced by rest of formula *)

  val heap : t -> Sh.seg iter
  (** Star-conjunction of segment atomic formulas *)

  val fv : ?ignore_ctx:unit -> ?ignore_pure:unit -> t -> Var.Set.t
  (** Free variables, a subset of vocabulary. *)

  (** Construct *)

  val emp : t
  (** Empty heap formula. *)

  val false_ : t
  (** Inconsistent formula. *)

  val seg : Sh.seg -> t
  (** Atomic segment formula. *)

  val star : t -> t -> t
  (** Star-conjoin formulas, extending to a common vocabulary, and avoiding
      capturing existentials. *)

  val starN : t list -> t
  (** Star-conjoin formulas, extending to a common vocabulary, and avoiding
      capturing existentials. *)

  val or_ : t -> t -> t
  (** Disjoin formulas, extending to a common vocabulary, and avoiding
      capturing existentials. *)

  val orN : Set.t -> t
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

  val extend_voc : Var.Set.t -> t -> t
  (** Extend vocabulary, renaming existentials as needed. *)

  val exists : Var.Set.t -> t -> t
  (** Introduce existential quantifier, binding a set of variables required
      to already be in the vocabulary. *)

  val name_exists : t -> (Var.Set.t * Sh.t) * Var.Context.t
  (** Eliminate existential quantifier, naming/unbinding existentially-bound
      variables: [name_exists xq] is [((xs, q), voc)] where [voc] is a
      source of variables fresh with respect to the vocabulary of [xq], and
      [∃xs. q] is equivalent to [xq]. *)

  val exists_fresh : Sh.t -> t Var.Fresh.m
  (** Existentially quantify the existentials of the current variable
      context. *)

  val qf : Sh.t -> t Var.Fresh.m
  (** An existential symbolic heap with no bound variables. *)

  val freshen : t -> wrt:Var.Set.t -> t * Var.Subst.t
  (** Freshen free variables with respect to [wrt], extending vocabulary
      with the new variables. *)

  val rename : Var.Subst.t -> t -> t
  (** Apply a renaming substitution, remove its domain from vocabulary and
      add its range. *)

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

  val dnf : t -> t iter
  (** Convert to disjunctive-normal form. *)

  (**/**)

  val dump_simplify : int ref
  val replay : string -> unit
end

val do_normalize : bool ref
