(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ses

(** Variables *)
module Var : Var_intf.VAR

(** Terms *)
module rec Term : sig
  type t [@@deriving compare, equal, sexp]

  (* pretty-printing *)
  val ppx : Var.t Var.strength -> t pp
  val pp : t pp

  module Map : Map.S with type key := t

  (** Construct *)

  (* variables *)
  val var : Var.t -> t

  (* arithmetic *)
  val zero : t
  val one : t
  val integer : Z.t -> t
  val rational : Q.t -> t
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mulq : Q.t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val pow : t -> int -> t

  (* sequences (of flexible size) *)
  val splat : t -> t
  val sized : seq:t -> siz:t -> t
  val extract : seq:t -> off:t -> len:t -> t
  val concat : t array -> t

  (* records (with fixed indices) *)
  val select : rcd:t -> idx:int -> t
  val update : rcd:t -> idx:int -> elt:t -> t
  val record : t array -> t
  val ancestor : int -> t

  (* uninterpreted *)
  val apply : Funsym.t -> t array -> t

  (* if-then-else *)
  val ite : cnd:Formula.t -> thn:t -> els:t -> t

  (** Destruct *)

  val d_int : t -> Z.t option

  (** Access *)

  val const_of : t -> Q.t option

  (** Query *)

  val fv : t -> Var.Set.t

  (** Traverse *)

  val fold_vars : init:'a -> t -> f:('a -> Var.t -> 'a) -> 'a

  (** Transform *)

  val map_vars : f:(Var.t -> Var.t) -> t -> t

  val fold_map_vars :
    t -> init:'a -> f:('a -> Var.t -> 'a * Var.t) -> 'a * t

  val rename : Var.Subst.t -> t -> t
end

(** Formulas *)
and Formula : sig
  type t [@@deriving compare, equal, sexp]

  val inject : t -> Term.t
  val project : Term.t -> t option

  (* pretty-printing *)
  val ppx : Var.t Var.strength -> t pp
  val pp : t pp

  (** Construct *)

  (* constants *)
  val tt : t
  val ff : t

  (* equality *)
  val eq : Term.t -> Term.t -> t
  val dq : Term.t -> Term.t -> t

  (* arithmetic *)
  val eq0 : Term.t -> t
  val dq0 : Term.t -> t
  val gt0 : Term.t -> t
  val ge0 : Term.t -> t
  val lt0 : Term.t -> t
  val le0 : Term.t -> t
  val gt : Term.t -> Term.t -> t
  val ge : Term.t -> Term.t -> t
  val lt : Term.t -> Term.t -> t
  val le : Term.t -> Term.t -> t

  (* uninterpreted *)
  val lit : Predsym.t -> Term.t array -> t

  (* connectives *)
  val not_ : t -> t
  val and_ : t -> t -> t
  val andN : t list -> t
  val or_ : t -> t -> t
  val orN : t list -> t
  val iff : t -> t -> t
  val xor : t -> t -> t
  val cond : cnd:t -> pos:t -> neg:t -> t

  (** Query *)

  val fv : t -> Var.Set.t

  (** Traverse *)

  val fold_vars : init:'a -> t -> f:('a -> Var.t -> 'a) -> 'a

  (** Transform *)

  val map_terms : f:(Term.t -> Term.t) -> t -> t
  val map_vars : f:(Var.t -> Var.t) -> t -> t

  val fold_map_vars :
    init:'a -> t -> f:('a -> Var.t -> 'a * Var.t) -> 'a * t

  val rename : Var.Subst.t -> t -> t
end

(** Sets of assumptions, interpreted as conjunction, plus reasoning about
    their logical consequences. *)
module Context : sig
  type t [@@deriving sexp]

  val pp : t pp

  val ppx_diff :
    Var.t Var.strength -> Format.formatter -> t -> Formula.t -> t -> bool

  include Invariant.S with type t := t

  val empty : t
  (** The empty context of assumptions. *)

  val add : Var.Set.t -> Formula.t -> t -> Var.Set.t * t
  (** Add (that is, conjoin) an assumption to a context. *)

  val union : Var.Set.t -> t -> t -> Var.Set.t * t
  (** Union (that is, conjoin) two contexts of assumptions. *)

  val inter : Var.Set.t -> t -> t -> Var.Set.t * t
  (** Intersect (that is, disjoin) contexts of assumptions. *)

  val interN : Var.Set.t -> t list -> Var.Set.t * t
  (** Intersect contexts of assumptions. Possibly weaker than logical
      disjunction. *)

  val dnf : Formula.t -> (Var.Set.t * Formula.t * t) iter
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

  val class_of : t -> Term.t -> Term.t list
  (** Equivalence class of [e]: all the terms [f] in the context such that
      [e = f] is implied by the assumptions. *)

  val fold_vars : init:'a -> t -> f:('a -> Var.t -> 'a) -> 'a
  (** Enumerate the variables occurring in the terms of the context. *)

  val fv : t -> Var.Set.t
  (** The variables occurring in the terms of the context. *)

  (** Solution Substitutions *)
  module Subst : sig
    type t

    val pp : t pp
    val is_empty : t -> bool

    val fold :
      t -> init:'a -> f:(key:Term.t -> data:Term.t -> 'a -> 'a) -> 'a

    val subst : t -> Term.t -> Term.t
    (** Apply a substitution recursively to subterms. *)

    val partition_valid : Var.Set.t -> t -> t * Var.Set.t * t
    (** Partition ∃xs. σ into equivalent ∃xs. τ ∧ ∃ks. ν where ks
        and ν are maximal where ∃ks. ν is universally valid, xs ⊇ ks
        and ks ∩ fv(τ) = ∅. *)
  end

  val apply_subst : Var.Set.t -> Subst.t -> t -> Var.Set.t * t
  (** Context induced by applying a solution substitution to a set of
      equations generating the argument context. *)

  val solve_for_vars : Var.Set.t list -> t -> Subst.t
  (** [solve_for_vars vss x] is a solution substitution that is implied by
      [x] and consists of oriented equalities [v ↦ e] that map terms [v]
      with free variables contained in (the union of) a prefix [uss] of
      [vss] to terms [e] with free variables contained in as short a prefix
      of [uss] as possible. *)

  val elim : Var.Set.t -> t -> t
  (** [elim ks x] is [x] weakened by removing oriented equations [k ↦ _]
      for [k] in [ks]. *)

  (**/**)

  val pp_raw : t pp

  val replay : string -> unit
  (** Replay debugging *)
end
