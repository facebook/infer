(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Variables *)
module Var : sig
  type t [@@deriving compare, equal, sexp]
  type strength = t -> [`Universal | `Existential | `Anonymous] option

  val ppx : strength -> t pp
  val pp : t pp

  module Map : Map.S with type key := t

  module Set : sig
    include NS.Set.S with type elt := t

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
    val ppx : strength -> t pp
    val pp : t pp
    val pp_xs : t pp
  end

  val fresh : string -> wrt:Set.t -> t * Set.t

  val identified : name:string -> id:int -> t
  (** Variable with the given [id]. Variables are compared by [id] alone,
      [name] is used only for printing. The only way to ensure [identified]
      variables do not clash with [fresh] variables is to pass the
      [identified] variables to [fresh] in [wrt]:
      [Var.fresh name ~wrt:(Var.Set.of_ (Var.identified ~name ~id))]. *)

  val id : t -> int
  val name : t -> string

  module Subst : sig
    type var := t
    type t [@@deriving compare, equal, sexp]
    type x = {sub: t; dom: Set.t; rng: Set.t}

    val pp : t pp
    val empty : t
    val freshen : Set.t -> wrt:Set.t -> x * Set.t
    val invert : t -> t
    val restrict : t -> Set.t -> x
    val is_empty : t -> bool
    val domain : t -> Set.t
    val range : t -> Set.t
    val fold : t -> init:'a -> f:(var -> var -> 'a -> 'a) -> 'a
  end
end

(** Terms *)
module rec Term : sig
  type t [@@deriving compare, equal, sexp]

  (* pretty-printing *)
  val ppx : Var.strength -> t pp
  val pp : t pp

  module Map : Map.S with type key := t

  (** Construct *)

  (* variables *)
  val var : Var.t -> t

  (* constants *)
  val zero : t
  val one : t
  val integer : Z.t -> t
  val rational : Q.t -> t

  (* arithmetic *)
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mulq : Q.t -> t -> t
  val mul : t -> t -> t

  (* sequences *)
  val splat : t -> t
  val sized : seq:t -> siz:t -> t
  val extract : seq:t -> off:t -> len:t -> t
  val concat : t array -> t

  (* records *)
  val select : rcd:t -> idx:t -> t
  val update : rcd:t -> idx:t -> elt:t -> t

  (* tuples *)
  val tuple : t array -> t
  val project : ary:int -> idx:int -> t -> t

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
  val ppx : Var.strength -> t pp
  val pp : t pp

  (** Construct *)

  (* constants *)
  val tt : t
  val ff : t

  (* comparisons *)
  val eq : Term.t -> Term.t -> t
  val dq : Term.t -> Term.t -> t
  val lt : Term.t -> Term.t -> t
  val le : Term.t -> Term.t -> t

  (* connectives *)
  val not_ : t -> t
  val and_ : t -> t -> t
  val andN : t list -> t
  val or_ : t -> t -> t
  val orN : t list -> t
  val cond : cnd:t -> pos:t -> neg:t -> t

  (** Query *)

  val fv : t -> Var.Set.t
  val is_true : t -> bool
  val is_false : t -> bool

  (** Traverse *)

  val fold_vars : init:'a -> t -> f:('a -> Var.t -> 'a) -> 'a

  (** Transform *)

  val map_vars : f:(Var.t -> Var.t) -> t -> t

  val fold_map_vars :
    init:'a -> t -> f:('a -> Var.t -> 'a * Var.t) -> 'a * t

  val rename : Var.Subst.t -> t -> t
  val disjuncts : t -> t list
end

(** Inference System *)
module Context : sig
  type t [@@deriving sexp]

  val pp : t pp
  val pp_classes : t pp

  val ppx_diff :
    Var.strength -> Format.formatter -> t -> Formula.t -> t -> bool

  include Invariant.S with type t := t

  val true_ : t
  (** The diagonal relation, which only equates each term with itself. *)

  val and_formula : Var.Set.t -> Formula.t -> t -> Var.Set.t * t
  (** Conjoin a (Boolean) term to a relation. *)

  val and_ : Var.Set.t -> t -> t -> Var.Set.t * t
  (** Conjunction. *)

  val orN : Var.Set.t -> t list -> Var.Set.t * t
  (** Nary disjunction. *)

  val rename : t -> Var.Subst.t -> t
  (** Apply a renaming substitution to the relation. *)

  val is_true : t -> bool
  (** Test if the relation is diagonal. *)

  val is_false : t -> bool
  (** Test if the relation is empty / inconsistent. *)

  val implies : t -> Formula.t -> bool
  (** [implies x f] holds only if [f] is a logical consequence of [x]. This
      only checks if [f] is valid in the current state of [x], without doing
      any further logical reasoning or propagation. *)

  val refutes : t -> Formula.t -> bool
  (** [refutes x f] holds only if [f] is inconsistent with [x]. *)

  val class_of : t -> Term.t -> Term.t list
  (** Equivalence class of [e]: all the terms [f] in the relation such that
      [e = f] is implied by the relation. *)

  val normalize : t -> Term.t -> Term.t
  (** Normalize a term [e] to [e'] such that [e = e'] is implied by the
      relation, where [e'] and its subterms are expressed in terms of the
      relation's canonical representatives of each equivalence class. *)

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

    val substf : t -> Formula.t -> Formula.t

    val partition_valid : Var.Set.t -> t -> t * Var.Set.t * t
    (** Partition ∃xs. σ into equivalent ∃xs. τ ∧ ∃ks. ν where ks
        and ν are maximal where ∃ks. ν is universally valid, xs ⊇ ks
        and ks ∩ fv(τ) = ∅. *)
  end

  val apply_subst : Var.Set.t -> Subst.t -> t -> Var.Set.t * t
  (** Relation induced by applying a substitution to a set of equations
      generating the argument relation. *)

  val solve_for_vars : Var.Set.t list -> t -> Subst.t
  (** [solve_for_vars vss r] is a solution substitution that is entailed by
      [r] and consists of oriented equalities [x ↦ e] that map terms [x]
      with free variables contained in (the union of) a prefix [uss] of
      [vss] to terms [e] with free variables contained in as short a prefix
      of [uss] as possible. *)

  val elim : Var.Set.t -> t -> t
  (** Weaken relation by removing oriented equations [k ↦ _] for [k] in
      [ks]. *)

  (* Replay debugging *)

  val replay : string -> unit
end

(** Convert from Llair *)

module Var_of_Llair : sig
  val reg : Llair.Reg.t -> Var.t
  val regs : Llair.Reg.Set.t -> Var.Set.t
end

module Term_of_Llair : sig
  val exp : Llair.Exp.t -> Term.t
end

module Formula_of_Llair : sig
  val exp : Llair.Exp.t -> Formula.t
end
