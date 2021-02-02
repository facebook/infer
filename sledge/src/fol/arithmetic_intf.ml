(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Arithmetic terms *)

module type S = sig
  type var
  type trm
  type t [@@deriving compare, equal, sexp]

  val ppx : var Var_intf.strength -> t pp

  (** Construct and Destruct atomic terms *)

  val const : Q.t -> t
  (** [const q] represents the constant [q] *)

  val get_const : t -> Q.t option
  (** [get_const a] is [Some q] iff [equal a (const q)] *)

  val trm : trm -> t
  (** [trm x] represents the indeterminate term [x] *)

  val get_trm : t -> trm option
  (** [get_trm a] is [Some x] iff [equal a (trm x)] *)

  type kind = Trm of trm | Const of Q.t | Interpreted | Uninterpreted

  val classify : t -> kind
  (** [classify a] is [Trm x] iff [get_trm a] is [Some x], [Const q] iff
      [get_const a] is [Some q], [Interpreted] if the principal operation of
      [a] is interpreted, and [Uninterpreted] otherwise. *)

  val is_uninterpreted : t -> bool
  (** [is_uninterpreted a] iff [classify a = Uninterpreted] *)

  (** Construct compound terms *)

  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mulc : Q.t -> t -> t
  val mul : trm -> trm -> t
  val div : trm -> trm -> t
  val pow : trm -> int -> t

  (** Transform *)

  val split_const : t -> t * Q.t
  (** Splits an arithmetic term into the sum of its constant and
      non-constant parts. That is, [split_const a] is [(b, c)] such that
      [a = b + c] and the absolute value of [c] is maximal. *)

  val partition_sign : t -> t * t
  (** [partition_sign a] is [(p, n)] such that [a] = [p - n] and all
      coefficients in [p] and [n] are non-negative. *)

  (** Traverse *)

  val trms : t -> trm iter
  (** [trms a] is the maximal foreign or noninterpreted proper subterms of
      [a]. Considering an arithmetic term as a polynomial,
      [trms (c × (Σᵢ₌₁ⁿ cᵢ × Πⱼ₌₁ᵐᵢ
      Xᵢⱼ^pᵢⱼ))] is the sequence of monomials
      [Πⱼ₌₁ᵐᵢ Xᵢⱼ^pᵢⱼ] for each [i]. If the arithmetic
      term is a monomial, [trms (Πⱼ₌₁ᵐ Xⱼ^pⱼ)] is the sequence
      of factors [Xⱼ] for each [j]. *)

  val map : t -> f:(trm -> trm) -> t
  (** Map over the {!trms}. *)

  (** Solve *)

  val solve_zero_eq : ?for_:trm -> trm -> (t * t) option
  (** [solve_zero_eq d] is [Some (e, f)] if [0 = d] can be equivalently
      expressed as [e = f] for some monomial subterm [e] of [d]. If [for_]
      is passed, then the subterm [e] must be [for_]. *)
end

(** Indeterminate terms, treated as atomic / variables except when they can
    be flattened using [EMBEDDING.get_arith]. *)
module type INDETERMINATE = sig
  type trm [@@deriving compare, equal, sexp]
  type var

  val ppx : var Var_intf.strength -> trm pp
  val pp : trm pp
  val vars : trm -> var iter
end

(** An embedding of arithmetic terms [t] into indeterminates [trm]. *)
module type EMBEDDING = sig
  type trm
  type t

  val to_trm : t -> trm
  (** Embedding from [t] to [trm]: [to_trm a] is arithmetic term [a]
      embedded in an indeterminate term. *)

  val get_arith : trm -> t option
  (** Partial projection from [trm] to [t]: [get_arith x] is [Some a] if
      [x = to_trm a]. This is used to flatten indeterminates that are
      actually arithmetic for the client, thereby enabling arithmetic
      operations to be interpreted more often. *)
end

(** A type [t] representing arithmetic terms over indeterminates [trm]
    together with a functor [Make] that takes an [EMBEDDING] of arithmetic
    terms [t] into indeterminates [trm] and produces an implementation of
    the primary interface [S]. *)
module type REPRESENTATION = sig
  type t [@@deriving compare, equal, sexp]
  type var
  type trm

  module Make (_ : EMBEDDING with type trm := trm and type t := t) :
    S with type var := var with type trm := trm with type t := t
end
