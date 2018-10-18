(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format
module Bound = Bounds.Bound
module Counter = Counter

module Boolean : sig
  type t

  val top : t

  val true_ : t

  val equal : t -> t -> bool

  val is_false : t -> bool

  val is_true : t -> bool
end

module Symbol = Symb.Symbol
module SymbolPath = Symb.SymbolPath
module SymbolTable = Symb.SymbolTable

module NonNegativePolynomial : sig
  include AbstractDomain.WithTop

  val zero : astate

  val one : astate

  val of_int_exn : int -> astate

  val is_symbolic : astate -> bool

  val is_top : astate -> bool

  val is_zero : astate -> bool

  val plus : astate -> astate -> astate

  val mult : astate -> astate -> astate

  val min_default_left : astate -> astate -> astate

  val subst : astate -> (Symb.Symbol.t -> Bound.t AbstractDomain.Types.bottom_lifted) -> astate

  val degree : astate -> int option

  val compare_by_degree : astate -> astate -> int

  val pp_degree : Format.formatter -> astate -> unit

  val encode : astate -> string

  val decode : string -> astate
end

module ItvRange : sig
  type t

  val to_top_lifted_polynomial : t -> NonNegativePolynomial.astate
end

module ItvPure : sig
  type astate [@@deriving compare]

  type t = astate

  val pp : F.formatter -> t -> unit

  val mone : t

  val zero : t

  val of_int : int -> t

  val lb : t -> Bound.t

  val ub : t -> Bound.t

  val is_finite : t -> bool

  val is_invalid : t -> bool

  val is_lb_infty : t -> bool

  val is_nat : t -> bool

  val is_symbolic : t -> bool

  val is_top : t -> bool

  val ( <= ) : lhs:t -> rhs:t -> bool

  val have_similar_bounds : t -> t -> bool

  val has_infty : t -> bool

  val make_positive : t -> t

  val join : t -> t -> t

  val le_sem : t -> t -> Boolean.t

  val lt_sem : t -> t -> Boolean.t

  val widen : prev:t -> next:t -> num_iters:int -> t

  val xcompare :
       lhs:t
    -> rhs:t
    -> [ `Equal
       | `LeftSmallerThanRight
       | `RightSmallerThanLeft
       | `NotComparable
       | `LeftSubsumesRight
       | `RightSubsumesLeft ]

  val get_symbols : t -> Symbol.t list

  val subst : t -> (Symb.Symbol.t -> Bound.t bottom_lifted) -> t bottom_lifted

  val plus : t -> t -> t

  val minus : t -> t -> t

  val mult : t -> t -> t
end

include module type of AbstractDomain.BottomLifted (ItvPure)

type t = astate [@@deriving compare]

val bot : t
(** _|_ *)

val m1_255 : t
(** [-1, 255] *)

val nat : t
(** [0, +oo] *)

val one : t
(** 1 *)

val pos : t
(** [1, +oo] *)

val top : t
(** [-oo, +oo] *)

val zero : t
(** 0 *)

val get_iterator_itv : t -> t

val of_bool : Boolean.t -> t

val of_int : int -> t

val of_big_int : Z.t -> t

val of_int_lit : IntLit.t -> t

val of_int64 : Int64.t -> t

val make_sym :
  ?unsigned:bool -> Typ.Procname.t -> Symb.SymbolTable.t -> Symb.SymbolPath.t -> Counter.t -> t

val lb : t -> Bound.t

val ub : t -> Bound.t

val is_false : t -> bool

val neg : t -> t

val normalize : t -> t

val get_symbols : t -> Symbol.t list

val eq : t -> t -> bool

val le : lhs:t -> rhs:t -> bool

val lnot : t -> Boolean.t

val range : t -> ItvRange.t

val div : t -> t -> t

val minus : t -> t -> t

val mult : t -> t -> t

val plus : t -> t -> t

val shiftlt : t -> t -> t

val shiftrt : t -> t -> t

val eq_sem : t -> t -> Boolean.t

val ge_sem : t -> t -> Boolean.t

val gt_sem : t -> t -> Boolean.t

val land_sem : t -> t -> Boolean.t

val le_sem : t -> t -> Boolean.t

val lor_sem : t -> t -> Boolean.t

val lt_sem : t -> t -> Boolean.t

val min_sem : t -> t -> t

val mod_sem : t -> t -> t

val ne_sem : t -> t -> Boolean.t

val prune_eq_zero : t -> t

val prune_ne_zero : t -> t

val prune_comp : Binop.t -> t -> t -> t

val prune_eq : t -> t -> t

val prune_ne : t -> t -> t

val subst : t -> (Symb.Symbol.t -> Bound.t bottom_lifted) -> t
