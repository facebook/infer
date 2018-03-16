(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format

module Symbol : sig
  type t
end

module SubstMap : Caml.Map.S with type key = Symbol.t

module Bound : sig
  type t [@@deriving compare]

  val pp : F.formatter -> t -> unit

  val zero : t

  val is_const : t -> int option

  val is_not_infty : t -> bool

  val is_symbolic : t -> bool

  val get_one_symbol : t -> Symbol.t

  val gt : t -> t -> bool

  val le : t -> t -> bool

  val lt : t -> t -> bool
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

  val is_false : t -> bool

  val is_finite : t -> bool

  val is_invalid : t -> bool

  val is_lb_infty : t -> bool

  val is_nat : t -> bool

  val is_one : t -> bool

  val is_symbolic : t -> bool

  val is_top : t -> bool

  val is_true : t -> bool

  val is_zero : t -> bool

  val ( <= ) : lhs:t -> rhs:t -> bool

  val equal : t -> t -> bool

  val have_similar_bounds : t -> t -> bool

  val has_infty : t -> bool

  val make_positive : t -> t

  val join : t -> t -> t

  val le_sem : t -> t -> t

  val lt_sem : t -> t -> t

  val widen : prev:t -> next:t -> num_iters:int -> t

  val xcompare :
    lhs:t -> rhs:t
    -> [ `Equal
       | `LeftSmallerThanRight
       | `RightSmallerThanLeft
       | `NotComparable
       | `LeftSubsumesRight
       | `RightSubsumesLeft ]

  val get_symbols : t -> Symbol.t list

  val subst : t -> Bound.t bottom_lifted SubstMap.t -> t bottom_lifted
end

include module type of AbstractDomain.BottomLifted (ItvPure)

type t = astate [@@deriving compare]

val bot : t
(** _|_ *)

val false_sem : t
(** 0 *)

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

val unknown_bool : t
(** [0, 1] *)

val zero : t
(** 0 *)

val of_int : int -> t

val of_int_lit : IntLit.t -> t

val of_int64 : Int64.t -> t

val make_sym : ?unsigned:bool -> Typ.Procname.t -> (unit -> int) -> t

val lb : t -> Bound.t

val ub : t -> Bound.t

val is_false : t -> bool

val lnot : t -> t

val neg : t -> t

val normalize : t -> t

val get_symbols : t -> Symbol.t list

val eq : t -> t -> bool

val le : lhs:t -> rhs:t -> bool

val _range : t -> Bound.t

val div : t -> t -> t

val minus : t -> t -> t

val mult : t -> t -> t

val plus : t -> t -> t

val shiftlt : t -> t -> t

val shiftrt : t -> t -> t

val eq_sem : t -> t -> t

val ge_sem : t -> t -> t

val gt_sem : t -> t -> t

val land_sem : t -> t -> t

val le_sem : t -> t -> t

val lor_sem : t -> t -> t

val lt_sem : t -> t -> t

val min_sem : t -> t -> t

val mod_sem : t -> t -> t

val ne_sem : t -> t -> t

val prune_zero : t -> t

val prune_comp : Binop.t -> t -> t -> t

val prune_eq : t -> t -> t

val prune_ne : t -> t -> t

val subst : t -> Bound.t bottom_lifted SubstMap.t -> t
