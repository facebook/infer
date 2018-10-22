(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

exception Not_One_Symbol

module SymLinear : sig
  module M = Symb.SymbolMap

  type t = Ints.NonZeroInt.t M.t

  val make :
       unsigned:bool
    -> Typ.Procname.t
    -> Symb.SymbolTable.t
    -> Symb.SymbolPath.t
    -> Counter.t
    -> t * t

  val eq : t -> t -> bool

  val is_zero : t -> bool
end

module Bound : sig
  type sign = Plus | Minus

  type min_max = Min | Max

  type t =
    | MInf
    | Linear of Z.t * SymLinear.t
    | MinMax of Z.t * sign * min_max * Z.t * Symb.Symbol.t
    | PInf

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val pp : F.formatter -> t -> unit

  val of_int : int -> t

  val of_big_int : Z.t -> t

  val minus_one : t

  val _255 : t

  val of_sym : SymLinear.t -> t

  val is_symbolic : t -> bool

  val get_one_symbol : t -> Symb.Symbol.t

  val get_mone_symbol : t -> Symb.Symbol.t

  val is_one_symbol : t -> bool

  val is_mone_symbol : t -> bool

  val mk_MinMax : Z.t * sign * min_max * Z.t * Symb.Symbol.t -> t

  val big_int_lb : t -> Z.t sexp_option

  val big_int_ub : t -> Z.t sexp_option

  val le : t -> t -> bool

  val lt : t -> t -> bool

  val gt : t -> t -> bool

  val eq : t -> t -> bool

  val xcompare : t PartialOrder.xcompare

  val underapprox_min : t -> t -> t

  val overapprox_min : t -> t -> t

  val overapprox_max : t -> t -> t

  val widen_l : t -> t -> t

  val widen_u : t -> t -> t

  val zero : t

  val one : t

  val mone : t

  val is_zero : t -> bool

  val is_const : t -> Z.t sexp_option

  val plus_l : t -> t -> t

  val plus_u : t -> t -> t

  val mult_const_l : Ints.NonZeroInt.t -> t -> t

  val mult_const_u : Ints.NonZeroInt.t -> t -> t

  val neg : t -> t

  val div_const : t -> Ints.NonZeroInt.t -> t sexp_option

  val get_symbols : t -> Symb.Symbol.t list

  val are_similar : t -> t -> bool

  val is_not_infty : t -> bool

  val subst_lb :
       t
    -> (Symb.Symbol.t -> t AbstractDomain.Types.bottom_lifted)
    -> t AbstractDomain.Types.bottom_lifted

  val subst_ub :
       t
    -> (Symb.Symbol.t -> t AbstractDomain.Types.bottom_lifted)
    -> t AbstractDomain.Types.bottom_lifted

  val simplify_bound_ends_from_paths : t -> t

  val is_same_symbol : t -> t -> Symb.SymbolPath.t option
end

type ('c, 's) valclass = Constant of 'c | Symbolic of 's | ValTop

module NonNegativeBound : sig
  type t = Bound.t

  val zero : t

  val of_bound : t -> t

  val classify : t -> (Ints.NonNegativeInt.t, t) valclass
end
