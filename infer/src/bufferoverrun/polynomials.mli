(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Bound = Bounds.Bound

module NonNegativePolynomial : sig
  include AbstractDomain.WithTop

  val zero : astate

  val one : astate

  val of_int_exn : int -> astate

  val is_symbolic : astate -> bool

  val is_top : astate -> bool

  val is_zero : astate -> bool

  val of_non_negative_bound : Bounds.NonNegativeBound.t -> astate

  val plus : astate -> astate -> astate

  val mult : astate -> astate -> astate

  val min_default_left : astate -> astate -> astate

  val subst : astate -> Bound.eval_sym -> astate

  val degree : astate -> int option

  val compare_by_degree : astate -> astate -> int

  val pp_degree : Format.formatter -> astate -> unit

  val pp_degree_hum : Format.formatter -> astate -> unit

  val encode : astate -> string

  val decode : string -> astate
end
