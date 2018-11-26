(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Bound = Bounds.Bound

module Degree : sig
  type t [@@deriving compare]

  val to_int : t -> int

  val is_zero : t -> bool

  val pp : Format.formatter -> t -> unit
end

module NonNegativePolynomial : sig
  include AbstractDomain.WithTop

  val zero : t

  val one : t

  val of_int_exn : int -> t

  val is_symbolic : t -> bool

  val is_top : t -> bool

  val is_zero : t -> bool

  val of_non_negative_bound : Bounds.NonNegativeBound.t -> t

  val plus : t -> t -> t

  val mult : t -> t -> t

  val min_default_left : t -> t -> t

  val subst : t -> Bound.eval_sym -> t

  val degree : t -> Degree.t option

  val compare_by_degree : t -> t -> int

  val pp_degree : Format.formatter -> t -> unit

  val pp_degree_hum : Format.formatter -> t -> unit

  val encode : t -> string

  val decode : string -> t
end
