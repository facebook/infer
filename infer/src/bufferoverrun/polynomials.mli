(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Bound = Bounds.Bound

module DegreeKind : sig
  type t = Linear | Log
end

module Degree : sig
  type t [@@deriving compare]

  val encode_to_int : t -> int
  (** Encodes the complex type [t] to an integer that can be used for comparison. *)

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

  val is_one : t -> bool

  val of_non_negative_bound : ?degree_kind:DegreeKind.t -> Bounds.NonNegativeBound.t -> t

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
