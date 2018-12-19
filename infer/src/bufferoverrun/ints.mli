(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module NonZeroInt : sig
  type t = private Z.t [@@deriving compare]

  exception DivisionNotExact

  val one : t

  val minus_one : t

  val of_big_int : Z.t -> t option

  val opt_to_big_int : t option -> Z.t

  val is_one : t -> bool

  val is_minus_one : t -> bool

  val is_multiple : Z.t -> t -> bool

  val is_negative : t -> bool

  val is_positive : t -> bool

  val ( ~- ) : t -> t

  val ( * ) : t -> t -> t

  val plus : t -> t -> t option

  val exact_div_exn : t -> t -> t

  val max : t -> t -> t

  val min : t -> t -> t
end

module NonNegativeInt : sig
  type t = private Z.t [@@deriving compare]

  val zero : t

  val one : t

  val of_big_int : Z.t -> t option

  val of_int_exn : int -> t

  val of_big_int_exn : Z.t -> t

  val to_int_exn : t -> int

  val is_zero : t -> bool

  val is_one : t -> bool

  val ( <= ) : lhs:t -> rhs:t -> bool

  val succ : t -> t

  val log2_ceil_exn : t -> t

  val ( + ) : t -> t -> t

  val ( * ) : t -> t -> t

  val max : t -> t -> t

  val pp : F.formatter -> t -> unit
end

module PositiveInt : sig
  type t = private NonNegativeInt.t [@@deriving compare]

  val one : t

  val of_big_int : Z.t -> t option

  val succ : t -> t

  val pp_exponent : F.formatter -> t -> unit
end
