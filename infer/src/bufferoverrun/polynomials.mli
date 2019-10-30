(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
end

module NonNegativeNonTopPolynomial : sig
  type t

  val get_symbols : t -> Bounds.NonNegativeBound.t list
end

module TopTraces : sig
  type t

  val make_err_trace : t -> Errlog.loc_trace
end

module NonNegativePolynomial : sig
  include PrettyPrintable.PrintableType

  type degree_with_term =
    (Degree.t * NonNegativeNonTopPolynomial.t, TopTraces.t) AbstractDomain.Types.below_above

  val pp_hum : Format.formatter -> t -> unit

  val leq : lhs:t -> rhs:t -> bool

  val top : t

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

  val subst : Typ.Procname.t -> Location.t -> t -> Bound.eval_sym -> t

  val degree : t -> Degree.t option

  val degree_str : t -> string

  val compare_by_degree : t -> t -> int

  val pp_degree : only_bigO:bool -> Format.formatter -> degree_with_term -> unit

  val polynomial_traces : t -> Errlog.loc_trace

  val encode : t -> string

  val decode : string -> t

  val get_degree_with_term : t -> degree_with_term
end
