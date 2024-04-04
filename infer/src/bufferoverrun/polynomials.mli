(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module DegreeKind : sig
  type t = Linear | Log
end

module Degree : sig
  type t [@@deriving compare]

  val encode_to_int : t -> int
  (** Encodes the complex type [t] to an integer that can be used for comparison. *)

  val is_constant : t -> bool
end

module NonNegativeNonTopPolynomial : sig
  type t

  val polynomial_traces : t -> (string * Errlog.loc_trace) list
end

module TopTraces : sig
  type t

  val make_err_trace : t -> Errlog.loc_trace
end

module UnreachableTraces : sig
  type t

  val make_err_trace : t -> Errlog.loc_trace
end

module NonNegativePolynomial : sig
  include PrettyPrintable.PrintableType

  type degree_with_term =
    ( UnreachableTraces.t
    , Degree.t * NonNegativeNonTopPolynomial.t
    , TopTraces.t )
    AbstractDomain.Types.below_above

  val pp_hum : Format.formatter -> t -> unit

  val leq : lhs:t -> rhs:t -> bool

  val top : t

  val of_unreachable : Location.t -> t

  val zero : t

  val one : t

  val of_int_exn : int -> t

  val is_symbolic : t -> bool

  val is_top : t -> bool

  val is_unreachable : t -> bool

  val is_zero : t -> bool

  val is_one : t -> bool

  val of_non_negative_bound : ?degree_kind:DegreeKind.t -> Bounds.NonNegativeBound.t -> t

  val of_func_ptr : Symb.SymbolPath.partial -> t

  val plus : t -> t -> t

  val mult_unreachable : t -> t -> t
  (** if one of the operands is unreachable, the result is unreachable *)

  val mult : t -> t -> t

  val mult_loop : iter:t -> body:t -> t

  val min_default_left : t -> t -> t

  val subst :
       Procname.t
    -> Location.t
    -> t
    -> Bounds.Bound.eval_sym
    -> FuncPtr.Set.eval_func_ptrs
    -> (Procname.t -> t option)
    -> default_closure_cost:Ints.NonNegativeInt.t
    -> t

  val degree : t -> Degree.t option

  val degree_str : t -> string

  val compare_by_degree : t -> t -> int

  val pp_degree : only_bigO:bool -> Format.formatter -> degree_with_term -> unit

  val polynomial_traces : t -> Errlog.loc_trace

  val encode : t -> string

  val decode : string -> t

  val get_degree_with_term : t -> degree_with_term
end
