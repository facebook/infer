(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractValue = PulseAbstractValue

(** {2 Arithmetic solver}

    Build formulas from SIL and tries to decide if they are (mostly un-)satisfiable. *)

module Term : sig
  (** Similar to {!Exp.t} but with no memory operations and with {!AbstractValue.t} instead of SIL
      variables. The rich structure allows us to represent all of SIL but is not a promise that we
      are able to meaningfully reason about all of it. *)
  type t

  val zero : t

  val of_absval : AbstractValue.t -> t

  val of_intlit : IntLit.t -> t

  val of_binop : Binop.t -> t -> t -> t

  val of_unop : Unop.t -> t -> t
end

type t

val pp : F.formatter -> t -> unit

val pp_with_pp_var : (F.formatter -> AbstractValue.t -> unit) -> F.formatter -> t -> unit
  [@@warning "-32"]
(** only used for unit tests *)

(** {3 Build formulas from non-formulas} *)

val ttrue : t

val of_term_binop : Binop.t -> Term.t -> Term.t -> t

val mk_equal : Term.t -> Term.t -> t

val mk_less_equal : Term.t -> Term.t -> t

val mk_less_than : Term.t -> Term.t -> t

(** {3 Combine formulas} *)

val aand : t -> t -> t

val nnot : t -> t

(** {3 Operations} *)

val simplify : keep:AbstractValue.Set.t -> t -> t

val fold_map_variables : t -> init:'a -> f:('a -> AbstractValue.t -> 'a * AbstractValue.t) -> 'a * t

val is_literal_false : t -> bool
(** Call [is_literal_false (normalize phi)] to check satisfiability. *)

val normalize : t -> t
(** Produces a semantically-equivalent formula¹ where all consequences of equalities have been
    applied and some ad-hoc arithmetic and logical reasoning has been performed. In particular, the
    canonical representation of a known-false formula is [ffalse], and [is_literal_false ffalse] is
    [true]. Probably a good idea to not throw away the result of calling this if you are going to
    re-use the formula.

    (¹) Except it might throw away disjuncts! *)
