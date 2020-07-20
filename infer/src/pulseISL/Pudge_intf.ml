(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractValue = PulseISLAbstractValue

module type S = sig
  type t

  val pp : F.formatter -> t -> unit

  val true_ : t

  module Var : sig
    type t

    val of_absval : AbstractValue.t -> t

    val to_absval : t -> AbstractValue.t
    (** use with caution: will crash the program if the given variable wasn't generated from an
        [AbstractValue.t] using [Var.of_absval] *)
  end

  module Term : sig
    type t

    val zero : t

    val of_intlit : IntLit.t -> t

    val of_absval : AbstractValue.t -> t

    val of_unop : Unop.t -> t -> t option

    val of_binop : Binop.t -> t -> t -> t option
  end

  module Formula : sig
    type t

    val eq : Term.t -> Term.t -> t

    val lt : Term.t -> Term.t -> t

    val not_ : t -> t

    val term_binop : Binop.t -> Term.t -> Term.t -> t option
  end

  val and_formula : Formula.t -> t -> t

  val and_ : t -> t -> t

  (** queries *)

  val is_unsat : t -> bool

  val is_known_zero : Term.t -> t -> bool
  (** [is_known_zero phi t] returns [true] if [phi |- t = 0], [false] if we don't know for sure *)

  (** operations *)

  val fold_map_variables : t -> init:'a -> f:('a -> Var.t -> 'a * Var.t) -> 'a * t

  val simplify : keep:AbstractValue.Set.t -> t -> t
  (** [simplify ~keep phi] attempts to get rid of as many variables in [fv phi] but not in [keep] as
      possible *)
end
