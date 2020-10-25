(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Propositional formulas *)

open Ses

module type TERM = sig
  type trm [@@deriving compare, equal, sexp]

  val zero : trm
  val eval_eq : trm -> trm -> bool option
  val eval_eq0 : trm -> bool option
  val eval_pos : trm -> bool option
end

(** Formulas, built from literals with predicate symbols from various
    theories, and propositional constants and connectives. Denote sets of
    structures. *)
module type FORMULA = sig
  type trm
  type fmls

  type fml = private
    (* propositional constants *)
    | Tt
    (* equality *)
    | Eq of trm * trm
    (* arithmetic *)
    | Eq0 of trm  (** [Eq0(x)] iff x = 0 *)
    | Pos of trm  (** [Pos(x)] iff x > 0 *)
    (* propositional connectives *)
    | Not of fml
    | And of {pos: fmls; neg: fmls}
    | Or of {pos: fmls; neg: fmls}
    | Iff of fml * fml
    | Cond of {cnd: fml; pos: fml; neg: fml}
    (* uninterpreted literals *)
    | Lit of Predsym.t * trm array
  [@@deriving compare, equal, sexp]

  val mk_Tt : unit -> fml
  val _Eq : trm -> trm -> fml
  val _Eq0 : trm -> fml
  val _Pos : trm -> fml
  val _Not : fml -> fml
  val _And : pos:fmls -> neg:fmls -> fml
  val _Or : pos:fmls -> neg:fmls -> fml
  val _Iff : fml -> fml -> fml
  val _Cond : fml -> fml -> fml -> fml
  val _Lit : Predsym.t -> trm array -> fml
  val and_ : fml -> fml -> fml
  val or_ : fml -> fml -> fml
end

(** Sets of formulas *)
module type FORMULA_SET = sig
  include Set.S

  val t_of_sexp : Sexp.t -> t
end

module type MAKE = functor (Trm : TERM) -> sig
  module rec Fml :
    (FORMULA with type trm := Trm.trm with type fmls := Fmls.t)
  and Fmls : (FORMULA_SET with type elt := Fml.fml)
end
