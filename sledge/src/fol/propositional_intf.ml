(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Propositional formulas *)

(** Formulas, built from literals with predicate symbols from various
    theories, and propositional constants and connectives. Denote sets of
    structures. *)
module type FORMULA = sig
  type trm
  type set

  type t = private
    (* propositional constants *)
    | Tt
    (* equality *)
    | Eq of trm * trm
    (* arithmetic *)
    | Eq0 of trm  (** [Eq0(x)] iff x = 0 *)
    | Pos of trm  (** [Pos(x)] iff x > 0 *)
    (* propositional connectives *)
    | Not of t
    | And of {pos: set; neg: set}
    | Or of {pos: set; neg: set}
    | Iff of t * t
    | Cond of {cnd: t; pos: t; neg: t}
    (* uninterpreted literals *)
    | Lit of Predsym.t * trm array
  [@@deriving compare, equal, sexp]

  val mk_Tt : unit -> t
  val _Eq : trm -> trm -> t
  val _Eq0 : trm -> t
  val _Pos : trm -> t
  val _Not : t -> t
  val _And : pos:set -> neg:set -> t
  val _Or : pos:set -> neg:set -> t
  val _Iff : t -> t -> t
  val _Cond : t -> t -> t -> t
  val _Lit : Predsym.t -> trm array -> t
  val and_ : t -> t -> t
  val or_ : t -> t -> t
  val is_negative : t -> bool
  val trms : t -> trm iter
  val map_and : t -> pos:set -> neg:set -> (t -> t) -> t
  val map_or : t -> pos:set -> neg:set -> (t -> t) -> t
end

(** Sets of formulas *)
module type FORMULA_SET = sig
  include Set.S

  val t_of_sexp : Sexp.t -> t
end
