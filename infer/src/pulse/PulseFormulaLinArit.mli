(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module CItv = PulseCItv
module SatUnsat = PulseSatUnsat
module Debug = PulseFormulaDebug
module Var = PulseFormulaVar

(** linear combination of variables, eg [2·x + 3/4·y + 12] *)
type t [@@deriving compare, yojson_of, equal]

(* ['term_t] is meant to be [Term.t] but we cannot mention [Term] yet as it depends on [LinArith];
     we resolve the circular dependency further down this file *)
type 'term_t subst_target =
  | QSubst of Q.t
  | ConstantSubst of 'term_t * Var.t option
      (** the constant term to substitute and, optionally, a fallback variable to substitute with
          (eg if a variable is equal to a constant string but also to a new canonical
          representative, the linear arithmetic domain needs to use the latter *)
  | VarSubst of Var.t
  | LinSubst of t
  | NonLinearTermSubst of 'term_t

val pp : (F.formatter -> Var.t -> unit) -> F.formatter -> t -> unit

val is_zero : t -> bool

val add : t -> t -> t

val minus : t -> t

val subtract : t -> t -> t

val mult : Q.t -> t -> t

val solve_eq : t -> t -> (Var.t * t) option SatUnsat.t
(** [solve_eq l1 l2] is [Sat (Some (x, l))] if [l1=l2 <=> x=l], [Sat None] if [l1 = l2] is always
    true, and [Unsat] if it is always false *)

val of_q : Q.t -> t

val of_var : Var.t -> t

val get_as_const : t -> Q.t option
(** [get_as_const l] is [Some c] if [l=c], else [None] *)

val get_as_var : t -> Var.t option
(** [get_as_var l] is [Some x] if [l=x], else [None] *)

val get_as_variable_difference : t -> (Var.t * Var.t) option
(** [get_as_variable_difference l] is [Some (x,y)] if [l=(x-y)] or [l=(y-x)], else [None] *)

val get_constant_part : t -> Q.t
(** [get_as_const (c + l)] is [c] *)

val get_coefficient : Var.t -> t -> Q.t option

val get_variables : t -> Var.t Seq.t

val fold_subst_variables : t -> init:'a -> f:('a -> Var.t -> 'a * _ subst_target) -> 'a * t

val fold : t -> init:'a -> f:('a -> Var.t * Q.t -> 'a) -> 'a

val subst_variables : t -> f:(Var.t -> _ subst_target) -> t

val subst_variable : Var.t -> _ subst_target -> t -> t
(** same as above for a single variable to substitute (more optimized) *)

val get_simplest : t -> Var.t option
(** the smallest [v∊l] according to [Var.is_simpler_than] *)

(** {2 Tableau-Specific Operations} *)

val is_restricted : t -> bool
(** [true] iff all the variables involved in the expression satisfy {!Var.is_restricted} *)

val solve_for_unrestricted : Var.t -> t -> (Var.t * t) option
(** if [l] contains at least one unrestricted variable then [solve_for_unrestricted u l] is
    [Some (x, l')] where [x] is the smallest unrestricted variable in [l] and [u=l <=> x=l']. If
    there are no unrestricted variables in [l] then [solve_for_unrestricted u l] is [None]. Assumes
    [u∉l]. *)

val pivot : Var.t * Q.t -> t -> t
(** [pivot (v, q) l] assumes [v] appears in [l] with coefficient [q] and returns [l'] such that
    [l' = -(1/q)·(l - q·v)]*)

val classify_minimized_maximized :
     t
  -> [ `Minimized
       (** all the coefficients are positive, hence the constant part is a lower bound of the value
           of the linear expression (assuming all variables are restricted) *)
     | `Maximized
       (** all the coefficients are negative, hence the constant part is an upper bound of the value
           of the linear expression (assuming all variables are restricted) *)
     | `Neither
     | `Constant  (** no variables in this linear expression *) ]

val is_minimized : t -> bool
(** [is_minimized l] iff [classify_minimized_maximized l] is either [`Minimized] or [`Constant] *)
