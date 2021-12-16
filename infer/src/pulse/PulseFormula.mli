(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module SatUnsat = PulseSatUnsat

(* NOTE: using [Var] for [AbstractValue] here since this is how "abstract values" are interpreted,
   in particular as far as arithmetic is concerned *)
module Var = PulseAbstractValue

(** {2 Arithmetic solver}

    Build formulas from SIL and tries to decide if they are (mostly un-)satisfiable. *)

type t [@@deriving compare, equal, yojson_of]

val pp : F.formatter -> t -> unit

val pp_with_pp_var : (F.formatter -> Var.t -> unit) -> F.formatter -> t -> unit
  [@@warning "-32"]
(** only used for unit tests *)

type function_symbol = Unknown of Var.t | Procname of Procname.t [@@deriving compare]

type operand =
  | AbstractValueOperand of Var.t
  | ConstOperand of Const.t
  | FunctionApplicationOperand of {f: function_symbol; actuals: Var.t list}

val pp_operand : F.formatter -> operand -> unit

(** {3 Build formulas} *)

(** some operations will return a set of new facts discovered that are relevant to communicate to
    the memory domain *)
type new_eq = EqZero of Var.t | Equal of Var.t * Var.t

type new_eqs = new_eq list

val ttrue : t

val and_equal : operand -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_equal_instanceof : Var.t -> Var.t -> Typ.t -> t -> (t * new_eqs) SatUnsat.t

val and_less_equal : operand -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_less_than : operand -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_equal_unop : Var.t -> Unop.t -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_equal_binop : Var.t -> Binop.t -> operand -> operand -> t -> (t * new_eqs) SatUnsat.t

val and_is_int : Var.t -> t -> (t * new_eqs) SatUnsat.t

val prune_binop : negated:bool -> Binop.t -> operand -> operand -> t -> (t * new_eqs) SatUnsat.t

(** {3 Operations} *)

val normalize : Tenv.t -> get_dynamic_type:(Var.t -> Typ.t option) -> t -> (t * new_eqs) SatUnsat.t
(** think a bit harder about the formula *)

val simplify :
     Tenv.t
  -> get_dynamic_type:(Var.t -> Typ.t option)
  -> can_be_pruned:Var.Set.t
  -> keep:Var.Set.t
  -> t
  -> (t * Var.Set.t * new_eqs) SatUnsat.t

val and_fold_subst_variables :
     t
  -> up_to_f:t
  -> init:'acc
  -> f:('acc -> Var.t -> 'acc * Var.t)
  -> ('acc * t * new_eqs) SatUnsat.t

val is_known_zero : t -> Var.t -> bool

val has_no_assumptions : t -> bool

val get_known_var_repr : t -> Var.t -> Var.t
(** get the canonical representative for the variable according to the known/post equality relation *)

val get_both_var_repr : t -> Var.t -> Var.t
(** get the canonical representative for the variable according to the both/pre+post equality
    relation *)
