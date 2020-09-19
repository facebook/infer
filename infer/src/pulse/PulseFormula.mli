(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(* NOTE: using [Var] for [AbstractValue] here since this is how "abstract values" are interpreted,
   in particular as far as arithmetic is concerned *)
module Var = PulseAbstractValue

(** {2 Arithmetic solver}

    Build formulas from SIL and tries to decide if they are (mostly un-)satisfiable. *)

type t

val pp : F.formatter -> t -> unit

val pp_with_pp_var : (F.formatter -> Var.t -> unit) -> F.formatter -> t -> unit
  [@@warning "-32"]
(** only used for unit tests *)

type 'a normalized = Unsat | Sat of 'a

type operand = LiteralOperand of IntLit.t | AbstractValueOperand of Var.t

(** {3 Build formulas} *)

val ttrue : t

val is_ttrue : t -> bool

val and_equal : operand -> operand -> t -> t normalized

val and_less_equal : operand -> operand -> t -> t normalized

val and_less_than : operand -> operand -> t -> t normalized

val and_equal_unop : Var.t -> Unop.t -> operand -> t -> t normalized

val and_equal_binop : Var.t -> Binop.t -> operand -> operand -> t -> t normalized

val prune_binop : negated:bool -> Binop.t -> operand -> operand -> t -> t normalized

(** {3 Operations} *)

val normalize : t -> t normalized
(** think a bit harder about the formula *)

val simplify : keep:Var.Set.t -> t -> t normalized

val and_fold_subst_variables :
  t -> up_to_f:t -> init:'acc -> f:('acc -> Var.t -> 'acc * Var.t) -> ('acc * t) normalized

val get_variables : t -> Var.Set.t
  
val is_known_zero : t -> Var.t -> bool

val as_int : t -> Var.t -> int option

(** {3 Notations} *)

include sig
  [@@@warning "-60"]

  (** Useful notations to deal with normalized formulas *)
  module SatUnsatMonad : sig
    [@@@warning "-32"]

    val map_normalized : ('a -> 'b) -> 'a normalized -> 'b normalized

    val ( >>| ) : 'a normalized -> ('a -> 'b) -> 'b normalized

    val ( let+ ) : 'a normalized -> ('a -> 'b) -> 'b normalized

    val bind_normalized : ('a -> 'b normalized) -> 'a normalized -> 'b normalized

    val ( >>= ) : 'a normalized -> ('a -> 'b normalized) -> 'b normalized

    val ( let* ) : 'a normalized -> ('a -> 'b normalized) -> 'b normalized
  end
end
