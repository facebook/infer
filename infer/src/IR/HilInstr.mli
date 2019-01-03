(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** type of a procedure call; either direct or via function pointer *)
type call = Direct of Typ.Procname.t | Indirect of HilExp.AccessExpression.t [@@deriving compare]

val pp_call : F.formatter -> call -> unit

type t =
  | Assign of HilExp.AccessExpression.t * HilExp.t * Location.t
      (** LHS access expression, RHS expression *)
  | Assume of HilExp.t * [`Then | `Else] * Sil.if_kind * Location.t
      (** Assumed expression, true_branch boolean, source of the assume (conditional, ternary, etc.) *)
  | Call of AccessPath.base * call * HilExp.t list * CallFlags.t * Location.t
      (** Var to hold the return, call expression, formals *)
  | ExitScope of Var.t list * Location.t
[@@deriving compare]

val pp : F.formatter -> t -> unit

(** Result of translating an SIL instruction *)
type translation =
  | Instr of t  (** HIL instruction to execute *)
  | Bind of Var.t * HilExp.AccessExpression.t  (** add binding to identifier map *)
  | Ignore  (** no-op *)

val of_sil :
     include_array_indexes:bool
  -> f_resolve_id:(Var.t -> HilExp.AccessExpression.t option)
  -> Sil.instr
  -> translation
(** convert an SIL instruction into an HIL instruction. The [f_resolve_id] function should map an
   SSA temporary variable to the access path it represents. Evaluating the HIL instruction should
   produce the same result as evaluating the SIL instruction and replacing the temporary variables
   using [f_resolve_id]. *)
