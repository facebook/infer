(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format

(** type of a procedure call; either direct or via function pointer *)
type call =
  | Direct of Typ.Procname.t
  | Indirect of AccessPath.Raw.t
[@@deriving compare]

val pp_call : F.formatter -> call -> unit

type t =
  | Write of AccessPath.Raw.t * HilExp.t * Location.t
  (** LHS access path, RHS expression *)
  | Assume of HilExp.t * [`Then | `Else] * Sil.if_kind * Location.t
  (** Assumed expression, true_branch boolean, source of the assume (conditional, ternary, etc.) *)
  | Call of AccessPath.base option * call * HilExp.t list * CallFlags.t * Location.t
  (** Var to hold the return if it exists, call expression, formals *)
[@@deriving compare]

val pp : F.formatter -> t -> unit

(** The result of translating an SIL instruction can either be a new HIL instruction or an update to
    the identifier map (in the case that the SIL instructions writes to a temporary) *)
type translation =
  | Instr of t
  | Update of Var.t * AccessPath.Raw.t
  | Ignore

(** Convert an SIL instruction to an HIL instruction *)
val of_sil : f_resolve_id:(Var.t -> AccessPath.Raw.t option) -> Sil.instr -> translation
