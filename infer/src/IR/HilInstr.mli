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
type call = Direct of Typ.Procname.t | Indirect of AccessPath.t [@@deriving compare]

val pp_call : F.formatter -> call -> unit

type t =
  | Assign of AccessPath.t * HilExp.t * Location.t  (** LHS access path, RHS expression *)
  | Assume of HilExp.t * [`Then | `Else] * Sil.if_kind * Location.t
      (** Assumed expression, true_branch boolean, source of the assume (conditional, ternary, etc.) *)
  | Call of AccessPath.base option * call * HilExp.t list * CallFlags.t * Location.t
      (** Var to hold the return if it exists, call expression, formals *)
  [@@deriving compare]

val pp : F.formatter -> t -> unit

(** Result of translating an SIL instruction *)
type translation =
  | Instr of t  (** HIL instruction to execute *)
  | Bind of Var.t * AccessPath.t  (** add binding to identifier map *)
  | Unbind of Var.t list  (** remove binding from identifier map *)
  | Ignore  (** no-op *)

val of_sil : f_resolve_id:(Var.t -> AccessPath.t option) -> Sil.instr -> translation
(** Convert an SIL instruction to an HIL instruction *)
