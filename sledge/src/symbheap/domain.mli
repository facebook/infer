(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Abstract domain *)

type t

val pp : t pp
val init : Global.t vector -> t
val join : t -> t -> t
val exec_assume : t -> Exp.t -> t option
val exec_inst : t -> Llair.inst -> (t, unit) result
val exec_return : t -> Var.t -> Exp.t -> t

val exec_intrinsic :
  t -> Var.t option -> Var.t -> Exp.t list -> (t, unit) result option

type from_call [@@deriving sexp_of]

val jump : Exp.t list -> Var.t list -> ?temps:Var.Set.t -> t -> t

val call :
     summaries:bool
  -> Exp.t list
  -> Var.t list
  -> Var.Set.t
  -> Global.t vector
  -> t
  -> t * from_call

val post : Var.Set.t -> from_call -> t -> t
val retn : Var.t list -> from_call -> t -> t
val dnf : t -> t list

val resolve_callee :
  (Var.t -> Llair.func list) -> Exp.t -> t -> Llair.func list
