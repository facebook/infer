(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Abstract domain *)

type t

val pp : t pp
val init : Global.t vector -> t
val join : t -> t -> t
val assume : t -> Exp.t -> t option
val exec_inst : t -> Llair.inst -> (t, t * Llair.inst) result

type from_call [@@deriving compare, equal, sexp]

val call : t -> Exp.t list -> Var.t list -> Var.Set.t -> t * from_call
val retn : Var.Set.t -> from_call -> t -> t

val resolve_callee :
  (Var.t -> Llair.func list) -> Exp.t -> t -> Llair.func list
