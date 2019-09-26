(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Abstract Domain *)
module type Dom = sig
  type t [@@deriving equal, sexp_of]

  val pp : t pp
  val report_fmt_thunk : t -> Formatter.t -> unit
  val init : Global.t vector -> t
  val join : t -> t -> t option
  val is_false : t -> bool
  val exec_assume : t -> Exp.t -> t option
  val exec_kill : t -> Var.t -> t
  val exec_move : t -> Var.t -> Exp.t -> t
  val exec_inst : t -> Llair.inst -> (t, unit) result

  val exec_intrinsic :
       skip_throw:bool
    -> t
    -> Var.t option
    -> Var.t
    -> Exp.t list
    -> (t, unit) result option

  type from_call [@@deriving sexp_of]

  val call :
       summaries:bool
    -> globals:Var.Set.t
    -> Exp.t list
    -> Var.t option
    -> Var.t list
    -> locals:Var.Set.t
    -> t
    -> t * from_call

  val recursion_beyond_bound : [`skip | `prune]
  val post : Var.Set.t -> from_call -> t -> t
  val retn : Var.t list -> Var.t option -> from_call -> t -> t
  val dnf : t -> t list

  val resolve_callee :
    (Var.t -> Llair.func list) -> Exp.t -> t -> Llair.func list * t

  type summary

  val pp_summary : summary pp

  val create_summary :
    locals:Var.Set.t -> formals:Var.Set.t -> t -> summary * t

  val apply_summary : t -> summary -> t option
end
