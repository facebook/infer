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
  val report_fmt_thunk : t -> Format.formatter -> unit
  val init : Llair.Global.t iarray -> t
  val join : t -> t -> t option
  val is_false : t -> bool
  val dnf : t -> t list
  val exec_assume : t -> Llair.Exp.t -> t option
  val exec_kill : t -> Llair.Reg.t -> t
  val exec_move : t -> (Llair.Reg.t * Llair.Exp.t) iarray -> t
  val exec_inst : t -> Llair.inst -> t option

  val exec_intrinsic :
       skip_throw:bool
    -> t
    -> Llair.Reg.t option
    -> Llair.Reg.t
    -> Llair.Exp.t list
    -> t option option

  type from_call [@@deriving sexp_of]

  val call :
       summaries:bool
    -> globals:Llair.Reg.Set.t
    -> actuals:Llair.Exp.t list
    -> areturn:Llair.Reg.t option
    -> formals:Llair.Reg.t list
    -> freturn:Llair.Reg.t option
    -> locals:Llair.Reg.Set.t
    -> t
    -> t * from_call

  val post : Llair.Reg.Set.t -> from_call -> t -> t
  val retn : Llair.Reg.t list -> Llair.Reg.t option -> from_call -> t -> t

  val resolve_callee :
    (string -> Llair.func list) -> Llair.Exp.t -> t -> Llair.func list * t

  val recursion_beyond_bound : [`skip | `prune]

  type summary

  val pp_summary : summary pp

  val create_summary :
    locals:Llair.Reg.Set.t -> formals:Llair.Reg.Set.t -> t -> summary * t

  val apply_summary : t -> summary -> t option
end
