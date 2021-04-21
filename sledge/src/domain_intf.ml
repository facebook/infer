(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Abstract Domain *)
module type Dom = sig
  type t [@@deriving compare, equal, sexp_of]

  val pp : t pp
  val init : Llair.GlobalDefn.t iarray -> t
  val join : t -> t -> t
  val dnf : t -> t list
  val exec_assume : t -> Llair.Exp.t -> t option
  val exec_kill : Llair.Reg.t -> t -> t
  val exec_move : (Llair.Reg.t * Llair.Exp.t) iarray -> t -> t
  val exec_inst : Llair.inst -> t -> t Or_alarm.t

  type from_call [@@deriving sexp_of]

  val call :
       summaries:bool
    -> globals:Llair.Global.Set.t
    -> actuals:Llair.Exp.t iarray
    -> areturn:Llair.Reg.t option
    -> formals:Llair.Reg.t iarray
    -> freturn:Llair.Reg.t option
    -> locals:Llair.Reg.Set.t
    -> t
    -> t * from_call

  val post : Llair.Reg.Set.t -> from_call -> t -> t
  val retn : Llair.Reg.t iarray -> Llair.Reg.t option -> from_call -> t -> t

  val resolve_callee :
    (string -> Llair.func option) -> Llair.Exp.t -> t -> Llair.func list

  val recursion_beyond_bound : [`skip | `prune]

  type summary

  val pp_summary : summary pp

  val create_summary :
    locals:Llair.Reg.Set.t -> formals:Llair.Reg.t iarray -> t -> summary * t

  val apply_summary : t -> summary -> t option
end
