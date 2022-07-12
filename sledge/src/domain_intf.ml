(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Abstract Domain *)
module type Domain = sig
  type t [@@deriving compare, equal, sexp_of]

  module Set : Set.S with type elt := t

  val pp : t pp
  val init : Llair.GlobalDefn.t iarray -> t
  val join : t -> t -> t
  val joinN : Set.t -> t
  val dnf : t -> t iter
  val exec_assume : ThreadID.t -> t -> Llair.Exp.t -> t option
  val exec_kill : ThreadID.t -> Llair.Reg.t -> t -> t
  val exec_move : ThreadID.t -> (Llair.Reg.t * Llair.Exp.t) iarray -> t -> t
  val exec_inst : ThreadID.t -> Llair.inst -> t -> t Or_alarm.t
  val resolve_int : ThreadID.t -> t -> Llair.Exp.t -> int list

  type from_call [@@deriving sexp_of]

  val call :
       summaries:bool
    -> ThreadID.t
    -> ?child:ThreadID.t
    -> globals:Llair.Global.Set.t
    -> actuals:Llair.Exp.t iarray
    -> areturn:Llair.Reg.t option
    -> formals:Llair.Reg.t iarray
    -> freturn:Llair.Reg.t option
    -> locals:Llair.Reg.Set.t
    -> t
    -> t * from_call

  val post : ThreadID.t -> Llair.Reg.Set.t -> from_call -> t -> t

  val retn :
       ThreadID.t
    -> Llair.Reg.t iarray
    -> Llair.Reg.t option
    -> from_call
    -> t
    -> t

  type term_code [@@deriving compare, sexp_of]

  val term :
    ThreadID.t -> Llair.Reg.t iarray -> Llair.Reg.t option -> t -> term_code

  val move_term_code : ThreadID.t -> Llair.Reg.t -> term_code -> t -> t

  val resolve_callee :
       (string -> Llair.func option)
    -> ThreadID.t
    -> Llair.Exp.t
    -> t
    -> Llair.func list

  val recursion_beyond_bound : [`skip | `prune]

  type summary

  val pp_summary : summary pp

  val create_summary :
       ThreadID.t
    -> locals:Llair.Reg.Set.t
    -> formals:Llair.Reg.t iarray
    -> t
    -> summary * t

  val apply_summary : t -> summary -> t option
end
