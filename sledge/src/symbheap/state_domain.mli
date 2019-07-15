(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Abstract domain *)

type t [@@deriving equal, sexp_of]

val pp : t pp
val init : Global.t vector -> t
val join : t -> t -> t
val is_false : t -> bool
val exec_assume : t -> Exp.t -> t option
val exec_inst : t -> Llair.inst -> (t, unit) result
val exec_return : t -> Var.t -> Exp.t -> t

val exec_intrinsic :
     skip_throw:bool
  -> t
  -> Var.t option
  -> Var.t
  -> Exp.t list
  -> (t, unit) result option

type from_call [@@deriving compare, equal, sexp]

val jump :
  Exp.t list -> Var.t list -> ?temps:Var.Set.t -> t -> t * from_call

val call :
     summaries:bool
  -> Exp.t list
  -> Var.t list
  -> Var.Set.t
  -> Var.Set.t
  -> t
  -> t * from_call

type function_summary = {xs: Var.Set.t; foot: t; post: t}

val pp_function_summary : Format.formatter -> function_summary -> unit
val apply_summary : function_summary -> t -> t option

(* formals should include all the parameters of the summary. That is both
   formals and globals.*)
val create_summary :
     locals:Var.Set.t
  -> formals:Var.Set.t
  -> entry:t
  -> current:t
  -> function_summary * t

val post : Var.Set.t -> t -> t
val retn : Var.t list -> from_call -> t -> t
val dnf : t -> t list

val resolve_callee :
  (Var.t -> Llair.func list) -> Exp.t -> t -> Llair.func list
